module Ir.Parse where

import Control.Applicative
import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Debug.Trace
import Ir.Printer
import Ir.Types
import Ir.Utils
import Language.C
import Text.PrettyPrint (render)
import Util (runUntilNoChange)
import Prelude hiding (dropWhile, takeWhile)

irParser :: Parser TranslationUnit
irParser = do
  structs <- runUntilNoChange (\m -> foldl go m m) . M.fromList <$> many parseStruct
  vars <- many parseVar
  fns <- many parseFunction
  return $ TranslationUnit (fmap (second $ replaceDType (f structs)) (vars ++ fns)) structs
  where
    go map (DStruct (TyStruct name as b offsets)) =
      let fields = fmap (replaceDType (f map)) <$> as
       in M.insert name (DStruct (TyStruct name fields b (calculateOffsets1 fields))) map
    go map _ = map
    f map t@(DStruct (TyStruct {..})) = fromMaybe t (M.lookup sname map)
    f _ x = x

parseStruct :: Parser (String, Dtype)
parseStruct = do
  b <- word "struct"
  name <- takeWhile (not . isSpace)
  word ":"
  xs <- betweenSymbols '{' '}' (sepByChar ',' parseField)
  return (B8.unpack name, DStruct (TyStruct (B8.unpack name) xs False (0, 0, [])))
  where
    parseField =
      Named Nothing <$> (word "%anon:" *> parseDtype)
        <|> do
          name <- takeWhile (not . (==) ':')
          dtype <- word ":" *> parseDtype
          return $ Named (Just $ B8.unpack name) dtype

whitespace = many1 (satisfy isSpace)

betweenSymbols s e p = word (char s) *> word p <* word (char e)

word p = skipWhitespace *> p <* skipWhitespace
  where
    skipWhitespace = many (satisfy isSpace)

parseParams = betweenSymbols '(' ')' $ sepByChar ',' parseDtype

sepByChar c parser = sepBy parser (word (char c))

parseVar :: Parser (String, Declaration)
parseVar = do
  word $ string "var"
  dtype <- word parseDtype
  name <- word "@" *> takeWhile (not . isSpace)
  init <- optional (word "=" *> word (parseInit1 dtype))
  return (B8.unpack name, Variable dtype init) -- TODO

parseInit1 (DInt w s _) = char '-' *> betweenSymbols '(' ')' (go (-1)) <|> go 1
  where
    go c = InitConst . (\v -> Int (c * v) w s) <$> decimal
parseInit1 (DFloat w _) = char '-' *> betweenSymbols '(' ')' (go (-1)) <|> go 1
  where
    go c = InitConst . (\v -> Float (c * v) w) <$> double
parseInit1 (DArray d _) = InitList <$> betweenSymbols '{' '}' (sepByChar ',' (parseInit1 d))
parseInit1 d | d == stringTy = InitConst . String . B8.unpack <$> betweenSymbols '"' '"' (takeWhile (/= '"'))
parseInit1 (DStruct (TyStruct _ xs _ _)) = InitList <$> betweenSymbols '{' '}' (go xs)
  where
    go [] = return []
    go (x : xs) = do
      a <- parseInit1 (item x)
      as <- char ',' *> go xs <|> return []
      return (a : as)

parseFunction :: Parser (String, Declaration)
parseFunction = word $ do
  (fname, signature) <- parseSignature
  body <- betweenSymbols '{' '}' parseBody
  return (B8.unpack fname, Function signature body)
  where
    parseSignature :: Parser (B.ByteString, FunctionSignature)
    parseSignature = do
      word "fun"
      dtype <- word parseDtype
      fname <- word (char '@' *> takeWhile (not . isSpace))
      params <- parseParams
      return (fname, FunctionSignature dtype params)
    parseBody :: Parser FunctionDefinition
    parseBody = do
      (ibid, allocs) <- parseInit
      blocks <- many (word parseBlock)
      return $ FunctionDefinition allocs (M.fromList blocks) ibid

parseBlock :: Parser (BlockId, Block)
parseBlock = do
  blockid <- word "block" *> word parseBlockId <* word (char ':')
  phinodes <- many (word parsePhi)
  instrs <- many parseInst
  exit <- parseExit
  return (blockid, Block phinodes instrs exit)

parsePhi = do
  word "%" *> parseBlockId
  dtype <- word ":p" *> decimal *> word ":" *> parseDtype
  name <- Just . B8.unpack <$> (word ":" *> takeWhile (not . isSpace)) <|> pure Nothing
  return (Named name dtype)

parseBlockId :: Parser BlockId
parseBlockId = char 'b' *> (BlockId <$> decimal)

parseAlloc = do
  ty <- takeWhile (/= ':') *> char ':' *> parseDtype <* char ':'
  nm <- takeWhile (not . isSpace)
  return $ Named (Just $ B8.unpack nm) ty

parseInit = do
  word "init:"
  ibid <- word "bid:" *> word parseBlockId
  word "allocations:"
  allocs <- many (word parseAlloc)
  return (ibid, allocs)

parseJArg :: Parser JumpArg
parseJArg = word $ do
  bid <- parseBlockId
  args <- betweenSymbols '(' ')' $ sepByChar ',' parseOperand
  return $ JumpArg bid args

parseExit :: Parser BlockExit
parseExit = word (jump <|> cjump <|> ret <|> switch)
  where
    jump = Jump <$> (word "j" *> parseJArg)
    cjump = ConditionalJump <$> (word "br" *> parseOperand <* word ",") <*> parseJArg <*> (word "," *> parseJArg)
    ret = Return <$> (word "ret" *> parseOperand)
    switch =
      Switch <$> (word "switch" *> parseOperand) <*> (word "default" *> parseJArg)
        <*> betweenSymbols '[' ']' (many (word ((,) <$> word parseConstant <*> word parseJArg)))

parseInst = do
  r <- word parseOperand <* word "="
  let dty = dty' r
  word (nop <|> store <|> load <|> typeCast <|> call dty <|> getElementPtr dty <|> binOp dty <|> unaryOp dty)
  where
    nop = Nop <$ word "nop"
    typeCast = TypeCast <$> (word "typecast" *> parseOperand) <*> (word "to" *> parseDtype)
    store = flip Store <$> (word "store" *> word parseOperand) <*> word parseOperand -- Operand Operand
    load = Load <$> (word "load" *> parseOperand)
    unaryOp d = UnaryOp <$> word parseUnaryOp <*> parseOperand <*> return d
    binOp d = BinOp <$> word parseBinOp <*> parseOperand <*> parseOperand <*> return d
    call d = do
      calee <- word "call" *> parseOperand
      args <- betweenSymbols '(' ')' $ sepByChar ',' parseOperand
      return $ Call calee args d

    getElementPtr d = GetElementPtr <$> (word "getelementptr" *> parseOperand) <*> (word "offset" *> parseOperand) <*> return d
    dty' (Register _ d) = d

parseBinOp =
  CMulOp <$ string "mul"
    <|> CDivOp <$ string "div"
    <|> CRmdOp <$ string "mod"
    <|> CAddOp <$ string "add"
    <|> CSubOp <$ string "sub"
    <|> CShlOp <$ string "shl"
    <|> CShrOp <$ string "shr"
    <|> CEqOp <$ string "cmp eq"
    <|> CNeqOp <$ string "cmp ne"
    <|> CLeOp <$ string "cmp lt"
    <|> CLeqOp <$ string "cmp le"
    <|> CGrOp <$ string "cmp gt"
    <|> CGeqOp <$ string "cmp ge"
    <|> CAndOp <$ string "and"
    <|> CXorOp <$ string "xor"
    <|> COrOp <$ string "or"

parseUnaryOp =
  CPlusOp <$ string "plus"
    <|> CMinOp <$ string "minus"
    <|> CNegOp <$ string "negate"

parseDtype :: Parser Dtype
parseDtype = parseInner >>= pointer
  where
    pointer d = try (char '*' *> const d) <|> return d
    const d = string "const" *> pointer (DPointer d True) <|> pointer (DPointer d False)

parseInner :: Parser Dtype
parseInner =
  DUnit False <$ word "unit" <|> int <$> (char 'i' *> decimal) <|> uint <$> (char 'u' *> decimal)
    <|> fun <$> (word "[ret:" *> parseDtype) <*> (word "params:" *> parseParams <* word "]")
    <|> float <$> (char 'f' *> decimal)
    <|> array <$> (word "[" *> decimal) <*> (word "x" *> parseDtype <* word "]")
    <|> do
      b <- False <$ word "struct" <|> True <$ word "const struct"
      ident <- structIdent
      return (DStruct (TyStruct ident [] b (0, 0, [])))
  where
    int s = DInt s True False
    uint s = DInt s False False
    fun ret args = DFunction (FunctionSignature ret args)
    float s = DFloat s False
    array siz ty = DArray ty siz

structIdent :: Parser String
structIdent = do
  first <- char '_' <|> char '%' <|> satisfy isAlphaNum
  rest <- many (satisfy isAlphaNum)
  return (first : rest)

parseOperand :: Parser Operand
parseOperand = word (Constant <$> parseConstant <|> parseRegister)

parseConstant :: Parser Constant
parseConstant = str <|> unit <|> undef <|> int <|> uint <|> float <|> globalvar
  where
    unit = Unit <$ word "unit:unit"
    undef = Undef <$> (word "undef" *> char ':' *> parseDtype)
    int = (\v s -> Int v s True) <$> decimal <*> (word ":i" *> decimal)
    uint = (\v s -> Int v s False) <$> decimal <*> (word ":u" *> decimal)
    float = Float <$> double <*> (word ":f" *> decimal)
    str = String . B8.unpack <$> (betweenSymbols '"' '"' (takeWhile (/= '"')) <* (char ':' *> parseDtype))

    globalvar = GlobalVariable <$> (B8.unpack <$> (word "@" *> takeWhile (/= ':'))) <*> (char ':' *> parseDtype)

parseRegister = Register <$> (char '%' *> (local <|> arg <|> temp)) <*> (char ':' *> parseDtype)
  where
    local = Local <$> (char 'l' *> decimal)
    arg = Arg <$> parseBlockId <*> (word ":p" *> decimal)
    temp = Temp <$> parseBlockId <*> (word ":i" *> decimal)

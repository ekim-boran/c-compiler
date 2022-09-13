int graph_dijkstra_dist[2000];
int graph_dijkstra_visited[2000];
int graph_weight[2000][2000];

void graph_weight_init(int n, int nonce, int *x, int (*weight)[2000])
{
  for (int i = 0; i < n; ++i)
  {
    weight[i][i] = 0;

    for (int j = 1; j < n; ++j)
    {
      weight[i][(i + j) % n] = ++*x;

      if (*x % (nonce + 1))
      {
        ++*x;
      }
    }
  }
}
int graph_dijkstra(int n, int nonce)
{
  if (!(n <= 2000))
  {
    return nonce;
  }

  int x = 0;
  graph_weight_init(n, nonce, &x, graph_weight);

  for (int i = 0; i < n; ++i)
  {
    graph_dijkstra_dist[i] = -1;
    graph_dijkstra_visited[i] = 0;
  }
  graph_dijkstra_dist[0] = 0;

  for (int step = 0; step < n; ++step)
  {
    int v = -1;
    for (int i = 0; i < n; ++i)
    {
      if (!(graph_dijkstra_dist[i] != -1 && !graph_dijkstra_visited[i]))
      {
        continue;
      }

      if (v != -1 && graph_dijkstra_dist[v] < graph_dijkstra_dist[i])
      {
        continue;
      }

      v = i;
    }

    if (v == -1)
    {
      break;
    }

    int dist = graph_dijkstra_dist[v];
    graph_dijkstra_visited[v] = 1;

    for (int i = 0; i < n; ++i)
    {
      if (graph_dijkstra_visited[i])
        continue;
      if (graph_dijkstra_dist[i] != -1 && graph_dijkstra_dist[i] < dist + graph_weight[v][i])
        continue;
      graph_dijkstra_dist[i] = dist + graph_weight[v][i];
    }
  }

  int result = 0;
  for (int i = 0; i < n; ++i)
  {
    result += graph_dijkstra_dist[i];
  }

  return result;
}

void printf(char *str, unsigned long);
unsigned long clock();
int main()
{
  unsigned long start_time = clock();
  int result = graph_dijkstra(2000, 10);
  printf("%d", (clock() - start_time));
  return result;
}

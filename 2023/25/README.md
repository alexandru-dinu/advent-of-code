---
title: "Day 25: Snowverload"
url: https://adventofcode.com/2023/day/25
tags: graphs
---

### Solution
```
Result: 525264
```

Initially, I solved it by visualizing the graph, manually removing the 3 bridge edges, then running DFS for finding the size of the connected components.

#### Upsolving

1. Using [`minimum_cut`][1] to find a cut of size exactly 3 with the source and the sink in separate partitions
2. Using [`edge_betweenness_centrality`][2] to find the 3 edges to be removed, having the largest betweenness centrality. This works because there will be lots of shortest paths passing through these 3 edges, as they act as a bridge.

### Usage
```
$ make
```

[1]: https://networkx.org/documentation/stable/reference/algorithms/generated/networkx.algorithms.flow.minimum_cut.html
[2]: https://networkx.org/documentation/stable/reference/algorithms/generated/networkx.algorithms.centrality.edge_betweenness_centrality.html

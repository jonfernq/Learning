## Graph Algorithms

> How are graphs represented in graph algorithms?

Graphs are data structures that represent the relationship between various objects. Graphs can be represented in different ways depending on the situation and the algorithm that we want to run on them. Here are some common ways to represent graphs:

- **Edge lists**: This is a list of edges, where each edge is an array of two vertex numbers or an object containing the vertex numbers and optionally the edge weight. The space required for an edge list is \\Theta (E) Θ(E), where E E is the number of edges. To find whether a particular edge is in the graph, we have to search through the edge list, which can take O (E) O(E) time if the edges are unordered¹.
- **Adjacency matrices**: This is a matrix of size V \\times V V×V, where V V is the number of vertices, and each entry is 1 if there is an edge between the corresponding vertices, or 0 otherwise. If the edges have weights, we can put them in the matrix entries instead of 1s. The space required for an adjacency matrix is \\Theta (V^2) Θ(V^2). To find whether a particular edge is in the graph, we just need to look up the matrix entry in O (1) O(1) time¹.
- **Adjacency lists**: This is a list of lists, where each list corresponds to a vertex and contains its neighbors. If the edges have weights, we can store them along with the neighbors. The space required for an adjacency list is \\Theta (V+E) Θ(V+E), where V V is the number of vertices and E E is the number of edges. To find whether a particular edge is in the graph, we have to search through the list of one of its endpoints, which can take O (V) O(V) time in the worst case³.

You can see some examples of these representations in ¹ and ³. You can also learn more about graphs and their properties in ² and ⁴.

Source: Conversation with Bing, 5/14/2023
- (1) Representing graphs (article) | Algorithms | Khan Academy. https://www.khanacademy.org/computing/computer-science/algorithms/graph-representation/a/representing-graphs.
- (2) Graph Theory Graph Representations - javatpoint. https://www.javatpoint.com/graph-theory-graph-representations.
- (3) Describing graphs (article) | Algorithms | Khan Academy. https://www.khanacademy.org/computing/computer-science/algorithms/graph-representation/a/describing-graphs.
- (4) Graph Representation Tutorials & Notes | Algorithms | HackerEarth. https://www.hackerearth.com/practice/algorithms/graphs/graph-representation/tutorial/.

---


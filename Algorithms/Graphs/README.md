## Graph Algorithms

Table of Contents 
- Graph Representation
- Dijkstra’s shortest path algorithm
- Applications 
- Python Object-Oriented Implementation

---
> How are graphs represented in graph algorithms?

Graphs are data structures that represent the relationship between various objects. Graphs can be represented in different ways depending on the situation and the algorithm that we want to run on them. Here are some common ways to represent graphs:

- **Edge lists**: This is a list of edges, where each edge is an array of two vertex numbers or an object containing the vertex numbers and optionally the edge weight. The space required for an edge list is Θ(E), where E is the number of edges. To find whether a particular edge is in the graph, we have to search through the edge list, which can take O(E) time if the edges are unordered¹.
- **Adjacency matrices**: This is a matrix of size V×V, where V is the number of vertices, and each entry is 1 if there is an edge between the corresponding vertices, or 0 otherwise. If the edges have weights, we can put them in the matrix entries instead of 1s. The space required for an adjacency matrix is Θ(V^2). To find whether a particular edge is in the graph, we just need to look up the matrix entry in O(1) time¹.
- **Adjacency lists**: This is a list of lists, where each list corresponds to a vertex and contains its neighbors. If the edges have weights, we can store them along with the neighbors. The space required for an adjacency list is Θ(V+E), where V is the number of vertices and E E is the number of edges. To find whether a particular edge is in the graph, we have to search through the list of one of its endpoints, which can take O(V) time in the worst case³.

You can see some examples of these representations in ¹ and ³. You can also learn more about graphs and their properties in ² and ⁴.

Source: Conversation with Bing, 5/14/2023
- (1) Representing graphs (article) | Algorithms | Khan Academy. https://www.khanacademy.org/computing/computer-science/algorithms/graph-representation/a/representing-graphs.
- (2) Graph Theory Graph Representations - javatpoint. https://www.javatpoint.com/graph-theory-graph-representations.
- (3) Describing graphs (article) | Algorithms | Khan Academy. https://www.khanacademy.org/computing/computer-science/algorithms/graph-representation/a/describing-graphs.
- (4) Graph Representation Tutorials & Notes | Algorithms | HackerEarth. https://www.hackerearth.com/practice/algorithms/graphs/graph-representation/tutorial/.

---
> What is Dijkstra’s shortest path algorithm?

Dijkstra’s shortest path algorithm is a popular algorithm for finding the shortest paths between nodes in a weighted graph, which may represent, for example, road networks. It was conceived by Dutch computer scientist Edsger W. Dijkstra in 1956 and published three years later.

The algorithm works by maintaining a set of visited nodes and a set of unvisited nodes. It starts at the source node and iteratively selects the unvisited node with the lowest distance from the source. It then visits the neighbors of this node and updates their distances if a shorter path is found. This process continues until the destination node is reached, or all reachable nodes have been visited².

The algorithm can be implemented using different data structures, such as arrays, priority queues, or heaps, to store and query the partial solutions sorted by distance from the source. The choice of data structure affects the time complexity of the algorithm. The original algorithm by Dijkstra does not use a min-priority queue and runs in O (V^2) O(V^2) time, where V V is the number of nodes. Using a Fibonacci heap min-priority queue can optimize the running time to O(E + V \\log V), where E is the number of edges³.

You can see some examples of how Dijkstra’s algorithm works in ¹ and ⁴. You can also learn more about the algorithm and its variants in ² and ⁵.

Source: Conversation with Bing, 5/14/2023
(1) What is Dijkstra’s Algorithm? | Introduction to Dijkstra's Shortest .... https://www.geeksforgeeks.org/introduction-to-dijkstras-shortest-path-algorithm/.
(2) Dijkstra's algorithm - Wikipedia. https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm.
(3) Dijkstra Algorithm - Dijkstra's shortest path algorithm - TutorialCup. https://tutorialcup.com/interview/algorithm/dijkstra-algorithm.htm.
(4) Dijkstra's Algorithm - Programiz. https://www.programiz.com/dsa/dijkstra-algorithm.
(5) Dijkstra's Shortest Path Algorithm with Examples - javatpoint. https://www.javatpoint.com/dijkstras-algorithm.

---

> What graph applications does Dijkstra’s shortest path algorithm have?

Dijkstra’s shortest path algorithm has many applications in graph problems where finding the shortest path between nodes is crucial. Some of these applications are:

- **Digital mapping services**: Dijkstra’s algorithm can be used to find the shortest route between two locations on a map, such as Google Maps. The nodes can represent cities or places, and the edges can represent roads or paths with weights corresponding to distances or travel times¹.
- **Social networking applications**: Dijkstra’s algorithm can be used to find the shortest path between two users in a social network, measured by the number of connections or handshakes between them. This can help suggest potential friends or contacts that a user may know¹.
- **Telephone network**: Dijkstra’s algorithm can be used to find the optimal route for a telephone call between two cities or places, minimizing the bandwidth or cost of the transmission. The nodes can represent switching stations, and the edges can represent transmission lines with weights corresponding to bandwidths¹.
- **IP routing**: Dijkstra’s algorithm can be used to find the open shortest path first (OSPF), which is a link-state routing protocol that determines the best path for data packets across a network of routers. The nodes can represent routers, and the edges can represent links with weights corresponding to metrics such as hop count, delay, or reliability².

You can learn more about these and other applications of Dijkstra’s algorithm in ¹ and ². You can also see some examples of how Dijkstra’s algorithm works in ³ and ⁴.

Source: Conversation with Bing, 5/14/2023
(1) Applications of Dijkstra’s shortest path algorithm. https://www.geeksforgeeks.org/applications-of-dijkstras-shortest-path-algorithm/.
(2) Dijkstra's Shortest Path Algorithm - A Detailed and Visual Introduction. https://www.freecodecamp.org/news/dijkstras-shortest-path-algorithm-visual-introduction/.
(3) Dijkstra’s algorithm to compute the shortest path through a graph. https://www.tutorialspoint.com/dijkstra-s-algorithm-to-compute-the-shortest-path-through-a-graph.
(4) Dijkstra's Shortest Path Algorithm | Brilliant Math & Science Wiki. https://brilliant.org/wiki/dijkstras-short-path-finder/.

---

> Write a Python class that implements Dijkstra’s algorithm taking a directed graph and a pair of nodes in the graph as input and returning  the shortest path.

```
import heapq

class Dijkstra:
    def __init__(self, graph):
        # graph is a dictionary of dictionaries that maps each node to its neighbors and weights
        self.graph = graph
        # dist is a dictionary that maps each node to its current shortest distance from the source
        self.dist = {}
        # prev is a dictionary that maps each node to its previous node in the shortest path
        self.prev = {}
        # pq is a priority queue of tuples (distance, node) that stores the nodes to be visited
        self.pq = []

    def shortest_path(self, source, target):
        # initialize the distances and previous nodes
        for node in self.graph:
            self.dist[node] = float('inf') # infinity
            self.prev[node] = None
        # set the distance of the source to 0 and add it to the priority queue
        self.dist[source] = 0
        heapq.heappush(self.pq, (0, source))
        # loop until the priority queue is empty or the target is found
        while self.pq:
            # get the node with the minimum distance from the priority queue
            distance, node = heapq.heappop(self.pq)
            # if the node is the target, we are done
            if node == target:
                break
            # otherwise, relax the edges from the node to its neighbors
            for neighbor, weight in self.graph[node].items():
                # calculate the new distance to the neighbor
                new_distance = distance + weight
                # if the new distance is smaller than the current distance, update it
                if new_distance < self.dist[neighbor]:
                    self.dist[neighbor] = new_distance
                    self.prev[neighbor] = node
                    # add the neighbor and its new distance to the priority queue
                    heapq.heappush(self.pq, (new_distance, neighbor))
        # if the target was not found, return None
        if self.prev[target] is None:
            return None
        # otherwise, reconstruct the shortest path by following the previous nodes
        path = []
        node = target
        while node is not None:
            path.append(node)
            node = self.prev[node]
        # reverse the path and return it
        path.reverse()
        return path
        
graph = {
    'A': {'B': 4, 'C': 2},
    'B': {'C': 5, 'D': 10},
    'C': {'E': 3},
    'D': {'F': 11},
    'E': {'D': 4},
    'F': {}
}

dijkstra = Dijkstra(graph)
path = dijkstra.shortest_path('A', 'F')
print(path)

# Output: ['A', 'C', 'E', 'D', 'F']
# This means that the shortest path from A to F is A -> C -> E -> D -> F, with a total distance of 2 + 3 + 4 + 11 = 20.
```

The program above works as follows:

- First, it creates a graph as a dictionary of dictionaries, where each key is a node and each value is another dictionary that maps its neighbors to their weights.
- Then, it creates an instance of the Dijkstra class and passes the graph as an argument to the constructor. The constructor initializes three attributes: graph, dist, and prev. Graph is the same as the input graph. Dist is a dictionary that maps each node to its current shortest distance from the source node. Prev is a dictionary that maps each node to its previous node in the shortest path. The constructor also creates a priority queue (pq) that will store the nodes to be visited along with their distances.
- Next, it calls the shortest_path method and passes the source node ('A') and the target node ('F') as arguments. The method does the following steps:
  - It initializes the distances and previous nodes for all nodes in the graph. It sets the distance of the source node to 0 and the rest to infinity. It sets the previous node of all nodes to None.
  - It adds the source node and its distance (0) to the priority queue.
  - It loops until the priority queue is empty or the target node is found. In each iteration, it does the following steps:
    - It pops the node with the minimum distance from the priority queue. This is the current node.
    - If the current node is the target node, it breaks out of the loop.
    - Otherwise, it loops through all the neighbors of the current node and their weights. For each neighbor, it does the following steps:
      - It calculates the new distance to the neighbor by adding the current distance and the weight of the edge.
      - If the new distance is smaller than the current distance of the neighbor, it updates its distance and previous node in the dist and prev dictionaries. It also adds the neighbor and its new distance to the priority queue.
  - After the loop ends, it checks if there is a path from the source to the target by looking at the prev dictionary. If there is no path, it returns None.
  - Otherwise, it reconstructs the shortest path by following the previous nodes from the target to the source and appending them to a list. It reverses the list and returns it as the output.

That's how the program above works. I hope this explanation was clear and helpful. 😊







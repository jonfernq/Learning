## Visualization of Data Structures 

### Binary Tree Visualization #2

Function given binary tree in matrix form and filename, writes out to current directory
jpeg and pdf with binary tree image. 

```
import graphviz

def graph_binary_tree(tree, filename):
    # Create a Graphviz graph object
    graph = graphviz.Digraph()

    # Add the nodes and edges to the graph
    def add_nodes(graph, i):
        if i < len(tree) and tree[i]:
            graph.node(str(i), label=str(tree[i]))
            if 2 * i + 1 < len(tree) and tree[2 * i + 1]:
                graph.edge(str(i), str(2 * i + 1))
            if 2 * i + 2 < len(tree) and tree[2 * i + 2]:
                graph.edge(str(i), str(2 * i + 2))
            add_nodes(graph, 2 * i + 1)
            add_nodes(graph, 2 * i + 2)

    # Add the root node to the graph
    graph.node(str(0), label=str(tree[0]))

    # Add nodes and edges to the graph
    add_nodes(graph, 0)

    # Render the graph to an image file
    graph.render(filename, format='pdf')
    graph.render(filename, format='jpeg')
    
# Define the binary tree using an array
tree = [1, 2, 3, 4, 5, None, 7]
graph_binary_tree(tree, 'binary_tree')
```
**OUTPUT:**

![binary_tree](https://user-images.githubusercontent.com/68504324/235374999-bfac27e8-13ec-4104-9861-783a073038ed.jpeg)

### Binary Tree Visualization #2

Here is a binary tree visualization with binary tree passed into function as a Python dictionary: 

```
import graphviz

# Define the binary tree using a dictionary
tree = {'A': ('B', 'C'), 'B': ('D', 'E'), 'C': ('F', 'G'), 'D': ('H', 'I'), 'E': ('J', None), 'F': (None, None), 'G': ('K', None)}
root = 'A'

def graph_binary_tree(tree, root):
    # Create a Graphviz graph object
    graph = graphviz.Digraph()

    # Add the nodes and edges to the graph
    def add_nodes(graph, parent, children):
        for child in children:
            if child is not None:
                graph.node(child)
                graph.edge(parent, child)
                add_nodes(graph, child, tree.get(child, (None, None)))

    # Add the root node to the graph
    graph.node(root)

    # Add nodes and edges to the graph
    add_nodes(graph, root, tree.get(root, (None, None)))

    # Render the graph to an image file
    graph.render('binary_tree', format='pdf')
    graph.render('binary_tree', format='jpeg')

graph_binary_tree(tree, root)
```

**OUTPUT:** 

![bintree](https://user-images.githubusercontent.com/68504324/235325068-5dfc84a2-7047-4ba7-ad51-e580f7c68c31.jpg)




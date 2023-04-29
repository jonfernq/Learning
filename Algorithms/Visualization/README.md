## Visualization of Data Structures 

### Binary Tree Visualization

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




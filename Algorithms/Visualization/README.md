## Visualization of Data Structures 

### Binary Tree Visualization

```
import graphviz

# Define the binary tree using a dictionary
tree = {'A': ('B', 'C'), 'B': ('D', 'E'), 'C': ('F', 'G'), 'D': ('H', 'I'), 'E': ('J', None), 'F': (None, None), 'G': ('K', None)}

# Create a Graphviz graph object
graph = graphviz.Digraph()

# Add the nodes and edges to the graph
def add_nodes1(graph, parent, tree):
    for node in tree[parent]:
        if node is not None:
            graph.node(node)
            graph.edge(parent, node)
            add_nodes(graph, node, tree)

def add_nodes(graph, parent, tree):
    children = tree.get(parent, None)
    if children is not None:
        for node in children:
            if node is not None:
                graph.node(node)
                graph.edge(parent, node)
                add_nodes(graph, node, tree)

add_nodes(graph, 'A', tree)

# Render the graph to an image file
graph.render('binary_tree6')

# Render the graph to a JPEG image file
graph.render('binary_tree5.jpg', format='jpg')
```

**OUTPUT:** 

![bintree](https://user-images.githubusercontent.com/68504324/235325068-5dfc84a2-7047-4ba7-ad51-e580f7c68c31.jpg)




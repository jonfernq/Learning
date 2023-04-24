```python
# strassen.py - Strassen matrix multiplication algorithm using recursion 

import numpy as np

def pretty_print_matrix(matrix, precision=2, suppress=True, separator='\t'):
    # Set formatting options
    np.set_printoptions(precision=precision, suppress=suppress, linewidth=100)

    # Print the matrix with custom formatting
    print(np.array2string(matrix, formatter={'float_kind': lambda x: "{:.{}f}".format(x, precision)}, separator=separator))

    
```


```python

"""
Matrix utility functions:

split_into_submatrices(matrix): returns four equally-sized submatrices (A11, A12, A21, A22) obtained 
by dividing n x n input matrix into four quadrants. The four quadrant matrices: top-left (A11), top-right (A12), 
bottom-left (A21), and bottom-right (A22) submatrices, each has n/2 rows and n/2 columns.

concatenate_quadrants(A11, A12, A21, A22): concatenates four equally sized quadrant matrices (A11, A12, A21, A22) 
horizontally and vertically to create a larger matrix (A11 in top-left quadrant, A12 in top-right, 
A21 in bottom-left quadrant, A22 in bottom-right).

size(matrix): size of an n x n matrix, the number of rows or columns in the matrix.
"""

def split_into_submatrices(matrix):
    # Input: matrix, an n x n matrix
    # Output: A11, A12, A21, A22, four equally sized submatrices of matrix
    
    n = size(matrix)
    split_idx = n // 2
    
    A11 = matrix[:split_idx, :split_idx]
    A12 = matrix[:split_idx, split_idx:]
    A21 = matrix[split_idx:, :split_idx]
    A22 = matrix[split_idx:, split_idx:]
    
    return A11, A12, A21, A22

def concatenate_quadrants(A11, A12, A21, A22):
    # Input: A11, A12, A21, A22, four equally sized matrices
    # Output: matrix, a combined matrix from the four quadrants
    
    top = np.concatenate((A11, A12), axis=1)
    bottom = np.concatenate((A21, A22), axis=1)
    matrix = np.concatenate((top, bottom), axis=0)
    
    return matrix

def size(matrix):
    # Input: matrix, an n x n matrix
    # Output: n, the size of the matrix (number of rows or columns)
    
    n = matrix.shape[0]
    return n

```


```python

"""

The strassen(matrix1, matrix2) function implements Strassen's algorithm for matrix multiplication. Here's a detailed plain English description of how the function works:

First, if matrices are size 1 x 1 (i.e., they have only one element), the product is just ordinary multiplication which is returned as result.

If matrices larger than 1 x 1, split into four equal-sized quadrants A11, A12, A21, and A22 for matrix1, and B11, B12, B21, and B22 for matrix2, using split_into_submatrices().

The 'strassen' function then recursively calls itself seven times with different combinations of these quadrant matrices, following Strassen's algorithm, calculating seven intermediate products:

P1 = strassen(A11 + A22, B11 + B22) 

P2 = strassen(A21 + A22, B11) 

P3 = strassen(A11, B12 - B22) 

P4 = strassen(A22, B21 - B11)

P5 = strassen(A11 + A12, B22) 

P6 = strassen(A21 - A11, B11 + B12) 

P7 = strassen(A12 - A22, B21 + B22)

The four quadrants of the final result matrix are calculated:

C11 = P1 + P4 - P5 + P7 

C12 = P3 + P5 

C21 = P2 + P4

C22 = P1 + P3 - P2 + P6 


"""


def strassen(matrix1, matrix2):
    # Input: matrix1, matrix2, two n x n matrices
    # Output: result, the product of matrix1 and matrix2 using Strassen's algorithm
    
    n = size(matrix1)
    
    # Base case: If matrices are 1 x 1, use naive multiplication
    if n == 1:
        return matrix1 * matrix2
    
    # Split matrices into quadrants
    A11, A12, A21, A22 = split_into_submatrices(matrix1)
    B11, B12, B21, B22 = split_into_submatrices(matrix2)
    
    # Recursive steps of Strassen's algorithm
    P1 = strassen(A11 + A22, B11 + B22)
    P2 = strassen(A21 + A22, B11)
    P3 = strassen(A11, B12 - B22)
    P4 = strassen(A22, B21 - B11)
    P5 = strassen(A11 + A12, B22)
    P6 = strassen(A21 - A11, B11 + B12)
    P7 = strassen(A12 - A22, B21 + B22)
    
    # Calculate result matrix
    C11 = P1 + P4 - P5 + P7
    C12 = P3 + P5
    C21 = P2 + P4
    C22 = P1 + P3 - P2 + P6
    
    # Concatenate quadrants to form the result matrix
    result = concatenate_quadrants(C11, C12, C21, C22)
    
    return result


```


```python
# Define the input arrays using Python Pandas 

A = np.array([[5., 1.],
              [8., 6.]]) 
                     
B = np.array([[2., 3.],
              [1., 7.]]) 

```


```python

# run the Strassen algorithm on the input matrices               
CS = strassen(A, B)    
pretty_print_matrix(CS, precision=3, suppress=True, separator='\t')

```

    [[11.000	22.000]
     [22.000	66.000]]
    


```python
# run ordinary matrix multiplication as a check 
CD = np.dot(A, B)      
pretty_print_matrix(CD, precision=3, suppress=True, separator='\t')
          
```

    [[11.000	22.000]
     [22.000	66.000]]
    


```python

```

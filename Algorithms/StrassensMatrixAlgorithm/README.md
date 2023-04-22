## Strassen's Matrix Multiplication Algorithm 

> Do you know about Strassen's algorithm for matrix multiplication ? 

Strassen's algorithm is a fast method for multiplying two matrices that reduces the number of required multiplications 
compared to the traditional matrix multiplication algorithm, known as the naive algorithm or the standard algorithm. 
It was developed by Volker Strassen in 1969 and is based on the principle of divide-and-conquer.

The key idea behind Strassen's algorithm is to divide each of the two input matrices into four smaller matrices, 
and recursively compute products of these smaller matrices using a set of seven multiplications and additional additions/subtractions. 
This allows for reducing the number of multiplications required for matrix multiplication from 8 in the naive algorithm to 7 in Strassen's algorithm, 
resulting in a faster overall computation time for large matrices.

Strassen's algorithm has a time complexity of O(n^log2(7)), which is approximately O(n^2.81), 
making it faster than the naive algorithm for large matrix sizes. However, due to its recursive nature 
and additional additions/subtractions, Strassen's algorithm may not always be the most efficient algorithm 
for small matrix sizes or for certain hardware architectures with efficient matrix multiplication operations. 
Other more sophisticated algorithms, such as the Coppersmith-Winograd algorithm, have been developed 
to further optimize matrix multiplication for certain matrix sizes and hardware configurations.

---
> Can you give an example for 2 X 2 matrices of - **this:** "reducing the number of multiplications required 
for matrix multiplication from 8 in the naive algorithm to 7 in Strassen's algorithm" ? 

Let's consider the multiplication of two 2x2 matrices, A and B, using both the naive algorithm and Strassen's algorithm.

Naive - **Algorithm:**

```markdown
A = | a11  a12 |    B = | b11  b12 |
    |         |        |         |
    | a21  a22 |        | b21  b22 |

C = | c11  c12 | = | a11*b11 + a12*b21  a11*b12 + a12*b22 |
    |         |     |                                            |
    | c21  c22 |     | a21*b11 + a22*b21  a21*b12 + a22*b22 |
```    
    
In the naive algorithm, we need to perform 8 multiplications to compute the four elements of the resulting matrix C.

**Strassen's - Algorithm:**

```scss
A = | a11  a12 |    B = | b11  b12 |
    |         |        |         |
    | a21  a22 |        | b21  b22 |

P1 = a11 * (b12 - b22)
P2 = (a11 + a12) * b22
P3 = (a21 + a22) * b11
P4 = a22 * (b21 - b11)
P5 = (a11 + a22) * (b11 + b22)
P6 = (a12 - a22) * (b21 + b22)
P7 = (a11 - a21) * (b11 + b12)

C = | c11  c12 | = | P5 + P4 - P2 + P6  P1 + P2 |
    |         |     |                                         |
    | c21  c22 |     | P3 + P4  P5 + P1 - P3 - P7 |
```

In Strassen's algorithm, we can see that we only need to perform 7 multiplications (P1 to P7) to compute the four elements of the resulting matrix C, instead of 8 as in the naive algorithm.

So, Strassen's algorithm reduces the number of multiplications required for matrix multiplication from 8 in the naive algorithm to 7, resulting in potentially faster matrix multiplication for large matrices. However, it's worth noting that Strassen's algorithm involves additional additions and subtractions compared to the naive algorithm, which may impact the overall computational efficiency depending on the specific hardware and matrix sizes.

---
> Can you write some simple pseudocode for the naive and Strassen's algorithm ? 

Here's a simple pseudocode for both the naive algorithm and Strassen's algorithm for matrix multiplication.

**Naive - Algorithm:**

```less
function multiply_matrices_naive(A, B):
    # - **Input:** A, B are n x n matrices
    # - **Output:** C, the product of A and B, an n x n matrix
    n = size(A)
    C = initialize n x n matrix with zeros
    
    for i = 1 to n - **do:**
        for j = 1 to n - **do:**
            for k = 1 to n - **do:**
                C[i][j] += A[i][k] * B[k][j]
                
    return C
```

**Strassen's - Algorithm:**

```bash
function multiply_matrices_strassen(A, B):
    # - **Input:** A, B are n x n matrices
    # - **Output:** C, the product of A and B, an n x n matrix
    n = size(A)
    
    # Base - **case:** If matrices are 1 x 1, perform standard multiplication
    if n == - **1:**
        C = initialize 1 x 1 matrix
        C[1][1] = A[1][1] * B[1][1]
        return C
    
    # Split matrices A and B into four equally sized submatrices
    A11, A12, A21, A22 = split_into_submatrices(A)
    B11, B12, B21, B22 = split_into_submatrices(B)
    
    # Recursively compute seven products (P1 to P7)
    P1 = multiply_matrices_strassen(A11, B12 - B22)
    P2 = multiply_matrices_strassen(A11 + A12, B22)
    P3 = multiply_matrices_strassen(A21 + A22, B11)
    P4 = multiply_matrices_strassen(A22, B21 - B11)
    P5 = multiply_matrices_strassen(A11 + A22, B11 + B22)
    P6 = multiply_matrices_strassen(A12 - A22, B21 + B22)
    P7 = multiply_matrices_strassen(A11 - A21, B11 + B12)
    
    # Compute four quadrants of the resulting matrix C
    C11 = P5 + P4 - P2 + P6
    C12 = P1 + P2
    C21 = P3 + P4
    C22 = P5 + P1 - P3 - P7
    
    # Concatenate the four quadrants to form the resulting matrix C
    C = concatenate_quadrants(C11, C12, C21, C22)
    
    return C
```

---
> Could you write some simple code that implements Strassen's algorithm that uses the first set of utility functions above?  And use the following 2 X 2 matrix multiplication as an example: [[5, 1],[8, 6]] X [[2,3],[1,7]] ? 

Here's an example implementation of Strassen's algorithm in Python, using the utility functions split_into_submatrices, concatenate_quadrants, and size as described earlier:

```python
import numpy as np

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

# Example usage with 2 x 2 matrices
matrix1 = np.array([[5, 1], [8, 6]])
matrix2 = np.array([[2, 3], [1, 7]])
result = strassen(matrix1, matrix2)

print("Matrix 1:")
print(matrix1)

print("Matrix 2:")
print(matrix2)

print("Result:")
print(result)
```

---
> Could you describe what those matrix utility functions above are doing in detailed plain English : split_into_submatrices, concatenate_quadrants, or size  ?

Here's a detailed explanation of the matrix utility functions in plain English:

- **split_into_submatrices(matrix):** This function takes an n x n matrix as input and returns four equally sized submatrices (A11, A12, A21, A22) obtained by splitting the input matrix into four quadrants. The input matrix is divided into top-left (A11), top-right (A12), bottom-left (A21), and bottom-right (A22) submatrices, each having n/2 rows and n/2 columns.

- **concatenate_quadrants(A11, A12, A21, A22):** This function takes four equally sized matrices (A11, A12, A21, A22) as input, which represent the four quadrants of a larger matrix. It then concatenates these four matrices horizontally and vertically to create a larger matrix, where A11 is in the top-left quadrant, A12 is in the top-right quadrant, A21 is in the bottom-left quadrant, and A22 is in the bottom-right quadrant.

- **size(matrix):** This function takes an n x n matrix as input and returns the size of the matrix, which is the number of rows or columns in the matrix. It simply retrieves the number of rows (or columns) from the input matrix and returns it as the size of the matrix.

---
> Can you describe in plain English and in detail what this function does: strassen(matrix1, matrix2) ? 

The strassen(matrix1, matrix2) function implements Strassen's algorithm for matrix multiplication. Here's a detailed plain English description of how the function works:

First, the size of the input matrices matrix1 and matrix2 is determined using the size() function, which returns the number of rows or columns in the matrix. If the matrices are of size 1 x 1 (i.e., they have only one element), the function directly computes the product using naive multiplication and returns the result.

If the matrices are larger than 1 x 1, they are split into four equal-sized quadrants A11, A12, A21, and A22 for matrix1, and B11, B12, B21, and B22 for matrix2, using the split_into_submatrices() function. This involves dividing each matrix into four equally sized submatrices, representing the top-left, top-right, bottom-left, and bottom-right quadrants, respectively.

The function then recursively calls itself seven times with different combinations of these quadrant matrices, as per the steps of Strassen's algorithm. Specifically, it calculates seven intermediate products P1, P2, P3, P4, P5, P6, and P7 as follows:

- P1 = strassen(A11 + A22, B11 + B22): Recursively calculates the product of the sum of A11 and A22 with the sum of B11 and B22.
- P2 = strassen(A21 + A22, B11): Recursively calculates the product of the sum of A21 and A22 with B11.
- P3 = strassen(A11, B12 - B22): Recursively calculates the product of A11 with the difference of B12 and B22.
- P4 = strassen(A22, B21 - B11): Recursively calculates the product of A22 with the difference of B21 and B11.
- P5 = strassen(A11 + A12, B22): Recursively calculates the product of the sum of A11 and A12 with B22.
- P6 = strassen(A21 - A11, B11 + B12): Recursively calculates the product of the difference of A21 and A11 with the sum of B11 and B12.
- P7 = strassen(A12 - A22, B21 + B22): Recursively calculates the product of the difference of A12 and A22 with the sum of B21 and B22.

Using these intermediate products, the function then calculates the four quadrants of the result matrix C11, C12, C21, and C22 as follows:

C11 = P1 + P4 - P5 + P7: Calculates the sum of P1, P4, and -P5, and adds P7.
C12 = P3 + P5: Calculates the sum of P3 and P5.
C21 = P2 + P4: Calculates the sum of P2 and P4.
C22 = P1 + P3 - P2 + P6: Calculates the sum of P1, `P3













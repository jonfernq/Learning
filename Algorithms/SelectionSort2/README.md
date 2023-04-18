## Selection Sort #2 

Selection sort is a simple comparison-based in place sorting algorithm.
Ascending sort order is chosen here to keep things simple. 

The smallest element is selected from the unsorted part of an array 
and moved to the beginning of the sorted part of the array.

### Pseudocode: 

```css
for i from 0 to n-1
    min_idx = i
    for j from i+1 to n
        if A[j] < A[min_idx]
            min_idx = j
    swap A[i] with A[min_idx]
```
In selection sort, the array has two parts, the sorted part to the left, and the unsorted part to the right.
To begin with the sorted part is empty and the unsorted part contains all the items to be sorted. 
Each iteration moves an item from the unsorted to the sorted part. 

Where:

- **A:** input array to be sorted
- **n:** length of the array
- **min_idx:** index of minimum element in unsorted portion of array
- **swap:** function that swaps position of two elements in array (or just in-line swap) 

### Python Code:

```css
def selection_sort(A):
    n = len(A)
    for i in range(n-1):
        min_idx = i
        for j in range(i, n):
            if A[j] < A[min_idx]:
                min_idx = j
        if min_idx != i:  #  if min_idx == i, minimum already at correct position, so unnecessary swap avoided
            temp = A[i]
            A[i] = A[min_idx]
            A[min_idx] = temp

# Test case
A = [64, 35, 12, 22, 11]
print("Original array:", A)
selection_sort(A)
print("Sorted array:", A)
```

![selection__](https://user-images.githubusercontent.com/68504324/232912404-033587fa-ddc6-494d-8c5e-90703823946c.jpg)

### Invariants: 

There are three potential loop invariants to track and verify for correctness: 

**Outer Loop:** 

- A[0...i-1] is sorted ([Ai...n] is unsorted portion)  
- smallest entries in a[0..i-1] (all entries in a[i..n-1] are larger than or equal to the entries in a[0..i-1]

**Inner Loop:**

- All entries in a[i..j-1] are greater than or equal to a[min_idx].

(De Rooij 2016)

### Arithmetic Progression (sequence)

First, here are essential definitions for the time complexity proof: 

a(n) = a(1) + (n - 1)d

- a(n) = nth term of arithmetic sequence (progression)
- d = common difference of succesive numbers

The sum of a finite arithmetic progression is an arithmetic series

Sum(n) = Sum of n terms of an arithmetic progression
= n(a(1) + a(n))/2

(Springer & European Mathematical Soceity 2023)

### Mathematical Proof of Time Complexity

In the worst case scenario the smallest array element is at the end of the array and the inner loop always finds a new minimum in each iteration. 

However, even if the array is already sorted, the iteration over the two embedded loops will still take place, thus best case time complexity is the same as worst case. The middle or 'average' case (not 'average' in the statistical sense) is thus the same also.  

The two embedded loops can be expressed as sum of O(n-i-1) for i ranging from 0 to n-2 which can be expressed as:

O(n-1) + O(n-2) + O(n-3) + ... + O(2) + O(1)

This is an arithmetic series with n-1 terms, where the common difference between consecutive terms is -1 
(since we are subtracting 1 from n in each term), and the first term is O(n-1).

The sum of an arithmetic series is calculated:

Sum = (n/2) * (2a + (n-1)d)

where n is the number of terms, a is the first term, and d is the common difference between consecutive terms.

Applying this formula to the arithmetic series O(n-1) + O(n-2) + O(n-3) + ... + O(2) + O(1), we get:

Sum = ((n-1)/2) * (2 * O(n-1) + (n-1) * (-1))

Simplifying further, we get:

Sum = ((n-1)/2) * (2O(n-1) - (n-1))

Dropping the constant terms and lower order terms, the sum can be expressed in big-O notation as:

Sum = O(n^2)

### Advantages & Disadvantages

To summarize, selection sort has time complexity of O(n^2) in the worst, average, and best cases, 
so it is not efficient for large datasets. 

However, it has the advantage of being simple to implement and understand. 

It also has low memory usage. Selection sort is an "in-place" sorting algorithm with the lowest possible space complexity of O(1). It operates directly on the input array and only swaps elements within the array to achieve sorted order, so it does not require allocation of additional memory as temporary storage during sorting. So, if memory usage needs to me minimized, this is a desirable feature. 

There are more efficient sorting algorithms such as merge sort, quicksort, and heapsort 
that are preferred for larger datasets.


### REFERENCES 

Furia, C.A. B. Meyer & S. Velder. "Loop invariants: analysis, classification, and examples."ACM Computing Surveys. vol. 46, no. 3, February 2014.

Cormen, T. H., Leiserson, C. E., & Rivest, R. L. (2022). Introduction to algorithms. MIT Laboratory for Computer Science. pp. 17-24. 

De Rooij, S. (2016, February 23). What is the loop invariant of selection sort? Quora. Retrieved April 19, 2023, from https://www.quora.com/What-is-the-loop-invariant-of-selection-sort

Meyer, B. (2009) Touch of Class. Learning to Program Well with Objects and Contracts. Berlin Heidelberg: Springer-Verlag. Retrieved April 16, 2023 from: https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=8ed14032893a16e4c762cc5fe09a4979dd0de527

Meyer, B. (2014, April 17). "Publish no loop without its invariant." Bertrand Meyer's technology+ blog.  Retrieved April 16, 2023, from https://bertrandmeyer.com/2011/05/12/publish-no-loop-without-its-invariant/ 

Paige, R.. "Programming with Invariants." IEEE Software, 3(1):56–69. January 1986.

Springer & European Mathematical Soceity (2023, March 26). Arithmetic series. Encyclopedia of Mathematics. Retrieved April 19, 2023, from https://encyclopediaofmath.org/index.php?title=Arithmetic_series

https://mathworld.wolfram.com/ArithmeticSeries.html

Wikipedia: Loop invariant, Arithmetic progression 





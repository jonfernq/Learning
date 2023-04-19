## Selection Sort 
- Algorithms Portfolio Project - Week 2
- Jon Fernquest

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

### Loop Invariants: 

There are three potential loop invariants that can be used with mathematical induction to proof correctness 
(Meyer 2009, 2014; Furia, Meyer and Velder 2014; Paige 1986): 

**Outer Loop:** 

- A[0...i-1] is sorted ([Ai...n] is unsorted portion)  
- smallest entries in a[0..i-1] (all entries in a[i..n-1] are larger than or equal to the entries in a[0..i-1]) 

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

(Springer & European Mathematical Soceity 2023; Wolfram 2023)

### Mathematical Proof of Time Complexity

In the worst case scenario the array is unsorted and the smallest array element is at the very end of the array, so that the inner loop always finds a new minimum in each iteration. 

However, even if the array is already sorted, the iteration over the two embedded loops will still take place, 
thus best case time complexity is the same as worst case. The middle or 'average' case (not 'average' in the statistical sense) is thus the same also.  

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

Dropping the constant terms and lower order terms, the sum can be expressed in Big-O notation as:

Sum = O(n^2)

### Advantages & Disadvantages: Uses and Real-Time Applications

To summarize, the uses and real-time applications of selection sort are limited because it has time complexity of O(n^2) in the worst, average, and best cases, so it is not efficient for large datasets. There are more efficient sorting algorithms such as merge sort, quicksort, and heapsort that are preferred for larger datasets. 

However, there are worse algorithms such as random shuffling until a sorted list is encountered, known as 'bogosort', with average time complexity of  O(nXn!). Even bubble sort is better than selection sort since it has O(n) best time complexity. 

Selection sort does have the advantage of being simple to implement and understand. This makes it useable in non-asymptotic use cases (Sande, Lau, and Ngo 2022:211-12). 

Selection sort also has low memory usage. Selection sort is an "in-place" sorting algorithm with the lowest possible space complexity of O(1). It operates directly on the input array and only swaps elements within the array to achieve sorted order, so it does not require allocation of additional memory as temporary storage during sorting. So, if memory usage needs to me minimized, this is a desirable feature. This is in contrast to 'merge sort' which uses the divide and conquer approach with time complexity of O(n log n) but space complexity of O(n log n) to O(log n) because it allocates memory. 

Similar to selection sort in its simplicity, 'insertion sort' is a good choice if data is already partially sorted. It has best time complexity of O(n) if the data is already sorted, and gets progressively better as more of the data is sorted. The sorted elements of insertion sort (and bubble sort) also retain their order after sorting, a desirable quality known as 'stability'. This is in contrast to selection sort which does not. For these reasons, the Dart Programming language standard library uses insertion sort for n less than or equal to 32 (Sande, Lau, and Ngo 2022:213). 

Insertion sort is thus similar to selection sort but a better choice for small n. 

Here is an informative summary from the Wikipedia Sorting algorithm page: 

![sorting_summary](https://user-images.githubusercontent.com/68504324/232928621-e1f60082-ff80-4cb2-b555-e624fd7ecccb.jpg)

### REFERENCES 

Furia, C.A. B. Meyer & S. Velder. (2014). "Loop invariants: analysis, classification, and examples."ACM Computing Surveys. vol. 46, no. 3, February 2014.

Cormen, T. H., Leiserson, C. E., & Rivest, R. L. (2022). Introduction to algorithms. MIT Laboratory for Computer Science. pp. 17-24. 

De Rooij, S. (2016, February 23). What is the loop invariant of selection sort? Quora. Retrieved April 19, 2023, from https://www.quora.com/What-is-the-loop-invariant-of-selection-sort

Meyer, B. (2009) Touch of Class. Learning to Program Well with Objects and Contracts. Berlin Heidelberg: Springer-Verlag. Retrieved April 16, 2023 from: https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=8ed14032893a16e4c762cc5fe09a4979dd0de527

Meyer, B. (2014, April 17). "Publish no loop without its invariant." Bertrand Meyer's technology+ blog.  Retrieved April 16, 2023, from https://bertrandmeyer.com/2011/05/12/publish-no-loop-without-its-invariant/ 

Paige, R. (1986) "Programming with Invariants." IEEE Software, 3(1):56–69. January 1986.

Sande, Jonathan, Kelvin Lau, and Vincent Ngo. (2022). 'Data Structures & Algorithms in Dart (First Edition): Implementing Practical Data Structures in Dart.' raywenderlich.com 

Springer & European Mathematical Soceity (2023, March 26). Arithmetic series. Encyclopedia of Mathematics. Retrieved April 19, 2023, from https://encyclopediaofmath.org/index.php?title=Arithmetic_series

Wolfram (2023, March 24). Arithmetic Series. WolframMathworld. Retrieved April 19, 2023, from https://mathworld.wolfram.com/ArithmeticSeries.html

Wikipedia: Loop invariant, Arithmetic progression; Sorting algorithm; Bogosort; Best, worst and average case; Comparison sort; Bubble sort; Selection sort; Insertion sort; Merge sort.  





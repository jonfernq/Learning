## Selection Sort #2 

Selection sort is a simple comparison-based in place sorting algorithm.
Ascending sort order is chosen here to keep things simple. 

The smallest element is selected from the unsorted part of an array 
and moved to the beginning of the sorted part of the array.

**Pseudocode:**

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
- **swap:** function that swaps position of two elements in array

### Advantages & Disadvantages

Selection sort has time complexity of O(n^2) in the worst, average, and best cases, 
so it is not efficient for large datasets. 

However, it has the advantage of being simple to implement and understand. 

It also has low memory usage. Selection sort is an "in-place" sorting algorithm with the lowest possible space complexity of O(1). It operates directly on the input array and only swaps elements within the array to achieve sorted order, so it does not require allocation of additional memory as temporary storage during sorting. So, if memory usage needs to me minimized, this is a desirable feature. 

There are more efficient sorting algorithms such as merge sort, quicksort, and heapsort 
that are preferred for larger datasets.







Regenerate response


## Quicksort 


> Please provide some recursive pseudocode for Quicksort

Here is the recursive pseudocode for Quicksort:

```css

function quicksort(arr, left, right)
    if left < right then
        pivot_index = partition(arr, left, right)
        quicksort(arr, left, pivot_index - 1)
        quicksort(arr, pivot_index + 1, right)

function partition(arr, left, right)
    pivot = arr[right]
    i = left - 1
    for j = left to right - 1 do
        if arr[j] <= pivot then
            i = i + 1
            swap(arr[i], arr[j])
    swap(arr[i + 1], arr[right])
    return i + 1
```



The `quicksort` function takes an array `arr`, a left index `left`, and a right index `right`. It first checks if `left < right`, which indicates that there is more than one element in the array. If this condition is true, it calls the `partition` function to choose a pivot element and partition the array into two subarrays. It then recursively calls `quicksort` on the left and right subarrays, sorting them in place.

The `partition` function takes an array `arr`, a left index `left`, and a right index `right`. It chooses a pivot element, which in this implementation is the rightmost element in the array. It then partitions the array into two subarrays by iterating over the array from `left` to `right - 1` and swapping elements to move all elements less than or equal to the pivot to the left subarray and all elements greater than the pivot to the right subarray. Finally, it swaps the pivot element with the first element in the right subarray and returns the index of the pivot element.

Note that this implementation uses an in-place partitioning algorithm, which means that it does not create additional arrays or use additional memory beyond the input array.

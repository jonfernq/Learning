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

---
> Please describe the essential idea behind the recursive version of quicksort below in plain English.

The essential idea behind the recursive version of quicksort is to divide the given array into smaller subarrays, sort them independently, and then combine them in a way that produces a sorted array.

The algorithm starts by choosing a pivot element from the array and then partitioning the array into two subarrays: one containing elements that are less than or equal to the pivot, and the other containing elements that are greater than the pivot. This is done by comparing each element in the array to the pivot and swapping elements as needed.

Once the partition is done, the algorithm recursively calls itself on each subarray to sort them. This is done by setting the left and right indices of the subarrays and calling the quicksort function on them.

The recursion continues until the base case is reached, which is when the subarrays have only one element or are empty. At this point, the recursion stops, and the subarrays are combined to produce a sorted array.

---
> Please derive the best, average and worst case time complexity for quicksort

The best, average, and worst-case time complexity of quicksort are as follows:

Best case:
In the best case, the pivot element is chosen such that it divides the input array into two subarrays of equal size. In this case, the partitioning process results in two subproblems of equal size, and the recursion tree is perfectly balanced. In this scenario, the best-case time complexity of quicksort is O(n log n), where n is the size of the input array.

Average case:
In the average case, the pivot element is chosen randomly, and the input array is partitioned into two subarrays of roughly equal size. The recursion tree is not perfectly balanced, but its height is still proportional to log n. The partitioning process will take O(n) time on average, and since we perform this operation recursively log n times, the average case time complexity of quicksort is O(n log n).

Worst case:
In the worst case, the pivot element is either the smallest or largest element in the input array, causing the partitioning process to create a subproblem of size n-1 and a subproblem of size 0. This scenario will cause the recursion tree to be unbalanced, with one branch having n-1 nodes, and the other branch having only one node. As a result, the worst-case time complexity of quicksort is O(n^2).

However, it is worth noting that the worst-case scenario is quite rare and can be avoided by using various techniques such as choosing a good pivot element, randomizing the pivot element, or using a hybrid sorting algorithm that switches to a different algorithm when the input size falls below a certain threshold. With these techniques, the worst-case time complexity of quicksort can be reduced to O(n log n).

---
> Please explain the time complexity of quicksort in terms of the master theorem.

The time complexity of quicksort can be analyzed using the master theorem, which provides a general framework for analyzing the time complexity of divide-and-conquer algorithms.

The master theorem states that for a recurrence relation of the form:

T(n) = aT(n/b) + f(n)

where a is the number of subproblems, n/b is the size of each subproblem, and f(n) is the time complexity of the work done to combine the subproblems, the time complexity of the algorithm can be expressed as:

T(n) =
O(n^log_b(a)) if f(n) = O(n^log_b(a - ε)) for some ε > 0
O(n^log_b(a) * log n) if f(n) = Θ(n^log_b(a))
O(f(n)) if f(n) = Ω(n^log_b(a + ε)) for some ε > 0

Applying the master theorem to quicksort, we have:

- a = 2, since we recursively call quicksort on two subarrays.
- b = 2, since we split the input array in half.
- f(n) = O(n), since the partitioning process takes linear time.

Using these values, we can see that log_b(a) = log_2(2) = 1, and f(n) = O(n) = O(n^1), so we are in the second case of the master theorem.

Therefore, the time complexity of quicksort is O(n log n) in the average and best cases. In the worst case, the partitioning process takes O(n^2) time, so f(n) = Ω(n^2) = Ω(n^1+ε) for some ε > 0, which means that the time complexity of quicksort is O(n^2) in the worst case.

In summary, the master theorem can be used to analyze the time complexity of quicksort, and it shows that the time complexity is O(n log n) in the average and best cases and O(n^2) in the worst case.



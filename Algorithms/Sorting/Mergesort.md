## Mergesort 

Table of Contents 
- Pseudocode
- Explanation
- Time Complexity
- Master Theorem
- Master Theorem for Mergesort 
- Proof by Mathematical Induction 
- Iterative Pseudocode
- References 

---
### Pseudocode 

> Can pseudocode for mergesort be made concise by using recursion?

Expressing pseudocode for merge sort in recursive form makes it more concise (see more expansive iterative further down the page). 

```java
merge_sort(arr):
    if length(arr) > 1:
        middle = length(arr) / 2

        // Divide the array into two halves and sort them recursively
        left_arr = merge_sort(arr[0:middle])
        right_arr = merge_sort(arr[middle:length(arr)])

        // Merge the sorted halves
        arr = merge(left_arr, right_arr)

    return arr

merge(left_arr, right_arr):
    merged_arr = []

    // Merge the left and right subarrays into a single sorted array
    while length(left_arr) > 0 and length(right_arr) > 0:
        if left_arr[0] <= right_arr[0]:
            merged_arr.append(left_arr[0])
            left_arr = left_arr[1:]
        else:
            merged_arr.append(right_arr[0])
            right_arr = right_arr[1:]

    // Add any remaining elements from the left or right subarray
    if length(left_arr) > 0:
        merged_arr.extend(left_arr)
    else:
        merged_arr.extend(right_arr)

    return merged_arr
```

The 'mergesort' function recursively divides into the input array into two halves until each half contains only one element. 

The `merge` function is used to merge the sorted subarrays back together into a single sorted array. 

The `append` method is used to add elements to the `merged_arr` list.

The `extend` method is used to add any remaining elements from the left or right subarray. 

--- 
### Explanation 

> Please describe the essential idea behind the recursive version of mergesort above in plain English.

Mergesort is a divide-and-conquer algorithm that recursively divides the input array into two halves, sorts each half, and then merges the sorted halves back together. 

Or, The merge sort algorithm applies the divide-and-conquer approach by first recursing downwards through successive recursive function calls dividing the array, when it reaches the base case and these function calls end, the array is in effective sorted order, and then recursing upwards returning from these function calls the divided arrays are merged back into the whole, now sorted, array.  

Or, first dividing the original array into two halves, and then it recursively sorting each half of the array. This process continues until each subarray contains only one element, at which point the array has been sorted.

Once each subarray is sorted, the algorithm merges them back together by comparing the first element of each subarray and selecting the smaller one to be added to the final sorted array. This process continues until all elements have been merged into a single sorted array.

---
### Time Complexity 

> Please derive the best, average and worst case time complexity for mergesort

The time complexity of mergesort can be analyzed in terms of the number of comparisons or the number of swaps performed during the algorithm. 

- **Best case time complexity:** when the input array is already sorted, the algorithm only needs to compare adjacent elements and won't need to perform any swaps, resulting in O(n log n) time complexity.

- **Average case time complexity:** when the input array is unsorted, each element needs to be compared to every other element at least once, so the time complexity is O(n log n). 

- **Worst case time complexity:** when the input array is sorted in reverse order, each element needs to be compared to every other element, and every pair of adjacent elements needs to be swapped with a time complexity of once again O(n log n).

The best, average, and worst case time complexity of mergesort is O(n log n).

In practice, the worst case rarely occurs, because the input array is usually not sorted in any particular order, so the average case time complexity of O(n log n) is a good estimate of the actual runtime of mergesort.

---
### Master Theorem 

> Are you familiar with the 'master theorem' for deriving recurrence relations for recursively-defined algorithms?

The master theorem is a powerful tool for analyzing the time complexity of algorithms that have a recurrence relation of the form:

T(n) = aT(n/b) + f(n)

where T(n) represents the running time of the algorithm on an input of size n, a is the number of subproblems at each level of recursion, b is the size of the subproblems relative to the original problem size, and f(n) is the cost of dividing the problem and combining the solutions.

The master theorem provides a framework for analyzing the time complexity of algorithms with such recurrence relations by dividing them into three cases, depending on the relative values of a, b, and f(n). The three cases are: 

1. If f(n) = $O(n^(log_b a - ε))$ for some constant ε > 0, then $T(n) = Θ(n^(log_b a))$. 
2. If f(n) = Θ(n^(log_b a)), then T(n) = Θ(n^(log_b a) log n). 
3. If f(n) = Ω(n^(log_b a + ε)) for some constant ε > 0, and if a*f(n/b) ≤ c*f(n) for some constant c < 1 and all sufficiently large n, then T(n) = Θ(f(n)).

The master theorem is a useful tool for analyzing the time complexity of many common recursive algorithms, such as binary search, mergesort, and quicksort. However, it is not always applicable, and there are many cases where a more nuanced analysis is required. In such cases, it may be necessary to use other techniques, such as substitution, iteration, or the method of guessing and verifying.

---
### Master Theorem for Mergesort 

> Please explain the time complexity of mergesort in terms of the master theorem. 

The time complexity of mergesort can be expressed by the recurrence relation:

T(n) = 2T(n/2) + Θ(n)

Here, a = 2 (since we are dividing the problem into two subproblems of equal size), b = 2 (since the subproblems are half the size of the original problem), and f(n) = Θ(n) (since the cost of dividing the problem and combining the solutions is proportional to the size of the input).

To analyze the time complexity of mergesort using the master theorem, we need to compare the value of f(n) to n^(log_b a). In this case, we have:

n^(log_b a) = n^(log_2 2) = n

So, we need to compare f(n) to n. Since f(n) is Θ(n), we have:

f(n) = Θ(n) = n^(log_b a)

This means that we are in case 2 of the master theorem, which tells us that the time complexity of mergesort is:

T(n) = Θ(n log n)

In other words, the time complexity of mergesort is proportional to n times the logarithm of n. This is a very efficient algorithm for sorting, and it is one of the most widely used sorting algorithms due to its excellent performance in practice.

---
### Proof By Mathematical Induction 

> Could you show that rigorously by using mathematical induction for the worst case time complexity ?

We can use mathematical induction to prove that the worst case time complexity of mergesort is O(n log n).

First, let's define T(n) as the worst case time complexity of mergesort on an input array of size n. We want to show that T(n) <= cn log n for some constant c and all n >= 1, where the inequality holds for sufficiently large n.

**Base case:**
For n = 1, the input array is already sorted and the algorithm terminates immediately. Therefore, T(1) = 0. We can choose c = 1 for the base case, since 0 <= c log 1.

**Inductive hypothesis:**
Assume that for all k < n, T(k) <= ck log k, where c is a constant.

**Inductive step:**
Let's consider the worst case scenario for an input array of size n, which is when the input array is sorted in reverse order. In this case, mergesort performs the following steps:
1. Divide the input array into two halves of size n/2.
2. Recursively sort each half, which takes T(n/2) time.
3. Merge the two sorted halves into a single sorted array, which takes O(n) time.

The total worst case time complexity T(n) can be expressed as T(n) = 2T(n/2) + O(n), according to the recurrence relation of mergesort.

Using the inductive hypothesis, we can substitute T(n/2) with c(n/2) log(n/2), so T(n) = 2c(n/2) log(n/2) + O(n).

Next, we can simplify the expression for T(n) using the logarithmic identity log(ab) = log a + log b:

T(n) = c(n log n - n log 2) + O(n)
T(n) = cn log n - cn + O(n)

Since O(n) is an upper bound on the worst case time complexity, we can choose a constant d such that O(n) <= dn for all n >= 1. 
Therefore, T(n) <= cn log n - cn + dn

To complete the proof, we need to choose a constant c such that cn log n - cn + dn <= cn log n for all n >= 1. This is equivalent to cn log n - cn + dn - cn log n <= 0, which simplifies to -cn + dn <= 0, or c >= d.

Therefore, we can choose c = 2d to obtain:
T(n) <= cn log n - cn + dn
T(n) <= 2dn log n - 2dn + dn
T(n) <= 2dn log n

Thus, we have shown that T(n) <= cn log n for some constant c and all n >= 1, where the inequality holds for sufficiently large n. This completes the proof that the worst case time complexity of mergesort is O(n log n).

---
### Iterative Pseudocode

> Please provide me some iterative (non-recursive) pseudocode for mergesort

Here's some pseudocode for the merge sort algorithm:

```less

merge_sort(arr, left, right)
    if left < right:
        middle = (left + right) / 2

        // Sort the left half of the array recursively
        merge_sort(arr, left, middle)

        // Sort the right half of the array recursively
        merge_sort(arr, middle + 1, right)

        // Merge the sorted halves
        merge(arr, left, middle, right)

merge(arr, left, middle, right)
    // Find the sizes of the left and right subarrays
    n1 = middle - left + 1
    n2 = right - middle

    // Create temporary arrays for the left and right subarrays
    left_arr[n1]
    right_arr[n2]

    // Copy the data from the original array to the temporary arrays
    for i = 0 to n1
        left_arr[i] = arr[left + i]
    for j = 0 to n2
        right_arr[j] = arr[middle + 1 + j]

    // Merge the left and right subarrays back into the original array
    i = 0
    j = 0
    k = left
    while i < n1 and j < n2
        if left_arr[i] <= right_arr[j]
            arr[k] = left_arr[i]
            i = i + 1
        else
            arr[k] = right_arr[j]
            j = j + 1
        k = k + 1

    // Copy the remaining elements of the left subarray, if any
    while i < n1
        arr[k] = left_arr[i]
        i = i + 1
        k = k + 1

    // Copy the remaining elements of the right subarray, if any
    while j < n2
        arr[k] = right_arr[j]
        j = j + 1
        k = k + 1
```

In this pseudocode, `arr` is the array to be sorted, `left` and `right` are the indices of the first and last elements of the subarray to be sorted, and `middle` is the index of the middle element of the subarray. The `merge_sort` function recursively sorts the left and right halves of the array, and then merges them back together using the `merge` function. The `merge` function takes in the temporary arrays `left_arr` and `right_arr`, which are used to hold the left and right subarrays while they are being merged.



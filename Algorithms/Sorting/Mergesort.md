## Mergesort 


---
> Please derive the best, average and worst case time complexity for mergesort

Mergesort is a divide-and-conquer algorithm that recursively divides the input array into two halves, sorts each half, and then merges the sorted halves back together. The time complexity of mergesort can be analyzed in terms of the number of comparisons or the number of swaps performed during the algorithm. 
- Best case time complexity: The best case occurs when the input array is already sorted. In this case, the algorithm only needs to compare adjacent elements and won't need to perform any swaps. The time complexity for this case is O(n log n). 
- Average case time complexity: The average case occurs when the input array is unsorted and has no special structure. In this case, each element needs to be compared to every other element at least once, so the time complexity is O(n log n). 
- Worst case time complexity: The worst case occurs when the input array is sorted in reverse order. In this case, each element needs to be compared to every other element, and every pair of adjacent elements needs to be swapped. This results in a time complexity of O(n log n).

In summary, the best, average, and worst case time complexity of mergesort is O(n log n) for all cases.

---
> Could you show that rigorously by using mathematical induction for the worst case time complexity >

We can use mathematical induction to prove that the worst case time complexity of mergesort is O(n log n).

First, let's define T(n) as the worst case time complexity of mergesort on an input array of size n. We want to show that T(n) <= cn log n for some constant c and all n >= 1, where the inequality holds for sufficiently large n.

Base case:
For n = 1, the input array is already sorted and the algorithm terminates immediately. Therefore, T(1) = 0. We can choose c = 1 for the base case, since 0 <= c log 1.

Inductive hypothesis:
Assume that for all k < n, T(k) <= ck log k, where c is a constant.

Inductive step:
Let's consider the worst case scenario for an input array of size n, which is when the input array is sorted in reverse order. In this case, mergesort performs the following steps:
1. Divide the input array into two halves of size n/2.
2. Recursively sort each half, which takes T(n/2) time.
3. Merge the two sorted halves into a single sorted array, which takes O(n) time.

The total worst case time complexity T(n) can be expressed as T(n) = 2T(n/2) + O(n), according to the recurrence relation of mergesort.

Using the inductive hypothesis, we can substitute T(n/2) with c(n/2) log(n/2), so T(n) = 2c(n/2) log(n/2) + O(n).

Next, we can simplify the expression for T(n) using the logarithmic identity log(ab) = log a + log b:
T(n) = c(n log n - n log 2) + O(n)
T(n) = cn log n - cn + O(n)

Since O(n) is an upper bound on the worst case time complexity, we can choose a constant d such that O(n) <= dn for all n >= 1. Therefore,
T(n) <= cn log n - cn + dn

To complete the proof, we need to choose a constant c such that cn log n - cn + dn <= cn log n for all n >= 1. This is equivalent to cn log n - cn + dn - cn log n <= 0, which simplifies to -cn + dn <= 0, or c >= d.

Therefore, we can choose c = 2d to obtain:
T(n) <= cn log n - cn + dn
T(n) <= 2dn log n - 2dn + dn
T(n) <= 2dn log n

Thus, we have shown that T(n) <= cn log n for some constant c and all n >= 1, where the inequality holds for sufficiently large n. This completes the proof that the worst case time complexity of mergesort is O(n log n).

--- 
> Is the best case time complexity very different ?

The best case time complexity of mergesort is significantly better than the worst case time complexity. In the best case scenario, when the input array is already sorted, mergesort takes advantage of this fact and skips the merging step. Instead, it simply returns the input array without any further processing. As a result, the best case time complexity of mergesort is O(n log n), which is the same as the average case time complexity.

In contrast, the worst case time complexity of mergesort occurs when the input array is sorted in reverse order. In this case, mergesort has to perform the maximum number of comparisons and swaps, resulting in a time complexity of O(n log n).

It is worth noting that although the best case time complexity is significantly better than the worst case, in practice, the worst case rarely occurs. This is because in most real-world scenarios, the input array is not sorted in any particular order, and the average case time complexity of O(n log n) is a good estimate of the actual runtime of mergesort.

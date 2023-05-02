## Mergesort 


---
> Please derive the best, average and worst case time complexity for mergesort

Mergesort is a divide-and-conquer algorithm that recursively divides the input array into two halves, sorts each half, and then merges the sorted halves back together. The time complexity of mergesort can be analyzed in terms of the number of comparisons or the number of swaps performed during the algorithm. 
- Best case time complexity: The best case occurs when the input array is already sorted. In this case, the algorithm only needs to compare adjacent elements and won't need to perform any swaps. The time complexity for this case is O(n log n). 
- Average case time complexity: The average case occurs when the input array is unsorted and has no special structure. In this case, each element needs to be compared to every other element at least once, so the time complexity is O(n log n). 
- Worst case time complexity: The worst case occurs when the input array is sorted in reverse order. In this case, each element needs to be compared to every other element, and every pair of adjacent elements needs to be swapped. This results in a time complexity of O(n log n).

In summary, the best, average, and worst case time complexity of mergesort is O(n log n) for all cases.

--- 
> Is the best case time complexity very different ?

The best case time complexity of mergesort is significantly better than the worst case time complexity. In the best case scenario, when the input array is already sorted, mergesort takes advantage of this fact and skips the merging step. Instead, it simply returns the input array without any further processing. As a result, the best case time complexity of mergesort is O(n log n), which is the same as the average case time complexity.

In contrast, the worst case time complexity of mergesort occurs when the input array is sorted in reverse order. In this case, mergesort has to perform the maximum number of comparisons and swaps, resulting in a time complexity of O(n log n).

It is worth noting that although the best case time complexity is significantly better than the worst case, in practice, the worst case rarely occurs. This is because in most real-world scenarios, the input array is not sorted in any particular order, and the average case time complexity of O(n log n) is a good estimate of the actual runtime of mergesort.


---
> Are you familiar with the 'master theorem' for deriving recurrence relations for recursively-defined algorithms?

The master theorem is a powerful tool for analyzing the time complexity of algorithms that have a recurrence relation of the form:

T(n) = aT(n/b) + f(n)

where T(n) represents the running time of the algorithm on an input of size n, a is the number of subproblems at each level of recursion, b is the size of the subproblems relative to the original problem size, and f(n) is the cost of dividing the problem and combining the solutions.

The master theorem provides a framework for analyzing the time complexity of algorithms with such recurrence relations by dividing them into three cases, depending on the relative values of a, b, and f(n). The three cases are: 

1. If f(n) = O(n^(log_b a - ε)) for some constant ε > 0, then T(n) = Θ(n^(log_b a)). 
2. If f(n) = Θ(n^(log_b a)), then T(n) = Θ(n^(log_b a) log n). 
3. If f(n) = Ω(n^(log_b a + ε)) for some constant ε > 0, and if a*f(n/b) ≤ c*f(n) for some constant c < 1 and all sufficiently large n, then T(n) = Θ(f(n)).

The master theorem is a useful tool for analyzing the time complexity of many common recursive algorithms, such as binary search, mergesort, and quicksort. However, it is not always applicable, and there are many cases where a more nuanced analysis is required. In such cases, it may be necessary to use other techniques, such as substitution, iteration, or the method of guessing and verifying.

---
> Please explain the time complexity of mergesort with a recurrence relation of the form:

T(n) = aT(n/b) + f(n)

And please explain it in terms of the 'master theorem' in the form: 

"where T(n) represents the running time of the algorithm on an input of size n, a is the number of subproblems at each level of recursion, b is the size of the subproblems relative to the original problem size, and f(n) is the cost of dividing the problem and combining the solutions."


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

> Could you show that rigorously by using mathematical induction for the worst case time complexity >

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

## Maximum Subarray Problem 

> Implement both the brute-force and recursive algorithms for the maximum subarray problem on your own computer. 
> Use [this source file](https://my.uopeople.edu/pluginfile.php/1692892/mod_assign/intro/MSIT5214PA3SourceFile.py) 
> to run your program on your own computer. Answer the following question based on your output.

> What problem size n0 gives the crossover point at which the recursive algorithm beats the brute-force algorithm? 

> Test your program by setting NUM_ITERATIONS = 1000. Currently, the value is 10. You will replace 10 with 1000.  Keep in mind this code is written in Python. 

> Run the program five times and take screenshots of your output to show the crossover results. (you should have 5 screenshots). 
Paste the screenshots into the same document you used to answer the questions in this activity.

> At what size does the crossover occur?

> What do you notice when NUM_ITERATIONS changes from 10 to 1000?

---

> Are you familiar with the ' maximum subarray problem' ? 

Yes, it is a classic problem in computer science and algorithms that involves finding the contiguous subarray with the maximum sum within a given array of numbers. The problem can be stated as - **follows:** Given an array of integers, find a contiguous subarray with the largest sum. This problem has various applications in areas such as data analysis, finance, and computer vision. There are several algorithms that can be used to solve the maximum subarray problem, including Kadane's algorithm, brute force approach, and divide-and-conquer algorithms like the "Max Subarray Sum" algorithm.

---
> Could you provide some pseudocode for the  ' brute force approach' ? 

The brute force approach to solving the maximum subarray problem involves checking all possible contiguous subarrays within the given array and selecting the one with the maximum sum. Here's a pseudocode implementation for the brute force - **approach:**

```r
function findMaximumSubarray(arr):
    n = length of arr
    max_sum = MIN_INT      # Initialize max_sum to a very small negative value
    start_index = -1      # Initialize start_index to -1
    end_index = -1        # Initialize end_index to -1

    # Iterate over all possible subarray starting indices
    for i = 0 to n - - **1:**
        current_sum = 0   # Initialize current_sum to 0

        # Iterate over all possible subarray ending indices
        for j = i to n - - **1:**
            current_sum = current_sum + arr[j]  # Add current element to current_sum

            # Update max_sum, start_index, and end_index if current_sum is greater
            # than the current max_sum
            if current_sum > - **max_sum:**
                max_sum = current_sum
                start_index = i
                end_index = j

    # Return the maximum sum, start index, and end index
    return max_sum, start_index, end_index
```

Note that this brute force approach has a time complexity of O(n^2), where n is the size of the input array. It involves nested loops, making it inefficient for large arrays. There are more efficient algorithms, such as Kadane's algorithm, which have a time complexity of O(n) and are commonly used to solve the maximum subarray problem.

---
>  Are the familiar with the version of the max subarray problem that finds the crossing maximum sub-array ? 

Yes, this variation of the problem is where the maximum subarray is allowed to span across the midpoint of the input array. 
It requires finding a subarray that crosses the midpoint and has the maximum sum compared to other subarrays that may not cross the midpoint.

Here's a pseudocode implementation of the "crossing maximum subarray" using a divide-and-conquer - approach:

```r
function findCrossingMaximumSubarray(arr, low, mid, high):
    left_sum = MIN_INT      # Initialize left_sum to a very small negative value
    right_sum = MIN_INT     # Initialize right_sum to a very small negative value
    max_left = -1           # Initialize max_left to -1
    max_right = -1          # Initialize max_right to -1
    sum = 0                 # Initialize sum to 0

    # Find the maximum sum in the left subarray that ends at mid
    for i = mid to low step -- **1:**
        sum = sum + arr[i]
        if sum > - **left_sum:**
            left_sum = sum
            max_left = i

    sum = 0                 # Reset sum to 0

    # Find the maximum sum in the right subarray that starts at mid + 1
    for j = mid + 1 to - **high:**
        sum = sum + arr[j]
        if sum > - **right_sum:**
            right_sum = sum
            max_right = j

    # Return the indices of the maximum subarray that crosses the midpoint
    return max_left, max_right, left_sum + right_sum

function findMaximumSubarray(arr, low, high):
    if low == - **high:**
        # Base - **case:** if array has only one element, return that element as the maximum subarray
        return low, high, arr[low]
    - **else:**
        mid = floor((low + high) / 2)

        # Recursively find the maximum subarray in the left subarray
        left_low, left_high, left_sum = findMaximumSubarray(arr, low, mid)

        # Recursively find the maximum subarray in the right subarray
        right_low, right_high, right_sum = findMaximumSubarray(arr, mid + 1, high)

        # Find the maximum subarray that crosses the midpoint
        cross_low, cross_high, cross_sum = findCrossingMaximumSubarray(arr, low, mid, high)

        # Compare the three maximum subarrays and return the one with the highest sum
        if left_sum >= right_sum and left_sum >= - **cross_sum:**
            return left_low, left_high, left_sum
        elif right_sum >= left_sum and right_sum >= - **cross_sum:**
            return right_low, right_high, right_sum
        - **else:**
            return cross_low, cross_high, cross_sum
```

This divide-and-conquer approach has a time complexity of O(n log n), where n is the size of the input array, 
and it can find the maximum subarray that crosses the midpoint efficiently.

---
> How do you prove this   "crossing maximum subarray" version is  O(n log n) ? 

The time complexity of the "crossing maximum subarray" version of the maximum subarray problem, 
as implemented using a divide-and-conquer approach, can be proven to be O(n log n) through a recurrence relation analysis.

In the divide-and-conquer approach, the input array is divided into two halves at each level of the recursion, 
and then the maximum subarray is found in the left subarray, right subarray, and the crossing subarray (subarray that crosses the midpoint). 
The maximum of these three subarrays is returned as the final maximum subarray.

At each level of the recursion, the size of the problem (i.e., the size of the input array) is halved, 
resulting in log n levels of recursion, where n is the size of the input array. 
This is because the array is divided into halves at each level until the base case of a single element is reached.

At each level of the recursion, finding the maximum subarray that crosses the midpoint 
(i.e., the findCrossingMaximumSubarray function) takes O(n) time in the worst case. 
This is because it involves iterating over the left and right subarrays 
to find the maximum sum that crosses the midpoint, and each iteration takes O(n) time in the worst case.

Therefore, the total time complexity of the "crossing maximum subarray" version 
using a divide-and-conquer approach is given by the product of the number of levels of recursion (log n) 
and the time taken to find the crossing maximum subarray at each level (O(n)):

```scss
Time complexity = O(n) * O(log n) = O(n log n)
```

Hence, the "crossing maximum subarray" version of the maximum subarray problem, as implemented using a divide-and-conquer approach, has a time complexity of O(n log n).

---
> Could you give a describe the basic idea of this  "crossing maximum subarray"  version ?  

The "crossing maximum subarray" version of the maximum subarray problem is a variation where the maximum subarray is allowed to span across the midpoint of the input array. The basic idea of this version is to divide the problem into three - **parts:** finding the maximum subarray in the left subarray, finding the maximum subarray in the right subarray, and finding the maximum subarray that crosses the midpoint.

The basic idea of the "crossing maximum subarray" version using a divide-and-conquer approach is as - **follows:**

- Divide the input array into two halves at the midpoint.

- Recursively find the maximum subarray in the left subarray and the right subarray by making recursive calls to the same function.

- Find the maximum subarray that crosses the midpoint. This can be done by iterating from the midpoint towards the beginning of the left subarray to find the maximum sum of a subarray that ends at the midpoint, and then iterating from the midpoint + 1 towards the end of the right subarray to find the maximum sum of a subarray that starts at the midpoint + 1. The sum of these two maximum subarrays is the maximum subarray that crosses the midpoint.

- Compare the three maximum subarrays found in steps 2 and 3, and return the one with the highest sum as the final maximum subarray.

The key insight in this approach is that the maximum subarray can either be entirely in the left subarray, entirely in the right subarray, or it can cross the midpoint. By finding the maximum subarray that crosses the midpoint, we consider all possible subarrays that span across the midpoint and ensure that the final maximum subarray is found accurately. This approach effectively handles cases where the maximum subarray spans across the midpoint and has a time complexity of O(n log n), making it an efficient solution to the "crossing maximum subarray" version of the maximum subarray problem.

---
> Could you provide a simple example with actual data? 

Let's consider the following example input - **array:**

**Input - array:** [-2, 3, 5, -1, 2, -4, 7, -3, 2]

We will use a divide-and-conquer approach to find the "crossing maximum subarray" in this input array.

- **Divide:** We start by dividing the input array into two halves at the midpoint, which is index 4.

```less
Left - **subarray:** [-2, 3, 5, -1]
Right - **subarray:** [2, -4, 7, -3, 2]
```
- **Recurse:** We recursively find the maximum subarray in the left subarray and the right subarray by making recursive calls to the same function.

For the left - **subarray:**

```less
Left - **subarray:** [-2, 3, 5, -1]
Maximum subarray in left - **subarray:** [3, 5]
```

For the right - **subarray:**

```less
Right - **subarray:** [2, -4, 7, -3, 2]
Maximum subarray in right - **subarray:** [2, -4, 7]
```

Find crossing maximum - **subarray:** Next, we find the maximum subarray that crosses the midpoint. We iterate from the midpoint towards the beginning of the left subarray to find the maximum sum of a subarray that ends at the midpoint, and then iterate from the midpoint + 1 towards the end of the right subarray to find the maximum sum of a subarray that starts at the midpoint + 1.

```less
Left - **subarray:** [-2, 3, 5, -1]
Maximum subarray ending at - **midpoint:** [3, 5]

Right - **subarray:** [2, -4, 7, -3, 2]
Maximum subarray starting at midpoint + - **1:** [2, -4, 7]

Crossing maximum - **subarray:** [3, 5, -1, 2, -4, 7]

- **Compare:** Finally, we compare the three maximum subarrays found in steps 2 and 3, and return the one with the highest sum as the final maximum subarray.

```less
Maximum subarray in left - **subarray:** [3, 5]
Maximum subarray in right - **subarray:** [2, -4, 7]
Crossing maximum - **subarray:** [3, 5, -1, 2, -4, 7]

Final maximum - **subarray:** [3, 5, -1, 2, -4, 7]

So, the "crossing maximum subarray" in the input array [-2, 3, 5, -1, 2, -4, 7, -3, 2] is [3, 5, -1, 2, -4, 7], with a sum of 12.

---
> Could you re-do this for the brute force approach above and compare ? 

Let's consider the same input array as - **before:** [-2, 3, 5, -1, 2, -4, 7, -3, 2]. We'll use the brute force approach to find the maximum subarray.

**Brute Force - Approach:**

Initialize max_sum to be the smallest possible integer value (e.g., -infinity), and set start_index and end_index to -1.

Iterate through all possible subarrays in the input array and calculate the sum of each subarray.

```yaml
Input - **array:** [-2, 3, 5, -1, 2, -4, 7, -3, 2]

All subarrays and their - **sums:**
- **Subarray:** [-2], - **Sum:** -2
- **Subarray:** [-2, 3], - **Sum:** 1
- **Subarray:** [-2, 3, 5], - **Sum:** 6
- **Subarray:** [-2, 3, 5, -1], - **Sum:** 5
- **Subarray:** [-2, 3, 5, -1, 2], - **Sum:** 7
- **Subarray:** [-2, 3, 5, -1, 2, -4], - **Sum:** 3
- **Subarray:** [-2, 3, 5, -1, 2, -4, 7], - **Sum:** 10
- **Subarray:** [-2, 3, 5, -1, 2, -4, 7, -3], - **Sum:** 7
- **Subarray:** [-2, 3, 5, -1, 2, -4, 7, -3, 2], - **Sum:** 9
- **Subarray:** [3], - **Sum:** 3
- **Subarray:** [3, 5], - **Sum:** 8
- **Subarray:** [3, 5, -1], - **Sum:** 7
- **Subarray:** [3, 5, -1, 2], - **Sum:** 9
- **Subarray:** [3, 5, -1, 2, -4], - **Sum:** 5
- **Subarray:** [3, 5, -1, 2, -4, 7], - **Sum:** 12
- **Subarray:** [3, 5, -1, 2, -4, 7, -3], - **Sum:** 9
- **Subarray:** [3, 5, -1, 2, -4, 7, -3, 2], - **Sum:** 11
- **Subarray:** [5], - **Sum:** 5
- **Subarray:** [5, -1], - **Sum:** 4
- **Subarray:** [5, -1, 2], - **Sum:** 6
- **Subarray:** [5, -1, 2, -4], - **Sum:** 2
- **Subarray:** [5, -1, 2, -4, 7], - **Sum:** 9
- **Subarray:** [5, -1, 2, -4, 7, -3], - **Sum:** 6
- **Subarray:** [5, -1, 2, -4, 7, -3, 2], - **Sum:** 8
- **Subarray:** [-1], - **Sum:** -1
- **Subarray:** [-1, 2], - **Sum:** 1
- **Subarray:** [-1, 2, -4], - **Sum:** -3
- **Subarray:** [-1, 2, -4, 7], - **Sum:** 4
- **Subarray:** [-1, 2, -4, 7, -3], - **Sum:** 1
- **Subarray:** [-1, 2, -4, 7,
```

---
> Could you explain the basic idea behind the brute force approach? 

The brute force approach for solving the maximum subarray problem involves trying out all possible subarrays of the given input array and calculating the sum of each subarray. The basic idea is to iterate through the array and consider each element as the starting point of a subarray, and then calculate the sum of all subarrays that start from that element, and keep track of the maximum sum encountered so far. This process is repeated for each element in the array, resulting in a total of n*(n+1)/2 subarrays to be considered, where n is the length of the input array.

The brute force approach exhaustively considers all possible subarrays, but it has a time complexity of O(n^2), which can be inefficient for large input arrays. This is because for each element in the array, we need to iterate through the remaining elements to calculate the sum of subarrays, resulting in a quadratic time complexity. Despite its simplicity, the brute force approach is not efficient for solving the maximum subarray problem for large input arrays. However, it can be useful for small input arrays or as a reference solution for validating more efficient algorithms.

---

**REFERENCES**

- Abhimanyu_97. (2021, November 10). Maximum sum subarray using divide and conquer | Set 2. GeeksforGeeks. Retrieved June 03, 2022, from https://www.geeksforgeeks.org/maximum-sum-subarray-using-divide-and-conquer-set-2/?ref=rp

- Lekberg J. (2020, September 12). Solving the maximum subarray in Python. Johnlekberg Blog. https://johnlekberg.com/blog/2020-09-12-max-sum.html

- Maximum subarray sum using divide and conquer algorithm. (2021, October 18). GeeksforGeeks. Retrieved June 03, 2022, from https://www.geeksforgeeks.org/maximum-subarray-sum-using-divide-and-conquer-algorithm/ 





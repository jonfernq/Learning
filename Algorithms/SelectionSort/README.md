## Selection Sort 

### Table of Contents

* [Specification](#section1)
* [Example](#section2)
* [Example: Separate Arrays](#section3)
* [Example Code With Print Statements](#section4)

### Specification

Selection sort is a simple sorting algorithm that works by repeatedly finding the minimum element 
from the unsorted part of the array and putting it at the beginning of the array.

The algorithm works by dividing the input array into two parts: the sorted part 
and the unsorted part. Initially, the sorted part is empty and the unsorted part is the entire array. 
In each iteration, the algorithm finds the smallest element in the unsorted part 
and swaps it with the first element of the unsorted part, thereby expanding the sorted part by one element.

The time complexity of the selection sort algorithm is O(n^2), where n is the number of elements in the array. Selection sort is not very efficient for large arrays, but it is simple and easy to implement.

---
### Example 

> Could you provide an example of sorting an actual array with selection sort, performing the operations mentioned above step-by-step ? 

Sure, I can give you an example of sorting an array using the selection sort algorithm.

Let's say we have the following unsorted array of integers:

```csharp
[64, 25, 12, 22, 11]
```

To sort this array using selection sort, we would follow these steps:

1. Set the first element (64) as the minimum element.

2. Compare the minimum element with the next element (25). Since 25 is less than 64, set it as the new minimum element.

3. Continue comparing the minimum element (25) with the remaining elements of the array. If a smaller element is found, set it as the new minimum element.

4. Once all the elements have been compared, swap the minimum element (11) with the first element of the unsorted part of the array (64).

5. Repeat the above steps for the remaining unsorted part of the array (excluding the first element).

Here are the step-by-step operations to sort the above array using selection sort:

```csharp
Step 1:
[64, 25, 12, 22, 11]  // original array
[11, 25, 12, 22, 64]  // minimum element (11) is swapped with first element (64)

Step 2:
[11, 25, 12, 22, 64]  // minimum element is 12
[11, 12, 25, 22, 64]  // minimum element (12) is swapped with second element (25)

Step 3:
[11, 12, 25, 22, 64]  // minimum element is still 22
[11, 12, 22, 25, 64]  // minimum element (22) is swapped with fourth element (25)

Step 4:
[11, 12, 22, 25, 64]  // minimum element is already the first element

Step 5:
[11, 12, 22, 25, 64]  // entire array is now sorted
```

As you can see, the selection sort algorithm first finds the minimum element in the unsorted part of the array and swaps it with the first element of the unsorted part. It then repeats this process for the remaining unsorted part of the array until the entire array is sorted in ascending order.

---
### Example: Separate Arrays 

> Here's an example of how the selection sort algorithm can be applied to two separate arrays, one for the sorted part and the other for the unsorted part:

```python
// unsorted array
unsorted_array = [64, 25, 12, 22, 11]

// sorted array (initialized as empty)
sorted_array = []

// repeat until the unsorted array is empty
while len(unsorted_array) > 0:
    # find the minimum element in the unsorted array
    min_idx = 0
    for i in range(1, len(unsorted_array)):
        if unsorted_array[i] < unsorted_array[min_idx]:
            min_idx = i
    
    # remove the minimum element from the unsorted array and add it to the sorted array
    sorted_array.append(unsorted_array.pop(min_idx))

print(sorted_array)
```

In this implementation, we start with an unsorted array and an empty sorted array. 
We repeatedly find the minimum element in the unsorted array and remove it from the unsorted array, 
adding it to the sorted array. We repeat this process until the unsorted array is empty.

Here are the step-by-step operations to sort the above array using selection sort:

```less
Step 1:
unsorted_array = [64, 25, 12, 22, 11]  // original unsorted array
sorted_array = []  // empty sorted array

min_idx = 4  // minimum element is 11
unsorted_array = [64, 25, 12, 22]  // 11 is removed from the unsorted array
sorted_array = [11]  // 11 is added to the sorted array

Step 2:
unsorted_array = [64, 25, 12, 22]  // remaining unsorted array
sorted_array = [11]  // sorted array

min_idx = 2  // minimum element is 12
unsorted_array = [64, 25, 22]  // 12 is removed from the unsorted array
sorted_array = [11, 12]  // 12 is added to the sorted array

Step 3:
unsorted_array = [64, 25, 22]  // remaining unsorted array
sorted_array = [11, 12]  // sorted array

min_idx = 2  // minimum element is 22
unsorted_array = [64, 25]  // 22 is removed from the unsorted array
sorted_array = [11, 12, 22]  // 22 is added to the sorted array

Step 4:
unsorted_array = [64, 25]  // remaining unsorted array
sorted_array = [11, 12, 22]  // sorted array

min_idx = 1  // minimum element is 25
unsorted_array = [64]  // 25 is removed from the unsorted array
sorted_array = [11, 12, 22, 25]  // 25 is added to the sorted array

Step 5:
unsorted_array = [64]  // remaining unsorted array
sorted_array = [11, 12, 22, 25]  // sorted array

min_idx = 0  // minimum element is 64
unsorted_array = []  // 64 is removed from the unsorted array
sorted_array = [11, 12, 22, 25, 64]  // 64 is added to the sorted array

Step 6:
unsorted_array = []  // remaining unsorted array is empty
sorted_array = [11, 12, 22,
```

---
### Example Code With Print Statements 

> Here's the same example code with print statements added to show the step-by-step operations:

```python
# unsorted array
unsorted_array = [64, 25, 12, 22, 11]

# sorted array (initialized as empty)
sorted_array = []

# repeat until the unsorted array is empty
while len(unsorted_array) > 0:
    # find the minimum element in the unsorted array
    min_idx = 0
    for i in range(1, len(unsorted_array)):
        if unsorted_array[i] < unsorted_array[min_idx]:
            min_idx = i
    
    # remove the minimum element from the unsorted array and add it to the sorted array
    min_val = unsorted_array.pop(min_idx)
    sorted_array.append(min_val)
    
    # print the current state of the arrays
    print("unsorted array: ", unsorted_array)
    print("sorted array: ", sorted_array)
    print("-------------------")

# print the final sorted array
print("final sorted array: ", sorted_array)
```

And here's the corresponding output with the step-by-step operations:

```markdown
unsorted array:  [25, 12, 22, 11, 64]
sorted array:  [11]
-------------------
unsorted array:  [25, 12, 22, 64]
sorted array:  [11, 12]
-------------------
unsorted array:  [25, 22, 64]
sorted array:  [11, 12, 22]
-------------------
unsorted array:  [25, 64]
sorted array:  [11, 12, 22, 25]
-------------------
unsorted array:  [64]
sorted array:  [11, 12, 22, 25, 64]
-------------------
final sorted array:  [11, 12, 22, 25, 64]
```
Each iteration of the while loop finds the minimum element in the unsorted array, removes it, and adds it to the sorted array. The current state of the arrays is printed after each iteration to show the step-by-step operations of the selection sort algorithm.




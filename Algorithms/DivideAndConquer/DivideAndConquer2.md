## Divide and Conquer 

> Explain the 'divide and conquer' approach.

'Divide and conquer' ia a problem-solving approach that divides a complex problem into easier sub-problems.
and then combines the solutions of these problems into a solution to the complex problem: 

- **Divide:** divide complex prroblem into easier smaller sub-problems. 

- **Conquer:** Solve sub-problems independently with same algorithm or approach, recursively or iteratively. 

- **Combine:** Combine solutions to sub-problems into solution to original complex problem. 

- **Base case:** A condition for terminating recursion or iteration, indicating when complex problem 
has been simplified enough to be solved without further division.

(Cormen 2022:76)

---
> Provide examples of the 'divide and conquer' approach

**1. Merge Sort:** Merge Sort is a sorting algorithm that uses the divide and conquer approach to sort an array of elements. It divides the array into two halves, recursively sorts the two halves, and then merges the sorted halves to obtain the final sorted array.

- Divide: Array to be sorted divided into two halves.

- Conquer: Recursively sort each successive smaller half until subproblems are solved trivially, namely when sub-array has only one or zero elements, which are already sorted by definition.

- Combine: Sorted sublists successively compared and combined in sorted orrder to form final sorted list.

**2. Quick Sort:** Selects pivot element from array, partitions array into two subarrays based on pivot, and recursively sorts the two subarrays. 

- Divide: Array to be sorted divided into two smaller sub-arrays based on a chosen pivot element. Elements in the original array are partitioned such that elements smaller than the pivot are placed in one sub-array, and elements larger than the pivot are placed in another sub-array. 

- Conquer: The smaller sublists created in the divide step (i.e., the sublists of elements smaller than the pivot and the sublists of elements larger than the pivot) are recursively sorted. Division is repeated until sub-arrays have only one or zero elements, which are already sorted by definition.

- Combine: Sorted sublists are re-combined to form final sorted list. Since the sorting is done in place by swapping elements within the original list, the combining step is not explicitly required. The pivot element is chosen and adjusted during the partitioning step to ensure that the elements smaller than the pivot are on one side, and the elements larger than the pivot are on the other side, effectively sorting the list.

**3. Binary Search:** Repeatedly divides the array into smaller halves until target element found or search space is exhausted.

**4. Fast Fourier Transform (FFT):** The FFT algorithm is a widely used algorithm in signal processing and other fields that uses the divide and conquer approach to efficiently compute the Discrete Fourier Transform (DFT) of a sequence of values.

---
> What are the applications of the divide and conquer approach? 

- **Sorting:** Merge Sort, Quick Sort, and Heap Sort.

- **Searching:** Binary Search finds position of target element in sorted array.

- **Matrix Operations:** Matrix multiplication, matrix exponentiation, and other matrix operations.

- **Computational Geometry:** finding the closest pair of points, convex hull, and line segment intersection.

- **Graph Algorithms:** Depth-First Search (DFS) and Breadth-First Search (BFS).

- **Numerical Computations:** fast Fourier transform (FFT).
 
- **Image Processing:** image filtering, edge detection, and image segmentation, process images by dividing into smaller subimages.

- **Parallel and Distributed Computing:** divide into smaller tasks solved independently by multiple processors.

---

> What are the pros and cons of the divide and conquer approach?

**Pros:**

- **Efficiency:** often leads to efficient algorithms, as it allows for the parallelization of tasks, reduces redundant computation, and minimizes the amount of data or work needed to be processed at each level of recursion. This can result in improved time complexity and reduced computational overhead.

- **Scalability:** The divide and conquer approach is makes it suitable for parallel and distributed computing environments, where multiple processors or nodes can work on subproblems concurrently, potentially leading to improved performance on large datasets.

- **Modularity:** promotes modularity in algorithm design, as it breaks down complex problems into smaller, more manageable subproblems. This can make the algorithm easier to understand, implement, and maintain.

- **Flexibility:** The divide and conquer approach is a general problem-solving technique that can be applied to a wide range of problem domains and applications. It can be used to solve problems of varying complexity and size, making it a versatile approach for problem solving.

**Cons:**

- **Overhead:** can sometimes introduce additional overhead, such as the cost of dividing the problem into smaller subproblems, combining the results, or managing the recursion. This can impact the overall performance of the algorithm, particularly for small problem sizes where the overhead may outweigh the benefits.

- **Complexity:** can introduce complexity into algorithm design, as it requires careful handling of the division, combination, and recursion steps. This can make the algorithm harder to implement, debug, and maintain, particularly for complex problems or when dealing with multiple levels of recursion.

- **Communication and Coordination:** In parallel and distributed computing environments, the divide and conquer approach may require communication and coordination among processors or nodes to exchange data or synchronize operations. This can introduce additional complexity and potential overhead, particularly in cases where communication or coordination is not efficient.

- **Problem Constraints:** may not be suitable for all types of problems or domains. Some problems may not have a natural way of being divided into smaller subproblems, or the division may not result in significant benefits in terms of efficiency or scalability. 

### REFERENCES: 

Cormen, T. H., Leiserson, C. E., Rivest, R. L., & Stein, C. (2022). *Introduction to Algorithms.** Fourth Edition. MIT Press. [Ebook]

Kleinberg, J., & Tardos, E. (2005). Algorithm Design. Pearson Education.

Sedgewick, R., & Wayne, K. (2011). *Algorithms.* Addison-Wesley Professional.

Dasgupta, S., Papadimitriou, C. H., & Vazirani, U. V. (2008). Algorithms. McGraw-Hill Education.

Goodrich, M. T., & Tamassia, R. (2015). Data Structures and Algorithms in Java. John Wiley & Sons.

Knuth, D. E. (1997). The Art of Computer Programming, Volume 1: Fundamental Algorithms. Addison-Wesley Professional.

Skiena, S. S. (2008). The Algorithm Design Manual. Springer.

Harel, D. (2012). Algorithmics: The Spirit of Computing. Addison-Wesley Professional.







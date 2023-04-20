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
> How is the divide and conquer approach useful in recursion to solve complex problems? 

The divide and conquer approach is often used in recursion to solve complex problems by breaking them down into smaller, more manageable subproblems. The recursive nature of the divide and conquer approach allows the problem to be divided into smaller instances of the same problem, which can be solved independently, and their results combined to obtain the final solution. This can simplify the problem-solving process and lead to efficient algorithms for complex problems.

Here's how the divide and conquer approach is typically used in recursion to solve complex problems:

- Divide: The problem is divided into smaller subproblems that are similar to the original problem. This can be achieved by breaking the problem into smaller instances of the same problem, often with reduced input size or complexity. The division step may involve partitioning the data, splitting a problem into smaller sub-problems, or recursively subdividing a problem into smaller parts.

- Conquer: Each smaller subproblem is solved independently using the same recursive approach. The base case of the recursion is typically defined, which represents the smallest subproblems that can be solved directly without further division. For base cases, the solution is usually straightforward and can be directly computed.

- Combine: The solutions of the smaller subproblems are combined to obtain the solution to the original problem. This step may involve merging, aggregating, or combining the results of the smaller subproblems to obtain the final solution to the original problem.

- Recurse: The divide, conquer, and combine steps are repeated recursively until the base case is reached and the final solution is obtained.

The divide and conquer approach is particularly useful in recursion as it allows for a systematic way to break down complex problems into smaller, more manageable subproblems. It can simplify the problem-solving process, reduce redundant computation, and lead to efficient algorithms for solving complex problems. By leveraging the recursive nature of the divide and conquer approach, it is possible to achieve elegant and optimized solutions for a wide range of complex problems.

---
> What is the role of the substitution method in recursion? 

The substitution method is a technique used in recursion to analyze the time complexity of recursive algorithms. It involves replacing the recursive function calls with their corresponding recursive function definitions and solving the resulting recurrence relation to determine the overall time complexity of the algorithm.

The substitution method typically involves the following steps:

- Writing down the recurrence relation: This involves expressing the time complexity of a recursive algorithm in terms of itself. For example, if the recurrence relation is T(n) = 2T(n/2) + f(n), where T(n) represents the time complexity of the algorithm for an input size of n, and f(n) represents the time taken by the non-recursive part of the algorithm, then the recurrence relation is written down as T(n) = 2T(n/2) + f(n).

- Making an educated guess: Based on the form of the recurrence relation, an educated guess is made for the solution of the recurrence relation. This guess is typically based on the pattern observed in the recursive function calls and the base case(s) of the recursion.

- Proving the guess using mathematical induction: The guessed solution is then proven using mathematical induction. The base case(s) are verified, and the inductive step is established to prove that the guessed solution holds for all values of the input size n.

- Deriving the final time complexity: Once the guessed solution is proven, it can be used to determine the overall time complexity of the recursive algorithm. The final time complexity is typically expressed in big-O notation, which provides an upper bound on the growth rate of the algorithm as the input size increases.

The substitution method is a useful technique for analyzing the time complexity of recursive algorithms, as it provides a systematic approach to derive the overall time complexity of the algorithm based on the recurrence relation. It helps in understanding the performance characteristics of recursive algorithms and in making informed decisions about algorithm design and optimization.

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

- **Numerical Computations:** fast Fourier transform (FFT), algorithms for multiplying large numbers (Karatsuba algorithm)
 
- **Image Processing:** image filtering, edge detection, and image segmentation, finding closest pair of points, processing of images by dividing into smaller subimages.

- **Compilers:**  top-down parsers (syntactic analysis of programming languages) 

- **Functional programming** functional programming features such as higher-order functions and recursion in languages such as Haskell, Lisp, and ML, are well-suited to implement divide and conquerin a conceptually simple fashion. In higher-order functions functions are passed as arguments to other functions. Recursion recursively solving smaller subproblems until they are small enough to be solved directly. Higher-order functions merge or concatenate results obtained from smaller subproblems in the combine step.

- **Database management:** Aggregate function* in SQL: "Aggregate functions present a bottleneck, because they potentially require having all input values at once. In distributed computing, it is desirable to divide such computations into smaller pieces, and distribute the work, usually computing in parallel, via a divide and conquer algorithm." 

- **Parallel and Distributed Computing:** divide into smaller tasks solved independently by multiple processors.

- **MapReduce** is a programming model and framework for processing large-scale data sets in a distributed and parallel manner. It is widely used for big data processing and is known for its scalability and fault tolerance.

The MapReduce framework orchestrates the distribution of data and computation across a cluster of machines. The "Map" step involves processing input data in parallel across multiple machines and generating intermediate key-value pairs as output. The intermediate results are then grouped by key and passed to the "Reduce" step. The "Reduce" step involves processing the grouped intermediate results in parallel across multiple machines and generating the final output, which is the solution to the original problem.

The divide and conquer approach is evident in the MapReduce model, as the processing of large data sets is divided into smaller chunks that can be processed independently in parallel, similar to the "divide" step in divide and conquer. The intermediate results from the "Map" step are combined and grouped by key, similar to the "combine" step in divide and conquer. Finally, the "Reduce" step involves processing the grouped intermediate results in parallel and generating the final output, similar to the "conquer" step in divide and conquer.

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

**Wikipedia:** Fork-join model; MapReduce; ivide-and-conquer algorithm; Aggregate function







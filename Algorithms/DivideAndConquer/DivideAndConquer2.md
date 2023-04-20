## Divide and Conquer 

> Explain the divide and conquer approach and provide examples. 

'Divide and conquer' is a problem-solving approach that divides a complex problem into easier sub-problems,
solves these sub-problems, and then combines the smaller solutions into a solution to the larger problem. 

**Steps:** 

- **Divide:** Recursively divide complex problem into easier smaller, sub-problems. 

- **Conquer:** Recursively solve sub-problems. 

- **Combine:** Combine smaller problem solutions into solution to original larger problem. 

(Cormen 2022:76)

**Examples:** 

**1. Merge Sort:** Divides array into two halves, recursively sorts the two halves, and then merges sorted halves into final sorted array.

- Divide: Array to be sorted divided into two halves.

- Conquer: Recursively sort successive smaller half arrays until these sub-problems are solved trivially, namely when sub-array has only one or zero elements, which are already sorted by definition.

- Combine: Sorted smaller sub-lists successively combined in sorted order to form final sorted list.

**2. Quick Sort:** Selects pivot element from array, partitions array into two sub-arrays based on pivot, and recursively sorts the two sub-arrays.

- Divide: Array to be sorted divided into two smaller sub-arrays based on a chosen pivot element. Elements in the original array are partitioned such that elements smaller than the pivot are placed in one sub-array, and elements larger than the pivot are placed in another sub-array. 

- Conquer: The smaller sublists created in the divide step are recursively sorted. Division is repeated until sub-arrays have only one or zero elements, which are already sorted by definition.

- Combine: Sorted sublists are re-combined to form final sorted list. Since the sorting is done in place by swapping elements within the original list, the combining step is not explicitly required. The pivot element is chosen and adjusted during the partitioning step to ensure that the elements smaller than the pivot are on one side, and the elements larger than the pivot are on the other side, effectively sorting the list.

(Sande, Lau, and Ngo 2022)

---
> How is the divide and conquer approach useful in recursion to solve complex problems? 

'Divide and conquer' as a problem-solving approach in programming attains rigor when combined with recursion and functional programming. 

Recursion enables the expression of computations as a series of function calls and function compositions. Mathematical induction can be used to prove correctness, termination, and other properties of functions defined recursively, where a function is defined in terms of itself. 

Recursion is well-situated in the functional programming paradigm, a paradigm which promotes a style of programming that is declarative, concise, modular, expressive, and rigorous. Functional programming is built on the mathematical foundation of the lambda calculus, where functions can be defined in a recursive manner and can operate on other functions as data. These 'higher-order functions' are 'first-class citizens' that can be passed as arguments, returned as values, and stored in data structures. This results in  'immutable' objects where data and variables do not change their values once defined. This results in programs without 'side effects', that is state changes that are difficult to track and control. This in turn makes it easier to reason about code and prove programs to be correct. 

However, divide and conquer can also be applied to non-recursive or imperative algorithms. Iterative or loop-based algorithms are typical for the non-recursive divide and conquer approach. Algorithms can often be implemented either recursively and non-recursively. Recursion may not be the most efficient or preferred approach due to the overhead associated with function calls. 

(Abelson, Sussman, & Sussman 1996; Knuth 1994; Barendregt 1984; Pierce et al 2021)

---
> What is the role of the 'substitution method' in recursion? 

Using the 'substitution method' a 'recurrence relation' is solved to find time complexity. 

**Steps:**

- **Recurrence relation:** the time complexity of a recursive algorithm is expressed in terms of itself, in a recurrence relation such as T(n) = 2T(n/2) + f(n), where T(n) represents the time complexity of the algorithm for an input size of n, and f(n) represents the time taken by the non-recursive part of the algorithm.

- **Educated Guess:** Based on the pattern observed in the recursive function calls and the base case of the recursion, an educated guess is made for the solution of the recurrence relation. 

- **Mathematical induction:** The proposed solution is then proven using mathematical induction. The base case is verified, and the inductive step is established to prove that the guessed solution holds for all values of the input.

- **Time complexity:** Once the guessed solution is proven, it can be used to determine the overall time complexity of the recursive algorithm, typically expressed in Big-O notation, providing an upper bound on the growth rate of the algorithm as the input size increases.

(Cormen 2022:90-5: Section 4.3)

---
> What are the applications of the divide and conquer approach? Provide Examples. 

- **Sorting:** Merge Sort, Quick Sort, and Heap Sort.

- **Searching:** Binary Search finds position of target element in sorted array. Repeatedly divides the array into smaller halves until target element found or search space is exhausted.

- **Matrix Operations:** Matrix multiplication, matrix exponentiation, and other matrix operations.

- **Computational Geometry:** finding the closest pair of points, convex hull, and line segment intersection.

- **Graph Algorithms:** Depth-First Search (DFS) and Breadth-First Search (BFS).

- **Numerical Computations:** fast Fourier transform (FFT), algorithms for multiplying large numbers (Karatsuba algorithm)
 
- **Image Processing:** image filtering, edge detection, and image segmentation, finding closest pair of points, processing of images by dividing into smaller subimages.

- **Compilers:**  top-down parsers (syntactic analysis of programming languages) 

- **Database management:** Aggregate function* in SQL: "Aggregate functions present a bottleneck, because they potentially require having all input values at once. In distributed computing, it is desirable to divide such computations into smaller pieces, and distribute the work, usually computing in parallel, via a divide and conquer algorithm." 

- **Parallel and Distributed Computing:** divide into smaller tasks solved independently by multiple processors.

- **MapReduce** is a programming model and framework for processing large-scale data sets in a distributed and parallel manner across a cluster of machines. It is widely used for big data processing and is known for its scalability and fault tolerance.

---
> What are the pros and cons of the divide and conquer approach?

**Pros:**

- **Efficiency:** allows for parallelization of tasks, reduces redundant computation, and minimizes the amount of data or work needed to be processed at each level of recursion. This can result in improved time complexity and reduced computational overhead.

- **Scalability:** suitable for parallel and distributed computing environments, where multiple processors or nodes can work on subproblems concurrently.

- **Modularity:** promotes modularity in algorithm design, breaking down complex problems into smaller, more manageable subproblems. This can make the algorithm easier to understand, implement, and maintain.

- **Flexibility:** a general and versatile problem-solving approach that can be applied to a wide range of problem domains and applications, of varying complexity and size.

**Cons:**

- **Overhead:** can sometimes introduce additional overhead, such as the cost of dividing the problem into smaller subproblems, combining the results, or managing the recursion. TFor small problem sizes the overhead may outweigh the benefits.

- **Complexity:** can introduce complexity into algorithm design which can make the algorithm harder to implement, debug, and maintain.

- **Communication and Coordination:** In parallel and distributed computing environments, may require communication and coordination among processors or nodes to exchange data or synchronize operations. This can introduce additional complexity and potential overhead.

- **Problem Constraints:** Some problems and problem domains may not have a natural way of dividing problems into smaller sub-problems, or the division may not result in significant benefits in terms of efficiency or scalability. 

### REFERENCES: 

Abelson, H., Sussman, G. J., & Sussman, J. (1996). *Structure and Interpretation of Computer Programs.* MIT Press.

Barendregt, H. (1984). *The Lambda Calculus: Its Syntax and Semantics.* North-Holland.

Cormen, T. H., Leiserson, C. E., Rivest, R. L., & Stein, C. (2022). *Introduction to Algorithms.* Fourth Edition. MIT Press. [Ebook]

Goodrich, M. T., & Tamassia, R. (2015). *Data Structures and Algorithms in Java.* John Wiley & Sons.

Knuth, D. E. (1994). *Concrete Mathematics: A Foundation for Computer Science.* Addison-Wesley.

Pierce, B. C., Casinghino, C., et al. (2021). *Software Foundations.* Electronic version available at https://softwarefoundations.cis.upenn.edu.

Sande, Jonathan, Kelvin Lau, and Vincent Ngo. (2022).*Data Structures & Algorithms in Dart (First Edition): Implementing Practical Data Structures in Dart.* raywenderlich.com

Sedgewick, R., & Wayne, K. (2011). *Algorithms.* Addison-Wesley Professional.

**Wikipedia:** Divide-and-conquer algorithm; Functional programming; Recursion; Recurrence relation; Call stack; Side effects; Immutable object; MapReduce; Aggregate function; Fork-join model







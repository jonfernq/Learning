## Hacker Rank: Difficult Intermediate Coding Challenges

> Please provide some examples of the most difficult questions on the intermediate Python  test.

1. Write a program that takes a list of integers as input and returns a list of all the pairs of integers in the list that sum up to a target value.

2. Write a function that takes a string as input and returns the length of the longest substring that contains only unique characters.

3. Write a program that takes a list of dictionaries representing students and their test scores, and returns a dictionary containing the average score for each subject.

4. Write a function that takes a list of integers as input and returns the median value.

``` 
def find_median(lst):
    # First, sort the list in ascending order
    lst.sort()

    # Check if the length of the list is odd or even
    if len(lst) % 2 == 0:
        # If the length is even, take the average of the middle two numbers
        mid = len(lst) // 2
        return (lst[mid - 1] + lst[mid]) / 2
    else:
        # If the length is odd, return the middle number
        mid = len(lst) // 2
        return lst[mid]

my_list = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
median = find_median(my_list)
print(median)  # Output: 4.0
```

5. Write a program that simulates a game of Blackjack, with a human player and a computer dealer.

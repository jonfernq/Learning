## Hacker Rank: Difficult Intermediate Coding Challenges

> Please provide some examples of the most difficult questions on the intermediate Python  test.

1. Write a program that takes a list of integers as input and returns a list of all the pairs of integers in the list that sum up to a target value.

```python 
def find_pairs(lst, target):
    # Initialize an empty list to hold the pairs
    pairs = []

    # Loop over each number in the list
    for i in range(len(lst)):
        # Loop over each subsequent number in the list
        for j in range(i + 1, len(lst)):
            # Check if the sum of the two numbers is equal to the target value
            if lst[i] + lst[j] == target:
                # If so, add the pair to the list of pairs
                pairs.append((lst[i], lst[j]))

    return pairs

my_list = [1, 2, 3, 4, 5, 6]
target = 7
pairs = find_pairs(my_list, target)
print(pairs)  # Output: [(1, 6), (2, 5), (3, 4)]
```

2. Write a function that takes a string as input and returns the length of the longest substring that contains only unique characters.

3. Write a program that takes a list of dictionaries representing students and their test scores, and returns a dictionary containing the average score for each subject.

```python 
def average_scores(students):
    # Initialize an empty dictionary to hold the average scores
    averages = {}

    # Loop over each student in the list
    for student in students:
        # Loop over each subject in the student's scores
        for subject, score in student['scores'].items():
            # Check if the subject is already in the averages dictionary
            if subject in averages:
                # If the subject is already in the dictionary, add the score to the total
                averages[subject]['total'] += score
                averages[subject]['count'] += 1
            else:
                # If the subject is not in the dictionary, initialize it with the first score
                averages[subject] = {'total': score, 'count': 1}

    # Calculate the average for each subject
    for subject, data in averages.items():
        averages[subject] = data['total'] / data['count']

    return averages

students = [
    {'name': 'Alice', 'scores': {'math': 80, 'english': 85, 'history': 90}},
    {'name': 'Bob', 'scores': {'math': 75, 'english': 70, 'history': 85}},
    {'name': 'Charlie', 'scores': {'math': 90, 'english': 80, 'history': 95}}
]

averages = average_scores(students)

print(averages) # Output: {'math': 81.67, 'english': 78.33, 'history': 90.0}
``` 

4. Write a function that takes a list of integers as input and returns the median value.

```python 
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

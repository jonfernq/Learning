## Hacker Rank: Basic Coding Challenges

> Could you provide some example questions from the basics test?

Here are some example questions that you might find on the "problem-solving basic" test on HackerRank:

1. Write a program that takes an integer as input and prints "Even" if the number is even or "Odd" if the number is odd.

2. Write a function that takes a list of integers as input and returns the sum of all the even numbers in the list.

4. Write a program that takes two strings as input and concatenates them together, separated by a space.

5. Write a program that takes a string as input and prints the reverse of the string.

6. Write a function that takes a list of strings as input and returns a new list with all the strings sorted in alphabetical order.

These questions are designed to test a candidate's understanding of basic programming concepts like conditional statements, loops, functions, and string manipulation. The actual questions on the test may vary depending on the specific version of the "problem-solving basic" test being used.

---
> Write a Python program that takes an integer as input and prints "Even" if the number is even or "Odd" if the number is odd.

```python
num = int(input("Enter a number: "))

if num % 2 == 0:
    print("Even")
else:
    print("Odd")
```

In this program, we first take an integer input from the user using the input() function and convert it to an integer using the int() function.

We then use an if-else statement to check if the input number is even or odd. The % operator is used to check if the remainder after division by 2 is zero, which indicates that the number is even. If the remainder is zero, the program prints "Even". Otherwise, it prints "Odd".

---
> Write a Python function that takes a list of integers as input and returns the sum of all the even numbers in the list.

```python
def sum_of_even_numbers(numbers):
    """
    Takes a list of integers as input and returns the sum of all the even numbers in the list.
    """
    # Initialize a variable to hold the sum of even numbers
    even_sum = 0
    
    # Iterate over each number in the list
    for num in numbers:
        # Check if the number is even
        if num % 2 == 0:
            # If the number is even, add it to the even_sum variable
            even_sum += num
    
    # Return the sum of even numbers
    return even_sum
```

In this function, we first initialize a variable called even_sum to 0. We then iterate over each number in the input list using a for loop. Inside the loop, we check if the number is even by checking if the remainder after division by 2 is 0. If the number is even, we add it to the even_sum variable.

Finally, we return the even_sum variable, which contains the sum of all the even numbers in the input list.

---
> Write a Python program that takes two strings as input and concatenates them together, separated by a space.

```python
str1 = input("Enter the first string: ")
str2 = input("Enter the second string: ")

concatenated_str = str1 + " " + str2

print(concatenated_str)
```

In this program, we first take two strings as input from the user using the input() function and store them in the variables str1 and str2.

We then concatenate the two strings together, separating them with a space, using the + operator and store the result in the variable concatenated_str.

Finally, we print the concatenated_str variable using the print() function, which prints the concatenated string to the console.

---
Write a Python program that takes a string as input and prints the reverse of the string.


```python
string = input("Enter a string: ")

# Use slicing to reverse the string
reversed_string = string[::-1]

print("Reverse of the string:", reversed_string)
```

In this program, we first take a string as input from the user using the input() function and store it in the variable string.

We then use slicing to reverse the string, by using the [::-1] notation. This notation starts at the end of the string, goes all the way to the beginning, and takes a step of -1 (i.e. goes backwards). The result is a reversed string, which we store in the variable reversed_string.

Finally, we print the reversed string using the print() function.

---
> Write a Python function that takes a list of strings as input and returns a new list with all the strings sorted in alphabetical order.

```python
def sort_strings(strings):
    """
    Takes a list of strings as input and returns a new list with all the strings sorted in alphabetical order.
    """
    sorted_strings = sorted(strings)
    return sorted_strings
```
In this function, we take a list of strings as input and store it in the parameter strings. We then use the built-in sorted() function to sort the list of strings in alphabetical order, and store the result in a new list called sorted_strings. Finally, we return the sorted list using the return statement.

To use this function, you can simply call it and pass in a list of strings as an argument, like this:

```python
my_strings = ["apple", "banana", "cherry", "date"]
sorted_strings = sort_strings(my_strings)
print(sorted_strings)
```

This will output the following sorted list:

```css
['apple', 'banana', 'cherry', 'date']
```


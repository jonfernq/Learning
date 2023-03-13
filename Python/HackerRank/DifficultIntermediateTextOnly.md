## Hacker Rank: Difficult Intermediate Coding Challenges

### Audio

- [Audio](https://drive.google.com/file/d/12lBHtkUxInZHE92dfc15DFpAVNwS5MW9/view?usp=sharing)

### Transcript

> Please provide some examples of the most difficult questions on the intermediate Python  test.

**1. Write a program that takes a list of integers as input and returns a list of all the pairs of integers in the list that sum up to a target value.**

**Explanation:**

The find_pairs function takes two inputs: a list of integers lst and a target integer value target. The function searches through the list and finds all pairs of numbers that add up to the target value, and returns these pairs as a list.

The function works by initializing an empty list called pairs to hold the pairs that add up to the target value. It then loops over each number in the list using a for loop, and for each number it loops over all subsequent numbers in the list using another for loop. This way, the function checks every possible pair of numbers in the list.

For each pair of numbers, the function checks if their sum is equal to the target value using an if statement. If the sum is equal to the target value, the pair of numbers is added to the pairs list using the append method.

Finally, the function returns the pairs list, which contains all pairs of numbers that add up to the target value.

In the example provided, the find_pairs function is called with my_list as the list of integers and target as the integer value 7. The function returns the pairs [(1, 6), (2, 5), (3, 4)], which are printed to the console using the print function. These pairs represent all possible pairs of numbers in my_list that add up to the target value of 7.

**2. Write a function that takes a string as input and returns the length of the longest substring that contains only unique characters.**

**Explanation:**

The function longest_unique_substring takes a string as input and returns the length of the longest substring in the string that contains only unique characters.

The function uses a sliding window approach to find the longest substring. It starts by initializing variables to keep track of the start and end indices of the substring, a set to keep track of the unique characters in the substring, and a variable to keep track of the length of the longest unique substring.

The function then loops over each character in the string using a while loop. At each iteration, it checks if the current character is in the set of seen characters. If the current character is not in the set, it adds it to the set and increments the end index. It also updates the max length if the current substring is longer than the previous longest substring.

If the current character is already in the set, it removes the first character in the substring from the set and increments the start index. This continues until the end index reaches the end of the string.

Finally, the function returns the length of the longest unique substring.

In the given example, the input string is "abcabcbb". The longest substring with unique characters is "abc", which has a length of 3. Therefore, the function returns 3 as output.

**3. Write a program that takes a list of dictionaries representing students and their test scores, and returns a dictionary containing the average score for each subject.**

**Explanation:**

This is a Python function that takes a list of dictionaries representing students and their test scores and returns a dictionary containing the average score for each subject.

The function first initializes an empty dictionary called averages to hold the average scores. It then loops over each student in the list of students, and for each student, it loops over each subject and score in the student's scores dictionary.

For each subject and score, the function checks if the subject is already in the averages dictionary. If it is, the function adds the score to the total for that subject and increments the count of scores for that subject. If the subject is not already in the dictionary, the function initializes it with the first score.

After looping over all the students and their scores, the function calculates the average score for each subject by dividing the total score for that subject by the count of scores for that subject. The function then returns the averages dictionary containing the average score for each subject.

**4. Write a function that takes a list of integers as input and returns the median value.**

**Explanation:**

The find_median() function takes a list of numbers as input and returns the median value of the list.

To find the median, the function first sorts the list in ascending order. Then, it checks if the length of the list is odd or even.

If the length of the list is even, the function takes the average of the middle two numbers in the sorted list. To do this, it finds the index of the middle number by dividing the length of the list by 2, and then rounds down to the nearest integer using integer division (//). It then returns the average of the numbers at that index and the previous index.

If the length of the list is odd, the function returns the middle number in the sorted list. To do this, it again finds the index of the middle number by dividing the length of the list by 2 and rounding down, and then returns the number at that index.

For example, if the input list is [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5], the function will sort it to [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]. Since the length of the list is odd, the function returns the middle number, which is 4.0.

**5. Write a program that simulates a game of Blackjack, with a human player and a computer dealer.**


This program creates a deck of cards and deals two cards to the player and the dealer. The player can then choose to hit (receive another card) or stand (keep their current hand). Once the player has finished their turn, the dealer takes their turn according to a simple set of rules (hit if their hand is less than 17, stand otherwise). Finally, the winner is determined based on who has the higher score without going over 21.

**Explanation:**

er and a computer dealer, each dealt two cards from a deck of cards. The aim of the game is to get as close to 21 points as possible without going over, and to beat the dealer's hand.

The program first creates a deck of cards, consisting of 52 cards in four suits (hearts, diamonds, spades, and clubs) and 13 ranks (2 through 10, plus Jack, Queen, King, and Ace). It then defines a function calculate_hand that calculates the total value of a hand of cards, and a function show_hand that displays the cards in a hand. It also defines a function get_player_choice that asks the player whether they want to hit (draw another card) or stand (keep their current hand).

The program then shuffles the deck and deals two cards to the player and two cards to the dealer, showing only one of the dealer's cards. The player's turn begins, and they are prompted to hit or stand. If they hit, another card is dealt and their hand is displayed. If their hand value goes over 21, they "bust" and lose the game. If they stand, the dealer's turn begins. The dealer must keep drawing cards until their hand value is at least 17. Once the dealer has finished drawing cards, their hand is displayed and the winner is determined based on who has the higher hand value without going over 21.

The program outputs the result of the game (win, lose, or tie) based on the values of the player's and dealer's hands.



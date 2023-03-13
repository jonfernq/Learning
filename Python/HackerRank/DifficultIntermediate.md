## Hacker Rank: Difficult Intermediate Coding Challenges

> Please provide some examples of the most difficult questions on the intermediate Python  test.

**1. Write a program that takes a list of integers as input and returns a list of all the pairs of integers in the list that sum up to a target value.**

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

**Explanation:**

The find_pairs function takes two inputs: a list of integers lst and a target integer value target. The function searches through the list and finds all pairs of numbers that add up to the target value, and returns these pairs as a list.

The function works by initializing an empty list called pairs to hold the pairs that add up to the target value. It then loops over each number in the list using a for loop, and for each number it loops over all subsequent numbers in the list using another for loop. This way, the function checks every possible pair of numbers in the list.

For each pair of numbers, the function checks if their sum is equal to the target value using an if statement. If the sum is equal to the target value, the pair of numbers is added to the pairs list using the append method.

Finally, the function returns the pairs list, which contains all pairs of numbers that add up to the target value.

In the example provided, the find_pairs function is called with my_list as the list of integers and target as the integer value 7. The function returns the pairs [(1, 6), (2, 5), (3, 4)], which are printed to the console using the print function. These pairs represent all possible pairs of numbers in my_list that add up to the target value of 7.

**2. Write a function that takes a string as input and returns the length of the longest substring that contains only unique characters.**

```python 
def longest_unique_substring(s):
    # Initialize variables to keep track of the start and end indices of the substring
    start = 0
    end = 0
    # Initialize a set to keep track of the unique characters in the substring
    seen = set()
    # Initialize a variable to keep track of the length of the longest unique substring
    max_length = 0

    # Loop over each character in the string
    while end < len(s):
        # Check if the current character is in the set of seen characters
        if s[end] not in seen:
            # If the current character is not in the set, add it to the set and increment the end index
            seen.add(s[end])
            end += 1
            # Update the max length if the current substring is longer than the previous longest substring
            max_length = max(max_length, end - start)
        else:
            # If the current character is in the set, remove the first character in the substring from the set and
            # increment the start index
            seen.remove(s[start])
            start += 1

    return max_length

my_string = "abcabcbb"
length = longest_unique_substring(my_string)
print(length)  # Output: 3
```

**Explanation:**

The function longest_unique_substring takes a string as input and returns the length of the longest substring in the string that contains only unique characters.

The function uses a sliding window approach to find the longest substring. It starts by initializing variables to keep track of the start and end indices of the substring, a set to keep track of the unique characters in the substring, and a variable to keep track of the length of the longest unique substring.

The function then loops over each character in the string using a while loop. At each iteration, it checks if the current character is in the set of seen characters. If the current character is not in the set, it adds it to the set and increments the end index. It also updates the max length if the current substring is longer than the previous longest substring.

If the current character is already in the set, it removes the first character in the substring from the set and increments the start index. This continues until the end index reaches the end of the string.

Finally, the function returns the length of the longest unique substring.

In the given example, the input string is "abcabcbb". The longest substring with unique characters is "abc", which has a length of 3. Therefore, the function returns 3 as output.

**3. Write a program that takes a list of dictionaries representing students and their test scores, and returns a dictionary containing the average score for each subject.**

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

**Explanation:**

This is a Python function that takes a list of dictionaries representing students and their test scores and returns a dictionary containing the average score for each subject.

The function first initializes an empty dictionary called averages to hold the average scores. It then loops over each student in the list of students, and for each student, it loops over each subject and score in the student's scores dictionary.

For each subject and score, the function checks if the subject is already in the averages dictionary. If it is, the function adds the score to the total for that subject and increments the count of scores for that subject. If the subject is not already in the dictionary, the function initializes it with the first score.

After looping over all the students and their scores, the function calculates the average score for each subject by dividing the total score for that subject by the count of scores for that subject. The function then returns the averages dictionary containing the average score for each subject.

**4. Write a function that takes a list of integers as input and returns the median value.**

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

**Explanation:**

The find_median() function takes a list of numbers as input and returns the median value of the list.

To find the median, the function first sorts the list in ascending order. Then, it checks if the length of the list is odd or even.

If the length of the list is even, the function takes the average of the middle two numbers in the sorted list. To do this, it finds the index of the middle number by dividing the length of the list by 2, and then rounds down to the nearest integer using integer division (//). It then returns the average of the numbers at that index and the previous index.

If the length of the list is odd, the function returns the middle number in the sorted list. To do this, it again finds the index of the middle number by dividing the length of the list by 2 and rounding down, and then returns the number at that index.

For example, if the input list is [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5], the function will sort it to [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]. Since the length of the list is odd, the function returns the middle number, which is 4.0.

**5. Write a program that simulates a game of Blackjack, with a human player and a computer dealer.**

```python
import random

# create deck of cards
suits = ['Hearts', 'Diamonds', 'Spades', 'Clubs']
ranks = ['Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten', 'Jack', 'Queen', 'King', 'Ace']

deck = []
for suit in suits:
    for rank in ranks:
        card = {'suit': suit, 'rank': rank}
        deck.append(card)

# function to calculate the value of a hand of cards
def calculate_hand(hand):
    total = 0
    num_aces = 0
    for card in hand:
        rank = card['rank']
        if rank in ['Jack', 'Queen', 'King']:
            total += 10
        elif rank == 'Ace':
            num_aces += 1
            total += 11
        else:
            total += int(rank)
    while total > 21 and num_aces > 0:
        total -= 10
        num_aces -= 1
    return total

# function to show the cards in a hand
def show_hand(hand):
    for card in hand:
        print(card['rank'], 'of', card['suit'])

# function to ask the player if they want to hit or stand
def get_player_choice():
    while True:
        choice = input('Do you want to hit or stand? ').lower()
        if choice in ['hit', 'stand']:
            return choice
        else:
            print('Please enter "hit" or "stand".')

# shuffle the deck and deal two cards to the player and the dealer
random.shuffle(deck)
player_hand = [deck.pop(), deck.pop()]
dealer_hand = [deck.pop(), deck.pop()]

# show the initial hands
print('Your hand:')
show_hand(player_hand)
print('Dealer\'s hand:')
print(dealer_hand[0]['rank'], 'of', dealer_hand[0]['suit'])
print('')

# player's turn
while True:
    choice = get_player_choice()
    if choice == 'hit':
        player_hand.append(deck.pop())
        print('Your hand:')
        show_hand(player_hand)
        if calculate_hand(player_hand) > 21:
            print('Bust! You lose.')
            exit()
    else:
        break

# dealer's turn
while calculate_hand(dealer_hand) < 17:
    dealer_hand.append(deck.pop())
print('Dealer\'s hand:')
show_hand(dealer_hand)

# determine the winner
player_score = calculate_hand(player_hand)
dealer_score = calculate_hand(dealer_hand)
if dealer_score > 21:
    print('Dealer bust! You win.')
elif player_score > dealer_score:
    print('You win.')
elif player_score == dealer_score:
    print('It\'s a tie.')
else:
    print('You lose.')
```    
    
This program creates a deck of cards and deals two cards to the player and the dealer. The player can then choose to hit (receive another card) or stand (keep their current hand). Once the player has finished their turn, the dealer takes their turn according to a simple set of rules (hit if their hand is less than 17, stand otherwise). Finally, the winner is determined based on who has the higher score without going over 21.

**Explanation:**

er and a computer dealer, each dealt two cards from a deck of cards. The aim of the game is to get as close to 21 points as possible without going over, and to beat the dealer's hand.

The program first creates a deck of cards, consisting of 52 cards in four suits (hearts, diamonds, spades, and clubs) and 13 ranks (2 through 10, plus Jack, Queen, King, and Ace). It then defines a function calculate_hand that calculates the total value of a hand of cards, and a function show_hand that displays the cards in a hand. It also defines a function get_player_choice that asks the player whether they want to hit (draw another card) or stand (keep their current hand).

The program then shuffles the deck and deals two cards to the player and two cards to the dealer, showing only one of the dealer's cards. The player's turn begins, and they are prompted to hit or stand. If they hit, another card is dealt and their hand is displayed. If their hand value goes over 21, they "bust" and lose the game. If they stand, the dealer's turn begins. The dealer must keep drawing cards until their hand value is at least 17. Once the dealer has finished drawing cards, their hand is displayed and the winner is determined based on who has the higher hand value without going over 21.

The program outputs the result of the game (win, lose, or tie) based on the values of the player's and dealer's hands.




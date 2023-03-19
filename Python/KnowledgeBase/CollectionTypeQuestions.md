
## Collection Type Questions 

### Tuples

1. What is a tuple in Python?

A tuple is an ordered, immutable collection of elements.

2. What is the syntax for creating a tuple?

Tuples are created by enclosing elements in parentheses, separated by commas.

3. Are tuples mutable or immutable?

Tuples are immutable, meaning their elements cannot be modified.

4. Can a tuple contain elements of different data types?

Yes, a tuple can contain elements of different data types.

5. How can you access individual elements of a tuple?

Individual elements of a tuple can be accessed using indexing, starting from 0.

6. Can a tuple be nested inside another tuple?

Yes, a tuple can be nested inside another tuple.

7. How can you concatenate two tuples together?

Two tuples can be concatenated using the + operator.

8. Can you modify an individual element of a tuple?

No, individual elements of a tuple cannot be modified.

9. How can you delete an entire tuple?

A tuple can be deleted using the del keyword.

10. How can you check if a particular element is present in a tuple?

The "in" keyword can be used to check if a particular element is present in a tuple.

11. Can you sort the elements of a tuple?

No, the elements of a tuple cannot be sorted.

12. How can you find the length of a tuple?

The len() function can be used to find the length of a tuple.

13. Can a tuple be converted to a list?

Yes, a tuple can be converted to a list using the list() function.

14. Can a list be converted to a tuple?

Yes, a list can be converted to a tuple using the tuple() function.

15. What is a named tuple in Python?

A named tuple is a subclass of a tuple with named fields.

16. What is the purpose of a named tuple?

A named tuple provides a way to give names to the elements in a tuple, making the code more readable and self-documenting.

17. What is the syntax for creating a named tuple?

Named tuples are created using the "namedtuple" function from the collections module.

18. How can you access individual fields of a named tuple?

Individual fields of a named tuple can be accessed using dot notation or indexing.

19. Can a named tuple be modified?

No, a named tuple, like a regular tuple, is immutable.

20. Can you convert a named tuple to a regular tuple?

Yes, a named tuple can be converted to a regular tuple using the tuple() function.

### Dictionaries: 

Q: What is a dictionary in Python?

A: A dictionary in Python is an unordered collection of key-value pairs where each key maps to a unique value.

Q: How do you create an empty dictionary in Python?

A: You can create an empty dictionary in Python using the curly braces {} or by using the built-in dict() function.

Q: How do you access a value in a dictionary in Python?

A: You can access a value in a dictionary in Python using its corresponding key inside square brackets [].

Q: What happens when you try to access a non-existent key in a dictionary in Python?

A: When you try to access a non-existent key in a dictionary in Python, it raises a KeyError exception.

Q: How do you add a key-value pair to a dictionary in Python?

A: You can add a key-value pair to a dictionary in Python by assigning a value to a new or existing key, using the syntax dict[key] = value.

Q: How do you modify the value of an existing key in a dictionary in Python?

A: You can modify the value of an existing key in a dictionary in Python by assigning a new value to the corresponding key, using the syntax dict[key] = new_value.

Q: How do you remove a key-value pair from a dictionary in Python?

A: You can remove a key-value pair from a dictionary in Python using the del keyword followed by the key you want to remove, like this: del dict[key].

Q: How do you check if a key exists in a dictionary in Python?

A: You can check if a key exists in a dictionary in Python using the in keyword, like this: key in dict.

Q: What happens when you try to access a non-existent key using the get() method in a dictionary in Python?

A: When you try to access a non-existent key using the get() method in a dictionary in Python, it returns None instead of raising a KeyError.

Q: How do you get the number of key-value pairs in a dictionary in Python?

A: You can get the number of key-value pairs in a dictionary in Python using the len() function.

Q: What is a dictionary comprehension in Python?

A: A dictionary comprehension in Python is a concise way to create a new dictionary by transforming an existing one or using a conditional statement.

---
### Lists

What is a list in Python?

A list is a collection of items of different data types enclosed in square brackets [ ] and separated by commas.

How can you create an empty list in Python?

You can create an empty list in Python by using the empty square brackets, like this: my_list = [].

What is the difference between a list and a tuple in Python?

A list is mutable, meaning you can add, remove or modify items in it, while a tuple is immutable, meaning its contents cannot be changed after creation.

How can you access an item in a list in Python?

You can access an item in a list by using its index. The index starts from 0 for the first item in the list. For example, to access the first item in a list, you can use myList[0].

How can you add an item to a list in Python?

You can add an item to a list using the append() method. For example, myList.append(5) adds the integer value 5 to the end of the list.

How can you remove an item from a list in Python?

You can remove an item from a list using the remove() method. For example, myList.remove(5) removes the integer value 5 from the list.

How can you insert an item into a specific position in a list in Python?

You can insert an item into a specific position in a list using the insert() method. For example, myList.insert(2, 5) inserts the integer value 5 into the third position (index 2) of the list.

How can you find the length of a list in Python?

You can find the length of a list by using the len() function. For example, len(myList) returns the number of items in the list.

How can you check if an item is in a list in Python?

You can check if an item is in a list by using the in keyword. For example, if 5 is an item in myList, you can use the expression 5 in myList, which returns True if the item is in the list, and False otherwise.

How can you concatenate two lists in Python?

You can concatenate two lists using the + operator. For example, myList1 + myList2 returns a new list that contains all the items in myList1 followed by all the items in myList2.

How can you slice a list in Python?

You can slice a list by specifying a range of indices. For example, myList[2:5] returns a new list that contains the items with indices 2, 3, and 4 in myList.

What is the difference between a shallow copy and a deep copy of a list in Python?

A shallow copy creates a new list with references to the original objects in the list, while a deep copy creates a new list with new copies of the objects in the original list.

How can you sort a list in Python?

You can sort a list using the sort() method. For example, myList.sort() sorts the items in the list in ascending order.

How can you reverse the order of a list in Python?

You can reverse the order of a list using the reverse() method. For example, myList.reverse() reverses the order of the items in the list.

---
### Sets 

What is a Python set?

Answer: A set is an unordered collection of unique elements in Python.

How do you create an empty set in Python?

Answer: An empty set can be created using either the set() function or the curly braces {}.

Can sets contain duplicates?

Answer: No, sets cannot contain duplicates as they are designed to only contain unique elements.

How do you add an element to a set in Python?

Answer: You can add an element to a set using the add() method or by using the union operator '|' with a singleton set.

How do you remove an element from a set in Python?

Answer: You can remove an element from a set using the remove() or discard() method.

What is the difference between the remove() and discard() methods of a set?

Answer: The remove() method raises a KeyError if the element is not present in the set, while the discard() method does not raise any error.

Can you iterate over a set in Python?

Answer: Yes, you can iterate over a set in Python using a for loop.

How do you check if an element is present in a set in Python?

Answer: You can use the 'in' keyword to check if an element is present in a set.

What is the difference between a set and a frozenset in Python?

Answer: A set is mutable, whereas a frozenset is immutable. Once a frozenset is created, you cannot add, remove or modify its elements.

How do you perform set operations such as union, intersection, and difference in Python?

Answer: You can use the union(), intersection(), and difference() methods or the '|' (union), '&' (intersection), and '-' (difference) operators to perform set operations in Python.

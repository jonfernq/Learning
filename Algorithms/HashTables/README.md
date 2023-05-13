## Hash Tables

**Problem Statement:**

Given a hash table of length m=11. 

Insert keys: 10, 22, 31, 4, 15, 28, 17, 88, 59.   

Using the division method and open addressing 
with the auxiliary hash function h(k)=k. 

Illustrate (draw) the result of inserting these keys using 

1. linear probing, 
2. quadratic probing with c1 = 1 and c2 = 3
3. double hashing with h1(k) = k and  h2(k) = k + 1

To clarify the meaning of the instructions:

This means that you have to insert the given keys into a hash table of length 11 using the following steps:

- Use the division method to compute the first hash function h1(k) = k mod 11, where k is the key and 11 is the table size. This will give you the first index to try to insert the key.
- Use open addressing to resolve any collisions that may occur when inserting the keys. - This means that if the first index is already occupied by another key, you have to find another index using a probe sequence.
- Use the auxiliary hash function h(k) = k to compute the second hash function h2(k) = h(k) mod 11. This will give you the increment for the probe sequence.
- Use linear probing to generate the probe sequence. This means that you have to add the increment to the first index and wrap around if necessary until you find an empty slot. - The probe sequence is given by hi(k) = (h1(k) + i * h2(k)) mod 11, where i is the probe number starting from 0.



### Linear Probing (Collision Resolution)

```
# Define the hash table size and the keys to insert
table_size = 11
keys = [10, 22, 31, 4, 15, 28, 17, 88, 59]

# Initialize the hash table with None values
hash_table = [None] * table_size

# Define the first hash function using the division method
def h1(key):
    return key % table_size

# Define the second hash function using the auxiliary hash function
# h2(k) = 7 - (k % 7),
def h2(key):
    return 7 - (key % 7)
    
# Define the linear probing function
def linear_probing(key, i):
    return (h1(key) + i * h2(key)) % table_size

# Define the insert function
def insert(key):
    # Try to insert the key at the first hash index
    index = h1(key)
    if hash_table[index] is None:
        hash_table[index] = key
        print(f"Inserted {key} at index {index}")
    else:
        # If there is a collision, use linear probing to find an alternative index
        i = 1 # Initialize the probe number
        while True:
            # Calculate the new index using linear probing
            index = linear_probing(key, i)
            if hash_table[index] is None:
                # If the new index is empty, insert the key there
                hash_table[index] = key
                print(f"Inserted {key} at index {index}")
                break
            else:
                # If the new index is occupied, increment the probe number and try again
                print(f"Index {index} occupied, {key} cannot be inserted in hash table: {hash_table}") 
                i += 1

# Insert all the keys into the hash table
for key in keys:
    insert(key)

# Print the final hash table
print(f"Final hash table: {hash_table}")


"""
Change the second hash function to a different function that is relatively prime to the table_size and does not produce zero. 
For example, if the table_size is 13, then a possible second hash function is h2(k) = 7 - (k % 7), 
which will always produce a value between 1 and 6.
"""
```
**OUTPUT:**

![linearprobing](https://github.com/jonfernq/Learning/assets/68504324/5e8c1cf5-5af3-41ea-9fa7-288c942c27a9)



### Double Hashing (Collision Resolution)


#### Case #1

```
# Define the hash table size and the keys to insert
table_size = 11
keys = [10, 22, 31, 4, 15, 28, 17, 88, 59]

# Initialize the hash table with None values
hash_table = [None] * table_size

# Define the first hash function
def h1(key):
    return key % table_size

# Define the second hash function
def h2(key):
    return 7 - (key % 7)

# Define the double hashing function
def double_hashing(key, i):
    return (h1(key) + i * h2(key)) % table_size

# Define the insert function
def insert(key):
    # Try to insert the key at the first hash index
    index = h1(key)
    if hash_table[index] is None:
        hash_table[index] = key
        print(f"Inserted {key} at index {index}")
    else:
        # If there is a collision, use double hashing to find an alternative index
        i = 1 # Initialize the probe number
        while True:
            # Calculate the new index using double hashing
            index = double_hashing(key, i)
            if hash_table[index] is None:
                # If the new index is empty, insert the key there
                hash_table[index] = key
                print(f"Inserted {key} at index {index}")
                break
            else:
                # If the new index is occupied, increment the probe number and try again
                i += 1
                # Print a message to show that there is a collision and why it occurred
                print(f"Collision for {key} at index {index} because it is occupied by {hash_table[index]}")

# Insert all the keys into the hash table
for key in keys:
    insert(key)

# Print the final hash table
print(f"Final hash table: {hash_table}")
```
**OUTPUT:**

![doublehashing](https://github.com/jonfernq/Learning/assets/68504324/2901c8fe-aedd-4f62-b3ef-d9734e182186)

---
#### Case #2:

```
# Define the hash table size and the keys to insert
table_size = 11
keys = [10, 22, 31, 4, 15, 28, 17, 88, 59]

# Initialize the hash table with None values
hash_table = [None] * table_size

# Define the first hash function
def h1(key):
    return key % table_size 

# Define the second hash function
def h2(key):
    return (key + 1) % table_size  

# Define the double hashing function
def double_hashing(key, i):
    return (key + i * (key + 1)) % table_size 

# Define the double hashing function
#def double_hashing(key, i):
#    return (h1(key) + i * h2(key)) % table_size

# Define the insert function
def insert(key):
    # Try to insert the key at the first hash index
    index = h1(key)
    print('INDEX: ', index) 
    if hash_table[index] is None:
        hash_table[index] = key
        print(f"Inserted {key} at index {index}")
    else:
        # If there is a collision, use double hashing to find an alternative index
        i = 1 # Initialize the probe number
        while True:
            # Calculate the new index using double hashing
            index = double_hashing(key, i)
            if hash_table[index] is None:
                # If the new index is empty, insert the key there
                hash_table[index] = key
                print(f"Inserted {key} at index {index}")
                break
            else:
                # If the new index is occupied, increment the probe number and try again
                i += 1
                # Print a message to show that there is a collision and why it occurred
                print(f"Collision for {key} at index {index} because it is occupied by {hash_table[index]}")

# Insert all the keys into the hash table
for key in keys:
    insert(key)

# Print the final hash table
print(f"Final hash table: {hash_table}")


"""

The double hashing function depends on the choice of the first and second hash functions, 
which can vary depending on the problem and the data. The general form of the double hashing function is:

- h(key, i) = (h1(key) + i * h2(key)) % table_size

where h1 and h2 are two different hash functions, i is the probe number, 
and table_size is the length of the hash table. The choice of h1 and h2 should 
ensure that they are independent and that they produce different values for different keys. 
A common choice for h2 is to use a prime number that is smaller than the table_size and relatively prime to it. 
This ensures that h2(key) is never zero and that it can generate all possible values between 0 and table_size - 1.


"""
```

**OUTPUT:**

![doublehash2](https://github.com/jonfernq/Learning/assets/68504324/7bb540a9-9eef-44d7-92ba-9feaabb0575f)




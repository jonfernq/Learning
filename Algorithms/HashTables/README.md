## Hash Tables


## Double Hashing (Collision Resolution)


### Case #1



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
### Case #2:

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




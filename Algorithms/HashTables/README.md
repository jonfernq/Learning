## Hash Tables


## Double Hashing (Collision Resolution)


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




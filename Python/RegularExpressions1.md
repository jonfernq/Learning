
## Regular Expressions

Multiple choice questions to test one's knowledge of Python regular expressions:

1. What will be the output of the following code snippet?

```python
import re

text = "The quick brown fox jumps over the lazy dog"
pattern = r"\b\w{5}\b"

matches = re.findall(pattern, text)
print(matches)
```

a) ['quick', 'brown', 'jumps']<br>
b) ['quick', 'brown', 'fox', 'jumps', 'over', 'lazy']<br>
c) ['brown', 'jumps']<br>
d) ['quick', 'brown', 'jumps', 'over', 'lazy']<br>

2. What will be the output of the following code snippet?

```python
import re

text = "My phone number is 123-456-7890."
pattern = r"\d{3}-\d{3}-\d{4}"

matches = re.findall(pattern, text)
print(matches)
```

a) ['123-456-7890']<br>
b) ['My phone number is 123-456-7890.']<br>
c) []<br>
d) ['My phone number is ', '123-456-7890.']<br>

3. What will be the output of the following code snippet?

```python
import re

text = "The quick brown fox jumps over the lazy dog"
pattern = r"\b[a-z]+\b"

matches = re.findall(pattern, text)
print(matches)
```

a) ['The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog']<br>
b) ['quick', 'brown', 'fox', 'jumps', 'over', 'lazy', 'dog']<br>
c) ['quick', 'brown', 'fox', 'jumps', 'over', 'lazy']<br>
d) ['The', 'quick', 'brown', 'fox', 'jumps', 'over', 'lazy']<br>

4. What will be the output of the following code snippet?

```python
import re

text = "My email address is example@example.com."
pattern = r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b"

matches = re.findall(pattern, text)
print(matches)
```

a) ['example@example.com']<br>
b) ['My email address is example@example.com.']<br>
c) []<br>
d) ['My email address is ', 'example@example.com.']<br>

5. What will be the output of the following code snippet?

```python
import re

text = "The quick brown fox jumps over the lazy dog"
pattern = r"\b[a-z]*o[a-z]*\b"

matches = re.findall(pattern, text)
print(matches)
```

a) ['brown', 'fox', 'over']<br>
b) ['brown', 'fox', 'jumps', 'over']<br>
c) ['brown', 'fox']<br>
d) ['brown', 'fox', 'jumps']<br>

6. What is the output of the following code?

```python
import re

pattern = r'^[a-zA-Z]+[0-9]*$'
strings = ['hello', '123world', 'good4', '123']

for string in strings:
    if re.match(pattern, string):
        print(string)
```        
        
a. 'hello', 'good4', '123' <br>
b. 'hello', '123world', 'good4' <br>
c. 'hello', 'good4' <br>
d. '123world', 'good4'

7. What is the output of the following code?

```python
import re

pattern = r'\b(\d+)\b.*\b\1\b'
strings = ['123 456 789 123', 'abc 123 123 abc', 'abc 123 abc 456', '123 abc 456 789']

for string in strings:
    if re.search(pattern, string):
        print(string)
```

a. '123 456 789 123', 'abc 123 123 abc', 'abc 123 abc 456', '123 abc 456 789' <br>
b. '123 456 789 123', 'abc 123 123 abc', 'abc 123 abc 456' <br>
c. '123 456 789 123', 'abc 123 123 abc' <br>
d. '123 456 789 123'

8. What is the output of the following code?

```python
import re

pattern = r'^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$'
strings = ['password123', 'PASSWORD123', 'Password', 'p123', 'password']

for string in strings:
    if re.match(pattern, string):
        print(string)
```

a. 'password123', 'PASSWORD123', 'Password' <br>
b. 'password123', 'PASSWORD123' <br>
c. 'password123' <br>
d. No output

9. What is the output of the following code?

```python
import re

pattern = r'(?i)([a-z]+)\1+'
strings = ['aaabbb', 'abc', 'AAABBB', 'AABB', 'aaaAAA']

for string in strings:
    if re.search(pattern, string):
        print(string)
```

a. 'aaabbb', 'AAABBB', 'AABB' <br>
b. 'aaabbb', 'AAABBB' <br>
c. 'aaabbb' <br>
d. No output

10. What is the output of the following code?

```python
import re

pattern = r'(?<!\w)[A-Z][a-z]+(?:\s[A-Z][a-z]+)*(?!\w)'
string = 'The Quick Brown Fox'

matches = re.findall(pattern, string)

print(matches)
```

a. ['The Quick Brown Fox'] <br>
b. ['The', 'Quick', 'Brown', 'Fox'] <br>
c. ['Quick', 'Brown'] <br>
d. No output



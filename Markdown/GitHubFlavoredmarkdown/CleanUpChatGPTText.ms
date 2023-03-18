## Clean Up CHatGPT Text

Program to do most of the tedious reformatting to make a Github Flavored Markdown (GFM) from a cut-and-paste from ChatGPT: 

Please write a Python program that reads in 
the lines of a text file and processes them as follows: 

1. Delete this string: "Jon Fernquest" (name of ChatGOT user) 

2. Delete the string "Copy code" and the whole line it is on.

3. Place '**' before and after strings 
that end in a colon, such as this: "spaCy:"
so you have "**spaCy:**"

4. For a line containing a question mark '?' or the word 'please' 
Place a blank line after the line. Before the line place a '>'.  

5. Place three backticks (```) after the following strings starting at the beginning of a line: 'python', 'yaml', 'diff', 'css', 'scss', 'bash', 'markdown'
  
6. Reduce sequences of two or more blank lines to one blank line.

```python
import re
from functools import reduce

def concat_strings(strings):
    return reduce(lambda x, y: x + y, strings)

def reduce_blank_lines(text):
    import re
    pattern = re.compile(r'\n{2,}')
    return pattern.sub('\n\n', text)

# Open input and output files
with open('input.txt', 'r', encoding='utf-8') as input_file, \
     open('output.txt', 'w', encoding='utf-8') as output_file:
  
    # Read in lines from input file
    lines = input_file.readlines()
    newlines = [] 
    # Process each line
    for line in lines:

        # 1. Delete "Jon Fernquest"
        line = line.replace("Jon Fernquest", "")

        # 2. Delete "Copy code" and whole line
        line = re.sub(r"Copy code.*\n", "", line)

        # 3. Place '**' before and after strings that end in a colon
        line = re.sub(r"(\w+:)", r"- **\1**", line)

        # 4. Place blank line after lines containing '?' or 'please'
        if '?' in line or 'please' in line:
            line = '> ' + line + '\n\n'

        # 5. Place three backticks after specified strings at beginning of line
        if re.match(r"^(python|yaml|diff|css|scss|bash|markdown)", line):
            line =  "```" + line.strip() + "\n"
        newlines.append(line) 
        
    text = concat_strings(newlines) 
    text = reduce_blank_lines(text)
    # Write transformed line to output file
    output_file.write(text) 

    # 6. Reduce sequences of two or more blank lines to one blank line
    # output_text = output_file.getvalue()
    #output_text = output_file.read()
    #output_text = re.sub(r"\n{2,}", "\n", output_text)

    # Write final transformed text to output file
    #output_file.seek(0)
    #output_file.write(output_text)
    ```
    
    

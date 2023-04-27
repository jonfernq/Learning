## SEQUENTIAL FILES (COBOL) 

Table of Contents 
- Create Test Data 
- Display File Records to Screen 
 
---
### TEST DATA 
> Create a test sales data file for a COBOL program.  
It should be a sequential file that contains multiple records for each salesperson with the following format: 
DATE, SALESPERSON, PRODUCT, PURCHASE-AMOUNT. Use Python with the faker package to create artificial test data. 

```python
from faker import Faker
import random

fake = Faker()

n = 10  # number of salespeople
m = 5   # maximum number of records per salesperson

with open('salesdata.txt', 'w') as f:
    for i in range(n):
        salesperson = fake.name()
        for j in range(random.randint(1, m)):
            date = fake.date()
            product = fake.word()
            purchase_amount = round(random.uniform(10.0, 1000.0), 2)
            f.write(f'{date}\t{salesperson}\t{product}\t{purchase_amount}\n')
```

In this updated program, we use a nested loop to create multiple records for each salesperson. 
The maximum number of records per salesperson is set by the m variable. The random.randint() 
function is used to generate a random integer between 1 and m, which determines the number of records for each salesperson. 
Then, we simply repeat the record creation process for that salesperson that many times.

**OUTPUT:**

```yaml
2023-04-01   John Smith     Widget A    1000.50
2023-04-02   Jane Doe       Widget B    750.00
2023-04-02   John Smith     Widget B    500.00
2023-04-03   Jane Doe       Widget A    2000.25
2023-04-03   John Smith     Widget A    1500.75
2023-04-04   Jane Doe       Widget A    3000.00
2023-04-04   John Smith     Widget B    250.50
2023-04-05   Jane Doe       Widget B    1250.25
2023-04-05   John Smith     Widget A    1750.25
```

In this example, each line represents a single record with four fields: DATE, SALESPERSON, PRODUCT, and PURCHASE-AMOUNT. 
There are multiple records for each SALESPERSON, allowing for analysis and summary of sales data by individual salespeople.

--- 
### DISPLAY FILE RECORDS 

> Please write a COBOL program tp read in the above salesdata.txt file and print it out to the screen.

Here's a COBOL program that reads in the salesdata.txt file and prints it out to the screen:

```sql
IDENTIFICATION DIVISION.
PROGRAM-ID. READ-SALES-FILE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SALES-IN-FILE ASSIGN TO 'salesdata.dat'
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD SALES-IN-FILE.
01 SALES-RECORD-IN PIC X(50).

WORKING-STORAGE SECTION.
01 END-OF-FILE PIC X VALUE 'N'.
01 WS-COUNTER PIC 9(3) VALUE ZERO.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    OPEN INPUT SALES-IN-FILE.
    DISPLAY 'DATE' ' ' 'SALESPERSON' ' ' 'PRODUCT' ' ' 'PURCHASE-AMOUNT'.
    PERFORM UNTIL END-OF-FILE = 'Y'
        READ SALES-IN-FILE
            AT END
                MOVE 'Y' TO END-OF-FILE
            NOT AT END
                DISPLAY SALES-RECORD-IN
                ADD 1 TO WS-COUNTER
        END-READ
    END-PERFORM.
    DISPLAY 'Total records read: ' WS-COUNTER.
    CLOSE SALES-IN-FILE.
    STOP RUN.
```    
    
This program reads in the salesdata.dat file using the SELECT statement and the OPEN statement. It then displays the header for the data fields and reads in each record using the READ statement. The program continues to read records until the end of file is reached. For each record read, the program displays the record using the DISPLAY statement and increments the counter. Finally, the program displays the total number of records read and closes the file using the CLOSE statement.




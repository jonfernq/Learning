## Date & Time Formatting'

```
IDENTIFICATION DIVISION. 
PROGRAM-ID.  Listing4-1. 
AUTHOR.  Michael Coughlan.
DATA DIVISION. 
WORKING-STORAGE SECTION.
01  UserName           PIC X(20). 

*> Receiving data item for DATE system variable: Format is YYMMDD 
01 CurrentDate.
   02  CurrentYear     PIC 99.
   02  CurrentMonth    PIC 99.
   02  CurrentDay      PIC 99.

*> Receiving data item for DAY system variable: Format is YYDDD
01 DayOfYear.
   02  FILLER          PIC 99.
   02  YearDay         PIC 9(3).

*> Receiving item for TIME: Format is HHMMSSss   s = S/100 
01 CurrentTime.
   02  CurrentHour     PIC 99.
   02  CurrentMinute   PIC 99.
   02  FILLER          PIC 9(4).

*> Receiving item for DATE YYYYMMDD system variable: Format is YYYYMMDD
01 Y2KDate.
   02 Y2KYear          PIC 9(4).
   02 Y2KMonth         PIC 99.
   02 Y2KDay           PIC 99.
   
*> Receiving item for DAY YYYYDDD system variable: Format is YYYYDDD
01 Y2KDayOfYear.
   02 Y2KDOY-Year      PIC 9(4).
   02 Y2KDOY-Day       PIC 999.
PROCEDURE DIVISION.
Begin.
    DISPLAY "Please enter your name - " WITH NO ADVANCING 
    ACCEPT  UserName 
    DISPLAY "**********************"
    ACCEPT CurrentDate  FROM DATE
    ACCEPT DayOfYear    FROM DAY
    ACCEPT CurrentTime  FROM TIME
    ACCEPT Y2KDate      FROM DATE YYYYMMDD
    ACCEPT Y2KDayOfYear FROM DAY YYYYDDD
    DISPLAY "Name is " UserName 
    DISPLAY "Date is " CurrentDay "-"  CurrentMonth "-" CurrentYear 
    DISPLAY "Today is day " YearDay " of the year"
    DISPLAY "The time is " CurrentHour ":" CurrentMinute 
    DISPLAY "Y2KDate is " Y2kDay SPACE Y2KMonth SPACE Y2KYear
    DISPLAY "Y2K Day of Year is " Y2KDoy-Day " of " Y2KDOY-Year
    STOP RUN. 
```
https://github.com/Apress/beg-cobol-for-programmers/blob/master/978-1-4302-6253-4_Coughlan_Ch04/Listing4-1%20AcceptAndDisplayExamples.cbl

### Compile & Run (without numbers in columns 1 to 6) 

![cobol___](https://user-images.githubusercontent.com/68504324/231672213-b8c12a99-08d5-4b48-b7c8-828a3dc5e0ff.jpg)

---
> What option in the GNUCOBOL config file allows the programmer to omit the numbers in columns one through 6 ?

In GNUCOBOL, the option that allows the programmer to omit the numbers in columns one through six is the --free compiler option. When this option is specified, GNUCOBOL treats the source code as free-form and ignores the requirement for line numbers in columns one through six. This allows the programmer to write COBOL code without the need for line numbers in the leftmost six columns, providing more flexibility in formatting and layout.

To add the -free option to the cobc command line for compiling a COBOL source file named datetime.cob, you can modify the command as follows:

```c
cobc -x -o datetime --free datetime.cob
```

The --free option tells the GNUCOBOL compiler to expect free-format source code instead of the default fixed-format source code. By using this option, you can write COBOL programs without adhering to the traditional fixed-format layout with columns and can use free-format layout, which allows for more flexible coding styles. Note that --free is a long option, and you can also use its short option -free in the same way, like this:

```c
cobc -x -o datetime -free datetime.cob
```

Both --free and -free are equivalent and can be used interchangeably.

https://gnucobol.sourceforge.io/doc/gnucobol.html





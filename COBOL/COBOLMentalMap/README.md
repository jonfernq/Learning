## COBOL Mental Map

### Based on 'COBOL for the 21st Century' 11th Edition (2005)

### Table of Contents (Short)  

Unit I: The Basics
- An Introduction to Structured Program Design in COBOL
- The IDENTIFICATION and ENVIRONMENT DIVISIONS
- Coding Complete COBOL Programs: The PROCEDURE DIVISION

Unit II: Designing Structured Programs
- Designing and Debugging Batch and Interactive COBOL Programs
- Computing in COBOL: The Arithmetic Verbs and Intrinsic Functions
- Decision Making Using the IF and EVALUATE Statements
- Iteration: Beyond the Basic PERFORM

Unit III: Writing High-Level COBOL Programs
- Control Break Processing
- Data Validation
- Array Processing and Table Handling

Unit IV: File Maintenance
- Sequential File Processing
- Sorting and Merging
- Indexed and Relative File Processing

Unit V: Advanced Topics
- Improving Program Performance Using the COPY, CALL, and Other Statements
- The Report Writer Module

Appendices
- A: COBOL Character Set and reserved Words
- B: Differences Among the COBOL Standards
- C: Glossary
- Index

---
### Table of Contents (Long)  

I. THE BASICS
- 1. An Introduction to Structured Program Design in COBOL

1.1. COMPUTER PROGRAMMING: AN OVERVIEW
- 1.1.1. Types of Computer Programs
- 1.1.2. Applications Programs
- 1.1.3. Machine Language Programs
- 1.1.4. Symbolic Programs

1.2. THE APPLICATIONS PROGRAM DEVELOPMENT PROCESS
- 1.2.1. Determine Program Specifications
- 1.2.2. Design the Program Using Program Planning Tools
- 1.2.3. Code and Enter the Program
- 1.2.4. Compile the Source Program
- 1.2.5. Test the Program
- 1.2.5.1. Debugging During the Compile and Test Phases
- 1.2.5.1.1. Compile-Time Errors
- 1.2.5.1.2. Execution Errors
- 1.2.5.2. Debugging Techniques
- 1.2.5.2.1. Desk Checking
- 1.2.5.2.2. Correcting Syntax Errors
- 1.2.5.2.3. Program Walkthroughs
- 1.2.5.2.4. Detecting Logic and Run-time Errors by Executing the Program
- 1.2.6. Document the Program

1.3. THE NATURE OF COBOL
- 1.3.1. COBOL Is a Business-Oriented Language
- 1.3.2. COBOL Is a Standard Language
- 1.3.3. COBOL Is an English-Like Language
- 1.3.4. COBOL Is Relatively Easy to Understand

1.4. A HISTORY OF COBOL AND THE ANS VERSIONS
- 1.4.1. When it Began
- 1.4.2. The American National Standards (ANS) Versions of COBOL
- 1.4.3. The Future of COBOL: Lessons Learned from the Year 2000 Problem
- 1.4.4. SELF-TEST

1.5. TECHNIQUES FOR IMPROVING PROGRAM DESIGN
- 1.5.1. Structured Programming Using Modular Design for Coding Paragraphs
- 1.5.2. The Top-Down Approach for Coding Modules

1.6. SAMPLE PROGREMS
- 1.6.1. Interactive vs. Batch Processing
- 1.6.2. An Overview of the Four Divisions
- 1.6.3. Definition of the Problem
- 1.6.3.1. Sample Interactive Program
- 1.6.3.1.1. Coding Rules
- 1.6.3.1.2. The IDENTIFICATION DIVISION of the Interactive Program
- 1.6.3.1.3. The DATA DIVISION of the Interactive Program
- 1.6.3.1.4. The PROCEDURE DIVISION of the Interactive Program
- 1.6.3.2. Sample Batch Program
- 1.6.3.2.1. Output Layout
- 1.6.3.2.2. The Program Illustrated
- 1.6.3.2.3. Coding Rules
- 1.6.3.2.4. The IDENTIFICATION and ENVIRONMENT DIVISIONs of the Batch Program
- 1.6.3.2.5. The DATA DIVISION of the Batch Program
- 1.6.3.2.6. The PROCEDURE DIVISION of the Batch Program
- 1.6.3.2.7. The Use of Periods
- 1.6.4. A Brief Overview of Program Planning Tools
- 1.6.5. Our Definitions for Batch and Interactive Programs
- 1.6.6. Summary of COBOL as a Language for Both Batch and Interactive Processing

1.7. ENTERING AND RUNNING A COBOL PROGRAM ON YOUR COMPUTER
1.8. CHAPTER SUMMARY
1.9. KEY TERMS
1.10. CHAPTER SELF-TEST
1.11. REVIEW QUESTIONS
1.12. PROGRAMMING ASSIGNMENTS


2. The IDENTIFICATION and ENVIRONMENT DIVISIONs

- 2.1. BASIC STRUCTURE OF A COBOL PROGRAM
- 2.1.1. Coding a Source Program
- 2.1.2. Coding Rules
- 2.1.2.1. The Main Body of a Program
- 2.1.2.2. Optional Entries: Identification and Page and Serial Numbers
- 2.1.2.3. Column 7: For Comments, Continuing Nonnumeric Literals, and Starting a New Page
- 2.1.2.3.1. Using Column 7 for Comments
- 2.1.2.3.2. Page-Eject with a Slash (/) in Column 7
- 2.1.2.4. Coding Rules for Areas A and B
- 2.1.3. Types of COBOL Entries

2.2. CODING REQUIREMENTS OF THE IDENTIFICATION DIVISION
- 2.2.1. Paragraphs in the IDENTIFICATION DIVISION
- 2.2.2. Understanding Instruction Formats as They Appear in Reference Manuals
- 2.2.3. Examples
- 2.2.4. SELF-TEST

2.3. THE SECTIONS OF THE ENVIRONMENT DIVISION
- 2.3.1. CONFIGURATION SECTION (Optional)
- 2.3.2. INPUT-OUTPUT SECTION

2.4. ASSIGNING FILES TO DEVICES IN THE ENVIRONMENT DIVISION
- 2.4.1. Overall Format
- 2.4.1.1. File-Name Rules
- 2.4.1.2. Implementor-Names or Device Specifications

2.5. CHAPTER SUMMARY
2.6. KEY TERMS
2.7. CHAPTER SELF-TEST
2.8. PRACTICE PROGRAM 1: INTERACTIVE PROCESSING
2.9. PRACTICE PROGRAM 2: BATCH PROCESSING
2.10. REVIEW QUESTIONS
2.11. PROGRAMMING AS- SIGNMENT- S


3. The DATA DIVISION

3.1. SYSTEMS DESIGN CONSIDERATIONS
- 3.1.1. The Relationship Between a Business Information System and its Programs
- 3.1.2. Interactive and Batch Processing
- 3.1.3. Designing Input for Applications Using Batch Processing

3.2. FORMING DATA-NAMES
- 3.2.1. Rules
- 3.2.2. Guidelines
- 3.2.2.1. Use Meaningful Data-Names
- 3.2.2.2. Use Prefixes or Suffixes in Data-Names Where Appropriate

3.3. THE FILE SECTION OF THE DATA DIVISION
- 3.3.1. An Overview
- 3.3.2. File Description Entries
- 3.3.2.1. LABEL RECORD(S) Clause—(Optional for COBOL 85, Required for COBOL 74, Eliminated from COBOL 2008)
- 3.3.2.2. RECORD CONTAINS Clause—(Optional)
- 3.3.2.3. BLOCK CONTAINS Clause—(Optional)
- 3.3.3. SELF-TEST
- 3.3.4. Record Description Entries
- 3.3.4.1. Defining a Record
- 3.3.4.2. Level Numbers
- 3.3.4.3. Fields May Be Defined as Either Elementary or Group Items
- 3.3.5. SELF-TEST
- 3.3.5.1. PICTURE (PIC) Clauses
- 3.3.5.1.1. Types of Data Fields
- 3.3.5.1.2. Size of Data Fields
- 3.3.5.1.3. Format of PIC Clauses
- 3.3.5.2. Allotting Space for Unused Areas in Record Description Entries
- 3.3.5.3. The Use of the Implied Decimal Point in PIC Clauses
- 3.3.5.4. Interactive Processing
- 3.3.5.5. Summary of Record Description Entrie- - - - - - - - - s

3.4. TYPES OF DATA
- 3.4.1. Variable and Constant Data
- 3.4.2. Types of Constants
- 3.4.2.1. Numeric Literal
- 3.4.2.2. Interactive Processing
- 3.4.2.3. Nonnumeric Literal
- 3.4.2.4. Figurative Constant

3.5. THE WORKING-STORAGE SECTION OF THE DATA DIVISION
- 3.5.1. Introduction
- 3.5.2. Uses of WORKING-STORAGE
- 3.5.3. VALUE Clauses for WORKING-STORAGE Entries
- 3.5.3.1. The Purpose of VALUE Clauses
- 3.5.3.2. Literals and Figurative Constants in VALUE Clauses
- 3.5.3.3. Continuation of Nonnumeric Literals in VALUE Clauses from One Line to the Next
- 3.5.4. The DATA DIVISION and Interactive Processing

- 3.6. CHAPTER SUMMARY
- 3.7. KEY TERMS
- 3.8. CHAPTER SELF-TEST
- 3.9. PRACTICE PROGRAM 1: BATCH PROCESSING
- 3.10. PRACTICE PROGRAM 2: INTERACTIVE PROCESSING
- 3.11. REVIEW QUESTIONS
- 3.12. DEBUGGING EXERCISES
- 3.13. PROGRAMMING ASSIGNMENTS


4. Coding Complete COBOL Programs: The PROCEDURE DIVISION
- 4.1. A REVIEW OF THE FIRST THREE DIVISIONS

- 4.2. THE FORMAT OF THE PROCEDURE DIVISION
- 4.2.1. Paragraphs that Serve as Modules
- 4.2.1.1. Defining Paragraphs
- 4.2.1.2. Rules for Forming Paragraph-Names
- 4.2.2. Statements within Paragraphs
- 4.2.3. The Sequence of Instructions in a Program
- 4.2.4. The Top-Down Approach for Coding Paragraphs

4.3. Statements Typically Coded in the Main Module of Batch Programs
- 4.3.1. OPEN Statement
- 4.3.1.1. The Instruction Format: A Review
- 4.3.1.2. The Purpose of the OPEN Statement
- 4.3.2. PERFORM UNTIL ... END-PERFORM Statement:A Structured Programming Technique
- 4.3.3. READ Statement
- 4.3.4. More on PERFORM Statements
- 4.3.4.1. The Simple PERFORM
- 4.3.4.2. A Summary of PERFORMs
- 4.3.5. End-of-Job Processing: The CLOSE and STOP RUN Statements
- 4.3.5.1. CLOSE Statement
- 4.3.5.2. STOP RUN Statement
- 4.3.5.3. SELF-TEST

4.4. STATEMENTS TYPICALLY CODED IN FULLY INTERACTIVE PROGRAMS

- 4.5. STATEMENTS TYPICALLY CODED FOR PROCESSING BATCH FILES
- 4.5.1. Simplified MOVE Statement
- 4.5.2. WRITE Statement

- 4.6. LOOKING AHEAD
- 4.7. PROCESS-AND-CREATE-OUTPUT MODULE FOR INTERACTIVE PROGRAMS
- 4.8. COMPARING BATCH AND INTERACTIVE PROGRAMS
- 4.9. REVIEW OF COMMENTS IN COBOL
- 4.10. Y2K-COMPLIANT DATE FIELDS
- 4.11. CHAPTER SUMMARY
- 4.12. KEY TERMS
- 4.13. CHAPTER SELF-TEST
- 4.14. PRACTICE PROGRAM
- 4.15. REVIEW QUESTIONS
- 4.16. DEBUGGING EXERCISES
- 4.17. PROGRAMMING ASSIGNMENTS

II. DESIGNING STRUCTURED PROGRAMS
- 5. Designing and Debugging Batch and Interactive COBOL Programs

5.1. WHAT MAKES A WELL-DESIGNED PROGRAM?
- 5.1.1. Program Logic Should Be Mapped Out Using a Planning Tool
- 5.1.2. Programs Should Be Structured
- 5.1.3. Programs Should Use a Top-Down Approach
- 5.1.4. Programs Should Be Modular

5.2. DESIGNING PROGRAMS BEFORE CODING THEM
- 5.2.1. How Programs Are Designed
- 5.2.2. Pseudocode
- 5.2.3. The Four Logical Control Structures
- 5.2.3.1. Sequence
- 5.2.3.2. Selection
- 5.2.3.3. Iteration
- 5.2.3.3.1. Format of a PERFORM UNTIL ... END-PERFORM Loop or Iteration
- 5.2.3.3.2. MAIN-MODULE
- 5.2.3.3.3. PARAGRAPH −1
- 5.2.3.3.4. A Simple PERFORM
- 5.2.3.4. Case Structure

5.3. ILLUSTRATING LOGICAL CONTROL STRUCTURES USING PSEUDOCODE
- 5.3.1. Example 1
- 5.3.2. Example 2

- 5.4. HIERARCHY CHARTS FOR TOP-DOWN PROGRAMMING
- 5.5. NAMING MODULES OR PARAGRAPHS
- 5.6. MODULARIZING PROGRAMS

5.7. A REVIEW OF TWO CODING GUIDELINES
- 5.7.1. Code Each Clause on a Separate Line
- 5.7.2. Indent Clauses within a Statement

5.8. MAKING INTERACTIVE PROGRAMS MORE USER-FRIENDLY

5.9. HOW TO BEGIN CODING, COMPILING, AND DEBUGGING PROGRAMS
- 5.9.1. Syntax Errors
- 5.9.2. Logic Errors
- 5.9.2.1. Designing Test Data That Is Comprehensive and Realistic
- 5.9.2.2. Checking for Logic Errors Using the DISPLAY Statement
- 5.9.2.2.1. Capturing Screen Displays in Interactive Processing
- 5.9.2.2.2. Common Error Notations Using Mainframe Compilers
- 5.9.3. The Use of Periods in the PROCEDURE DIVISION

- 5.10. CHAPTER SUMMARY
- 5.11. KEY TERMS
- 5.12. CHAPTER SELF-TEST
- 5.13. PRACTICE PROGRAM 1
- 5.14. PRACTICE PROGRAM 2
- 5.15. REVIEW QUESTIONS 
- 5.16. PROGRAMMING ASSIGNMENTS 


6. Moving Data, Printing Information, and Displaying Output Interactively
6.1. INTRODUCTION

6.2. THE INSTRUCTION FORMATS OF THE MOVE STATEMENT
6.2.1. SELF-TEST

6.3. NUMERIC MOVE
6.3.1. When Sending and Receiving Fields Have the Same PIC Clauses
6.3.2. When Sending and Receiving Fields Have Different PIC Clauses
6.3.2.1. Moving Integer Portions of Numeric Fields
6.3.2.2. Moving Decimal Portions of Numeric Fields
6.3.3. Moving Numeric Literals to Numeric Fields
6.3.4. Moving Signed Numbers: An Introduction
6.3.5. SELF-TEST

6.4. NONNUMERIC OR ALPHANUMERIC MOVE
6.4.1. Basic Rules
6.4.2. A Group Move Is Considered a Nonnumeric Move
6.4.3. SELF-TEST

6.5. OTHER OPTIONS OF THE MOVE STATEMENT
6.5.1. Qualification of Names
6.5.2. Performing Multiple Moves with a Single Statement
6.5.3. Reference Modification: Accessing Segments of a Field

6.6. PRODUCING PRINTED OUTPUT AND SCREEN DISPLAYS
6.6.1. Features of Printed Output and Screen Displays
6.6.1.1. Use of Edit Symbols
6.6.1.2. Spacing of Forms
6.6.1.3. Alignment of Information
6.6.1.4. Printing Headings, Total Lines, and Footings
6.6.1.5. The Printer Spacing Chart
6.6.2. The Editing Function
6.6.2.1. Printing or Displaying Decimal Points
6.6.2.2. Suppressing Leading Zeros
6.6.2.3. Printing or Displaying Dollar Signs and Commas
6.6.2.4. Printing or Displaying Asterisks (*) for Check Protection
6.6.2.5. Printing or Displaying Plus or Minus Signs
6.6.2.6. Printing or Displaying Debit and Credit Symbols for Accounting Applications
6.6.2.7. Printing or Displaying Spaces, Zeros, or Slashes as Separators within Fields
6.6.3. SELF-TEST
6.6.4. Editing Using Floating Strings
6.6.5. BLANK WHEN ZERO Option
6.6.6. Defining Print Records in WORKING-STORAGE
6.6.7. The WRITE . . . FROM Statement
6.6.8. The JUSTIFIED RIGHT Option
6.6.9. The ADVANCING Option for Spacing Printed Forms
6.6.9.1. Advancing the Paper a Fixed Number of Lines
6.6.9.2. Advancing the Paper to a New Page
6.6.9.2.1. The PAGE Option
6.6.9.3. End-of-Page Control—with the Use of a Programmed Line Counter
6.6.10. SELF-TEST
6.6.11. Printing Page Numbers
6.6.12. Printing or Displaying the Date of the Run
6.6.12.1. Accepting DATE Using Compilers That Are Not Y2K Compliant
6.6.12.1.1. The Y2K Problem
6.6.13. Printing or Displaying Quotation Marks

6.7. DISPLAYING OUTPUT INTERACTIVELY USING SCREEN INPUT AND OUTPUT
6.8. DISCUSSION OF AN INTERACTIVE SOLUTION TO PROGRAMMING ASSIGNMENT 4
6.9. EXAMPLE: CALCULATION OF TUITION
6.10. ADDITIONAL SCREEN SECTION ENTRIES
6.11. CHAPTER SUMMARY
6.12. KEY TERMS
6.13. CHAPTER SELF-TEST
6.14. PRACTICE PROGRAM
6.15. REVIEW QUESTIONS
6.16. INTERPRETING INSTRUCTION FORMATS
6.17. DEBUGGING EXERCISES
6.18. PROGRAMMING ASSIGNMENTS


7. Computing in COBOL: The Arithmetic Verbs and Intrinsic Functions

7.1. THE BASIC ARITHMETIC VERBS
7.1.1. ADD Statement
7.1.2. SELF-TEST
7.1.3. SUBTRACT Statement
7.1.4. SELF-TEST
7.1.5. MULTIPLY and DIVIDE Statements
7.1.5.1. Basic Instruction Format
7.1.5.2. Examples of Arithmetic Operations
7.1.5.3. Use of the REMAINDER Clause in the DIVIDE Operation
7.1.6. SELF-TEST

7.2. OPTIONS AVAILABLE WITH ARITHMETIC VERBS
7.2.1. ROUNDED Option
7.2.2. ON SIZE ERROR Option
7.2.3. NOT ON SIZE ERROR Clause
7.2.4. Determining the Size of Receiving Fields
7.2.5. SELF-TEST

7.3. THE COMPUTE STATEMENT
7.3.1. Basic Format
7.3.2. Order of Evaluation
7.3.3. Comparing COMPUTE to Arithmetic Verbs

7.4. USE OF SIGNED NUMBERS IN ARITHMETIC OPERATIONS
7.4.1. The Use of S in PIC Clauses for Fields That Can Be Negative
7.4.2. Rules for Performing Arithmetic with Signed Numbers
7.4.3. Entering Signed Numbers

7.5. INTRINSIC FUNCTIONS
7.5.1. Calendar Functions
7.5.2. Statistical and Numerical Analysis Functions
7.5.3. Trigonometric Functions
7.5.4. Financial Functions
7.5.5. Character and String Functions

7.6. CHAPTER SUMMARY
7.7. KEY TERMS
7.8. CHAPTER SELF-TEST
7.9. PRACTICE PROGRAM
7.10. REVIEW QUESTIONS
7.11. DEBUGGING EXERCISES
7.12. PROGRAMMING ASSIGNMENTS

8. Decision Making Using the IF and EVALUATE Statements
8.1. SELECTION USING A SIMPLE IF STATEMENT
8.1.1. A Review Of Logical Control Structures
8.1.2. Basic Conditional Statements
8.1.2.1. The Instruction Format for an IF Statement
8.1.2.2. Interpreting Instruction Formats
8.1.2.2.1. ELSE Is Optional
8.1.2.3. Example of an IF Statement Without an ELSE Clause
8.1.2.3.1. More Than One Operation Can Be Performed When a Condition Exists
8.1.2.4. Coding Guidelines
8.1.2.4.1. Indenting
8.1.2.4.2. Using Relational Operators in Place of Words
8.1.2.4.3. Do Not Mix Field Types in a Comparison
8.1.3. Planning Conditional Statements with Pseudocode
8.1.4. How Comparisons Are Performed
8.1.5. ASCII and EBCDIC Collating Sequences
8.1.6. Ending Conditional Sentences with a Period or an END-IF Scope Terminator
8.1.7. The CONTINUE Clause
8.1.8. SELF-TEST

8.2. SELECTION USING OTHER OPTIONS OF THE IF STATEMENT
8.2.1. Nested Conditional
8.2.2. Compound Conditional
8.2.2.1. OR in a Compound Conditional
8.2.2.2. AND in a Compound Conditional
8.2.2.3. Using AND and OR in the Same Statement
8.2.2.3.1. Introduction
8.2.2.3.2. Order of Evaluation of Compound Conditionals
8.2.2.3.3. Order of Evaluation: Possibility 1
8.2.2.3.4. Order of Evaluation: Possibility 2
8.2.2.3.5. Examples
8.2.3. Sign and Class Tests
8.2.3.1. Sign Test
8.2.3.2. Class Test
8.2.4. Negating Conditionals
8.2.4.1. Negating Simple Conditionals
8.2.4.2. Negating Compound Conditionals

8.3. USING IF STATEMENTS TO DETERMINE LEAP YEARS
8.4. CONDITION-NAMES
8.5. THE EVALUATE STATEMENT: USING THE CASE STRUCTURE AS AN ALTERNATIVE TO SELECTION
8.6. CHAPTER SUMMARY
8.7. KEY TERMS
8.8. CHAPTER SELF-TEST
8.9. PRACTICE PROGRAM
8.10. REVIEW QUESTIONS
8.11. DEBUGGING EXERCISES
8.12. PROGRAMMING ASSIGNMENTS


9. Iteration: Beyond the Basic PERFORM

9.1. THE SIMPLE PERFORM REVIEWED
9.1.1. The Basic Formats
9.1.2. Modularizing Programs Using PERFORM Statements
9.1.3. Nested PERFORM: A PERFORM within a PERFORM
9.1.4. Executing a Group of Paragraphs with a Simple PERFORM
9.1.5. The Use and Misuse of GO TO Statements
9.1.6. The EXIT Statement

9.2. ITERATION USING OTHER TYPES OF PERFORMS
9.2.1. PERFORM UNTIL ... (A Review)
9.2.2. Coding a Loop with a PERFORM
9.2.3. Perform ... Times
9.2.4. Examples of Loops
9.2.5. PERFORM VARYING

9.3. USING NESTED PERFORM VARYING STATEMENTS
9.4. THE PERFORM WITH TEST AFTER OPTION
9.5. CHAPTER SUMMARY
9.6. KEY TERMS
9.7. CHAPTER SELF-TEST
9.8. PRACTICE PROGRAM
9.9. REVIEW QUESTIONS
9.10. DEBUGGING EXERCISES
9.11. PROGRAMMING ASSIGNMENTS


III. WRITING HIGH-LEVEL COBOL PROGRAMS
10. Control Break Processing

10.1. AN INTRODUCTION TO CONTROL BREAK PROCESSING
10.1.1. Types of Reports: A Systems Overview
10.1.1.1. Detail or Transaction Reports
10.1.1.2. Exception Reports
10.1.1.3. Summary Reports
10.1.1.3.1. Displayed Output: A Systems Overview
10.1.2. An Example of a Control Break Procedure

10.2. PROGRAM REQUIREMENTS FOR CONTROL BREAK PROCESSING
10.2.1. A Single-Level Control Break
10.2.1.1. 200-DETAIL-RTN
10.2.1.1.1. 300-CONTROL-BREAK
10.2.2. Refinements to Improve the Quality of a Control Break Report
10.2.2.1. Printing a Final Total
10.2.2.2. Starting a New Page After Each Control Break
10.2.2.3. Sequence-Checking or Sorting: To Ensure That Input Data Was Entered in the Correct Sequence
10.2.2.4. Executing the Control Break Module from the Main Module After an End-of-File Condition Has Been Met
10.2.3. Summary of a Single-Level Control Break Procedure
10.2.4. SELF-TEST

10.3. MULTIPLE-LEVEL CONTROL BREAKS
10.4. CHAPTER SUMMARY
10.5. KEY TERMS
10.6. CHAPTER SELF-TEST
10.7. PRACTICE PROGRAM 1
10.8. PRACTICE PROGRAM 2
10.9. REVIEW QUESTIONS
10.10. DEBUGGING EXERCISES
10.11. PROGRAMMING ASSIGNMENTS

11. Data Validation

11.1. AVOIDING LOGIC ERRORS BY VALIDATING INPUT
11.1.1. Why Input to a Business System Must Be Validated
11.1.2. Some Consequences Of Invalid Input
11.1.2.1. Inaccurate Output
11.1.2.2. Logic Errors Resulting from Erroneous Input
11.1.3. Data Validation Techniques
11.1.3.1. Testing Fields to Ensure a Correct Format
11.1.3.2. Checking for Missing Data
11.1.3.3. The INSPECT Statement: Tallying and Replacing Specific Characters With Other Characters to Minimize Errors
11.1.4. SELF-TEST
11.1.4.1. Testing for Reasonableness
11.1.4.2. Condition-Names: Checking Coded Fields for Valid Contents
11.1.4.3. Sequence Checking
11.1.5. Using the EVALUATE Verb for Data Validation
11.1.6. Other Methods for Validating Data
11.1.6.1. Use of Control Listings for Manual Validation of Input
11.1.6.2. Verification as a Means of Validating Input

11.2. WHAT TO DO IF INPUT ERRORS OCCUR
11.2.1. Print an Error Record Containing the Key Field, the Contents Of the Erroneous Field, and an Error Message
11.2.2. Stop the Run
11.2.3. Partially Process Or Bypass Erroneous Records
11.2.4. Stop the Run if the Number Of Errors Exceeds a Predetermined Limit
11.2.5. Use Switches
11.2.6. Print Totals
11.2.6.1. Print a Count of All Records and a Count of All Errors
11.2.6.2. Print a Batch Total

11.3. GLOBAL CONSIDERATIONS IN COBOL
11.4. WHEN DATA SHOULD BE VALIDATED
11.5. UNDERSTANDING PROGRAM INTERRUPTS

11.6. OTHER METHODS FOR IMPROVING PROGRAM PERFORMANCE
11.6.1. Verifying File-Names With ACCEPT and DISPLAY Statements When Using a PC Compiler for Interactive Processing
11.6.2. The READ ... INTO Statement in Place of Using READ and MOVE Statements
11.6.3. Clearing Fields Using the INITIALIZE Statement
11.6.4. Improving Program Efficiency with the USAGE Clause
11.6.4.1. Format
11.6.4.2. USAGE IS DISPLAY
11.6.4.3. USAGE IS PACKED-DECIMAL or COMPUTATIONAL-3 (COMP-3)—A Common Enhancement
11.6.4.4. USAGE IS COMPUTATIONAL (COMP)

11.7. CHAPTER SUMMARY
11.8. KEY TERMS
11.9. CHAPTER SELF-TEST
11.10. PRACTICE PROGRAM
11.11. REVIEW QUESTIONS
11.12. DEBUGGING EXERCISES
11.13. PROGRAMMING ASSIGNMENTS

12. Array Processing and Table Handling

12.1. AN INTRODUCTION TO SINGLE-LEVEL OCCURS CLAUSES
12.1.1. Why OCCURS Clauses Are Used
12.1.1.1. Using an OCCURS to Define a Series of Input Fields Each with the Same Format
12.1.1.1.1. Defining Fields with an OCCURS Clause
12.1.1.1.2. Defining a Subscript
12.1.1.1.3. Coding Rules for Subscripts
12.1.1.1.4. A Subscript May Be an Integer or an Identifier
12.1.2. SELF-TEST
12.1.2.1. Using an OCCURS in WORKING-STORAGE for Storing Totals
12.1.3. Rules for Use of the OCCURS Clause
12.1.3.1. Levels 02–49
12.1.3.2. Defining Elementary or Group Items with an OCCURS Clause
12.1.3.3. Accessing a WORKING-STORAGE Area Defined by an OCCURS Clause
12.1.3.3.1. ADD-RTN

12.2. PROCESSING DATA STORED IN AN ARRAY
12.2.1. Using OCCURS with VALUE and REDEFINES Clauses
12.2.2. Printing Data Stored in an Array
12.2.3. SELF-TEST

12.3. USING AN OCCURS CLAUSE FOR TABLE HANDLING
12.3.1. Defining a Table
12.3.2. Storing the Table in WORKING-STORAGE Using Data on a Disk
12.3.3. Storing the Table in WORKING-STORAGE Using the Keyboard
12.3.4. Looking Up Data in a Table: Finding a Match

12.4. USE OF THE SEARCH STATEMENT FOR TABLE AND ARRAY PROCESSING
12.4.1. Format of the SEARCH Statement
12.4.2. The INDEXED BY Clause and the SEARCH Statement
12.4.3. Modifying the Contents of an Index
12.4.3.1. The SET Statement
12.4.3.2. Initializing an Index Before Using the SEARCH
12.4.4. Searching a Table–Foreign Exchange Example
12.4.5. Using Two WHEN Clauses for an Early Exit from a SEARCH
12.4.6. Searching for Multiple Matches
12.4.7. Internal vs External Tables

12.5. LOOKING UP TABLE DATA FOR ACCUMULATING TOTALS
12.5.1. SELF-TEST

12.6. THE SEARCH ... VARYING OPTION FOR PROCESSING PARALLEL TABLES
12.7. THE SEARCH ALL STATEMENT

12.7.1. Definition of a Serial Search
12.7.2. The Binary Search as an Alternative to the Serial Search
12.7.3. Format of the SEARCH ALL Statement
12.7.4. ASCENDING or DESCENDING KEY with the SEARCH ALL Statement

12.8. MULTIPLE-LEVEL OCCURS CLAUSE
12.8.1. Defining a Double-Level or Two-Dimensional Array
12.8.2. Accessing a Double-Level or Two-Dimensional Array
12.8.2.1. Using PERFORM . . . VARYING . . . AFTER
12.8.3. Using a Double-Level or Two-Dimensional Array for Accumulating Totals
12.8.4. Performing a Look-Up Using a Double-Level OCCURS

12.9. CHAPTER SUMMARY
12.10. KEY TERMS
12.11. CHAPTER SELF-TEST
12.12. PRACTICE PROGRAM
12.13. REVIEW QUESTIONS
12.14. DEBUGGING EXERCISES
12.15. PROGRAMMING ASSIGNMENTS


IV. FILE MAINTENANCE

13. Sequential File Processing

13.1. SYSTEMS OVERVIEW OF SEQUENTIAL FILE PROCESSING
13.1.1. Master Files
13.1.2. Typical Master File Procedures: A Systems Overview
13.1.2.1. Designing a Master File for Sequential Processing in Batch Mode
13.1.2.2. Creating a Master File for Sequential Processing in Batch Mode
13.1.2.3. Creating a Transaction File for Sequential Processing in Batch Mode
13.1.2.4. Updating a Master File for Sequential Processing in Batch Mode
13.1.2.5. Reporting from a Master File for Sequential Processing in Batch Mode
13.1.2.6. Creating Original Master and Transaction Files for New Systems

13.2. SEQUENTIAL FILE UPDATING—CREATING A NEW MASTER FILE USING A PREVIOUS MASTER FILE AND A TRANSACTION FILE
13.2.1. The Files Used
13.2.1.1. Input Master File
13.2.1.2. Input Transaction File
13.2.1.3. Output Master File
13.2.1.4. Control Listing or Audit Trail
13.2.2. The Ordering of Records for Sequential Updates
13.2.3. The Procedures Used for Sequential Updates
13.2.3.1. The Main Module
13.2.3.2. How Input Transaction and Master Records Are Processed
13.2.3.2.1. T-ACCT-NO IS EQUAL TO M-ACCT-NO
13.2.3.2.2. T-ACCT-NO IS GREATER THAN M-ACCT-NO
13.2.3.2.3. T-ACCT-NO IS LESS THAN M-ACCT-NO
13.2.3.3. Illustrating the Update Procedure with Examples
13.2.3.3.1. The Use of HIGH-VALUES for End-of-File Conditions
13.2.4. SELF-TEST

13.3. VALIDITY CHECKING IN UPDATE PROCEDURES
13.3.1. Checking for New Accounts
13.3.2. Checking for Delete Codes and Deleting Records from a Sequential Master File
13.3.3. Checking for Sequence Errors

13.4. UPDATE PROCEDURES WITH MULTIPLE TRANSACTION RECORDS FOR EACH MASTER RECORD
13.5. THE BALANCED LINE ALGORITHM FOR SEQUENTIAL FILE UPDATING

13.6. SEQUENTIAL FILE UPDATING—REWRITING RECORDS ON A DISK
13.6.1. The REWRITE Statement for a Disk File Opened as I-O
13.6.2. Using an Activity-Status Field for Designating Records to Be Deleted
13.6.3. The EXTEND Option for Adding Records to the End of a Sequential File

13.7. FILE MANAGEMENT TIPS
13.8. MATCHING FILES FOR CHECKING PURPOSES
13.9. INTERACTIVE UPDATING OF A SEQUENTIAL FILE
13.10. CHAPTER SUMMARY
13.11. KEY TERMS
13.12. CHAPTER SELF-TEST
13.13. PRACTICE PROGRAM
13.14. REVIEW QUESTIONS
13.15. DEBUGGING EXERCISES
13.16. PROGRAMMING ASSIGNMENTS


14. Sorting and Merging

14.1. THE SORT FEATURE: AN OVERVIEW
14.1.1. Format of the SORT Statement
14.1.2. ASCEDING or DESCENDING KEY
14.1.2.1. Collating Sequence
14.1.2.2. Sequencing Records with More Than One SORT Key
14.1.3. Coding a Simple SORT Procedure with the USING and GIVING Options
14.1.4. SELF-TEST

14.2. PROCESSING DATA BEFORE AND/OR AFTER SORTING
14.2.1. INPUT PROCEDURE
14.2.2. SELF-TEST
14.2.3. OUTPUT PROCEDURE
14.2.4. When to Use INPUT and/or OUTPUT PROCEDUREs

14.3. THE MERGE STATEMENT
14.4. CHAPTER SUMMARY
14.5. KEY TERMS
14.6. CHAPTER SELF-TEST
14.7. PRACTICE PROGRAM
14.8. REVIEW QUESTIONS
14.9. DEBUGGING EXERCISES
14.10. PROGRAMMING ASSIGNMENTS


15. Indexed and Relative File Processing

15.1. SYSTEMS CONSIDERATIONS FOR ORGANIZING DISK FILES
15.1.1. Sequential File Organization
15.1.2. Indexed File Organization
15.1.3. Relative File Organization

15.2. FEATURES OF MAGNETIC DISKS AND DISK DRIVES

15.3. PROCESSING INDEXED DISK FILES
15.3.1. Creating an Indexed File
15.3.1.1. The SELECT Statement
15.3.1.1.1. The ORGANIZATION Clause
15.3.1.1.2. The ACCESS Clause
15.3.1.1.3. The RECORD KEY Clause
15.3.1.2. The INVALID KEY Clause
15.3.2. Updating an Indexed File Randomly
15.3.2.1. The SELECT Statement
15.3.2.2. Opening an Indexed File as I-O
15.3.2.3. The READ Statement
15.3.2.4. REWRITE a Disk Record to Update It
15.3.2.5. Illustrating a Simple Update Procedure for an Indexed File
15.3.2.5.1. Reading from a Transaction File as Input
15.3.2.5.2. Accepting Transaction Data from a Keyboard Instead of a Disk File.
15.3.2.6. Additional Features of an Update Procedure
15.3.3. Updating an Indexed File with Multiple Transaction Records for Each Master Record
15.3.4. Accessing or Reading from an Indexed File for Reporting Purposes
15.3.5. SELF-TEST

15.4. ADDITIONAL OPTIONS FOR INDEXED FILE PROCESSING
15.4.1. Using ALTERNATE RECORD KEYS
15.4.2. The START Statement

15.4.3. Accessing an Indexed File Dynamically
15.4.3.1. The ACCESS IS DYNAMIC Clause
15.4.3.2. ACCESS IS DYNAMIC and the START Statement
15.4.3.3. The READ ... NEXT RECORD ... Instruction
15.4.3.4. Sequential Access of Records with the Use of ALTERNATE RECORD KEYs

15.4.4. SELF-TEST
15.4.5. A Review of INVALID KEY Clauses
15.4.6. ALTERNATE RECORD KEYS Reduce the Need for Multiple Copies of a File
15.4.7. The FILE STATUS Clause
15.4.8. Exception Handling with the USE Statement
15.4.9. SELF-TEST

15.5. USING AN INDEXED DISK FILE AS AN EXTERNAL TABLE

15.6. PROCESSING RELATIVE DISK FILES
15.6.1. What Is a Relative File?
15.6.2. Creating Relative Files
15.6.3. Sequential reading of relative files
15.6.4. Random reading of relative files
15.6.5. Random updating of relative files
15.6.6. SELF-TEST

15.7. CONVERTING A KEY FIELD TO A RELATIVE KEY
15.8. CHAPTER SUMMARY
15.9. KEY TERMS
15.10. CHAPTER SELF-TEST
15.11. PRACTICE PROGRAM
15.12. REVIEW QUESTIONS
15.13. DEBUGGING EXERCISES
15.14. PROGRAMMING ASSIGNMENTS

V. ADVANCED TOPICS

16. Improving Program Productivity Using The COPY, CALL, and Other Statements
16.1. COPY STATEMENT
16.1.1. Introduction
16.1.2. Entries that Can Be Copied
16.1.3. An Example
16.1.4. The Full Format for the COPY Statement
16.1.5. SELF-TEST

16.2. CALL STATEMENT
16.2.1. Why Use a CALL Statement?
16.2.2. Format of the CALL Statement
16.2.2.1. Called Program Requirements
16.2.2.1.1. PROGRAM-ID
16.2.2.1.2. LINKAGE SECTION
16.2.2.1.3. PROCEDURE DIVISION USING
16.2.2.1.4. EXIT PROGRAM
16.2.2.2. Calling Program Requirements
16.2.3. Examples

16.3. TEXT MANIPULATION WITH THE STRING AND UNSTRING STATEMENTS
16.3.1. The STRING Statement
16.3.1.1. The Basic Format
16.3.1.2. OVERFLOW Option
16.3.1.3. POINTER Option
16.3.1.4. General Rules for Using the STRING
16.3.2. The UNSTRING Statement
16.3.2.1. The Basic Format
16.3.2.2. General Rules for Using the UNSTRING

16.4. CHAPTER SUMMARY
16.5. KEY TERMS
16.6. CHAPTER SELF-TEST
16.7. PRACTICE PROGRAM
16.8. REVIEW QUESTIONS
16.9. DEBUGGING EXERCISES
16.10. PROGRAMMING ASSIGNMENTS

17. The Report Writer Module
17.1. INTRODUCTION

17.2. THE BENEFITS OF THE REPORT WRITER MODULE
17.2.1. For Detail and Summary Printing
17.2.2. For Control Break Processing
17.2.3. For Printing Headings and Footings

17.3. THE REPORT SECTION IN THE DATA DIVISION
17.3.1. The RD Entry within the REPORT SECTION
17.3.1.1. CONTROL Clause
17.3.1.2. PAGE LIMIT Clause
17.3.2. SELF-TEST

17.3.3. Clauses Used at the Group Level within a Report Group Description
17.3.3.1. TYPE Clause—Required
17.3.3.2. LINE Clause
17.3.3.3. NEXT GROUP Clause

17.3.4. SELF-TEST

17.3.5. Clauses Used at the Elementary Level within a Report Group Description
17.3.5.1. The SOURCE Clause
17.3.5.2. The SUM Clause
17.3.5.3. The RESET Phrase

17.3.6. REVIEW
17.3.6.1. Page Heading
17.3.6.2. Detail Line
17.3.6.3. CONTROL FOOTING for Printing Control Totals
17.3.6.4. CONTROL FOOTING for Printing the Final Total

17.4. PROCEDURE DIVISION STATEMENTS
17.4.1. INITIATE Statement
17.4.2. GENERATE Statement
17.4.3. TERMINATE Statement

17.5. CHAPTER SUMMARY
17.6. KEY TERMS
17.7. CHAPTER SELF-TEST
17.8. PRACTICE PROGRAM
17.9. REVIEW QUESTIONS
17.10. DEBUGGING EXERCISES
17.11. PROGRAMMING ASSIGNMENTS

VI. APPENDIXES
A. COBOL Character Set and Reserved Words
A.1. COBOL CHARACTERS
A.2. COBOL RESERVED WORDS
A.3. FUNCTION NAMES INCLUDED IN THE EXTENSIONS TO COBOL 85
A.4. NEW COBOL 2008 RESERVED WORDS
B. Differences Among the COBOL Standards
B.1. CHANGES IN THE NEW COBOL STANDARD (2008)
C. Glossary

https://www.oreilly.com/library/view/cobol-for-the/9780471722618/
https://www.amazon.in/COBOL-21st-Century-Stern/dp/8126517921



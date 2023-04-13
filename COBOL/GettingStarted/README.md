## Getting Started with COBOL

---

### Hello World Program:

```
000001 Identification Division.
000002 Program-Id. Greeting.
000003 Environment Division.
000004 Configuration Section.
000005 Source-Computer. My-Computer.
000006 Object-Computer. My-Computer.
000007 Data Division.
000008 Procedure Division.
000009 Greeting-Start.
000010 Display "Greetings from ChatGPT".
000011 Stop Run.
```
---
## Compile Steps: 

![helloworldchatgpt](https://user-images.githubusercontent.com/68504324/231666331-609840bf-9872-4ce7-a8b6-c1e2ad2e69cf.jpg)

---
### DISPLAY & ACCEPT: With Edit Mask to Suppress Leading Zeros 

```
000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID.  MyCalc.
000003 AUTHOR.  ChatGPT.
000004 DATA DIVISION.
000005 WORKING-STORAGE SECTION.
000006 01 FirstNumber       PIC 9     VALUE ZEROS.
000007 01 SecondNumber      PIC 9     VALUE ZEROS.
000008* Edit mask 'Z9' to suppress leading zeros when printing two digit number  
000008 01 CalcResult        PIC Z9    VALUE 0.
000009 01 UserPrompt        PIC X(38) VALUE
           "Please enter a single-digit number: ".
000010 PROCEDURE DIVISION.
000011 CalculateResult.
000012     DISPLAY UserPrompt.
000013     ACCEPT FirstNumber.
000012     DISPLAY UserPrompt.
000014     ACCEPT SecondNumber.
000015     COMPUTE CalcResult = FirstNumber + SecondNumber.
000016     DISPLAY "Result is = ", CalcResult.
000017     STOP RUN. 
```

---
> So Z9 to suppress leading zeros is called an 'edit mask' ?  

Yes, in COBOL, an "edit mask" is a formatting pattern that is used to specify how numeric data items should be displayed or edited. It consists of a combination of characters, including numeric placeholders (0, 9), sign indicators (S, +, -), and editing characters (., ,, /, $, etc.), that define the desired format for displaying numeric data.

In the context of the COBOL statement DISPLAY "Result is = ", Z9 CalcResult, the Z9 is an edit mask that specifies how the numeric data item CalcResult should be displayed. The Z character is used to suppress leading zeros, and the 9 character is a placeholder for a single numeric digit. When the CalcResult data item is displayed using this edit mask, any leading zeros in the value of CalcResult will be suppressed, and only the significant digits will be displayed.

Here are some examples of how the 'Z' character can be used in edit masks in COBOL:

ZZZZ.ZZ: This edit mask would display a numeric data item with at least four digits before the decimal point and two digits after the decimal point. Leading zeros would be suppressed.

ZZ,ZZZ.99: This edit mask would display a numeric data item with at least two digits before the comma, followed by groups of three digits separated by commas, and two digits after the decimal point. Leading zeros would be suppressed.

Z,ZZZ,ZZZ: This edit mask would display a numeric data item with at least one digit before the comma, followed by groups of three digits separated by commas. Leading zeros would be suppressed.

ZZ,ZZ9.99: This edit mask would display a numeric data item with at least two digits before the comma, followed by groups of three digits separated by commas, and two digits after the decimal point. If there are less than two significant digits before the decimal point, leading zeros would be displayed.

Z99,999: This edit mask would display a numeric data item with at least one digit before the first two digits, followed by a comma, and then groups of three digits separated by commas. Leading zeros would be suppressed.

Remember that the 'Z' character is used to suppress leading zeros in COBOL edit masks, allowing for more flexible and customizable display of numeric data items in COBOL programs.

---
> So it seems that COBOL syntax and semantics are very different from other common programming languages such as C, Java, JavScript and Python. I guess that this is because these other common languages are in the same programming language family tree, desdending for the most part from Algol and C ? 

Yes, that's correct. COBOL is a programming language that was specifically designed for business applications and has its own unique syntax and semantics, which can be quite different from other general-purpose programming languages like C, Java, JavaScript, and Python.

COBOL was developed in the late 1950s and early 1960s, and it has its roots in the era of mainframe computers and batch processing. It was primarily designed for handling large-scale data processing tasks in business environments, such as banking, finance, and government. As a result, COBOL has a focus on data manipulation and handling, with features like its extensive support for record-based file processing and data hierarchy.

The syntax and semantics of COBOL were designed to be highly readable and understandable for business users, with a focus on English-like statements and a heavy use of explicit data definitions. This makes COBOL code highly self-documenting and readable, which was important in the era when business users were often involved in writing and maintaining COBOL programs.

In contrast, other programming languages like C, Java, JavaScript, and Python are part of the procedural or object-oriented programming (OOP) paradigms, which have different design goals and origins. They have their roots in languages like Algol, C, and Simula, which were developed for different purposes and have different syntax and semantics compared to COBOL.

While there may be some similarities and overlaps in programming concepts and constructs among different programming languages, each language has its own unique syntax, semantics, and design philosophy, which may make them quite different from one another. As such, learning COBOL may require some effort to understand its unique syntax and semantics, especially for programmers familiar with other programming languages.


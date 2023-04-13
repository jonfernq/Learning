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
> so Z9 to suppress leading zeros is called an 'edit mask' ?  

Yes, in COBOL, an "edit mask" is a formatting pattern that is used to specify how numeric data items should be displayed or edited. It consists of a combination of characters, including numeric placeholders (0, 9), sign indicators (S, +, -), and editing characters (., ,, /, $, etc.), that define the desired format for displaying numeric data.

In the context of the COBOL statement DISPLAY "Result is = ", Z9 CalcResult, the Z9 is an edit mask that specifies how the numeric data item CalcResult should be displayed. The Z character is used to suppress leading zeros, and the 9 character is a placeholder for a single numeric digit. When the CalcResult data item is displayed using this edit mask, any leading zeros in the value of CalcResult will be suppressed, and only the significant digits will be displayed.

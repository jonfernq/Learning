## User-Defined Class Names

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Listing5-1.
       AUTHOR. Michael Coughlan.
      *> Shows how user defined class names are created and used

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS HexNumber IS "0" THRU "9", "A" THRU "F"
           CLASS RealName    IS "A" THRU "Z", "a" THRU "z", "'", SPACE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NumIn       PIC X(4).
       01 NameIn      PIC X(15).

       PROCEDURE DIVISION.
       Begin.
          DISPLAY "Enter a Hex number (4 characters from 0 to 9 & A to F) - " 
          ACCEPT NumIn.
          IF NumIn IS HexNumber THEN
             DISPLAY NumIn " is a Hex number"
           ELSE
             DISPLAY NumIn " is not a Hex number"
          END-IF

          DISPLAY "----------------------------------"
          DISPLAY "Enter a name - " WITH NO ADVANCING
          ACCEPT NameIn
          IF NameIn IS ALPHABETIC
             DISPLAY NameIn " is alphabetic"
           ELSE
             DISPLAY NameIn " is not alphabetic"
          END-IF

          IF NameIn IS RealName THEN
             DISPLAY NameIn " is a real name"
           ELSE
             DISPLAY NameIn " is not a real name"
          END-IF
          STOP RUN.
```

**Output:** 

![classnames](https://user-images.githubusercontent.com/68504324/231704935-517a02ed-0d29-49aa-8089-12c487277c64.jpg)

Note: Compile used the --free option because a line went over 72 characters.

---
> So what is the max line length  in gnucobol ? 

The maximum line length in GNUCOBOL, also known as COBOL85, is typically 72 characters. This is the standard line length specified by the ANSI COBOL standard, which is widely followed in most COBOL implementations, including GNUCOBOL.

In COBOL, programs are typically written in a fixed-format layout, where each line is divided into multiple fields, such as the Area A, Area B, Area C, and Area D. The first 6 characters in each line are used for line sequence numbers, and the subsequent characters are used for the actual COBOL code, data definitions, and other program elements.

The maximum line length of 72 characters is a common restriction in COBOL to ensure that programs can be easily printed and read on standard line printers that were common when the language was first developed. It is also considered a best practice for readability and maintainability of COBOL code.

However, it's worth noting that GNUCOBOL, like many other modern COBOL implementations, also supports free-format layout, where the restriction of 72 characters per line may not apply. Free-format COBOL allows for more flexible line lengths and formatting, but it's important to adhere to coding standards and conventions for consistency and readability in collaborative programming environments.


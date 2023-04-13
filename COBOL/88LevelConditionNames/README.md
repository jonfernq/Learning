## 88-Level Condition Names

```
IDENTIFICATION DIVISION.
PROGRAM-ID. Listing5-2.
AUTHOR.  Michael Coughlan.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  CityCode PIC 9 VALUE ZERO.
    88 CityIsDublin         VALUE 1.
    88 CityIsLimerick       VALUE 2.
    88 CityIsCork           VALUE 3.
    88 CityIsGalway         VALUE 4.
    88 CityIsSligo          VALUE 5.
    88 CityIsWaterford      VALUE 6.
    88 UniversityCity       VALUE 1 THRU 4.
    88 CityCodeNotValid     VALUE 0, 7, 8, 9.  
     
PROCEDURE DIVISION.
Begin.
   DISPLAY "Enter a city code (1-6) - " WITH NO ADVANCING
   ACCEPT CityCode 
   IF CityCodeNotValid
      DISPLAY "Invalid city code entered"
     ELSE
       IF CityIsLimerick 
          DISPLAY "Hey, we're home."
       END-IF
       IF CityIsDublin
          DISPLAY "Hey, we're in the capital." 
       END-IF
       IF UniversityCity 
          DISPLAY "Apply the rent surcharge!"
       END-IF
   END-IF
   STOP RUN.
```

https://github.com/Apress/beg-cobol-for-programmers/blob/master/978-1-4302-6253-4_Coughlan_Ch05/Listing5-2.cbl

In COBOL, the construct shown in the example is called "88-level" or "Level-88". It is used to define condition names, which are boolean flags that can be used to represent and test conditions or states in the data.

In the example you provided, the "88-level" condition names are used to define different values for the "CityCode" data item. Each condition name represents a specific value or range of values for the "CityCode" data item, based on the values specified in the "VALUE" clause.

For example:

"CityIsDublin" is defined as a condition name that represents the value "1" for "CityCode".
"CityIsLimerick" is defined as a condition name that represents the value "2" for "CityCode".
"CityIsCork" is defined as a condition name that represents the value "3" for "CityCode".
"CityIsGalway" is defined as a condition name that represents the value "4" for "CityCode".
"CityIsSligo" is defined as a condition name that represents the value "5" for "CityCode".
"CityIsWaterford" is defined as a condition name that represents the value "6" for "CityCode".
"UniversityCity" is defined as a condition name that represents a range of values from "1" to "4" (inclusive) for "CityCode".
"CityCodeNotValid" is defined as a condition name that represents the values "0", "7", "8", or "9" for "CityCode".
These condition names can be used in COBOL logic statements, such as "IF" statements or "PERFORM" statements, to make decisions or perform actions based on the values of the associated data items. Condition names provide a way to make COBOL code more readable and self-documenting, as they provide meaningful names for specific values or ranges of values in the data.



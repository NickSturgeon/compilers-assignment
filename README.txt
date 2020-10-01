A scanner/parser for PLATYPUS programs written for CST8152

Compile in ANSI C
Use with ./platyc <input_file> <output_file>

Produces a valid C program

####Example

Platypus program:
```
!! This program calculates the sum of 32767 even numbers.
!! The program is "lexically" and "syntactically" correct
!! and should not generate any error
PLATYPUS {
 
 a=+0.0;
 
 sum008 = 7.87050 ;
 READ(a,sum008);
 i=0; 
 WHILE TRUE (i < 32767 .OR. i == 32767)REPEAT{
   i = i + 2;
   a=
   a*i/0.5
   ;
   sum008 = sum008 + a - 1 ;
 };
 IF TRUE(text@ == "")THEN {
   text@ = "prog" << "ram";
 }
 ELSE {
   text@ = text@ << "ram";
 };
 WRITE("\* This is a platypus -:)-<-<-- \*");
 WRITE(text@);
 
 IF FALSE (text@ == "program".OR.sum008<>8..AND.i>10)THEN {
  WRITE(sum008);
  WRITE();
 }
 ELSE{};
}
```

Output C program:
```C
#include <stdio.h>
#include <string.h>
int main() {
    float a;
    float sum008;
    int i;
    char text[2048];
    a =  + 0;
    sum008 = 7.8705;
    scanf("%f", &a);
    scanf("%f", &sum008);
    i = 0;
    while (i < 32767 || i == 32767) {
        i = i + 2;
        a = a * i / 0.5;
        sum008 = sum008 + a - 1;
    }
    if (strcmp(text, "") == 0) {
        strcpy(text, "prog");
        strcat(text, "ram");
    } else {
        strcpy(text, text);
        strcat(text, "ram");
    }
    printf("\* This is a platypus -:)-<-<-- \*\n");
    printf("%s\n", text);
    if (!(strcmp(text, "program") == 0 || sum008 != 8 && i > 10)) {
        printf("%f\n", sum008);
        printf("\n");
    } else {
    }
    return 0;
}
```

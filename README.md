A scanner/parser for PLATYPUS programs written for CST8152

Compile in ANSI C

Use with ./platyc <input_file>

View the other branch for the C transpiler version

#### Example ouput:
```
Reading file ../data/input_files/ass3r.pls ....Please wait

Printing input buffer parameters:

The capacity of the buffer is:  466
The current size of the buffer is:  466

Printing input buffer contents:

!!This is a syntactically correct PLATYPUS program
!!Weiler's law:
!!"Nothing is impossible for the man who doesn't have to do it himself."
!!"Parsing is passing." S^R & Compilers' law

PLATYPUS{
 a=-000;
 b=+0.;
 READ(c);
 READ(d,e,f);
 c=((d+e)/a)*f-(((d-e)*a)/f);
 WHILE TRUE(a<>b .OR. c==d .AND. e<f .OR. a>0)REPEAT{
   IF TRUE(a==1 .AND. b==0.0)THEN{
    c=-(5.9);
   }ELSE {c=-c;};  
 };   
 WRITE();
 WRITE("Results: ");
 WRITE(d,e,f,a);
}?

Parsing the source file...

PLATY: Primary arithmetic expression parsed
PLATY: Unary arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Assignment expression (arithmetic) parsed
PLATY: Assignment statement parsed
PLATY: Primary arithmetic expression parsed
PLATY: Unary arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Assignment expression (arithmetic) parsed
PLATY: Assignment statement parsed
PLATY: Variable list parsed
PLATY: Input statement parsed
PLATY: Variable list parsed
PLATY: Input statement parsed
PLATY: Primary arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Additive arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Multiplicative arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Multiplicative arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Additive arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Multiplicative arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Multiplicative arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Additive arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Assignment expression (arithmetic) parsed
PLATY: Assignment statement parsed
PLATY: Primary a_relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Relational expression parsed
PLATY: Logical AND expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Relational expression parsed
PLATY: Logical OR expression parsed
PLATY: Logical OR expression parsed
PLATY: Conditional expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Primary a_relational expression parsed
PLATY: Relational expression parsed
PLATY: Logical AND expression parsed
PLATY: Conditional expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Primary arithmetic expression parsed
PLATY: Unary arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Assignment expression (arithmetic) parsed
PLATY: Assignment statement parsed
PLATY: Primary arithmetic expression parsed
PLATY: Unary arithmetic expression parsed
PLATY: Arithmetic expression parsed
PLATY: Assignment expression (arithmetic) parsed
PLATY: Assignment statement parsed
PLATY: Selection statement parsed
PLATY: Iteration statement parsed
PLATY: Output list (empty) parsed
PLATY: Output statement parsed
PLATY: Output list (string literal) parsed
PLATY: Output statement parsed
PLATY: Variable list parsed
PLATY: Output statement parsed
PLATY: Program parsed
PLATY: Source file parsed

Collecting garbage...
```

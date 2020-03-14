/* 
File name: table.h
Compiler: MS Visual Studio 2019
Author: Nicholas Sturgeon, 040911218
Course: CST 8152 – Compilers, Lab Section: 013 Assignment: 3
Date: 2019/12/05
Professor: Sv. Ranev
Purpose: Transition Table and function declarations necessary for the scanner implementation
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */
#define SEOF ((unsigned char)EOF)

/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF,
 */

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS (-1) /* Invalid state */

#define SOR 100 /* State outside range RTE */
#define WST 101 /* Wrong state type RTE */

/* State transition table definition */
#define TABLE_COLUMNS 8
#define TABLE_ROWS 13
/*transition table - type of states defined in separate table */
int st_table[TABLE_ROWS][TABLE_COLUMNS] = {
/* State 0 */  {  1,  6,  4, ES, ES,  9, ER, ES },
/* State 1 */  {  1,  1,  1,  2,  3,  2,  2,  2 },
/* State 2 */  { IS, IS, IS, IS, IS, IS, IS, IS },
/* State 3 */  { IS, IS, IS, IS, IS, IS, IS, IS },
/* State 4 */  { ES,  4,  4,  7,  5,  5,  5,  5 },
/* State 5 */  { IS, IS, IS, IS, IS, IS, IS, IS },
/* State 6 */  { ES,  6, ES,  7, ES,  5,  5,  5 },
/* State 7 */  {  8,  7,  7, 8,   8,  8,  8,  8 },
/* State 8 */  { IS, IS, IS, IS, IS, IS, IS, IS },
/* State 9 */  {  9,  9,  9,  9,  9, 10, ER,  9 },
/* State 10 */ { IS, IS, IS, IS, IS, IS, IS, IS },
/* State 11 */ { IS, IS, IS, IS, IS, IS, IS, IS },
/* State 12 */ { IS, IS, IS, IS, IS, IS, IS, IS }
};
 
/* Accepting state table definition */
#define ASWR     2  /* accepting state with retract */
#define ASNR     1  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

int as_table[TABLE_ROWS] = {
        NOAS,
        NOAS,
        ASWR,
        ASNR,
        NOAS,
        ASWR,
        NOAS,
        NOAS,
        ASWR,
        NOAS,
        ASNR,
        ASNR,
        ASWR
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  
typedef Token (*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[TABLE_ROWS] = {
        NULL,
        NULL,
        aa_func02,
        aa_func03,
        NULL,
        aa_func05,
        NULL,
        NULL,
        aa_func08,
        NULL,
        aa_func10,
        aa_func12,
        aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char * kw_table[KWT_SIZE] = {
    "ELSE",
    "FALSE",
    "IF",
    "PLATYPUS",
    "READ",
    "REPEAT",
    "THEN",
    "TRUE",
    "WHILE",
    "WRITE"   
};

#endif

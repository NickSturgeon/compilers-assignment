/*
File name: buffer.h
Compiler: MS Visual Studio 2019
Author: Nicholas Sturgeon, 040911218
Course: CST 8152 – Compilers, Lab Section: 013 Assignment: 3
Date: 2019/12/05
Professor: Sv. Ranev
Purpose: Header file for the buffer containing constant definitions, struct and function declarations
Function list:
    b_allocate();
    b_addc();
    b_clear();
    b_free();
    b_isfull();
    b_limit();
    b_capacity();
    b_mark();
    b_mode();
    b_incfactor();
    b_load();
    b_isempty();
    b_getc();
    b_eob();
    b_print();
    b_compact();
    b_rflag();
    b_retract();
    b_reset();
    b_getcoffset();
    b_rewind();
    b_location();
*/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <stdlib.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value 1 */
#define RT_FAIL_2 (-2)         /* operation failure return value 2 */
#define LOAD_FAIL (-2)         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */
#define MODE_FIX 'f'
#define B_MODE_FIX 0
#define MODE_ADD 'a'
#define B_MODE_ADD 1
#define MODE_MUL 'm'
#define B_MODE_MUL (-1)

#define MAX_CAPACITY (SHRT_MAX - 1) /* -1 for proper array indexing */

#define PERCENT_MAX 100

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  (unsigned short)0xFFFC
#define SET_EOB        (unsigned short)0x0002
#define RESET_EOB      (unsigned short)0xFFFD
#define CHECK_EOB      (unsigned short)0x0002
#define SET_R_FLAG     (unsigned short)0x0001
#define RESET_R_FLAG   (unsigned short)0xFFFE
#define CHECK_R_FLAG   (unsigned short)0x0001

/* user data type declarations */
typedef struct BufferDescriptor {
    char* cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, * pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */

Buffer* b_allocate(short, char, char);
pBuffer b_addc(pBuffer const, char);
int b_clear(Buffer* const);
void b_free(Buffer* const);
int b_isfull(Buffer* const);
short b_limit(Buffer* const);
short b_capacity(Buffer* const);
short b_mark(pBuffer const, short);
int b_mode(Buffer* const);
size_t b_incfactor(Buffer* const);
int b_load(FILE* const, Buffer* const);
int b_isempty(Buffer* const);
char b_getc(Buffer* const);
int b_eob(Buffer* const);
int b_print(Buffer* const, char);
Buffer* b_compact(Buffer* const, char);
char b_rflag(Buffer* const);
short b_retract(Buffer* const);
short b_reset(Buffer* const);
short b_getcoffset(Buffer* const);
int b_rewind(Buffer* const);
char* b_location(Buffer* const);

#endif

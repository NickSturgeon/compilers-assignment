/*
File name: scanner.c
Compiler: MS Visual Studio 2019
Author: Nicholas Sturgeon, 040911218
Course: CST 8152 – Compilers, Lab Section: 013 Assignment: 3
Date: 2019/12/05
Professor: Sv. Ranev
Purpose: SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
		 as required for CST8152, Assignment #2
		 scanner_init() must be called before using the scanner.
Function list:
	scanner_init();
	malar_next_token();
	get_next_state();
	char_class();
	aa_func02();
	aa_func03();
	aa_func08();
	aa_func05();
	aa_func10();
	aa_func12();
	strcpy_overflow();
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum; /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/* pointer to temporary lexeme buffer */
static pBuffer sc_buf; /* pointer to input source buffer */
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static void strcpy_overflow(char dest[], char src[], size_t limit); /* copies string with overflow protection */
static Token runtime_error(int code); /* returns a runtime error token with the specified code */

/* Initializes scanner */
int scanner_init(pBuffer psc_buf) {
    if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
    /* in case the buffer has been read previously  */
    b_rewind(psc_buf);
    b_clear(str_LTBL);
    line = 1;
    sc_buf = psc_buf;
    return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
Purpose: Gets the next Token from the scanner buffer
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: b_getc()
				  b_retract()
				  strcpy()
				  strcat()
			  	  b_mark()
				  b_getcoffset()
				  get_next_state()
				  b_allocate()
				  b_reset()
				  b_addc()
				  b_compact()
				  b_location()
				  b_free()
				  
Parameters: None
Return value: Token
Algorithm: - Checks for any special case tokens
		   - Return if found, otherwise
		   - Run the transition table to get an accepting state
		   - Run the appropriate accepting function
		   - Return the token
*/
Token malar_next_token(void) {
    Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
    unsigned char c; /* input symbol */
    int state = 0; /* initial state of the FSM */
    short lexstart; /* start offset of a lexeme in the input char buffer (array) */
    short lexend;  /* end offset of a lexeme in the input char buffer (array) */

    /* DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */
    int state_type; /* the type of state (NOAS, ASNR, ASWR) */
    short lex_length; /* the length of the lexeme string */

    while (1) { /* endless loop broken by token returns it will generate a warning */

        /* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */
        c = b_getc(sc_buf);

#ifdef DEBUG
        printf("LINE: %d\n", line);
#endif

/* Part 1: Implementation of token driven scanner */
/* every token is possessed by its own dedicated code */

        switch (c) {
            case SEOF:
                t.code = SEOF_T;
                if (SEOF == '\0') {
                    t.attribute.seof = SEOF_0;
                } else {
                    t.attribute.seof = SEOF_EOF;
                }
                return t;
            case '\n':
                line++; /* new line */
                continue;
            case ' ':
			case '\v':
            case '\t':
			case '\f':
            case '\r': /* for DOS line endings */
                continue;
            case '{':
                t.code = LBR_T;
                return t;
            case '}':
                t.code = RBR_T;
                return t;
            case '(':
                t.code = LPR_T;
                return t;
            case ')':
                t.code = RPR_T;
                return t;
            case ';':
                t.code = EOS_T;
                return t;
            case ',':
                t.code = COM_T;
                return t;
            case '+':
                t.code = ART_OP_T;
                t.attribute.arr_op = PLUS;
                return t;
            case '-':
                t.code = ART_OP_T;
                t.attribute.arr_op = MINUS;
                return t;
            case '*':
                t.code = ART_OP_T;
                t.attribute.arr_op = MULT;
                return t;
            case '/':
                t.code = ART_OP_T;
                t.attribute.arr_op = DIV;
                return t;
            case '.':
                c = b_getc(sc_buf);
				/* check for .AND. or .OR., retract if not found */
                if (c == 'A') {
                    if (b_getc(sc_buf) == 'N') {
                        if (b_getc(sc_buf) == 'D') {
                            if (b_getc(sc_buf) == '.') {
                                t.code = LOG_OP_T;
                                t.attribute.log_op = AND;
                                return t;
                            }
                            b_retract(sc_buf);
                        }
                        b_retract(sc_buf);
                    }
                    b_retract(sc_buf);
                } else if (c == 'O') {
                    if (b_getc(sc_buf) == 'R') {
                        if (b_getc(sc_buf) == '.') {
                            t.code = LOG_OP_T;
                            t.attribute.log_op = OR;
                            return t;
                        }
                        b_retract(sc_buf);
                    }
                    b_retract(sc_buf);
                }
                b_retract(sc_buf);
                t.code = ERR_T; /* if not .AND. or .OR., then it is an error */
                strcpy(t.attribute.err_lex, ".");
                return t;
            case '!':
                c = b_getc(sc_buf);
				/* check for second ! */
                if (c == '!') {
                    c = b_getc(sc_buf);
                    while (c != '\n' && c != '\0')
                        c = b_getc(sc_buf); /* get to the end of the line or file */
                    b_retract(sc_buf);
                    continue;
                } else {
					/* not a valid comment */
					t.attribute.err_lex[0] = '!';
                    t.attribute.err_lex[1] = c;
					t.attribute.err_lex[2] = '\0';
                    while (c != '\n' && c != '\0')
                        c = b_getc(sc_buf); /* get to the end of the line or file */
                    t.code = ERR_T;
                    b_retract(sc_buf);
                    return t;
                }
            case '=':
                c = b_getc(sc_buf);
                if (c == '=') {
					/* == compare */
                    t.code = REL_OP_T;
                    t.attribute.rel_op = EQ;
                } else {
					/* = assignment */
                    b_retract(sc_buf);
                    t.code = ASS_OP_T;
                }
                return t;
            case '>':
                t.code = REL_OP_T;
                t.attribute.rel_op = GT;
                return t;
            case '<':
                c = b_getc(sc_buf);
                if (c == '>') {
					/* <> not equal */
                    t.code = REL_OP_T;
                    t.attribute.rel_op = NE;
                } else if (c == '<') {
					/* << string concatenation */
                    t.code = SCC_OP_T;
                } else {
					/* < less than */
                    b_retract(sc_buf);
                    t.code = REL_OP_T;
                    t.attribute.rel_op = LT;
                }
                return t;
        }

		/* Part 2: Implementation of transition table */

		/* we already have the char so we must mark the previous position */
        lexstart = b_mark(sc_buf, (short)(b_getcoffset(sc_buf) - 1));

		/* run through the transition table until we get an error or accepting state */
        while (1) {
            state = get_next_state(state, (char)c);
			/* validate valid state */
			if (state < 0 || state > (TABLE_ROWS - 1)) {
				return runtime_error(SOR);
			}

            state_type = as_table[state];
			/* validate valid state type */
			if (state_type < NOAS || state_type > ASWR) {
				return runtime_error(WST);
			}

			/* stop checking if error or accepting state */
            if (state_type == NOAS) {
                c = b_getc(sc_buf);
            } else {
                break;
            }
        }

        if (state_type == ASWR) {
			/* return the last char to the buffer */
            b_retract(sc_buf);
        }

        lexend = b_getcoffset(sc_buf);
        lex_length = (lexend - lexstart);

		/* fixed buffer since we know the size of the lexeme */
        lex_buf = b_allocate(lex_length, 0, 'f');
        b_reset(sc_buf); /* return to the start of the lexeme */

		/* add the lexeme to the buffer */
        for (; lex_length > 0; lex_length--) {
            char tmp = b_getc(sc_buf);
            if (tmp == '\n') line++;
            b_addc(lex_buf, tmp);
        }
		b_compact(lex_buf, '\0');

		/* get the appropriate state function */
		b_mark(lex_buf, 0);
        t = aa_table[state](b_location(lex_buf));

        b_free(lex_buf);
        return t;
    } /* end while(1) */
}

/*
Purpose: Gets the next transition table state
Author: Svillen Ranev
History/Versions: 1.19.2	2019/10/02
Called functions: char_class()
Parameters: state (int) - the current state
			c (char) - the char to use to get the next state
Return value: int - the next state value
*/
int get_next_state(int state, char c) {
    int col;
    int next;
    col = char_class(c);
    next = st_table[state][col];

#ifdef DEBUG
    printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

    assert(next != IS);

#ifdef DEBUG
    if (next == IS) {
        printf("Scanner Error: Illegal state:\n");
        printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
        exit(1);
    }
#endif

    return next;
}

/*
Purpose: Gets the next transition table column for the char
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: None		  
Parameters: c (char) - the char to use to get the next column
Return value: int - the next column index
*/
int char_class(char c) {
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
        return 0;
    } else if (c == '0') {
        return 1;
    } else if (c >= '1' && c <= '9') {
        return 2;
    } else if (c == '.') {
        return 3;
    } else if (c == '@') {
        return 4;
    } else if (c == '"') {
        return 5;
    } else if (c == (char)SEOF) {
        return 6;
    } else {
        return 7;
    }
}

/* ACCEPTING FUNCTIONS */

/*
Purpose: Accepting function for keywords (KW) and arithmetic variable
		 identifiers (VID - AVID)
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: strcpy()
			  	  strncpy()
				  strcmp()
				  strlen()
Parameters: lexeme[] (char) - string containing the lexeme to identify
Return value: Token - token value containing lexeme information 
*/
Token aa_func02(char lexeme[]) {
	Token t = { 0 }; /* token to return */
    int i; /* index of keyword array */

	/* check for keyword first */
    for (i = 0; i < KWT_SIZE; i++) {
        if (strcmp(kw_table[i], lexeme) == 0) {
            t.code = KW_T;
            t.attribute.kwt_idx = i;
            return t;
        }
    }

    t.code = AVID_T;
	/* check if lexeme is too big for the VID */
    if (strlen(lexeme) > VID_LEN) {
        strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
        t.attribute.vid_lex[VID_LEN] = '\0';
    } else {
        strcpy(t.attribute.vid_lex, lexeme);
    }

    return t;
}

/*
Purpose: Accepting function for string variable identifier (VID - SVID)
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: strcpy()
			  	  strncpy()
				  strlen()
Parameters: lexeme[] (char) - string containing the lexeme to identify
Return value: Token - token value containing lexeme information 
*/
Token aa_func03(char lexeme[]) {
	Token t = { 0 }; /* token to return */

    t.code = SVID_T;
	/* check if lexeme is too big for the VID */
    if (strlen(lexeme) > (VID_LEN - 1)) {
        strncpy(t.attribute.vid_lex, lexeme, (VID_LEN - 1));
        t.attribute.vid_lex[VID_LEN - 1] = '@';
        t.attribute.vid_lex[VID_LEN] = '\0';
    } else {
        strcpy(t.attribute.vid_lex, lexeme);
    }

    return t;
}

/*
Purpose: Accepting function for floating-point literal (FPL)
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: atof()
			  	  strcpy_overflow()
Parameters: lexeme[] (char) - string containing the lexeme to identify
Return value: Token - token value containing lexeme information 
*/
Token aa_func08(char lexeme[]) {
    Token t = { 0 }; /* token to return */
    double d; /* conversion of lexeme */

	/* transistion table ensures this will work */
    d = atof(lexeme);

	/* check if value is valid */
	if ((d > FLT_MAX || d < FLT_MIN) && d != 0) {
		/* doesn't fit in 4-byte float */
        t.code = ERR_T;
        strcpy_overflow(t.attribute.err_lex, lexeme, ERR_LEN);
	}
	else {
        t.code = FPL_T;
		t.attribute.flt_value = (float)d;
	}

    return t;
}

/*
Purpose: Accepting function for integer literal(IL) - decimal constant (DIL)
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: atol()
			  	  strcpy_overflow()
Parameters: lexeme[] (char) - string containing the lexeme to identify
Return value: Token - token value containing lexeme information 
*/
Token aa_func05(char lexeme[]) {
    Token t = { 0 }; /* token to return */
    long l; /* conversion of lexeme */

	/* transistion table ensures this will work */
    l = atol(lexeme);

	/* check if value is valid, must be positive */
    if (l > SHRT_MAX) {
		/* doesn't fit in 2-byte int */
        t.code = ERR_T;
        strcpy_overflow(t.attribute.err_lex, lexeme, ERR_LEN);
    } else {
        t.code = INL_T;
        t.attribute.int_value = (int) l;
    }

    return t;
}

/*
Purpose: Accepting function for string literal (SL)
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: b_limit()
				  strlen()
			  	  b_addc()
Parameters: lexeme[] (char) - string containing the lexeme to identify
Return value: Token - token value containing lexeme information 
*/
Token aa_func10(char lexeme[]) {
    Token t = { 0 }; /* token to return */
    size_t i; /* index of lexeme */

    t.code = STR_T;
	/* store the current buffer position so that we can retrieve it later */
    t.attribute.get_int = b_limit(str_LTBL);

    for (i = 1; i < (strlen(lexeme) - 1); i++) {
        b_addc(str_LTBL, lexeme[i]);
    }
    b_addc(str_LTBL, '\0');

    return t;
}

/*
Purpose: Accepting function for error token (ES & ER)
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: strcpy_overflow()
Parameters: lexeme[] (char) - string containing the lexeme to identify
Return value: Token - token value containing lexeme information 
*/
Token aa_func12(char lexeme[]) {
    Token t = { 0 }; /* token to return */

    t.code = ERR_T;
    strcpy_overflow(t.attribute.err_lex, lexeme, ERR_LEN);

    return t;
}

/* ADDITIONAL FUNCTIONS */

/*
Purpose: Copies string and inserts "..." to the end if it is 
		 too long to fit in the dest
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: strlen()
				  strncpy()
				  strcpy()
Parameters: dest[] (char) - destination to copy the string to
			src[] (char) - source to copy the string from
			limit (int) - length limit of dest
Return value: None
*/
void strcpy_overflow(char dest[], char src[], size_t limit) {
    if (strlen(src) > limit) {
        strncpy(dest, src, limit - 3);
        dest[limit - 3] =  '.';
        dest[limit - 2] =  '.';
        dest[limit - 1] =  '.';
        dest[limit] = '\0';
    } else {
        strcpy(dest, src);
    }
}

/*
Purpose: Return a runtime error token with the specified code
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/11/12
Called functions: strcpy()
Parameters: code (int) - the run time error code
Return value: Token - token with the run time error info
*/
Token runtime_error(int code) {
	Token t = { 0 };
	t.code = RTE_T;
	scerrnum = code;
	strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
	return t;
}

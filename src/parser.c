/*
File name: parser.c
Compiler: MS Visual Studio 2019
Author: Nicholas Sturgeon, 040911218
Course: CST 8152 – Compilers, Lab Section: 013 Assignment: 3
Date: 2019/12/05
Professor: Sv. Ranev
Purpose: Implementation file for the parser containing function definitions.
		 Parses a file witten in the PLATYPUS language.
Function list:
	parser();
	program();
	opt_statements();
	statements();
	statements_p();
	statement();
	input_statement();
	output_statement();
	assignment_statement();
	assignment_expression();
	string_expression();
	string_expression_p();
	primary_string_expression();
	arithmetic_expression();
	arithmetic_expression_unary();
	arithmetic_expression_primary();
	arithmetic_expression_additive();
	arithmetic_expression_additive_p();
	arithmetic_additive_operator();
	arithmetic_expression_multiplicative();
	arithmetic_expression_multiplicative_p();
	arithmetic_multiplicative_operator();
	variable_list();
	variable_list_p();
	output_list();
	selection_statement();
	iteration_statement();
	pre_condition();
	conditional_expression();
	logical_or_expression();
	logical_or_expression_p();
	logical_and_expression();
	logical_and_expression_p();
	relational_expression();
	relational_operator();
	primary_a_relational_expression();
	primary_s_relational_expression();
	gen_incode();
	match();
	syn_printe();
	syn_eh();
*/

#include <stdio.h>

#include "parser.h"
#include "token.h"
#include "buffer.h"

extern Token malar_next_token();
extern pBuffer str_LTBL;
extern int line;

extern char * kw_table[];

Token lookahead;
int synerrno;

/*
Purpose: Gets the next Token from the scanner buffer and
		 begins parsing the program
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/12/05
Called functions: malar_next_token()
				  program()
				  match()
				  gen_incode()
				  
Parameters: None
Return value: None
*/
void parser() {
    lookahead = malar_next_token();
    program();
    match(SEOF_T, NO_ATTR);
    gen_incode("PLATY: Source file parsed");
}

/* <program> -> PLATYPUS {<opt_statements>} */
/* FIRST(<program>) = { KW_T(PLATYPUS) } */
void program() {
    match(KW_T, PLATYPUS);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    gen_incode("PLATY: Program parsed");
}

/* <opt_statements> -> statements | ε */
/* FIRST(<opt_statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ε } */
void opt_statements() {
    switch(lookahead.code) {
        case AVID_T:
        case SVID_T:
            statements();
            break;
        case KW_T:
            /* check for IF, WHILE, READ, WRITE and in statements_p() */
            if (lookahead.attribute.get_int == IF
                || lookahead.attribute.get_int == WHILE
                || lookahead.attribute.get_int == READ
                || lookahead.attribute.get_int == WRITE) {
                statements();
                break;
            }
        default: /* empty string – optional statements */
            gen_incode("PLATY: Opt_statements parsed");
    }
}

/* <statements> -> <statement> <statements’> */
/* FIRST(<statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) } */
void statements() {
    statement();
    statements_p();
}

/* <statements’> -> <statement> <statements’> | ε */
/* FIRST(<statements’>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ε } */
void statements_p() {
    switch (lookahead.code) {
        case AVID_T:
        case SVID_T:
            statement();
            statements_p();
            break;
        case KW_T:
            /* check for IF, WHILE, READ, WRITE and in statements_p() */
            if (lookahead.attribute.get_int == IF
                || lookahead.attribute.get_int == WHILE
                || lookahead.attribute.get_int == READ
                || lookahead.attribute.get_int == WRITE) {
                statement();
                statements_p();
                break;
            }
        default: /* nothing */;
    }
}

/* <statement> -> <assignment statement> | <selection statement> | <iteration statement> | <input statement> | <output statement> */
/* FIRST(<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) } */
void statement() {
    switch (lookahead.code) {
        case AVID_T:
        case SVID_T:
            assignment_statement();
            break;
        case KW_T:
            switch (lookahead.attribute.get_int) {
                case READ:
                    input_statement();
                    break;
                case WHILE:
                    iteration_statement();
                    break;
                case IF:
                    selection_statement();
                    break;
                case WRITE:
                    output_statement();
                    break;
            }
            break;
        default:
			syn_printe();
    }
}

/* <input statement> -> READ (<variable list>); */
/* FIRST(<input statement>) = { KW_T(READ) } */
void input_statement() {
    match(KW_T, READ);
    match(LPR_T, NO_ATTR);
    variable_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Input statement parsed");
}

/* <output statement> -> WRITE (<output list>); */
/* FIRST(<output statement>) = { KW_T(WRITE) } */
void output_statement() {
    match(KW_T, WRITE);
    match(LPR_T, NO_ATTR);
    output_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Output statement parsed");
}

/* <assignment statement> -> <assignment expression>; */
/* FIRST(<assignment statement>) = { AVID_T, SVID_T } */
void assignment_statement() {
    assignment_expression();
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Assignment statement parsed");
}

/* <assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression> */
/* FIRST(<assignment expression>) = { AVID_T, SVID_T } */
void assignment_expression() {
    switch (lookahead.code) {
        case AVID_T:
            match(AVID_T, NO_ATTR);
            match(ASS_OP_T, NO_ATTR);
            arithmetic_expression();
            gen_incode("PLATY: Assignment expression (arithmetic) parsed");
            break;
        case SVID_T:
            match(SVID_T, NO_ATTR);
            match(ASS_OP_T, NO_ATTR);
            string_expression();
            gen_incode("PLATY: Assignment expression (string) parsed");
            break;
        default:
			syn_printe();
    }
}

/* <string expression> -> <primary string expression> <string expression’> */
/* FIRST(<string expression> = { SVID_T, STR_T } */
void string_expression() {
    primary_string_expression();
    string_expression_p();
    gen_incode("PLATY: String expression parsed");
}

/* <string expression’> -> << <primary string expression> <string expression’> | ε */
/* FIRST(<string expression’>) = { SCC_OP_T, ε } */
void string_expression_p() {
    if (lookahead.code == SCC_OP_T) {
        match(SCC_OP_T, NO_ATTR);
        primary_string_expression();
        string_expression_p();
    }
}

/* <primary string expression> -> SVID_T | STR_T */
/* FIRST(<primary string expression>) = { SVID_T, STR_T } */
void primary_string_expression() {
    switch (lookahead.code) {
        case SVID_T:
            match(SVID_T, NO_ATTR);
            break;
        case STR_T:
            match(STR_T, NO_ATTR);
            break;
        default:
			syn_printe();
    }
    gen_incode("PLATY: Primary string expression parsed");
}

/* <arithmetic expression> -> <unary arithmetic expression> | <additive arithmetic expression> */
/* FIRST(<arithmetic expression>) = { ART_OPT_T(PLUS), ART_OPT_T(MINUS), AVID_T, FPL_T, INL_T, LPR_T } */
void arithmetic_expression() {
    switch (lookahead.code) {
        case ART_OP_T:
            arithmetic_expression_unary();
            break;
        case AVID_T:
        case INL_T:
        case FPL_T:
        case LPR_T:
            arithmetic_expression_additive();
            break;
        default:
			syn_printe();
    }
    gen_incode("PLATY: Arithmetic expression parsed");
}

/* <unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression> */
/* FIRST(<unary arithmetic expression>) = { ART_OPT_T(PLUS), ART_OPT_T(MINUS) } */
void arithmetic_expression_unary() {
    switch (lookahead.attribute.get_int) {
        case PLUS:
            match(ART_OP_T, PLUS);
            break;
        case MINUS:
            match(ART_OP_T, MINUS);
            break;
        default:
			syn_printe();
    }
    arithmetic_expression_primary();
    gen_incode("PLATY: Unary arithmetic expression parsed");
}

/* <primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>) */
/* FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T } */
void arithmetic_expression_primary() {
    switch (lookahead.code) {
        case AVID_T:
            match(AVID_T, NO_ATTR);
            break;
        case FPL_T:
            match(FPL_T, NO_ATTR);
            break;
        case INL_T:
            match(INL_T, NO_ATTR);
            break;
        case LPR_T:
            match(LPR_T, NO_ATTR);
            arithmetic_expression();
            match(RPR_T, NO_ATTR);
            break;
        default:
			syn_printe();
    }
    gen_incode("PLATY: Primary arithmetic expression parsed");
}

/* <additive arithmetic expression> -> <multiplicative arithmetic expression> <additive arithmetic expression’> */
/* FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T } */
void arithmetic_expression_additive() {
    arithmetic_expression_multiplicative();
    arithmetic_expression_additive_p();
}

/* <additive arithmetic expression’> -> <additive arithmetic operator> <additive arithmetic expression’> | ε */
/* FIRST(<additive arithmetic expression’>) = { ART_OPT_T(PLUS), ART_OPT_T(MINUS), ε } */
void arithmetic_expression_additive_p() {
    if (lookahead.code == ART_OP_T) {
        if (lookahead.attribute.get_int == PLUS
            || lookahead.attribute.get_int == MINUS) {
            arithmetic_additive_operator();
            arithmetic_expression_additive_p();
            gen_incode("PLATY: Additive arithmetic expression parsed");

        }
    }
}

/* <additive arithmetic operator> -> + <multiplicative arithmetic expression> | - <multiplicative arithmetic expression> */
/* FIRST(<additive arithmetic operator>) = { ART_OPT_T(PLUS), ART_OPT_T(MINUS) } */
void arithmetic_additive_operator() {
    switch (lookahead.attribute.get_int) {
        case PLUS:
            match(ART_OP_T, PLUS);
            arithmetic_expression_multiplicative();
            break;
        case MINUS:
            match(ART_OP_T, MINUS);
            arithmetic_expression_multiplicative();
            break;
        default:
			syn_printe();
    }
}

/* <multiplicative arithmetic expression> -> <primary arithmetic expression> <multiplicative arithmetic expression’> */
/* FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T } */
void arithmetic_expression_multiplicative() {
    arithmetic_expression_primary();
    arithmetic_expression_multiplicative_p();
}

/* <multiplicative arithmetic expression’> -> <multiplicative arithmetic operator> <multiplicative arithmetic expression’> | ε */
/* FIRST(<multiplicative arithmetic expression’>) = { ART_OPT_T(MULT), ART_OPT_T(DIV), ε } */
void arithmetic_expression_multiplicative_p() {
    if (lookahead.code ==  ART_OP_T) {
        if (lookahead.attribute.get_int == MULT
            || lookahead.attribute.get_int == DIV) {
            arithmetic_multiplicative_operator();
            arithmetic_expression_multiplicative_p();
            gen_incode("PLATY: Multiplicative arithmetic expression parsed");
        }
    }
}

/* <multiplicative arithmetic operator> -> * <primary arithmetic expression> | / <primary arithmetic expression> */
/* FIRST(<multiplicative arithmetic operator>) = { ART_OPT_T(MULT), ART_OPT_T(DIV) } */
void arithmetic_multiplicative_operator() {
    switch (lookahead.attribute.get_int) {
        case MULT:
            match(ART_OP_T, MULT);
            arithmetic_expression_primary();
            break;
        case DIV:
            match(ART_OP_T, DIV);
            arithmetic_expression_primary();
            break;
        default:
			syn_printe();
    }
}

/* <variable list> -> <variable identifier> <variable list’> */
/* FIRST(<variable list>) = { AVID_T, SVID_T } */
void variable_list() {
    switch (lookahead.code) {
        case AVID_T:
            match(AVID_T, NO_ATTR);
            variable_list_p();
            break;
        case SVID_T:
            match(SVID_T, NO_ATTR);
            variable_list_p();
            break;
        default:
			syn_printe();
    }
    gen_incode("PLATY: Variable list parsed");
}

/* <variable list’> -> ,<variable identifier> <variable list’> | ε */
/* FIRST(<variable list’>) = { COM_T, ε } */
void variable_list_p() {
    if (lookahead.code == COM_T) {
        match(COM_T, NO_ATTR);
        switch (lookahead.code) {
            case AVID_T:
                match(AVID_T, NO_ATTR);
                variable_list_p();
                break;
            case SVID_T:
                match(SVID_T, NO_ATTR);
                variable_list_p();
                break;
            default:
				syn_printe();
        }
    }
}

/* <output list> -> <opt variable list> | STR_T */
/* FIRST(<output list>) = { STR_T, AVID_T, SVID_T, ε } */
void output_list() {
    switch (lookahead.code) {
        case STR_T:
            match(STR_T, NO_ATTR);
            gen_incode("PLATY: Output list (string literal) parsed");
            break;
        case AVID_T:
        case SVID_T:
            variable_list();
            break;
        default:
            gen_incode("PLATY: Output list (empty) parsed");
    }
}

/* <selection statement> -> IF <pre-condition> (<conditional expression>) THEN { <opt_statements> } ELSE { <opt_statements> }; */
/* FIRST(<selection statement>) = { KW_T(IF) } */
void selection_statement() {
    match(KW_T, IF);
    pre_condition();
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Selection statement parsed");
}

/* <iteration statement> -> WHILE <pre-condition> (<conditional expression>) REPEAT {<statements>}; */
/* FIRST(<iteration statement>) = { KW_T(WHILE) } */
void iteration_statement() {
    match(KW_T, WHILE);
    pre_condition();
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Iteration statement parsed");
}

/* <pre-condition> -> TRUE | FALSE */
/* FIRST(<pre-condition>) = { KW_T(TRUE), KW_T(FALSE) } */
void pre_condition() {
    if (lookahead.code == KW_T) {
        switch (lookahead.attribute.get_int) {
            case TRUE:
                match(KW_T, TRUE);
                break;
            case FALSE:
                match(KW_T, FALSE);
                break;
            default:
				syn_printe();
        }
    } else {
		syn_printe();
    }
}

/* <conditional expression> -> <logical OR expression> */
/* FIRST(<conditional expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T } */
void conditional_expression() {
    logical_or_expression();
    gen_incode("PLATY: Conditional expression parsed");
}

/* <logical OR expression> -> <logical AND expression> <logical OR expression’> */
/* FIRST(<logical OR expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T } */
void logical_or_expression() {
    logical_and_expression();
    logical_or_expression_p();
}

/* <logical OR expression’> -> .OR. <logical AND expression> <logical OR expression’> | ε */
/* FIRST(<logical OR expression’>) = { LOG_OP_T(OR), ε } */
void logical_or_expression_p() {
    if (lookahead.code == LOG_OP_T
        && lookahead.attribute.get_int == OR) {
        match(LOG_OP_T, OR);
        logical_and_expression();
        logical_or_expression_p();
        gen_incode("PLATY: Logical OR expression parsed");
    }
}

/* <logical AND expression> -> <relational expression> <logical AND expression’> */
/* FIRST(<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T } */
void logical_and_expression() {
    relational_expression();
    logical_and_expression_p();
}

/* <logical AND expression’> -> .AND. <relational expression> <logical AND expression’> |  */
/* FIRST(<logical AND expression’>) = { LOG_OP_T(AND), ε } */
void logical_and_expression_p() {
    if (lookahead.code == LOG_OP_T
        && lookahead.attribute.get_int == AND) {
        match(LOG_OP_T, AND);
        relational_expression();
        logical_and_expression_p();
        gen_incode("PLATY: Logical AND expression parsed");
    }
}

/* <relational expression> -> <primary a_relational expression> <relational operator> <primary a_relational expression> 
		| <primary s_relational expression> <relational operator> <primary s_relational expression> */
/* FIRST(<relational expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T } */
void relational_expression() {
    switch (lookahead.code) {
        case AVID_T:
        case INL_T:
        case FPL_T:
            primary_a_relational_expression();
            relational_operator();
            primary_a_relational_expression();
            break;
        case SVID_T:
            primary_s_relational_expression();
            relational_operator();
            primary_s_relational_expression();
            break;
        default:
			syn_printe();
    }
    gen_incode("PLATY: Relational expression parsed");
}

/* <relational operator> -> == | <> | > | < */
/* FIRST(<relational_operator>) = { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT) } */
void relational_operator() {
    if (lookahead.code == REL_OP_T) {
        switch (lookahead.attribute.get_int) {
            case EQ:
                match(REL_OP_T, EQ);
                break;
            case NE:
                match(REL_OP_T, NE);
                break;
            case GT:
                match(REL_OP_T, GT);
                break;
            case LT:
                match(REL_OP_T, LT);
                break;
            default:
				syn_printe();
        }
    } else {
		syn_printe();
    }
}

/* <primary_a_relational_expression> -> AVID_T | FPL_T | INL_T */
/* FIRST(<primary_a_relational_expression>) -> { AVID_T, FPL_T, INL_T } */
void primary_a_relational_expression() {
    switch (lookahead.code) {
        case AVID_T:
            match(AVID_T, NO_ATTR);
            break;
        case FPL_T:
            match(FPL_T, NO_ATTR);
            break;
        case INL_T:
            match(INL_T, NO_ATTR);
            break;
        default:
			syn_printe();
    }
    gen_incode("PLATY: Primary a_relational expression parsed");
}

/* <primary s_relational expression> -> <primary string expression> */
/* FIRST(<primary s_relational_expression>) = { SVID_T, STR_T } */
void primary_s_relational_expression() {
    primary_string_expression();
    gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
Purpose: Prints the parsed statement or expression
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/12/05
Called functions: printf()
				  
Parameters: out - the parsed statement or expression to display
Return value: None
*/
void gen_incode(const char* out) {
    printf("%s\n", out);
}

/*
Purpose: Matches the lookahead token to the specified token,
		 or displays a syntax error if not a match
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/12/05
Called functions: malar_next_token()
				  syn_printe()
				  syn_eh()
				  
Parameters: pr_token_code - the token code to match with
			pr_token_attribute - the token attribute
				value to match with, if applicable
Return value: None
*/
void match(int pr_token_code, int pr_token_attribute) {
    int match = pr_token_code == lookahead.code;
    if (match && (pr_token_code == KW_T
        || pr_token_code == LOG_OP_T
        || pr_token_code == ART_OP_T
        || pr_token_code == REL_OP_T)) {
        match = match && (pr_token_attribute == lookahead.attribute.get_int);
    }

    if (match) {
        if (lookahead.code != SEOF_T) {
            lookahead = malar_next_token();
            if (lookahead.code == ERR_T) {
                syn_printe();
                lookahead = malar_next_token();
                synerrno++;
            }
        }
    } else {
        syn_eh(pr_token_code);
    }
}

/*
Purpose: Prints the syntax error of the lookahead token
Author: Svillen Ranev
History/Versions: 1.0	2019/12/05
Called functions: printf()
				  
Parameters: None
Return value: None
*/
void syn_printe() {
    Token t = lookahead;

    printf("PLATY: Syntax error:  Line:%3d\n", line);
    printf("*****  Token code:%3d Attribute: ", t.code);
    switch(t.code) {
        case ERR_T: /* ERR_T     0   Error token */
            printf("%s\n", t.attribute.err_lex);
            break;
        case SEOF_T: /*SEOF_T    1   Source end-of-file token */
            printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
            break;
        case AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
        case SVID_T :/* SVID_T    3  String Variable identifier token */
            printf("%s\n", t.attribute.vid_lex);
            break;
        case FPL_T: /* FPL_T     4  Floating point literal token */
            printf("%5.1f\n", t.attribute.flt_value);
            break;
        case INL_T: /* INL_T      5   Integer literal token */
            printf("%d\n", t.attribute.get_int);
            break;
        case STR_T: /* STR_T     6   String literal token */
            b_mark(str_LTBL, t.attribute.str_offset);
            printf("%s\n", b_location(str_LTBL));
            break;
        case SCC_OP_T: /* 7   String concatenation operator token */
            printf("NA\n");
            break;
        case ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
            printf("NA\n");
            break;
        case ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
            printf("%d\n", t.attribute.get_int);
            break;
        case REL_OP_T: /* REL_OP_T  10   Relational operator token */
            printf("%d\n", t.attribute.get_int);
            break;
        case LOG_OP_T:/* LOG_OP_T 11  Logical operator token */
            printf("%d\n", t.attribute.get_int);
            break;
        case LPR_T: /* LPR_T    12  Left parenthesis token */
            printf("NA\n");
            break;
        case RPR_T: /* RPR_T    13  Right parenthesis token */
            printf("NA\n");
            break;
        case LBR_T: /*    14   Left brace token */
            printf("NA\n");
            break;
        case RBR_T: /*    15  Right brace token */
            printf("NA\n");
            break;
        case KW_T: /*     16   Keyword token */
            printf("%s\n", kw_table[t.attribute.get_int]);
            break;
        case COM_T: /*    17   Comma token */
            printf("NA\n");
            break;
        case EOS_T: /*    18  End of statement *(semi - colon) */
            printf("NA\n");
            break;
        default:
            printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }
}

/*
Purpose: Prints the syntax error and tries to find the next
		 occurence of the specified token
Author: Nicholas Sturgeon
History/Versions: 1.0	2019/12/05
Called functions: syn_printe()
				  malar_next_token()
				  exit()
				  
Parameters: sync_token_code - the token to skip ahead to
Return value: None
*/
void syn_eh(int sync_token_code) {
	synerrno++;
	syn_printe();

    while (lookahead.code != SEOF_T
        && lookahead.code != sync_token_code) {
        lookahead = malar_next_token();
    }

    if (lookahead.code == sync_token_code) {
        if (lookahead.code != SEOF_T) {
            lookahead = malar_next_token();
        }
    } else {
        exit(synerrno);
    }
}

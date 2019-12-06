/*
File name: parser.h
Compiler: MS Visual Studio 2019
Author: Nicholas Sturgeon, 040911218
Course: CST 8152 – Compilers, Lab Section: 013 Assignment: 3
Date: 2019/12/05
Professor: Sv. Ranev
Purpose: Header file for the parser containing function declarations
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

#ifndef PARSER_H_
#define PARSER_H_

void parser();

void program();

void opt_statements();
void statements();
void statements_p();
void statement();

void input_statement();
void output_statement();

void assignment_statement();
void assignment_expression();

void string_expression();
void string_expression_p();
void primary_string_expression();

void arithmetic_expression();
void arithmetic_expression_unary();
void arithmetic_expression_primary();

void arithmetic_expression_additive();
void arithmetic_expression_additive_p();
void arithmetic_additive_operator();

void arithmetic_expression_multiplicative();
void arithmetic_expression_multiplicative_p();
void arithmetic_multiplicative_operator();

void variable_list();
void variable_list_p();
void output_list();

void selection_statement();
void iteration_statement();
void pre_condition();

void conditional_expression();
void logical_or_expression();
void logical_or_expression_p();
void logical_and_expression();
void logical_and_expression_p();

void relational_expression();
void relational_operator();
void primary_a_relational_expression();
void primary_s_relational_expression();

void gen_incode(const char*);

void match(int, int);

void syn_printe();
void syn_eh(int);

#endif

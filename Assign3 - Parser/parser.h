/*************************************************************************************************************************************
File name			: parser.h
Compiler			: MS Visual Studio 2015
Author				: Kevin Lai, 040812704  Xingyi Wu, 04887028
Course				: CST 8152 - Compilers, Lab Section: 12, 14
Assignment			: 3
Date				: December 6th, 2018
Professor			: Sv. Ranev
Purpose				: Integrate the Parser with your existing lexical analyzer and symbol table
                      in order to complete the front-end of your PLATYPUS compiler
Function list		: parser(); match(); syn_eh(); syn_printe(); gen_incode(); program(); opt_statements();
					  statements(); statement(); statements_p(); assignment_statement(); assignment_expression();
					  selection_statement(); iteration_statement(); pre_condition(); input_statement(); variable_list();
					  variable_listp(); variable_identifier(); output_statement(); output_list(); arithmetic_expression();
					  unary_arithmetic_expression(); additive_arithmetic_expression(); additive_arithmetic_expression_p();
					  multiplicative_arithmetic_expression(); multiplicative_arithmetic_expression_p();
					  primary_arithmetic_expression(); string_expression(); string_expression_p(); primary_string_expression();
					  conditional_expression(); logical_OR_expression(); logical_OR_expression_p(); logical_AND_expression();
					  logical_AND_expression_p(); relational_expression(); primary_a_relational_expression_p();
					  primary_a_relational_expression(); primary_s_relational_expression(); primary_s_relational_expression_p();
*************************************************************************************************************************************/

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#define NO_ATTR -1
#define ELSE 0	
#define FALSE 1 
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

static Token lookahead; /* current input token */
int synerrno; /* number of syntax errors */

/* external object */
extern Token malar_next_token(void); /* token regconition reported from scanner */
extern int line; /* source code line number - defined in scanner.c */
extern Buffer * str_LTBL; /* this buffer implements String Literal Table */
extern char * kw_table[]; /* Keyword lookup table - defined in table.h */

/* function declarations(prototypes) */
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* ch);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_p(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void input_statement(void);
void variable_list(void);
void variable_listp(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void primary_a_relational_expression_p(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void primary_s_relational_expression_p(void);

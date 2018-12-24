/*************************************************************************************************************************************
File name			: parser.c
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

#include "parser.h"
#include <stdlib.h>

/******************************************************************************************************************************************
Purpose				: The function is to parse PLATYPUS program
Author				: Svillen Ranev
Versions			: 1.0
Called Functions	: none
Parameters			: none
Return value		: none
Algorithm			: - calling malar_next_token() to fetch a token then call program function
					  - check for end of source file by matching SEOF_T	
*****************************************************************************************************************************************/
void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*********************************************************************************************************************************************
Purpose				: The function is match current token and the token required by parser and check if they are match.
Author				: Kevin, Xingyi
Versions			: 1.0
Called Functions	: syn_eh(); malar_next_token(); syn_printe()
Parameters			: pr_token_code: int - required token by parser
					  pr_token_attribute: int - required token attribute by parser
Return value		: none
Algorithm			: - compare current input token code with the required token code, if not match, print error by calling syn_eh and return
					  - if match then check if current token is SEOF_T, if yes, then return, if not, advance to next input token
					  - if new is error token, print error by calling function syn_printe(), and advance to next input token again,
					  - increments the error counter and return.
**********************************************************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {

	/*match is unsuccessful, calls the error hander and returns*/
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code); return;
	}

	/*match is successful and token code is SEOF*/
	if (lookahead.code == SEOF_T) return;

	switch (pr_token_code) {
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code); return;
		}
		break;
	}

	/*match and successful and the lookahead is not SEOF_T*/
	lookahead = malar_next_token();

	/*new lookahead token is ERR_T */
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}/*end function match*/

 /*********************************************************************************************************************************************
 Purpose		    : The function implements a panic mode error recovery
 Author				: Xingyi
 Versions			: 1.0
 Called Functions	: syn_printe(); malar_next_token(); exit();
 Parameters			: sync_token_code: int - required token by parser
 Return value		: none
 Algorithm			: - calls syn_printe() and increments the error counter.
					  - go into a loop and advances the token forward by one and then checks if lookahead token is SEOF_T
					  - if the lookahead token is SEOF_T and the sync_token_code that were trying to match with is not SEOF_T then we exit()
						with synerrno passed as a argument. otherwise sync_token_code is SEOF_T which at that point we return;
					  - while the lookahead.code is not equal to the sync_token_code we iterator through the loop again.
					  - if the lookahead token matches with the sync_token_code and is not SEOF then we advance the token forward and return
 **********************************************************************************************************************************************/
void syn_eh(int sync_token_code) {

	syn_printe();
	synerrno++;

	/* advancing next input and check */
	do {
		lookahead = malar_next_token();

		if (lookahead.code == SEOF_T) {
			if (sync_token_code != SEOF_T)
				exit(synerrno);
			else return;
		}
	} while (lookahead.code != sync_token_code);

	lookahead = malar_next_token();
	return;
}

/*********************************************************************************************************************************************
Purpose		        : error printing function
Author				: Sv. Ranev
Versions			: 1.0
Called Functions	: printf(); 
Parameters			: none
Return value		: none
**********************************************************************************************************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
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

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

 /*********************************************************************************************************************************************
 Purpose		    : The function takes a string as an argument and prints it
 Author				: Xingyi
 Versions			: 1.0
 Called Functions	: none
 Parameters			: ch: char* - a sequence of character as a string
 Return value		: none
 Algorithm			:
 **********************************************************************************************************************************************/
void gen_incode(char* ch) {
	printf("%s\n", ch);
}

/*********************************************************************************************************************************************
Purpose				: <program> -> PLATYPUS {<opt_statements>}
                      FIRST(<program>) -> { PLATYPUS }
Author				: Sv. Ranev
Versions			: 1.0
Called Functions	: match(); opt_statements(); gen_incode();
Algorithm			: none
**********************************************************************************************************************************************/
void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*********************************************************************************************************************************************
Purpose				: <opt_statements> - > <statements> | e
					  FIRST(<opt_statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE),e }
Author				: Sv. Ranev
Versions			: 1.0
Called Functions	: statements(); gen_incode();
**********************************************************************************************************************************************/
void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statements(); break;
		}
	default: /* empty string – optional statements */
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*********************************************************************************************************************************************
Purpose				: <statements> -> <statement><statements’>
					  FIRST (<statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
Author				: Kevin
Versions			: 1.0
Called Functions	: statement(); statements_p();
**********************************************************************************************************************************************/
void statements(void) {
	statement();
	statements_p();
}

/*********************************************************************************************************************************************
Purpose				: <statement> -> <assignment statement>|<selection statement>|<iteration statement>|<input statement>|<output statement>
					  FIRST (<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
Author				: Xingyi
Versions			: 1.0
Called Functions	: assignment_statement(); selection_statement(); iteration_statement(); input_statement(); output_statement(); syn_printe();
**********************************************************************************************************************************************/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF: selection_statement(); break;
		case WHILE: iteration_statement(); break;
		case READ: input_statement(); break;
		case WRITE: output_statement(); break;
		}
		break;
	default: syn_printe(); break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <statements’> -> <statement><statements’> | e
					  FIRST (<statements’>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE),e }
Author				: Kevin
Versions			: 1.0
Called Functions	: statements();
**********************************************************************************************************************************************/
void statements_p(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:statement(); statements_p();
		break;
	case KW_T: if (lookahead.attribute.get_int != PLATYPUS
		&& lookahead.attribute.get_int != ELSE
		&& lookahead.attribute.get_int != REPEAT
		&& lookahead.attribute.get_int != THEN
		&& lookahead.attribute.get_int != TRUE
		&& lookahead.attribute.get_int != FALSE) {
		statement(); statements_p();
		break;
	}
			   break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <assignment statement> -> <assignment expression>;
					  FIRST(<assignment statement>) = { AVID_T, SVID_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: assignment_expression(); match(); gen_incode();
**********************************************************************************************************************************************/
void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*********************************************************************************************************************************************
Purpose				: <assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
					  FIRST(<assignment statement>) = { AVID_T, SVID_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); arithmetic_expression(); string_expression(); gen_incode(); syn_printe();
**********************************************************************************************************************************************/
void assignment_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR); /*call match to advance lookahead*/
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe(); break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <selection statement> ->
					  IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
					  ELSE { <opt_statements> } ;
					  FIRST (<selection statement>) = { KW_T(IF) }
Author				: Xingyi
Versions			: 1.0
Called Functions	: match(); pre_condition(); conditional_expression(); opt_statements(); gen_incode();
**********************************************************************************************************************************************/
void selection_statement(void) {
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

/*********************************************************************************************************************************************
Purpose				: <iteration statement> ->
					  WHILE <pre-condition> (<conditional expression>)
					  REPEAT { <statements>};
					  FIRST (<iteration statement>) = { KW_T(WHILE) }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); pre_condition(); conditional_expression(); statements(); gen_incode();
**********************************************************************************************************************************************/
void iteration_statement(void) {
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

/*********************************************************************************************************************************************
Purpose				: <pre-condition> ->
					  TRUE | FALSE
					  FIRST(<pre-condition>) = { KW_T(TRUE), KW_T(FALSE) }
Author				: Xingyi
Versions			: 1.0
Called Functions	: match(); syn_printe();
**********************************************************************************************************************************************/
void pre_condition(void) {
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case TRUE: match(KW_T, TRUE);
			break;
		case FALSE: match(KW_T, FALSE);
			break;
		default: syn_printe(); break;
		}
		break;
	default: syn_printe(); break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <input statement> ->
					  READ (<variable list>);
					  FIRST (<input statement>) = { KW_T(READ) }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); variable_list(); gen_incode();
**********************************************************************************************************************************************/
void input_statement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*********************************************************************************************************************************************
Purpose				: <variable list> -> <variable identifier> <variable list’>
					  FIRST (<variable list>) = { AVID_T, SVID_T }
Author				: Xingyi
Versions			: 1.0
Called Functions	: variable_identifier(); variable_listp(); gen_incode();
**********************************************************************************************************************************************/
void variable_list(void) {
	variable_identifier();
	variable_listp();
	gen_incode("PLATY: Variable list parsed");
}

/*********************************************************************************************************************************************
Purpose				: <variable list’> -> ,<variable identifier> <variable list’> |  e
					  FIRST (<variable list’>) = { COM_T, e }
Author				: Xingyi
Versions			: 1.0
Called Functions	: match(); variable_identifier(); variable_listp();
**********************************************************************************************************************************************/
void variable_listp(void) {
	switch (lookahead.code) {
	case COM_T: match(COM_T, NO_ATTR);
		variable_identifier(); variable_listp();
		break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <variable identifier> -> AVID | SVID
					  FIRST (<variable identifier>) = { AVID_T, SVID_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); syn_printe();
**********************************************************************************************************************************************/
void variable_identifier(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: match(lookahead.code, NO_ATTR);
		break;
	default: syn_printe(); break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <output statement> -> WRITE (<output_list>);
					  FIRST(<output statement>) = { KW_T(WRITE) }
Author				: Xingyi
Versions			: 1.0
Called Functions	: match(); output_list(); gen_incode();
**********************************************************************************************************************************************/
void output_statement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*********************************************************************************************************************************************
Purpose				: <output_list> -> <opt_variable list> | STR_T;
					  FIRST (<output_list>) = {AVID_T, SVID_T, STR_T, e }
Author				: Kevin
Versions			: 1.0
Called Functions	: variable_list(); match(); gen_incode();
**********************************************************************************************************************************************/
void output_list(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: variable_list();
		break;
	case STR_T: match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <arithmetic expression> - > <unary arithmetic expression>|<additive arithmetic expression>
					  FIRST (<arithmetic expression>) = { -, +, AVID_T, FPL_T, INL_T, ( }
Author				: Xingyi
Versions			: 1.0
Called Functions	: unary_arithmetic_expression(); gen_incode(); syn_printe(); additive_arithmetic_expression();
**********************************************************************************************************************************************/
void arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;
		default: syn_printe(); break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: additive_arithmetic_expression();
		break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
					  FIRST (<unary arithmetic expression>) = { -, +}
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); primary_arithmetic_expression(); gen_incode(); syn_printe();
**********************************************************************************************************************************************/
void unary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			break;
		default: syn_printe(); break;
		}
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression’>
					  FIRST (<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author				: Xingyi
Versions			: 1.0
Called Functions	: multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
**********************************************************************************************************************************************/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*********************************************************************************************************************************************
Purpose				: <additive arithmetic expression’> ->
						+ <multiplicative arithmetic expression><additive arithmetic expression’>
					  | -  <multiplicative arithmetic expression><additive arithmetic expression’>
					  | e
					  FIRST (<additive arithmetic expression’>) = { +, -, e}
Author				: Xingyi
Versions			: 1.0
Called Functions	: match(); additive_arithmetic_expression(); gen_incode();
**********************************************************************************************************************************************/
void additive_arithmetic_expression_p(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			break;
		}
	}
}

/*********************************************************************************************************************************************
Purpose				: <multiplicative arithmetic expression> -> <primary arithmetic expression><multiplicative arithmetic expression’>
					  FIRST (<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author				: Kevin
Versions			: 1.0
Called Functions	: primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
**********************************************************************************************************************************************/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*********************************************************************************************************************************************
Purpose				: <multiplicative arithmetic expression’> ->
						* <primary arithmetic expression><multiplicative arithmetic expression’>
					  | / <primary arithmetic expression><multiplicative arithmetic expression’>
					  | e
					  FIRST (<multiplicative arithmetic expression’>) = { *, / , e}
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p(); gen_incode()
**********************************************************************************************************************************************/
void multiplicative_arithmetic_expression_p(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case MULT:
		case DIV:
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default:
			break;
		}
	}
}

/*********************************************************************************************************************************************
Purpose				: <primary arithmetic expression> ->
						AVID_T
					  | FPL_T
					  | INL_T
					  | (<arithmetic expression>)
					  FIRST (<primary arithmetic expression > )= { AVID_T, FPL_T, INL_T, ( }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); gen_incode(); arithmetic_expression(); syn_printe();
**********************************************************************************************************************************************/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <string expression> -> <primary string expression> <string expression'>
					  FIRST ( <string expression> ) = { SVID_T, STR_T }
Author				: Xingyi
Versions			: 1.0
Called Functions	: primary_string_expression(); string_expression_p(); gen_incode();
**********************************************************************************************************************************************/
void string_expression(void) {
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <string expression'> -> #  <primary string expression> <string expression'> | e
					  FIRST ( <string expression'> ) = { #, e}
Author				: Xingyi
Versions			: 1.0
Called Functions	: primary_string_expression(); string_expression_p(); match();
**********************************************************************************************************************************************/
void string_expression_p(void) {
	switch (lookahead.code) {
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
		break;
	default:
		break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <primary string expression> ->
								   SVID_T
								 | STR_T
					  FIRST ( <primary string expression>) = { SVID_T, STR_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); syn_printe(); gen_incode();
**********************************************************************************************************************************************/
void primary_string_expression(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T: match(lookahead.code, NO_ATTR);
		break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <conditional expression> -> <logical OR  expression>
					  FIRST (<conditional expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: logical_OR_expression(); gen_incode();
**********************************************************************************************************************************************/
void conditional_expression(void) {
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <logical OR expression> -> <logical AND expression> <logical OR expression’>
					  FIRST (<logical OR expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author				: Xingyi
Versions			: 1.0
Called Functions	: logical_AND_expression(); logical_OR_expression_p();
**********************************************************************************************************************************************/
void logical_OR_expression(void) {
	logical_AND_expression();
	logical_OR_expression_p();
}

/*********************************************************************************************************************************************
Purpose				: <logical OR expression’> -> .OR.  <logical AND expression><logical OR expression’> | e
					  FIRST (<logical OR expression’>) = { .OR. , e }
Author				: Kevin
Versions			: 1.0
Called Functions	: match(); logical_AND_expression(); logical_OR_expression_p(); gen_incode();
**********************************************************************************************************************************************/
void logical_OR_expression_p(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_p();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
		break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <logical AND expression> -> <relational expression> <logical AND expression’>
					  FIRST (<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author				: Xingyi
Versions			: 1.0
Called Functions	: relational_expression(); logical_AND_expression_p();
**********************************************************************************************************************************************/
void logical_AND_expression(void) {
	relational_expression();
	logical_AND_expression_p();
}


/*********************************************************************************************************************************************
Purpose				: <logical AND expression’> -> .AND.  <relational expression><logical AND expression’> | e
					  FIRST (<logical AND expression’>) = {.AND. , e }
Author				: Xingyi
Versions			: 1.0
Called Functions	: match(LOG_OP_T, AND); relational_expression(); logical_AND_expression_p(); gen_incode();
**********************************************************************************************************************************************/
void logical_AND_expression_p(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_p();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
		break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <relational expression> ->
							  <primary a_relational expression> <primary  a_relational expression’>
							| <primary s_relational  expression> <primary s_relational expression’>
					  FIRST(<relational expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: primary_a_relational_expression(); primary_a_relational_expression_p(); gen_incode();
					  primary_s_relational_expression(); primary_s_relational_expression_p(); syn_printe();
**********************************************************************************************************************************************/
void relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expression_p();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_p();
		break;
	default: syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <primary a_relational expression’> ->
							 == <primary a_relational expression>
						   | <> <primary a_relational expression>
						   | > <primary a_relational expression>
						   | < <primary a_relational expression>
					  FIRST(<primary a_relational expression’>) = { ==, <>, >, < }
Author				: Xingyi
Versions			: 1.0
Called Functions	: primary_a_relational_expression(); syn_printe(); match();
**********************************************************************************************************************************************/
void primary_a_relational_expression_p(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_a_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe(); break;
	}
}

/*********************************************************************************************************************************************
Purpose				: <primary a_relational expression> ->
									AVID_T
								  | FPL_T
								  | INL_T
					  FIRST(<primary a_relational expression>)= {  AVID_T, FPL_T, INL_T}
Author				: Xingyi
Versions			: 1.0
Called Functions	: gen_incode(); syn_printe(); match();
**********************************************************************************************************************************************/
void primary_a_relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T: match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <primary s_relational expression> -> <primary string expression>
					  FIRST(<primary s_relational expression >)= { SVID_T, STR_T }
Author				: Kevin
Versions			: 1.0
Called Functions	: primary_string_expression(); gen_incode(); syn_printe();
**********************************************************************************************************************************************/
void primary_s_relational_expression(void) {
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*********************************************************************************************************************************************
Purpose				: <primary s_relational expression’> ->
									  == <primary s_relational expression>
									| <> <primary s_relational expression>
									| > <primary s_relational expression>
									| < <primary s_relational expression>
					  FIRST(<primary s_relational expression’>) = { ==, <>, >, <  }
Author				: Kevin
Versions			: 1.0
Called Functions	: primary_s_relational_expression(); match(); syn_printe();
**********************************************************************************************************************************************/
void primary_s_relational_expression_p(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_s_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe(); break;
	}
}
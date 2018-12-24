/*************************************************************************************************************************************
File name			: scanner.c
Compiler			: MS Visual Studio 2015
Author				: Kevin Lai, 040812704  Xingyi Wu, 04887028
Course				: CST 8152 - Compilers, Lab Section: 12, 14
Assignment			: 2
Date				: November 08 2018
Professor			: Sv. Ranev
Purpose				: Functions implementing a Lexical Analyzer (Scanner)
Function list		: scanner_init(), malar_next_token(), get_next_state(), char_class(), aa_func02(), aa_func03
					 aa_func05(), aa_func08, aa_func10, aa_func11(), aa_func12(), iskeyword()
*************************************************************************************************************************************/

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
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(Buffer * psc_buf)
{
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}


/*************************************************************************************************************************************
Purpose				: Performs token recognition by reading lexeme from the input stream (sc_buf)
Author				: Kevin Lai, Xingyi Wu
Versions			: 1.0
Called Functions	: b_getc();isspace();b_retract(); b_getcoffset();b_mark();b_reset(); b_addc();
Parameters			: void
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
*					- reads one character at a time from the input buffer (sc_buf)
*					- if the character read is equal to SEOF, breaks out of the while loop
*					- else loops through, processing tokens one by one (for special cases or exceptions)
*					- or using transition table (for AVID, SVID, DIL, FIL, SL, ES)
*					- returns a token structure if a token pattern (as defined in the lexical grammar)
*					  matches with a lexeme found in the stream of input symbols.
*					- otherwise returns a Error Token if a error occurs
*************************************************************************************************************************************/
Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	int i; /*used as a loop iterator*/
	char tempC; /* variable char to hold first character following !*/

	while (1)
	{ /* endless loop broken by token returns it will generate a warning */

		c = b_getc(sc_buf);

		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */

		/* isspace() detects ' ', '\t', '\v', '\n', '\f', '\r' */
		if (isspace(c))
		{
			if (c == '\n')line++;
			continue;
		}

		switch (c)
		{
			case SEOF: case '\0': t.code = SEOF_T;
				if (c == '\0') t.attribute.seof = SEOF1;
				else t.attribute.seof = SEOF2;
				return t;
				/* special case tokens: '(' , ')' , '{' , '}' , '>' , '<>' , '<', ';', ',' '#'*/
			case '(': t.code = LPR_T; return t;
			case ')': t.code = RPR_T; return t;
			case '{': t.code = LBR_T; return t;
			case '}': t.code = RBR_T; return t;
			case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t;
			case '<':
				t.code = REL_OP_T;
				if (b_getc(sc_buf) == '>')
				{
					t.attribute.rel_op = NE;
					return t;
				}
				b_retract(sc_buf);
				t.attribute.rel_op = LT;
				return t;
			case ';': t.code = EOS_T; return t;
			case ',': t.code = COM_T; return t;
			case '#': t.code = SCC_OP_T; return t;

				/*Arithmetic Tokens*/
			case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;
			case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
			case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;
			case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;

				/* '=', '==' */
			case '=':
				if (b_getc(sc_buf) == '=')
				{
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
					return t;
				}
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;

				/* ' !! ' means we have a comment operator*/
			case '!':

				/* set the first char after ! to tempC */
				tempC = c = b_getc(sc_buf);

				/* if not comment, set error token,
				 * store ! and the first char into err_lex
				 * add \0 at the end
				 */
				if (tempC == SEOF || tempC == '\0')
				{
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					t.attribute.err_lex[1] = '\0';
					b_retract(sc_buf);
					return t;
				}
				else if (tempC != '!')
				{
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					/* skip printing a blank line */
					if(c!='\n')
					t.attribute.err_lex[1] = c;

					t.attribute.err_lex[2] = '\0';
				}
				/* continue to loop till the end of line to ignore non-comment symbols  */
				while (c != '\n')
				{
					c = b_getc(sc_buf);
					if (c == SEOF||c=='\0')
					{
						t.code = SEOF_T;
						return t;
					}
				}
				/* increment lines */
				line++;
				/* not comment then return error token */
				if (tempC != '!')
					return t;
				/* continue to check next line */
				continue;

				/* logical operator: .AND., .OR. */
			case '.':

				/* mark the first char after . */
				b_mark(sc_buf, b_getcoffset(sc_buf));
				c = b_getc(sc_buf);

				/* check first and following char is 'AND.' or 'OR.' */
				if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.')
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;
				}
				else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;
				}
				/* if not, reset to go back sc_buf mark_offset
				 * set error token and only store . to err_lex
				 * add \0 at the end
				 */
				else
				{
					b_reset(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = '.';
					t.attribute.err_lex[1] = '\0';
					return t;
				}

			default:
				break;
		}

		/* Part 2: Implementation of Finite State Machine (DFA)
				   or Transition Table driven Scanner
				   Note: Part 2 must follow Part 1 to catch the illegal symbols
		*/
		/* check c is a alphabet and number character or string literal opening quotation(")*/
		if (isalnum((int) c) != 0 || c == '"' || c == '\0')
		{
			/* set the mark at the beginning of the lexeme and save it in the lexstart */
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
			/* FSM1: get the next state from the transition table */
			state = get_next_state(state, c, &accept);

			/* loop while transitioning states
			 * if not accepting (accept == NOAS), back to FSM1
			 * if yes, leave the machine,
			 * and call an accepting function as described below.
			 */
			while (accept == NOAS)
			{
				c = b_getc(sc_buf);
				state = get_next_state(state, c, &accept);
			}

			/*to check if need to retract*/
			if (accept == ASWR)
			{
				b_retract(sc_buf);
			}
			/* set lexend to sc_buf getc_offset */
			lexend = b_getcoffset(sc_buf);

			/* create a temporrary lexeme buffer lex_buf by b_allocated() in fix mode
			 * and check run time error if b_allocated() is failed
			 * return error token
			 */
			if (!(lex_buf = b_allocate(lexend - lexstart, 0, 'f')))
			{
				t.code = ERR_T;
				scerrnum = 1;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				return t;
			}
			/* set scanner buffer getcoffset back to mark */
			b_reset(sc_buf);
			/* copy lexeme into temp lexeme lex_buf */
			for (i = lexstart; i < lexend; i++)
			{
				c = b_getc(sc_buf);
				b_addc(lex_buf, c);
			}
			b_compact(lex_buf, '\0');

			/* set the token by call accepting function callback table
			 * the index of array table is stored in the variable state
			 * the argument of the function is the string stored in lex_buf
			 */
			t = aa_table[state](b_location(lex_buf, 0));
			/* free allocated dynamic memory lex_buf*/
			b_free(lex_buf);

			return t;
		}
		/*if we reach here then we need to set error token*/
		t.code = ERR_T;
		t.attribute.err_lex[0] = c;
		t.attribute.err_lex[1] = '\0';
		return t;
	} /*end while loop*/
	return t;
}

/*************************************************************************************************************************************
Purpose				: The function is to get the next state
Author				: Svillen Ranev
Versions			: 1.0
Called Functions	: none
Parameters			: state: int -the current state
					  c: char -input character from input buffer (sc_buf)
					  accept: int* -pointer to a int holding value corresponding to NOAS, ASNR, ASWR, ES or IS
Return value		: next: int	-the next state
Algorithm			: none
*************************************************************************************************************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS)
	{
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*************************************************************************************************************************************
Purpose				: The function returns the column number in the transition table st_table for the input character c.
Author				: Xingyi Wu
Versions			: 1.0
Called Functions	: isalpha(); isdigit()
Parameters			: void
Return value		: int val (represent the number of column in st_table)
Algorithm			: - each column represent different type of characters,
					  - start from the first column ( index = 0 ) in the st_table
					  - and return the corresponding column number
					  - return int val
*************************************************************************************************************************************/
int char_class(char c)
{
	int val; /* int to be returned */

	/* column 1: a-z A-Z */
	if (isalpha(c)) val = 0;

	/* column 2: 0 */
	else if (c == '0') val = 1;

	/* column 3: 1-9 */
	else if (isdigit(c) && c != '0') val = 2;

	/* column 4: . */
	else if (c == '.') val = 3;

	/* column 5: $ */
	else if (c == '$') val = 4;

	/*column 7: " */
	else if (c == '"') val = 6;

	/* column 8: SEOF or \0*/
	else if (c == SEOF || c == '\0') val = 7;

	/* column 6: other */
	else val = 5;

	return val;
}

/*************************************************************************************************************************************
Purpose				: Accepting function for the arithmetic variable identifier and keywords (VID - AVID/KW)
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: iskeyword(); strlen();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
					- create a local Token and int keyWordNum which is initalized to iskeyword();
					- so that when passing lexeme to the iskeyword(), it returns keyWordNum to check if the kexeme is a keyword,
					- if yes, return a token with the corresponding attribute for the keyword,
					- the attribute code for the key word is its index in the keyword lookup table (kw_table in table.h),
					- if not, set Token to AVID, if the lexeme is longer than VID_LEN (see token.h) characters,
					- only first VID_LEN characters are stored into the variable attribute array vid_lex[](see token.h),
					- add \0 at the end to make a C-type string.
					- return token.
*************************************************************************************************************************************/
Token aa_func02(char lexeme[])
{
	Token t = { 0 }; /*Token to be returned*/
	int i; /* variable for a loop iterator */
	int keyWordNum = iskeyword(lexeme); /* variable used for index of ky_table */

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/* lexeme is keywords */
	if (keyWordNum != RT_FAIL_1)
	{
		/* set keword token */
		t.code = KW_T;
		/* set token attribute to the index */
		t.attribute.kwt_idx = keyWordNum;
		return t;
	}
	/* lexeme is not keyword*/
	else
	{
		/* set AVID token */
		t.code = AVID_T;
	}
	/* lexeme is longer than VID_LEN */
	if (strlen(lexeme) > VID_LEN)
	{
		/* loop the lexeme till the VID_LEN*/
		for (i = 0; i < VID_LEN; i++)
		{
			/* store first VID_LEN into vid_lex[] */
			t.attribute.vid_lex[i] = lexeme[i];
		}
		/* add \0 to the end */
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	/* lexeme is shorter than or equal to VID_LEN */
	else
	{
		/* store the whole lexeme into vid_lex */
		strcpy(t.attribute.vid_lex, lexeme);
	}
	return t;
}

/*************************************************************************************************************************************
Purpose				: Accepting function for the string variable identifier and keywords (VID - SVID)
Author				: Xingyi Wu
Versions			: 1.0
Called Functions	: strlen(); strncpy();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
					- create a local and set a SVID token,
					- if the lexeme is longer than VID_LEN characters,
					- only first VID_LEN -1 characters are stored into the variable attribute array vid_lex[]
					- then the $ character is appened to the name,
					- and add \0 at the end to make a C-type string.
					- return token.
*************************************************************************************************************************************/
Token aa_func03(char lexeme[])
{
	Token t = { 0 }; /* Token to be returned */
	int i; /* variable for a loop iterator */
	t.code = SVID_T; /* set SVID_T token */

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/* lexeme is longer than VID_LEN */
	if (strlen(lexeme) > VID_LEN)
	{
		/* loop the lexeme till the VID_LEN-1 */
		for (i = 0; i < VID_LEN - 1; i++)
		{
			/* store first VID_LEN-1 into vid_lex */
			t.attribute.vid_lex[i] = lexeme[i];
		}
		/* add $ to VID_LEN - 1*/
		t.attribute.vid_lex[VID_LEN - 1] = '$';
		/* add \0 to the end*/
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	/* lexeme is shorter than or equal to VID_LEN */
	else
	{
		/* store lexeme into vid_lex */
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}

	return t;
}

/*************************************************************************************************************************************
Purpose				: Accepting function for the integer literal(IL) - decimal constant (DIL)
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: atol(); aa_table[ES]();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
					- create a local token, a local long variable (num) to hold integer lexeme after converting by using atol();
					- if long num is out of range, the function return error token, which its attribute is lexeme
					- call error token function aa_table[11](), which is in Accepting function (action) callback table (see table.h);
					- set INL_T token
					- return token.
*************************************************************************************************************************************/
Token aa_func05(char lexeme[])
{
	Token t = { 0 }; /* Token to be returned */
	long num = atol(lexeme); /* variable to hold integer lexeme after converting */

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/* (int)lexeme is out of range */
	if (num > SHRT_MAX || num < SHRT_MIN)
	{
		/* set error token */
		t = aa_table[ES](lexeme);
	}
	/* in the range */
	else
	{
		/* set integer token */
		t.code = INL_T;
		/* set attribute to the converted value */
		t.attribute.int_value = (short)num;
	}
	return t;
}

/*************************************************************************************************************************************
Purpose				: Accepting function for the floating - point literal (FPL)
Author				: Xingyi Wu
Versions			: 1.0
Called Functions	: atof(); aa_table[ES]();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
					- create a local token, a local double variable (temp) to hold floating lexeme after converting by using atof();
					- if double temp is out of range, the function return error token, which its attribute is lexeme
					- call error token function aa_table[11](), which is in Accepting function (action) callback table (see table.h);
					- set FPL_T token
					- return token.
*************************************************************************************************************************************/
Token aa_func08(char lexeme[])
{
	Token t = { 0 }; /* Token to be returned */
	double temp = atof(lexeme); /* variable to hold floating lexeme after converting */

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/* (float)lexeme is out of range */
	if (temp > FLT_MAX || (temp < FLT_MIN && temp > 0))
	{
		/* set error token */
		t = aa_table[ES](lexeme);
	}
	else
	{
		/* set float token */
		t.code = FPL_T;
		/* set attribute to the converted value */
		t.attribute.flt_value = (float) temp;
	}

	return t;

}

/*************************************************************************************************************************************
Purpose				: Accepting function for the string literal(SL)
Author				: Kevin Lai, Xingyi Wu
Versions			: 1.0
Called Functions	: b_limit(); b_addc(); strlen();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
					- set the attribute of the string token, which is the offset from the beginning of the str_LTBL
					  to the location where the first char of the lexeme content will be added to, which is b_limit(str_LTBL)
					- during the copying process, ignore the opening and closing "
					- if the string lexeme contains line terminators, the line counter is incremented
					- add \0 at the end of the string table str_LTBL
					- set STR_T token
					- return token.
*************************************************************************************************************************************/
Token aa_func10(char lexeme[])
{

	Token t = { 0 }; /* Token to be returned */
	int i; /* variable for a loop iterator */

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/* set string offset to location where the first char of the lexeme content will be added to */
	t.attribute.str_offset = b_limit(str_LTBL);

	for (i = 0; i < (int) strlen(lexeme); i++)
	{
		/* copy lexeme into string table but ignore the opening and closing quotations " */
		if (lexeme[i] != '"')
			b_addc(str_LTBL, lexeme[i]);

		/* increment line if lexeme contains line terminators */
		if (lexeme[i] == '\n')
			line++;
	}
	/* add \0 to the end of string table*/
	b_addc(str_LTBL, '\0');
	/* set string code */
	t.code = STR_T;

	return t;
}

/*************************************************************************************************************************************
Purpose				: Accepting function for the error token
Author				: Xingyi Wu
Versions			: 1.0
Called Functions	: strlen(); strncpy();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
Algorithm			: Performs token recognition by
					- set ERR_T token
					- if ERROR lexeme is longer than ERR_LEN characters,
					- only first VID_LEN -3 characters are stored into err_lex[],
					- then three dots ... are added to the end of the err_lex C-type string
					- if the error lexeme contains line terminators, the line counter is incremented.
					- return token.
*************************************************************************************************************************************/
Token aa_func11(char lexeme[])
{
	Token t = { 0 }; /* Token to be returned */
	int i; /* variable for a loop iterator */
	int length = strlen(lexeme); /* variable to set lexeme string length */
	t.code = ERR_T; /* set error code */

#ifdef DEBUG
	printf("lexeme: |%s|\u", lexeme);
#endif

	/* check is lexeme is longer than ERR_LEN 20 characters */
	if (length > ERR_LEN)
	{
		/* copy 17 lexeme chars to err_lex */
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		/* last 3 chars are stored as . */
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		/* add \0 at the end */
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else
	{
		/* copy the lexeme to token attribute of err_lex */
		strncpy(t.attribute.err_lex, lexeme, length);
		/* add \0 at the end */
		t.attribute.err_lex[length] = '\0';
	}
	/* increment line if lexeme contains line terminators */
	for (i = 0; i < length; i++)
	{
		if (lexeme[i] == '\n')
		{
			line++;
		}
	}
	return t;
}


/*************************************************************************************************************************************
Purpose				: Accepting function with retract for the error token, sets token to aa_table[ES] to get error token.
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: aa_table[ES]();
Parameters			: char lexeme[]
Return value		: Token (representing the corresponding token that was recognized)
*************************************************************************************************************************************/
Token aa_func12(char lexeme[])
{
	Token t = { 0 }; /* Token to be returned */
	t = aa_table[ES](lexeme);
	return t;
}

/*************************************************************************************************************************************
Purpose				: keywords lookup function
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: strcmp()
Parameters			: char * kw_lexeme
Return value		: int i
Algorithm			: Performs token recognition by
					- check if characters passing the function is in the keyword lookup table (kw_table in table.h),
					- if yes, return index of that keyword in the table,
					- if not, return -1
*************************************************************************************************************************************/
int iskeyword(char * kw_lexeme)
{
	int i; /* variable for a loop iterator */

	/* compare string kw_lexeme with kw_table[] and return the index */
	for (i = 0; i < KWT_SIZE; i++)
	{
		if (strcmp(kw_table[i], kw_lexeme) == 0) return i;
	}
	return RT_FAIL_1;
}
/*************************************************************************************************************************************
File name			: buffer.h
Compiler			: MS Visual Studio 2015
Author				: Kevin Lai, 040812704
Course				: CST 8152 - Compilers, Lab Section: 12
Assignment			: 1
Date				: October 03 2018
Professor			: Sv. Ranev
Purpose				: Programming and Using Dynamic Structures (buffers) with C
Function list		: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark()
					  b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(), b_print(), b_compact()
					  b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()
*************************************************************************************************************************************/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1         /* fail return value */
#define RT_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail return value */

/* You should add your own constant definitions here */
#define B_FULL /*used for b_isfull macro function (if B_FULL is defined than the macro will be used)*/
#define FIXED 0 /*fixed mode*/
#define ADDITIVE 1 /*additive mode*/
#define MULTIPLICATIVE -1 /*multiplicative mode*/
#define FIXED_MODE 'f' /*fixed mode (char value)*/
#define ADDITIVE_MODE 'a' /*additive mode (char value)*/
#define MULTIPLICATIVE_MODE 'm' /*multiplicative mode(char value)*/
#define MAXIMUM_ALLOWED_POSITIVE_VALUE (SHRT_MAX-1) /*use to indicate the maximum allowed value for our buffer capacity*/
#define ZERO 0 /*value of 0*/
#define ONE 1 /*value of 1*/
#define HEXA256 0x100 /*hexa decimal value for 256*/
#define HUNDRED 100 /*value of 100*/

/* Enter your bit-masks constant definitions here */
#define DEFAULT_FALGS  0xFFFC  /*default flags value*/
#define SET_EOB        0x0001  /*set eob mask*/
#define RESET_EOB      0xFFFE  /*reset eob mask*/
#define CHECK_EOB      0x0001  /*check eob mask*/
#define SET_R_FLAG     0x0002  /*set r_flag mask*/
#define RESET_R_FLAG   0xFFFD  /*reset r_flag mask*/
#define CHECK_R_FLAG   0x0002  /*check r_flag mask*/

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/

Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
#ifndef B_FULL
int b_isfull(Buffer * const pBD);
#endif
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_eob(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);

#ifdef B_FULL
#define b_isfull(pBD) ((pBD) ? (pBD)->addc_offset == (pBD)->capacity ? ONE : ZERO : RT_FAIL_1)  /*use this macro function if B_FULL is defined*/
#else
#undef B_FULL
#endif

#endif


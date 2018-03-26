/********************************************************************************************************************
File name:			buffer.h
Compiler:			MS Visual Studio 2015
Author:				Sv.Ranev modified by Zaid Versey, 040836453
Course:				CST 8152 - Compilers, Lab Section: 13
Assignment:			1
Date:				September 29, 2017
Professor:			Sv.Ranev
Purpose:			Preprocessor directives, type declarations and prototypes necessary for buffer implementation

Function list:		b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark()
b_mode(), b_incfactor(), b_load(), b_isempty(), b_eob(), b_getc(), b_print(), b_compact()
b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()

Consts & macros:    RT_FAIL1, RT_FAIL2, LOAD FAIL, SUCCESS, SET_R_FLAG SET_EOB, MODE_FIXED, MODE_ADD, MODE_MULT,
ADD_MAX_INC, MULT_MAX_INC, MAX_VALUE, IS_FULL, NOT_FULL, IS_EMPTY, NOT_EMPTY, TRUE, MYDEBUG
********************************************************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001)*/ /*to enforce C89 type comments  - to make //comments an warning */

							/*#pragma warning(error:4001) *//* to enforce C89 comments - to make // comments an error */

															/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

															/* constant definitions */
															/* You may add your own constant definitions here */
#define RT_FAIL1 -1 				/* fail return value */
#define RT_FAIL2 -2         		/* fail return value */
#define LOAD_FAIL -2       			/* load fail error */
#define SUCCESS 0					/* value when operation is successful */
#define SET_R_FLAG 1       			/* realloc flag set value */
#define SET_EOB 1					/* value for eob when buffer is full */
#define MODE_FIXED 0 				/* fixed buffer value */
#define MODE_ADD 1 					/* additive buffer value */
#define MODE_MULT -1				/* multiplicative buffer value */
#define ADD_MAX_INC 255				/* max increment value in additive mode */
#define MULT_MAX_INC 100			/* max increment value in multiplicative mode */
#define MAX_CAPACITY SHRT_MAX - 1 	/* maximum capacity allowed for buffer */
#define IS_FULL 1					/* value when buffer IS full */
#define NOT_FULL 0					/* value when buffer is NOT full */
#define IS_EMPTY 1					/* value when buffer IS empty */
#define NOT_EMPTY 0					/* value when buffer is NOT empty */
#define TRUE 1						/* for while loop */
#undef 	MYDEBUG						/* used for debugging */


															/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(Buffer * const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);

#endif


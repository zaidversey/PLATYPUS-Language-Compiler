/********************************************************************************************************************
File name:			table.h
Compiler:			MS Visual Studio 2015
Author:				Sv.Ranev modified by Zaid Verey (040936453) & Vitor Cortes de Souza (040853069)
Course:				CST 8152 - Compilers, Lab Section: 13
Assignment:			2 - Scanner
Date:				November 30, 2017
Professor:			Sv.Ranev
Purpose:			Preprocessor directives, define kewords, transition symbol table and prototypes for
					Scanner implementation

Function list:		aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func11(), aa_func12()

Consts & macros:    TABLE_H, BUFFER_H, NULL, ES, ER, NOT_KEYWORD, IS_KEYWORD, TABLE_COLUMNS KWT_SIZE
					ASWR, ASNR, NOAS
********************************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/

/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*       space
*  !!comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', '#' ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/
#define PLATY_MAX 32767 /*max int value - 2 bytes*/
#define SEOF1 255 /*max int value - 2 bytes*/
#define SEOF2 '\0' /*max int value - 2 bytes*/
#define ES 12 /*Error state no retract*/
#define ER 13 /*Error state with retract*/
#define IS -1 /*Inavalid state */
#define NOT_KEYWORD -1
#define IS_KEYWORD 1

/*	State transition table definition
	transition table - type of states defined in separate table */
#define TABLE_COLUMNS 9

int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1, 1, 1, 1, 6, 4, IS, IS, IS },
	/* State 1 */{ 1, 1, 1, 1, 1, 1, 2, 3, 2 },
	/* State 2 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS },
	/* State 3 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS },
	/* State 4 */{ ES, ES, ES, ES, 4, 4, 7, 5, 5 },
	/* State 5 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS },
	/* State 6 */{ 9, ES, ES, ES, 5, ES, 7, ER, 5 },	
	/* State 7 */{ 8, 8, 8, 8, 7,  7, 8, 8, 8 },
	/* State 8 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS },
	/* State 9 */{ ER, ER, 10, ER, 10, 10, ER, ER, ER },	
	/* State 10 */{ ES, ES, 10, ES, 10, 10, 11, 11, 11 },
	/* State 11 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS },
	/* State 12 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS },
	/* State 13 */{ IS, IS, IS ,IS, IS, IS, IS, IS, IS }

};

/* Accepting state table definition */
#define ASWR 1  /* accepting state with retract */
#define ASNR 2  /* accepting state with no retract */
#define NOAS 3  /* not accepting state */

int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, NOAS, ASWR, ASNR, ASWR };

/* Accepting action function declarations */
/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.*/

Token aa_func02(char *lexeme);/*AVID/KW*/
Token aa_func03(char *lexeme);/*SVID*/
Token aa_func05(char *lexeme);/*DIL*/
Token aa_func08(char *lexeme);/*FPL*/
Token aa_func11(char *lexeme);/*HIL*/
Token aa_func12(char *lexeme);/*ES/ER*/
/*Token aa_func13(char *lexeme);*/

/* defining a new type: pointer to function (of one char * argument)
returning Token
*/
typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
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
	NULL,
	aa_func11,
	aa_func12, /*no retract*/
	aa_func12  /*with retract*/

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
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

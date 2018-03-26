/********************************************************************************************************************
File name:			parser.h
Compiler:			MS Visual Studio 2015
Author:				Zaid Verey (040936453) & Vitor Cortes de Souza (040853069)
Course:				CST 8152 - Compilers, Lab Section: 13
Assignment:			3 - Parser
Date:
Professor:			Sv.Ranev
Purpose:			Preprocessor directives, type declarations and prototypes necessary for parser implementation

Function list:

Consts & macros:
********************************************************************************************************************/
#ifndef PARSER_H_
#define PARSER_H_

#define DEBUG  /* for conditional processing */
#undef  DEBUG

#include "buffer.h"
#include "token.h"

/*Constants*/
#define NO_ATTR -1
#define ELSE 0
#define P_FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define P_TRUE 7
#define WHILE 8
#define WRITE 9

static Token lookahead;
static Buffer* sc_buf;
int synerrno = 0;

/*external links*/
extern Token malar_next_token(Buffer*);
extern int line;
extern Buffer* str_LTBL;
extern char* kw_table[];


/*Parser Functions:*/
void parser(Buffer*);
void match(int, int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char*);

/*Production Functions*/
void program();
void opt_statements();
void statements();
void statements_p();
void statement();
void assignmentStatement();
void assignmentExpression();
void selectionStatement();
void iterationStatement();
void pre_condition();
void inputStatement();
void variableList();
void variableList_p();
void variableIdentifier();
void outputStatement();
void outputStatement_p();
void opt_variableList();
void arithmeticExpression();
void unaryArithmeticExpression();
void additiveArithmeticExpression();
void additiveArithmeticExpression_p();
void multiplicativeArithmeticExpression();
void multiplicativeArithmeticExpression_p();
void primaryArithmeticExpression();
void stringExpression();
void stringExpression_p();
void primaryStringExpression();
void conditionalExpression();
void logicalOrExpression();
void logicalOrExpression_p();
void logicalAndExpression();
void logicalAndExpression_p();
void relationalExpression();
void primaryA_relExpression();
void primaryA_relExpression_p();
void primaryS_relExpression();
void primaryS_relExpression_p();

#endif

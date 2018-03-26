/********************************************************************************************************************
File name:			parser.c
Compiler:			MS Visual Studio 2015
Author:				Zaid Verey (040936453) & Vitor Cortes de Souza (040853069)
Course:				CST 8152 - Compilers, Lab Section: 13
Assignment:			3 - Parser
Date:				5 January 2018
Professor:			Sv.Ranev
Purpose:			Functions implementing a Syntactic Analyzer (Parser)

Function list:	 program(), opt_statements(), statements(), statements_p(), statement(),
				 assignmentStatement(), assignmentExpression(), selectionStatement(), iterationStatement(),
				 pre_condition(), inputStatement(), variableList(), variableList_p(), variableIdentifier(),
				 outputStatement(), outputStatement_p(), opt_variableList(), arithmeticExpression(),
				 unaryArithmeticExpression(), additiveArithmeticExpression(),additiveArithmeticExpression_p(),
				 multiplicativeArithmeticExpression(), multiplicativeArithmeticExpression_p(),
				 primaryArithmeticExpression(), stringExpression(), stringExpression_p(), primaryStringExpression(),
				 conditionalExpression(), logicalOrExpression(), logicalOrExpression_p(), logicalAndExpression(),
				 logicalAndExpression_p(), relationalExpression(), primaryA_relExpression(), primaryA_relExpression_p(),
				 primaryS_relExpression(), primaryS_relExpression_p()
********************************************************************************************************************/

/* project header files */
#include "parser.h"

/********************************************************************************************************************
Purpose:			Parser
Author:				Sv. Ranev
History/Versions:	1.0
Called functions:	malar_next_token(), program(),  match(), gen_incode()
Parameters:			in_buf		Buffer pointer       (N/A)
Return value:		none
*********************************************************************************************************************/
void parser(Buffer* in_buf) {
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/********************************************************************************************************************
Purpose:			Matches two tokens: the current input token (lookahead) and the
					token required by the parser. The token required by the parser is represented by two
					integers - the token code (pr_token_code), and the token attribute

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	syn_eh(), malar_next_token()
Parameters:			pr_token_code, pr_token_attribute	int, int
Return value:		none
*********************************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {
#ifdef DEBUG
printf("Match Function: Token is: %d, and attribute is: %d\n", pr_token_code, pr_token_attribute );
#endif
	/*If current token code does not match required token code - handle error and return*/
	if (pr_token_code != lookahead.code) {
		syn_eh(pr_token_code);
		return;
	}
	/*Verify token code and attibute*/
	switch (pr_token_code) {
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			/*check if current token and required token attributes match*/
			if (pr_token_attribute != lookahead.attribute.get_int) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		default:
			break;
	}/*Match Succesful*/

	 /*Check if current token is SEOF*/
	if (lookahead.code == SEOF_T) {
		return;
	}

	lookahead = malar_next_token(sc_buf);

	/*check if the new token is ERR_T*/
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token(sc_buf);
		synerrno++;
	}

}/*end of match()*/

/********************************************************************************************************************
 Purpose:				Error handling, Implements panic mode.

 Author:				Zaid Versey & Vitor de Souza
 History/Versions:		1.0
 Called functions:		syn_printe();
 Parameters:			sync_token_code		int
 Return value:		none
 *********************************************************************************************************************/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;

	/*advance input token until matching token is found*/
	do {
		lookahead = malar_next_token(sc_buf);

		/*matching token found*/
		if (sync_token_code == lookahead.code) {
			if (lookahead.code != SEOF_T) {
				lookahead = malar_next_token(sc_buf);
			}
			return;
		}
		/*if source end of file reached*/
		if (lookahead.code == SEOF_T) {
			exit(synerrno);
			return;
		}
	} while (sync_token_code != lookahead.code);
}/*end of syn_eh()*/

 /********************************************************************************************************************
 Purpose:				Print errors found by the parser

 Author:				Svillen Ranev
 History/Versions:		1.0
 Called functions:		b_location()
 Parameters:			none
 Return value:			none
 *********************************************************************************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
		case  ERR_T: /* ERR_T     0   Error token */
			printf("%s\n", t.attribute.err_lex);
			break;
		case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
			printf("NA\n");
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
			break;
	}/*end switch*/
}/* end syn_printe()*/

 /********************************************************************************************************************
 Purpose:				Prints a message identifying the production being parsed

 Author:				Zaid Versey & Vitor de Souza
 History/Versions:		1.0
 Called functions:		none
 Parameters:			message		char pointer
 Return value:			none
 *********************************************************************************************************************/
void gen_incode(char* message) {
	printf("%s\n", message);
}/*end of gen_incode()*/

 /********************************************************************************************************************
 Purpose:				Representation of the PLATYPUS production. 
						A PLATYPUS program is a sequence of statements - no statements at all, one statement,
						or more than one statement, enclosed in braces { }.

 Production:			<program> -> PLATYPUS {<opt_statements>} SEOF
 FIRST SET:			 	FIRST(<program>) = {KW_T(PLATYPUS)}

 Author:				Svillen Ranev
 History/Versions:		1.0
 Called functions:		match(), opt_statements(), gen_incode()
 Parameters:			none
 Return value:			none
 *********************************************************************************************************************/
void program() {
#ifdef DEBUG
printf("1 - I am here: program\n");
#endif
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <opt_statements> production.

Production:			<opt_statements> -> <statements> | e
FIRST SET:			FIRST(<opt_statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e }

Author:				Svillen Ranev
History/Versions:	1.0
Called functions:	statements(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void opt_statements() {
#ifdef DEBUG
printf("2 - I am here: opt_statements\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break; /*first set here*/
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT) {
			statements();
			break;
		}

		/*in case of an empty string opt_statements*/
	default:
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <statements> production.
					The sequence of a PLATYPUS program is controlled by statements

Production:			<statements> -> <statement><statements'>
FIRST SET:			FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	statement(), statements_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void statements() {
#ifdef DEBUG
printf("3 - I am here: statements\n");
#endif
	statement(); statements_p();
}

/********************************************************************************************************************
Purpose:			Representation of the <statements'> production.

Production:			<statements'> -> <statement><statements'> | e
FIRST SET:			FIRST(<statements'>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	statement(), statement_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void statements_p() {
#ifdef DEBUG
printf("4 - I am here: statements_p\n");
#endif
	switch (lookahead.code){
		case KW_T:
			switch(lookahead.attribute.kwt_idx){
				case IF:
				case WHILE:
				case READ:
				case WRITE:
					break;
				
				default:
					return;
			}
			/*Dont break to continue to statements below*/
		case AVID_T:
		case SVID_T:
			statement();
			statements_p();
			break;
	}
}

/****************************************************************************************************************************
Purpose:			Representation of the <statement> production.

Production:			<statement> -> <assignment statement> | <selection statement> |<iteration statement> |<input statement>
						|<output statement>
FIRST SET:			FIRST(<statement>) = {AVID, SVID, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	assignmentStatement(), selectionStatement(), iterationStatement(), inputStatement(), outputStatement(), 
					syn_printe()
Parameters:			none
Return value:		none
****************************************************************************************************************************/
void statement() {
#ifdef DEBUG
printf("5 - I am here: statement\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T: assignmentStatement(); 
			break;
		case KW_T:
			switch (lookahead.attribute.kwt_idx) {
				case IF:
					selectionStatement();
					break;
				case WHILE:
					iterationStatement();
					break;
				case READ:
					inputStatement();
					break;
				case WRITE:
					outputStatement();
					break;
				default: /*print error if wrong keyword*/
					syn_printe();
					break;
			}
			break;
		default:
			syn_printe();
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <assignment statement> production.

Production:			<assignment statement> -> <assignment expression>;
FIRST SET:			FIRST(<assignment statement>) = { AVID_T, SVID_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	assignmentExpression(), match(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void assignmentStatement() {
#ifdef DEBUG
printf("6 - I am here: assignmentStatement\n");
#endif
	assignmentExpression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <assignment expression> production.

Production:			< assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
FIRST SET:			FIRST(<assignment expression>) = { AVID_T, SVID_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), arithmeticExpression(), gen_incode(), stringExpression(), syn_printe()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void assignmentExpression() {
#ifdef DEBUG
printf("7 - I am here: assignmentExpression\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
			match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR);
			arithmeticExpression();
			gen_incode("PLATY: Assignment expression (arithmetic) parsed"); 
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR);
			stringExpression();
			gen_incode("PLATY: Assignment expression (string) parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <selection statement> production.

Production:			<selection statement> -> IF TRUE ( <conditional expression> ) THEN { <opt_statements> }
						ELSE { <opt_statements> } ;
FIRST SET:			FIRST(<selection statement>) = { KW_T(IF) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), conditionalExpression(), opt_statements(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void selectionStatement() {
#ifdef DEBUG
printf("8 - I am here: selectionStatement\n");
#endif
	match(KW_T, IF); match(KW_T, P_TRUE); match(LPR_T, NO_ATTR);
	conditionalExpression(); match(RPR_T, NO_ATTR); match(KW_T, THEN);
	match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR);
	match(KW_T, ELSE); match(LBR_T, NO_ATTR); 
	opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the < iteration statement> production.

Production:			<iteration statement> -> WHILE <pre-condition> (<conditional expression>)
						REPEAT { statements };
FIRST SET:			FIRST(<iteration statement>) = { KW_T(WHILE) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), conditionalExpression(), pre_condition(), statements(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void iterationStatement() {
#ifdef DEBUG
printf("9 - I am here: iterationStatement\n");
#endif
	match(KW_T, WHILE); pre_condition(); match(LPR_T, NO_ATTR);
	conditionalExpression();  match(RPR_T, NO_ATTR); match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR); statements(); match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <pre condition> production.

Production:			<pre-condition> -> TRUE | FALSE
FIRST SET:			FIRST(<pre_condition>) = { KW_T(TRUE), KW_T(FALSE) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), syn_printe()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void pre_condition() {
#ifdef DEBUG
printf("10 - I am here: pre_condition\n");
#endif
	switch (lookahead.code) {
		case KW_T:
			switch (lookahead.attribute.kwt_idx) {
				case P_TRUE:
				case P_FALSE:
					match(lookahead.code, lookahead.attribute.kwt_idx);
					break;
				default:
					syn_printe();
					break;
			}
			break;
		default:
			syn_printe(); /*no empty, print error*/
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <input statement> production.

Production:			<input statement> -> READ ( <variable list> );
FIRST SET:			FIRST(<input statement>) = { KW_T(READ) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), variableList(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void inputStatement() {
#ifdef DEBUG
printf("11 - I am here: inputStatement\n");
#endif
	match(KW_T, READ); match(LPR_T, NO_ATTR); variableList();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <variable list> production.

Production:			<variable list> -> <variable identifier> <variable list'>
FIRST SET:			FIRST(<variable list>) = { AVID_T , SVID_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	variableIdentifier(), variableList_p(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void variableList() {
#ifdef DEBUG
printf("12 - I am here: variableList\n");
#endif
	variableIdentifier();
	variableList_p();
	gen_incode("PLATY: Variable list parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <variable list'> production.

Production:			<variable list'> -> ,<variable identifier> <variable list'> | e
FIRST SET:			FIRST(<variable list'>) = { COM_T, e}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), variableIdentifier(), variableList_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void variableList_p() {
#ifdef DEBUG
printf("13 - I am here: variableList_p\n");
#endif
	switch (lookahead.code) {
		case COM_T:
			match(COM_T, NO_ATTR);
			variableIdentifier();
			variableList_p();
			break;

		default: /*empty*/
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <variable identifier> production.

Production:			<variable indentifier> -> AVID_T | SVID_T
FIRST SET:			FIRST(<variable identifier>) = { AVID_T, SVID_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), syn_printe()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void variableIdentifier() {
#ifdef DEBUG
printf("14 - I am here: variableIdentifier\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			match(lookahead.code, NO_ATTR);
			break;
			
		default:/* print error if other */
			syn_printe();
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <output statement> production.

Production:			<output statement> -> WRITE (<output statement'>); 
FIRST SET:			FIRST(<output statement>) = { KW_T(WRITE) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), outputStatement(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void outputStatement() {
#ifdef DEBUG
printf("15 - I am here: outputStatement\n");
#endif
	match(KW_T, WRITE); match(LPR_T, NO_ATTR);
	outputStatement_p(); match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <output statement'> production.

Production:			<output statement'> -> <opt_variable list> | STR_T
FIRST SET:			FIRST(<output statement'>) = { AVID_T, SVID_T, e, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	opt_variableList(), match(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void outputStatement_p() {
#ifdef DEBUG
printf("16 - I am here: outputStatement_p\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:  opt_variableList(); break;
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;
		default:
			gen_incode("PLATY: Output list (empty) parsed");
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <opt_variable list> production.

Production:			<opt_variable list> -> <variable list> | e
FIRST SET:			FIRST(<opt_variable list>) = { AVID_T, SVID_T, e}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	variableList, gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void opt_variableList() {
#ifdef DEBUG
printf("17\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			variableList();
			break;
		default:
			/*gen_incode("PLATY: Output list (empty) parsed");*/
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <arithmetic expression> production.

Production:			<arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>
FIRST SET:			FIRST(<arithmetic expression>) = { ART_OP_T(-), ART_OP_T(+), AVID_T, FPL_T, INL_T, LPR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	unaryArithmeticExpression(), additiveArithmeticExpression(), syn_printe(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void arithmeticExpression() {
#ifdef DEBUG
printf("18\n");
#endif
	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case MINUS:
				case PLUS:
					unaryArithmeticExpression();
					gen_incode("PLATY: Unary arithmetic expression parsed");
					break;
				default: /*print error if wrong ART_OP_T*/
					syn_printe();
					return;
			}
			break;
		case AVID_T:
		case FPL_T:
		case INL_T:
		case LPR_T:
			additiveArithmeticExpression();
			break;
		default:/*print error if other*/
			syn_printe();
			/*return;*/
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <unary arithmetic expression> production.

Production:			<unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
FIRST SET:			FIRST(<unary arithmetic expression>) = { ART_OP_T(-), ART_OP_T(+) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), primaryArithmeticExpression(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void unaryArithmeticExpression() {
#ifdef DEBUG
printf("19\n");
#endif
	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case MINUS:
				case PLUS:
					match(lookahead.code, lookahead.attribute.arr_op);
					primaryArithmeticExpression();
					/*gen_incode("PLATY: Unary arithmetic expression parsed");*/
					break;
				default: /*print error if wrong ART_OP_T*/
					syn_printe();
					break;
			}
			break;

		default:/*print error if other*/
			syn_printe();
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <additive arithmetic expression> production.

Production:			<additive arithmetic expression> -> <multiplicative arithmetic expression> <additive arithmetic expression'>
FIRST SET:			 FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	multiplicativeArithmeticExpression(), additiveArithmeticExpression_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void additiveArithmeticExpression() {
#ifdef DEBUG
printf("20\n");
#endif
	multiplicativeArithmeticExpression();
	additiveArithmeticExpression_p();
}

/********************************************************************************************************************
Purpose:			Representation of the <additive arithmetic expression'> production.

Production:			<additive arithmetic expression'> ->
						+ <multiplicative arithmetic expression> <additive arithmetic expression'>
						| - <multiplicative arithmetic expression> <additive arithmetic expression'>
						|e
FIRST SET:			FIRST(<additive arithmetic expression�>) = { ART_OP_T(+),ART_OP_T(-), e }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), multiplicativeArithmeticExpression(), additiveArithmeticExpression_p(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void additiveArithmeticExpression_p() {
#ifdef DEBUG
printf("21\n");
#endif
	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case PLUS:
				case MINUS:
					match(lookahead.code, lookahead.attribute.arr_op);
					multiplicativeArithmeticExpression();
					additiveArithmeticExpression_p();
					break;

				default:
					return;
			}
		break;

		default:/*empty*/
			/*gen_incode("PLATY: Additive arithmetic expression parsed");*/
			return;
	}
	gen_incode("PLATY: Additive arithmetic expression parsed");
}

/**************************************************************************************************************************************
Purpose:			Representation of the <multiplicative arithmetic expression> production.

Production:			<multiplicative arithmetic expression> -> <primary arithmetic expression> <multiplicative arithmetic expression'>
FIRST SET:			FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	primaryArithmeticExpression(), multiplicativeArithmeticExpression_p()
Parameters:			none
Return value:		none
***************************************************************************************************************************************/
void multiplicativeArithmeticExpression() {
#ifdef DEBUG
printf("22\n");
#endif
	primaryArithmeticExpression();
	multiplicativeArithmeticExpression_p();
}

/********************************************************************************************************************
Purpose:			Representation of the <multiplicative arithmetic expression'> production.

Production:			<multiplicative arithmetic expression'> ->
						* <primary arithmetic expression> <multiplicative arithmetic expression'>
						| / <primary arithmetic expression> <multiplicative arithmetic expression'>
						| e
FIRST SET:			FIRST(<multiplicative arithmetic expression'>) = { ART_OP_T(*), ART_OP_T(/), e }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), primaryArithmeticExpression(), multiplicativeArithmeticExpression_p(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void multiplicativeArithmeticExpression_p() {
#ifdef DEBUG
printf("23\n");
#endif
	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case MULT:
				case DIV:
					match(lookahead.code, lookahead.attribute.arr_op);
					primaryArithmeticExpression();
					multiplicativeArithmeticExpression_p();
					break;
				default:
					return;
			}
			break;

		default:/*empty*/
			/*gen_incode("PLATY: Multiplicative arithmetic expression parsed");*/
			return;
	}
	gen_incode("PLATY: Multiplicative arithmetic expression parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <primary arithmetic expression> production.

Production:			<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
FIRST SET:			FIRST (<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), arithmeticExpression(), syn_printe(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void primaryArithmeticExpression() {
#ifdef DEBUG
printf("I am here: 24\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T:
			match(lookahead.code, NO_ATTR);
			break;
		case LPR_T:
			match(LPR_T, NO_ATTR);
			arithmeticExpression();
			match(RPR_T, NO_ATTR);
			break;
		default: /*print error if other*/
			syn_printe();
			/*return;*/
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <string expression> production.

Production:			<string expression> -> <primary string expression> <string expression'>
FIRST SET:			FIRST (<string expression>) = { SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	primaryStringExpression(), stringExpression_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void stringExpression() {
#ifdef DEBUG
printf("I am here: 25\n");
#endif
	primaryStringExpression();
	stringExpression_p();
}

/********************************************************************************************************************
Purpose:			Representation of the <string expression'> production.

Production:			<string expression'> -> # <primary string expression> <string expression'> | e
FIRST SET:			FIRST (<string expression'>) = { SCC_OP_T, e}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	primaryStringExpression(), stringExpression_p(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void stringExpression_p() {
#ifdef DEBUG
printf("I am here: 26\n");
#endif
	switch (lookahead.code) {
		case SCC_OP_T:
			match(SCC_OP_T, NO_ATTR);
			primaryStringExpression();
			stringExpression_p();
			break;
		default:
			gen_incode("PLATY: String expression parsed");
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <primary string expression> production.

Production:			<primary string expression> -> SVID_T | STR_T
FIRST SET:			FIRST (<primary string expression>) = { SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), syn_printe(),  gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void primaryStringExpression() {
#ifdef DEBUG
printf("I am here: 27\n");
#endif
	switch (lookahead.code) {
		case SVID_T:
		case STR_T:
			match(lookahead.code, NO_ATTR);
			gen_incode("PLATY: Primary string expression parsed");
			break;
		default:/*print error if other*/
			syn_printe();
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <conditional expression> production.

Production:			<conditional expression> -> <logical OR expression>
FIRST SET:			FIRST (<conditional expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	logicalOrExpression(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void conditionalExpression() {
#ifdef DEBUG
printf("I am here: 28\n");
#endif
	logicalOrExpression();
	gen_incode("PLATY: Conditional expression parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <logical OR expression> production.

Production:			<logical OR expression> -> <logical AND expression> <logical OR expression'>
FIRST SET:			FIRST (<logical OR expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	logicalAndExpression(), logicalOrExpression_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void logicalOrExpression() {
#ifdef DEBUG
printf("I am here: 29\n");
#endif
	logicalAndExpression();
	logicalOrExpression_p();
}

/********************************************************************************************************************
Purpose:			Representation of the <logical OR expression'> production.

Production:			<logical OR expression'> -> .OR. <logical AND expression> <logical OR expression'> | e
FIRST SET:			FIRST (<logical OR expression'>) = { LOG_OP_T(.OR.) , e}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	logicalAndExpression(), logicalOrExpression_p(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void logicalOrExpression_p() {
#ifdef DEBUG
printf("I am here: 30\n");
#endif
	switch (lookahead.code) {
		case LOG_OP_T:
			switch (lookahead.attribute.log_op) {
				case OR:
					match(LOG_OP_T, OR);
					logicalAndExpression();
					logicalOrExpression_p();
					gen_incode("PLATY: Logical OR expression parsed");
					
					break;
				default:
					break;
			}
			break;
		default:
			/*gen_incode("PLATY: Logical OR expression parsed");*/
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <logical AND expression> production.

Production:			<logical AND expression> -> <relational expression> <logical AND expression'>
FIRST SET:			 FIRST (<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	relationalExpression(),logicalAndExpression_p()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void logicalAndExpression() {
#ifdef DEBUG
printf("I am here: 31\n");
#endif
	relationalExpression();
	logicalAndExpression_p();
}

/********************************************************************************************************************
Purpose:			Representation of the <logical AND expression'> production.

Production:			<logical AND expression'> -> .AND. <relational expression> <logical AND expression'> | e
FIRST SET:			 FIRST (<logical AND expression'>) = {LOG_OP_T(.AND.) , e}

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	relationalExpression(), logicalAndExpression_p(), match(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void logicalAndExpression_p() {
#ifdef DEBUG
printf("I am here: 32\n");
#endif
	switch (lookahead.code) {
		case LOG_OP_T:
			switch (lookahead.attribute.log_op) {
				case AND:
					match(LOG_OP_T, AND);
					relationalExpression();
					logicalAndExpression_p();
					gen_incode("PLATY: Logical AND expression parsed");
					break;
				default:
					break;
			}
			break;
		default:
			/*gen_incode("PLATY: Logical AND expression parsed");*/
			break;
	}
}

/*********************************************************************************************************************************
Purpose:			Representation of the <relational expression> production.

Production:			<relational expression> ->
						<primary a_relational expression> <primary a_relational expression'>| 
						<primary s_relational expression> <primary s_relational expression'>
FIRST SET:			 FIRST <relational expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	primaryA_relExpression(), primaryA_relExpression_p(), primaryS_relExpression(), primaryS_relExpression_p()
					gen_incode(), syn_printe()
Parameters:			none
Return value:		none
**********************************************************************************************************************************/
void relationalExpression() {
#ifdef DEBUG
printf("I am here: 33\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T: primaryA_relExpression(); 
					primaryA_relExpression_p(); 
					break;
		case SVID_T:
		case STR_T: primaryS_relExpression(); 
					primaryS_relExpression_p(); 
					break;
		default: /*no empty*/
			syn_printe();
			/*return;*/
			break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <primary a_relational expression> production.

Production:			<primary a_relational expression> -> AVID_T | FPL_T | INL_T
FIRST SET:			 FIRST (<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), syn_printe(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void primaryA_relExpression() {
#ifdef DEBUG
printf("I am here: 34\n");
#endif
	switch (lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T:
			match(lookahead.code, NO_ATTR);
			/*gen_incode("PLATY: Primary a_relational expression parsed");*/
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/********************************************************************************************************************
Purpose:			Representation of the <primary a_relational expression'> production.

Production:			<primary a_relational expression'> ->
						== <primary a_relational expression>
						| <> <primary a_relational expression>
						| < <primary a_relational expression>
						| > <primary a_relational expression>
FIRST SET:			 FIRST (<primary a_relational expression'>) = { REL_OP_T(EQ), REL_OP_T(NE),
							REL_OP_T(LT), REL_OP_T(GT) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), primaryA_relExpression(), syn_printe(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void primaryA_relExpression_p() {
#ifdef DEBUG
printf("I am here: 35\n");
#endif
	switch (lookahead.code){
		case REL_OP_T:
			switch (lookahead.attribute.rel_op){
				case EQ:
				case NE:
				case GT:
				case LT:
					match(lookahead.code, lookahead.attribute.rel_op);
					primaryA_relExpression();
					break;
			}
			break;
		default:
			syn_printe();
			break;
	}
}

/********************************************************************************************************************
Purpose:			Representation of the <primary s_relational expression> production.

Production:			<primary s_relational expression> -> <primary string expression>
FIRST SET:			 FIRST (<primary s_relational expression>) = { SVID_T, STR_T }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	primaryStringExpression(), gen_incode()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void primaryS_relExpression() {
#ifdef DEBUG
printf("I am here: 36\n");
#endif
	primaryStringExpression();
	gen_incode("PLATY: Primary s_relational expression parsed");

}

/********************************************************************************************************************
Purpose:			Representation of the <primary s_relational expression'> production.

Production:			<primary s_relational expression’> ->
						== <primary s_relational expression>
						| <> <primary s_relational expression>
						| < <primary s_relational expression>
						| > <primary s_relational expression>
FIRST SET:			FIRST (<primary s_relational expression’>) = { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT) }

Author:				Zaid Versey & Vitor de Souza
History/Versions:	1.0
Called functions:	match(), primaryS_relExpression(), syn_printe()
Parameters:			none
Return value:		none
*********************************************************************************************************************/
void primaryS_relExpression_p() {
#ifdef DEBUG
printf("I am here: 37\n");
#endif
	switch (lookahead.code) {
		case REL_OP_T:
			switch (lookahead.attribute.rel_op) {
				case EQ:
				case NE:
				case GT:
				case LT:
					match(lookahead.code, lookahead.attribute.rel_op);
					primaryS_relExpression();
					break;
			}
			break;
		default: /*print error if other*/
			syn_printe();
			break;
	}
}

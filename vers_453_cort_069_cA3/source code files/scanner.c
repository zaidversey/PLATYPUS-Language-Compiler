/********************************************************************************************************************
File name:			scanner.c
Compiler:			MS Visual Studio 2015
Author:				Zaid Verey (040936453) & Vitor Cortes de Souza (040853069)
Course:				CST 8152 - Compilers, Lab Section: 13
Assignment:			2 - Scanner
Date:				November 30, 2017
Professor:			Sv.Ranev
Purpose:			Functions implementing a Lexical Analyzer (Scanner)

Function list:		scanner_init(), malar_next_token(), get_next_state(), char_class(),iskeyword(char * kw_lexeme),
					aa_func02(), aa_func03(), aa_func05(),aa_func08(), aa_func11(),aa_func12(),
					b_isempty (), b_rewind(), b_clear, 
********************************************************************************************************************/
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
extern Buffer * str_LTBL; /* String literal table */
int line; /* current line number of the source code */
extern int scerrnum; /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */
/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup function */
Token runtimeError (void); /* runtime error function*/

/********************************************************************************************************************
Purpose:			Initialize the scanner
Author:				Sv. Ranev
History/Versions:	1.0
Called functions:	b_isempty (), b_rewind(), b_clear
Parameters:			sc_buf		Buffer pointer       (N/A)
Return value:		EXIT_SUCCESS        int     	 ON SUCCESS
					EXIT_FALIUE			int			 ON FAILURE
Algorithm:			- Check if the source buffer is empty
					- Rewind to begining of buffer
					- Clear string literal buffer
					- set line count to the 1st line
*********************************************************************************************************************/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/* scerrnum = 0;  *//*no need - global ANSI C */
}

/********************************************************************************************************************
Purpose:			Perform token recognition by reading in a lexeme (character by character) from the 
					input stream (sc_buf)
Author:				Zaid Versey & Vitor Cortes de Souza
History/Versions:	1.0
Called functions:	b_getc(), b_retract(), b_mark(), b_getcoffset(), b_reset(), b_addc(), strcpy(), isalpha(),
					isdigit(), get_next_state(), b_allocate, b_free(), 
Parameters:			sc_buf		Buffer pointer       (N/A)
Return value:		t        	Token     	 		 (N/A)
Algorithm:			- Read in a character one at a time forming a lexeme
					- Return a token when a pattern is matched
					- Check for runtime errors
					- Token driven and transion table driven
*********************************************************************************************************************/
Token malar_next_token(Buffer * sc_buf) {
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */

	int i = 0; /*counter*/
	int isNotComment = 0; /*Non-zero if wrong comment*/
	int charCount = 0; 

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER*/
		c = b_getc(sc_buf);
		switch (c) {

		/*Ignore all white space characters: */
		case '\t': /*horizontal tab*/
		case '\v': /*verical tabe*/
		case '\f': /*form feed*/
		case '\r': /*carriage return*/
		case ' ': continue;
		case '\n':
			line++; continue; 

		/*Assignment operator or Equals operator*/
		case '=':
			/*check next letter for "=", if it is, Equals operator, if not Assignmnet Operator and retract*/
			c = b_getc(sc_buf);

			if (c != '=') {
				t.code = ASS_OP_T;
				/*no attribute*/
				b_retract(sc_buf);
				return t;
			}
			t.code = REL_OP_T;
			t.attribute.rel_op = EQ;
			return t;

		/*Left paranthesis*/
		case '(':
			t.code = LPR_T;
			/*no attribute*/
			return t;

		/*Right paranthesis*/
		case ')':
			t.code = RPR_T;
			/*no attribute*/
			return t;

		/*Left brace*/
		case '{':
			t.code = LBR_T;
			/*no attribute*/
			return t;

		/*Right brace*/
		case '}':
			t.code = RBR_T;
			/*no attribute*/
			return t;

		/*Less Than(<) operator or Not Equals (<>) operator*/
		case '<':
			t.code = REL_OP_T;
			/*check next letter for ">", if it is - NotEquals operator, if not - Less Than Operator and retract*/
			c = b_getc(sc_buf);

			if (c == '>') {
				t.attribute.rel_op = NE;
				return t;
			}
			t.attribute.rel_op = LT;
			b_retract(sc_buf);
			return t;

		/*Greater Than operator*/
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;

		/*Comma*/
		case ',':
			t.code = COM_T;
			/*no attribute*/
			return t;
		/*End of Statement*/
		case ';':
			t.code = EOS_T;
			/*no attribute*/
			return t;

		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;

		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;

		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;

		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		/*String concat*/
		case '#':
			t.code = SCC_OP_T;
			/*no attribute*/
			return t;

		/*Ignore comments*/
		case '!':
			t.code = ERR_T; /*Error if not comment*/
			c = b_getc(sc_buf);

			/*if next character is not '!', set ERR_T, attribute and commentFlag*/
			if (c != '!') {
				isNotComment = 1;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
			}

			/*ignore everything till end of line, error or not(assume it's a comment)*/
			while (c != '\n' && c != SEOF1 && c != SEOF2) {
				c = b_getc(sc_buf);
			}

			if(c == '\n' || c == '\r') line++;
			/*if not comment return ERR_T else continue(ignore comment)*/
			if (isNotComment != 0) {
				return t; /*WARNING -Ignore as it depends on commentFlag*/
			}
			continue;

		/*String literal*/
		case '"':
			
			b_mark(sc_buf, b_getcoffset(sc_buf));/*set mark to beginning of string*/

			/*keep getting next character until '"' is found or SEOF, count the lines*/
			do {
				c = b_getc(sc_buf);
				charCount++;
				if(c == '\n') line++;
			} while (c != '"' && c != SEOF1 && c != SEOF2);

			b_reset(sc_buf); /*retract to mark*/

			/*if end quote is found, add string to str_LTBL*/
			if (c == '"') {
				t.code = STR_T;
				/*postion of first char of string in str table will be at addc_offest*/
				t.attribute.str_offset = b_limit(str_LTBL);/*str_LTBL->addc_offset;*/
				c = b_getc(sc_buf); /* get next character, otherwise c will be " and it won't get in the loop */
				
				/*add the string to string literal table character by character*/
				while (c != '"') {/*WARNING assignment in condtitional*/
					if (b_addc(str_LTBL, c) == NULL) {
						return runtimeError ();
					}
					
					c = b_getc(sc_buf);
				}
				/*add /0 to end string*/
				if (b_addc(str_LTBL, '\0') == NULL) {
					return runtimeError ();
				}
				return t;
			}
			/*if end quotes not found (SEOF) set error token) */
			t.code = ERR_T;
			b_retract(sc_buf);/* need this to print '"' */
			if (charCount > ERR_LEN) {
				charCount = ERR_LEN - 3;
				for (i = 0; i < charCount; i++) {
					c = b_getc(sc_buf);
					t.attribute.err_lex[i] = c;
				}
				for (i = charCount; i < ERR_LEN; i++) {
					t.attribute.err_lex[i] = '.';
				}
				t.attribute.err_lex[ERR_LEN] = '\0';

			}
			else {
				for (i = 0; i < charCount; i++) {
					c = b_getc(sc_buf);
					t.attribute.err_lex[i] = c;
				}
				t.attribute.err_lex[i] = '\0';
			}

			/*Ignore rest till end of file */
			while (c != SEOF1 && c != SEOF2) {
				c = b_getc(sc_buf);
			}
			return t;
		/* "." | ".AND." | ".OR."*/
		case '.':
			b_mark(sc_buf, b_getcoffset(sc_buf));

			c = b_getc(sc_buf);
			/*retractCount++;*/
			if (c == 'A') {
				c = b_getc(sc_buf);
				/*retractCount++;*/
				if (c == 'N') {
					c = b_getc(sc_buf);
					/*retractCount++;*/
					if (c == 'D') {
						c = b_getc(sc_buf);
						/*retractCount++;*/
						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
			}else if (c == 'O') {
				c = b_getc(sc_buf);
				/*retractCount++;*/
				if (c == 'R') {
					c = b_getc(sc_buf);
					/*retractCount++;*/
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}

			t.code = ERR_T;
			strcpy(t.attribute.err_lex, ".");
			b_reset(sc_buf);
			return t;

		/*End of file*/
		case SEOF1:
		case SEOF2:
			t.code = SEOF_T;
			/*no attribute*/
			return t;
		/*SVID | AVID | IL | FPL | HIL*/
		default:
			if (isalpha(c) || isdigit(c)) {

				b_retract(sc_buf);
				/*set lexstart to the beginning of the lexeme*/
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));
				
				/*Loop through state machine until accepting state */
				while (accept == NOAS) {
					c = b_getc(sc_buf);
					state = get_next_state(state, c, &accept);

				}

				/*if the accepting state is with retract, than retract */
				if (accept == ASWR) {
					b_retract(sc_buf);
				}

				/*set lexend to end of the lexeme*/
				lexend = b_getcoffset(sc_buf);
				/*create lexeme buffer to the size of the lexeme*/
				lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');/* +1 for "\0"*/
				if (lex_buf == NULL) {
					return runtimeError ();
				}
				/*reset back to mark*/
				b_reset(sc_buf);

				/*copy the lexeme from input stream buffer(sc_buf) to the lexem buffer (lex_buf)*/
				for (i = 0; i < (lexend - lexstart); i++) {
					c = b_getc(sc_buf);
					b_addc(lex_buf, c);
				}
				/*add "\0" to end string of lexeme buffer*/
				b_addc(lex_buf, '\0');

				/*call corresponding accepting state function to get correct token*/
				t = aa_table[state](b_location(lex_buf, 0));
				b_free(lex_buf); /*free lexeme buffer*/

				return t;

			}else { /*invalid character*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			}
		}/*end of switch*/
	}/*end of while*/
}/*end of function*/

/********************************************************************************************************************
Purpose:			Determine next state 
Author:				Svillen Ranev
History/Versions:	1.0
Called functions:	char_class(), printf(), exit(), assert(),  
Parameters:			state		int			current state: 1 - 13
					c			char		character from input buffer
					accept		int*		reference to accepting state variable	  	
Return value:		next       	int     	the next state
Algorithm:			- get current state corresponding transition table column
					- get the next state using current state and column
					- set accepting state from the derived state (next)
					- return next state
*********************************************************************************************************************/
int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/********************************************************************************************************************
Purpose:			Determine transition table column of current character
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	isalpha(), islower(), isdigit(), 
Parameters:			c			char		character from input buffer
Return value:		val       	int     	column number
Algorithm:			- Analyze character
					- determine column position
					- return corresponding column number
*********************************************************************************************************************/
int char_class(char c) {
	int val = 8;/*other*/

	if (isalpha(c)) {
		if (islower(c)) {
			val = 1;/*a - z*/
			if (c == 'x') {
				val = 0;
			}
		}
		else {/* A-Z*/
			if (c >= 'A' && c <= 'F') {/*A-F*/
				val = 2;
			}
			else {/*G-Z*/
				val = 3;
			}
		}

	}else if (isdigit(c)) {
		if (c == '0') { /*Zero*/
			val = 4;
		}else if (c >= '1' && c <= '9') {/*NzD*/
			val = 5;
		}

	}else if (c == '.') {
		val = 6;
	}else if (c == '$') {
		val = 7;
	}
	return val;
}

/************DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS**************/

/********************************************************************************************************************
Purpose:			Recognise AVID or KW token
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	iskeword(), strlen() 
Parameters:			lexeme			char*		lexeme
Return value:		t      			Token     	AVID
Algorithm:			- Analyze lexeme
					- determine if keword or AVID token
					- determine if AVID is integer or float
					- set and return determined token and attribute
*********************************************************************************************************************/
Token aa_func02(char lexeme[]) {/*AVID /KW*/
	Token t; /*token*/
	t.code = AVID_T; /*token intitially AVID_T*/
	int keywordIndex = iskeyword(lexeme); /*kewword table index if keyword*/
	int lexemeLength = strlen(lexeme); /*length of lexeme*/
	int i; /*counter*/

	/*if lexeme is a keyword, return keyword token*/
	if (keywordIndex != NOT_KEYWORD) {
		t.code = KW_T;
		t.attribute.kwt_idx = keywordIndex;
		return t;
	}

	/*if AVID length is longer than VID_LEN, then length is forced to VID_LEN and rest truncated*/
	if (lexemeLength > VID_LEN) {
		lexemeLength = VID_LEN;
	}

	/*copy AVID lexeme to token attribute*/
	for (i = 0; i < lexemeLength; i++) {
		t.attribute.vid_lex[i] = lexeme[i];
	}
	t.attribute.vid_lex[i] = '\0'; /* "\0" to end string*/

	return t;
}

/********************************************************************************************************************
Purpose:			Recognise SVID token
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	strlen() 
Parameters:			lexeme			char*		lexeme
Return value:		t      			Token     	SVID
Algorithm:			- Analyze lexeme
					- check lexeme length
					- set and return SVID token and attribute
*********************************************************************************************************************/
Token aa_func03(char lexeme[]) {
	Token t;
	t.code = SVID_T;
	int lexemeLength = strlen(lexeme); /*get lexeme length*/
	int i;

	/*if SVID length is longer than VID_LEN, then length is forced to VID_LEN and rest truncated*/
	if (lexemeLength > VID_LEN) {
		lexemeLength = VID_LEN;
	}

	/*copy SVID lexeme to token attribute*/
	for (i = 0; i < lexemeLength; i++) { 
		t.attribute.vid_lex[i] = lexeme[i];
	}
	t.attribute.vid_lex[--i] = '$';
	t.attribute.vid_lex[++i] = '\0';
	return t;
}

/********************************************************************************************************************
Purpose:			Recognise INL token
Author:				Vitor Cortes de Souza
History/Versions:	1.0
Called functions:	atoi(), aa_func12()
Parameters:			lexeme			char*		lexeme
Return value:		t      			Token     	INL_T
Algorithm:			- Analyze and convert lexeme to INL
					- check range (2 bytes)
					- set and return INL token and attribute
*********************************************************************************************************************/
Token aa_func05(char lexeme[]) {
	Token t;
	long num; /*contains the converted lexeme*/
	int lexemeLength = strlen(lexeme);
	/*check lexeme length - cannot exceed INL_LEN*/
	if (lexemeLength > INL_LEN){
		return aa_func12(lexeme);
	}

	num = atol(lexeme); /*convert lexeme to long*/

	if (num > PLATY_MAX || num < 0) {
		return aa_func12(lexeme);
	}

	t.code = INL_T;
	t.attribute.int_value = num;
	return t;
}

/********************************************************************************************************************
Purpose:			Recognise FPL token
Author:				Vitor Cortes de Souza
History/Versions:	1.0
Called functions:	atof(), aa_func12()
Parameters:			lexeme			char*		lexeme
Return value:		t      			Token     	FPL_T
Algorithm:			- Convert lexeme to FPL
					- check range (4 bytes)
					- set and return FPL token and attribute
*********************************************************************************************************************/
Token aa_func08(char lexeme[]) {
	Token t;
	double f; /*contains the converted lexeme*/

	f = (double)atof(lexeme); /*convert lexeme to float*/
	
	if (f != 0 && (f < FLT_MIN || f > FLT_MAX)) { /*check range*/
		return aa_func12(lexeme);
	}

	t.code = FPL_T;
	t.attribute.flt_value = (float)f;
	return t;
}

/********************************************************************************************************************
Purpose:			Recognise HIL token
Author:				Vitor Cortes de Souza
History/Versions:	1.0
Called functions:	strtol(), aa_func12() 
Parameters:			lexeme			char*		lexeme
Return value:		t      			Token     	INL_T
Algorithm:			- convert lexeme INL base 16 (hexadecimal value int)
					- check range ()
					- set and return INL token and attribute
*********************************************************************************************************************/
Token aa_func11(char lexeme[]) {
	Token t;
	long h; /*contains the converted lexeme*/
	int lexemeLength = strlen(lexeme);
	/*check lexeme length - cannot exceed INL_LEN*/
	if (lexemeLength > INL_LEN + 1){ /* +1 for 'x'*/
		return aa_func12(lexeme);
	}

	h = strtol(lexeme, NULL, 16); /*either this or atolh(char*)*/
	
	if (h > PLATY_MAX || h < 0 ) { /*out of range*/
		return aa_func12(lexeme);
	}

	t.code = INL_T;
	/*t.attribute.int_value = h;*/
	t.attribute.int_value = h;
	return t;
}

/********************************************************************************************************************
Purpose:			Recognise ES/ER token
Author:				Vitor Cortes de Souza
History/Versions:	1.0
Called functions:	strlen() 
Parameters:			lexeme			char*		lexeme
Return value:		t      			Token     	ERR_T
Algorithm:			- Analyze lexeme
					- check lexeme length
					- set and return ER/ES token and attribute
*********************************************************************************************************************/
Token aa_func12(char lexeme[]) {/*error state for both retract and non retract,,, retract will allready by handled*/
	Token t;
	t.code = ERR_T;
	unsigned int i;

	/*if lexeme is longer than ERR_LEN, then length is forced to ERR_LEN - 3 and rest truncated, ... added*/
	if (strlen(lexeme)>ERR_LEN) {
		for (i = 0; i < ERR_LEN - 3; i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		for (i; i < ERR_LEN; i++) {
			t.attribute.err_lex[i] = '.';
		}
		t.attribute.err_lex[ERR_LEN] = '\0';

	}
	else {
		for (i = 0; i < strlen(lexeme); i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = '\0';
	}
	return t;
}

/********************************************************************************************************************
Purpose:			determine is lexeme is a keyword
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	strcmp()
Parameters:			kw_lexeme	char*		lexeme
Return value:		i       	int     	keword table index
					NOT_KEYWORD int			const
Algorithm:			- Analyze lexeme to see if it is a keyword
					- if keword return keword table index
					- otherwise return NOT_KEYWORD const
*********************************************************************************************************************/
int iskeyword(char * lexeme) {
	unsigned int i;/*array counter*/

	/*loop through keyword table to see if lexeme matches a keyword*/
	for (i = 0; i < KWT_SIZE; i++) {

		if (strcmp(lexeme, kw_table[i]) == 0) {
			return i;
		}
	}
	return NOT_KEYWORD;
}

/********************************************************************************************************************
Purpose:			set scernum and return Error token
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	strcpy()
Parameters:			t			Token		--
Return value:		t			Token		ERR_T
Algorithm:			- set scernum and token
					- set "RUN TIME ERROR" in token attribute
					- return error token
*********************************************************************************************************************/
Token runtimeError (void) {
	Token t;
	scerrnum = 1;
	t.code = ERR_T;
	strcpy(t.attribute.err_lex, "RUN TIME ERROR");
	return t;
}

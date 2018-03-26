/********************************************************************************************************************
File name:			buffer.c
Compiler:			MS Visual Studio 2015
Author:				Zaid Versey, 040836453
Course:				CST 8152 - Compilers, Lab Section: 13
Assignment:			1
Date:				September 29, 2017
Professor:			Sv.Ranev
Purpose:			This file contains only the function definitions that will allow for the creation,
implementation, and manipulation of a character buffer

Function list:		b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark()
b_mode(), b_incfactor(), b_load(), b_isempty(), b_eob(), b_getc(), b_print(), b_compact()
b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()
********************************************************************************************************************/

#include "buffer.h"

/********************************************************************************************************************
Purpose:			This function dynamically allocates memory the buffer structure in heap memory, and assigns its
corresponding values with values from the function parameters
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	calloc(), malloc(), free()
Parameters:			init_capacity      short       (0 to SHRT_MAX - 1)
					inc_factor         short       (mode add: 1 - 255, mode mult: 1 - 100)
					o_mode              char        (f, a , m)
Return value:		b                   pBuffer     ON SUCCESS
					NULL                N/A         ON FAILURE
Algorithm:			- Check arguments for validity
					- Allocate memory for buffer descriptor, then validate
					- Allocate memory for character buffer, then validate
					- Determine the buffer's mode and set the mode and increment factor for the buffer if in
					valid range, otherwise free any dynamically allocated memory and return NULL
*********************************************************************************************************************/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {
	pBuffer b; /*pointer to buffer descriptor*/

			   /*check if initial capacity is in range*/
	if (init_capacity < 0 || init_capacity > MAX_CAPACITY) {
		return NULL;
	}

	/*allocate memory for Buffer Descriptor, check for NULL*/
	if ((b = (Buffer *)calloc(1, sizeof(Buffer))) == NULL) /*Possible warning - assignment in conditional*/
		return NULL;

	/*assign the capacity*/
	b->capacity = init_capacity;

	/*allocate memory for character buffer, check for NULL
	- if return NULL, free any previously allocated memory and return NULL*/
	if ((b->cb_head = (char*)malloc(sizeof(char)*(init_capacity))) == NULL) { /*Possible warning - assignment in conditional*/
		free(b); /*Free buffer descriptor*/
		return NULL;
	}

	/*ASSIGN THE BUFFER DESCRIPTOR VALUES DEPENDING ON THE MODE:*/

	/*if inc_factor = 0, its the same as fixed mode*/
	if ((unsigned char)inc_factor == 0) {
		if (init_capacity == 0) {
			free(b->cb_head);
			free(b);
			return NULL;
		}
		b->mode = MODE_FIXED;
		b->inc_factor = 0;
	}
	else {

		switch (o_mode) {
			/*FIXED*/
		case 'f':
			/*If initial capacity is 0 in fixed mode, no character can be added*/
			if (init_capacity == 0) {
				free(b->cb_head);
				free(b);
				return NULL;
			}

			/*if incfactor  >= 0*/
			b->mode = MODE_FIXED;
			b->inc_factor = 0;
			break;

			/*ADDITIVE*/
		case 'a':
			if ((unsigned char)inc_factor <= ADD_MAX_INC) {
				b->mode = MODE_ADD;
				b->inc_factor = inc_factor;
			}
			else { /* Free any allocated memory & return NULL*/
				free(b->cb_head);
				free(b);
				return NULL;
			}
			break;

			/*MULTIPLICATIVE*/
		case 'm':
			if ((unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= MULT_MAX_INC) {
				b->mode = MODE_MULT;
				b->inc_factor = inc_factor;
			}
			else {
				free(b->cb_head);
				free(b);
				return NULL;
			}
			break;

		default: /*if mode not (f, a, m)*/
			free(b->cb_head);
			free(b);
			return NULL;
		}

	}

	return b;
}

/********************************************************************************************************************
Purpose:			This function adds a character to the buffer, and depending on the mode, expands the buffer if
if buffer capacity is reached. Capacity can expand until SHRT_MAX - 1
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	realloc(), sizeof(),
Parameters:			pBD         const pBuffer       (N/A)
					symbol      char                (Signed (-128 - 127)casted to Unsigned (0 - 255))
Return value:		pBD         pBuffer             ON SUCCESS
					NULL        N/A                 ON FAILURE
Algorithm:          - Check arguments for validity
					- If buffer capacity not reached, add the character to the buffer
					- If buffer capacity has been reached
					# Check if current capacity has not reached the maximum capacity allowed (SHRT_MAX - 1)
					# If mode is fixed - do not expand buffer
					# If mode is additive - add inc_factor to current capacity. and check if the newCapacity is
					in the valid range. Reallocate buffer to the newcapacity, and add the character
					# If mode is multiplicative - Multiply inc_factor with avaialable space, get percentage
					increment. Check increment for zero and adjust. Add the newIncrement to the current
					capacity. Check if newCapacity is in valid range. Reallocate buffer to the newcapacity,
					and add the character
					# set applicable buffer descriptor values
					- Return pointer to the buffer
*********************************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	short newCapacity = 0; 		/* the new capacity of the buffer if needed*/
	short availableSpace = 0; 	/* the available space from the limit to the Maximum space allowed*/
	short newIncrement = 0; 	/* Determines how much to increment the current capacity by*/
	char * temp;				/* Used as temporary vairiable to reallocate memory, in case of failure*/

	if (pBD == NULL)
		return NULL;

	pBD->r_flag = 0; /*reset r_flag*/

					 /*if the current capacity of the buffer is not full and addc_offset in valid range, add charcter to buffer*/
	if ((short)(pBD->addc_offset * sizeof(char)) < pBD->capacity) {
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}
	else {

		/* if current capacity of the buffer is full, reallocate or return depending on mode */
		/*if ((short)(pBD->addc_offset * sizeof(char)) == pBD->capacity) {*//*if ONE*/
																			/* if the current capacity has reached max capactiy, don't allocate more space*/
		if (pBD->capacity >= MAX_CAPACITY)
			return NULL;

		switch (pBD->mode) {
		case MODE_FIXED:
			return NULL;
			break;

		case MODE_ADD:
			newCapacity = sizeof(char)*(pBD->capacity + (unsigned char)pBD->inc_factor);

			if (newCapacity < 0)
				return NULL;
			/*if newCapacity == SHRT_MAX, set to SHRT_MAX - 1*/
			if (newCapacity > MAX_CAPACITY)
				newCapacity = MAX_CAPACITY;
			break;

		case MODE_MULT:
			availableSpace = (MAX_CAPACITY)-pBD->capacity;
			newIncrement = (availableSpace * (unsigned char)pBD->inc_factor / 100);
			/*if available space is below 100, newIncrement will always tend to 0, hence just add the available space*/
			if (newIncrement == 0)
				newIncrement = availableSpace;

			newCapacity = sizeof(char)*(pBD->capacity + newIncrement);
			break;
		}/*end of switch*/

		 /*if newCapacity overflows, return NULL*/

		 /*RESIZE TO NEW CAPACITY, ADD THE CHARACTER, AND CHANGE ACCORDING BUFFER DESCRIPTORS:*/

		 /*reallocate memory for character buffer, check for NULL*/
		if ((temp = (char*)realloc(pBD->cb_head, newCapacity)) == NULL)/*Possible warning - assignment in conditional*/
			return NULL;

		/*if realloc did reallocate buffer to a new memory location, set the flag*/
		if (temp != pBD->cb_head) {
			pBD->r_flag = SET_R_FLAG;
			pBD->cb_head = temp; /* if realloc successful, point cb_head to new memory location*/
		}

		pBD->cb_head[pBD->addc_offset++] = symbol; /* add the symbol to buffer using addc_offset, then increment it by 1*/
		pBD->capacity = newCapacity;

		return pBD;
	}/* end of if ONE*/

	/*return NULL;*/ /*addcoff < 0 || > 0, */
}

/********************************************************************************************************************
Purpose:	        Reset all buffer descriptor values to original state
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		0       int                 ON SUCCESS
-1      int                 ON FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
int b_clear(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->eob = 0;
	pBD->r_flag = 0;

	return SUCCESS;
}

/********************************************************************************************************************
Purpose:			Frees buffer descriptor and character buffer from the heap
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		void
Algorithm:			- - -
*********************************************************************************************************************/
void b_free(Buffer * const pBD) {
	if (pBD == NULL)
		return;

	free(pBD->cb_head);
	free(pBD);
}

/********************************************************************************************************************
Purpose:			Checks to see if the capacity of the character buffer has been reached or not
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		1       int                 IS FULL
0       int                 NOT FULL
-1      int                 FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
int b_isfull(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;
	else if ((short)(pBD->addc_offset * sizeof(char)) == pBD->capacity)
		return IS_FULL;
	else
		return NOT_FULL;
}

/********************************************************************************************************************
Purpose:			Returns the occupied size of the character buffer
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->addc_offset    short
-1     int                 FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
short b_limit(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	return pBD->addc_offset;
}

/********************************************************************************************************************
Purpose:			Returns the current capacity of the character buffer
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->capacity   short
-1      int                 FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
short b_capacity(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	return pBD->capacity;
}

/********************************************************************************************************************
Purpose:			Sets the mark position for the character buffer if in valid range.
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
mark    short               (0 - LIMIT)
Return value:		pBD->markc_offset   short
-1      int                 FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
short b_mark(Buffer * const pBD, short mark) {
	if (pBD == NULL)
		return RT_FAIL1;

	if (mark < 0 || mark > pBD->addc_offset)
		return RT_FAIL1;

	/*pBD->markc_offset = mark;*/
	return pBD->markc_offset = mark;
}

/********************************************************************************************************************
Purpose:			Returns the mode of the buffer
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->mode   char            (1, 0, -1)
-2          int             FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
int b_mode(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL2;

	return pBD->mode;
}

/********************************************************************************************************************
Purpose:			Returns the increment factor of the buffer
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->inc_factor     unsigned char
265     int     FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
size_t b_incfactor(Buffer * const pBD) {
	if (pBD == NULL)
		return 256;

	return (unsigned char)pBD->inc_factor;
}

/********************************************************************************************************************
Purpose:			Loads an input file into the buffer one character at a time
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	fgetc(), feof(), b_addc()
Parameters:			fi      const FILE*     (till EOF)
pBD     const pBuffer       (N/A)
Return value:		numChars        int
-2              int     LOAD FAIL
-1              int     FAILURE
Algorithm:			- Check arguments for validity
- Until EOF is not reached, keep inserting the characters from the file to the buffer
by calling function addc(). Checks if addc() fails to load character to buffer, maybe
in the case when addc cannot allocate more space for the buffer when its capacity becomes
greater than SHRT_MAX - 1
- Return the number of characters added to the buffer
*********************************************************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	char character; 	/*character stored & read from the file to be added to the buffer*/
	int numChars = 0; 	/* number of characters to be successfully added to the buffer*/

	if (fi == NULL || pBD == NULL)
		return RT_FAIL1;

	/*continuously add character to buffer until EOF is reached*/
	while (TRUE) { /*WARNING - Constant in while */
		character = (char)fgetc(fi);
		if (feof(fi))
			break;

		if (b_addc(pBD, character) == NULL)
			return LOAD_FAIL;

		numChars++;
	}
	return numChars;
}

/********************************************************************************************************************
Purpose:			Checks if the buffer is empty or not
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		1       int         IS EMPTY
0       int         NOT EMPTY
-1      int         FAILURE
Algorithm:          - - -
*********************************************************************************************************************/
int b_isempty(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	if (pBD->addc_offset == 0)
		return IS_EMPTY;

	return NOT_EMPTY;
}

/********************************************************************************************************************
Purpose:			Returns the end of buffer(eob) flag
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->eob    int
-1      int         FAILURE
Algorithm:	        - - -
*********************************************************************************************************************/
int b_eob(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	return pBD->eob;
}

/********************************************************************************************************************
Purpose:			Returns the character from the buffer by the position determined by getc_offset.
Checks if getc_offset is in valid range
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		character   char
-1          char        IF EOB REACHED
-2          char        FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
char b_getc(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL2;

	/*if EOB reached*/
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = SET_EOB;
		return RT_FAIL1;
	}
	/*if EOB not reached*/
	pBD->eob = 0;

	/* return the char then increment getc_offset*/
	return pBD->cb_head[pBD->getc_offset++];
}

/********************************************************************************************************************
Purpose:			Print the contents of the buffer character by character. Checks for empty buffer.
Returns the number of characters read/printed from the buffer
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	printf(), b_isempty(), b_eob(), b_getc()
Parameters:			pBD     const pBuffer       (N/A)
Return value:		character       int
-1              int         FAILURE
Algorithm:			- Check arguments for validity
- Checks if the buffer is empty
#If so print relevant msg
- While EOB is not reached
# Keep printing the contents of the buffer, character by character, using
function b_getc()
# Check if EOB is not reached after b_getc() is called
-return the number of character printed
*********************************************************************************************************************/
int b_print(Buffer * const pBD) {
	int characters = 0;	/* counter for number of character printed*/
	char c;				/*	used to store and print the character from the buffer*/
	if (pBD == NULL)
		return RT_FAIL1;

	if (b_isempty(pBD) == IS_EMPTY) {
		printf("Empty buffer\n");
		return characters;
	}

	/*countinuously get the character from the buffer & print it, until EOB is reached*/
	while (TRUE) { /*while end of buffer not reached*/
		c = (char)b_getc(pBD);

		if (pBD->eob == 1)
			break;
		printf("%c", c);
		characters++;
	}
	printf("\n");
	return characters;
}

/********************************************************************************************************************
Purpose:			Shrink(resize) the buffer to buffers current limit and adds the symbol to the end of the buffer
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	sizeof(), realloc()
Parameters:			pBD     const pBuffer       (N/A)
symbol  char
Return value:		pBD     pBuffer
NULL    ON FAILURE
Algorithm:			- Check arguments for validity
- Determine the newCapacity - addc_offset + 1
- Check newCapacity for overflow
- Reallocate memory for the buffer according to newCapacity
- Determine reallocation flag and set accordingly
- Add the symbol to the end of the buffer
- return the pointer to the buffer
*********************************************************************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	char *temp;			/* Used as temporary vairiable to reallocate memory, in case of failure*/
	short newCapacity;	/* Determines the new capacity for the buffer*/

	if (pBD == NULL)
		return NULL;

	pBD->r_flag = 0;
	newCapacity = sizeof(char)*(pBD->addc_offset + 1); /*size of datatype * how many you want + 1 more space for special character*/
													   /*check for overflow*/
	if (newCapacity < 0)
		return NULL;

	/*reallocate memory for character buffer, check for NULL*/
	if ((temp = (char*)realloc(pBD->cb_head, newCapacity)) == NULL)/*Possible warning - assignment in conditional*/
		return NULL;

	/*if realloc did reallocate buffer to a new memory location, set the flag*/
	if (temp != pBD->cb_head) {
		pBD->r_flag = SET_R_FLAG;
		pBD->cb_head = temp;
	}

	/*add the symbol to end of buffer & set all buffer descriptor values accordingly:*/
	pBD->capacity = newCapacity;
	pBD->cb_head[pBD->addc_offset++] = symbol;

	return pBD;
}

/********************************************************************************************************************
Purpose:			Returns the reallocation flag(r_flag)
Author:				Zaid Versey
History/Versions:	1.0
Parameters:			pBD     const pBuffer       (N/A)
Return value:		character       char
Return value:		pBD->r_flag     char
-1              char        FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
char b_rflag(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	return pBD->r_flag;
}

/********************************************************************************************************************
Purpose:			Decrements getc_offset by 1. Checks if getc_offset doesn't point to the head, else it will
point to negative (<0) after retraction
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->getc_offset    short
-1                  short   FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
short b_retract(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	if (pBD->getc_offset == 0)/* if points to head */
		return RT_FAIL1;

	return --pBD->getc_offset;/* decrement first then return */
}

/********************************************************************************************************************
Purpose:			Changes getc_offset back to markc_offset
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->getc_offset    short
-1                  short   FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
short b_reset(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	/*pBD->getc_offset = pBD->markc_offset;*/

	return pBD->getc_offset = pBD->markc_offset;
}

/********************************************************************************************************************
Purpose:			Returns the value of getc_offset
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		pBD->getc_offset    short
-1                  short   FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
short b_getcoffset(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	return pBD->getc_offset;
}

/********************************************************************************************************************
Purpose:			Sets getc_offset and markc_offset to the beginning of the buffer, so buffer can read again
from the start
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD     const pBuffer       (N/A)
Return value:		0       int     SUCCESS
-1      int     FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
int b_rewind(Buffer * const pBD) {
	if (pBD == NULL)
		return RT_FAIL1;

	pBD->getc_offset = pBD->markc_offset = 0;
	/*pBD->eob = 0;*//* getc_offset was set back to the head of buffer, hence EOB not reached*/

	return SUCCESS;
}

/********************************************************************************************************************
Purpose:			Returns a pointer to the location of the character buffer inidicated by loc_offset
Author:				Zaid Versey
History/Versions:	1.0
Called functions:	N/A
Parameters:			pBD             const pBuffer       (N/A)
loc_offset      short               (1 - addc_offset)
Return value:		memory address  char*
NULL    ON FAILURE
Algorithm:			- - -
*********************************************************************************************************************/
char * b_location(Buffer * const pBD, short loc_offset) {
	if (pBD == NULL)
		return NULL;

	if (loc_offset < 0 || loc_offset >= pBD->addc_offset)
		return NULL;

	return pBD->cb_head + loc_offset;
	/*return (char*)&pBD->cb_head[loc_offset];*/
}

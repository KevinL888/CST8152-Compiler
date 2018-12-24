/*************************************************************************************************************************************
File name			: buffer.c
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

#include "buffer.h"

/*************************************************************************************************************************************
Purpose				: creates a new buffer and sets the operational mode along with inc factor 
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: calloc() malloc() free()
Parameters			: short init_capacity : used to set the initial capacity of our character buffer(0 - 32766) bytes
					  char inc_factor : used as our increment when we need to reallocate our buffer to a bigger size capacity (0 - 255) bytes
					  char o_mode : used to specify our operational mode 'f':fixed 'a':additive 'm':multiplicative. ('f', 'a', 'm') bytes. 
Return value		: (Buffer) NULL: buffer was not created or returns a pointer to buffer descriptor structure
Algorithm			: - tries to allocate memory for a buffer structure calling calloc
					  - tries to allocate memory for one dynamic character buffer using malloc(cb_head) if the initial capacity
					   is from 0 - maximum allowed positive value. Otherwise we free the memory allocated for the buffer desc 
					   and return NULL.
					  - depending on inc_factor and o_mode
						Fixed Mode: check to see that parameters mode is 'f' then we check to see if capacity is 0, if the capacity
									is 0 then we free the memory allocated for the buffer desc and the start address of our chracter
									buffer and return NULL. Otherwise we can set the mode and inc_factor member of our buffer descriptor
									struct to 0 to indicate Fixed mode.

						Additive Mode: check to see that parameters mode is 'a' and that inc_factor is from 1 - 255 bytes. Set the
							   mode member of our buffer desc to 1 and the inc_factor equal to the given parameter as long as
							   it is from 1 - 255 bytes.

						Multiplicative Mode: check to see that parameters mode is 'm' and that inc_factor is from 1- 100 bytes. Set
											 mode member of our buffer desc to -1 and the inc_facor equal to the given parameter
											 as long as it is from 1- 100 bytes.
					  if none of these cases are a match than we just free up the memory dynamically allocated for the buffer desc
					  and the address to the beginning of the character buffer and return NULL.
					  - set capacity member of our buffer desc struct to parameter (initial capacity).
					  - set flags member of our buffer desc to DEFAULT CONSTANT defined in the buffer.h file.
					  - return a pointer to the buffer descriptor.
*************************************************************************************************************************************/

Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	pBuffer buffer = calloc(1, sizeof(Buffer));   /*dynamically allocate memory for buffer descriptor struct*/

	if (init_capacity >= ZERO && init_capacity <= MAXIMUM_ALLOWED_POSITIVE_VALUE)
	{
		buffer->cb_head = malloc(sizeof(char) * init_capacity);  /*dynamically allocate memory for beginning address of character buffer*/

		if (!buffer->cb_head)
			return NULL;
	}
	else {
		free(buffer); /*free buffer if initial capacity is a negative number or if it is bigger than the maximum allowed positive value*/
		return NULL;
	}
	
	/*fixed mode assigns inc factor to 0 because the buffer capacity is fixed and will never increment*/
	if ((o_mode == FIXED_MODE || (unsigned char)inc_factor == ZERO) || (o_mode == FIXED_MODE && (unsigned char)inc_factor != ZERO))
	{
		/*case where initial capacity is set to 0 in fixed mode (cannot add to buffer that has 0 capacity in fixed mode)*/
		if (init_capacity == ZERO) {
			free(buffer->cb_head);
			free(buffer);
			return NULL;
		}

		buffer->mode = buffer->inc_factor = FIXED;
	}
	/*additive mode we assign our mode to 1 and increment factor from 1 - 255 (bytes)*/
	else if (o_mode == ADDITIVE_MODE && (unsigned char)inc_factor > ZERO && (unsigned char)inc_factor <= UCHAR_MAX)
	{
		buffer->mode = ADDITIVE;
		buffer->inc_factor = inc_factor;
	}
	/*multiplicative mode we assign our mode to -1 and increment factor from 1 - 100 (bytes)*/
	else if (o_mode == MULTIPLICATIVE_MODE && (unsigned char)inc_factor > ZERO && (unsigned char)inc_factor <= HUNDRED)
	{
		buffer->mode = MULTIPLICATIVE;
		buffer->inc_factor = inc_factor;
	}
	/*if non of the cases are applicable then we just free the memory we allocated for 
	the buffer desc and starting address then return null*/
	else {
		free(buffer->cb_head);
		free(buffer);
		return NULL;
	}
	buffer->capacity = init_capacity;

	buffer->flags = DEFAULT_FALGS;

	return buffer;
}

/*************************************************************************************************************************************
Purpose				: adds a character symbol to the buffer. Resizes the buffer using realloc if it is full depending on
					  the operational mode. Sets the R flag if buffer is reallocated and returns NULL if catches a run-time error.

Author				: Kevin Lai
Versions			: 1.0
Called Functions	: realloc()
Parameters			: pBuffer const pBD : pointer to a buffer descriptor
					  char symbol: character to be added to the character buffer(any ascii character that can exist). 
Return value		: (pBuffer) returns pointer to buffer descriptor struct
Algorithm			: check to see if pBD(buffer descriptor) exist, returns NULL if it doesn't. Resets R flag back to default(0).
					  checks to see if there is enough room to add symbol to the buffer. If the buffer capacity is full 
					  (will try to resize the buffer by increasing capacity depending on the operational mode). 

					  Mode Fixed: return NULL  

					  Mode Additive: check to see if our buffer capacity is negative  or already at full capacity(return NULL)
					                 assign the new capacity as current capacity + inc factor, then checks to see if we have a
									 capacity overflow that results in negative if so we return NULL, otherwise we check if
									 current capacity is still lower than the maximum allowed positive value for the buffer
									 if so then we assign maximum allowed positive value as the new capacity

					  Mode Multiplicative:
									 checks to see if buffer is at full capacity. calculate new increment using formula
									 available space * inc_factor / 100. checks to see if the current capacity + the new inc factor
									 is less than the maximum allowed positive value and new increment is not 0
									 (no use to reallocate if new increment is 0), if case is true then we assigns the new capacity
									 as current capacity + new increment. Otherwise we check to see if the current capacity is still
									 lower then maximum allowed positive value for the buffer and assign maximum allowed positive value
									 as our new capacity.

					  Functions expands character buffer by using realloc, if realloc fails(NULL) than we return NULL sets the R flag bit
					  if the character buffer memory location has been changed. After reallocation appends the character symbol to the buffer,
					  increments addc_offset by 1 and updates the capacity variable. If at anytime there is a run-time error the
					  function will return NULL.
*************************************************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	short newCapacity = 0; /*calculates new capacity(current capacity + inc_factor)*/
	short newInc = 0;/*calculates new increment*/
	char *temp = NULL; /*temp variable used for when we reallocate buffer to see if location has been changed*/

	if (!pBD) return NULL;

	pBD->flags &= RESET_R_FLAG; /*resets R flag back to default (0)*/

	if (pBD->addc_offset < pBD->capacity) /*checks to see if there is enough room in the buffer to add new character symbol*/
	{
		*(pBD->cb_head + pBD->addc_offset++) = symbol;

		return pBD;
	}
	/*tries resizing the buffer depending on the operatational mode*/
	switch (pBD->mode)
	{
	case FIXED: return NULL;
		break;

	case ADDITIVE:

		/*checks to see if capacity is less than 0 or if capacity is greater than or equal to maximum allowed positive value*/
		if (pBD->capacity >= MAXIMUM_ALLOWED_POSITIVE_VALUE) return NULL;

		newCapacity = pBD->capacity + (unsigned char) pBD->inc_factor;

		/*checks to see if we have a capacity overflow that will result in a negative capacity
		otherwise we assign the new capacity with inc_factor*/
		if (newCapacity < ZERO)
			return NULL;
		else if (newCapacity > MAXIMUM_ALLOWED_POSITIVE_VALUE)
			newCapacity = MAXIMUM_ALLOWED_POSITIVE_VALUE;
		
		break;

	case MULTIPLICATIVE:

		if (pBD->capacity == MAXIMUM_ALLOWED_POSITIVE_VALUE) return NULL; 

		newInc = (MAXIMUM_ALLOWED_POSITIVE_VALUE - pBD->capacity) * (unsigned long) pBD->inc_factor / HUNDRED;

		/*check to see if current capacity + new increment is not greater than maximum allowed positive value*/
		if ((pBD->capacity + newInc < MAXIMUM_ALLOWED_POSITIVE_VALUE) && newInc != ZERO)
		{
			newCapacity = pBD->capacity + newInc; 
		}
		else if (pBD->capacity < MAXIMUM_ALLOWED_POSITIVE_VALUE) 
		{
			newCapacity = MAXIMUM_ALLOWED_POSITIVE_VALUE; 
		}
		break;

	default: return NULL;
		break;
	}

	pBD->capacity = newCapacity;

	temp = realloc(pBD->cb_head, pBD->capacity);  /*dynamically re-allocate memory (resize the character buffer)*/

	if (temp == NULL) return NULL;  /*if realloc fails and cannot resize the character buffer return NULL*/

	if (temp != (pBD->cb_head))  /*checks to see if memory address has been changed from re-allocating the buffer*/
	{
		pBD->flags |= SET_R_FLAG;  /*set the R flag bit to 1 (reallocation flag bit)*/
		pBD->cb_head = temp;   
	}
	if (pBD->addc_offset < pBD->capacity)   /*checks to see if there is room in buffer to add a character symbol*/
		*(pBD->cb_head + pBD->addc_offset++) = symbol;
	else
		return NULL;

	return pBD;
}

/*************************************************************************************************************************************
Purpose				: re-initializes offsets and flag variables so that the buffer will appear empty
					  and next character symbol will be appended to the beginning of the buffer
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: none
Parameters			: (int) Buffer * const pBD : pointer to a buffer descriptor
Return value		: -1(run time error), 0(function proceeded with no errors)
Algorithm			: 
*************************************************************************************************************************************/

int b_clear(Buffer * const pBD)
{
	if (!pBD) return RT_FAIL_1;

	/*set all offset values and flag back to default (0)*/
	pBD->addc_offset = pBD->getc_offset = pBD->markc_offset = ZERO;
	pBD->flags &= DEFAULT_FALGS;
	return ZERO;
}

/*************************************************************************************************************************************
Purpose				: de-allocates all memory that was occupied by the character buffer and the buffer structure
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: free()
Parameters			: Buffer * const pBD : pointer to a buffer descriptor
Return value		: (void) no return type
Algorithm			:
*************************************************************************************************************************************/
void b_free(Buffer * const pBD)
{
	/*free up all memory that was dynamically allocated for the character buffer*/
	if (pBD)
	{
		if (pBD->cb_head)
			free(pBD->cb_head);
		free(pBD);
	}
}

/*************************************************************************************************************************************
Purpose				: returns if the buffer is full or not
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: Buffer * const pBD : pointer to a buffer descriptor
Return value		: (int) function returns 1 if buffer is full otherwise it returns 0
Algorithm			:
*************************************************************************************************************************************/

  #ifndef B_FULL
int b_isfull(Buffer * const pBD)
{
	return pBD ? pBD->addc_offset == pBD->capacity ? ONE : ZERO : RT_FAIL_1;

}
 #endif


/*************************************************************************************************************************************
Purpose				: function return the current limit of the character buffer (the space that is being used by
					  all added (stored) characters measured in characters)
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: Buffer * const pBD  : pointer to a buffer descriptor
Return value		: (short) function returns -1 (RT_FAIL_1) : buffer was not created or returns the limit of the buffer(addc_offset)
Algorithm			:
*************************************************************************************************************************************/
short b_limit(Buffer * const pBD)
{
	return pBD ? pBD->addc_offset : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: functions returns the current capacity of the character buffer
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: Buffer * const pBD : pointer to a buffer descriptor
Return value		: (short) function returns -1 (RT_FAIL_1) : buffer was not created or returns current capacity of the buffer
Algorithm			:
*************************************************************************************************************************************/
short b_capacity(Buffer * const pBD)
{
	return pBD ? pBD->capacity : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: function sets the markc_offset variable to the argument that is passed in as a parameter (mark).
					  The parameter (mark) must be within the current limit of the character buffer range
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD : pointer to a buffer descriptor
					  short mark : value that is passed in and set to buffer desc member markc_offset(0-32766)
Return value		: (short) function returns -1 (RT_FAIL_1) : buffer was not created or returns the markc_offset
Algorithm			:
*************************************************************************************************************************************/
short b_mark(pBuffer const pBD, short mark)
{
	return pBD ? mark >= ZERO && mark <= pBD->addc_offset ? pBD->markc_offset = mark : RT_FAIL_1 : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: function returns value of mode
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD : pointer to a buffer descriptor
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or returns the operational mode
Algorithm			:
*************************************************************************************************************************************/
int b_mode(Buffer * const pBD)
{
	return pBD ? pBD->mode : RT_FAIL_2;
}

/*************************************************************************************************************************************
Purpose				: function returns the non-negative value of inc_factor
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD : pointer to a buffer descriptor
Return value		: (size_t) function returns 0x100 int(256) on failure or pBD->inc_factor casted as a unsigned char
Algorithm			:
*************************************************************************************************************************************/
size_t b_incfactor(Buffer * const pBD)
{
	return pBD ? (unsigned char)pBD->inc_factor : HEXA256;
}

/*************************************************************************************************************************************
Purpose				: Loads the file into the character buffer
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: fgetc(), feof(), b_addc(), ungetc(), printf()
Parameters			: FILE * const fi(pointer to a file stream), pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created
					  or returns -2 (LOAD_FAIL) character unable to be added to buffer
					  otherwise it returns addc_offset
Algorithm			: -reads an open input file that is specified by a file stream that is passed in as a parameter.
					  -uses the function fgetc to read one character at a time into the character buffer. 
					  -if the character is unable to be added, ungetc is called to return the character to the file stream
					  -prints the character as a character and integer. This process keeps happening until we reach file end of file 
					   which we then break from the loop
					  -return addc_offset (int).
*************************************************************************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD)
{
	char c = ZERO;  /*variable used to store character fetched from fgetc function*/

	if (!fi || !pBD) return RT_FAIL_1;

		/*loop until end of file stream.*/
		for (;;)
		{
			c = (char) fgetc(fi);  /*fetch a character using fgets and store it in a char variable (c)*/

			if (feof(fi)) /*check to see if we are at end of file*/
			{
				break;
			}

			if (!b_addc(pBD, c))   /*add character to buffer if there is enough room*/
			{
				ungetc(c, fi);   /*return character to the file stream*/
				printf("The last character read from the file is: %c %d\n", c, c);
				return LOAD_FAIL;
			}

		}
		return pBD->addc_offset;
	
}

/*************************************************************************************************************************************
Purpose				: function tells if the buffer is empty or not
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created
					  or returns 1 if buffer is empty otherwise it returns 0 if full.
Algorithm			:
*************************************************************************************************************************************/
int b_isempty(Buffer * const pBD)
{
	return pBD ? pBD->addc_offset == ZERO ? ONE : ZERO : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: function is used to read the buffer. Sets the flag field bits according to certain conditions and fetches a 
					  character from the buffer.
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (char) function returns -2 (RT_FAIL_2) : buffer was not created.
					  return 0 if EOB is set to 1 otherwise return the character at getc_offset in the character buffer
Algorithm			: function is used to read the buffer. validates the argument and using bitwise operation
					  sets the flag field eob bit to 1 if getc_offset is equal to addc_offset and returns 0.
					  Otherwise it sets eob bit to 0, increments the getc_offset by 1 and return the character located at getc_offset.

					  -check to see if pBD(buffer descriptor) exist, if not than return -2 (RT_FAIL_2)
					  -set EOB flag bit to 1 if getc_offset is equal to addc_offset (meaning we are at the end of the buffer)
					  otherwise we check to see if EOB bit is 1 and if it is than we reset the EOB bit back to 0
					  - increment the getc_offset by 1 and return the character that is located at getc_offset
*************************************************************************************************************************************/
char b_getc(Buffer * const pBD)
{
	if (!pBD) return RT_FAIL_2; 

	if (pBD->getc_offset == pBD->addc_offset)
	{
		pBD->flags |= SET_EOB; /*set EOB flag bit to 1 (means character buffer is full)*/
		return ZERO;
	}
	pBD->flags &= RESET_EOB;

	return *(pBD->cb_head + pBD->getc_offset++);
}

/*************************************************************************************************************************************
Purpose				: function uses bit wise operation to check the flag field eob bit and returns the eob bit field.
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or
					  returns the eob bit field, if it is 1 (signifies end of buffer) and 0 (hasn't reached the end of buffer yet).
Algorithm			:
*************************************************************************************************************************************/
int b_eob(Buffer * const pBD)
{
	return pBD ? (pBD->flags & CHECK_EOB) : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: function prints character by character the contents of the character buffer.
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: b_getc(), printf(), b_eob()
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or 0 if empty buffer. Otherwise we return
					  returns number of characters printed
Algorithm			: - checks to see if pBD(buffer descriptor) exist, returns -1 (RT_FAIL_1) if it does't.
					  - checks to see if addc_offset is equal to 0 and prints Empty buffer if true and returns 0.
					  - create a loop that fetchs a character using b_getc and saves it in a variable (c) then checks to see if
					    were at the end of the buffer using b_eob function (1 means full , 0 means not full yet) if were full than
						we break out of loop. Otherwise we print the character and keep iterating. 
					  - print a new line when we get out of loop and return getc_offset as an int
*************************************************************************************************************************************/
int b_print(Buffer * const pBD)
{
	char c = ZERO;  /*variable used to fetch character from b_getc function*/

	if (!pBD) return RT_FAIL_1;
	if (pBD->addc_offset == ZERO)
	{
		printf("Empty buffer!\n");
		return ZERO;
	}

	/*loop until end of buffer (eob flag bit is set to 1) printing all characters in the character buffer*/
	for(;;)
	{
		c = b_getc(pBD);

		/*check to see if we are at end of buffer so we don't print null character*/
		if (b_eob(pBD)) {
			break;
		}
		printf("%c", c);
	}
	printf("\n");
	return pBD->getc_offset;
}

/*************************************************************************************************************************************
Purpose				: function tries to reallocate buffer and add one to the buffer capacity and than adds the character symbol 
					  using addc_offset.
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: realloc()
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or 0 if empty buffer. Otherwise we return
					  returns number of characters printed
Algorithm			: - check to see if pBD exist otherwise we just return null.
					  - reallocate memory for buffer descriptor using realloc(increase the buffer size by 1) and save address to temp.
					  - check to see if the reallocation was success or if it return NULL(failure)
					  - check to see if temp has the same address as cb_head(old buffer location)
					  - set R flag if the address has been changed and also set cb_head to point to the new buffer location (temp)
					  - add the new character symbol using addc_offset
					  - update the new capacity (pBD->capacity)
					  - return the pointer to the buffer descriptor
*************************************************************************************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol)
{
	if (!pBD) return NULL;

	char *temp; /*temp variable used for when we reallocate buffer to see if location has been changed*/

	pBD->flags &= RESET_R_FLAG; /*resets R flag back to default (0)*/

	if (pBD->addc_offset == SHRT_MAX)
		return NULL;

	temp = realloc(pBD->cb_head, pBD->addc_offset + ONE);  /*re-allocate memory for buffer capacity( add one to the capacity)*/

	if (temp == NULL) return NULL;

	/*check to see if reallocating buffer has changed the memory address*/
	if (temp != (pBD->cb_head)) {
		pBD->flags |= SET_R_FLAG;
		pBD->cb_head = temp;
	}

	*(pBD->cb_head + pBD->addc_offset++) = symbol;
	pBD->capacity = pBD->addc_offset;
	return pBD;
}

/*************************************************************************************************************************************
Purpose				: function uses bit wise operation to check the flag field r bit and returns the r bit field.
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or
					  returns the flag field r bit ( 1 means that the location was changed)
					  otherwise 0 (buffer location was not changed)
Algorithm			:
*************************************************************************************************************************************/
char b_rflag(Buffer * const pBD)
{
	return pBD ? (pBD->flags & CHECK_R_FLAG) : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: function decrements getc_offset by 1
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or
					  returns getc_offset.
Algorithm			:
*************************************************************************************************************************************/
short b_retract(Buffer * const pBD)
{
	return pBD ? pBD->getc_offset > ZERO ? --(pBD->getc_offset) : RT_FAIL_1 : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: function sets getc_offset to the value of the current markc_offset
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or
					  returns getc_offset.
Algorithm			:
*************************************************************************************************************************************/
short b_reset(Buffer * const pBD)
{
	return pBD ? pBD->getc_offset = pBD->markc_offset : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: functions returns getc_offset to calling function
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (short) function returns -1 (RT_FAIL_1) : buffer was not created or
					  returns getc_offset
Algorithm			:
*************************************************************************************************************************************/
short b_getcoffset(Buffer * const pBD)
{
	return pBD ? pBD->getc_offset : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: functions sets getc_offset and markc_offset to 0
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
Return value		: (int) function returns -1 (RT_FAIL_1) : buffer was not created or
					  returns 0 if function proceeds without error
Algorithm			:
*************************************************************************************************************************************/
int b_rewind(Buffer * const pBD)
{
	return pBD ? pBD->getc_offset = pBD->markc_offset = ZERO : RT_FAIL_1;
}

/*************************************************************************************************************************************
Purpose				: returns the pointer to a character in the buffer specified by loc_offset
Author				: Kevin Lai
Versions			: 1.0
Called Functions	: N/A
Parameters			: pBuffer const pBD (buffer descriptor structure)
					  short loc_offset : offset used to get a location of a pointer to a character in the buffer. ( 0 - 32766) bytes
Return value		: (char) NULL: buffer was not created otherwise return a pointer to a character
					  in the buffer array specified by loc_offset
Algorithm			:
*************************************************************************************************************************************/
char * b_location(Buffer * const pBD, short loc_offset)
{
	return pBD ? loc_offset >= 0 && loc_offset < pBD->addc_offset ? (pBD->cb_head + loc_offset) : NULL : NULL;
}





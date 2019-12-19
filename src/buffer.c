/*
File name: buffer.c
Compiler: MS Visual Studio 2019
Author: Nicholas Sturgeon, 040911218
Course: CST 8152 – Compilers, Lab Section: 013 Assignment: 3
Date: 2019/12/05
Professor: Sv. Ranev
Purpose: Implementation file for the buffer containing function definitions
Function list:
    b_allocate();
    b_addc();
    b_clear();
    b_free();
    b_isfull();
    b_limit();
    b_capacity();
    b_mark();
    b_mode();
    b_incfactor();
    b_load();
    b_isempty();
    b_getc();
    b_eob();
    b_print();
    b_compact();
    b_rflag();
    b_retract();
    b_reset();
    b_getcoffset();
    b_rewind();
    b_location();
*/

#include "buffer.h"

/*
Purpose: Allocates space in memory for a Buffer and its character
         array based on provided parameters.
         Returns a pointer to the Buffer on success, or NULL on error.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: calloc()
                  malloc()
                  free()
Parameters: init_capacity: short (0 to SHRT_MAX - 1)
            inc_factor: char (0 to UCHAR_MAX)
            o_mode: char ('a', 'f', or 'm')
Return value: Buffer*
Algorithm: - Checks for invalid values
           - Initializes memory for the buffer
           - Sets initial values
           - Returns a pointer to the buffer
*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {
    Buffer* buffer; /* pointer to the buffer object to return */
    short capacity = (init_capacity == 0) ? DEFAULT_INIT_CAPACITY : init_capacity; /* capacity to assign to the buffer */

    /* guards */
    /* invalid init_capacity; capacity must be >= 0 and < max value of short */
    if (init_capacity < 0 || init_capacity > MAX_CAPACITY)
        return NULL;
    /* invalid o_mode; operational mode must be 'a', 'm', or 'f' */
    if (o_mode != MODE_ADD && o_mode != MODE_MUL && o_mode != MODE_FIX)
        return NULL;
    /* invalid inc_factor for 'm'; must not be greater than 100% */
    if (o_mode == MODE_MUL && inc_factor > PERCENT_MAX)
        return NULL;

    /* initialize memory */
    buffer = calloc(1, sizeof(Buffer));
    if (buffer == NULL)
        return NULL;
    buffer->cb_head = malloc(capacity);

    /* assign initial values */
    if (init_capacity == 0) {
        buffer->inc_factor = (o_mode == MODE_FIX) ? 0 : DEFAULT_INC_FACTOR;
        switch (o_mode) {
            case MODE_FIX:
                buffer->mode = B_MODE_FIX;
                break;
            case MODE_ADD:
                buffer->mode = B_MODE_ADD;
                break;
            case MODE_MUL:
                buffer->mode = B_MODE_MUL;
                break;
        }
    }
    else if (o_mode == MODE_FIX || inc_factor == 0) {
        buffer->mode = B_MODE_FIX;
        buffer->inc_factor = 0;
    }
    else if (o_mode == MODE_ADD) {
        buffer->mode = B_MODE_ADD;
        buffer->inc_factor = inc_factor;
    }
    else if (inc_factor <= 100) {
        buffer->mode = B_MODE_MUL;
        buffer->inc_factor = inc_factor;
    }
    else {
        free(buffer->cb_head);
        free(buffer);
        return NULL;
    }

    buffer->capacity = capacity;
    buffer->flags = DEFAULT_FLAGS;

    return buffer;
}

/*
Purpose: Adds a character to the Buffer's character array if there is space.
         If not, it attempts to increase the capacity based on the Buffer's
         properties or, if there is no remaining space, returns NULL;
         Sets the R flag if the memory location of the character array changed
         on reallocation.
         Increments the addc offset.
         Returns a pointer to the Buffer on success.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: b_incfactor()
                  printf()
                  realloc()
Parameters: pBD: pBuffer const
            symbol: char
Return value: pBuffer
Algorithm: - Resets R flag
           - Checks if the character array is full
           - Adds the character and returns if there is room, otherwise:
           - Determines new capacity
           - Reallocates with new capacity
           - Changes the memory location and sets the R flag if applicable
           - Adds the character to the character array
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
    long new_capacity; /* new capacity after increase */
    char* new_mem_loc; /* new memory location after realloc */

    if (pBD == NULL)
        return NULL;

    /* reset R flag */
    pBD->flags &= RESET_R_FLAG;

    /* check if full */
    if (pBD->addc_offset == MAX_CAPACITY)
        return NULL;

    /* if there is space, add the character, increment addc_offset and return */
    if (pBD->addc_offset < pBD->capacity) {
        *(pBD->cb_head + pBD->addc_offset++) = symbol;
        return pBD;
    }

    /* fixed mode can't increase capacity */
    if (pBD->mode == B_MODE_FIX)
        return NULL;

    /* determine new capacity */
    if (pBD->mode == B_MODE_ADD) {
        new_capacity = pBD->capacity + b_incfactor(pBD);
        if (new_capacity < 0)
            return NULL;
    }
    else if (pBD->mode == B_MODE_MUL) {
        short available_space = MAX_CAPACITY - pBD->capacity;
        if (available_space == 0)
            return NULL;

        new_capacity = pBD->capacity +
                       (available_space * (pBD->inc_factor / 100.0));

        if (new_capacity == pBD->capacity && new_capacity < MAX_CAPACITY)
            new_capacity = MAX_CAPACITY;
    }
    else {
        printf("Unsupported mode: %d\n", pBD->mode);
        return NULL;
    }

    /* check if new capacity is valid */
    if (new_capacity <= MAX_CAPACITY) {
        pBD->capacity = (short)new_capacity;
    }
    else if (new_capacity < 0) {
        return NULL;
    }
    else {
        pBD->capacity = MAX_CAPACITY;
    }

    /* reallocate memory with new capacity */
    new_mem_loc = realloc(pBD->cb_head, pBD->capacity);
    if (new_mem_loc == NULL)
        return NULL;

    /* assign new memory location if it changed */
    if (pBD->cb_head != new_mem_loc) {
        pBD->flags |= SET_R_FLAG;
        pBD->cb_head = new_mem_loc;
    }

    /* add the character */
    *(pBD->cb_head + pBD->addc_offset++) = symbol;

    return pBD;
}

/*
Purpose: Resets the properties of the Buffer relating to reading and writing to
         the character array, in order to allow re-writing to the Buffer.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: int (0 = Success, -1 = Error)
*/
int b_clear(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    /* reset values */
    pBD->addc_offset = 0;
    pBD->getc_offset = 0;
    pBD->markc_offset = 0;

    return 0;
}

/*
Purpose: Free all allocated memory for the Buffer and its character array.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: free()
Parameters: pBD: Buffer* const
Return value: None
*/
void b_free(Buffer* const pBD) {
    if (pBD != NULL) {
        free(pBD->cb_head);
        free(pBD);
    }
}

/*
Purpose: Checks if the Buffer is full and can no longer be written to.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: int (0 = Available, 1 = Full, -1 = Error)
*/
int b_isfull(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    if (pBD->addc_offset == pBD->capacity)
        return 1;

    return 0;
}

/*
Purpose: Returns the amount of chars currently in the Buffer's character array.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: short
*/
short b_limit(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    return pBD->addc_offset;
}

/*
Purpose: Returns the amount of chars currently in the Buffer's character array.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: short
 * Returns the current capacity of the Buffer's character array.
 */
short b_capacity(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    return pBD->capacity;
}

/*
Purpose: Sets the Buffer's markc_offset using the value of mark.
         If mark is not within the Buffer's addc_offset value, it
         returns an error.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: pBuffer const
            mark: short (0 to current addc_offset)
Return value: short
*/
short b_mark(pBuffer const pBD, short mark) {
    if (pBD == NULL)
        return RT_FAIL_1;

    /* check bounds */
    if (mark < 0 || mark > pBD->addc_offset)
        return RT_FAIL_1;

    pBD->markc_offset = mark;

    return pBD->markc_offset;
}

/*
Purpose: Returns the Buffer's mode value.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: int (-1, 0, or 1 = mode, -2 = Error)
*/
int b_mode(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_2; /* Since -1 is a valid mode */

    return pBD->mode;
}

/*
Purpose: Returns the Buffer's inc_factor value.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: size_t (0 - 255)
*/
size_t b_incfactor(Buffer* const pBD) {
    if (pBD == NULL)
        return UCHAR_MAX;

    /* normalize incfactor if below zero to get it into the range of 0 - 255 */
    if (pBD->inc_factor < 0)
        return pBD->inc_factor + UCHAR_MAX + 1;

    return pBD->inc_factor;
}

/*
Purpose: Loads a FILE's contents into the Buffer's character array.
         If the file is larger than the Buffer's max capacity, it returns
         the last character read to the FILE buffer and returns an error.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: feof()
                  getc()
                  b_addc()
                  ungetc()
Parameters: fi: FILE* const
            pBD: Buffer* const
Return value: int (0 = Success, -1 = Error)
Algorithm: - Checks for invalid values
           - Loops though file buffer
           - Adds character to buffer while valid
*/
int b_load(FILE* const fi, Buffer* const pBD) {
    char c; /* holds the next character from the file buffer */

    if (fi == NULL || pBD == NULL || pBD->cb_head == NULL)
        return RT_FAIL_1;

    /* loop through file and add each character to the buffer */
    while (!feof(fi)) {
        /* get next char and check for end of file */
        if ((c = (char)getc(fi)) != EOF) {
            if (b_addc(pBD, c) == NULL) {
                ungetc(c, fi); /* add character back to file buffer */
                return LOAD_FAIL;
            }
        }
    }

    return 0;
}

/*
Purpose: Checks if the Buffer's character array is empty.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: int (1 = Empty, 0 = Not Empty, -1 = Error)
*/
int b_isempty(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    return pBD->addc_offset == 0;
}

/*
Purpose: Returns the next character from the Buffer's character array.
         Sets EOB flag if there are no more characters to read.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: char
*/
char b_getc(Buffer* const pBD) {
    if (pBD == NULL || pBD->cb_head == NULL)
        return RT_FAIL_2;

    /* set end-of-buffer flag if all have been read */
    if (pBD->getc_offset == pBD->addc_offset) {
        pBD->flags |= SET_EOB;
        return 0;
    }
    else {
        pBD->flags &= RESET_EOB;
    }

    return *(pBD->cb_head + pBD->getc_offset++);
}

/*
Purpose: Returns the current value of the Buffer's EOB flag.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: int (0 or 1)
*/
int b_eob(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    return (pBD->flags & CHECK_EOB) >> 1;
}

/*
Purpose: Prints the contents of the Buffer's character array using printf().
         If 'nl' is non-zero, prints a new line at the end.
         Return the number of characters printed.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: b_getc()
                   b_eob()
                  printf()
Parameters: pBD: Buffer* const
            nl: char
Return value: int
Algorithm: - Checks for invalid values
           - Stores the current offset
           - Prints each character until b_eob() returns true
           - Restores the original offset to the buffer
           - Prints a newline if applicable
           - Returns a count of the characters printed
*/
int b_print(Buffer* const pBD, char nl) {
    int count = 0; /* number of characters read */
    short cur_offset; /* hold the current offset */

    if (pBD == NULL || pBD->cb_head == NULL)
        return RT_FAIL_1;

    cur_offset = pBD->getc_offset;

    /* print all characters in the buffer */
    pBD->getc_offset = 0;
    for (;;) {
        char c = b_getc(pBD);
        if (!b_eob(pBD))
            printf("%c", c);
        else
            break;
        count++;
    }
    if (nl != 0) {
        printf("\n");
    }

    /* restore the offset */
    pBD->getc_offset = cur_offset;

    return count;
}

/*
Purpose: Shrinks the Buffer's character array's capacity to fit the current
         number of characters plus space for one more character.
         Sets the R flag if the memory location of the character array changed
         on reallocation.
         Adds 'symbol' to the end of the character array and increments addc_offset.
         Returns a pointer to the Buffer.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: realloc()
Parameters: pBD: Buffer* const
            symbol: char
Return value: Buffer*
Algorithm: - Checks for invalid values
           - Reallocates memory to fit the current buffer
           - Handle memory location change if applicable
           - Adds the specified character to the end of the buffer
*/
Buffer* b_compact(Buffer* const pBD, char symbol) {
    char* new_mem_loc; /* holds the new memory location after reallocation */

    if (pBD == NULL || pBD->cb_head == NULL)
        return NULL;

    /* reset flag */
    pBD->flags &= RESET_R_FLAG;

    /* reallocate memory to fit */
    new_mem_loc = realloc(pBD->cb_head, pBD->addc_offset + 1);
    if (new_mem_loc == NULL) {
        return NULL;
    }

    /* set flag if memory location changed */
    if (pBD->cb_head != new_mem_loc) {
        pBD->flags |= SET_R_FLAG;
        pBD->cb_head = new_mem_loc;
    }

    /* set new capacity and add character */
    pBD->capacity = pBD->addc_offset + 1;
    *(pBD->cb_head + pBD->addc_offset++) = symbol;

    return pBD;
}

/*
Purpose: Returns the current value of the Buffer's R flag.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: char (0 or 1)
*/
char b_rflag(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    return pBD->flags & CHECK_R_FLAG;
}

/*
Purpose: Decrements the Buffer's getc_offset by one.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: short
*/
short b_retract(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    if (pBD->getc_offset == 0)
        return RT_FAIL_1;

    /* decrement and return */
    return --(pBD->getc_offset);
}

/*
Purpose: Sets the Buffer's getc_offset to markc_offset.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: short
*/
short b_reset(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    pBD->getc_offset = pBD->markc_offset;

    return pBD->getc_offset;
}

/*
Purpose: Gets the current value of the Buffer's getc_offset.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: short
*/
short b_getcoffset(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    return pBD->getc_offset;
}

/*
Purpose: Resets the Buffer's getc_offset and markc_offset
         so that the Buffer may be rewritten to.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: int (0 = Success, -1 = Error)
*/
int b_rewind(Buffer* const pBD) {
    if (pBD == NULL)
        return RT_FAIL_1;

    /* reset values */
    pBD->getc_offset = 0;
    pBD->markc_offset = 0;

    return 0;
}

/*
Purpose: Gets the memory address of the current mark
         in the Buffer.
Author: Nicholas Sturgeon
History/Versions: 1.0    2019/10/02
Called functions: None
Parameters: pBD: Buffer* const
Return value: char*
*/
char* b_location(Buffer* const pBD) {
    if (pBD == NULL)
        return NULL;

    return pBD->cb_head + pBD->markc_offset;
}

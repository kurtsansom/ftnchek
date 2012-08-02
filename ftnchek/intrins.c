/* $Id: intrins.c,v 1.4 2003/06/05 00:19:59 landrito Rel $

	Handles datatyping of intrinsic functions.
*/

/*


Copyright (c) 2001 by Robert K. Moniot.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Acknowledgement: the above permission notice is what is known
as the "MIT License."
*/
#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "intrins.h"

	/* Define positional flags to allow specifying more
	   than one allowed type of argument for generics.
	 */

#define I   (1 << type_INTEGER)
#define R   (1 << type_REAL)
#define D   (1 << type_DP)
#define C   (1 << type_COMPLEX)
#define Z   (1 << type_DCOMPLEX)
#define L   (1 << type_LOGICAL)
#define STR (1 << type_STRING)
#define DT  (1 << MIN_DTYPE_ID)	/* all derived types are merged to this */
#define ANY (I|R|D|C|Z|L|STR|DT) /* for intrinsics that act on any type */

	/* Table below contains information from Table 5, pp. 15-22
	   to 15-25 of the standard.  Note: num_args == -1 means 1 or 2 args,
	   num_args == -2 means 2 or more args.  Value of arg_type is the OR
	   of all allowable types (I, R, etc. as defined above).  Value of
	   result_type is type returned by function (type_INTEGER, etc.).
	   If result_type is type_GENERIC, function type is same as arg type.

	   If you add your own intrinsics to this list, the order is not
	   important and the table size adjusts automatically.
	*/

				/* Intrinsic function handlers */

PROTO(PRIVATE int ii_abs,( Token *args ));
PROTO(PRIVATE int ii_dim,( Token *args ));
PROTO(PRIVATE int ii_dot_product,( Token *args ));
PROTO(PRIVATE int ii_ichar,( Token *args ));
PROTO(PRIVATE int ii_index,( Token *args ));
PROTO(PRIVATE int ii_kind,( Token *args ));
PROTO(PRIVATE int ii_lbound,( Token *args ));
PROTO(PRIVATE int ii_len,( Token *args ));
PROTO(PRIVATE int ii_matmul,( Token *args ));
PROTO(PRIVATE int ii_max,( Token *args ));
PROTO(PRIVATE int ii_maxloc,( Token *args ));
PROTO(PRIVATE int ii_merge,( Token *args ));
PROTO(PRIVATE int ii_min,( Token *args ));
PROTO(PRIVATE int ii_minloc,( Token *args ));
PROTO(PRIVATE int ii_mod,( Token *args ));
PROTO(PRIVATE int ii_modulo,( Token *args ));
PROTO(PRIVATE int ii_null,( Token *args ));
PROTO(PRIVATE int ii_pack,( Token *args ));
PROTO(PRIVATE int ii_reshape,( Token *args ));
PROTO(PRIVATE int ii_selected_int_kind,( Token *args ));
PROTO(PRIVATE int ii_selected_real_kind,( Token *args ));
PROTO(PRIVATE int ii_shape,( Token *args ));
PROTO(PRIVATE int ii_sign,( Token *args ));
PROTO(PRIVATE int ii_size,( Token *args ));
PROTO(PRIVATE int ii_spread,( Token *args ));
PROTO(PRIVATE int ii_ubound,( Token *args ));
PROTO(PRIVATE int ii_unpack,( Token *args ));

PRIVATE IntrinsInfo intrinsic[]={


	/* Table contains: name, num_args, arg_type, result_type, flags.
	   Special num_args values are defined in symtab.h.

	   Flags: I_F77 if it is in Table 5 p. 15-24, I_NONF77 otherwise
		  I_NONF90 if it is not in Chap 13 of F90 standard
		  I_NONSTD = I_NONF77|I_NONF90|I_NONF95|I_NONF10
		  I_MIXED_ARGS if arguments are not all of same type.
		  I_NONPURE if function arg may be modified (eg RAND).
		  I_INQ if function is inquiry about arg (eg LEN, ASSOCIATED)
		  I_C_TO_R indicates complex -> real in generic cases
		      (ABS,IMAG,REAL).
		  I_SP_R indicates specific REAL result (REAL)
	          I_NOTARG if it is a generic with no specific meaning,
		      or if it is a type conversion, lexical relationship,
		      or min or max (cf. p. 15-3, sec. 15.3.2)
		  I_EXTRA indicates common nonstd function
		  I_VMS indicates VMS-specific function
		  I_UNIX indicates UNIX-specific function
		  I_EVAL specifies to run handler even if result not integer
		  I_PTR function returns a pointer
		  I_ELEM function is elemental
		  I_ARRY yields array-valued result different shape from args
	 */

#define I_NONSTD (I_NONF77|I_NONF90|I_NONF95)
  /* define shorthands for different standards */
#define I_F77 		0x00	/* Standard intrinsic (no flag: placeholder) */
#define I_F90 (I_NONF77)  		/* introduced in F90 */
#define I_F95 (I_NONF77|I_NONF90)	/* introduced in F95 */

  /* Standard intrinsic functions.  Those marked with 5 were added in
     F95.  The section numbers used here are those of F95; for F90
     section numbers replace 13.11 by 13.10 and 13.12 by 13.11. */

/*
13.11.1 Argument presence inquiry function
PRESENT (A)                Argument presence
*/

{"PRESENT",	1,	ANY,	type_LOGICAL,	I_F90|I_INQ,NULL},

/*
13.11.2 Numeric functions
ABS (A)                            Absolute value
AIMAG (Z)                          Imaginary part of a complex number
AINT (A [, KIND])                  Truncation to whole number
ANINT (A [, KIND])                 Nearest whole number
CEILING (A [, KIND])               Least integer greater than or equal to number
CMPLX (X [, Y, KIND])              Conversion to complex type
CONJG (Z)                          Conjugate of a complex number
DBLE (A)                           Conversion to double precision real type
DIM (X, Y)                         Positive difference
DPROD (X, Y)                       Double precision real product
FLOOR (A [, KIND])                 Greatest integer less than or equal to number
INT (A [, KIND])                   Conversion to integer type
MAX (A1, A2 [, A3,...])            Maximum value
MIN (A1, A2 [, A3,...])            Minimum value
MOD (A, P)                         Remainder function
MODULO (A, P)                      Modulo function
NINT (A [, KIND])                  Nearest integer
REAL (A [, KIND])                  Conversion to real type
SIGN (A, B)                        Transfer of sign
*/

{"ABS", 	1,	I|R|D|C|Z,type_GENERIC,	I_F77|I_ELEM|I_C_TO_R,ii_abs},
{"AIMAG",	1,	C,	type_REAL,	I_F77|I_ELEM,NULL},
{"AINT",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM|I_OK,NULL},
{"ANINT",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM|I_OK,NULL},
{"CEILING",	1,	R|D,	type_INTEGER,	I_F90|I_ELEM|I_OK,NULL},
{"CMPLX",	I_1or2,	I|R|D|C|Z,type_COMPLEX,	I_F77|I_ELEM|I_NOTARG|I_OK|I_CMPLX,NULL},
{"CONJG",	1,	C,	type_COMPLEX,	I_F77|I_ELEM,NULL},
{"DBLE",	1,	I|R|D|C|Z,type_DP,	I_F77|I_ELEM|I_NOTARG,NULL},
{"DIM",		2,	I|R|D,	type_GENERIC,	I_F77|I_ELEM,ii_dim},
{"DPROD",	2,	R,	type_DP,	I_F77|I_ELEM,NULL},
{"FLOOR",	1,	R|D,	type_INTEGER,	I_F90|I_ELEM|I_OK,NULL},
{"INT", 	1,	I|R|D|C|Z,type_INTEGER,	I_F77|I_ELEM|I_NOTARG|I_OK,NULL},
{"MAX",		I_2up,	I|R|D,	type_GENERIC,	I_F77|I_ELEM|I_NOTARG,ii_max},
{"MIN", 	I_2up,	I|R|D,	type_GENERIC,	I_F77|I_ELEM|I_NOTARG,ii_min},
{"MOD", 	2,	I|R|D,	type_GENERIC,	I_F77|I_ELEM,ii_mod},
{"MODULO",	2,	I|R|D,	type_GENERIC,	I_F90|I_ELEM,ii_modulo},
{"NINT",	1,	R|D,	type_INTEGER,	I_F77|I_ELEM|I_OK,NULL},
{"REAL",	1,	I|R|D|C|Z,type_GENERIC, I_F77|I_ELEM|I_NOTARG|I_C_TO_R|I_SP_R|I_OK,NULL},
{"SIGN",	2,	I|R|D,	type_GENERIC,	I_F77|I_ELEM,ii_sign},

			/* Fortran 66 specific versions */
{"AMAX0",	I_2up,	I,	type_REAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"AMAX1",	I_2up,	R,	type_REAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"AMIN0",	I_2up,	I,	type_REAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"AMIN1",	I_2up,	R,	type_REAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"AMOD",	2,	R,	type_REAL,	I_F77|I_ELEM,NULL},
{"CABS",	1,	C,	type_REAL,	I_F77|I_ELEM,NULL},
{"DABS",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DDIM",	2,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DINT",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DMAX1",	I_2up,	D,	type_DP,	I_F77|I_ELEM|I_NOTARG,NULL},
{"DMIN1",	I_2up,	D,	type_DP,	I_F77|I_ELEM|I_NOTARG,NULL},
{"DMOD",	2,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DNINT",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DSIGN",	2,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"FLOAT",	1,	I,	type_REAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"IABS",	1,	I,	type_INTEGER,	I_F77|I_ELEM,ii_abs},
{"IDIM",	2,	I,	type_INTEGER,	I_F77|I_ELEM,ii_dim},
{"IDINT",	1,	D,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,NULL},
{"IDNINT",	1,	D,	type_INTEGER,	I_F77|I_ELEM,NULL},
{"IFIX",	1,	R,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,NULL},
{"ISIGN",	2,	I,	type_INTEGER,	I_F77|I_ELEM,ii_sign},
{"MAX0",	I_2up,	I,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,ii_max},
{"MAX1",	I_2up,	R,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,NULL},
{"MIN0",	I_2up,	I,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,ii_min},
{"MIN1",	I_2up,	R,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,NULL},
{"SNGL",	1,	D,	type_REAL,	I_F77|I_ELEM|I_NOTARG,NULL},

/*
13.11.3 Mathematical functions
ACOS (X)                           Arccosine
ASIN (X)                           Arcsine
ATAN (X)                           Arctangent
ATAN2 (Y, X)                       Arctangent
COS (X)                            Cosine
COSH (X)                           Hyperbolic cosine
EXP (X)                            Exponential
LOG (X)                            Natural logarithm
LOG10 (X)                          Common logarithm (base 10)
SIN (X)                            Sine
SINH (X)                           Hyperbolic sine
SQRT (X)                           Square root
TAN (X)                            Tangent
TANH (X)                           Hyperbolic tangent
*/

{"ACOS",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"ASIN",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"ATAN",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"ATAN2",	2,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"COS", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_ELEM,NULL},
{"COSH",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"EXP",		1,	R|D|C|Z,type_GENERIC,	I_F77|I_ELEM,NULL},
{"LOG", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_ELEM|I_NOTARG,NULL},
{"LOG10",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM|I_NOTARG,NULL},
{"SIN", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_ELEM,NULL},
{"SINH",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"SQRT",	1,	R|D|C|Z,type_GENERIC,	I_F77|I_ELEM,NULL},
{"TAN", 	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},
{"TANH",	1,	R|D,	type_GENERIC,	I_F77|I_ELEM,NULL},

			/* Fortran 66 specific versions */
{"ALOG",	1,	R,	type_REAL,	I_F77|I_ELEM,NULL},
{"ALOG10",	1,	R,	type_REAL,	I_F77|I_ELEM,NULL},
{"CCOS",	1,	C,	type_COMPLEX,	I_F77|I_ELEM,NULL},
{"CEXP",	1,	C,	type_COMPLEX,	I_F77|I_ELEM,NULL},
{"CLOG",	1,	C,	type_COMPLEX,	I_F77|I_ELEM,NULL},
{"CSIN",	1,	C,	type_COMPLEX,	I_F77|I_ELEM,NULL},
{"CSQRT",	1,	C,	type_COMPLEX,	I_F77|I_ELEM,NULL},
{"DACOS",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DASIN",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DATAN",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DATAN2",	2,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DCOS",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DCOSH",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DEXP",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DLOG",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DLOG10",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DSIN",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DSINH",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DSQRT",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DTAN",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},
{"DTANH",	1,	D,	type_DP,	I_F77|I_ELEM,NULL},

/*
13.11.4 Character functions
ACHAR (I)                          Character in given position
                                      in ASCII collating sequence
ADJUSTL (STRING)                   Adjust left
ADJUSTR (STRING)                   Adjust right
CHAR (I [, KIND])                  Character in given position
                                      in processor collating sequence
IACHAR (C)                         Position of a character
                                      in ASCII collating sequence
ICHAR (C)                          Position of a character
                                      in processor collating sequence
INDEX (STRING, SUBSTRING [, BACK]) Starting position of a substring
LEN_TRIM (STRING)                  Length without trailing blank characters
LGE (STRING_A, STRING_B)           Lexically greater than or equal
LGT (STRING_A, STRING_B)           Lexically greater than
LLE (STRING_A, STRING_B)           Lexically less than or equal
LLT (STRING_A, STRING_B)           Lexically less than
REPEAT (STRING, NCOPIES)           Repeated concatenation
SCAN (STRING, SET [, BACK])        Scan a string for a character in a set
TRIM (STRING)                      Remove trailing blank characters
VERIFY (STRING, SET [, BACK])      Verify the set of characters in a string
*/

{"ACHAR",	1,	I,	type_STRING,	I_F90|I_ELEM|I_CHAR,NULL},
{"ADJUSTL",	1,	STR,	type_STRING,	I_F90|I_ELEM,NULL},
{"ADJUSTR",	1,	STR,	type_STRING,	I_F90|I_ELEM,NULL},
{"CHAR",	1,	I,	type_STRING,	I_F77|I_ELEM|I_NOTARG|I_CHAR|I_OK,NULL},
{"IACHAR",	1,	STR,	type_INTEGER,	I_F90|I_ELEM,ii_ichar},
{"ICHAR",	1,	STR,	type_INTEGER,	I_F77|I_ELEM|I_NOTARG,ii_ichar},
{"INDEX",	2,	STR,	type_INTEGER,	I_F77|I_ELEM,ii_index},
{"LEN_TRIM", 	1,	STR,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"LGE", 	2,	STR,	type_LOGICAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"LGT", 	2,	STR,	type_LOGICAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"LLE", 	2,	STR,	type_LOGICAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"LLT", 	2,	STR,	type_LOGICAL,	I_F77|I_ELEM|I_NOTARG,NULL},
{"REPEAT", 	2,	STR|I,	type_STRING,	I_F90|I_MIXED_ARGS,NULL},
{"SCAN", 	I_2or3,	STR|I|L,type_INTEGER,	I_F90|I_MIXED_ARGS|I_ELEM,NULL},
{"TRIM", 	1,	STR,	type_STRING,	I_F90,NULL},
{"VERIFY", 	I_2or3,	STR|L,	type_INTEGER,	I_F90|I_MIXED_ARGS|I_ELEM,NULL},

/*
13.11.5 Character inquiry function
LEN (STRING)                       Length of a character entity
*/

{"LEN", 	1,	STR,	type_INTEGER,	I_F77|I_INQ,ii_len},

/*
13.11.6 Kind functions
KIND (X)                           Kind type parameter value
SELECTED_INT_KIND (R)              Integer kind type parameter value,
                                      given range
SELECTED_REAL_KIND ([P, R])        Real kind type parameter value,
                                      given precision and range
*/

{"KIND",	1,	ANY,	type_INTEGER,	I_F90|I_INQ,ii_kind},
{"SELECTED_INT_KIND",1,	I,	type_INTEGER,	I_F90,ii_selected_int_kind},
{"SELECTED_REAL_KIND",I_1or2,I,	type_INTEGER,	I_F90,ii_selected_real_kind},

/*
13.11.7 Logical function
LOGICAL (L [, KIND])               Convert between objects of type logical with
                                      different kind type parameters
*/
{"LOGICAL", 	1,	L,	type_LOGICAL,	I_F90|I_MIXED_ARGS|I_ELEM|I_OK,NULL},

/*
13.11.8 Numeric inquiry functions
DIGITS (X)                         Number of significant digits of the model
EPSILON (X)                        Number that is almost negligible
                                      compared to one
HUGE (X)                           Largest number of the model
MAXEXPONENT (X)                    Maximum exponent of the model
MINEXPONENT (X)                    Minimum exponent of the model
PRECISION (X)                      Decimal precision
RADIX (X)                          Base of the model
RANGE (X)                          Decimal exponent range
TINY (X)                           Smallest positive number of the model
*/
{"DIGITS",	1,	I|R|D,	type_INTEGER,	I_F90|I_INQ,NULL},
{"EPSILON",	1,	R|D,	type_GENERIC,	I_F90|I_INQ,NULL},
{"HUGE",	1,	I|R|D,	type_GENERIC,	I_F90|I_INQ,NULL},
{"MAXEXPONENT",	1,	R|D,	type_INTEGER,	I_F90|I_INQ,NULL},
{"MINEXPONENT",	1,	R|D,	type_INTEGER,	I_F90|I_INQ,NULL},
{"PRECISION",	1,	R|D|C|Z,type_INTEGER,	I_F90|I_INQ,NULL},
{"RADIX",	1,	I|R|D,	type_INTEGER,	I_F90|I_INQ,NULL},
{"RANGE",	1,	I|R|D|C|Z,type_INTEGER,	I_F90|I_INQ,NULL},
{"TINY",	1,	R|D,	type_GENERIC,	I_F90|I_INQ,NULL},

/*
13.11.9 Bit inquiry function
BIT_SIZE (I)                       Number of bits of the model
*/
{"BIT_SIZE",	1,	I,	type_INTEGER,	I_F90|I_INQ,NULL},

/*
13.11.10 Bit manipulation functions
BTEST (I, POS)                     Bit testing
IAND (I, J)                        Logical AND
IBCLR (I, POS)                     Clear bit
IBITS (I, POS, LEN)                Bit extraction
IBSET (I, POS)                     Set bit
IEOR (I, J)                        Exclusive OR
IOR (I, J)                         Inclusive OR
ISHFT (I, SHIFT)                   Logical shift
ISHFTC (I, SHIFT [, SIZE])         Circular shift
NOT (I)                            Logical complement
*/

{"BTEST",	2,	I,	type_LOGICAL,	I_F90|I_ELEM,NULL},
{"IAND",	2,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"IBCLR",	2,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"IBITS",	3,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"IBSET",	2,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"IEOR",	2,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"IOR",		2,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"ISHFT",	2,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"ISHFTC",	I_2or3,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"NOT",		1,	I,	type_INTEGER,	I_F90|I_ELEM,NULL},

/*
13.11.11 Transfer function
TRANSFER (SOURCE, MOLD [, SIZE])   Treat first argument as if
                                      of type of second argument
*/

    /* TRANSFER ( GENERIC, GENERIC, INTEGER ) */
{"TRANSFER",	I_2or3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},

/*
13.11.12 Floating-point manipulation functions
EXPONENT (X)                       Exponent part of a model number
FRACTION (X)                       Fractional part of a number
NEAREST (X, S)                     Nearest different processor number in
                                      given direction
RRSPACING (X)                      Reciprocal of the relative spacing
                                   of model numbers near given number
SCALE (X, I)                       Multiply a real by its base to an integer power
SET_EXPONENT (X, I)                Set exponent part of a number
SPACING (X)                        Absolute spacing of model numbers near given
                                   number
*/

{"EXPONENT",	1,	R|D,	type_INTEGER,	I_F90|I_ELEM,NULL},
{"FRACTION",	1,	R|D,	type_GENERIC,	I_F90|I_ELEM,NULL},
{"NEAREST",	2,	R|D,	type_GENERIC,	I_F90|I_ELEM,NULL},
{"RRSPACING",	1,	R|D,	type_GENERIC,	I_F90|I_ELEM,NULL},
{"SCALE",	2,	R|D|I,	type_GENERIC,	I_F90|I_MIXED_ARGS|I_ELEM,NULL},
{"SET_EXPONENT",2,	R|D|I,	type_GENERIC,	I_F90|I_MIXED_ARGS|I_ELEM,NULL},
{"SPACING",	1,	R|D,	type_GENERIC,	I_F90|I_ELEM,NULL},

/*
13.11.13 Vector and matrix multiply functions
DOT_PRODUCT (VECTOR_A, VECTOR_B)   Dot product of two rank-one arrays
MATMUL (MATRIX_A, MATRIX_B)        Matrix multiplication
*/

{"DOT_PRODUCT",	2,	I|R|D|C|L,	type_GENERIC,	I_F90|I_MIXED_ARGS|I_ARRY,ii_dot_product},
{"MATMUL",	2,	I|R|D|C|L,	type_GENERIC,	I_F90|I_MIXED_ARGS|I_ARRY,ii_matmul},

/*
13.11.14 Array reduction functions
ALL (MASK [, DIM])                 True if all values are true
ANY (MASK [, DIM])                 True if any value is true
COUNT (MASK [, DIM])               Number of true elements in an array
MAXVAL (ARRAY [, DIM] [, MASK])       Maximum value in an array
MINVAL (ARRAY [, DIM] [, MASK])       Minimum value in an array
PRODUCT (ARRAY [, DIM] [, MASK])      Product of array elements
SUM (ARRAY [, DIM] [, MASK])          Sum of array elements
*/
	/* Note: ordering of array,mask,dim args not enforced.  For type_GENERIC
	   result will be type of first argument.
	 */
{"ALL",		I_1or2,	I|L,	type_LOGICAL,	I_F90|I_MIXED_ARGS,NULL},
{"ANY",		I_1or2,	I|L,	type_LOGICAL,	I_F90|I_MIXED_ARGS,NULL},
{"COUNT",	I_1or2,	I|L,	type_INTEGER,	I_F90|I_MIXED_ARGS,NULL},
{"MAXVAL",	I_1to3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},
{"MINVAL",	I_1to3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},
{"PRODUCT",	I_1to3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},
{"SUM",		I_1to3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},

/*
13.11.15 Array inquiry functions
ALLOCATED (ARRAY)                  Array allocation status
LBOUND (ARRAY [, DIM])             Lower dimension bounds of an array
SHAPE (SOURCE)                     Shape of an array or scalar
SIZE (ARRAY [, DIM])               Total number of elements in an array
UBOUND (ARRAY [, DIM])             Upper dimension bounds of an array
*/
{"ALLOCATED",   1,      ANY,	type_LOGICAL,	I_F90|I_INQ,NULL},
{"LBOUND",	1,	ANY,	type_INTEGER,	I_F90|I_ARRY|I_INQ,ii_lbound},
{"SHAPE",	1,	ANY,	type_INTEGER,	I_F90|I_ARRY|I_INQ,ii_shape},
{"SIZE",	I_1or2, ANY,	type_INTEGER,	I_F90|I_MIXED_ARGS|I_INQ,ii_size},
{"UBOUND",	1,	ANY,	type_INTEGER,	I_F90|I_ARRY|I_INQ,ii_ubound},

/*
13.11.16 Array construction functions
MERGE (TSOURCE, FSOURCE, MASK)     Merge under mask
PACK (ARRAY, MASK [, VECTOR])      Pack an array into an array of rank one
                                      under a mask
SPREAD (SOURCE, DIM, NCOPIES)      Replicates array by adding a dimension
UNPACK (VECTOR, MASK, FIELD)       Unpack an array of rank one into an array
                                      under a mask
*/
{"MERGE",	3,	ANY,	type_GENERIC,	I_F90|I_ARRY|I_MIXED_ARGS,ii_merge},
{"PACK",	I_2or3,	ANY,	type_GENERIC,	I_F90|I_ARRY|I_MIXED_ARGS,ii_pack},
{"SPREAD",	3,	ANY,	type_GENERIC,	I_F90|I_ARRY|I_MIXED_ARGS,ii_spread},
{"UNPACK",	3,	ANY,	type_GENERIC,	I_F90|I_ARRY|I_MIXED_ARGS,ii_unpack},

/*
13.11.17 Array reshape function
RESHAPE (SOURCE, SHAPE[, PAD, ORDER]) Reshape an array
*/
{"RESHAPE",	I_2to4,	ANY,	type_GENERIC,	I_F90|I_ARRY|I_MIXED_ARGS,ii_reshape},

/*
13.11.18 Array manipulation functions
CSHIFT (ARRAY, SHIFT [, DIM])      Circular shift
EOSHIFT (ARRAY, SHIFT [, BOUNDARY, DIM]) End-off shift
TRANSPOSE (MATRIX)                 Transpose of an array of rank two
*/

{"CSHIFT",	I_2or3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},
{"EOSHIFT",	I_2or3,	ANY,	type_GENERIC,	I_F90|I_MIXED_ARGS,NULL},
{"TRANSPOSE",	1,	ANY,	type_GENERIC,	I_F90,NULL},

/*
13.11.19 Array location functions
MAXLOC (ARRAY [, DIM] [, MASK])       Location of a maximum value in an array
MINLOC (ARRAY [, DIM] [, MASK])       Location of a minimum value in an array
*/
{"MAXLOC",	I_1to3,	I|R|L,	type_INTEGER,	I_F90|I_ARRY|I_MIXED_ARGS,ii_maxloc},
{"MINLOC",	I_1to3,	I|R|L,	type_INTEGER,	I_F90|I_ARRY|I_MIXED_ARGS,ii_minloc},

/*
13.11.20 Pointer association status functions
ASSOCIATED (POINTER [, TARGET])    Association status inquiry or comparison
NULL ([MOLD])                    5 Returns disassociated pointer
*/

{"ASSOCIATED",  I_1or2, ANY,	type_LOGICAL,	I_F90|I_INQ,NULL},
{"NULL",	I_0or1,	ANY,	type_GENERIC,	I_F95|I_INQ|I_EVAL|I_PTR|I_NULL,ii_null},

/*
13.12 Intrinsic subroutines
CPU_TIME (TIME)                  5 Obtain processor time
DATE_AND_TIME ([DATE, TIME,        Obtain date and time
  ZONE, VALUES])
MVBITS (FROM, FROMPOS,             Copies bits from one integer to another
  LEN, TO, TOPOS)
RANDOM_NUMBER (HARVEST)            Returns pseudorandom number
RANDOM_SEED ([SIZE, PUT, GET])     Initializes or restarts the
                                   pseudorandom number generator
SYSTEM_CLOCK ([COUNT,              Obtain data from the system clock
  COUNT_RATE, COUNT_MAX])
*/

{"CPU_TIME",	1,	R|D,	type_SUBROUTINE,I_F95,NULL},
{"DATE_AND_TIME",I_1to4,I|STR,	type_SUBROUTINE,I_F90|I_MIXED_ARGS,NULL},
{"MVBITS",	5,	I,	type_SUBROUTINE,I_F90|I_ELEM,NULL},
{"RANDOM_NUMBER",1,	R|D,	type_SUBROUTINE,I_F90,NULL},
{"RANDOM_SEED",	I_0or1,	I,	type_SUBROUTINE,I_F90,NULL},
{"SYSTEM_CLOCK",I_0or1,	I,	type_SUBROUTINE,I_F90,NULL},


				/* Nonstandard intrinsics */

/* Nonstandard double and quad precision intrinsics are given the
   I_ELEM flag where their standard counterparts are elementary (which
   is all of them).  Since these are nonstandard, the Fortran standard
   does not specify whether they are elementary or not.  Presumably if
   a Fortran 90 compiler implements them, it will make them elementary
   if they can be.
 */

		/* DOUBLE COMPLEX intrinsics are included regardless
		   of -intrinsics option, since they are essential
		   to support of this datatype.
		 */
{"DCMPLX",	I_1or2,	I|R|D|C|Z,type_DCOMPLEX,I_NONSTD|I_ELEM|I_NOTARG,NULL},
{"DCONJG",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"DIMAG",	1,	Z,	type_DP,	I_NONSTD|I_ELEM,NULL},
{"IMAG",	1,	C|Z,	type_GENERIC,	I_NONSTD|I_ELEM|I_NOTARG|I_C_TO_R,NULL},
{"DREAL",	1,	Z,	type_DP,	I_NONSTD|I_ELEM,NULL},
{"CDABS",	1,	Z,	type_DP,	I_NONSTD|I_ELEM,NULL},
{"ZABS",	1,	Z,	type_DP,	I_NONSTD|I_ELEM,NULL},
{"CDSQRT",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"ZSQRT",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"CDEXP",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"ZEXP",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"CDLOG",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"ZLOG",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"CDSIN",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"ZSIN",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"CDCOS",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},
{"ZCOS",	1,	Z,	type_DCOMPLEX,	I_NONSTD|I_ELEM,NULL},

		/* DFLOAT has been available in almost all Fortran
                   implementations for decades, but curiously, was
                   omitted from the Fortran 66 and Fortran 77
                   standards.  A separate intrinsic is essential,
                   because DBLE(FLOAT()) will lose bits for integer
                   arguments larger than the REAL fraction size.  If
                   we don't include it here, declaration file output
                   will incorrectly type it as REAL instead of DOUBLE
                   PRECISION. -- NHFB */

{"DFLOAT",	1,	I,	type_DP,	I_NONSTD|I_ELEM,NULL},

		/* Quad precision intrinsics are included regardless
		   of -intrinsics option, since they are essential
		   to support of this datatype.  (Actually most of
		   them are better handled by generics.)
		 */
{"IQINT",	1,	R,	type_INTEGER,	I_NONSTD|I_ELEM|I_NOTARG|I_QARG,NULL},
{"SNGLQ",	1,	R,	type_REAL,	I_NONSTD|I_ELEM|I_NOTARG|I_QARG,NULL},
{"QREAL",	1,	C,	type_QUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QARG|I_QUAD,NULL},
{"DBLEQ",	1,	R,	type_DP,	I_NONSTD|I_ELEM|I_NOTARG|I_QARG,NULL},
{"QFLOAT",	1,	I,	type_QUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QUAD,NULL},
{"QEXTD",	1,	D,	type_QUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QUAD,NULL},
{"QEXT",	1,	I|R|D,	type_QUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QUAD,NULL},
{"QCMPLX",	I_1or2,	I|R|D|C|Z,type_CQUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QUAD,NULL},
{"QINT",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QNINT",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"IQNINT",	1,	R,	type_INTEGER,	I_NONSTD|I_ELEM|I_QARG,NULL},
{"QABS",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"CQABS",	1,	C,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QMOD",	2,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QSIGN",	2,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QDIM",	2,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QPROD",	2,	D,	type_QUAD,	I_NONSTD|I_ELEM|I_QUAD,NULL},
{"QMAX1",	I_2up,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QARG|I_QUAD,NULL},
{"QMIN1",	I_2up,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_NOTARG|I_QARG|I_QUAD,NULL},
{"QIMAG",	1,	C,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QCONJG",	1,	C,	type_CQUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QSQRT",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"CQSQRT",	1,	C,	type_CQUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QEXP",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"CQEXP",	1,	C,	type_CQUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QLOG",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"CQLOG",	1,	C,	type_CQUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QLOG10",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QSIN",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"CQSIN",	1,	C,	type_CQUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QCOS",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"CQCOS",	1,	C,	type_CQUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QTAN",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QARSIN",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QARCOS",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QATAN",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QATAN2",	2,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QSINH",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QCOSH",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},
{"QTANH",	1,	R,	type_QUAD,	I_NONSTD|I_ELEM|I_QARG|I_QUAD,NULL},

#ifdef EXTRA_INTRINSICS

	/* Nonstandard but widely used intrinsics.  These follow both
	   VMS and AIX defns, so they are probably de facto standard.
	   Not included: specifics covered by a generic.
	   N.B. Argument checking is not tight for these: some
	   take arrays, 0 or 1 arguments, etc. that are not
	   handled by check_intrins_args().  Remarks are placed by
	   these cases.
	 */

		/* Address-of function */
{"LOC",		1,I|R|D|C|Z|L|STR,type_INTEGER,	I_NONSTD|I_EXTRA,NULL},

		/* Utility routines */
{"EXIT",       I_0or1,	I,	type_SUBROUTINE,I_NONSTD|I_EXTRA,NULL},
#endif

		/* Unix only.  These are a selected subset of the F77
		   library routines listed in the USENIX manual section 3F.
		 */
#ifdef UNIX_INTRINSICS
{"ABORT",	1,	STR,	type_SUBROUTINE,I_NONSTD|I_UNIX,NULL},
{"AND",		2,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
		/* I, then STR not enforced in GETARG. */
{"GETARG",	2,	I|STR,	type_SUBROUTINE,I_MIXED_ARGS|I_NONSTD|I_UNIX,NULL},
{"GETENV",	2,	STR,	type_SUBROUTINE,I_NONSTD|I_UNIX,NULL},
{"GMTIME",	2,	I,	type_SUBROUTINE,I_NONSTD|I_UNIX,NULL},/*2nd arg array(9)*/
#ifdef IARGC_NO_ARG
{"IARGC",	0,	0,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
#else
#ifdef IARGC_ONE_ARG
{"IARGC",	1,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
#else  /* default is to allow 0 or 1 */
{"IARGC",	I_0or1,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
#endif
#endif
{"LSHIFT",	2,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
{"LTIME",	2,	I,	type_SUBROUTINE,I_NONSTD|I_UNIX,NULL},/*2nd arg array(9)*/
{"OR",		2,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
#ifdef RAND_NO_ARG	/*RAND() form*/
{"IRAND",	0,	0,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
{"RAND",	0,	0,	type_REAL,	I_NONSTD|I_UNIX,NULL},
#else
#ifdef RAND_ONE_ARG	/*RAND(ISEED) form*/
{"IRAND",	1,	I,	type_INTEGER,	I_NONSTD|I_UNIX|I_NONPURE,NULL},
{"RAND",	1,	I,	type_REAL,	I_NONSTD|I_UNIX|I_NONPURE,NULL},
#else				/* Allow either form */
{"IRAND",	I_0or1,	I,	type_INTEGER,	I_NONSTD|I_UNIX|I_NONPURE,NULL},
{"RAND",	I_0or1,	I,	type_REAL,	I_NONSTD|I_UNIX|I_NONPURE,NULL},
#endif
#endif
{"RSHIFT",	2,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
{"SRAND",	1,	I|R,	type_SUBROUTINE,I_NONSTD|I_UNIX,NULL},/*AIX has this*/
{"SYSTEM",	1,	STR,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
{"TIME",	I_0or1,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
{"XOR",		2,	I,	type_INTEGER,	I_NONSTD|I_UNIX,NULL},
#endif

#ifdef VMS_INTRINSICS		/* VMS only */
{"DATE",	1,	STR,	type_SUBROUTINE,I_NONSTD|I_VMS,NULL},
{"ERRSNS",	5,	I,	type_SUBROUTINE,I_NONSTD|I_VMS,NULL},
{"IDATE",	3,	I,	type_SUBROUTINE,I_NONSTD|I_VMS,NULL},
{"RAN",		1,	I,	type_REAL,	I_NONSTD|I_VMS|I_NONPURE,NULL},
{"SECNDS",	1,	R,	type_REAL,	I_NONSTD|I_VMS,NULL},
{"SIZEOF",	1,	I|R|D|C|Z|L|STR,type_INTEGER,	I_NONSTD|I_VMS,NULL},
{"TIME",	1,	STR,	type_SUBROUTINE,I_NONSTD|I_VMS,NULL},
#endif

#undef I
#undef R
#undef D
#undef C
#undef Z
#undef L
#undef STR
#undef DT
#undef ANY
};

#define NUM_INTRINSICS (sizeof(intrinsic)/sizeof(intrinsic[0]))


			/* Definitions of routines start here */

PROTO( PRIVATE void set_intrinsic_numargs, ( const char *name, int choice ));
PROTO(PRIVATE unsigned long kwd_hash,( const char *s ));


#define EMPTY 255

PRIVATE unsigned char intrins_hashtab[INTRINS_HASHSZ];

/*    init_intrins_hashtab:
                 Initializes the intrinsic hash table by clearing it to EMPTY
                 and then hashes all the intrinsic names into the table.
*/

unsigned long
init_intrins_hashtab(VOID)
{
    unsigned i,h;
    unsigned long hnum;
    unsigned long numclashes=0;

    for(h=0;h<INTRINS_HASHSZ;h++) {
           intrins_hashtab[h] = EMPTY;
    }
    for(i=0; i < NUM_INTRINSICS; i++) {
	   hnum = kwd_hash(intrinsic[i].name);
	   while(h=hnum%INTRINS_HASHSZ, intrins_hashtab[h] != EMPTY) {
		hnum = rehash(hnum);
		numclashes++;
	   }
	   intrins_hashtab[h] = i;
    }
    return numclashes;
}


		/* Function called by do_preps to alter intrinsic table for
		   user-selected options respecting RAND and IARGC.
		 */

#ifndef STANDARD_INTRINSICS
void
set_intrinsic_options(VOID)
{
  int numargs;

				/* numargs = 0 if only no_arg is set, and
				   1 if only one_arg is set.  numargs =
				   I_0or1 if both are set.  If neither
				   is set (-intrinsic=none),
				   return to compile-time default. */


				/* Form a two-bit number */
  switch( (intrinsic_rand_one_argument<<1) | intrinsic_rand_no_argument ) {
    case 1:
      numargs = 0;		/* no_argument */
      break;
    case 2:
      numargs = 1;		/* one_argument */
      break;
    case 3:
      numargs = I_0or1;		/* both cases */
      break;
    default:
      numargs= (DEF_INTRINSIC_RAND & 1?
		    (DEF_INTRINSIC_RAND & 2? I_0or1: 0)
		    :(DEF_INTRINSIC_RAND & 2? 1:I_0or1));
      break;
  }

  set_intrinsic_numargs("RAND",numargs);
  set_intrinsic_numargs("IRAND",numargs);

  switch( (intrinsic_iargc_one_argument<<1) | intrinsic_iargc_no_argument ) {
    case 1:
      numargs = 0;		/* no_argument */
      break;
    case 2:
      numargs = 1;		/* one_argument */
      break;
    case 3:
      numargs = I_0or1;		/* both cases */
      break;
    default:
      numargs = (DEF_INTRINSIC_IARGC & 1?
		    (DEF_INTRINSIC_IARGC & 2? I_0or1: 0)
		    :(DEF_INTRINSIC_IARGC & 2? 1:I_0or1));
      break;
  }

  set_intrinsic_numargs("IARGC",numargs);
}

#endif /* not STANDARD_INTRINSICS */



PRIVATE
#if HAVE_STDC
void set_intrinsic_numargs(const char *name, int choice)
#else /* K&R style */
void set_intrinsic_numargs(name,choice)
     char *name;		/* Name of function to fix up */
     int choice;		/* 0 = none, 1 = one, I_0or1 = either */
#endif /* HAVE_STDC */
{
  IntrinsInfo *defn;

  defn = find_intrinsic(name);
  if(defn != (IntrinsInfo *)NULL) {
    defn->num_args = choice;
  }
}


	/* Function to look up an intrinsic function name in table.
	   If found, returns ptr to table entry, otherwise NULL.
	*/
IntrinsInfo *
#if HAVE_STDC
find_intrinsic(const char *s)
	        			/* given name */
#else /* K&R style */
find_intrinsic(s)
	char *s;			/* given name */
#endif /* HAVE_STDC */
{
	unsigned i, h;
	unsigned long hnum;

	hnum = kwd_hash(s);
	for(;;) {
	  h=hnum%INTRINS_HASHSZ;
	  if( (i=intrins_hashtab[h]) == EMPTY )
	    break;		/* Not found */

				/* Something found: see if a match */
	  if( strcmp(s,intrinsic[i].name) == 0
#ifndef STANDARD_INTRINSICS
	      &&
	      ((intrinsic[i].intrins_flags&(I_EXTRA|I_VMS|I_UNIX))==0 ||
	       ((intrinsic[i].intrins_flags&I_EXTRA) && intrinsic_set_extra) ||
	       ((intrinsic[i].intrins_flags&I_UNIX) && intrinsic_set_unix) ||
	       ((intrinsic[i].intrins_flags&I_VMS) && intrinsic_set_vms)
	      )
#endif
	      ) {

	    return &intrinsic[i];
	  }
	  else {		/* No match: try next */
	    hnum = rehash(hnum);
	  }
	}
				/* Not an intrinsic function */
	return (IntrinsInfo *)NULL;
}

	/* kwd_hash: Same as hash() but always uses full length of keyword.
	   To keep the keyword table clash-free on any machine,
	   packs only 4 bytes per word even if long is bigger */

PRIVATE unsigned long
#if HAVE_STDC
kwd_hash(const char *s)
#else /* K&R style */
kwd_hash(s)
    char *s;
#endif /* HAVE_STDC */
{
    unsigned long sum = 0, wd;
    int i = 0,j;

    int n = strlen(s);

    while (i < n) {
         wd = 0;
         for(j=1; j <= 4 && i < n; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (4 - j) * 8;}

	sum ^= wd;}
    return sum;
}


/* Intrinsic function evaluation routines.  Traditionally these were
   run only if arguments are EVALUATED_EXPRs or function is an inquiry
   function.  Now in order to do proper array shape checking in exprs,
   those with I_ARRY flag are run in any case, to produce result of
   new shape.  They need not produce a meaningful result value.

   Note that for inquiry functions or functions requiring more than
   one argument it is possible (on erroneous code) to have too few or
   no arguments, so those handlers need to check.  Argument type may
   also be incorrect.  (Checking of argument number and type is done
   elsewhere and does not prevent invoking the routine.)

 */

/* List to match dummy args by name to tokens holding args */
typedef struct {
  const char* keyword;		/* name of dummy arg */
  Token **arg;			/* arg to match */
  int optional:1;		/* OPTIONAL flag */
} KeywordArgList;


PROTO(PRIVATE int match_keyword,(Token *args, KeywordArgList key_arg_list[], int n));

int match_keyword(Token *args, KeywordArgList key_arg_list[], int n)
{
  Token *t = args->next_token;
  int i, k;				/* arg number */

  for(k=0; k<n; k++) 			/* initialize results to NULL */
    *(key_arg_list[k].arg) = (Token *)NULL;

					/* Go thru tokens and match
					 * with keywords */
  k=0;
  while(k<n && t != NULL) {
    if( keyword_present(t) ) {
      for(i=0; i<n; i++) {		/* for legal code could start i=k */
	if( keyword_name_match(t,key_arg_list[i].keyword) ) {
	  *(key_arg_list[i].arg) = t;
	  break;
	}
      }
    }
    else {				/* no keyword: match token to position */
      *(key_arg_list[k].arg) = t;
    }
    t = t->next_token;
    k++;
  }
					/* verify all non-optional args matched */
  for(k=0; k<n; k++) {
    if( *(key_arg_list[k].arg) == (Token *)NULL &&
	!key_arg_list[k].optional )
      return FALSE;
  }
  return TRUE;
}


PRIVATE int
#if HAVE_STDC
ii_abs(Token *args)			/* ABS(A) */
#else /* K&R style */
ii_abs(args)
     Token *args;
#endif /* HAVE_STDC */
{
  Token *a;
  int val, result=0;

  if( (a = args->next_token) != NULL ) {

    if(a->TOK_type != type_INTEGER) {/* wrong arg type: message given elsewhere */
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      val = int_expr_value(a);
      result = (val >= 0? val: -val);
    }
  }

  return result;
}

PRIVATE int
ii_sign(Token *args)			/* SIGN(A=magnitude,B=sign) */
{
  Token *a,*b;
  int magnitude,sign, result=0;
  KeywordArgList key_arg_list[] = {
    {"A",	&a,	0},
    {"B",	&b,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if(a->TOK_type != type_INTEGER || b->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      magnitude = int_expr_value(a);
      if(magnitude < 0) magnitude = -magnitude;
      sign = int_expr_value(b);
      result = (sign >= 0? magnitude: -magnitude);
    }
  }

  return result;
}

PRIVATE int
ii_dim(Token *args)			/* DIM(X=int,Y=int) */
{
  Token *x,*y;
  int val, result=0;
  KeywordArgList key_arg_list[] = {
    {"X",	&x,	0},
    {"Y",	&y,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if(x->TOK_type != type_INTEGER || y->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      val = int_expr_value(x)-int_expr_value(y);
      result = (val >= 0? val: 0);
    }
  }

  return result;
}

PRIVATE int
ii_mod(Token *args)			/* MOD(A=int,P=int) */
{
  Token *a,*p;
  int val1,val2,quotient, result=0;
  KeywordArgList key_arg_list[] = {
    {"A",	&a,	0},
    {"P",	&p,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if(a->TOK_type != type_INTEGER || p->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      val1 = int_expr_value(a);
      val2 = int_expr_value(p);
      if(val2 != 0) {
	if((val1 < 0) == (val2 < 0)) {
	  quotient = val1/val2;	/* Both positive or both negative*/
	}
	else {
	  quotient = -(-val1/val2);	/* Unlike signs */
	}
	result = val1 - quotient*val2;
      }
    }  
  }

  return result;
}

PRIVATE int
ii_modulo(Token *args)			/* MODULO(A,P) */
{
  Token *a,*p;
  int val1,val2,quotient, result=0;
  KeywordArgList key_arg_list[] = {
    {"A",	&a,	0},
    {"P",	&p,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if(a->TOK_type != type_INTEGER || p->TOK_type != type_INTEGER) {/* don't evaluate with real args */
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      val1 = int_expr_value(a);
      val2 = int_expr_value(p);
      if(val2 != 0) {
	if((val1 < 0) == (val2 < 0)) {
	  quotient = val1/val2;	/* Both positive or both negative*/
	}
	else {
	  quotient = -((-val1/val2)+1);	/* Unlike signs */
	}
	result = val1 - quotient*val2;
      }
    }
  }

  return result;
}


PRIVATE int
#if HAVE_STDC
ii_max(Token *args)			/* MAX(int,int,...) */
#else /* K&R style */
ii_max(args)			/* MAX(int,int,...) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args;
  int val,result=0,n=0;
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,"\nEvaluating MAX(");
#endif
  while( (t=t->next_token) != NULL) {

      if(t->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
	make_false(EVALUATED_EXPR,args->TOK_flags);
	break;
      }
      else {
	val = int_expr_value(t);
	if(n++ == 0 || val > result)
	  result = val;
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,"%d ",val);
#endif
      }
  }
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,") = %d",result);
#endif
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_min(Token *args)			/* MIN(int,int,...) */
#else /* K&R style */
ii_min(args)			/* MIN(int,int,...) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args;
  int val,result=0,n=0;
  while( (t=t->next_token) != NULL) {
      if(t->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
	make_false(EVALUATED_EXPR,args->TOK_flags);
	break;
      }
      else {
	val = int_expr_value(t);
	if(n++ == 0 || val < result)
	  result = val;
      }
  }
  return result;
}

PRIVATE int
ii_ichar(Token *args)		/* ICHAR(C=string) */
{
  Token *c = args->next_token;
  if( c != NULL ) {
    if(c->TOK_type != type_STRING || !is_true(LIT_CONST,c->TOK_flags)) {
      make_false(PARAMETER_EXPR,args->TOK_flags);
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      return c->value.string[0];	/* Processor collating sequence is used */
    }
  }

  return 0;
}

PRIVATE int
ii_len(Token *args)		/* LEN(STRING) */
{
  Token *string=args->next_token;
  int val,result=0;

    /* Set the PARAMETER_EXPR flag since LEN of string does
       not require contents to be known */
    if( string != NULL && string->TOK_type == type_STRING && (val = string->size) > 0 ) {
      make_true(PARAMETER_EXPR,args->TOK_flags);
      make_true(EVALUATED_EXPR,args->TOK_flags);
      result = val;
    }
    else {			/* nonstring or adjustable or unknown */
      make_false(PARAMETER_EXPR,args->TOK_flags);
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }

  return result;
}

PRIVATE int
ii_index(Token *args)		/* INDEX(STRING,SUBSTRING,[,BACK]) */
{
  Token *string,*substring,*back;
  KeywordArgList key_arg_list[] = {
    {"STRING",		&string,	0},
    {"SUBSTRING",	&substring,	0},
    {"BACK",		&back,		1},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if(string->TOK_type != type_STRING
	|| substring->TOK_type != type_STRING
	|| !is_true(LIT_CONST,string->TOK_flags) || !is_true(LIT_CONST,substring->TOK_flags)) {
      make_false(EVALUATED_EXPR,args->TOK_flags);
    }
    else {
      int i;
      char *s1=string->value.string;
      char *s2=substring->value.string;
      int n1=strlen(s1), n2=strlen(s2);

      for(i=1; n1 > 0 && n1 >= n2; i++,s1++,n1--) {
	if(strncmp(s1,s2,n2) == 0)
	  return i;
      }
    }
  }

  return 0;
}

PRIVATE int
ii_null(Token *args)		/* NULL( [mold] ) */
{
  /* Type of result is handled by type_GENERIC.  Here we set
     token flags to be copied to result.
   */
  make_true(PARAMETER_EXPR,args->TOK_flags);

  /* ALLOCATED_EXPR & ASSOCIATED_EXPR do not need to be unset since
   * all token flags are cleared by default.
   */

  /* Override default copying of EVALUATED_EXPR from args to result */
  make_false(EVALUATED_EXPR,args->TOK_flags);
  return 0;
}

PRIVATE int
ii_reshape(Token *args)  /* RESHAPE ( SOURCE, SHAPE [,PAD] [,ORDER] ) */

{
  Token *source, *shape, *pad, *order;
  int result = 0;
  KeywordArgList key_arg_list[] = {
    {"SOURCE",	&source,	0},
    {"SHAPE",	&shape,		0},
    {"PAD",	&pad,		1},
    {"ORDER",	&order,		1},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    /* Since array-valued results are not yet supported, we ignore PAD
       and ORDER */
    /* Both arguments must be arrays. Error message NOT given
       elsewhere -- it should be. */
    if( is_true(ARRAY_EXPR,source->TOK_flags) &&
	is_true(ARRAY_EXPR,shape->TOK_flags) ) {
      if( array_dims(shape->array_dim) != 1 ) {
	syntax_error(shape->line_num, shape->col_num,
	    "shape must be 1-dimensional array");
      }
      else {
	make_true(ARRAY_EXPR,args->TOK_flags);
	/* When storing of array valued parameters is supported, we
	   should copy shape to array_dim.  For now, just record new
	   number of dimensions, and total size same as before.
	   */
	args->array_dim = array_dim_info(array_size(shape->array_dim), /* new dims */
	    array_size(source->array_dim)); /* new size */
	result = 0; /* if both source & shape are int constants this should be array-valued result */
      }
    }
  }

  return result;
}

PRIVATE int
ii_dot_product(Token *args)  /* DOT_PRODUCT (VECTOR_A, VECTOR_B) */
{
  int result = 0;
  Token *a, *b;
  KeywordArgList key_arg_list[] = {
    {"VECTOR_A",	&a,	0},
    {"VECTOR_B",	&b,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    /* check if a is vector of rank 1 */
    if (!is_true(ARRAY_EXPR, a->TOK_flags) ||
	array_dims(a->array_dim) != 1) {
      syntax_error(a->line_num, a->col_num, "must be 1-dimensional array");
    }
    /* check if b is vector of rank 1 */
    else if (!is_true(ARRAY_EXPR, b->TOK_flags) ||
	array_dims(b->array_dim) != 1) {
      syntax_error(b->line_num, b->col_num, "must be 1-dimensional array");
    }
    /* check if b has same size as a */
    else if (array_size(b->array_dim) != array_size(a->array_dim)) {
      syntax_error(b->line_num, b->col_num, "array size mismatch");
    }
    else {
      /* clear dimension info of scalar result */
      args->array_dim = array_dim_info(0,0);
    }
  }

  /* to prevent cascading of errors */
  make_false(ARRAY_EXPR,args->TOK_flags);

  return result;
}

PRIVATE int
ii_matmul(Token *args)  /* MATMUL (MATRIX_A, MATRIX_B) */
{
  int result = 0;
  Token *a, *b;
  KeywordArgList key_arg_list[] = {
    {"MATRIX_A",	&a,	0},
    {"MATRIX_B",	&b,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    /* check if a is matrix of rank 1 or 2 */
    if( !is_true(ARRAY_EXPR, a->TOK_flags) ||
	( array_dims(a->array_dim) != 1 && 
	  array_dims(a->array_dim) != 2 ) ) {
      syntax_error(a->line_num, a->col_num, 
	  "must be 1 or 2 dimensional array");
    }
    /* check if b is matrix of rank 1 or 2 */
    else if( !is_true(ARRAY_EXPR, b->TOK_flags) ||
	( array_dims(b->array_dim) != 1 && 
	  array_dims(b->array_dim) != 2 ) ) {
      syntax_error(b->line_num, b->col_num, 
	  "must be 1 or 2 dimensional array");
    }
    /* array multiplication rank constraint is not implemented
     * properly because we don't store length in each dimension */
    else if( (array_dims(a->array_dim) == 1 && 
	  array_size(a->array_dim) != array_dims(b->array_dim)) ||
	(array_dims(a->array_dim) == 2 &&
	 (array_dims(b->array_dim) == 1 &&
	  array_size(b->array_dim) != 2) ) 
	) {
      char *a_dims = ulongtostr(array_dims(a->array_dim));
      char *b_dims = ulongtostr(array_dims(b->array_dim));
      syntax_error(a->line_num, a->col_num, 
	  "array rank mismatch:");
      msg_tail(a_dims);
      msg_tail("and");
      msg_tail(b_dims);
    }
    else {
      int result_dim = 
	( array_dims(a->array_dim) == array_dims(b->array_dim) ) ?
	2 : 1;
      args->array_dim = array_dim_info_unk_size(result_dim);
      make_true(ARRAY_EXPR,args->TOK_flags);
    }
  }

  return result;
}


PRIVATE int
ii_merge( Token *args )  /* MERGE (TSOURCE, FSOURCE, MASK) */

{
  Token *tsource, *fsource, *mask;
  KeywordArgList key_arg_list[] = {
    {"TSOURCE",	&tsource,	0},
    {"FSOURCE",	&fsource,	0},
    {"MASK",	&mask,		0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    /* Result array properties are those of TSOURCE */
    copy_flag(ARRAY_EXPR,args->TOK_flags,tsource->TOK_flags);
    args->array_dim = tsource->array_dim;
  }

  return 0;
}

PRIVATE int
ii_pack( Token *args )		/* PACK (ARRAY, MASK [, VECTOR]) */
{
  /* FIXME: IMPLEMENTATION IS INCOMPLETE: does not try to determine if a
     known shape results, which it will be in some cases.  Only dims
     of result (always 1) will be correct. */
  make_true(ARRAY_EXPR,args->TOK_flags);
  args->array_dim = array_dim_info_unk_size(1);
  return 0;
}

PRIVATE int
ii_spread( Token *args )	/* SPREAD (SOURCE, DIM, NCOPIES) */
{
  Token *source, *dim, *ncopies;	/* dim, ncopies not used yet */
  KeywordArgList key_arg_list[] = {
    {"SOURCE",	&source,	0},
    {"DIM",	&dim,		0},
    {"NCOPIES",	&ncopies,	0},
  };
  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {
  /* FIXME: IMPLEMENTATION IS INCOMPLETE: does not try to determine if a
     known shape results, which it will be in some cases.  Only dims
     of result (1+dims of SOURCE) will be correct. */
    make_true(ARRAY_EXPR,args->TOK_flags);
    args->array_dim = array_dim_info_unk_size(array_dims(source->array_dim)+1);
  }
  return 0;
}

PRIVATE int
ii_unpack( Token *args )	/* UNPACK (VECTOR, MASK, FIELD) */
{
  Token *vector, *mask, *field;
  KeywordArgList key_arg_list[] = {
    {"VECTOR",	&vector,	0},
    {"MASK",	&mask,		0},
    {"FIELD",	&field,		0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if(is_true(ARRAY_EXPR,mask->TOK_flags)) { /* should check all 3 */
      make_true(ARRAY_EXPR,args->TOK_flags);
      args->array_dim = mask->array_dim;
    }
  }

  return 0;
}

PRIVATE int
ii_shape(Token *args)		/* SHAPE( SOURCE ) */
{
  Token *source;
  int result=size_UNKNOWN;
  KeywordArgList key_arg_list[] = {
    {"SOURCE",	&source,	0},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    /* When storing of constant array values is implemented, these flags
       will be meaningful.  For now, they are bogus.
       */
    make_true(PARAMETER_EXPR,args->TOK_flags);
    make_true(EVALUATED_EXPR,args->TOK_flags);

    /* Result is a rank-one array of size equal to number of
       dimensions of argument.  Scalar arg OK, gives dims 1 size 0 */
    make_true(ARRAY_EXPR,args->TOK_flags);
    args->array_dim = array_dim_info(1,array_dims(source->array_dim));
    /* FIXME: Here we should assign an array value equal to sizes
       of the dimensions of argument if known. */
    result = 0;
  }

  return result;
}

PROTO(PRIVATE int ii_lubound,(Token *args, int lower));

/* Routine to return lower or upper bound
   array of argument, depending on value of lower=TRUE, FALSE rsptly.
FIXME: Since array shapes are not stored, there is no difference at present.
 */
PRIVATE int
ii_lubound(Token *args, int lower)
{
  Token *array, *dim;
  int result=0;
  KeywordArgList key_arg_list[] = {
    {"ARRAY",	&array,	0},
    {"DIM",	&dim,	1},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {
    if( is_true(ARRAY_EXPR,args->TOK_flags) ) { /* arg must be array */
    /* When storing of constant array values is implemented, these flags
       will be meaningful.  For now, they are bogus.
     */
      make_true(PARAMETER_EXPR,args->TOK_flags);
      make_true(EVALUATED_EXPR,args->TOK_flags);

      if( dim != NULL ) {
	/* If DIM given, result is scalar. */
	/* Here we should look up the size of dimension dim */
	args->array_dim = array_dim_info(0,0);
	result = 0;
      }
      else {
    /* Result is a rank-one array of size equal to number of
       dimensions of argument. */
	make_true(ARRAY_EXPR,args->TOK_flags);
	args->array_dim = array_dim_info(1,array_dims(array->array_dim));
		/* Here we should assign an array value equal to lower bounds
		   of the dimensions of argument if known. */
	result = lower?0:0;	/* gratuitous use of lower so not unused */
      }
    }
  }

  return result;
}

PRIVATE int
ii_lbound(Token *args)		/* LBOUND( array [,dim] ) */
{
  return ii_lubound(args, TRUE);
}

PRIVATE int
ii_ubound(Token *args)		/* UBOUND( array [,dim] ) */
{
  return ii_lubound(args, FALSE);
}

/* Handler for minloc and maxloc.  FIXME: Result will be same for both until
   some day array-valued constants are stored, then sometimes it may
   be evaluated.
 */
PROTO(PRIVATE int ii_minmaxloc,(Token *args, int min));

PRIVATE int
ii_minmaxloc(Token *args, int min)
{
  Token *array, *dim, *mask = (Token *)NULL;
  int result=0;
  KeywordArgList key_arg_list[] = {
    {"ARRAY",	&array,	0},
    {"DIM",	&dim,	1},
    {"MASK",	&mask,	1},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {
  /* if mask is NULL, but dim is not, then it could be MAXLOC(ARRAY,MASK).
   * Disambiguate by data type. */
 
    if( mask == NULL && dim != NULL ) {
      if( datatype_of(dim->TOK_type) == type_LOGICAL ) { /* it is MASK */
	mask = dim;
	dim = NULL;
      }
      if( dim != NULL ) {	/* we really do have DIM */
	/* Result is of rank 1 less than array and shape of array with
	   dimension dim omitted.  For now we just set size unknown. */
	int dims = array_dims(array->array_dim);
	if( dims > 1 ) {
	  make_true(ARRAY_EXPR,args->TOK_flags);
	  args->array_dim = array_dim_info_unk_size(dims-1);
	}
	else {
	  args->array_dim = array_dim_info(0,0); /* reduced to scalar */
	}
      }
      else {			/* dim absent: rank 1, size=rank of array */
	make_true(ARRAY_EXPR,args->TOK_flags);
	args->array_dim = array_dim_info(1,array_size(array->array_dim));
      }
      result = (min?0:0);	/* use min, unused for now */
    }
  }
  return result;
}

PRIVATE int
ii_minloc(Token *args)		/* MINLOC (ARRAY [, DIM] [, MASK]) */
{
  return ii_minmaxloc(args,TRUE);
}

PRIVATE int
ii_maxloc(Token *args)		/* MAXLOC (ARRAY [, DIM] [, MASK]) */
{
  return ii_minmaxloc(args,FALSE);
}


PRIVATE int
ii_size(Token *args)		/* SIZE( array [,dim] ) */
{
  Token *array, *dim;
  array_dim_t array_dim;
  int result=size_UNKNOWN;
  KeywordArgList key_arg_list[] = {
    {"ARRAY",	&array,	0},
    {"DIM",	&dim,	1},
  };

  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {

    if( is_true(ARRAY_EXPR,args->TOK_flags) ) {
      /* Here if dim is provided, we should access shape of array to
	 get size of the the given dimension.  FIXME: Since shape is not
	 stored, do the right thing only if dim absent, otherwise set
	 result size to unknown.
       */
      array_dim = array->array_dim;
      if( array_size_is_unknown(array_dim) || dim != NULL ) {
	make_false(PARAMETER_EXPR,args->TOK_flags);
	make_false(EVALUATED_EXPR,args->TOK_flags);
	result = size_UNKNOWN;
      }
      else {
	make_true(PARAMETER_EXPR,args->TOK_flags);
	make_true(EVALUATED_EXPR,args->TOK_flags);
	result = array_size(array_dim);
      }
    }
    else {
      syntax_error(array->line_num,array->col_num,
		   "array-valued argument expected");
    }
  }

  return result;
}

/* Kind functions */
PRIVATE int
ii_kind( Token *args )			/* KIND( X ) */
{
  make_true(PARAMETER_EXPR,args->TOK_flags);
  make_true(EVALUATED_EXPR,args->TOK_flags);
  if( args->next_token != NULL ) {
    return args->next_token->kind;
  }
  else {
    return 0;
  }
}

PRIVATE int
ii_selected_int_kind( Token *args )	/* SELECTED_INT_KIND( R ) */
{
  Token *range = args->next_token;
  int kind = kind_DEFAULT_INTEGER;

  if(range != NULL && range->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    kind = selected_int_kind( int_expr_value(range) );
    make_true(PARAMETER_EXPR,args->TOK_flags);
    make_true(EVALUATED_EXPR,args->TOK_flags);
  }

  return kind;
}

/* This needs to be rewritten when call-by-name is supported, to
   distinguish one-arg cases of P or R. */
PRIVATE int
ii_selected_real_kind( Token *args )	/* SELECTED_REAL_KIND([P], [R]) */
{
  Token *precision, *range;
  int kind = kind_DEFAULT_REAL;
  KeywordArgList key_arg_list[] = {
    {"P",	&precision,	1},
    {"R",	&range,		1},
  };
  if( match_keyword(args,key_arg_list,NUM_LIST_ITEMS(key_arg_list)) ) {
    if( (precision != NULL && datatype_of(precision->TOK_type) != type_INTEGER) ||
	(range != NULL && datatype_of(range->TOK_type) != type_INTEGER) ||
	(precision == NULL && range == NULL) ) {
	make_false(EVALUATED_EXPR,args->TOK_flags);/* invalid args: message given elswr */
    }
    else {				/* all valid: proceed */
      if( precision != NULL ) {
	if( range != NULL ) {		/* (P,R) form */
	  kind = selected_real_kind_p_r(int_expr_value(precision),int_expr_value(range));
	}
	else {				/* (P) form */
	  kind = selected_real_kind_p( int_expr_value(precision) );
	}
      }
      else {				/* (R) form */ 
	kind = selected_real_kind_r(int_expr_value(range));
      }
      make_true(PARAMETER_EXPR,args->TOK_flags);
      make_true(EVALUATED_EXPR,args->TOK_flags);
    }
  }

  return kind;
}


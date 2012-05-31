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
PROTO(PRIVATE int ii_ichar,( Token *args ));
PROTO(PRIVATE int ii_index,( Token *args ));
PROTO(PRIVATE int ii_len,( Token *args ));
PROTO(PRIVATE int ii_max,( Token *args ));
PROTO(PRIVATE int ii_min,( Token *args ));
PROTO(PRIVATE int ii_mod,( Token *args ));
PROTO(PRIVATE int ii_null,( Token *args ));
PROTO(PRIVATE int ii_sign,( Token *args ));
PROTO(PRIVATE int ii_size,( Token *args ));


PRIVATE IntrinsInfo intrinsic[]={


	/* Table contains: name, num_args, arg_type, result_type, flags.
	   Special num_args values are defined in symtab.h.

	   Flags: I_F77 if it is in Table 5 p. 15-24, I_NONF77 otherwise
		  I_NONF90 if it is not in Chap 13 of F90 standard
		  I_NONSTD = I_NONF77|I_NONF90 for convenience
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
		  I_ELEM function is elemental (currently flag not used)
		  I_XFRM function is transformational (currently not used)
	 */

#define I_NONSTD (I_NONF77|I_NONF90)

				/* Fortran 77 intrinsics */
{"INT", 	1,	I|R|D|C|Z,type_INTEGER,	I_F77|I_NOTARG,NULL},
{"IFIX",	1,	R,	type_INTEGER,	I_F77|I_NOTARG,NULL},
{"IDINT",	1,	D,	type_INTEGER,	I_F77|I_NOTARG,NULL},
{"REAL",	1,	I|R|D|C|Z,type_GENERIC, I_F77|I_NOTARG|I_C_TO_R|I_SP_R,NULL},
{"FLOAT",	1,	I,	type_REAL,	I_F77|I_NOTARG,NULL},
{"SNGL",	1,	D,	type_REAL,	I_F77|I_NOTARG,NULL},
{"DBLE",	1,	I|R|D|C|Z,type_DP,	I_F77|I_NOTARG,NULL},
{"CMPLX",	I_1or2,	I|R|D|C|Z,type_COMPLEX,	I_F77|I_NOTARG,NULL},
{"ICHAR",	1,	STR,	type_INTEGER,	I_F77|I_NOTARG,ii_ichar},
{"CHAR",	1,	I,	type_STRING,	I_F77|I_NOTARG|I_CHAR,NULL},
{"AINT",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DINT",	1,	D,	type_DP,	I_F77,NULL},
{"ANINT",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DNINT",	1,	D,	type_DP,	I_F77,NULL},
{"NINT",	1,	R|D,	type_INTEGER,	I_F77,NULL},
{"IDNINT",	1,	D,	type_INTEGER,	I_F77,NULL},
{"ABS", 	1,	I|R|D|C|Z,type_GENERIC,	I_F77|I_C_TO_R,ii_abs},
{"IABS",	1,	I,	type_INTEGER,	I_F77,ii_abs},
{"DABS",	1,	D,	type_DP,	I_F77,NULL},
{"CABS",	1,	C,	type_REAL,	I_F77,NULL},
{"MOD", 	2,	I|R|D,	type_GENERIC,	I_F77,ii_mod},
{"AMOD",	2,	R,	type_REAL,	I_F77,NULL},
{"DMOD",	2,	D,	type_DP,	I_F77,NULL},
{"SIGN",	2,	I|R|D,	type_GENERIC,	I_F77,ii_sign},
{"ISIGN",	2,	I,	type_INTEGER,	I_F77,ii_sign},
{"DSIGN",	2,	D,	type_DP,	I_F77,NULL},
{"DIM",		2,	I|R|D,	type_GENERIC,	I_F77,ii_dim},
{"IDIM",	2,	I,	type_INTEGER,	I_F77,ii_dim},
{"DDIM",	2,	D,	type_DP,	I_F77,NULL},
{"DPROD",	2,	R,	type_DP,	I_F77,NULL},
{"MAX",		I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG,ii_max},
{"MAX0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG,ii_max},
{"AMAX1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG,NULL},
{"DMAX1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG,NULL},
{"AMAX0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG,NULL},
{"MAX1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG,NULL},
{"MIN", 	I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG,ii_min},
{"MIN0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG,ii_min},
{"AMIN1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG,NULL},
{"DMIN1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG,NULL},
{"AMIN0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG,NULL},
{"MIN1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG,NULL},
{"LEN", 	1,	STR,	type_INTEGER,	I_F77|I_INQ,ii_len},
{"INDEX",	2,	STR,	type_INTEGER,	I_F77,ii_index},
{"AIMAG",	1,	C,	type_REAL,	I_F77,NULL},
{"CONJG",	1,	C,	type_COMPLEX,	I_F77,NULL},
{"SQRT",	1,	R|D|C|Z,type_GENERIC,	I_F77,NULL},
{"DSQRT",	1,	D,	type_DP,	I_F77,NULL},
{"CSQRT",	1,	C,	type_COMPLEX,	I_F77,NULL},
{"EXP",		1,	R|D|C|Z,type_GENERIC,	I_F77,NULL},
{"DEXP",	1,	D,	type_DP,	I_F77,NULL},
{"CEXP",	1,	C,	type_COMPLEX,	I_F77,NULL},
{"LOG", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_NOTARG,NULL},
{"ALOG",	1,	R,	type_REAL,	I_F77,NULL},
{"DLOG",	1,	D,	type_DP,	I_F77,NULL},
{"CLOG",	1,	C,	type_COMPLEX,	I_F77,NULL},
{"LOG10",	1,	R|D,	type_GENERIC,	I_F77|I_NOTARG,NULL},
{"ALOG10",	1,	R,	type_REAL,	I_F77,NULL},
{"DLOG10",	1,	D,	type_DP,	I_F77,NULL},
{"SIN", 	1,	R|D|C|Z,type_GENERIC,	I_F77,NULL},
{"DSIN",	1,	D,	type_DP,	I_F77,NULL},
{"CSIN",	1,	C,	type_COMPLEX,	I_F77,NULL},
{"COS", 	1,	R|D|C|Z,type_GENERIC,	I_F77,NULL},
{"DCOS",	1,	D,	type_DP,	I_F77,NULL},
{"CCOS",	1,	C,	type_COMPLEX,	I_F77,NULL},
{"TAN", 	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DTAN",	1,	D,	type_DP,	I_F77,NULL},
{"ASIN",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DASIN",	1,	D,	type_DP,	I_F77,NULL},
{"ACOS",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DACOS",	1,	D,	type_DP,	I_F77,NULL},
{"ATAN",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DATAN",	1,	D,	type_DP,	I_F77,NULL},
{"ATAN2",	2,	R|D,	type_GENERIC,	I_F77,NULL},
{"DATAN2",	2,	D,	type_DP,	I_F77,NULL},
{"SINH",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DSINH",	1,	D,	type_DP,	I_F77,NULL},
{"COSH",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DCOSH",	1,	D,	type_DP,	I_F77,NULL},
{"TANH",	1,	R|D,	type_GENERIC,	I_F77,NULL},
{"DTANH",	1,	D,	type_DP,	I_F77,NULL},
{"LGE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG,NULL},
{"LGT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG,NULL},
{"LLE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG,NULL},
{"LLT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG,NULL},

				/* Intrinsics introduced in Fortran 90 */
		/* Array and pointer inquiry functions */
{"ALLOCATED",   1,      ANY,	type_LOGICAL,	I_NONF77|I_INQ,NULL},
{"ASSOCIATED",  I_1or2, ANY,	type_LOGICAL,	I_NONF77|I_INQ,NULL},
{"SIZE",	I_0or1, ANY,	type_INTEGER,	I_NONF77|I_INQ,ii_size},

		/* Array reduction functions*/
	/* Note: ordering of array,mask,dim args not enforced.  For type_GENERIC
	   result will be type of first argument.
	 */
{"ALL",		I_1or2,	I|L,	type_LOGICAL,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},
{"ANY",		I_1or2,	I|L,	type_LOGICAL,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},
{"COUNT",	I_1or2,	I|L,	type_INTEGER,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},
{"MAXVAL",	I_1to3,	ANY,	type_GENERIC,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},
{"MINVAL",	I_1to3,	ANY,	type_GENERIC,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},
{"PRODUCT",	I_1to3,	ANY,	type_GENERIC,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},
{"SUM",		I_1to3,	ANY,	type_GENERIC,	I_MIXED_ARGS|I_NONF77|I_XFRM,NULL},


{"NULL",	I_0or1,	ANY,	type_GENERIC,	I_NONF77|I_INQ|I_EVAL|I_PTR|I_NULL,ii_null},
{"RANDOM_NUMBER",1,	R,	type_SUBROUTINE,I_NONF77},
{"RANDOM_SEED",	I_0or1,	I,	type_SUBROUTINE,I_NONF77},

		/* Bit test & Shift operations */
{"BTEST",	2,	I,	type_LOGICAL,	I_NONF77|I_ELEM,NULL},
{"IAND",	2,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"IBCLR",	2,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"IBITS",	3,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"IBSET",	2,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"IEOR",	2,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"IOR",		2,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"ISHFT",	2,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"ISHFTC",	3,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"NOT",		1,	I,	type_INTEGER,	I_NONF77|I_ELEM,NULL},
{"MVBITS",	5,	I,	type_SUBROUTINE,I_NONF77|I_ELEM,NULL},

				/* Nonstandard intrinsics */

		/* DOUBLE COMPLEX intrinsics are included regardless
		   of -intrinsics option, since they are essential
		   to support of this datatype.
		 */
{"DCMPLX",	I_1or2,	I|R|D|C|Z,type_DCOMPLEX,I_NONSTD|I_NOTARG,NULL},
{"DCONJG",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"DIMAG",	1,	Z,	type_DP,	I_NONSTD,NULL},
{"IMAG",	1,	C|Z,	type_GENERIC,	I_NONSTD|I_NOTARG|I_C_TO_R,NULL},
{"DREAL",	1,	Z,	type_DP,	I_NONSTD,NULL},
{"CDABS",	1,	Z,	type_DP,	I_NONSTD,NULL},
{"ZABS",	1,	Z,	type_DP,	I_NONSTD,NULL},
{"CDSQRT",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"ZSQRT",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"CDEXP",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"ZEXP",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"CDLOG",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"ZLOG",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"CDSIN",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"ZSIN",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"CDCOS",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},
{"ZCOS",	1,	Z,	type_DCOMPLEX,	I_NONSTD,NULL},

		/* DFLOAT has been available in almost all Fortran
                   implementations for decades, but curiously, was
                   omitted from the Fortran 66 and Fortran 77
                   standards.  A separate intrinsic is essential,
                   because DBLE(FLOAT()) will lose bits for integer
                   arguments larger than the REAL fraction size.  If
                   we don't include it here, declaration file output
                   will incorrectly type it as REAL instead of DOUBLE
                   PRECISION. -- NHFB */

{"DFLOAT",	1,	I,	type_DP,	I_NONSTD,NULL},

		/* Quad precision intrinsics are included regardless
		   of -intrinsics option, since they are essential
		   to support of this datatype.  (Actually most of
		   them are better handled by generics.)
		 */
{"IQINT",	1,	R,	type_INTEGER,	I_NONSTD|I_NOTARG|I_QARG,NULL},
{"SNGLQ",	1,	R,	type_REAL,	I_NONSTD|I_NOTARG|I_QARG,NULL},
{"QREAL",	1,	C,	type_QUAD,	I_NONSTD|I_NOTARG|I_QARG|I_QUAD,NULL},
{"DBLEQ",	1,	R,	type_DP,	I_NONSTD|I_NOTARG|I_QARG,NULL},
{"QFLOAT",	1,	I,	type_QUAD,	I_NONSTD|I_NOTARG|I_QUAD,NULL},
{"QEXTD",	1,	D,	type_QUAD,	I_NONSTD|I_NOTARG|I_QUAD,NULL},
{"QEXT",	1,	I|R|D,	type_QUAD,	I_NONSTD|I_NOTARG|I_QUAD,NULL},
{"QCMPLX",	I_1or2,	I|R|D|C|Z,type_CQUAD,	I_NONSTD|I_NOTARG|I_QUAD,NULL},
{"QINT",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QNINT",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"IQNINT",	1,	R,	type_INTEGER,	I_NONSTD|I_QARG,NULL},
{"QABS",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"CQABS",	1,	C,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QMOD",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QSIGN",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QDIM",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QPROD",	2,	D,	type_QUAD,	I_NONSTD|I_QUAD,NULL},
{"QMAX1",	I_2up,	R,	type_QUAD,	I_NONSTD|I_NOTARG|I_QARG|I_QUAD,NULL},
{"QMIN1",	I_2up,	R,	type_QUAD,	I_NONSTD|I_NOTARG|I_QARG|I_QUAD,NULL},
{"QIMAG",	1,	C,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QCONJG",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QSQRT",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"CQSQRT",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QEXP",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"CQEXP",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QLOG",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"CQLOG",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QLOG10",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QSIN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"CQSIN",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QCOS",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"CQCOS",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QTAN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QARSIN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QARCOS",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QATAN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QATAN2",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QSINH",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QCOSH",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},
{"QTANH",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD,NULL},

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



PRIVATE int
#if HAVE_STDC
ii_abs(Token *args)
#else /* K&R style */
ii_abs(args)
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t;
  int val, result=0;
  t = args->next_token;
  if(t->TOK_type != type_INTEGER) {/* wrong arg type: message given elsewhere */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val = int_expr_value(t);
    result = (val >= 0? val: -val);
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_sign(Token *args)			/* SIGN(value,sign) */
#else /* K&R style */
ii_sign(args)			/* SIGN(value,sign) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  int val1,val2, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->TOK_type != type_INTEGER
     || t2->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val1 = int_expr_value(t1);
    if(val1 < 0) val1 = -val1;
    val2 = int_expr_value(t2);
    result = (val2 >= 0? val1: -val1);
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_dim(Token *args)			/* DIM(int,int) */
#else /* K&R style */
ii_dim(args)			/* DIM(int,int) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  int val, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->TOK_type != type_INTEGER
     || t2->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val = int_expr_value(t1)-int_expr_value(t2);
    result = (val >= 0? val: 0);
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_mod(Token *args)			/* MOD(int,int) */
#else /* K&R style */
ii_mod(args)			/* MOD(int,int) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  int val1,val2,quotient, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->TOK_type != type_INTEGER
     || t2->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val1 = int_expr_value(t1);
    val2 = int_expr_value(t2);
    if((val1 < 0) == (val2 < 0)) {
      quotient = val1/val2;	/* Both positive or both negative*/
    }
    else {
      quotient = -(-val1/val2);	/* Unlike signs */
    }
    result = val1 - quotient*val2;
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
#if HAVE_STDC
ii_ichar(Token *args)		/* ICHAR(string) */
#else /* K&R style */
ii_ichar(args)		/* ICHAR(string) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args->next_token;

  if(t->TOK_type != type_STRING || !is_true(LIT_CONST,t->TOK_flags)) {
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    return t->value.string[0];	/* Processor collating sequence is used */
  }
  return 0;
}

PRIVATE int
#if HAVE_STDC
ii_len(Token *args)		/* LEN(string) */
#else /* K&R style */
ii_len(args)		/* LEN(string) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args->next_token;
  int val,result=0;

		/* Set the PARAMETER_EXPR flag since LEN of string does
		   not require contents to be known */
  if( t->TOK_type == type_STRING && (val = t->size) > 0 ) {
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
#if HAVE_STDC
ii_index(Token *args)		/* INDEX(str1,str2) */
#else /* K&R style */
ii_index(args)		/* INDEX(str1,str2) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  t1=args->next_token;
  t2=t1->next_token;

  if(t2 == NULL || t1->TOK_type != type_STRING
     || t2->TOK_type != type_STRING
     || !is_true(LIT_CONST,t1->TOK_flags) || !is_true(LIT_CONST,t2->TOK_flags)) {
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    int i;
    char *s1=t1->value.string;
    char *s2=t2->value.string;
    int n1=strlen(s1), n2=strlen(s2);

    for(i=1; n1 > 0 && n1 >= n2; i++,s1++,n1--) {
      if(strncmp(s1,s2,n2) == 0)
	return i;
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
ii_size(Token *args)		/* SIZE( array, [dim] ) */
{
  Token *array=args->next_token;
  Token *dim = NULL;
  array_dim_t array_dim;
  int result=0;

  if( array != NULL )
    dim = array->next_token;

  if( is_true(ARRAY_EXPR,args->TOK_flags) ) {
      /* Here if dim is provided, we should access shape of array to
	 get size of the the given dimension.  Since shape is not
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

  return result;
}

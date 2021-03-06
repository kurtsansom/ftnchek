/* $Id: symtab.h,v 1.43 2003/08/07 19:28:50 moniot Exp $

	Shared declarations for symbol-table routines.  Note: uses
	declarations in ftnchek.h.

Copyright (c) 1999 by Robert K. Moniot.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
ROBERT K. MONIOT OR FORDHAM UNIVERSITY BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of ftnchek shall not be used
in advertising or otherwise to promote the sale, use or other dealings in
this Software without prior written authorization from the author.


*/

#define SYMTAB_H		/* for intrins.h */

#ifdef SYMTAB			/* "home" for variables is symtab.c */
#define SYM_SHARED
#else
#define SYM_SHARED extern
#endif

#ifdef DYNAMIC_TABLES
#ifdef __TURBOC__	/* Turbo C has only one free() */
#define cfree free
#endif
#endif

				/* Statement sequence info for fortran.y
				   and to give hints to forlex.c */
#define SEQ_HEADER   1
#define SEQ_IMPLICIT 2
#define SEQ_SPECIF   3
#define SEQ_STMT_FUN 4
#define SEQ_EXEC     5
#define SEQ_END      6

				/* Separate sequence defs to allow checking
				   f90 sequence requirements. */
#define F90_SEQ_HEADER		1
#define F90_SEQ_USE		2
#define F90_SEQ_IMPLICIT_NONE	3
#define F90_SEQ_IMPLICIT	4
#define F90_SEQ_SPECIF		5
#define F90_SEQ_EXEC		6
#define F90_SEQ_CONTAINS	7
#define F90_SEQ_INTERNAL	8
#define F90_SEQ_END		9

		/* Definitions of symbol table information */

/*	Token subclasses (classes are in tokdefs.h)
 */

#define relop_EQ	0
#define relop_NE	1
#define relop_LE	2
#define relop_LT	3
#define relop_GE	4
#define relop_GT	5



	/* Storage Class types for variables, consts, and externals */
#define class_VAR 0
#define class_SUBPROGRAM 1
#define class_COMMON_BLOCK 2
#define class_STMT_FUNCTION 3
#define class_LABEL 4
#define class_NAMELIST 5
#define class_DTYPE 6  /* derived type */
#define class_MODULE 7

	/* Data types for variables, consts, and externals */
	/* N.B. 0 thru 7 are wired into lookup tables in exprtype.c */
#define type_UNDECL 0
#define type_ERROR 0		/* for result of erroneous operation */
#define type_INTEGER 1
#define type_REAL 2
#define type_DP 3
#define type_COMPLEX 4
#define type_DCOMPLEX 5
#define type_LOGICAL 6
#define type_STRING 7
#define type_HOLLERITH 8
#define type_GENERIC 9
#define type_SUBROUTINE 10
#define type_COMMON_BLOCK 11
#define type_PROGRAM 12
#define type_BLOCK_DATA 13
#define type_LABEL 14
#define type_NAMELIST 15
#define type_MODULE 16

#define MAX_ELEMENTARY_TYPE type_MODULE /* update if new type added above */
#define MIN_DTYPE_ID (MAX_ELEMENTARY_TYPE + 1) /* starting id for derived types */

#define size_DEFAULT	(0L)	/* code for standard numeric sizes */
#define size_ADJUSTABLE	(-1L)	/* codes for special char string lengths */
#define size_UNKNOWN	(-2L)
 /* for now we don't differentiate the following */
#define size_ASSUMED_SIZE size_UNKNOWN	/* A([lb:]*) */
#define size_ASSUMED_SHAPE size_UNKNOWN /* A([lb]:) */
#define size_DEFERRED size_UNKNOWN	/* A(:) */

			/* Defns for support of quad precision.  It is
			   implemented as REAL*4n, not a genuine type of
			   its own, since no QUAD type declaration exists.
			   Here n = BpW or whatever wordsize is set at runtime.
			 */
#define type_QUAD type_REAL
#define size_QUAD (4*type_size[type_REAL])
#define type_CQUAD type_COMPLEX
#define size_CQUAD (8*type_size[type_REAL])

/* Internally defined kind numbers are designed to be in a distinct
   space from user-defined ones, and distinct from each other if
   defined differently.  They are negative.  The default kinds are the
   negative of their respective type numbers, except for QUAD, which
   does not have its own type number, so we use the first available
   number after the defined types.  The selected int and
   real kinds are coded so that the type and the selected range and
   precision can be recovered.  See selected_int_kind() et al for details  */
#define kind_DEFAULT_INTEGER (-type_INTEGER)
#define kind_DEFAULT_REAL (-type_REAL)
#define kind_DEFAULT_DP (-type_DP)
#define kind_DEFAULT_LOGICAL (-type_LOGICAL)
#define kind_DEFAULT_CHARACTER (-type_STRING)
#define kind_DEFAULT_QUAD  (-MIN_DTYPE_ID)
#define kind_DEFAULT_UNKNOWN (-(MIN_DTYPE_ID+1))

				/* tests for elementary vs derived type */
#define is_elementary_type(t) ((unsigned)(t) < MIN_DTYPE_ID)
#define is_derived_type(t) ((unsigned)(t) >= MIN_DTYPE_ID)
				/* test for types usable in exprs */
#define is_computational_type(t) ((unsigned)(t) <= (unsigned)type_HOLLERITH)
				/* test for numeric types */
#define is_numeric_type(t) ((unsigned)(t) <= (unsigned)type_DCOMPLEX)
				/* test for int, char, or logical (case-expr) */
#define is_case_type(t)  ((unsigned)(t) == (unsigned)type_INTEGER || \
			  (unsigned)(t) == (unsigned)type_LOGICAL || \
			  (unsigned)(t) == (unsigned)type_STRING)
				/* test for type allowed as parameter */
#define is_param_type(t) (((unsigned)(t)>(unsigned)0) && \
		  ((unsigned)(t)<=(unsigned)type_STRING || \
		   is_derived_type(t) || \
		   (unsigned)(t) == (unsigned)type_GENERIC))
				/* test for numeric or logical type */
#define is_num_log_type(t) ((unsigned)(t) <= type_LOGICAL)
				/* test for real/d.p./complex/d.complx type */
#define is_float_type(t) ((unsigned)(t)>=type_REAL && (unsigned)(t)<=type_DCOMPLEX)

	/* Type categories equate DoubleP to Real, Double Complex
	   to Complex, and Hollerith to Int to simplify expression
	   type propagation and argument checking.  Computational
	   types only, except that since subroutine can be passed
	   as an argument, table goes up that high.  */
SYM_SHARED
unsigned char type_category[]
#ifdef SYMTAB
={	type_UNDECL,
	type_INTEGER,
	type_REAL,
	type_REAL,
	type_COMPLEX,
	type_COMPLEX,
	type_LOGICAL,
	type_STRING,
	type_INTEGER,
	type_GENERIC,
	type_SUBROUTINE,
}
#endif
;
	/* Equivalence types equate Real, DoubleP, Complex and Double
	   Complex, for use in checking mixed equivalence and mixed
	   common, since it is standard and portable to interpret complex
	   as a pair of real values: real part and imag part */
SYM_SHARED
unsigned char equiv_type[]
#ifdef SYMTAB
={	type_UNDECL,
	type_INTEGER,
	type_REAL,
	type_REAL,
	type_REAL,
	type_REAL,
	type_LOGICAL,
	type_STRING,
	type_INTEGER}
#endif
;

/*
 * statement types referencing labels
 */

#define LAB_NO_TYPE 	   0               /* label never defined */           
#define LAB_SPECIFICATION  1
#define LAB_FORMAT	   2
#define LAB_EXECUTABLE	   3
#define LAB_GOTO	   4
#define LAB_ASSIGN	   5
#define LAB_DO		   6
#define LAB_IO		   7
#define LAB_CALL           8
SYM_SHARED
const char *lab_type_name[]
#ifdef SYMTAB
 = {
    "undef",
    "specif",
    "format",
    "exec",
    "goto",
    "assign",
    "do",
    "I/O",
    "arg",
}
#endif
;

typedef unsigned char BYTE;

			/* Define int big enough for class/type field */

typedef unsigned long type_t;

/* Define int big enough for encoded kinds which can be -10^8, need 4 bytes. */
#if (SIZEOF_INT >= 4)
typedef int kind_t;
#else
#if (SIZEOF_LONG >= 4)
typedef long kind_t;
#endif
#endif

/* eventually we should go over to this */
#if 0
typedef struct {
    kind_t kind;			/* KIND parameter */
    short type;				/* type id */
    short class;			/* storage class */
} type_t;
#endif


		/* Array of class and type name translations */
SYM_SHARED
const char *class_name[]
#ifdef SYMTAB
 = {
	"",
	"subprog",
	"common",
	"stmt fun",
	"label",
	"namelist",
	"derivedtype",
	"module"
}
#endif
;
SYM_SHARED
const char *elementary_type_name[]		/* Type names as used in warnings etc.
				   NOTE: Constrained to 4 letters */
#ifdef SYMTAB
 = {
	"undf",
	"intg",
	"real",
	"dble",
	"cplx",
	"dcpx",
	"logl",
	"char",
	"holl",
	"genr",
	"subr",
	"comm",
	"prog",
	"data",
	"labl",
	"naml",
	"mdul",
}
#endif
;

SYM_SHARED
const char *type_table[]		/* Names as used in FORTRAN statements */
#ifdef SYMTAB
 =  {
	"??ERROR??",
	"INTEGER",
	"REAL",
	"DOUBLE PRECISION",
	"COMPLEX",
	"DOUBLE COMPLEX",		/* could be "COMPLEX*16" too */
	"LOGICAL",
	"CHARACTER",
/* The rest do not appear in actual use, here for completeness only */
	"HOLLERITH",
	"GENERIC",
	"SUBROUTINE",
	"COMMON",
	"PROGRAM",
	"BLOCK DATA",
	"LABEL",
	"NAMELIST",
	"MODULE",
    }
#endif
;


/* Here declare typical sizes of objects of each data type, for use in
checking argument and common block size matchups.  BpW (bytes per word)
is defined in ftnchek.h */


SYM_SHARED
BYTE type_size[]
#ifdef SYMTAB
={
	0, /*undf*/
      BpW, /*intg*/
      BpW, /*real*/
    2*BpW, /*dble*/
    2*BpW, /*cplx*/
    4*BpW, /*dcpx*/
      BpW, /*logl*/
	1, /*char*/
      BpW, /*holl*/
	0, /*genr*/
	0, /*subr*/
	0, /*comm*/
	0, /*prog*/
	0, /*data*/
	0, /*labl*/
	0, /*naml*/
	0, /*modu*/
}
#endif
;



		/* implicit and default typing lookup table.  Two extra spots
		   provided to accommodate '$' and '_' too.  The size defns
		   should accommodate EBCDIC as well as ASCII. */
/*
SYM_SHARED
int implicit_type[('Z'-'A'+1)+2],	// indexed by [char - 'A']
    implicit_size[('Z'-'A'+1)+2];
SYM_SHARED
char *implicit_len_text[('Z'-'A'+1)+2];
*/

typedef struct {
    long size;	/* total size of an array */
    short dims;	/* number of dimensions in an array */
} array_dim_t;

	/* Declaration of Token data structure.  N.B. do not change without
	   consulting preamble of fortran.y for uses with nonterminals.
	 */

			/* temporary equivs for future separate fields */
				/* these are for array-bounds tokens */
#define TOK_dims tclass
#define TOK_elts tsubclass



struct tokstruct {
	union {
		long integer;
		DBLVAL dbl;
		const char *string;
	} value;		/* Value of constant */
	array_dim_t array_dim;	/* array size and no. of dims */
	struct tokstruct
	  *left_token,		/* Left child in expr tree */
	  *next_token;		/* Right child or next in linked list */
	const char *src_text;	/* Original text string of token */
	long tclass,tsubclass;	/* Token category and subcategory */
	long size;		/* sizeof(datatype) */
	type_t TOK_type;	/* Storage class & data type of identifier */
	kind_t kind;		/* Kind parameter */
	unsigned TOK_flags:32;	/* Exprtype flags (see defns below) */
	/* int symtab_index; */	/* symtab top when encountered (for scoping)*/
	LINENO_t line_num;	/* Line where token occurred */
	COLNO_t col_num;	/* Column where token occurred */
	unsigned size_is_adjustable : 1;
	unsigned size_is_expression : 1;
};

typedef struct tokstruct Token;

#ifdef  YYSTYPE
#undef  YYSTYPE
#endif
#define YYSTYPE Token	/* Type defn for yylval and Yacc stack */



SYM_SHARED
int loc_symtab_top,	/* Next avail spot in local symbol table */
   curr_scope_bottom	/* Start in local symbol table of innermost scope */
#ifdef SYMTAB
= -1
#endif
    ,
   glob_symtab_top;	/* Next avail spot in global symbol table */



SYM_SHARED
unsigned long loc_str_top;	/* Top of local stringspace */

SYM_SHARED
   unsigned long srctextspace_top; /* Top of token src text space */

SYM_SHARED
   unsigned long ptrspace_top;	/* Top of pointer space */

SYM_SHARED
   unsigned long param_info_space_top;	/* Top of parameter info space */

SYM_SHARED
   unsigned long token_space_top,	/* Top of token space */
		 token_head_space_top;	/* Top of TL_head space */

	/* Counts of extra items dynamically allocated, for -resource */
SYM_SHARED
  int extra_locstrspace,
      extra_paraminfospace,
      extra_srctextspace,
      extra_tokheadspace,
      extra_tokspace,
      extra_ptrspace;

SYM_SHARED
  LINENO_t top_file_line_num;

SYM_SHARED
  int global_save,	/* prog unit contains SAVE with no list */
  module_accessibility;	/* module PUBLIC or PRIVATE default accessibility */

		/* Define names for anonymous things */
#ifdef SYMTAB
char blank_com_name[] = "%BLANK",  /* id for blank common entry in symtab */
     unnamed_prog[]="%MAIN",	  /* id for unnamed program prog unit */
     unnamed_block_data[]="%DAT00";  /* id for unnamed block data prog unit */
int  block_data_number=0;       /* count of multiple anonymous block data */
#else
extern char blank_com_name[],
	    unnamed_prog[],
	    unnamed_block_data[];
extern int block_data_number;
#endif

typedef int LABEL_t;              /* a label (0-99999) */         

#define NO_LABEL ((LABEL_t)-1)    /* label never used/defined */

                /* Symbol table argument list declarations */

typedef union {		/* InfoUnion: misc info about symtab entry */
	     struct ALHead *arglist;	/* ptr to func/subr argument list */
	     struct CMHead *comlist;    /* ptr to common block list */
	     struct TLHead *toklist;  /* ptr to token list */
	     struct IInfo *intrins_info;/* ptr to intrinsic func info */
	     struct PInfo *param;	/* parameter information field */
} InfoUnion;

typedef struct {	/* ArgListElement: holds subprog argument data */
	array_dim_t array_dim;	/* array size and no. of dims */
	char *name;		/* name of dummy arg or text of actual arg */
	char *keyword;		/* name of keyword */
	int keyword_index;	/* index of keyword in definition list */
	InfoUnion info;
	struct gSymtEntry *common_block; /* block it belongs to if any */
	long common_index;	/* index in block */
	long size;
	type_t type;
	kind_t kind;
	short same_as;	/* index if two actual arguments the same */
	unsigned is_lvalue: 1,
		 set_flag: 1,
		 assigned_flag: 1,
		 used_before_set: 1,
		 array_var: 1,
		 array_element: 1,
		 declared_external: 1,
		 active_do_var: 1,
		 intent_in: 1,
		 intent_out: 1,
		 optional: 1;
} ArgListElement;


typedef struct ALHead {	    /* ArgListHeader: head node of argument list */
	array_dim_t array_dim;	/* array dims of function result */
	long size;
	kind_t kind;
	type_t type;
	short numargs;
	ArgListElement *arg_array;
	struct gSymtEntry *prog_unit;
	const char *filename,*topfile;
	LINENO_t line_num,top_line_num;
	unsigned
	     array_result: 1,	/* function return array result */
	     is_defn: 1,
	     is_call: 1,
	     is_interface: 1,
	     external_decl: 1,	/* EXTERNAL decl, not arg list */
             actual_arg: 1;	/* subprog passed as arg */
	struct ALHead *next;
} ArgListHeader;

		/* Symbol table common block list declarations */

typedef struct {	/* ComListElement: holds common var data */
	array_dim_t dimen_info;
	char *name;		/* name of common variable */
	long size;
	type_t type;
	kind_t kind;
	unsigned		/* copies of flags from symtab */
	  used:1,
	  set:1,
	  used_before_set:1,
	  assigned:1,
	  marked:1;		/* for listing of offenders */
} ComListElement;

typedef struct CMHead {	/* ComListHeader: head node of common var list */
	short numargs;
	LINENO_t line_num,top_line_num;
	ComListElement *com_list_array;
	struct gSymtEntry *prog_unit;
	const char *filename,*topfile;
	struct CMHead *next;
	unsigned
	  any_used:1,		/* any of its variables accessed */
	  any_set:1,		/* any of its variables set */
	  saved:1;		/* declared in SAVE statement */
} ComListHeader;


typedef struct TLHead {	/* TokenListHeader: head node of token list */
	Token *tokenlist;
	struct TLHead *next;
	const char *filename;
	LINENO_t line_num, top_line_num;
	unsigned
	  external_decl:1,
	  actual_arg:1,
	  is_interface:1,
	  is_defn:1;
} TokenListHeader;

typedef struct {	/* ModVar: holds module var data */
	char *name;		/* local name of module var */
	char *usename;		/* original name in module */
	unsigned		/* copies of flags from symtab */
	  used:1,
	  set:1,
	  assigned:1,
	  used_before_set:1,
	  marked:1,		/* for listing of offenders */
	  check_usage;		/* for checking usage , those not in
				   an ONLY list will not be checked */
} ModVar;

typedef struct MVHead {	/* ModVarListHeader: head node of module var list */
	short numargs;		/* number of module vars */
	LINENO_t line_num,top_line_num;	/* location of USE stmt */
	ModVar *mod_var_array;	/* list of module vars */
	struct gSymtEntry *prog_unit;	/* prog unit that USEs module */
	const char *filename,*topfile;	/* location of USEing prog unit */
	struct MVHead *next;
	unsigned
	  in_module:1,		/* usage information from module */
	  any_used:1,		/* any of its vars accessed in prog unit*/
	  any_set:1;		/* any of its vars set in prog unit */
} ModVarListHeader;

			/* Structure for intrinsic-function info */

	/* Define special num_args values for intrinsics that have
	   variable numbers of arguments. */
#define I_1or2	(-1)		/* 1 or 2 arguments */
#define I_2up	(-2)		/* 2 or more arguments */
#define I_0or1	(-3)		/* 0 or 1 argument */
#define I_1to3	(-4)		/* 1, 2, or 3 arguments */
#define I_1to4	(-5)		/* 1 to 4 arguments */
#define I_2or3	(-6)		/* 2 or 3 arguments */
#define I_2to4	(-7)		/* 2 to 4 arguments */

			/* for intrins_flags field */

	/* Define IDs for particular intrinsics that need special treatment */
#define I_CHAR		0x1	/* CHAR function returns size=1 */
#define I_SP_R		0x2	/* specific REAL function */
#define I_NULL		0x3	/* NULL function yields disassociated result */
#define I_ID		0xf	/* mask for hex digit */
				/* macro to identify particular functions */
#define INTRINS_ID(flags) ((flags)&I_ID)

		/* Various properties of intrinsics */
#define I_NONF77	0x10	/* Nonstandard */
#define I_MIXED_ARGS	0x20	/* Has mixed arg types */
#define I_NONPURE	0x40	/* Arg need not be set when called */
#define I_C_TO_R	0x80	/* Complex -> real in generic form */
#define I_NOTARG	0x100	/* Not allowed as actual argument */
#define I_INQ		0x200	/* inquiry, ignores value of arg */
#define I_QARG		0x400	/* Arg type is R*16 or X*32 */
#define I_QUAD		0x800  	/* Result type is R*16 or X*32 */
#define I_EXTRA		0x1000 	/* commonly found extra intrinsics */
#define I_VMS		0x2000 	/* VMS systems only */
#define I_UNIX		0x4000 	/* Unix systems only */
#define I_NONF90	0x8000 	/* Not in F90 standard */
#define I_NONF95	0x10000	/* Not in F95 standard */
#define I_EVAL		0x20000 /* Always evaluate */
#define I_PTR		0x40000 /* Returns a pointer */
#define I_ELEM		0x80000	/* Elemental (acts elementwise on arrays) */
#define I_ARRY		0x100000/* Returns array result diff shape from arg */
#define I_OK		0x200000/* Takes optional KIND argument */
#define I_CMPLX		0x400000/* CMPLX intrinsic */

			/* Define flag type big enough for 6 hex digits */
#if (SIZEOF_SHORT > 2)
typedef unsigned short intrins_flags_t;
#else
#if (SIZEOF_INT > 2)
typedef unsigned int intrins_flags_t;
#else
#if (SIZEOF_LONG > 2)
typedef unsigned long intrins_flags_t;
#endif
#endif
#endif

typedef struct IInfo{
	const char *name;	   /* intrinsic name */
	short num_args;	   /* number of args (<0 codes for varying) */
	unsigned arg_type; /* bitwise OR of 1<<type for allowed types */
	int result_type;   /* type code of result */
	intrins_flags_t
	      intrins_flags;	/* nonstandard,  mixed arg types */
	int (*ii_handler)( Token *args ); /* evaluation handler  */
} IntrinsInfo;

			/* Structure for parameter info */
typedef struct PInfo{
	const char *src_text;		/* source text of parameter value */
	union {
	  long integer;		/* integer value */
	  DBLVAL dbl;		/* float value */
	  const char *string;		/* character string value */
	} value;
	int seq_num;		/* position in parameter definitions */
} ParamInfo;
	

			/* Structure for call-tree child list */
typedef struct childlist {
  struct gSymtEntry *child;	/* Pointer to child's symtab entry */
  struct childlist *next;/* Pointer to next child on list */
} ChildList;

		/*  Identifier symbol table declaration */


typedef struct lSymtEntry{
	array_dim_t array_dim;	/* array size and no. of dims */
	char *name;             /* Identifier name in stringspace */
	InfoUnion info;
	union{
	  char *text;		/* Source text string */
	  char **textvec;	/* List of source text strings */
	  TokenListHeader *toklist; /* for namelist & common block makedecls */
	} src;
	struct lSymtEntry *mask;  /* Points to masked identifier in enclosing scope*/
	struct lSymtEntry *equiv_link;	/* Link for equivalence lists */
		/* common_block is a ptr to block if this is a common
		   variable, and common_index is its position (starting
		   from 1).  For block, common_index is the count of
		   variables in it. */
	struct gSymtEntry *common_block;
	long common_index;
				/* Prog unit where variable declared.  For
				   module variables it is original source. */
	char *home_unit;
	long size;		/* Size of object in bytes */
		/* Object can be referenced in an include file. Next fields
		   are line numbers within file where object is referred
		   to, then come indexes into include-file list.  */
	LINENO_t line_declared, line_set, line_used, line_assocd, line_allocd;
	short file_declared, file_set, file_used, file_assocd, file_allocd;
	type_t  type;		/* Type & storage class: see macros below */
	kind_t kind;		/* Kind parameter */
			/* Flags */
	unsigned
	     used_flag: 1,	/* value is accessed (read from variable) */
	     set_flag: 1,	/* variable is set or passed as subr arg */
	     assigned_flag: 1,	/* value is really set (by assignment stmt) */
	     used_before_set: 1,/* set_flag is not set when used_flag is set */
	     is_current_prog_unit: 1, /* this symtab entry is the main prog unit */
	     library_prog_unit: 1,	/* prog unit was processed in -library mode */
	     active_do_var: 1,	/* variable is an active DO index */
	     forall_var: 1,	/* variable is an active FORALL index */
	     array_var: 1,	/* variable is dimensioned */
	     common_var: 1,	/* variable is in common */
	     entry_point: 1,	/* name of an entry point */
	     parameter: 1,	/* name of a parameter */
	     argument: 1,	/* dummy argument */
	     external: 1,	/* function or subr called by this routine */
	     intrinsic: 1,	/* intrinsic function */
	     saved: 1,		/* named in SAVE statement */
	     allocatable: 1,	/* has ALLOCATABLE attribute */
	     pointer: 1,	/* has POINTER attribute */
	     associated_flag: 1,	/* pointer is associated */
	     used_before_associated: 1, /* pointer used when NULL */
             allocated_flag:1,    /* pointer/allocatable variable is allocated */
             used_before_allocation:1, /* pointer/allocatable variable is used before allocation */
	     target: 1,		/* has TARGET attribute */
	     assigned_as_target: 1,	/* assigned to a pointer */
	     public_attr: 1,		/* has PUBLIC attribute / public type */
	     private_attr: 1,		/* has PRIVATE attribute / private type */
	     private_components: 1,	/* PRIVATE decl inside type defn */
	     sequence: 1,	/* has SEQUENCE attribute */
	     intent_in: 1,	/* has IN attribute */
	     intent_out: 1,	/* has OUT attribute */
	     optional: 1,	/* has OPTIONAL attribute */
	     invoked_as_func: 1, /* usage as f(x) was seen */
	     defined_in_include: 1, /* to suppress some warnings if unused */
	     defined_in_module: 1, /* imported via USE */
	     declared_external: 1, /* explicitly declared external */
	     declared_intrinsic: 1, /* explicitly declared intrinsic */
	     size_is_adjustable : 1, /* CHARACTER*(*) declaration */
	     size_is_expression : 1, /* CHARACTER*(expr) declaration */
	     result_var : 1, /* variable is result name for a function */
	     elemental : 1, 	/* ELEMENTAL subprogram */
	     pure : 1, 		/* PURE subprogram */
	     recursive : 1, 	/* RECURSIVE subprogram */
	     kind_is_bogus : 1;	/* kind not determined by lookahead */
} Lsymtab;

typedef struct gSymtEntry{	/* Global symbol table element */
	char *name;             /* Identifier name in stringspace */
	InfoUnion info;
	union {
	  struct childlist *child_list; /* List of callees (for prog unit) */
	  struct gSymtEntry *prog_unit; /* Prog unit (for interior entry) */
	} link;
	struct gSymtEntry *mask;	/* entry masked by internal */
	ModVarListHeader *modvarlist;	/* List of module variables */
	long size;
	type_t  type;		/* Type & storage class: see macros below */
	kind_t kind;		/* Kind parameter */
			/* Flags.  See remarks above */
	unsigned
	     used_flag: 1,
	     set_flag: 1,
	     assigned_flag: 1,
	     library_prog_unit: 1,
	     internal_entry: 1,	/* entry point other than at the top */
	     invoked_as_func: 1,
	     visited: 1,	   /* this entry point is in call tree */
	     visited_somewhere: 1, /* some entry point of prog unit is in call tree */
	     defined: 1,	/* is defined somewhere */
	     defined_in_include: 1,
	     declared_external: 1,
	     internal_subprog: 1,
	     module_subprog: 1,
	     valid: 1,
	     private_attr: 1,	/* accessibility of module subprogs */
	     from_module: 1,	/* procedure imported via USE stmt */
			/* The following flags are for project-file use.
			   They get reset when a file is opened and accumulate
			   values as file is read.  */
	     used_this_file: 1,
	     set_this_file: 1,
	     invoked_as_func_this_file: 1,
	     declared_external_this_file: 1,
	     recursive: 1,	/* RECURSIVE subprogram */
	     elemental : 1, 	/* ELEMENTAL subprogram */
	     pure : 1; 		/* PURE subprogram */
} Gsymtab;


		/*  Identifier hashtable declaration  */

typedef struct hashEntry {
	char	*name;		/* Identifier name in stringspace */
	Lsymtab	*loc_symtab,	/* Local symtab entry for vars etc. */
		*com_loc_symtab;/* Local symtab entry for common blocks */
	Gsymtab	*glob_symtab,	/* Global symtab entry for vars etc. */
		*com_glob_symtab;/* Global symtab entry for common blocks */
} HashTable;

SYM_SHARED
  int current_prog_unit_hash	/* hashtable index of current prog unit name */
#ifdef SYMTAB
 = -1
#endif
;

typedef struct implicit_def {	/* structure to hold IMPLICIT definition */
  int implicit_none;		/* true if IMPLICIT NONE */
  int type[('Z'-'A'+1)+2],	/* indexed by [char - 'A'] */
		size[('Z'-'A'+1)+2];
  char *len_text[('Z'-'A'+1)+2];
} Implicit;

SYM_SHARED
Implicit implicit_info;

PROTO(void set_implicit_none, ( void ));

/*
int implicit_type[('Z'-'A'+1)+2],	// indexed by [char - 'A']
    implicit_size[('Z'-'A'+1)+2];
SYM_SHARED
char *implicit_len_text[('Z'-'A'+1)+2];
*/



/* Lookahead kluge for getting explicit interface of internal
   procedures */

/* Struct for storing info about internal procedure interface.
   At present with lookahead kluge, we only store info needed
   for local checking.
 */
typedef struct {
  int datatype;
  long size;
  kind_t kind;
  array_dim_t array_dim;
  unsigned
    kind_is_bogus:1,		/* unable to determine kind */
    array:1,			/* function returns an array */
    pointer:1,			/* function returns a pointer */
    target:1,			/* function returns a target */
    elemental:1,		/* elemental procedure */
    pure:1,			/* pure procedure */
    recursive:1;		/* recursive procedure */
} ProcInterface;


SYM_SHARED
int file_has_contains;	/* init_scan() records presence of CONTAINS stmt */

/* names for return values of search_for_internal */
#define LOOKAHEAD_NOTFOUND 0
#define LOOKAHEAD_INTERNAL 1
#define LOOKAHEAD_MODULE 2

PROTO(int search_for_internal,(const char *name, ProcInterface *interface));


/* Local scope management */

typedef struct scope_struct {
    int symt_index; /* array index in local symbol table */
    int hash_num;   /* hash number for current scoping unit */
    int exec_stmt_count;
    Implicit implicit;
} Scope;

SYM_SHARED Scope loc_scope[MAXSCOPES]; /* stores local scope info */
SYM_SHARED int loc_scope_top;    /* next available slot in scope stack */

PROTO(void push_loc_scope, ( void )); /* open a new scope */
PROTO(int pop_loc_scope, ( void ));		/* exit a scope */
PROTO(void move_outside_scope, (Lsymtab *symt));	  /* move symt entry into enclosing scope */
PROTO(int in_curr_scope, ( const Lsymtab *entry ));/* test symt entry in scope */
PROTO(int find_scope, ( const Lsymtab *entry )); /* return index of scope that entry belongs to */
PROTO(int empty_scope,( void ));
PROTO(int in_enclosing_scope, (const Lsymtab *entry));

/* Global scope management */

void clean_globals(int h, SUBPROG_TYPE limit); /* check if global symbol table entries are true globals */

				/* Symbolic names for I/O access modes */
typedef enum {
     IO_ACCESS_DEFAULT, IO_ACCESS_DIRECT, IO_ACCESS_SEQUENTIAL
} IO_ACCESS_TYPE;

				/* Symbolic names for I/O forms */
typedef enum {
     IO_FORM_DEFAULT, IO_FORM_UNFORMATTED, IO_FORM_FORMATTED
} IO_FORM_TYPE;

#define IO_UNIT_UNKNOWN -1
#define IO_UNIT_DEFAULT -2	/* For unit=* */
				/* Struct for I/O unit usage */
typedef struct {
     int line_num;		/* location of I/O usage */
     int unit_no;		/* unit number if known, else UNKNOWN */
     int unit_id;		/* hash num of unit if variable, else UNKNOWN or DEFAULT */
     IO_ACCESS_TYPE io_access;	/* access mode of file */
     IO_FORM_TYPE io_form;	/* form specified for file */
     int io_operation;		/* input, output, open, close, etc. */
} IO_Unit_Info;

SYM_SHARED
IO_Unit_Info* io_unit_info	/* Array of I/O usage instances */
#ifdef SYMTAB
 = (IO_Unit_Info*)NULL
#endif
;

SYM_SHARED
int max_io_unit_usages		/* current size of I/O usage array */
#ifdef SYMTAB
 = 0
#endif
;

SYM_SHARED
int num_io_unit_usages		/* number of I/O usage instances in list */
#ifdef SYMTAB
 = 0
#endif
;
				/* Struct for include-file list */
typedef struct {
    const char *fname;		/* name of include file */
    LINENO_t line;		/* line of topfile where included */
    short footnote;             /* footnote number--for printing
				   labels */
} IncFile;

SYM_SHARED
IncFile* incfile_list
#ifdef SYMTAB
 = (IncFile*)NULL
#endif
;

SYM_SHARED
int num_incfiles		/* number of include-files in list */
#ifdef SYMTAB
 = 0
#endif
;

SYM_SHARED
short inctable_index;		/* index of current include-file in list */

				/* Struct for chunks of string space */
typedef struct STSpace {
  struct STSpace *next;
  char strspace[STRSPACESZ];
} StrSpace;

				/* Struct for providing chunks of space
				   for parameter info. */
typedef struct PISpace {
  struct PISpace *next;
  ParamInfo paraminfospace[PARAMINFOSPACESZ];
} ParamInfoSpace;

				/* Struct for providing chunks of space
				   for token list headers for arg lists etc. */
typedef struct THSpace {
  struct THSpace *next;
  TokenListHeader tokheadspace[TOKHEADSPACESZ];
} TokHeadSpace;


				/* Struct for providing chunks of space
				   for tokens for arg lists etc.  */
typedef struct TSpace {
  struct TSpace *next;
  Token tokenspace[TOKENSPACESZ];
} TokenSpace;

				/* Struct for providing chunks of space
				   for pointers to array & param text */
typedef struct PSpace {
  struct PSpace *next;
  char * ptrspace[PTRSPACESZ];
} PtrSpace;


		/* Macro to zero out symbol table entry */

#define clear_symtab_entry(S) {register unsigned i;\
				 for(i=0;i<sizeof(*S);i++)((char*)S)[i]=0;}


	/* These macros pack and unpack datatype and storage class in type
	   field of symbol table entry. Datatype portion is 12 bits. */

#define datatype_of(TYPE) ((unsigned)((TYPE) & 0xFFF))
#define storage_class_of(TYPE) ((unsigned)((TYPE) >> 12))
#define type_pack(SCLASS,DTYPE) ((unsigned)(((SCLASS)<<12) + (DTYPE)))


	/* This macro is for pattern matching in flag checking */

#define flag_combo(A,B,C) (((A)<<2) | ((B)<<1) | (C))

 	/* Macros for working with array_dim field */
				/* get no. of dimensions */
#define array_dims(DIM_INFO) ((DIM_INFO).dims)
				/* get no. of elements */
#define array_size(DIM_INFO) ((DIM_INFO).size)
				/* test if no. of elements is unknown.  This
				   includes size_UNKNOWN and size_ALLOCATABLE */
#define array_size_is_unknown(DIM_INFO) ((DIM_INFO).size < 0)
				/* create info field of given dims&size */
#define array_dim_info(DIM,SIZE) ((array_dim_t){(SIZE),(DIM)})
				/* create info field for unknown size */
#define array_dim_info_unk_size(DIM) ((array_dim_t){size_UNKNOWN,(DIM)})

	/* Macros for testing keyword names */
				/* test whether a token has keyword */
#define keyword_present(t) ((t) != NULL && (t)->left_token != NULL && \
      			    (t)->left_token->tclass == '=')
			/* test whether a keyword matchs string.
			 * Use only if keyword_present(t) is true.  */
#define keyword_name_match(t,s) (strcmp((t)->left_token->left_token->src_text, (s)) == 0)
	

		/* Defns used by expression type propagation mechanisms
		   in fortran.y and exprtype.c  The flags go in token.TOK_flags
		   Make sure size of TOK_flags declared above suffices for
		   largest item in the list below.
		 */

#define make_true(flag,x) ((x) |= ((unsigned)flag))	/* x.flag <-- true   */
#define make_false(flag,x) ((x) &= ~((unsigned)flag))	/* x.flag <-- false  */
#define is_true(flag,x) ((x) & (flag))			/* x.flag == true?   */
#define copy_flag(flag,x,y)  ((x) |= ((y)&((unsigned)flag))) /* x.flag <-- y.flag */

#define ID_EXPR			0x1	/* a variable, value.integer is hashtab index */
#define LVALUE_EXPR		0x2	/* assignable */
#define CONST_EXPR		0x4	/* compile-time constant per std 6.7*/
#define LIT_CONST		0x8	/* a number or string literal */
#define ARRAY_EXPR		0x10	/* an array or array element */
#define ARRAY_ELEMENT_EXPR	0x20 	/* an array element */
#define INT_QUOTIENT_EXPR	0x40 	/* contains INT/INT */
#define STMT_FUNCTION_EXPR	0x80
#define PARAMETER_EXPR		0x100/* == CONST_EXPR || intrinsic || **real */
#define EVALUATED_EXPR		0x200  /* token.value has value of expr */
#define SET_FLAG		0x400  /* id may be set */
#define ASSIGNED_FLAG		0x800  /* id is set in assignment stmt */
#define USED_BEFORE_SET		0x1000  /* id used beforre set */
#define COMPLEX_FLAG		0x2000	/* remembers complex_const_allowed */
#define CHAR_ID_EXPR		0x4000	/* char var or array elt not substr */
#define DIM_BOUND_EXPR		0x8000	/* no array or func ref (5.1.1.1) */
#define IN_ASSIGN 		0x10000	/* for tracking assgn stmt lhs */
#define COMMA_FLAG		0x20000/* keeps track of extra or missing
				   	  commas in exprlists (obsolete) */
#define NONSTD_USAGE_FLAG	0x40000	/* concentrator for -f77 warnings */
#define NOT_DO_TERMINAL_STMT    0x80000 /* stmt illegal as end of DO loop */
#define DO_VARIABLE		0x100000 /* id is active DO index variable */
#define SYNTAX_ERROR_FLAG	0x200000/* concentrator for syntax errors */
#define POINTER_EXPR		0x400000 /* has POINTER attribute */
#define TARGET_EXPR		0x800000 /* has TARGET attribute */
#define DTYPE_COMPONENT		0x1000000 /* is a component ref, e.g. A%B */
#define ASSOCIATED_EXPR		0x2000000 /* pointer is associated */
#define ALLOCATED_EXPR		0x4000000 /* object is allocated */
#define ALLOCATABLE_EXPR	0x8000000 /* has ALLOCATABLE attribute */

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
SYM_SHARED
Lsymtab	*loc_symtab
#ifdef SYMTAB
  =(Lsymtab *)NULL
#endif
;
SYM_SHARED
Gsymtab *glob_symtab
#ifdef SYMTAB
  =(Gsymtab *)NULL
#endif
;
SYM_SHARED
HashTable *hashtab
#ifdef SYMTAB
  =(HashTable *)NULL
#endif
;

#else				/* static tables declared at compile time */
		/* Each major table is housed in a separate file so that
		   on IBM PC architecture with huge memory model
		   each will be in its own 64K segment not all in one. */
#ifndef PLSYMTAB
extern
#endif
Lsymtab	loc_symtab[LOCSYMTABSZ]; /* Local identifiers */
#ifndef PGSYMTAB
extern
#endif
Gsymtab glob_symtab[GLOBSYMTABSZ]; /* Global identifiers: subrs and com blks */
#ifndef EXPRTYPE
extern
#endif
HashTable hashtab[HASHSZ];	/* Hash table for identifier lookup */

#endif/* end static tables */

	/* The following tables start life as statically declared
	   tables, but when add'l space is needed, new structs of same
	   kind will be allocated and linked via next field of struct.
	   Because they are dynamically extended, they are not in
	   the DYNAMIC_TABLES section or its complement above.  Note
	   that as global variables they start off at 0, so next field
	   of each is implicitly initialized to NULL.  */

#ifndef FORLEX
extern
#endif
TokenSpace tokspace;	/* Tokens for arg lists etc */

#ifndef PROJECT
extern
#endif
TokHeadSpace tokheadspace;/* Tokenlist headers */

#ifndef PROJECT
extern
#endif
ParamInfoSpace paraminfospace;/* Parameter info structs */

#ifndef PROJECT
extern
#endif
PtrSpace ptrspace;	/* Space for storing arrays of pointers */

#ifndef SYMTAB
extern
#endif
StrSpace lstrspace;	/* String space for local identifiers */

#ifndef SYMTAB
extern
#endif
StrSpace srctextspace;/* String space for token source text */


		/* Shared routines */


			/* in exprtype.c */
PROTO(void assignment_stmt_type,( Token *term1, Token *equals, Token *term2 ));
PROTO(void binexpr_type,( Token *term1, Token *op, Token *term2, Token
		  *result ));
PROTO(void check_initializer_type, ( Token *assignee_list, Token *equals, Token *expr_list));
PROTO(void check_elemental_args,( Token *id, Token *arg ));
PROTO(void func_ref_expr,( Token *id, Token *args, Token *result ));
PROTO(void primary_id_expr,( Token *id, Token *primary ));
PROTO(void stmt_fun_arg_cmp,( const Lsymtab *symt, const Token *d_arg, const Token *a_arg ));
PROTO(int substring_size,( Token *id, Token *limits ));
PROTO(array_dim_t subarray_size,( Token *id, Token *subscript_list ));
PROTO(void unexpr_type,( Token *term1, Token *op, Token *result ));
PROTO(int intrins_arg_cmp,( IntrinsInfo *defn, Token *t));

			/* in advance.c */
PROTO(int see_double_colon,( void ));
PROTO(void mark_prog_unit_srcline,( LINENO_t line_num ));

			/* in forlex.c */
PROTO(void implied_id_token,( Token *t, char *s ));
PROTO(int yylex,( void ));

			/* in keywords.c */
PROTO(const char *keytok_name,(int tclass));

			/* in fortran.y/fortran.c */
PROTO(void check_seq_header,( Token *t ));
PROTO(Token * append_token,( Token *tlist, Token *t ));
PROTO(void process_forall_construct,(Token *t));
PROTO(SUBPROG_TYPE, find_subprog_type(int stmt_class));

			/* in prlists.c */
PROTO(ModVar * new_modvar,( unsigned count ));
PROTO(ModVarListHeader * new_modvarlistheader,( void ));
ArgListHeader *make_dummy_arg_array_wrapper(Token *t);

			/* in prlocsym.c */
PROTO(void print_loc_symbols,( void ));

			/* in makehtml.c */
PROTO(void make_html,(Lsymtab **sym_list, char *mod_name, Lsymtab *prog_unit ));
PROTO(const char *strip_blanks,(const char *s));

			/* in makedcls.c */
PROTO(void make_declarations,( Lsymtab *sym_list[], char *mod_name ));

			/* in project.c */
PROTO(void write_module_file, (int h));
PROTO(void read_module_file, (int h, Token *only, int only_list_mode));

			/* in symtab.c */
PROTO(void apply_attr,( Token *id, int attr ));
#if 0
PROTO(int array_dims, (array_dim_t dim_info));
PROTO(unsigned long array_size, (array_dim_t dim_info));
PROTO(array_dim_t array_dim_info, ( short dim, unsigned long size ));
PROTO(int array_size_is_unknown,(array_dim_t dim_info));
#endif
PROTO(int array_dim_cmp,(array_dim_t a, array_dim_t b));
PROTO(void call_func,( Token *id, Token *arg ));
PROTO(void call_subr,( Token *id, Token *arg ));
PROTO(const char * char_expr_value,( Token *t ));
PROTO(void check_loose_ends,( int curmodhash ));
PROTO(void declare_type,( Token *id, int datatype, kind_t kind, long size, char *size_text ));
PROTO(void def_arg_name,( Token *id ));
PROTO(void def_array_dim,( Token *id, Token *arg ));
PROTO(void def_com_block,( Token *id, Token *comlist ));
PROTO(void def_com_variable,( Token *id ));
PROTO(int def_curr_prog_unit,( Token *id ));
PROTO(void def_do_variable,( Token *id ));
PROTO(void def_equiv_name,( Token *id ));
PROTO(void def_ext_name,( Token *id ));
PROTO(void def_function,( int datatype, long size, char *size_text, kind_t kind,
		   Token *id, Token *args, SUBPROG_TYPE subprogtype ));
PROTO(void def_intrins_name,( Token *id ));
PROTO(void def_module,( Token *id, Token *only, int only_list_mode ));
PROTO(void def_namelist,( Token *id, Token *list ));
PROTO(void def_namelist_item,( Token *id ));
PROTO(void def_parameter,( Token *id, Token *val, int noparen ));
PROTO(void def_result_name,( Token *id ));
PROTO(void def_stmt_function,( Token *id, Token *args ));
PROTO(void do_assignment_stmt,( Token *stmt ));
PROTO(void do_ASSIGN,( Token *id ));
PROTO(void do_assigned_GOTO,( Token *id ));
PROTO(void do_ENTRY,( Token *id, Token *args, int hashno ));
PROTO(int do_RETURN,( int hashno, Token *keyword ));
PROTO(void do_bind_spec,(Token *p, SUBPROG_TYPE subprogtype));
PROTO(void do_suffix,(int stmt_class, SUBPROG_TYPE subprogtype, int hashno, Token *suffix, int result_var_hashno));
PROTO(void equivalence,( Token *id1, Token *id2 ));
PROTO(void equivalence_result_vars,(int result_hashno));
PROTO(DBLVAL float_expr_value,( Token *t ));
PROTO(int get_size,( const Lsymtab *symt, int type ));
PROTO(char * get_size_text,( const Lsymtab *symt, int type ));
PROTO(kind_t get_kind,(const Lsymtab *symt, int type));
PROTO(int get_type,( const Lsymtab *symt ));
PROTO(unsigned hash_lookup,( const char *s ));
PROTO(Gsymtab* install_global,( int h, int datatype, int storage_class ));
Lsymtab *install_local(int h, int datatype, int storage_class);
PROTO(int int_expr_value,( Token *t ));
PROTO(int logical_expr_value,( Token *t ));
PROTO(void def_forall_index, (Token *t));
PROTO(char * new_global_string,( const char *s ));
PROTO(void free_textvec,( char **p ));
PROTO(void mark_recursive, (int *is_recursive));
PROTO(char * new_src_text,( const char *s, int len ));
PROTO(char * new_src_text_alloc,( int size ));
PROTO(char * new_tree_text,( Token *t ));
PROTO(char ** new_textvec,( int n ));
PROTO(Token * new_token,( void ));
PROTO(void msg_expr_tree, (const Token *t));
#ifdef DEVELOPMENT
PROTO(void print_src_text,( Token *t ));
PROTO(void print_expr_tree,( Token *t ));
PROTO(void print_expr_list,( Token *t ));
#endif
PROTO(void process_lists,( int curmodhash ));
PROTO(void record_io_unit_id, (Token *id));
PROTO(void record_io_usage, (Token *stmt));
PROTO(void ref_array,( Token *id, Token *subscrs ));
PROTO(void ref_namelist,( Token *id, int stmt_class ));
PROTO(void ref_identifier,( Token *id ));
PROTO(void ref_variable,( Token *id ));
PROTO(void save_com_block,( Token *id ));
PROTO(void set_implicit_type,( int type, long size, char *len_text, int c1, int c2 ));
PROTO(void stmt_function_stmt,( Token *id ));
PROTO(char * token_name,( Token *t ));
PROTO(const char * type_name,( int t ));
PROTO(const char * global_type_name,( int t ));
PROTO(void undef_do_variable,( int h ));
PROTO(void use_actual_arg,( Token *id ));
PROTO(void use_implied_do_index,( Token *id ));
PROTO(void use_io_keyword,( Token *keyword, Token *value, int stmt_class ));
PROTO(void use_special_open_keywd,( Token *id ));
PROTO(void use_lvalue,( Token *id ));
PROTO(void use_parameter,( Token *id ));
PROTO(void use_variable,( Token *id ));
PROTO(void use_pointer,( Token *id ));
PROTO(void use_pointer_lvalue,( Token *id, Token *rhs ));
PROTO(void use_target,(Token *id));
PROTO(void do_allocate,( Token *id ));
PROTO(void do_deallocate,( Token *id ));
PROTO(void do_nullify,( Token *id ));
PROTO(char* typespec, ( int t, int has_size, long size,
			int has_len, long len));
PROTO(char* global_typespec, ( int t, int has_size, long size,
			int has_len, long len));
				/* The following size is conservative,
				   to make sure no buffer overruns occur.
				 */
		/* Maximum length of a typespec() result.  */
#define MAX_TYPESPEC (4+4+6*sizeof(long))

/* Routines to set and retrieve info about kind numbers */
PROTO(kind_t default_kind,(int type));
PROTO(kind_t selected_int_kind,( int range ));
PROTO(kind_t selected_real_kind_r,( int range ));
PROTO(kind_t selected_real_kind_p,( int precision ));
PROTO(kind_t selected_real_kind_p_r,( int precision, int range ));
PROTO(int kind_type,(kind_t kind));
PROTO(int kind_range,(kind_t kind));
PROTO(int kind_precision,(kind_t kind));
PROTO(int kind_is_default,(kind_t kind));
/* msg_tail printout of kind info */
PROTO(void report_kind,( kind_t k ));

				/* in symtab.c (formerly hash.c) */
PROTO(unsigned long hash,( const char *s ));
PROTO(unsigned long rehash,( unsigned long hnum ));


			/* To stop printing errors after limit is reached,
			   unless limit is 0.  Increment error count
			   even if don't print.
			 */
#define CASCADE_LIMIT(ERROR_COUNT) (++(ERROR_COUNT) > error_cascade_limit \
			&& error_cascade_limit > 0)


		/* prototypes of label routines */
void init_labtable(void);
void print_labels(void);
void print_label_refs(void); 
void check_labels(char *mod_name);
void sort_labtable(void);
int def_label(Token *t, int type);
void def_do_label(Token *t);
void ref_label(Token *t, int type);
void update_label_resources(void);

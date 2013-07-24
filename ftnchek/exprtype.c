/* exprtype.c -- propagates datatype thru expressions.


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

/* I. */

/* $Id: exprtype.c,v 1.15 2005/02/07 00:38:01 moniot Exp $

	Routines to propagate datatype through expressions.

	binexpr_type()		Yields result type of binary expression.
	unexpr_type()		Yields result type of unary expression.
	assignment_stmt_type()	Checks assignment statement type.
	func_ref_expr(id,args,result) Forms token for a function invocation.
	primary_id_expr()	Forms token for primary which is an identifier.
	stmt_fun_arg_cmp(t1,t2) Checks agreement between stmt func args.
    int	int_power(x,n)		Computes x**n for value propagation.
        init_typesizes(wdsize)	Sets standard type sizes
*/

#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#define EXPRTYPE
#include "symtab.h"
#include "symutils.h"
#include "dtypes.h"
#include "tokdefs.h"



PROTO(PRIVATE const char* sized_typename,( int type, long size ));
PROTO(PRIVATE void report_mismatch,( const Token *term1, const Token *op, const Token *term2 ));
PROTO(PRIVATE void report_type,( const Token *t ));
PROTO(PRIVATE void report_kind_property,(int value, const char *tag));
PROTO(PRIVATE int int_power,( int x, int n ));
PROTO(PRIVATE int array_section_size,(Token *id, int dim, Token *bounds));
PROTO(PRIVATE void check_array_conformance,(Token *term1, Token *term2,
		    Token *result, int op_line_num, int op_col_num));


	/* shorthand for datatypes.  must match those in symtab.h */
	/* N.B. Also, the fact that type_DEFAULT=0 is assumed in size
	   propagation code. */
#define E 0	/*  Error for invalid type combos  */
#define I 1
#define R 2
#define D 3
#define C 4
#define Z 5
#define L 6
#define S 7
#define H 8
#define NumT (H+1)		/* number of types in tables below */

#define W 10		/*  Warning for nonstandard type combos: W>NumT */

			/* for  + - / * **	ANSI book pp. 6-5,6-6	*/
			    /* Mixed double+complex = double complex with
			       warning, double + double complex is OK */
PRIVATE unsigned char arith_expr_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  I,  R,  D,  C,  Z,  E,  E,  E },	/* I */
{ E,  R,  R,  D,  C,  Z,  E,  E,  E },	/* R */
{ E,  D,  D,  D,W+Z,  Z,  E,  E,  E },	/* D */
{ E,  C,  C,W+Z,  C,  Z,  E,  E,  E },	/* C */
{ E,  Z,  Z,  Z,  Z,  Z,  E,  E,  E },	/* Z */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* S */
{ E,  E,  E,  E,  E,  E,  E,  E,  E }	/* H */
};

			/* for  relops.  Corresponds to arith type table
			   except that nonstandard comparisons of like
			   types have warning, not error. */
PRIVATE unsigned char rel_expr_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  L,  L,  L,  L,  L,  E,  E,W+L },	/* I */
{ E,  L,  L,  L,  L,  L,  E,  E,  E },	/* R */
{ E,  L,  L,  L,W+L,  L,  E,  E,  E },	/* D */
{ E,  L,  L,W+L,  L,  L,  E,  E,  E },	/* C */
{ E,  L,  L,  L,  L,  L,  E,  E,  E },	/* Z */
{ E,  E,  E,  E,  E,  E,W+L,  E,W+L },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  L,  E },	/* S */
{ E,W+L,  E,  E,  E,  E,W+L,  E,W+L }	/* H */
};

			/* Result of assignment:  lvalue = expr.  Here rows
			   correspond to type of lvalue, columns to type
			   of expr */
PRIVATE unsigned char assignment_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  I,  I,  I,  I,  I,  E,  E,W+I },	/* I */
{ E,  R,  R,  R,  R,  R,  E,  E,W+R },	/* R */
{ E,  D,  D,  D,  D,  D,  E,  E,W+D },	/* D */
{ E,  C,  C,  C,  C,  C,  E,  E,W+C },	/* C */
{ E,  Z,  Z,  Z,  Z,  Z,  E,  E,W+Z },	/* Z */
{ E,  E,  E,  E,  E,  E,  L,  E,W+L },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  S,  E },	/* S */
{ E,  E,  E,  E,  E,  E,  E,  E,  E }	/* H not possible for lvalue */
};


#define COMMA_LIST (opclass == ',') /* Flag to modify behavior of binexpr_type */

#define AC_LIST (op->tsubclass == tok_l_ac_delimiter) /* Flag to distinguish array constructor
						    * from intrinsic arg list */

	/* Routine used in printing diagnostics: returns string "type" for
	   unsized objects, "type*size" for explicitly sized things.  Due
	   to use of local static variable, cannot be invoked twice in the
	   same expression.  */
PRIVATE const char*
#if HAVE_STDC
sized_typename(int type, long int size)
#else /* K&R style */
sized_typename(type,size)
  int type; long size;
#endif /* HAVE_STDC */
{
  static char strbuf[]="type*000000"; /* template */
  static const char *char_unk="char*(?)";
  static const char *char_adj="char*(*)";
  if(size == size_DEFAULT) {
    return type_name(type);	/* no explicit size */
  }
  else {
    if(type != S || size > 0) {
      (void)sprintf(strbuf,"%4s*%ld",	/* type*size */
	    type_name(type),
	    size%1000000);
    }
    else {			/* handle special character size codes */
      if(size == size_ADJUSTABLE)
	return char_adj;
      else /*size_UNKNOWN*/
	return char_unk;
    }
  }
  return strbuf;
}


void
init_typesizes(VOID)
		/* Only executes once.  Thus cannot change wordsize
		   after processing starts. */
{
  static int trapdoor=FALSE, ptr_trapdoor=FALSE;
  if(trapdoor) {
    if(given_wordsize != local_wordsize) {
      (void)fprintf(stderr,
	      "\nSorry-Cannot change wordsize after processing starts");
    }
    given_wordsize = local_wordsize;
  }
  else {
    trapdoor = TRUE;
    local_wordsize = given_wordsize;
    if(given_wordsize != 0) {
      if(given_wordsize != BpW) {
	type_size[I] = type_size[R] = type_size[L] = (BYTE)given_wordsize;
	type_size[D] = type_size[C] = (BYTE)(2*given_wordsize);
	type_size[Z] = (BYTE)(4*given_wordsize);
      }
    }
  }

				/* Cray pointer size is set separately */
  if(ptr_trapdoor) {
    if(given_ptrsize != local_ptrsize) {
      (void)fprintf(stderr,
	      "\nSorry-Cannot change pointer size after processing starts");
    }
    given_ptrsize = local_ptrsize;
  }
  else {
    ptr_trapdoor = TRUE;
    local_ptrsize = given_ptrsize;
  }
}


	/* this routine propagates type in binary expressions */

void
#if HAVE_STDC
binexpr_type(Token *term1, Token *op, Token *term2, Token *result)
#else /* K&R style */
binexpr_type(term1,op,term2,result)
	Token *term1, *op, *term2, *result;
#endif /* HAVE_STDC */
{
    int	opclass = op->tclass,
	type1 = datatype_of(term1->TOK_type),
	type2 = datatype_of(term2->TOK_type),
	result_type;
    kind_t kind1 = term1->kind,
	   kind2 = term2->kind;
    long
	size1 = term1->size,
	size2 = term2->size,
        result_size;
    int I_logop_I=FALSE;		/* for f90_mixed_type warning */

    if( ! is_computational_type(type1) ) {
      if( misc_warn ) {
		syntax_error(term1->line_num,term1->col_num,
			"numeric or character quantity expected:");
		report_type(term1);
      }
      result_type = E;
    }
    else if( ! is_computational_type(type2) ) {
      if( misc_warn ) {
		syntax_error(term2->line_num,term2->col_num,
			"numeric or character quantity expected:");
		report_type(term2);
      }
      result_type = E;
    }
    else {
	switch(opclass) {
				/* arithmetic operators: use lookup table */
	    case '+':
	    case '-':
	    case '*':
	    case '/':
	    case tok_power:
		result_type = (unsigned)arith_expr_type[type1][type2];
		break;

				/* relational operators: use lookup table */
 	    case tok_relop:
		result_type = (unsigned)rel_expr_type[type1][type2];
		break;

				/*  logical operators: operands should be
				    logical, but allow integers with a
				    warning. */
	    case tok_AND:
	    case tok_OR:
	    case tok_EQV:
	    case tok_NEQV:
		if(type1 == L && type2 == L)
		    result_type = L;
		else if(type1 == I && type2 == I) {
		    result_type = W+I;
		    I_logop_I = TRUE;
		}
		else
		    result_type = E;
		break;

				/*  // operator: operands must be strings */
	    case tok_concat:
		if(type1 == S && type2 == S)
		    result_type = S;
		else
		    result_type = E;
		break;

			/* Intrinsic function argument list: no promotion
			   across type categories.  Accept matching type
			   categories: size match will be checked later. */
	    case ',': /* COMMA_LIST */
		if( type_category[type1] != type_category[type2] )
		  result_type = E;
		else if(type1 == S)
		  result_type = S;
		else
		  result_type = (unsigned)arith_expr_type[type1][type2];
		break;

	    case tok_defined_op:
	        warning(op->line_num,op->col_num,
		      "defined operators not yet supported: type assumed same as 1st operand");
		result_type = type1; /* punt */
		break;

	    default:
		oops_message(OOPS_NONFATAL,
			     op->line_num,op->col_num,
			     "operator unknown: type not propagated");
		result_type = type1;
		break;
	}

	if( (type1 != E && type2 != E) ) {
	    if( result_type == E) {
	      if(COMMA_LIST) {
		syntax_error(op->line_num,op->col_num,
			     "type mismatch between");
		if( AC_LIST )
			msg_tail("array constructor elements");
		else
			msg_tail("intrinsic function arguments:");
		report_mismatch(term1,op,term2);
	      }
	      else if( misc_warn ) {
		syntax_error(op->line_num,op->col_num,
			     "operands cannot be combined in expression:");
		report_mismatch(term1,op,term2);
	      }
	    }
	    else if(result_type >= W) {	/* W result */
				/* F90 warning suppressed for numeric exprs */
	      if(f77_mixed_expr ||
		 (f90_mixed_expr && ((type1>=L || type2>=L) || I_logop_I)) ) {
		nonstandard(op->line_num,op->col_num,f90_mixed_expr,0);
		msg_tail(": incompatible type combination in expression:");
		report_mismatch(term1,op,term2);
	      }
	      result_type -= W;
	    }
				/* Obscure standard rule 6.2.2. We have to look
				   for IN_ASSIGN flag in any of 3 places, since
				   it gets prematurely turned off in
				   fun_or_substr_handle production if one of
				   operands is a substring expression.  */
	    else if( f77_mixed_expr &&
		     opclass == tok_concat &&
		     !is_true(IN_ASSIGN,
			 (term1->TOK_flags|op->TOK_flags|term2->TOK_flags)) &&
		 ((size1==size_ADJUSTABLE && !is_true(CONST_EXPR,term1->TOK_flags))
	       || (size2==size_ADJUSTABLE && !is_true(CONST_EXPR,term2->TOK_flags))) ) {
		nonstandard(op->line_num,op->col_num,0,0);
		msg_tail(": adjustable size cannot be concatenated here");
	    }

	    /* propagate kind parameter */
	    if (kind1 == kind2) {
		result->kind = kind1;
	    }
	    /* only do kind checking for expressions with dissimilar 
	     * declared kinds */
	    else if( !( kind_is_default(kind1) &&
		        kind_is_default(kind2) ) ) {
		if( port_concrete_kind &&
		    type1 != S && /* concat of mixed kinds is syntax error below */
			( (kind_type(kind1) == type_UNDECL &&
			   kind_type(kind2) != type_UNDECL) ||
			  (kind_type(kind2) == type_UNDECL &&
			   kind_type(kind1) != type_UNDECL) )
		  ) {
		    nonportable(op->line_num,op->col_num,
			    "mixes selected and concrete kinds:");
		    report_kind(kind1);
		    report_type(term1);
		    msg_tail("and");
		    report_kind(kind2);
		    report_type(term2);
		}

		/* perform kind propagation in expression */
		if (type1 == type2) {
		    /* for INTEGER check decimal exponent range */
		    if (type1 == I) {
			result->kind = 
			    ( kind_range(kind1) > kind_range(kind2) ) ?
			    kind1 : kind2;
		    }

		    /* for REAL and COMPLEX check decimal precision */
		    if (type1 == R || type1 == D || type1 == Z) {
			/* QUAD is type_REAL with its own default
			 * kind.   Complex has kind of its real components.
			 */
			if( (kind1 == kind_DEFAULT_QUAD &&
			     (kind2 == kind_DEFAULT_REAL ||
			      kind2 == kind_DEFAULT_DP )) ||
			    (kind2 == kind_DEFAULT_QUAD &&
			     (kind1 == kind_DEFAULT_REAL ||
			      kind1 == kind_DEFAULT_DP)) ) {
			    /* quad with default real or dp is quad */
			    result->kind = kind_DEFAULT_QUAD;
			}
			else if( kind1 == kind_DEFAULT_QUAD ||
				 kind2 == kind_DEFAULT_QUAD) {
			    if( port_mixed_kind ) {
				nonportable(op->line_num,op->col_num,
					"operation mixes default and non-default kinds");
				report_kind(kind1);
				report_type(term1);
				msg_tail("and");
				report_kind(kind2);
				report_type(term2);
			    }
			    result->kind = kind_DEFAULT_QUAD; /* punt */
			}
			else {		/* take the higher precision */
			    result->kind = 
				( kind_precision(kind1) > 
				  kind_precision(kind2) ) ?
				kind1 : kind2;
			}
		    }

		    /* logical operators yield default logical kind */
		    if (type1 == type_LOGICAL) {
			result->kind = default_kind(type_LOGICAL);
		    }

		    /* for CHARACTER, the kinds must match in a
		     * concatenation operation 
		     */
		    if (type1 == S && opclass == tok_concat) {
		        /* expression gets kind of the first expression */
			if (kind1 == kind2) {
			    result->kind = kind1;
			}
			else {
			    syntax_error(op->line_num,op->col_num,
				    "kind mismatch in concatenation");
			    report_kind(kind1);
			    msg_tail("and");
			    report_kind(kind2);
			}
		    }
		}
		/* if types are not the same, expression gets kind of
		 * the result type
		 */
		else {
		    /* if one is integer type then result will get kind
		     * of the non-integer type which is the result_type
		     */
		    if (type1 == I || type2 == I) {
			result->kind = ( type1 == result_type ) ?
			    kind1 : kind2;
		    }

		    /* if the types are either real or complex then
		     * result will get kind of the type with greater
		     * precision
		     */
		    else {
			result->kind = ( kind_precision(kind1) >
				         kind_precision(kind2) ) ?
			    kind1 : kind2;
		    }
		}
	    }
	    /* kinds were never defined for one or both expressions
	     * then set to kind of result type for safety */
	    else {
		if( port_mixed_kind &&
		    ( kind1 != default_kind(type1) && 
		      kind2 != default_kind(type2) ) &&
		    ( ( type1 == type_DP && 
		        (type2 == type_REAL && kind2 < 0) ) ||
		      ( type2 == type_DP &&
		        (type1 == type_REAL && kind1 < 0) ) ) ) {
		        nonportable(op->line_num,op->col_num,
			    "expression mixes default D.P. and selected real kinds: assuming D.P. more precise");
		}

		result->kind = ( type1 == result_type ) ?
		    kind1 : kind2;
	    }

	}
    }

				/* Figure out the size of result */
    result_size = size_DEFAULT;
    if(result_type != E ) {	/* Error type gets DEFAULT size */

      if(opclass == tok_concat) {	/* string//string yields sum of lengths */
	if(size1 == size_UNKNOWN || size2 == size_UNKNOWN)
	  result_size = size_UNKNOWN;
	else
	  if(size1 == size_ADJUSTABLE || size2 == size_ADJUSTABLE)
	    result_size = size_ADJUSTABLE;
	  else {
	    result_size = size1 + size2;
	    if(port_long_string && result_size > 255)
	    nonportable(op->line_num,op->col_num,
			"character expression length exceeds 255");
	  }

      }
			/* DEFAULT op DEFAULT always yields DEFAULT. So need
			   to handle only explicitly sized expressions,
			   except intrinsic arglists, where no promotion
			   of plain real to dble or plain complex to dcpx,
			   and check for promotions of real types.
			 */
      else if(COMMA_LIST?
	      (type1 != type2 || 
	       (type1 == type2  && is_numeric_type(type1) &&
		(size1 != size_DEFAULT || size2 != size_DEFAULT))) :
	      ((size1 != size_DEFAULT || size2 != size_DEFAULT) ||
	        (trunc_promotion &&
		 is_float_type(type1) && is_float_type(type2))))
     {
				/* Local variables for convenience.
				   N.B. Use tc1/2,ls1/2 for tests,
				   t1/2,s1/2 for assigning result.
				 */
	int t1,t2;	/* sorted types: t1 <= t2. */
	long s1,s2;	/* sizes of t1 and t2. */
	int tc1,tc2;	/* type categories: D->R and Z->C */
	long ls1,ls2;	/* local sizes = declared size else type_size */
	int defsize1,defsize2; /* flags for default size */

				/* Sort so that t1 <= t2 */
	if(type1 <= type2) {
	  t1 = type1; s1 = size1;
	  t2 = type2; s2 = size2;
	}
	else {
	  t1 = type2; s1 = size2;
	  t2 = type1; s2 = size1;
	}
				/* Assign type categories and local sizes */
	tc1 = type_category[t1];
	tc2 = type_category[t2];

	defsize1 = (s1 == size_DEFAULT);
	defsize2 = (s2 == size_DEFAULT);
	ls1 = (defsize1? type_size[t1]: s1);
	ls2 = (defsize2? type_size[t2]: s2);

#ifdef DEBUG_EXPRTYPE
if(debug_latest)
  (void)fprintf(list_fd,"\nt1=%s s1=%ld ls1=%ld t2=%s s2=%ld ls2=%ld",
	  type_name(t1),s1,ls1, type_name(t2), s2, ls2);
#endif
	if(tc1 == tc2) {/* same type category */
				/* Intrins args: size promotion illegal */
	  if(COMMA_LIST && ls1 != ls2) {
	      syntax_error(op->line_num,op->col_num,
			   "precision mismatch in");
	      if( AC_LIST )
		      msg_tail("array constructor");
	      else
		      msg_tail("intrinsic argument list");
	      report_mismatch(term1,op,term2);
	  }
				/* Give -port warning if e.g. plain I+I*2
				   (variables only) */
	  else if(port_mixed_size || local_wordsize==0) {
	    if(defsize1 != defsize2
	        && !is_true(CONST_EXPR,term1->TOK_flags)
	        && !is_true(CONST_EXPR,term2->TOK_flags))
	    {
	      nonportable(op->line_num,op->col_num,
			  COMMA_LIST?(AC_LIST?
				      "array constructor":"intrinsic argument list"):
			  "expr");
	      msg_tail("mixes default and explicit");
	      msg_tail((is_numeric_type(t1)&&is_numeric_type(t2))?
			 "precision":"size");
	      msg_tail("operands:");
	      report_mismatch(term1,op,term2);
	    }
	  }

		/* If same type category, use the larger of the two sizes if
		   both declared.  If only one size declared, use the
		   larger of the declared size and the default size.
		   If result is equal in size to default, use size_DEFAULT.
		*/
	  if(ls1 > ls2) {
	    result_size = s1;
	  }
	  else if(ls2 > ls1) {
	    result_size = s2;
	  }
	  else /*ls1 == ls2*/{
	    if(!defsize1 && !defsize2)
	      result_size = s1;	/* otherwise DEFAULT */
	  }
	}/* end(tc1==tc2) */
	else /* tc1!=tc2 */ {
			/* Differing type categories: only two cases. */

				/* Case 1:  I + R|D|C|Z
				   Result: size of dominant type */
	  if(tc1 == I) {
	    result_size = s2;
	  }
				/* Case 2:  R|D + C|Z
				   Result: larger of C|Z and 2*size of R|D */
	  else {
	    if(ls2 >= 2*ls1)
	      result_size = s2;
	    else
	      result_size = 2*s1; /* 2*size_DEFAULT = 0 is still DEFAULT */
	  }
	}/* end tc1 != tc2 */
				/* change D or Z to default size or else
				   to explicitly sized R or C
				 */
	if(result_type == D || result_type == Z) {
	  if(result_size != size_DEFAULT
	     && result_size != type_size[result_type])
	       result_type = (result_type==D)?R:C;
	     else
	       result_size = size_DEFAULT;
	}

				/* Give -trunc warning if a real or
				   complex type is promoted to double. */
	if(trunc_promotion && !COMMA_LIST && is_float_type(t1) ) {
		  /* First clause checks R+R size agreement */
	  if( (type_category[result_type] == R && ls1 != ls2)
		     /* Second clause checks R+C and C+C */
	     || (type_category[result_type] == C &&
		 (type_category[t1] == R? ls2 != 2*ls1 : ls2 != ls1)) ){
	    warning(op->line_num,op->col_num,
		    "promotion may not give desired precision:");
	    report_mismatch(term1,op,term2);
	  }

	  /* For comparisons we have R==R, C==C, or R==C. */
	  if( opclass == tok_relop &&
	      (type_category[t1] == type_category[t2]? ls2 != ls1:ls2 != 2*ls1) ) {
	    warning(op->line_num,op->col_num,
		    "comparison mixes terms of different precision:");
	    report_mismatch(term1,op,term2);
	  }
	}

      }/*end if(non-DEFAULT sizes)*/

	/* Result of compare gets DEFAULT size.  However, we want to
	   go thru the above code for relops in case of type/size
	   mismatches that should be reported.  Here for relops we
	   replace any result_size that got set above by DEFAULT size.
	*/
      if( opclass == tok_relop) {
	   result_size = size_DEFAULT;
      }
    }/*end if(result_type != E)*/

#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
(void)fprintf(list_fd,"\nsize of %s %c",sized_typename(type1,size1),
	ispunct(opclass)?opclass:'~');
(void)fprintf(list_fd," %s = ",sized_typename(type2,size2));
(void)fprintf(list_fd,"%s",sized_typename(result_type,result_size));
}
#endif

    result->TOK_type = type_pack(class_VAR, result_type);
    result->TOK_flags = 0;	/* clear all flags */
    result->size = result_size;


		/* Keep track of constant expressions */
    if( is_true(CONST_EXPR,term1->TOK_flags)
	 && is_true(CONST_EXPR,term2->TOK_flags)
         && !(opclass==tok_power && type2!=I) ) { /* exclude **REAL */
		make_true(CONST_EXPR,result->TOK_flags);
    }

		/* Parameter expressions are like constant exprs
		   except we bend the rules to allow intrinsic functions
		   and **REAL */
    if( is_true(PARAMETER_EXPR,term1->TOK_flags)
	 && is_true(PARAMETER_EXPR,term2->TOK_flags) ) {
		make_true(PARAMETER_EXPR,result->TOK_flags);
    }

			/* Keep track of dimension bound expressions */
    if( is_true(DIM_BOUND_EXPR,term1->TOK_flags)
	&& is_true(DIM_BOUND_EXPR,term2->TOK_flags) ) {
      make_true(DIM_BOUND_EXPR,result->TOK_flags);
    }

    if( is_true(EVALUATED_EXPR,term1->TOK_flags)
	 && is_true(EVALUATED_EXPR,term2->TOK_flags) ) {
		make_true(EVALUATED_EXPR,result->TOK_flags);
    }


#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,"\nconst param eval: (%d %d %d) %s (%d %d %d) = (%d %d %d)",
is_true(CONST_EXPR,term1->TOK_flags),
is_true(PARAMETER_EXPR,term1->TOK_flags),
is_true(EVALUATED_EXPR,term1->TOK_flags),

op->src_text,

is_true(CONST_EXPR,term2->TOK_flags),
is_true(PARAMETER_EXPR,term2->TOK_flags),
is_true(EVALUATED_EXPR,term2->TOK_flags),

is_true(CONST_EXPR,result->TOK_flags),
is_true(PARAMETER_EXPR,result->TOK_flags),
is_true(EVALUATED_EXPR,result->TOK_flags));
#endif

  if(! COMMA_LIST) {		/* Remaining steps only applicable to exprs */

		/* Remember if integer division was used */
    if(result_type == type_INTEGER &&
	   (opclass == '/' ||
	    (is_true(INT_QUOTIENT_EXPR,term1->TOK_flags) ||
	     is_true(INT_QUOTIENT_EXPR,term2->TOK_flags))) ) {
		make_true(INT_QUOTIENT_EXPR,result->TOK_flags);
    }
		/* Issue warning if integer expr involving division is
		   later converted to any real type, or if it is used
		   as an exponent. */
    if( is_true(INT_QUOTIENT_EXPR,term1->TOK_flags)
	|| is_true(INT_QUOTIENT_EXPR,term2->TOK_flags) ) {

	int r=result_type;
	if(r == type_LOGICAL)		/* relational tests are equivalent */
	    r = arith_expr_type[type1][type2];		/* to subtraction */

	if(opclass == tok_power && is_true(INT_QUOTIENT_EXPR,term2->TOK_flags) ) {
	  if(trunc_int_div_exponent) {
	    warning(op->line_num,op->col_num,
		    "integer quotient expr");
	    msg_expr_tree(term2);
	    msg_tail("used in exponent");
	  }
	  if( ! is_true(INT_QUOTIENT_EXPR,term1->TOK_flags) )
		make_false(INT_QUOTIENT_EXPR,result->TOK_flags);
	}
	else if( r == type_REAL || r == type_DP || r == type_COMPLEX) {
	  if(trunc_int_div_real) {
	    warning(op->line_num,op->col_num,
		    "integer quotient expr");
	    msg_expr_tree(is_true(INT_QUOTIENT_EXPR,term1->TOK_flags)?
				  term1:term2);
	    msg_tail(" converted to real");
	  }
	}
    }

    /* Arrays combined in expr must have conformable shapes */

    check_array_conformance(term1,term2,result,op->line_num,op->col_num);

			/* If either term is an identifier, set use flag */
    if(is_true(ID_EXPR,term1->TOK_flags))
	use_variable(term1);
    if(is_true(ID_EXPR,term2->TOK_flags))
	use_variable(term2);

		/* Propagate the value of integer constant expressions */

    if(is_true(EVALUATED_EXPR,result->TOK_flags)) {
	if(result_type == type_INTEGER) {	/* Only ints propagated */
	  int a = int_expr_value(term1),
	      b = int_expr_value(term2),
	      c;
	  switch(opclass) {
	    case '+': c = a+b; break;
	    case '-': c = a-b; break;
	    case '*': c = a*b; break;
	    case '/': if(b == 0) {
		        if(misc_warn) {
			  syntax_error(term2->line_num,term2->col_num,
				"division by zero attempted");
			}
			c = 0;
		      }
		      else {
			c = a/b;
		      }
		      break;
	    case tok_power: c = int_power(a,b); break;
	    case tok_AND: c = a&b; break;
	    case tok_OR: c = a|b; break;
	    case tok_EQV: c = ~(a^b); break;
	    case tok_NEQV: c = a^b; break;
	    case tok_defined_op:
	        warning(op->line_num,op->col_num,
		      "defined operator result value not calculated: 0 assumed");
		c = 0;
		break;
	    default:
	      oops_message(OOPS_NONFATAL,
			   op->line_num,op->col_num,
			   "invalid int expr operator");
			c = 0; break;
	  }

	  make_true(EVALUATED_EXPR,result->TOK_flags);
	  result->value.integer = c;	/* Result goes into token value */

				/* Integer division (including i**neg)
				   that yields 0 is suspicious.  */
	  if(trunc_int_div_zero)
	    if(c==0 && (opclass=='/' || opclass==tok_power)) {
	      warning(op->line_num,op->col_num,
	    		"integer const expr yields result of 0");
	    }
	}
      }
				/* Also nonconstant**neg is 0 unless
				   nonconstant=1 */
      else if(trunc_int_neg_power)
	if(result_type == type_INTEGER && opclass == tok_power
	      && is_true(EVALUATED_EXPR,term2->TOK_flags)
	      && int_expr_value(term2) < 0) {
	  warning(op->line_num,op->col_num,
		  "integer to negative power usually yields 0");
	}
  }/* end if !COMMA_LIST */
}/*binexpr_type*/


	/* this routine propagates type in unary expressions */

void
#if HAVE_STDC
unexpr_type(Token *op, Token *term1, Token *result)
#else /* K&R style */
unexpr_type(op,term1,result)
	Token *term1, *op, *result;
#endif /* HAVE_STDC */
{
   int	opclass = op->tclass,
	type1 = datatype_of(term1->TOK_type),
	result_type;

    if( ! is_computational_type(type1) ) {
      if( misc_warn ) {
		syntax_error(term1->line_num,term1->col_num,
			"numeric quantity expected:");
		report_type(term1);
      }
      result_type = E;
    }
    else {
	switch(opclass) {
			/* arith operators: use diagonal of lookup table */
	    case '+':
	    case '-':
		result_type = arith_expr_type[type1][type1];
		break;

				/*  NOT: operand should be
				    logical, but allow integers with a
				    warning. */
	    case tok_NOT:
		if(type1 == L)
		    result_type = L;
		else if(type1 == I)
		    result_type = W+I;
		else
		    result_type = E;
		break;

	    case tok_defined_op:
	        warning(op->line_num,op->col_num,
		      "defined operators not yet supported: type assumed same as operand");
		result_type = type1; /* punt */
		break;

	    default:
		oops_message(OOPS_NONFATAL,
			     op->line_num,op->col_num,
			     "unary operator type not propagated");
		result_type = type1;
		break;
	}

	if( type1 != E ) {
	    if( result_type == E) {
	      if( misc_warn ) {
		  syntax_error(op->line_num,op->col_num,
			"expression incompatible with operator:");
		msg_tail(op->src_text);
		msg_tail("used with");
		report_type(term1);
	      }
	    }
	    else if(result_type >= W) {
	      if(f77_mixed_expr || f90_mixed_expr) {
		nonstandard(op->line_num,op->col_num,f90_mixed_expr,0);
		msg_tail(": incompatible type used with operator:");
		msg_tail(op->src_text);
		msg_tail("used with");
		report_type(term1);
	      }
	      result_type -= W;
	    }
	}
    }

    result->TOK_type = type_pack(class_VAR, result_type);
    /* unary expressions get kind of the operand */
    result->kind = term1->kind;
    result->TOK_flags = 0;	/* clear all flags */
    result->size = term1->size;	/* result is same size as operand */

		/* Keep track of constant expressions */
    copy_flag(CONST_EXPR,result->TOK_flags,term1->TOK_flags);
    copy_flag(PARAMETER_EXPR,result->TOK_flags,term1->TOK_flags);
    copy_flag(DIM_BOUND_EXPR,result->TOK_flags,term1->TOK_flags);

	    /* Propagate array property */
    copy_flag(ARRAY_EXPR,result->TOK_flags,term1->TOK_flags);
    result->array_dim = term1->array_dim;

		/* Remember if integer division was used */
    if(result_type == type_INTEGER)
	    copy_flag(INT_QUOTIENT_EXPR,result->TOK_flags,term1->TOK_flags);

    if(is_true(ID_EXPR,term1->TOK_flags))
	use_variable(term1);

		/* Propagate the value of integer constant expressions */
    if(is_true(EVALUATED_EXPR,term1->TOK_flags)) {
	if(result_type == type_INTEGER) {	/* Only ints propagated */
	  int a = int_expr_value(term1),
	      c;
	  switch(opclass) {
	    case '+': c = a; break;
	    case '-': c = -a; break;
	    case tok_NOT: c = ~a; break;
	    case tok_defined_op:
	        warning(op->line_num,op->col_num,
		      "defined operator result value not calculated: 0 assumed");
		c = 0;
		break;

	    default: oops_message(OOPS_NONFATAL,
			     op->line_num,op->col_num,
			     "invalid int expr operator");
			c = 0; break;
	  }
	  make_true(EVALUATED_EXPR,result->TOK_flags);
	  result->value.integer = c;	/* Result goes into token value */
	}
    }
}

/* Routine to check conformance of arrays in a binary expression or
   assignment.
	   OK:
	     array = scalar
	     array = array (matching shape)
	     array op scalar
	     scalar op array
	     array op array (matching shape)
	   Not OK:
	     scalar = array
	     array = array (mismatching shape)
	     array op array (mismatching shape)

   Note however that scalar = array is already flagged in grammar at
   assignment_stmt production, so we only need to catch shape mismatch
   of array =/op array.

   If size of either array is unknown, size test passes, but
   rank is still checked.
 */

PRIVATE
void check_array_conformance(Token *term1, Token *term2, Token *result,
     int op_line_num, int op_col_num)
{
    int conformance_ok = TRUE;

    if( is_true(ARRAY_EXPR, term1->TOK_flags) &&
	is_true(ARRAY_EXPR, term2->TOK_flags) ) {
      long array1_size = array_size(term1->array_dim);
      long array2_size = array_size(term2->array_dim);
      int array1_dims = array_dims(term1->array_dim);
      int array2_dims = array_dims(term2->array_dim);

			/* Only check if both sides are nonscalar. */
      if( array1_dims != 0 && array2_dims != 0 ) {

				/* First check rank match */
	if( array1_dims != array2_dims ) {
	  syntax_error(op_line_num,op_col_num,
		       "array rank mismatch:");
	  msg_tail(ulongtostr((unsigned long)array1_dims));
	  msg_tail("and");
	  msg_tail(ulongtostr((unsigned long)array2_dims));
	  conformance_ok = FALSE;
	}

	else {		/* rank match OK: check shape */
	    /* When we support array shapes, each dimension size match
	       should be checked.  For now just check total size, if known.
	     */
	  if( !array_size_is_unknown(term1->array_dim) &&
	      !array_size_is_unknown(term2->array_dim) ) {
	    if( array1_size != array2_size ) {
	      syntax_error(op_line_num,op_col_num,
			   "array size mismatch:");
	      msg_tail(ulongtostr((unsigned long)array1_size));
	      msg_tail("and");
	      msg_tail(ulongtostr((unsigned long)array2_size));
	      conformance_ok = FALSE;
	    }
	  }
	}
      }
    }
    if( result != NULL ) {	/* evaluate result array_dim  */
      if( conformance_ok ) {
	if( is_true(ARRAY_EXPR, term1->TOK_flags) ) {
	  make_true(ARRAY_EXPR, result->TOK_flags);
	  result->array_dim = term1->array_dim;
	}
	else {
	  copy_flag(ARRAY_EXPR,result->TOK_flags,term2->TOK_flags);
	  result->array_dim = term2->array_dim;
	}
      }
      else {			/* to avoid error cascades, reset to scalar */
	make_false(ARRAY_EXPR, result->TOK_flags);
	result->array_dim = array_dim_info(0,0);
      }
    }
      
}

/* Routine to check conformance of elemental procedure args.  It
   updates the array_dim field of the args handle to be the conformed
   arrayness of the list.
 */
void
check_elemental_args(Token *id, Token *args)
{
  Token *curr_arg, *next_arg;

  if( args == NULL || (curr_arg = args->next_token) == NULL )
    return;			/* no args given */

			/* Array dim info is stored in args handle.
			   Initialize with info from first arg.
			 */
  args->array_dim = curr_arg->array_dim;

  while( (next_arg = curr_arg->next_token) != NULL ) {
    /* Check array conformance of this pair of args.  Routine updates
       array_dim info stored in args handle.
     */
    check_array_conformance(curr_arg, next_arg, args,
		  next_arg->line_num, next_arg->col_num);
    curr_arg = next_arg;
  }
}

	/* This routine checks type and size match in assignment statements
	   and in parameter assignments. In assignments, the kind parameter
	   of the RHS expression is truncated to the kind parameter of the
	   LHS primary. */


void
#if HAVE_STDC
assignment_stmt_type(Token *term1, Token *equals, Token *term2)
#else /* K&R style */
assignment_stmt_type(term1,equals,term2)
	Token *term1, *equals, *term2;
#endif /* HAVE_STDC */
{
    int type1 = datatype_of(term1->TOK_type),
	type2 = datatype_of(term2->TOK_type),
	result_type;
    kind_t kind1 = term1->kind,
	   kind2 = term2->kind;


    /* When called from check_initializer_type, the "equals" operator
       may be '/' for bastard initializers like INTEGER A(3) / 1, 2, 3
       /.  In that case we want the caret in warnings to come under
       the assigned value.  For '=' and '=>' of F90 initializers and
       true assignment statements the caret should come under the
       assignment operator.
     */
    LINENO_t equals_line_num;
    COLNO_t equals_col_num;
    if( equals->tclass == '/' ) {
      equals_line_num = term2->line_num;
      equals_col_num = term2->col_num;
    }
    else {
      equals_line_num = equals->line_num;
      equals_col_num = equals->col_num;
    }

    if(type2 == type_GENERIC) {	/* takes type from assignee */
      type2 = type1;
      /* (size is propagated below) */
      kind2 = kind1;
    }

    if( is_derived_type(type1) || is_derived_type(type2) ) {
      if( (is_derived_type(type1) != is_derived_type(type2)) ||
	  ( type1 != type2 ) ) {
	syntax_error(equals_line_num,equals_col_num,
		     "type mismatch:");
	report_type(term2);
	msg_tail("assigned to");
	report_type(term1);
      }
      else {
	  check_array_conformance(term1, term2, (Token*)NULL,
		equals_line_num, equals_col_num);
      }
    }
    else if( ! is_computational_type(type1) ) {
      if( misc_warn ) {
		syntax_error(term1->line_num,term1->col_num,
			"numeric or character quantity expected:");
		report_type(term1);
      }
      result_type = E;
    }
    else if( ! is_computational_type(type2) ) {
      if( misc_warn ) {
		syntax_error(term2->line_num,term2->col_num,
			"numeric or character quantity expected:");
		report_type(term2);
      }
      result_type = E;
    }
    else {
	result_type = (unsigned)assignment_type[type1][type2];

	if( (type1 != E && type2 != E) ) {
	    if( result_type == E) {
	      if( misc_warn ) {
		syntax_error(equals_line_num,equals_col_num,
			"type mismatch:");
		report_type(term2);
		msg_tail("assigned to");
		report_type(term1);
	      }
	    }
	    else {
	      int trunc_warning_given=FALSE;
	      /* warn about kind truncation only if kind was declared
		 for at least one operand.
	       */

	      if ( trunc_kind && kind1 != kind2 &&
		   !( kind_is_default(kind1) &&
		      kind_is_default(kind2) )
		 ) {

		int kind1_report, kind2_report;

		if (type1 == type_INTEGER && type2 == type_INTEGER) {
		  kind1_report = kind_range(kind1);
		  kind2_report = kind_range(kind2);

		  if (kind1_report < kind2_report) {
		    warning(equals_line_num,equals_col_num,
			    "possible integer mismatch:");
		    report_kind_property(kind2_report,"range");
		    msg_tail("assigned to");
		    report_kind_property(kind1_report,"range");
		    msg_tail(": may overflow");
		    trunc_warning_given = TRUE;
		  }
		}
		else if (type1 == type_REAL && type2 == type_REAL) {
		  kind1_report = kind_precision(kind1);
		  kind2_report = kind_precision(kind2);

		  if( port_mixed_kind &&
		      (kind1_report <= 0 || kind2_report <= 0) &&
		      /* special case of quad assigned to real or v-v, will
			 be warned about twice if -trunc=size-pro/demotion
			 or -port=mixed-size is given.
		       */
		      !( (port_mixed_size || trunc_promotion || trunc_size_demotion) &&
			 (kind1 == kind_DEFAULT_QUAD || kind2 == kind_DEFAULT_QUAD) )
		    ) {
		    nonportable(equals_line_num,equals_col_num,
			"assignment mixes kinds:");
		    report_kind_property(kind2_report,"precision");
		    msg_tail("assigned to");
		    report_kind_property(kind1_report,"precision");
		    msg_tail("real");
		    trunc_warning_given = TRUE;
		  }

		  if (kind1_report < kind2_report &&
		      !( (port_mixed_size || trunc_promotion) &&
			  kind1 == kind_DEFAULT_QUAD )
		    ) {
		    warning(equals_line_num,equals_col_num,
			"possible real mismatch:");
		    report_kind_property(kind2_report,"precision");
		    msg_tail("assigned to");
		    report_kind_property(kind1_report,"precision");
		    msg_tail("real: may not give desired precision");
		    trunc_warning_given = TRUE;
		  }
		}

	      }

	      if(!trunc_warning_given) {
		int concrete_with_selected = FALSE;
		int dp_with_selected = FALSE;

		/* Warn under -port=concrete-kind if concrete numeric
		 * or logical kind is mixed with selected kind
		 */
		if( port_concrete_kind ) {
		  concrete_with_selected = (
		    type1 != S && /* mixed char kinds reported below as syntax error */
		    ( kind1 != default_kind(type1) ||
		      kind2 != default_kind(type2) ) &&
		    ( (kind1 >= 0 && kind2 < 0) ||
		      (kind2 >= 0 && kind1 < 0) )
		    );
		}

	      /* Warn under -port=mixed-kind about mixing default
	       * double precision and selected real kind.  Note that
	       * type_DP can only be default DP kind.
	       */
		if( port_mixed_kind ) {
		  dp_with_selected = (
		    ( (kind1 != default_kind(type1) && kind1 != kind_DEFAULT_QUAD) ||
		      (kind2 != default_kind(type2) && kind2 != kind_DEFAULT_QUAD) ) &&
		    ( ( type1 == type_DP && 
			(type2 == type_REAL && kind2 < 0) ) ||
		      ( type2 == type_DP &&
			(type1 == type_REAL && kind1 < 0) ) ) 
		    );
		}

		if( concrete_with_selected || dp_with_selected ) {
		  nonportable(equals_line_num,equals_col_num,(char *)NULL);
		  report_kind(kind2);
		  report_type(term2);

		  msg_tail("assigned to");

		  report_kind(kind1);
		  report_type(term1);

		  msg_tail(": precision may differ");
		}
	      }

	      check_array_conformance(term1, term2,(Token *)NULL,
		    equals_line_num, equals_col_num);
		
	      if(result_type >= W) {		/* W result */
		if(f77_mixed_expr || f90_mixed_expr) {
		  nonstandard(equals_line_num,equals_col_num,f90_mixed_expr,0);
		  msg_tail(": incompatible type combination:");
		  report_type(term2);
		  msg_tail("assigned to");
		  report_type(term1);
		}
		result_type -= W;
	      }

			/* Watch for truncation to lower precision type */
	      if(trunc_precision ||
		 port_mixed_size || local_wordsize==0) {
		long size1 = term1->size;
		long size2 = term2->size;
		int type_trunc=FALSE, /* flags for kind of truncation */
		    size_trunc=FALSE,
		    mixed_size=FALSE,
		    promotion=FALSE,
		    trunc_warn,mixed_warn;

		if( datatype_of(term2->TOK_type) == type_GENERIC )
		  size2 = size1;

		if(size1 == size_DEFAULT && size2 == size_DEFAULT) {
		  type_trunc = ( is_numeric_type(type1) &&
				 is_numeric_type(type2) &&
				(type1 < type2 ||
					/* C = D truncates precision of D */
				(type1 == C && type2 == D)) );

				/* Watch for promotions also */
		  if(type_category[type2] == R) {
		    if(type_category[type1] == R) /* R|D = R|D */
		      promotion = (type1 > type2);
		    else if(type_category[type1] == C) /* C|Z = R|D */
		      promotion =
			((int)type_size[type1] > 2*(int)type_size[type2]);
		  }
		  else if(type_category[type2] == C) /* any = C|Z */
		    promotion = (type1 > type2);
		}
		else if(type1 == S) { /* character strings */
		  if(size1>0 && size2>0) /* ignore ADJUSTABLE and UNKNOWN */
		    size_trunc = size1 < size2;
		} else {
		  int tc1,tc2;/* type categories: D->R, Z->C, H->I */
		  int ls1,ls2;/* local sizes */

				/* Assign type categories and local sizes */
		  tc1 = type_category[type1];
		  tc2 = type_category[type2];
		  ls1 = size1; if(ls1 == size_DEFAULT)  ls1 = type_size[type1];
		  ls2 = size2; if(ls2 == size_DEFAULT)  ls2 = type_size[type2];

				/* type truncation: any numeric type category
				   to a lower category. */
		  type_trunc = ( /***is_numeric_type(type1) &&
				 is_numeric_type(type2) &&***/
				 tc1 < tc2 );

				/* size truncation: assigned to smaller
				   local size.  For C = R correct test is
				   Csize < 2*Rsize */
		  if(tc1 == C && tc2 == R) {
		    size_trunc = (ls1 < ls2*2);
		    promotion = (ls1 > ls2*2);
		  }
		  else {
				/* Suppress size truncation warning if rhs
				   is a literal constant that is sure to fit.
				   For logicals this is always the case; for
				   integers we use a suitable threshold.
				 */
		    if( (size_trunc = (ls1 < ls2)) &&
			is_true(LIT_CONST,term2->TOK_flags) ){
			switch(tc2) {
			  case L:
			      size_trunc = FALSE;
			      break;
			  case I:
			      if( term2->value.integer <= SMALL_INT_VALUE )
				  size_trunc = FALSE;
			      break;
			}
		    }
		    promotion = ((tc2 == R || tc2 == C) && (ls1 > ls2));
		  }
				/* mixed size: default size assigned to
				   declared size of like type category
				   or vice-versa. -port only, and superseded
				   by truncation warning if any. */
		  mixed_size = (tc1 == tc2) &&
			   (size1==size_DEFAULT ||
			   (size2==size_DEFAULT &&
			    !is_true(CONST_EXPR,term2->TOK_flags)));

		}

			/* Under -trunc, report type truncation or size
			   truncation.  Say "possibly" if -nowordsize.
			   Also report promotions under -trunc.
			   If no truncation warning given and under -port,
			   report mixed assignment */
#ifdef DEBUG_EXPRTYPE
#define TorF(x) ((x)?"":"no")
if(debug_latest) {
(void)fprintf(list_fd,"\nassign %s =",sized_typename(type1,size1));
(void)fprintf(list_fd," %s : ",sized_typename(type2,size2));
(void)fprintf(list_fd,"%s type %s size %s mixed",
	TorF(type_trunc),
	TorF(size_trunc),
	TorF(mixed_size));
}
#endif
		trunc_warn = (trunc_promotion && promotion) ||
			     (trunc_type_demotion && type_trunc) ||
			     (trunc_size_demotion && size_trunc);
		mixed_warn = ((port_mixed_size || local_wordsize==0) &&
				mixed_size);
		/* don't report tailored messaged for quad types which
		 * have type_REAL and cannot be distinguished as such */
		if (trunc_warn) {
		  warning(equals_line_num,equals_col_num,"");
		  if (!kind_is_default(kind2)) {
		    report_kind(kind2);
		  }
		  report_type(term2);
		  if(trunc_warn && !type_trunc && mixed_size
		       && local_wordsize == 0)
		    msg_tail("possibly");
		  if( ( (kind_is_default(kind1) &&
			 kind_is_default(kind2)) ||
			(size_trunc && type1==S) ) /* char size_trunc is for sure */
		    ) {
		      if(promotion)
			  msg_tail("promoted to");
		      else
			  msg_tail("truncated to");
		  }
		  else {
		      msg_tail("assigned to");
		  }
		  if ( !kind_is_default(kind1) ) {
		    report_kind(kind1);
		  }
		  report_type(term1);
		  if( (type1==R || type1==C || type1==D || type1==Z) &&
		      (promotion || (kind1 != default_kind(type1))) )
		    msg_tail(": may not give desired precision");
		}
		else if(mixed_warn) {
		  nonportable(equals_line_num,equals_col_num,
		    "mixed default and explicit");
		  msg_tail((is_numeric_type(type1)&&is_numeric_type(type2))?
			 "precision":"size");
		  msg_tail("items:");
		  report_type(term2);
		  msg_tail("assigned to");
		  report_type(term1);
		}
	      }

		/* Issue warning if integer expr involving division is
		   later converted to any real type. */
    if(trunc_int_div_real) {
      if( is_true(INT_QUOTIENT_EXPR,term2->TOK_flags) ) {

	int r=result_type;

	if( r == type_REAL || r == type_DP || r == type_COMPLEX) {
	    warning(equals_line_num,equals_col_num,
			"integer quotient expr");
	    msg_expr_tree(term2);
	    msg_tail(" converted to real");
	}
      }
    }

	    }/*end else (result_type != E)*/
	}/*end if (type1,type2 != E)*/
    }/*end else (is_computational_type(type2))*/



/**** handling for pointer assignment ***/
    if (equals->tclass == tok_rightarrow){
      if( is_true(POINTER_EXPR,term2->TOK_flags) || is_true(TARGET_EXPR,term2->TOK_flags) ) {
	if( is_true(ID_EXPR,term2->TOK_flags ) )
	  use_target(term2);	/* assignment to pointer is not use_variable */
      }
      else  {
	syntax_error(term2->line_num,term2->col_num,
		     "pointer/target attribute expected on RHS quantity");
	msg_expr_tree(term2);
      }

      if( is_true(POINTER_EXPR,term1->TOK_flags) ) {
	use_pointer_lvalue(term1,term2);
      }
      else {
	syntax_error(term1->line_num,term1->col_num,
		     "pointer attribute expected on lvalue");
	msg_expr_tree(term1);
      }
    }
    else {
/**** handling for non-pointer assignment ***/
      if(is_true(ID_EXPR,term2->TOK_flags))
	use_variable(term2);

      use_lvalue(term1);
    }

}

void
check_initializer_type(Token *assignee_list, Token *equals, Token *expr_list)
{
    Token *t;
    if( expr_list->next_token == (Token*)NULL ) {
	t = expr_list;		/* simple token, not a list */
    }
    else {
				/* token lists are built in reverse, so
				   restore to order in source statement */
	t = expr_list->next_token = reverse_tokenlist(expr_list->next_token);
    }

				/* Go thru list, checking match.
				   At this time, assignee can only be a single
				   variable
				 */
    while( t!=NULL ) {
	assignment_stmt_type(assignee_list,equals,t);
	t = t->next_token;
    }
}

	/* Make an expression-token for a function invocation */

void
#if HAVE_STDC
func_ref_expr(Token *id, Token *args, Token *result)
#else /* K&R style */
func_ref_expr(id,args,result)
	Token *id,*args,*result;
#endif /* HAVE_STDC */
{
	Lsymtab *symt;
	IntrinsInfo *defn;
	int rettype, retsize;
	/* initialize to zero to skip kind checking */
	kind_t retkind;

	symt = hashtab[id->value.integer].loc_symtab;

	if( symt->intrinsic ) {
	    defn = symt->info.intrins_info;
			/* Intrinsic functions: type stored in info field */
	    rettype = defn->result_type;
	    retsize = size_DEFAULT;
	    if( defn->intrins_flags & I_QUAD ) { /* Intrinsic result is quad */
				/* These are either R*16 or X*32 */
	      retsize = ((rettype==type_QUAD)? size_QUAD: size_CQUAD);
	      retkind = kind_DEFAULT_QUAD; /* same kind for real or complex */
	    }

		/* Generic Intrinsic functions: use propagated arg type */
	    else if(rettype == type_GENERIC) {
		if(args->next_token == NULL) {
		    /* NULL with no arg takes type of assignee, so
		       leave its type as from table, type_GENERIC, and
		       type will be set in assignment_stmt_type.  For
		       other generic intrinsics, having no args is an
		       error so make them type_UNDECL to raise an
		       error.
		     */
		  if( INTRINS_ID(defn->intrins_flags) != I_NULL ) {
		    rettype = type_UNDECL;
		  }
		  retsize = size_DEFAULT;
		}
		else {
		  rettype = args->TOK_type;
		  retsize = args->size;
		}
			/* special case: REAL(integer|[d]real) ->  real */
		if((INTRINS_ID(defn->intrins_flags) == I_SP_R) &&
		   (rettype != type_COMPLEX) && (rettype != type_DCOMPLEX)) {
			rettype = type_REAL;
			retsize = size_DEFAULT;
		}

			/* special cases: */
			/*       ABS([d]complex) -> [d]real */
			/*      IMAG([d]complex) -> [d]real */
			/*      REAL([d]complex) -> [d]real */
		if(rettype == type_COMPLEX && (defn->intrins_flags&I_C_TO_R)) {
			rettype = type_REAL;
			retsize = retsize/2;
		}
		if(rettype == type_DCOMPLEX &&(defn->intrins_flags&I_C_TO_R)) {
			rettype = type_DP;
			retsize = size_DEFAULT;
		}
		/* If function takes an optional kind arg, use value gotten
		   in check_intrins_args */
		if(defn->intrins_flags&I_OK &&
		   args->kind != kind_DEFAULT_UNKNOWN)
		  retkind = args->kind;
		else
		  retkind = default_kind(rettype);
	    }/* end type_GENERIC */
	      else {		/* non-generic */

				/* special case: CHAR(code): size=1 */
		if(INTRINS_ID(defn->intrins_flags) == I_CHAR) {
		  retsize = 1;
		}
		if(defn->intrins_flags&I_OK &&
		   args->kind != kind_DEFAULT_UNKNOWN)
		  retkind = args->kind;
		else
		  retkind = default_kind(rettype);
	      }
	}
	else {			/* non-intrinsic */
	    rettype = get_type(symt);
	    retsize = get_size(symt,rettype);
	    retkind = get_kind(symt,rettype);
	}
		/* referencing function makes it no longer a class_SUBPROGRAM
		   but an expression. */
#ifndef TOK_type
	result->tclass = id->tclass;
#endif
	result->tsubclass = 0;
	result->TOK_type = type_pack(class_VAR,rettype);
#ifndef TOK_flags
	result->TOK_flags = 0;	/* clear all flags */
#endif
	result->size = retsize;
	result->kind = retkind;
	result->next_token = (Token *)NULL;

	/* transfer pointer/target attributes to token */
      if(symt->pointer) {
	make_true(POINTER_EXPR,result->TOK_flags);

	  /* For user and most intrinsic functions assume result is
	     both assoc'd and alloc'd.  NULL intrinsic status is done
	     below.
	  */
	make_true(ASSOCIATED_EXPR,result->TOK_flags);
	make_true(ALLOCATED_EXPR,result->TOK_flags);
      }
      else
	make_false(POINTER_EXPR,result->TOK_flags);
      if(symt->target)
	make_true(TARGET_EXPR,result->TOK_flags);
      else
	make_false(TARGET_EXPR,result->TOK_flags);

      /* ELEMENTAL function result gets array shape of its argument(s) */
      if(symt->elemental) {
	result->array_dim = args->array_dim;
	copy_flag(ARRAY_EXPR,result->TOK_flags,args->next_token->TOK_flags);
      }
      else {			/* non-ELEMENTAL: use declared shape */
	result->array_dim = symt->array_dim;
	if(symt->array_var)
	  make_true(ARRAY_EXPR,result->TOK_flags);
      }

#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
(void)fprintf(list_fd,"\n%sFunction %s() = %s",
symt->intrinsic?"Intrinsic ":"",
symt->name,sized_typename(rettype,retsize));
 (void)fprintf(list_fd," dims %d size %ld kind %d",
    array_dims(result->array_dim),array_size(result->array_dim),result->kind);
}
#endif

		/* If intrinsic and all arguments are PARAMETER_EXPRs,
		   then result is one too. */
	if( symt->intrinsic ) {
	  int (*handler)( Token *args );
	  int evaluated_result;
	  defn = symt->info.intrins_info; /* done above, repeat to avoid compiler uninit warnings */
	  evaluated_result =
	    (is_true(EVALUATED_EXPR,args->TOK_flags) ||
	     defn->intrins_flags&I_INQ);
				/* Evaluate intrinsic if a handler is
				   defined and: result is integer or
				   has always-evaluate flag; and the
				   args are evaluated or function is
				   inquiry.  If result shape not same as arg,
				   handler will be called to determine the
				   new shape, and will do its
				   own checking of EVALUATED_EXPR if need be.
				 */
	  if( (handler = defn->ii_handler) != NULL &&
	      ( ((rettype == type_INTEGER || defn->intrins_flags&I_EVAL) &&
		 evaluated_result)
		|| defn->intrins_flags&I_ARRY ) )
	    {
		     result->value.integer = (*handler)(args);
		     /* Array-valued intrinsic evaluators put array
			info into args. */
		     if( defn->intrins_flags&I_ARRY ) {
				/* result may be array valued */
		       copy_flag(ARRAY_EXPR,result->TOK_flags,args->TOK_flags);
		       result->array_dim = args->array_dim;
		     }
				/* Evaluation routines can affect the flags */
		     copy_flag(EVALUATED_EXPR,result->TOK_flags,args->TOK_flags);
		     /* Intrinsics with const args and inquiry intrinsics
			yield const results
		      */
		     if(evaluated_result)
		       make_true(CONST_EXPR,result->TOK_flags);
	    }
	  else
	    {
	      result->value.integer = 0;
	      make_false(EVALUATED_EXPR,result->TOK_flags);
	    }
	  copy_flag(PARAMETER_EXPR,result->TOK_flags,args->TOK_flags);

	  /* NULL intrinsic makes pointer disassociated and
	     deallocated.  Above these flags were set, so here we need
	     to clear them.
	   */
	  if( defn->intrins_flags & I_NULL ) {
	    make_false(ASSOCIATED_EXPR,result->TOK_flags);
	    make_false(ALLOCATED_EXPR,result->TOK_flags);
	  }

#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
(void)fprintf(list_fd,"\n%s(...) ",defn->name);
if(is_true(EVALUATED_EXPR,args->TOK_flags))
  (void)fprintf(list_fd,"=%ld",result->value.integer);
else
  (void)fprintf(list_fd,"not evaluated");
(void)fprintf(list_fd,": const param eval=(%d %d %d)",
is_true(CONST_EXPR,result->TOK_flags),
is_true(PARAMETER_EXPR,result->TOK_flags),
is_true(EVALUATED_EXPR,result->TOK_flags));
}
#endif
	}
}/*func_ref_expr*/



		/* Make an expression-token for primary consisting of
		   a symbolic name */

void
#if HAVE_STDC
primary_id_expr(Token *id, Token *primary)
#else /* K&R style */
primary_id_expr(id,primary)
	Token *id,*primary;
#endif /* HAVE_STDC */
{
	Lsymtab *symt;
	int id_type;
	kind_t id_kind;
	symt = hashtab[id->value.integer].loc_symtab;
	id_type=get_type(symt);
	id_kind=get_kind(symt,id_type);

#ifndef TOK_type
	primary->tclass = id->tclass;
#endif
	primary->tsubclass = 0;
	primary->TOK_type = type_pack(storage_class_of(symt->type),id_type);
	primary->kind = id_kind;
#ifndef TOK_flags
	primary->TOK_flags = 0;
#endif
	primary->size =get_size(symt,id_type);
	primary->left_token = (Token *) NULL;
	primary->array_dim = symt->array_dim;

	make_true(ID_EXPR,primary->TOK_flags);

	if( storage_class_of(symt->type) == class_VAR) {
		if(symt->parameter) {
		    make_true(CONST_EXPR,primary->TOK_flags);
		    make_true(PARAMETER_EXPR,primary->TOK_flags);
		    make_true(EVALUATED_EXPR,primary->TOK_flags);
		}
		else {
		    make_true(LVALUE_EXPR,primary->TOK_flags);
		}
		if(symt->active_do_var) {
		    make_true(DO_VARIABLE,primary->TOK_flags);
		}
		if(symt->array_var)
		    make_true(ARRAY_EXPR,primary->TOK_flags);
		if(symt->set_flag || symt->common_var || symt->parameter
				  || symt->argument)
		    make_true(SET_FLAG,primary->TOK_flags);
		if(symt->assigned_flag)
		    make_true(ASSIGNED_FLAG,primary->TOK_flags);
		if(symt->used_before_set)
		    make_true(USED_BEFORE_SET,primary->TOK_flags);
	}
	else if(storage_class_of(symt->type) == class_STMT_FUNCTION) {
		make_true(STMT_FUNCTION_EXPR,primary->TOK_flags);
	}

#ifdef DEBUG_PARSER
if(debug_parser){
	(void)fprintf(list_fd,"\nprimary %s: TOK_type=0x%x TOK_flags=0x%x",
		symt->name,primary->TOK_type,primary->TOK_flags);
      }
#endif
}/*primary_id_expr*/

int
#if HAVE_STDC
intrins_arg_cmp(IntrinsInfo *defn, Token *t)
                       		/* Definition */
              			/* Argument */
#else /* K&R style */
intrins_arg_cmp(defn,t)
     IntrinsInfo *defn;		/* Definition */
     Token *t;			/* Argument */
#endif /* HAVE_STDC */
{
  int defn_types=defn->arg_type;
  int a_type = datatype_of(t->TOK_type);
  int type_OK;

  if( is_derived_type(a_type) ) { /* merge all derived types into one */
    a_type = MIN_DTYPE_ID;
  }
				/* Check for argument type mismatch.
				 */
	    type_OK = ( (1<<a_type) & defn_types );
	    if(! type_OK ) {
	      int ct;/* compatible type */
				/* Accept compatible types if
				   sizes agree, e.g. DSQRT(REAL*8).
				   The macros check the two cases and
				   set ct to the compatible type.
				 */
#define EXCEPTION1 (a_type==type_REAL && ((1<<(ct=type_DP))&defn_types))
#define EXCEPTION2 (a_type==type_COMPLEX&&((1<<(ct=type_DCOMPLEX))&defn_types))

	      if(!( (EXCEPTION1||EXCEPTION2) && t->size==type_size[ct] )){
		syntax_error(t->line_num,t->col_num,
			"illegal argument data type for intrinsic function");
		msg_tail(defn->name);
		msg_tail(":");
		report_type(t);
	      }
	      else {
		if(port_mixed_size || local_wordsize==0) {
		  nonportable(t->line_num,t->col_num,
	      "argument precision may not be correct for intrinsic function");
		  msg_tail(defn->name);
		  msg_tail(":");
		  report_type(t);
		}
		type_OK = TRUE; /* Acceptable after all */
	      }
	    }/* end if(! type_OK) */
			/* Quad intrinsics need a special check
			   to verify that real or cplx arg size is right.
			 */
	    else if(defn->intrins_flags & I_QARG) {
	      if(t->size != ((a_type==type_REAL)? size_QUAD: size_CQUAD)) {
		syntax_error(t->line_num,t->col_num,
			"illegal argument data type for intrinsic function");
		msg_tail(defn->name);
		msg_tail(":");
		report_type(t);
	      }
	    }

  return type_OK;
}/*intrins_arg_cmp*/


				/* Check agreement between statement function
				   dummy (t1) and actual (t2) args.  At this
				   time, checks only class, type and size,
				   not arrayness.  */
void
#if HAVE_STDC
stmt_fun_arg_cmp(const Lsymtab *symt, const Token *d_arg, const Token *a_arg)
#else /* K&R style */
stmt_fun_arg_cmp(symt,d_arg,a_arg)
     Lsymtab *symt;
     Token *d_arg,*a_arg;
#endif /* HAVE_STDC */
{
  int d_class = class_VAR,
      a_class = storage_class_of(a_arg->TOK_type),
      d_type = datatype_of(d_arg->TOK_type),
      a_type = datatype_of(a_arg->TOK_type),
      d_size = d_arg->size,
      a_size = a_arg->size,
      d_defsize = (d_size == size_DEFAULT),
      a_defsize = (a_size == size_DEFAULT);
  int d_cmptype= (d_type==type_HOLLERITH && a_type!=type_STRING)?
				a_type:type_category[d_type];
  int a_cmptype= (a_type==type_HOLLERITH && d_type!=type_STRING)?
				d_type:type_category[a_type];

  if(!(port_mixed_size || local_wordsize==0)) {
    if(d_defsize)
      d_size = type_size[d_type];
    if(a_defsize)
      a_size = type_size[a_type];
  }

  if(d_size < 0 || a_size < 0) { /* char size_ADJUSTABLE or UNKNOWN */
    d_size = a_size = size_DEFAULT;	/* suppress warnings on size */
    d_defsize = a_defsize = TRUE; /* these are not used at present */
  }

  if(d_class != a_class || d_cmptype != a_cmptype ||
     (d_type == type_STRING? d_size > a_size: d_size != a_size) ) {
		syntax_error(a_arg->line_num,a_arg->col_num,
		  "argument mismatch in stmt function");
		msg_tail(symt->name); /* Give the stmt func name */
		msg_tail(": dummy");
		report_type(d_arg); /* Dummy arg type */
		msg_tail("vs actual");
		report_type(a_arg);
  }
}/*stmt_fun_arg_cmp*/


				/* Routine to document the types of
				   two terms and their operator */
PRIVATE void
#if HAVE_STDC
report_mismatch(const Token *term1, const Token *op, const Token *term2)
#else /* K&R style */
report_mismatch(term1,op,term2)
     Token *term1,*op,*term2;
#endif /* HAVE_STDC */
{
  report_type(term1);
  msg_tail(op->src_text);
  report_type(term2);
}
				/* Routine to document the type
				   of a token, with its name if it
				   has one. */
PRIVATE void
#if HAVE_STDC
report_type(const Token *t)
#else /* K&R style */
report_type(t)
     Token *t;
#endif /* HAVE_STDC */
{
  msg_tail(sized_typename((int)datatype_of(t->TOK_type),t->size));
  if(is_true(ID_EXPR,t->TOK_flags))
    msg_tail(hashtab[t->value.integer].name);
  else if(is_true(LIT_CONST,t->TOK_flags)) {
    msg_tail("const");
    msg_expr_tree(t);
  }
  else {
    msg_tail("expr");
    msg_expr_tree(t);
  }
}

/* Prints value of range or precision of a kind.  tag="range" or "precision" */
PRIVATE void
report_kind_property(int value, const char *tag)
{
  if( value == 0 ) {msg_tail("default"); msg_tail(tag);}
  else if( value == -1 ) {msg_tail("processor-dependent"); msg_tail(tag);}
  else  {msg_tail(tag); msg_tail(ulongtostr((unsigned long)value));}
}

int
#if HAVE_STDC
substring_size(Token *id, Token *limits)
#else /* K&R style */
substring_size(id,limits)
     Token *id,*limits;
#endif /* HAVE_STDC */
{
	int id_type,id_len;
	int startindex,endindex,substr_len;
	Token *start_bound, *end_bound;
	id_type = datatype_of(id->TOK_type);

	substr_len=size_UNKNOWN;

	if(id_type != type_STRING) {
	  syntax_error(id->line_num,id->col_num,
		       "string variable expected");
	}
	else {
	  id_len = id->size;
	  if( id_len == size_ADJUSTABLE ) /* map len=* to unknown for simpler tests */
	    id_len = size_UNKNOWN;
	  /* get tokens holding starting and ending bounds */
	  start_bound = limits->left_token->left_token;
	  end_bound = limits->left_token->next_token;
	  if( start_bound->tclass == tok_empty ) /* empty start bounds expr (:...) */
	    startindex = 1;
	  else
	    startindex = start_bound->value.integer;

	  if( end_bound->tclass == tok_empty ) /* empty end bounds expr (...:) */
	    endindex = id_len;
	  else
	    endindex = end_bound->value.integer;

	  if(startindex != size_UNKNOWN && endindex != size_UNKNOWN) {
		/* Check limits validity.  Note end index<=0 and start>end allowed in F90 */
	    int f90_invalid, f77_invalid;
	    f90_invalid = (startindex <= 0 ||
		   (id_len != size_UNKNOWN && (startindex > id_len || endindex > id_len)) );
	    f77_invalid = (endindex <= 0 || startindex > endindex);
	    if( f90_invalid || (f77_substring_bounds && f77_invalid) ) {
	      syntax_error(limits->line_num,limits->col_num,
		      "invalid substring limits");
	      if(! f90_invalid)
		msg_tail("for F77");
	    }
	    /* calculate len whether limits valid or not */
	    substr_len = endindex-startindex+1;
	    if(substr_len < 0)	/* F90 rule allows zero-length substrings */
	      substr_len = 0;
	  }
	/* One common special case is if bounds are same quantity, then
	   length=1 regardless whether value of bound is known.  In
	   general we could compare expr trees but that gets messy, so
	   we only test identifier name match, e.g. S(I:I).  In that
	   case src_text will point to same place so just compare
	   ptrs.
	 */
	  else {		/* one or both bounds size_UNKNOWN */
	    if( start_bound->tclass == tok_identifier &&
		end_bound->tclass == tok_identifier &&
		start_bound->src_text == end_bound->src_text ) {
	      substr_len = 1;
	    }
	  }
	}
#ifdef DEBUG_ARRAY
	  if(debug_latest) {
	    fprintf(list_fd,"\nSubstring %s(",id->src_text);
#ifdef DEVELOPMENT
	    print_expr_list(limits);
#endif
	    fprintf(list_fd,") len=%d",substr_len);
	  }
#endif
	return substr_len;
}

/* Figures out the size of an array section or element */
array_dim_t
subarray_size(Token *id, Token *subscript_list)
{
    /* Note : Sizes for array sections with empty bounds is not supported
     * because we do not store the size of individual dimensions. This
     * may be implemented in the near future.
     */

    long size = 1;
    int dims = 0;

    /* Argument is a handle which acts as a header for the list */
    Token *subscript = subscript_list->next_token;

    while (subscript != NULL) {
	/* An array section */
	if (subscript->left_token == NULL ||
	    subscript->left_token->tclass != ':' ) { /* rank-0 subscript */
	    /* when shapes are supported, verify that index is in range */
	}
	else {				/* rank-1 array section */
	    long dim_size = array_section_size(id,dims,subscript->left_token);
	    if(dim_size == size_UNKNOWN)
		size = size_UNKNOWN;
	    else if(size != size_UNKNOWN)
		size *= dim_size;
	    dims++;
	}
	subscript = subscript->next_token;
    }

    if (dims == 0)		   /* result is scalar? */
	size = 0;			/* by convention scalar is (0,0) */

#ifdef DEBUG_ARRAY
    if(debug_latest) {
	fprintf(list_fd,"\nArray %s(",id->src_text);
#ifdef DEVELOPMENT			/* needed for print_expr_tree */
	print_expr_list(subscript_list->next_token);
#endif
	fprintf(list_fd,") dims %d size %ld",dims,size);
    }
#endif

    return array_dim_info(dims,size);

}

/* Routine to find size of array section specified by bounds, for
   dimension dim (numbering from 0 to rank-1 of array).
 */
PRIVATE int
array_section_size(Token *id, int dim, Token *bounds)
{
    int startindex, endindex, stride_len, array_len;
    Token *start_bound, *end_bound, *stride;

    array_len = size_UNKNOWN;

    /* get tokens holding starting and ending bounds */
    if (bounds->left_token->tclass == ':') { /* has stride */
	start_bound = bounds->left_token->left_token;
	end_bound = bounds->left_token->next_token;
	stride = bounds->next_token;
    }
    else {
	start_bound = bounds->left_token;
	end_bound = bounds->next_token;
	stride = (Token *)NULL;
    }

    if( start_bound->tclass == tok_empty ) { /* empty start bounds expr (:...) */
	   /* When shapes are implemented, this should be start index
	      of dimension dim. */
	startindex = size_UNKNOWN;
    }
    else {
	/* When shapes are implemented, should check that this is in range. */
	startindex = start_bound->value.integer;
    }

    if( end_bound->tclass == tok_empty ) {/* empty end bounds expr (...:) */
	   /* When shapes are implemented, this should be end index
	      of dimension dim. */
	endindex = size_UNKNOWN;
    }
    else {
	/* When shapes are implemented, should check that this is in range. */
	endindex = end_bound->value.integer;
    }

    if( stride == NULL ) /* no stride */
	stride_len = 1;
    else /* note stride cannot be empty if it is prefixed by a ':' */
	stride_len = stride->value.integer;

    if(stride_len == 0) {
	syntax_error(stride->line_num,stride->col_num,
		"Stride cannot be zero");
    }
    else {
      if(startindex != size_UNKNOWN && endindex != size_UNKNOWN &&
	    stride_len != size_UNKNOWN) {
	array_len = (endindex-startindex+stride_len)/stride_len;
	if(array_len < 0)
	    array_len = 0;
      }
      else {		/* one or both bounds or stride size_UNKNOWN */
      /* Handle common special case of identical bounds, unit stride */
	if( start_bound->tclass == tok_identifier &&
	    end_bound->tclass == tok_identifier &&
	    stride_len == 1 &&	/* stride > 1 would not make sense here */
	    start_bound->src_text == end_bound->src_text ) {
	  array_len = 1;
	}
      }
    }

    return array_len;
}


	/* Integer power: uses recursion x**n = (x**(n/2))**2 */
PRIVATE int
#if HAVE_STDC
int_power(int x, int n)
#else /* K&R style */
int_power(x,n)
	int x,n;
#endif /* HAVE_STDC */
{
	int temp;
			/* Order of tests puts commonest cases first */
	if(n > 1) {
		temp = int_power(x,n>>1);
		temp *= temp;
		if(n&1) return temp*x;	/* Odd n */
		else	return temp;	/* Even n */
	}
	else if(n == 1) return x;
	else if(n < 0) return 1/int_power(x,-n);	/* Usually 0 */
	else return 1;
}



				/* Undefine special macros */
#undef E
#undef I
#undef R
#undef D
#undef C
#undef L
#undef S
#undef H
#undef W

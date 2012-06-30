/* $Id: fortran.y,v 1.64 2004/11/18 02:06:04 moniot Exp $

    fortran.y:

	  Yacc grammar for Fortran program checker.  Uses the yylex()
	  in file FORLEX.C

*/

%{

/*
  fortran.c:


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

/*

	    This grammar is ANSI standard-conforming, except for:
		-- complex constant and a few other ambiguities needing
		   significant lookahead cannot be split across lines.

	    Extensions supported:
	        -- Case insensitive.
	 	-- Hollerith constants.
		-- Variable names may be longer than 6 characters.  Also
		   allows underscores and dollar signs in names.
		-- DO ... ENDDO and DO WHILE loop forms allowed.
		-- NAMELIST supported.
		-- TYPE and ACCEPT I/O statements allowed.
		-- Tabs are permitted in input, and (except in character data)
		   expand into blanks up to the next column equal to 1 mod 8.
		-- Type declarations INTEGER*2, REAL*8, etc. are allowed.
		-- IMPLICIT NONE allowed.
                -- CASE construct supported
*/


#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#include "symtab.h"
#include "symutils.h"
#include "dtypes.h"

	/* The following section is for use with bison-derived
	   parser.  Define alloca to be malloc for those cases
	   not covered by the cases covered there.  The ifdefs
	   are those in the skeleton parser with includes removed */
#ifdef AIXC	/* IBM RS/6000 xlc compiler does it this way */
#pragma alloca
#endif
#ifndef alloca
#ifdef __GNUC__
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#else /* Not sparc */
#ifdef MSDOS
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#define alloca malloc
#endif /* alloca now defined.  */

#ifndef YYDEBUG	/* If not declared otherwise... */
int yydebug;	/* declare yydebug to satisfy extern in ftnchek.c */
#ifdef DEVELOPMENT
#define YYDEBUG 1		/* For development it is handy */
#else
#define YYDEBUG 0
#endif
#endif

#ifdef DEVELOPMENT
#define DEBUG_PARSER
#endif

PRIVATE int current_datatype,	/* set when parse type_name or type_stmt */
    current_size_is_adjustable, /* set in CHARACTER declarations */
    current_size_is_expression, /* set in CHARACTER declarations */
    current_save_attr,	/* set if SAVE attribute given in type decl */
    current_external_attr,	/* set if EXTERNAL attr given */
    current_intrinsic_attr,	/* set if INTRINSIC attr given */
    current_parameter_attr,	/* set if PARAMETER attr given */
    current_pointer_attr,       /* set if POINTER attr given */
    current_target_attr,        /* set if TARGET attr given */
    current_allocatable_attr,   /* set if ALLOCATABLE attr given */
    current_public_attr,	/* set if PUBLIC attr given */
    current_private_attr,	/* set if PRIVATE attr given */
    current_intent_in_attr,	/* set if intent IN attr given */
    current_intent_out_attr,	/* set if intent OUT attr given */
    current_optional_attr,	/* set if OPTIONAL attr given */
    current_access_spec,	/* value of access spec in type defn stmt */
    current_elemental_attr,	/* subprog is ELEMENTAL */
    current_pure_attr,		/* subprog is PURE */
    current_recursive_attr,	/* subprog is RECURSIVE */
    label_dummy_arg_count,	/* number of labels in dummy argument list */
    len_selector_given, /* flag for use in processing CHARACTER decls */
    charspec_keywd_given,	/* for catching illegal forms */
    len_spec_item_count,/* count of items in CHARACTER len-selector list */
    control_item_count;	/* count of items in control_info_list */

kind_t current_kind;		/* set when parse kind parameter */

PRIVATE Token *current_dim_bound_list;	/* attr-based dim bound tokenlist */

PRIVATE void apply_intent_attr(Token *t); /* apply INTENT attribute */

	/* Information about the current I/O statement */
int current_io_unit_id;		/* hashnum of unit id of current I/O operation */
int  current_io_unit_no;	/* unit number of current I/O operation */
IO_ACCESS_TYPE current_io_access;/* access mode (direct/sequential) */
IO_FORM_TYPE current_io_form;	/* form (formatted/unformatted) */

int io_internal_file,	/* Flag for catching misuse of internal files */
    io_list_directed,	/* Flag for use in processing io control lists */
    io_warning_given;		/* to prevent multiple warnings */
			/* Flag shared with forlex for lexing hints */
int stmt_sequence_no,   /* set when parsing, reset to 0 at end_stmt */
    f90_stmt_sequence_no;
PRIVATE long current_typesize;	/* for type*len declarations: value of len */
PRIVATE char *current_len_text;	/* for type*len declarations: text of len */

/*PRIVATE int kind_warning_given=FALSE;*/ /* to say "not interpreted" only once */

PRIVATE Token
    len_spec_token,		/* Holds character length spec temporarily */
    dim_bound_token;		/* Holds attr-based dim-bound list header */

extern LINENO_t prev_stmt_line_num; /* shared with advance */

LINENO_t true_prev_stmt_line_num;	/* shared with symtab.c */

PRIVATE int
    current_prog_unit_type,
    executable_stmt=FALSE,
    prev_stmt_class=0,		 /* flags for lexer */
    labeled_stmt_type,		 /* for label handling */
    if_line_num, if_col_num,	/* for picky construct-usage warnings */
    prev_goto=FALSE,
    goto_flag=FALSE,	/* if unconditional GOTO was encountered */

/*---------------addition-----------------------------*/
    contains_ended,		/* var to remember that a CONTAINS block just ended */
    inside_function=FALSE,	/* is inside a function */
    contains_sect=FALSE,	/* for contains block */
    interface_block=FALSE,	/* for interface block */
    in_forall_construct=FALSE,	/* for FORALL construct */
    in_where_construct=FALSE,	/* for WHERE construct */
    sequence_dtype=FALSE,	/* for derived types with SEQUENCE attr */
    private_dtype=FALSE;	/* for derived types with PRIVATE attr */
/*----------------------------------------------------*/



int 
    complex_const_allowed=FALSE, /* for help in lookahead for these */
    construct_name_seen=FALSE,	/* for help recognizing DO */
    param_noparen=FALSE,	/* for different PARAMETER stmt semantics */
    in_assignment_stmt=FALSE,
    in_attrbased_typedecl=FALSE,/* help is_keyword lex type, attr :: list */
    inside_format=FALSE,	/* when inside parens of FORMAT  */
    integer_context=FALSE,	/* says integers-only are to follow */
    use_keywords_allowed=FALSE,	/* help for recognizing ONLY in USE stmt */
    generic_spec_allowed=FALSE; /* help for recognizing generic_spec */

		/* Macro for initializing attributes of type decl. */
#define reset_type_attrs() (\
    current_save_attr = FALSE, \
    current_external_attr = FALSE, \
    current_intrinsic_attr = FALSE, \
    current_parameter_attr = FALSE, \
    current_pointer_attr = FALSE, \
    current_target_attr = FALSE, \
    current_allocatable_attr = FALSE, \
    current_public_attr = FALSE, \
    current_private_attr = FALSE, \
    current_intent_in_attr = FALSE, \
    current_intent_out_attr = FALSE, \
    current_optional_attr = FALSE, \
    current_dim_bound_list = NULL, \
    current_datatype = 0, \
    current_kind = kind_DEFAULT_UNKNOWN, \
    current_pure_attr = FALSE, \
    current_elemental_attr = FALSE, \
    current_recursive_attr = FALSE )

			/* Define stuff for checking block nesting */

#define MAX_BLOCK_DEPTH 100	/* maximum depth of nesting blocks */

	/* USE_YYTNAME allows messages to incorporate token names from
	   the yytname array.  This option should be enabled if bison
	   is used to generate parser, not otherwise.  Enabling it
	   also assumes fortran.c has been edited to remove "tok_"
	   from token names (see Makefile).
	*/
#ifndef NO_YYTNAME
#if defined( YYDEBUG ) && defined( YYBISON )
#define USE_YYTNAME
#endif
#endif

				/* Convenience typedef for category of block */
typedef enum {subprog, construct} BLOCK_TYPE;


typedef struct {
    int sclass;			/* stmt_class of block opener */
    char *name;			/* name of block or subprogram */
    LABEL_t label;		/* label of closing statement for DO */
    LINENO_t first_line;	/* line number of block opener */
    BLOCK_TYPE blocktype;	/* category for wording of warnings */
    int do_var_hash;		/* hash index for index variable of DO block */

/*------------------------------ addition --------------------------*/
	SUBPROG_TYPE subprogtype; /* differentiates between module_subprog
									 and internal_subprog */
/*-------------------------------------------------------------------*/
} BlockStack;

PRIVATE BlockStack block_stack[MAX_BLOCK_DEPTH];

PRIVATE char *
    curr_stmt_name;	/* subprog or END-subprog name; DO, IF construct name*/

PRIVATE int
    block_depth=0;		/* depth of nesting of current block */

				/* Defns of private functions */

PRIVATE void check_token(Token *);	/* hook for ddd debugging */
PROTO(PRIVATE void push_block,(Token *t, int stmt_class, BLOCK_TYPE blocktype,
			       char *name, LABEL_t label));
PROTO(PRIVATE void pop_block,(Token *t, int stmt_class,
			      char *name, LABEL_t label));
PROTO(PRIVATE void check_construct_name_match,(Token *stmt, char *name));

PROTO(PRIVATE Token * add_tree_node,( Token *node, Token *left, Token *right ));
PROTO(PRIVATE void check_stmt_sequence,( Token *t, int seq_num ));
PROTO(PRIVATE void check_f90_stmt_sequence,( Token *t, int f90_seq_num ));
PROTO(PRIVATE void do_binexpr,( Token *l_expr, Token *op, Token *r_expr,
			Token *result ));
PROTO(PRIVATE type_t do_bounds_type,( Token *t1, Token *t2, Token *t3 ));
PROTO(PRIVATE void do_unexpr,( Token *op, Token *expr, Token *result ));
PROTO(PRIVATE void set_attr_flags,( Token *t, Token *result ));
PROTO(PRIVATE Token * empty_token,( Token *t ));
PROTO(PRIVATE void END_processing,( Token *t ));
PROTO(PRIVATE void init_io_ctrl_list,( void ));
PROTO(PRIVATE void record_default_io,( void ));
PROTO(PRIVATE void process_attrs,(Token *t,Token *dim_bounds));
PROTO(PRIVATE void process_prefix_attrs,(Token *t));

#ifdef DEBUG_PARSER
PROTO(PRIVATE void print_exprlist,( char *s, Token *t ));
PROTO(PRIVATE void print_comlist,( char *s, Token *t ));
#endif

SUBPROG_TYPE find_subprog_type(int stmt_class);
PRIVATE int get_curr_block_class();
PRIVATE char *get_curr_block_name();
PRIVATE void block_stack_top_swap();

		/* Uses of Token fields for nonterminals: */
/* NOTE: As of Aug 1994 these are undergoing revision to separate the
         use of class, subclass fields */
/*
  1. dim_bound_lists: dimensioning info for arrays:
       token.class = no. of dimensions,  --> TOK_dims
       token.subclass = no. of elements  --> TOK_elts
  2. expressions
       token.value.integer = hash index (of identifier)
       token.TOK_type = type_pack = storage_class << 4 + datatype
       token.TOK_flags: CONST_EXPR, LVALUE_EXPR, etc.
       token.TOK_flags: COMMA_FLAG used to handle extra/missing commas
*/


%}

%token tok_identifier
%token tok_array_identifier
%token tok_label
%token tok_integer_const
%token tok_real_const
%token tok_dp_const
%token tok_quad_const
%token tok_complex_const
%token tok_dcomplex_const
%token tok_logical_const
%token tok_string
%token tok_hollerith
%token tok_edit_descriptor
%token tok_letter
%token tok_relop	/* .EQ. .NE. .LT. .LE. .GT. .GE. */
%token tok_AND
%token tok_OR
%token tok_EQV
%token tok_NEQV
%token tok_NOT
%token tok_power	/*   **   */
%token tok_concat	/*   //   */
%token tok_double_colon /*   ::   */
%token tok_underscore	/* special underscore to avoid s/r conflicts */
%token tok_lparen	/* special left paren to avoid s/r conflicts */
%token tok_rightarrow   /*   =>   */
%token tok_l_ac_delimiter	/*   (/   */
%token tok_r_ac_delimiter	/*   /)   */
%token tok_ABSTRACT
%token tok_ACCEPT
%token tok_ALLOCATABLE
%token tok_ALLOCATE
%token tok_ASSIGN
%token tok_ASSIGNMENT
%token tok_BACKSPACE
%token tok_BIND
%token tok_BLOCKDATA
%token tok_BYTE
%token tok_CALL
%token tok_CASE
%token tok_CASEDEFAULT
%token tok_CHARACTER
%token tok_CLOSE
%token tok_COMMON
%token tok_COMPLEX
%token tok_CONTAINS
%token tok_CONTINUE
%token tok_CYCLE
%token tok_DATA
%token tok_DEALLOCATE
%token tok_DIMENSION
%token tok_DO
%token tok_DOUBLECOMPLEX
%token tok_DOUBLEPRECISION
%token tok_DOWHILE
%token tok_ELEMENTAL 
%token tok_ELSE
%token tok_ELSEWHERE /* not lexed but substituted by grammar */
%token tok_END
%token tok_ENDBLOCKDATA
%token tok_ENDDO
%token tok_ENDFILE
%token tok_ENDFORALL
%token tok_ENDFUNCTION
%token tok_ENDIF
%token tok_ENDINTERFACE
%token tok_ENDMODULE
%token tok_ENDPROGRAM
%token tok_ENDSELECT
%token tok_ENDSUBROUTINE
%token tok_ENDTYPE
%token tok_ENDWHERE
%token tok_ENTRY
%token tok_EQUIVALENCE
%token tok_EXTENDS
%token tok_EXTERNAL
%token tok_EXIT
%token tok_FORALL
%token tok_FORMAT
%token tok_FUNCTION
%token tok_GOTO
%token tok_IF
%token tok_IMPLICIT
%token tok_IN
%token tok_INCLUDE
%token tok_INOUT
%token tok_INQUIRE
%token tok_INTEGER
%token tok_INTENT
%token tok_INTERFACE
%token tok_INTRINSIC
%token tok_LOGICAL
%token tok_MODULE
%token tok_NAMELIST
%token tok_NONE
%token tok_NON_INTRINSIC
%token tok_NULLIFY
%token tok_ONLY
%token tok_OPEN
%token tok_OPERATOR
%token tok_OPTIONAL
%token tok_OUT
%token tok_PARAMETER
%token tok_PAUSE
%token tok_POINTER
%token tok_PRINT
%token tok_PRIVATE
%token tok_PROCEDURE
%token tok_PROGRAM
%token tok_PROTECTED
%token tok_PUBLIC
%token tok_PURE
%token tok_READ
%token tok_REAL
%token tok_RECURSIVE
%token tok_RESULT
%token tok_RETURN
%token tok_REWIND
%token tok_SAVE
%token tok_SELECTCASE
%token tok_SEQUENCE
%token tok_STOP
%token tok_SUBROUTINE
%token tok_TARGET
%token tok_THEN
%token tok_TO
%token tok_TYPE
%token tok_USE
%token tok_VOLATILE
%token tok_WHERE
%token tok_WHILE
%token tok_WRITE

%token tok_illegal  /* Illegal token unused in grammar: induces syntax error */

%token tok_empty    /* For empty tokens used to fill gaps in expr trees */

%token EOS	127	/* Character for end of statement.  */

%nonassoc tok_relop

%left REDUCE ')'	/* Used at unit_io to force a reduction */


%%
	/*  The following grammar is based on the ANSI manual, diagrams
	 *  of section F.  Numbers in the comments refer to the diagram
	 *  corresponding to the grammar rule.
	 */


/* 1-5 */

prog_body	:	stmt_list
		|	/* empty file */
		;

stmt_list	:	stmt_list_item
		|	stmt_list stmt_list_item
		;


stmt_list_item	:	ordinary_stmt
			{

				/* Create id token for prog if unnamed.  NOTE:
				   this clobbers $1.class, value, src_text.
				 */
			  if(current_prog_unit_hash == -1) {
			    implied_id_token(&($1),unnamed_prog);
			    def_function(
					 type_PROGRAM,	/* type */
					 size_DEFAULT,	/* size */
					 (char *)NULL,	/* size text */
					 (kind_t)0, /* kind */
					 &($1),		/* name */
					 (Token*)NULL,	/* args */
					 find_subprog_type(tok_PROGRAM));
			    current_prog_unit_hash =
			      def_curr_prog_unit(&($1));
			    current_prog_unit_type = type_PROGRAM;

				/* Pretend this is a PROGRAM statement */
			    if(style_req_prog_stmt) {
				warning($1.line_num,$1.col_num,
			"Program does not start with a PROGRAM statement");
			    }
			    push_block(&($$),tok_PROGRAM,subprog,
				       hashtab[current_prog_unit_hash].name,
				       NO_LABEL);
				/* It is possible for a block construct to
				   be the initial statement, and if so it
				   has earlier been pushed onto stack.  Detect
				   this situation and swap stack entries to
				   make them nest correctly.
				 */
			    if(block_depth > 1 &&
			       block_stack[block_depth-2].first_line == $1.line_num) {
			      block_stack_top_swap();
				/* If said block construct has pushed local scope,
				   (currently only applies to TYPE statement)
				   then the scope stack likewise needs to have
				   its top two entries swapped so the %MAIN is
				   outside the construct's scope.
				 */
			      if( curr_scope_bottom != 0 ) {
				move_outside_scope(hashtab[current_prog_unit_hash].loc_symtab);
				/* Fix the value of hash_num saved on
				   scope stack when scope was pushed
				*/
				loc_scope[loc_scope_top-1].hash_num =
				  current_prog_unit_hash;
			      }
			    }
			  }

					/* Handle END statement.  Note that
					   curr_stmt_class of structured END
					   stmts have been merged into tok_END.
					 */
			  if(curr_stmt_class == tok_END) {

			    pop_block(&($$),$$.tclass,
				      curr_stmt_name,NO_LABEL);

			    /* END implies RETURN for subprogram except in
			     * an interface block.
			     */
			    if(prev_stmt_class != tok_RETURN && !interface_block)
			      (void)do_RETURN(current_prog_unit_hash,&($1));
				
			    END_processing(&($$));
			    goto_flag = prev_goto = FALSE;
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  in_attrbased_typedecl = FALSE;
			  true_prev_stmt_line_num = $$.line_num;
			}
 		|	include_stmt
		|	EOS	/* "sticky" EOF for needed delay */
		;

			/* Statements: note that ordering by category
			   of statement is not enforced in the grammar
			   but is deferred to semantic processing.
			 */

ordinary_stmt	:	stmt
		|	end_stmt
			{
				/* Treat END PROGRAM et al as plain END */
			    curr_stmt_class = tok_END;

			}
		;

stmt		:	tok_label unlabeled_stmt
			{
				/* Put definition of label into table, and
				   if it marks end of a DO range, pop block.
				 */
			  int do_label = def_label(&($1),labeled_stmt_type);
			  if( do_label || $2.tclass == tok_ENDDO ) {
			    if(is_true(NOT_DO_TERMINAL_STMT,$2.TOK_flags)) {
			      syntax_error($2.line_num,$2.col_num,
			"statement cannot be terminal statement of a DO loop");
			    }
			    else {
			      pop_block(&($2),$2.tclass,curr_stmt_name,
				      do_label?
					(LABEL_t)($1.value.integer):
					NO_LABEL);
			    }
			  }

				/* Issue picky warnings about labeled
				   statements.  FORMAT has an excuse
				   for existing, so warnings for it
				   are separately controlled.  */
			  if( style_labeled_exec &&
			      curr_stmt_class != tok_FORMAT ) {
			    warning($1.line_num,$1.col_num,
				    "obsolescent feature: labeled statement");
			  }
			  else if( style_labeled_format &&
			      curr_stmt_class == tok_FORMAT ) {
			    warning($2.line_num,$2.col_num,
				    "obsolescent feature: FORMAT statement");
			  }

			  if(executable_stmt)
			    prev_goto = goto_flag;
			  $$ = $2;
			}
		|	unlabeled_stmt
			{
			  if(executable_stmt) {
			    if(prev_goto)
				syntax_error($1.line_num, NO_COL_NUM,
					"No path to this statement");
			    prev_goto = goto_flag;
			  }

			  if( curr_stmt_class == tok_FORMAT &&
			      misc_warn ) {
			      syntax_error($1.line_num,$1.col_num,
				      "FORMAT statement has no label");
			  }
			  if( $1.tclass == tok_ENDDO )
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			}
		;
        
unlabeled_stmt	:	subprogram_header
			{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			    labeled_stmt_type = LAB_SPECIFICATION;
			    push_block(&($1),$1.tclass,subprog,
				       hashtab[current_prog_unit_hash].name,
				       NO_LABEL);
			}
        |   non_subprogram_header
            {
/*--------------------addition--------------------------------*/

				if (contains_sect) 
                    syntax_error($1.line_num,$1.col_num,
                        "contains statement should be followed by a subprogram declaration");
                contains_sect = FALSE;
            }
		|	contains_stmt
			{ 
                if (interface_block)
                    syntax_error($1.line_num,$1.col_num,
                        " contains statement invalid inside interface block");

				if (block_stack[block_depth-1].subprogtype ==
			            internal_subprog) {
					syntax_error($1.line_num,$1.col_num,
                        "contains statement invalid inside internal subprogram");
				}
				else {
					contains_sect = TRUE; 
					push_loc_scope(); /* get ready for new scope */

#ifdef DEBUG_BLOCKCHECK
                if(debug_latest) 
                    printf("\nCONTAINS stmt: setting contains_sect=TRUE");
#endif
				}

				stmt_sequence_no = 0;
/*-----------------------------------------------------------*/
			}
        ;

non_subprogram_header   
        :   specification_stmt
			{
			    executable_stmt = FALSE;
			/* labeled_stmt_type set in lower productions */
			}
		|	executable_stmt
			{	

/*--------------------addition--------------------------------*/

                if (interface_block)
                    syntax_error($1.line_num,$1.col_num,
                        "executable statement invalid inside interface block");

/*-----------------------------------------------------------*/

			/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, $1.TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN) {
			    stmt_sequence_no = SEQ_STMT_FUN;
			    f90_stmt_sequence_no = F90_SEQ_SPECIF;
			    executable_stmt = FALSE;
			  }
			  else {
			    stmt_sequence_no = SEQ_EXEC;
			    f90_stmt_sequence_no = F90_SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			  }
			  labeled_stmt_type = LAB_EXECUTABLE;
			}
		|	restricted_stmt
			{

/*--------------------addition--------------------------------*/

                if (interface_block)
                    syntax_error($1.line_num,$1.col_num,
                        " restricted statement invalid inside interface block");

/*-----------------------------------------------------------*/

			    stmt_sequence_no = SEQ_EXEC;
			    f90_stmt_sequence_no = F90_SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			    labeled_stmt_type = LAB_EXECUTABLE;
			}
		|	error EOS
			{
			    executable_stmt = TRUE;
			    if(stmt_sequence_no == 0)
			      stmt_sequence_no = SEQ_HEADER;
			    if(f90_stmt_sequence_no == 0)
			      f90_stmt_sequence_no = SEQ_HEADER;
			    complex_const_allowed = FALSE; /* turn off flags */
			    use_keywords_allowed = FALSE;
                generic_spec_allowed = FALSE;
			    inside_format=FALSE;
			    integer_context = FALSE;
			    in_assignment_stmt = FALSE;
			    $$.line_num = prev_stmt_line_num; /* best guess */
			    labeled_stmt_type = LAB_EXECUTABLE;
			    reset_type_attrs();
			    yyerrok; /* (error message already given) */
			}
		;

subprogram_header:	prog_stmt
			{
			    current_prog_unit_type = type_PROGRAM;
			    if (interface_block)
				syntax_error($1.line_num,$1.col_num,
				    "program statment invalid inside interface block");
			}
		|	function_stmt
			{
			    current_prog_unit_type = type_SUBROUTINE;
			    inside_function = TRUE;
			    reset_type_attrs();
			}
		|	subroutine_stmt
			{
			    current_prog_unit_type = type_SUBROUTINE;
			    inside_function = FALSE;
			    reset_type_attrs();
			}
		|	block_data_stmt
			{
			    if (interface_block)
				syntax_error($1.line_num,$1.col_num,
				    "block data statment invalid inside interface block");
			    current_prog_unit_type = type_BLOCK_DATA;
			}
		|	module_stmt
			{
			    if (interface_block)
				syntax_error($1.line_num,$1.col_num,
				    "module statement invalid inside interface block");
				current_prog_unit_type = type_MODULE;
			}
		;

end_stmt	:	unlabeled_end_stmt
		|	tok_label
			unlabeled_end_stmt
			{
			  if( def_label(&($1),LAB_EXECUTABLE) ) {
			    syntax_error($2.line_num,$2.col_num,
			"statement cannot be terminal statement of a DO loop");
				/* Pop it anyway to keep stack consistent */
			    pop_block(&($2),$2.tclass,curr_stmt_name,
				      (LABEL_t)($1.value.integer));
			  }
			  $$ = $2;
			}
		;

				/* Various END statements not checked for
				   balance
				 */
unlabeled_end_stmt:	unnamed_end_stmt
			{
			    curr_stmt_name = NULL;
			}

		|	named_end_stmt
		;

unnamed_end_stmt:	tok_END EOS
		|	end_subprog_token EOS
		;

named_end_stmt:		end_subprog_token symbolic_name EOS
			{
			    curr_stmt_name = hashtab[$2.value.integer].name;
			}
		;

end_subprog_token:	tok_ENDBLOCKDATA
		|	tok_ENDFUNCTION {inside_function = FALSE;}
		|	tok_ENDMODULE
		|	tok_ENDPROGRAM
		|	tok_ENDSUBROUTINE
		;

include_stmt	:	tok_INCLUDE char_literal_const EOS
 			{
			  /* Named constant not allowed in the kind
			     parameter of the character literal */
			  if(f77_include) {
			      nonstandard($1.line_num,$1.col_num,0,0);
			  }
 			  open_include_file($2.value.string,$1.line_num);
 			}
 		;

/* 5,6 */
		/* Note that stmt_function_stmt is not distinguished from
		   assignment_stmt, but assign (label to variable) is.
		   Also, format_stmt w/o label is accepted here.
		   ANSI standard for statement sequencing is enforced here. */
specification_stmt:	anywhere_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     if(f90_stmt_sequence_no < F90_SEQ_IMPLICIT_NONE) {
				stmt_sequence_no = F90_SEQ_IMPLICIT_NONE;
			     }
			     /* labeled_stmt_type set below  */
			}
		|	parameter_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&($1),SEQ_SPECIF);
			     }
			     if(f90_stmt_sequence_no < F90_SEQ_IMPLICIT) {
				   f90_stmt_sequence_no = F90_SEQ_IMPLICIT;
			     }
			     else if(f90_stmt_sequence_no > F90_SEQ_SPECIF) {
			       check_f90_stmt_sequence(&($1),F90_SEQ_SPECIF);
			     }
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	implicit_stmt
			{
			  check_stmt_sequence(&($1),SEQ_IMPLICIT);
			/* f90 seq checks done at implicit_stmt */
			  labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	data_stmt
			{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			     if(f90_stmt_sequence_no <= F90_SEQ_SPECIF) {
				f90_stmt_sequence_no = F90_SEQ_SPECIF;
		 	     }
			     else {
			       check_f90_stmt_sequence(&($1),F90_SEQ_EXEC);
			     }
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	specif_stmt
			{
			  check_stmt_sequence(&($1),SEQ_SPECIF);
			  check_f90_stmt_sequence(&($1),F90_SEQ_SPECIF);
			  labeled_stmt_type = LAB_SPECIFICATION;
			  reset_type_attrs();
			}
		|	use_stmt
		|	interface_stmt
		 	{
		 	    interface_block = TRUE;
			    push_block(&($1),$1.tclass,construct,curr_stmt_name, NO_LABEL);
		 	    stmt_sequence_no = 0; /* allow subprog decls */
		 	    push_loc_scope();
			    generic_spec_allowed = FALSE;
		 	}
		|	end_interface_stmt
		 	{
		 	    pop_loc_scope();
		 	    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
		 	    interface_block = FALSE;
			    generic_spec_allowed = FALSE;
			    stmt_sequence_no = SEQ_IMPLICIT;
		 	}
		;

anywhere_stmt	:	entry_stmt
			{
			     goto_flag = prev_goto = FALSE;
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	format_stmt
			{
			     labeled_stmt_type = LAB_FORMAT;
			}
		;

specif_stmt	:	dimension_stmt
		|	equivalence_stmt
		|	common_stmt
		|	namelist_stmt
		|	type_stmt
		|	attrbased_type_stmt
		|	external_stmt
		|	intrinsic_stmt
		|	save_stmt
		|       cray_pointer_stmt
                |       pointer_stmt
                |       target_stmt
                |       allocatable_stmt
		|	access_stmt
		|	intent_stmt
		|	optional_stmt
		|	procedure_stmt
		 	{
		 	    if (!interface_block)
		 	        syntax_error($1.line_num,$1.col_num,
		 	            "procedure statement invalid outside interface block");
		 	}
		|	derived_type_def_stmt
		 	{
		 	    push_block(&($1),$1.tclass,construct,curr_stmt_name, NO_LABEL);
			    push_loc_scope();
		 	}
		|	end_derived_type_stmt
		 	{
			    char *dtype_name = get_curr_block_name();
			    process_dtype_components(dtype_name);
			    pop_loc_scope();
		 	    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);

		 	    curr_stmt_name = NULL;
			    sequence_dtype = FALSE;
			    private_dtype = FALSE;
			}
		|	sequence_stmt
		;

/* 7 */
executable_stmt:		/* Allowed in logical IF */
			transfer_stmt
			{
			    goto_flag=TRUE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	nontransfer_stmt
			{
			    goto_flag=FALSE;
			}
		;

transfer_stmt	:	unconditional_goto
		|	assigned_goto
			{
			     if( f95_assign ) {
			       nonstandard($1.line_num,$1.col_num,0,f95_assign);
			       msg_tail(": assigned GOTO");
			     }
			}
		|	arithmetic_if_stmt
			{
			    if(style_req_block_if) {
				warning(if_line_num, if_col_num,
					"non-structured IF statement");
			    }
			}
		|	cycle_or_exit_stmt
			{
			  check_construct_name_match(&($1),
				     curr_stmt_name);
			}
		|	stop_stmt
		|	return_stmt
		;

nontransfer_stmt:	assignment_stmt
		|	assign_stmt
		|	call_stmt
		|	computed_goto	/* fallthru allowed */
		|	continue_stmt
		|	pause_stmt	
		|	io_stmt
			{
			   record_io_usage(&($1));
			}
                |       allocate_stmt
                |       deallocate_stmt
                |       nullify_stmt
                |       where_stmt
                |       forall_stmt
                ;

io_stmt:		read_stmt
			{
			     /* If form not defined by now, READ is unformatted.
				If no REC=num was seen, then it is sequential.
			      */
			   if(current_io_form == IO_FORM_DEFAULT)
			      current_io_form = IO_FORM_UNFORMATTED;
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			}
		|	accept_stmt
		|	write_stmt
			{
			   if(current_io_form == IO_FORM_DEFAULT)
			      current_io_form = IO_FORM_UNFORMATTED;
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			}
		|	print_stmt
		|       type_output_stmt
		|	open_stmt
			{
			  /* In OPEN, default ACCESS is SEQUENTIAL, and default FORM
			     is FORMATTED for ACCESS=SEQUENTIAL, UNFORMATTED for
			     ACCESS=DIRECT.
			   */
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			   if(current_io_form == IO_FORM_DEFAULT) {
			     if(current_io_access == IO_ACCESS_SEQUENTIAL)
			       current_io_form = IO_FORM_FORMATTED;
			     else
			       current_io_form = IO_FORM_UNFORMATTED;
			   }
			}
		|	close_stmt
		|	inquire_stmt
		|	io_positioning_stmt
			{
			     /* These statements only apply to sequential access */
			   current_io_access = IO_ACCESS_SEQUENTIAL;
			}
		;

io_positioning_stmt:	rewind_stmt
		|	backspace_stmt
		|	endfile_stmt
		;




restricted_stmt:		/* Disallowed in logical IF */
			restricted_nontransfer_stmt
			{
			    goto_flag=FALSE;
			}
		|	else_or_endif_stmt
			{
			    prev_goto = goto_flag = FALSE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	select_case_stmt
			{
			    goto_flag = TRUE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	case_or_endselect_stmt
			{
			    prev_goto = goto_flag = FALSE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	where_construct_stmt
			{
			    push_block(&($1),tok_WHERE,construct,curr_stmt_name,NO_LABEL);
			    in_where_construct = TRUE;
			}
		|	elsewhere_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),tok_WHERE,construct,curr_stmt_name,NO_LABEL);
			}
		|	end_where_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    /* WHERE constructs can only have WHERE
			     constructs nested within them.  Therefore
			     it is sufficient to test whether the
			     enclosing block is not a WHERE construct
			     to know we are no longer inside a WHERE
			     construct */
			    if (get_curr_block_class() != tok_WHERE)
				in_where_construct = FALSE;
			}
		|	forall_construct_stmt
			{
			    push_block(&($1),tok_FORALL,construct,curr_stmt_name,NO_LABEL);
			    in_forall_construct = TRUE;
			}
		|	end_forall_stmt
			{
			    process_forall_construct(&($1));
			    END_processing(&($$));

			    pop_loc_scope();
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    /* FORALL constructs can only have FORALL
			     constructs and WHERE constructs nested
			     within.  Since any WHERE construct cannot
			     contain a FORALL and must be closed by
			     the time the END FORALL is reached, it is
			     sufficient to test whether the enclosing
			     block is not a FORALL construct to know
			     we are no longer inside a FORALL construct

			     Note that FORALL index variables have
			     local scope within a FORALL
			     construct. Since they are popped by
			     pop_loc_scope, their forall_var flags do
			     not need to be turned off as is done for
			     active_do_var.  */
			    if (get_curr_block_class() != tok_FORALL)
				in_forall_construct = FALSE;
			}
		;

restricted_nontransfer_stmt:
			logical_if_stmt
		|	block_if_stmt
			{
			/* Note that $1 at this point is expr, not tok_IF.
			   This is undesirable for our purpose here, but
			   needed for more important stuff elsewhere.
			 */
			    push_block(&($1),tok_IF,construct,curr_stmt_name,NO_LABEL);
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	do_stmt
			{	/* Flag DO w/o label or DO WHILE forms here */
			  if(is_true(NONSTD_USAGE_FLAG,$1.TOK_flags))
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard($1.line_num,$1.col_num,0,0);
#else
			    syntax_error($1.line_num,$1.col_num,
				    "Nonstandard syntax");
#endif
			  push_block(&($1),tok_DO,construct,curr_stmt_name,
				     (LABEL_t)($1.tsubclass));
			  make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
				/* Record hash index of DO variable in the
				   block stack entry for this statement.
				 */
			  block_stack[block_depth-1].do_var_hash = $1.value.integer;
			}

		|	enddo_stmt
			{
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard($1.line_num,$1.col_num,0,0);
#else
			    syntax_error($1.line_num,$1.col_num,
				    "Nonstandard syntax");
#endif
				/* pop_block is done at stmt production, where
				   optional label can be checked for match.
				 */
			}
		;

else_or_endif_stmt:	else_if_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),tok_IF,construct,curr_stmt_name,NO_LABEL);
			}
		|	else_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),tok_IF,construct,curr_stmt_name,NO_LABEL);
			}
		|	end_if_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			}
		;

case_or_endselect_stmt:	case_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),tok_SELECTCASE,construct,curr_stmt_name,NO_LABEL);
			}
		|	case_default_stmt
			{
			    pop_block(&($1),tok_CASE,curr_stmt_name,NO_LABEL);
			    push_block(&($1),tok_SELECTCASE,construct,curr_stmt_name,NO_LABEL);
			}
		|	end_select_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			}
		;

/* 8 */
prog_stmt	:	tok_PROGRAM {check_seq_header(&($1));}
				 symbolic_name EOS
			{
			     def_function(
					  type_PROGRAM,	/* type */
					  size_DEFAULT,	/* size */
					  (char *)NULL,	/* size text */
					  (kind_t)0, /* kind */
					  &($3),	/* name */
					  (Token*)NULL,/* args */
					  find_subprog_type($1.tclass));
			     current_prog_unit_hash =
			       def_curr_prog_unit(&($3));
			}
		;

			/* Note that function & subroutine entry not
			 * distinguished in this grammar.
			 */
/* 9 */
entry_stmt	:	unlabeled_entry_stmt EOS
	   		{
			  equivalence_result_vars($1.value.integer);
	   		}
		|	unlabeled_entry_stmt suffix
			{
			  do_suffix(tok_ENTRY, internal_subprog,
					current_prog_unit_hash,&($2),$1.value.integer);
			  equivalence_result_vars($2.value.integer);
			}
			EOS
		 ;

unlabeled_entry_stmt	:	tok_ENTRY symbolic_name
			{
			  do_ENTRY(&($2),(Token*)NULL
				   ,current_prog_unit_hash);
			  initial_flag = TRUE;
			  $$.value.integer = $2.value.integer;
			}
		|	tok_ENTRY symbolic_name '(' dummy_argument_list ')'
			{
			  do_ENTRY(&($2),&($4)
				   ,current_prog_unit_hash);
			  initial_flag = TRUE;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&($4));
#endif
			  $$.value.integer = $2.value.integer;
			}
		;


/* 10 */
function_stmt	:   unlabeled_function_stmt EOS
		|   unlabeled_function_stmt suffix
			{
                	  if (block_depth == 0 /* external subprog has not been pushed yet */
			      ||  block_stack[block_depth-1].sclass == tok_MODULE ) {
			    do_suffix(tok_FUNCTION, module_subprog,
				      $1.value.integer,&($2),$1.value.integer);
			  }
			  else {
			    do_suffix(tok_FUNCTION, internal_subprog,
				      $1.value.integer,&($2),$1.value.integer);
			  }
			} 
	            EOS
		;

unlabeled_function_stmt
		:	prefixed_function_handle symbolic_name 
			{
			     if(f77_function_noparen || f90_function_noparen) {
				nonstandard($2.line_num,
			     (unsigned)($2.col_num+strlen(token_name(&$2))),
					    f90_function_noparen,0);
				msg_tail(": parentheses required");
			     }
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      current_kind,
				      &($2),
				      (Token*)NULL,
					  find_subprog_type($1.tclass));
			 process_prefix_attrs(&($2));
			 current_prog_unit_hash=
			   def_curr_prog_unit(&($2));

             initial_flag = TRUE;
			 $$.value.integer = $2.value.integer;
			}
		|	prefixed_function_handle symbolic_name
				'(' dummy_argument_list ')' 
			{
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      current_kind,
				      &($2),
				      &($4),
					  find_subprog_type($1.tclass));
			 process_prefix_attrs(&($2));
			 current_prog_unit_hash=
			   def_curr_prog_unit(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
             initial_flag = TRUE;
			 $$.value.integer = $2.value.integer;
			}
		|	plain_function_handle symbolic_name 
			{
			     if(f77_function_noparen || f90_function_noparen) {
				nonstandard($2.line_num,
			      (unsigned)($2.col_num+strlen(token_name(&$2))),
					    f90_function_noparen,0);
				msg_tail(": parentheses required");
			     }
			 def_function(
				      type_UNDECL,
				      size_DEFAULT,
				      (char *)NULL,
				      kind_DEFAULT_UNKNOWN,
				      &($2),
				      (Token*)NULL,
					  find_subprog_type($1.tclass));
			 current_prog_unit_hash=
			   def_curr_prog_unit(&($2));
            
             initial_flag = TRUE;

			 $$.value.integer = $2.value.integer;
			}
		|	plain_function_handle symbolic_name 
				'(' dummy_argument_list ')' 
			{
			 def_function(
				      type_UNDECL,	/* type */
				      size_DEFAULT,	/* size */
				      (char *)NULL,	/* size text */
				      kind_DEFAULT_UNKNOWN,
				      &($2),		/* name */
				      &($4),		/* args */
					  find_subprog_type($1.tclass));
			 current_prog_unit_hash=
			   def_curr_prog_unit(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif

             initial_flag = TRUE;
			 $$.value.integer = $2.value.integer;
			}
		;

/*------------------- original ----------------------------*/

/*typed_function_handle:	type_name function_keyword
			{
			    $$ = $2; /* needed for block opener info */
/*			}
		; */

/*--------------------------------------------------------*/

/*----------------------- addition---------------------------*/

prefixed_function_handle:  prefix function_keyword
            {
			    $$ = $2; /* needed for block opener info */
            }
        ;

prefix  :   prefix_spec
        |   prefix prefix_spec 
        ;

prefix_spec :   declaration_type_spec
        |   tok_ELEMENTAL
	    {
		current_elemental_attr = TRUE;
	    }
        |   tok_PURE
	    {
		current_pure_attr = TRUE;
	    }
        |   tok_RECURSIVE
	    {
		current_recursive_attr = TRUE;
	    }
        ;

declaration_type_spec:  type_name
        ;

		/* Fortran2010 R1231 */

suffix  :  suffix_item
            {
                /* form suffix items into a token list */
                $$.next_token = append_token((Token *)NULL,&($1));
                $$.left_token = (Token *)NULL; /* clean up inherited field */
            }
        |  suffix suffix_item
            {
                $$.next_token = append_token($1.next_token,&($2));
            }
        ;

suffix_item : proc_language_binding_spec
        |   tok_RESULT '(' result_argument ')'
            {   
                $$ = $3;		/* replace RESULT token by identifier token */
				initial_flag = TRUE;	/* so BIND will be lexed */
            }
        

result_argument	:	symbolic_name
            {
                def_result_name(&($1));
                primary_id_expr(&($1),&($$));
            }
	    ;

proc_language_binding_spec: language_binding_spec
        ;

language_binding_spec  :    unnamed_bind    
        |   tok_BIND '(' C_keyword ',' bind_name ')'
            {
                /* create tree with C keyword as left child, bind name as right */
                $$.left_token = add_tree_node(&($1),&($3),&($5));

		initial_flag = TRUE;	/* so RESULT will be lexed */

            }
        ;

/* separate production needed for this to give token for tree node */
unnamed_bind    :   tok_BIND '(' C_keyword ')'
            {
                /* make C keyword the sole child of BIND */
                $$.left_token = add_tree_node(&($1),&($3),(Token *)NULL);
		initial_flag = TRUE;	/* so RESULT will be lexed */
            }
        ;

C_keyword    :   symbolic_name
        ;

bind_name   :   symbolic_name '=' expr
            {
                /* construct tree node with "NAME" and identifier as children of '=' */
                $$.left_token = add_tree_node(&($2),&($1),&($3));
                    
                        /* check if expr is constant and scalar */
                if (!is_true(CONST_EXPR,$3.TOK_flags) ||
                      is_true(ARRAY_EXPR,$3.TOK_flags))
                    syntax_error( $3.line_num, $3.col_num,
                       "scalar constant expression expected");
            }
        ;

intent_attr	:	intent_handle intent_spec
	    	;

		/* note: '(' is placed in handle because it sets
		 * initial_flag off which would disable recognition of
		 * intent_spec keywords */
intent_handle	:	tok_INTENT '('
			{
			    initial_flag = TRUE;
			}
		;

intent_spec	:	tok_IN ')'
			{
                          current_intent_in_attr = TRUE;
			}
		|	tok_OUT ')'
			{
                          current_intent_out_attr = TRUE;
			}
		|	tok_INOUT ')'
			{
                          current_intent_in_attr = TRUE;
                          current_intent_out_attr = TRUE;
			}
		;

intent_stmt_handle:	intent_attr
		|	intent_attr tok_double_colon
		;

intent_stmt	:	intent_stmt_handle intent_id_list EOS
	    	;

intent_id_list	:	intent_id
	       	|	intent_id_list ',' intent_id
		;

intent_id	:	variable_name
			{
			  apply_intent_attr(&($1));
			}
		;

optional_stmt	:	tok_OPTIONAL attr_decl_list EOS
	      	;

/*--------------------------------------------------------*/


plain_function_handle:	function_keyword
		;

function_keyword:	tok_FUNCTION
			{
			  check_seq_header(&($1));
			}
		;

	/* This production is used by prefix (for function statements)
	and by implicit_stmt.  It is not used by type declaration
	statements, which need to use different syntax for char
	and non-char declarations.
	*/
type_name	:	arith_type_name
		|	plain_char_type_name
		|	char_type_name
		|	derived_type_name
		;

/* 11 not present: see 9 */


/*----------------------- addition---------------------------*/

module_stmt	:   module_handle symbolic_name EOS
			{
                def_function(
				       type_MODULE,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($2),
				       (Token*)NULL,
					   find_subprog_type($1.tclass));
			    current_prog_unit_hash = def_curr_prog_unit(&($2));
			}
		;

module_handle	:	tok_MODULE
			{
			    check_seq_header(&($1));
			}
		;
/*------------------------------------------------------------*/


/* 12 */
subroutine_stmt	:   unlabeled_subroutine_stmt EOS
        |   unlabeled_subroutine_stmt proc_language_binding_spec
			{
                if (block_depth == 0 /* external subprog has not been pushed yet */
                        || block_stack[block_depth-1].sclass == tok_MODULE ) {
                    do_bind_spec(($2).left_token,module_subprog);
                }
                else {
                    do_bind_spec(($2).left_token,internal_subprog);
                }
			} EOS
		;

unlabeled_subroutine_stmt
		:	prefixed_subroutine_handle symbolic_name 
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($2),
				       (Token*)NULL,
					   find_subprog_type($1.tclass));
			  process_prefix_attrs(&($2));
			  current_prog_unit_hash=
			    def_curr_prog_unit(&($2));
			  
              initial_flag = TRUE;
			}
		|	prefixed_subroutine_handle symbolic_name
				'(' dummy_argument_list ')' 
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($2),
				       &($4),
					   find_subprog_type($1.tclass));
			  process_prefix_attrs(&($2));
			  current_prog_unit_hash=
			    def_curr_prog_unit(&($2));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&($4));
#endif
              initial_flag = TRUE;
			}
		|	plain_subroutine_handle symbolic_name 
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($2),
				       (Token*)NULL,
					   find_subprog_type($1.tclass));
			  current_prog_unit_hash=
			    def_curr_prog_unit(&($2));
              
              initial_flag = TRUE;
			}
		|	plain_subroutine_handle symbolic_name
				'(' dummy_argument_list ')' 
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($2),
				       &($4),
					   find_subprog_type($1.tclass));
			  current_prog_unit_hash=
			    def_curr_prog_unit(&($2));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&($4));
#endif
              
              initial_flag = TRUE;
			}
		;

/*--------------------- addition -----------------------------------*/

prefixed_subroutine_handle:    prefix subroutine_keyword
			{
			    $$ = $2; /* needed for block opener info */
			}
		;

plain_subroutine_handle:    subroutine_keyword
        ;

subroutine_keyword :	tok_SUBROUTINE
			{
			  check_seq_header(&($1));
			}
		;

/*--------------------------------------------------------------------*/

dummy_argument_list:	/* empty */
			{
			    $$.next_token = (Token*)NULL;
			}
		|	non_empty_arg_list
		;

non_empty_arg_list:	dummy_argument
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	non_empty_arg_list ',' dummy_argument
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

dummy_argument	:	symbolic_name
			{
			     def_arg_name(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	'*'
			{
			     $$.TOK_type = type_pack(class_LABEL,type_LABEL);
			     $$.size = size_DEFAULT;
			     $$.TOK_flags = 0;
			     $$.left_token = (Token *)NULL;
			     label_dummy_arg_count++;
			}
		;

/* 13 not present: see 9 */

/* 14 */
block_data_stmt	:	block_data_handle EOS
			{
				  /* form name %DATnn */
			  if( ++block_data_number == 2 ) {
			    /* warn once if more than one unnamed */
			    syntax_error($1.line_num,$1.col_num,
				 "unnamed BLOCK DATA unit conflicts with a previous declaration");
			  }
			  (void)sprintf(unnamed_block_data+4,"%02d",
					block_data_number%100);
			  implied_id_token(&($1),unnamed_block_data);

			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($1),
				       (Token*)NULL,
					   find_subprog_type($1.tclass));
			  current_prog_unit_hash=
			    def_curr_prog_unit(&($1));
			}
		|	block_data_handle symbolic_name EOS
			{
			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       (kind_t)0,
				       &($2),
				       (Token*)NULL,
					   find_subprog_type($1.tclass));
			  current_prog_unit_hash=
			    def_curr_prog_unit(&($2));
			}
		;

block_data_handle:	tok_BLOCKDATA
			{
			  check_seq_header(&($1));
			}

		;
/* 15 */
dimension_stmt	:	tok_DIMENSION array_declarator_list EOS
		|	tok_DIMENSION tok_double_colon array_declarator_list EOS
		;

array_declarator_list:	array_declarator
		|	array_declarator_list ',' array_declarator
		;

/* 16 */
array_declarator:	symbolic_name '(' dim_bound_list ')'
			{
			     def_array_dim(&($1),&($3));
			}
		;

dim_bound_list	:	dim_bound_item      /* token class = no. of dimensions,
					       subclass = no. of elements */
			{
			     $$.TOK_dims = 1;
			     $$.TOK_elts = $1.TOK_elts;
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	dim_bound_list ',' dim_bound_item
			{
			     $$.TOK_dims = $1.TOK_dims + 1; /* one more dimension */
			     if( $1.TOK_elts < 0 )
				 $$.TOK_elts = $1.TOK_elts; /* propagate unknown size */
			     else if( $3.TOK_elts < 0 )
				 $$.TOK_elts = $3.TOK_elts;
			     else	/* both known */
			         $$.TOK_elts = $1.TOK_elts * $3.TOK_elts;
			     $$.next_token = append_token($1.next_token,&($3));
			}
		;

dim_bound_item	:	dim_bound_expr
			{
			      if( datatype_of($1.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.TOK_flags) )
				$$.TOK_elts = $1.value.integer;
			      else
				$$.TOK_elts = size_UNKNOWN;
			}
		|	dim_bound_expr ':' dim_bound_expr
			{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of($1.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.TOK_flags)
				 && datatype_of($3.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$3.TOK_flags) )
				$$.TOK_elts = $3.value.integer - $1.value.integer + 1;
			      else
				$$.TOK_elts = size_UNKNOWN;

			      $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	'*'
			{
			     $$.TOK_elts = size_ASSUMED_SIZE;
			     $$.left_token = (Token *)NULL;
			}
		|	dim_bound_expr ':' '*'
			{
			     $$.TOK_elts = size_ASSUMED_SIZE;
			     $3.left_token = (Token *)NULL;
			     $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	dim_bound_expr ':'
			{
			     $$.TOK_elts = size_ASSUMED_SHAPE;
			     $$.left_token = add_tree_node(&($2),&($1),(Token*)NULL);
			}
                |       ':'
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": deferred-shape array spec");
			     }
			     $$.TOK_elts = size_DEFERRED;
			}
		;

/* 17 */
equivalence_stmt:	tok_EQUIVALENCE {equivalence_flag = TRUE;}
			equivalence_list EOS {equivalence_flag = FALSE;}
		;

equivalence_list:	'(' equivalence_list_item ')'
		|	equivalence_list ',' '(' equivalence_list_item ')'
		;

equivalence_list_item:	equiv_entity ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		|	equivalence_list_item ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		;

/* 17 */
equiv_entity	:	symbolic_name
			{
			     def_equiv_name(&($1));
			}
		|	array_equiv_name
			{
			     def_equiv_name(&($1));
			}
		|	substring_equiv_name
			{
			     def_equiv_name(&($1));
			}
		;

array_equiv_name:	tok_array_identifier '(' subscript_list ')'
				/* should check */
		;

substring_equiv_name:	tok_identifier '(' bounds_range ')'
		|	array_equiv_name '(' bounds_range ')'
		;

/* 19 */
common_stmt	:	tok_COMMON common_variable_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$), &($2));
			     if(is_true(COMMA_FLAG,$2.TOK_flags))
			   	syntax_error(
					     $2.line_num,$2.col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif

			}
		|	tok_COMMON common_block_list EOS
			{
			     if(is_true(COMMA_FLAG,$2.TOK_flags))
				syntax_error(
					     $2.line_num,$2.col_num,
					     "trailing comma");

			}
		|	tok_COMMON common_variable_list common_block_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$),&($2));
			     if(is_true(COMMA_FLAG,$3.TOK_flags))
				syntax_error(
					     $3.line_num,$3.col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif
			}
		;

	/*  The following defns allow trailing commas and missing commas in
	    order to tolerate the optional comma before /blockname/.  The
	    token TOK_flags holds comma status to allow errors to be caught. */
common_block_list:	labeled_common_block
			{
			     $$.TOK_flags = $1.TOK_flags;
			}
		|	common_block_list labeled_common_block
			{
			     $$.TOK_flags = $2.TOK_flags;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
			}
		;

labeled_common_block:	common_block_name common_variable_list
			{
			     def_com_block(&($1),&($2));
			     $$.TOK_flags = $2.TOK_flags;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&($2));
#endif
			}
		;

common_block_name:	'/' symbolic_name '/'
			{
			     $$ = $2;
			}

		|	'/'  '/'		/* block with no name */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		|	tok_concat		/* "//" becomes this */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		;

common_variable_list:	common_list_item
			{
			    $$.TOK_flags = $1.TOK_flags;
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	common_variable_list common_list_item
			{
			    if(!is_true(COMMA_FLAG,$1.TOK_flags))
				syntax_error(
					$2.line_num,$2.col_num-1,
					"missing comma");
			    $$.TOK_flags = $2.TOK_flags;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

common_list_item:	common_entity
			{			   /* no comma */
			     $$.TOK_flags = $1.TOK_flags;
			     make_false(COMMA_FLAG,$$.TOK_flags);
			}
		|	common_entity ','
			{			   /* has comma */
			     $$.TOK_flags = $1.TOK_flags;
			     make_true(COMMA_FLAG,$$.TOK_flags);
   			}
		;

common_entity	:	symbolic_name
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	array_declarator
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		;


/* NAMELIST : Not Standard
   Syntax is:
	NAMELIST /group/ var [,var...] [[,] /group/ var [,var...]...]
*/

namelist_stmt	:	tok_NAMELIST namelist_list EOS
			{
			    if(is_true(COMMA_FLAG,$2.TOK_flags))
				syntax_error($2.line_num,
				 (unsigned)($2.col_num+strlen(token_name(&$2))),
					"trailing comma");
			    if(f77_namelist) {
				nonstandard($1.line_num,$1.col_num,0,0);
			    }
			}
		;

namelist_list	:	namelist_decl
		|	namelist_list namelist_decl
			{
			    $$ = $2;
			}
		;

namelist_decl	:	namelist_name namelist_var_list
			{
			     def_namelist(&($1),&($2));
			     $$ = $2;
			}
		;

namelist_name	:	'/' symbolic_name '/'
			{
			    $$ = $2;
			}
		;

namelist_var_list:	namelist_item
			{
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	namelist_var_list namelist_item
			{
			    if(!is_true(COMMA_FLAG,$1.TOK_flags))
				syntax_error(
					$2.line_num,$2.col_num-1,
					"missing comma");
			    $$.TOK_flags = $2.TOK_flags;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

namelist_item	:	symbolic_name
			{			   /* no comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_false(COMMA_FLAG,$$.TOK_flags);
			}
		|	symbolic_name ','
			{			   /* has comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_true(COMMA_FLAG,$$.TOK_flags);
			}
		;

/* 20 */
type_stmt	:	arith_type_name arith_type_decl_list EOS
		|	plain_char_type_name char_type_decl_list EOS
		|	char_type_name char_type_decl_list EOS
		|	char_type_name ',' char_type_decl_list EOS
		|	derived_type_name arith_type_decl_list EOS
		;

				/* Attribute-based type declarations */
attrbased_type_stmt:	arith_attrbased_type_handle
			    tok_double_colon arith_type_decl_list EOS
			{
			  if(f77_attrbased_typedecl) {
			    nonstandard($2.line_num, $2.col_num,0,0);
			    msg_tail(": attribute-based variable declaration");
			  }
			}
		|	char_attrbased_type_handle
			    tok_double_colon char_type_decl_list EOS
			{
			  if(f77_attrbased_typedecl) {
			    nonstandard($2.line_num, $2.col_num,0,0);
			    msg_tail(": attribute-based variable declaration");
			  }
			}
		|	derived_attrbased_type_handle  tok_double_colon
			{
				/* make sure this is not an illegal forward ref */
			  Lsymtab *symt = hashtab[($1).value.integer].loc_symtab;
      			  int type = get_type(symt);
      			  if( is_derived_type(type) &&
			    dtype_table[type]->line_declared == NO_LINE_NUM ) { /* fwd ref */
			      int in_dtype_def = (get_curr_block_class() == tok_TYPE);
      			      if( !(in_dtype_def && current_pointer_attr) ) {
      			        syntax_error(($1).line_num,($1).col_num,"Type");
      			        msg_tail(dtype_table[type]->name);
      			        msg_tail("not defined at this point");
      			      }
      			  }

			}
					/* use non-char declaration list */
			 arith_type_decl_list EOS
		;

arith_attrbased_type_handle: arith_type_name
		|	arith_type_name ',' attr_list
		;

char_attrbased_type_handle: plain_char_type_name
		|	plain_char_type_name ',' attr_list
		|	char_type_name
		|	char_type_name ',' attr_list
		;

derived_attrbased_type_handle:	derived_type_name	
		|	derived_type_name ',' attr_list
		;

attr_list	:	type_attr
		|	attr_list ',' type_attr
		;

type_attr	:	tok_DIMENSION '(' dim_bound_list ')'
			{
				/* turn back on flags turned off by punct  */
			  in_attrbased_typedecl = initial_flag = TRUE;
			  dim_bound_token = $3;	/* save copy of header */
			  current_dim_bound_list = &dim_bound_token;
			}
		|	tok_SAVE
			{
			     current_save_attr = TRUE;
			}
		|	tok_EXTERNAL
			{
			     current_external_attr = TRUE;
			}
		|	tok_INTRINSIC
			{
			     current_intrinsic_attr = TRUE;
			}
		|	tok_PARAMETER
			{
			     current_parameter_attr = TRUE;
			}
                |       tok_POINTER
                        {
                             current_pointer_attr = TRUE;
                        }
                |       tok_TARGET
                        {
                             current_target_attr = TRUE;
                        }
                |       tok_ALLOCATABLE
                        {
                             current_allocatable_attr = TRUE;
                        }
		|	tok_PUBLIC
	    		{
			  current_public_attr = TRUE;
			}
        	|	tok_PRIVATE
			{
			  current_private_attr = TRUE;
			}
		|   	language_binding_spec  
			{
			    /* Fortran 2010 attribute is parsed but not
			    checked */
			}
        	|   	intent_attr
        	|   	tok_OPTIONAL
			{
			    current_optional_attr = TRUE;
			}
        	|   	tok_PROTECTED
			{
			    /* Fortran 2010 attribute is parsed but not
			    checked */
			}
        	|   	tok_VOLATILE
			{
			    /* Fortran 2010 attribute is parsed but not
			    checked */
			}
		;


arith_type_name	:	sizeable_type_name {
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			}
				/* Allow *len to modify some arith types */
		|	sizeable_type_name '*' nonzero_unsigned_int_const
			{
			    current_typesize = $3.value.integer;
			    current_len_text = NULL;
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && $3.value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && $3.value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			     if(f77_typesize || f90_typesize) {
				nonstandard($3.line_num,$3.col_num,f90_typesize,0);
			     }

				/* Give hint to lexer to continue taking attrs
				   as keywords despite non-initial position */
			     if(see_double_colon())
				 in_attrbased_typedecl = TRUE;
			}
				/* Parse KIND selectors */
		|	sizeable_type_name left_paren {integer_context=FALSE;}
				kind_selector {integer_context=TRUE;} ')'
			{
				/* Treat all KINDs as default size,
				 * except default QUAD */
			  if( current_kind == kind_DEFAULT_QUAD )
			    current_typesize = size_QUAD;
			  else
			    current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			  in_attrbased_typedecl = initial_flag = TRUE;
			}

				/* Other type disallow *len modifier */
		|	unsizeable_type_name
		;

sizeable_type_name:	tok_INTEGER
			{
			     current_datatype = type_INTEGER;
			     current_kind = default_kind(type_INTEGER);
			     integer_context = TRUE;
			}
		|	tok_REAL
			{
			     current_datatype = type_REAL;
			     current_kind = default_kind(type_REAL);
			     integer_context = TRUE;
			}
		|	tok_COMPLEX
			{
			     current_datatype = type_COMPLEX;
			     current_kind = default_kind(type_COMPLEX);
			     integer_context = TRUE;
			}
		|	tok_LOGICAL
			{
			     current_datatype = type_LOGICAL;
			     current_kind = default_kind(type_LOGICAL);
			     integer_context = TRUE;
			}
		;

unsizeable_type_name:	tok_DOUBLEPRECISION
			{
			     current_datatype = type_DP;
			     current_kind = default_kind(type_DP);
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			}
		|	tok_DOUBLECOMPLEX
			{
			     current_datatype = type_DCOMPLEX;
			     current_kind = default_kind(type_DCOMPLEX);
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     if(f77_double_complex || f90_double_complex) {
				nonstandard($1.line_num,$1.col_num,f90_double_complex,0);
			     }
			}
		|	tok_BYTE /* treate BYTE as a form of integer for now */
			{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			     current_len_text = NULL;
			     if(f77_byte || f90_byte)
			       nonstandard($1.line_num,$1.col_num,f90_byte,0);
			}
		;

				/* When F90 kind intrinsics are supported,
				   expr should become int_constant_expr.
				   For now, keep it lax to avoid spurious
				   warnings.
				 */
kind_selector	:	expr
			{
			  if(f77_attrbased_typedecl) {
			    nonstandard($1.line_num, $1.col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }

			  current_kind = int_expr_value(&($1));
			}
		|	symbolic_name '=' expr
			{
			  int type_of_kind;
			  int erroneous=FALSE;
			  if( strcmp(hashtab[$1.value.integer].name,"KIND")
			      == 0 ) {
			    current_kind = int_expr_value(&($3));
			    type_of_kind = kind_type(current_kind);

			    if( type_of_kind != type_UNDECL &&
			        current_datatype != type_of_kind ) {
			      warning($2.line_num,$2.col_num,
			              "kind parameter defined for");
			      msg_tail(type_name(type_of_kind));
			      msg_tail("type used to declare");
			      msg_tail(type_name(current_datatype));
			    }
			    if (port_concrete_kind &&
			        type_of_kind == type_UNDECL) {
			      nonportable($2.line_num,$2.col_num,
			              "declaration uses concrete kind parameter");
			    }
			  }
			  else {
			    syntax_error($1.line_num,$1.col_num,
					 "unrecognized keyword");
			    msg_tail(hashtab[$1.value.integer].name);
			    erroneous=TRUE;
			  }
			  if(!erroneous && f77_attrbased_typedecl) {
			    nonstandard($1.line_num, $1.col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }

			}
		;


plain_char_type_name:	tok_CHARACTER
			{
			     current_datatype = type_STRING;
			     current_kind = default_kind(type_STRING);
			     current_typesize = 1;
			     current_len_text = NULL;
			     current_size_is_adjustable = 0;
			     current_size_is_expression = 0;
			     integer_context = TRUE;
			     len_selector_given = FALSE;
			     charspec_keywd_given = FALSE;
			}
		;

char_type_name	:	plain_char_type_name char_selector
			{
			     current_typesize = $2.value.integer;
			     current_size_is_adjustable = $2.size_is_adjustable;
			     current_size_is_expression = $2.size_is_expression;
				/* Save length spec text if expression */
			     if(current_size_is_expression) {
			       if($2.left_token == NULL)
				 current_len_text = new_tree_text(&($2));
			       else
				 current_len_text = new_tree_text($2.left_token);
			     }
			     else
			       current_len_text = NULL;

				/* Give hint to lexer to continue taking attrs
				   as keywords despite non-initial position */
			     if(see_double_colon())
				 in_attrbased_typedecl = TRUE;
			}
		;

char_selector	:	'*' len_specification
			{
			  $$ = $2;
			}

				/* This production uses a special left paren
				   to avoid a shift/reduce conflict with
				     IMPLICIT CHARACTER(letter_list)
				   The lexer knows when to produce this
				   special left paren.
				 */
		|	left_paren {len_spec_item_count = 0;} len_spec_list ')'
			{
			  if( len_selector_given ) {
			    $$ = len_spec_token; /* Recover spec saved below */
				/* Store as a parenthesized expr tree */
			    $$.left_token = add_tree_node(&($1),
							  &len_spec_token,
							  (Token*)NULL);
			  }
				/* If len_spec_list does not specify a LEN,
				   use the current default values.
				 */
			  else {
			    $$.left_token = (Token *)NULL;
			    $$.value.integer = current_typesize;
			    $$.size_is_adjustable = current_size_is_adjustable;
			    $$.size_is_expression = current_size_is_expression;
			  }
			  if(f77_attrbased_typedecl) {
			    nonstandard($1.line_num, $1.col_num,0,0);
			    msg_tail(": F90-style variable declaration");
			  }
			}
		;

			/* This production simply turns the special left paren
			   back into a regular paren in case it matters
			   somewhere.  The grammar still knows it's special.
			 */
left_paren	:	tok_lparen
			{
			  $$.tclass = '('; /* make it a regular paren */
			}
		;


arith_type_decl_list:	arith_type_decl_item
		|	arith_type_decl_list ',' arith_type_decl_item
		;

			/* Allow the combined type declaration and data value form.
			 */
arith_type_decl_item: arith_type_decl_entity
			{
			     if( current_parameter_attr) {
				syntax_error($1.line_num,$1.col_num,
					     "PARAMETER lacks initializer");
			     }
			}
				/* Handle bastard initializers (combined type decl
				   and data statement) here.
				 */
		|	 arith_type_decl_entity '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    if(f77_initializers || f90_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    check_initializer_type(&($$),&($2),&($4));
			}
				/* Handle F90 initializers here.  Note that
				   this production will not be reached in
				   non attribute-based type declarations since
				   it will be lexed as an assignment statement.
				 */
		|	arith_type_decl_entity {integer_context=FALSE;complex_const_allowed = TRUE;}
				assignment_op parameter_expr
			{
			    if(current_parameter_attr)
				def_parameter(&($1),&($4),FALSE);

			    if(f77_initializers) {
				nonstandard($3.line_num,$3.col_num,
					    0,0);
				msg_tail(": F90-style initializer");
			    }
			    check_initializer_type(&($$),&($3),&($4));
			    integer_context=TRUE;
			    complex_const_allowed = FALSE;
			}
		;

arith_type_decl_entity: scalar_type_decl_entity
		|	array_declarator_entity
		;

scalar_type_decl_entity:symbolic_name
			{
			     declare_type(&($1),
					  current_datatype,
					  current_kind,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),current_dim_bound_list);
			     primary_id_expr(&($1),&($$));
			     set_attr_flags(&($1),&($$));
			}
		;

array_declarator_entity: array_declarator
			{
			     declare_type(&($1),
					  current_datatype,
					  current_kind,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),(Token *)NULL);
			     primary_id_expr(&($1),&($$));
			     set_attr_flags(&($1),&($$));
			}
		;

char_type_decl_list:	char_type_decl_item
		|	char_type_decl_list ',' char_type_decl_item
		;

char_type_decl_item: char_type_decl_entity
			{
			     if( current_parameter_attr) {
				syntax_error($1.line_num,$1.col_num,
					     "PARAMETER lacks initializer");
			     }
			     set_attr_flags(&($1),&($$));
			}
				/* Handle bastard initializers here */
		|	 char_type_decl_entity '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    if(f77_initializers || f90_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    primary_id_expr(&($1),&($$));
			    set_attr_flags(&($1),&($$));
			    check_initializer_type(&($$),&($2),&($4));
			}
				/* Handle F90 initializers here */
		|	char_type_decl_entity '=' parameter_expr
			{
			    if(current_parameter_attr)
				def_parameter(&($1),&($3),FALSE);

			    primary_id_expr(&($1),&($$));
			    if(f77_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    0,0);
				msg_tail(": F90-style initializer");
			    }
			    set_attr_flags(&($1),&($$));
			    check_initializer_type(&($$),&($2),&($3));
			}
   		;

char_type_decl_entity:char_scalar_type_decl_entity
		|	char_array_declarator_entity
		;

char_scalar_type_decl_entity: symbolic_name
			{
			     $1.size_is_adjustable = current_size_is_adjustable;
			     $1.size_is_expression = current_size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_kind,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),current_dim_bound_list);
			     set_attr_flags(&($1),&($$));
			}
		|	symbolic_name '*' len_specification
			{
			     $1.size_is_adjustable = $3.size_is_adjustable;
			     $1.size_is_expression = $3.size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_kind,
					  $3.value.integer,
					  new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token ));
			     process_attrs(&($1),current_dim_bound_list);
			     set_attr_flags(&($1),&($$));
			}
		;

char_array_declarator_entity: array_declarator
			{
			     $1.size_is_adjustable = current_size_is_adjustable;
			     $1.size_is_expression = current_size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_kind,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),(Token *)NULL);
			     set_attr_flags(&($1),&($$));
			}
		|	array_declarator '*' len_specification
			{
			     $1.size_is_adjustable = $3.size_is_adjustable;
			     $1.size_is_expression = $3.size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_kind,
					  $3.value.integer,
					  new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token ));
			     process_attrs(&($1),(Token *)NULL);
			     set_attr_flags(&($1),&($$));
			}
		;

/*---------------------------addition----------------------------*/

contains_stmt:  tok_CONTAINS EOS
        ;

use_stmt    :   use_handle use_spec
            {
                generic_spec_allowed = FALSE;
            }
        ;

use_handle  :   use_intro
        |   use_intro tok_double_colon
        ;

use_intro   :  tok_USE
	|  use_keyword_comma  module_nature
	;

use_keyword_comma: tok_USE ','
            {
		initial_flag = TRUE; /* enable lexing [NON_]INTRINSIC */
            }
	    ;


module_nature   :   tok_INTRINSIC 
        |   tok_NON_INTRINSIC 
        ;

use_spec:	module_name
		{
		  read_module_file($1.value.integer,(Token *)NULL,FALSE);
		}
		| module_name_comma use_only
		{
		  read_module_file($1.value.integer,&($2),TRUE);
		}
		| module_name_comma rename_list
		{
		  read_module_file($1.value.integer,&($2),FALSE);
		}
	;


module_name_comma: module_name ','
	    {
		use_keywords_allowed = TRUE; /* enable lexing ONLY keyword */
	    }
        ;

module_name :   symbolic_name
            {
                generic_spec_allowed = TRUE;
            }
        ;

rename_list :   rename
	    {
	        $$.next_token = append_token((Token *)NULL,&($1));
	    }
        |   rename_list ',' rename
	    {
	        $$.next_token = append_token($1.next_token,&($3));
	    }
        ;

rename  :   symbolic_name tok_rightarrow symbolic_name
	    {
		 $3.left_token = append_token((Token *)NULL,&($1));
		 $$ = $3;
	    }
        |   tok_OPERATOR '(' operator ')'
            tok_rightarrow tok_OPERATOR '(' operator ')'
        ;

use_only    :   only_keywd ':' EOS
	    {
	        $$.next_token = (Token *)NULL; /* no list */
	    }
        |   only_keywd ':' only_list EOS
	    {
	        $$ = $3; /* synthesize the only token list */
	    }
        ;

only_keywd: tok_ONLY
	    {
		use_keywords_allowed = FALSE; 
	    }
	;

only_list   : only_item  
	    {
	        $$.next_token = append_token((Token *)NULL,&($1));
	    }
        |   only_list ',' only_item
	    {
	        $$.next_token = append_token($1.next_token,&($3));
	    }
        ;

only_item   :   generic_spec
        |   rename
        ;

generic_spec    :   symbolic_name
		{
		    curr_stmt_name = hashtab[$1.value.integer].name;
		}
					/* for these other forms, save
					 * minimum info necessary for
					 * checking block match in
					 * curr_stmt_name.  SHOULD FIX.
					 */
        |   tok_OPERATOR '(' operator ')'
		{
		  curr_stmt_name = $3.src_text;
		}
        |   tok_ASSIGNMENT '(' '=' ')'
		{
		  curr_stmt_name = $3.src_text;
		}
        |   defined_io_generic_spec
		{
		  curr_stmt_name = $1.src_text;
	        /* Fortran 2010 feature which is parsed but not checked */
		}
        ;

operator    :   tok_power | '*' | '/' | '+' | '-' | tok_concat 
        | tok_relop | tok_NOT | tok_AND | tok_OR | tok_EQV | tok_NEQV 
			/*        | defined_operator */
            {
                /* defined unary or binary operator of the form
                   .myoperator. */
            }
        ;

defined_io_generic_spec :   tok_READ '(' symbolic_name ')'
        |   tok_WRITE '(' symbolic_name ')'
            {

                /*
defined_io_generic_spec :   symbolic_name '(' symbolic_name ')'
            {
            if ((strcmp(hashtab[($1).value.integer].name, "READ") != 0)
               && (strcmp(hashtab[($1).value.integer].name, "WRITE") != 0))
                    syntax_error(($1).line_num, ($1).col_num,
                      "defined-io-generic-spec : READ or WRITE");

          if ((strcmp(hashtab[($3).value.integer].name,"FORMATTED") != 0)
            &&(strcmp(hashtab[($3).value.integer].name,"UNFORMATTED")!=0))
                    syntax_error(($3).line_num, ($3).col_num,
                      "defined-io-generic-spec : [UN]FORMATTED");
            }
        ;
        */

            }
        ;

interface_stmt  :   interface_handle EOS
		{
		    curr_stmt_name = (char *)NULL;
		}
	        |   interface_handle generic_spec EOS
		    /* curr_stmt_name is set in generic_spec production */
	        ;

interface_handle    :   tok_INTERFACE
            {
                generic_spec_allowed = TRUE;
            }
        |   tok_ABSTRACT tok_INTERFACE
            {
	        /* ABSTRACT is a Fortran 2010 attribute which is parsed
		but not checked */

                /* needed to simplify matching in pop_block */
                $$ = $2;
                generic_spec_allowed = TRUE;
            }
        ;

procedure_stmt  :   procedure_stmt_handle non_empty_arg_list
        ;

procedure_stmt_handle  :   tok_PROCEDURE 
		{
	        /* Fortran 2010 feature which is parsed but not checked */
		}
        |   tok_PROCEDURE tok_double_colon 
		{
	        /* Fortran 2010 feature which is parsed but not checked */
		}
        |   tok_MODULE tok_PROCEDURE 
        |   tok_MODULE tok_PROCEDURE tok_double_colon 
		{
	        /* Fortran 2010 feature which is parsed but not checked */
		}
        ;

end_interface_stmt   :   end_interface_handle EOS
		{
		    curr_stmt_name = (char *)NULL;
		}
	        |   end_interface_handle generic_spec
		    /* curr_stmt_name is set in generic_spec production */
        ;

end_interface_handle :   tok_ENDINTERFACE
		{
		    generic_spec_allowed = TRUE;
		}
	;

derived_type_def_stmt   :   derived_type_handle dtype_name EOS
        ;

dtype_name: symbolic_name
		{
		  def_dtype($1.value.integer,$1.line_num,$1.col_num,
			  current_access_spec,TRUE);
		  curr_stmt_name = hashtab[$1.value.integer].name;
		}
	;
derived_type_handle :   derived_type_keyword
		{			/* access spec not declared */
		  current_access_spec = 0;
		}
        |   derived_type_keyword tok_double_colon
		{			/* access spec not declared */
		  current_access_spec = 0;
		}
        |   derived_type_keyword ',' access_spec tok_double_colon
		{
		  current_access_spec = $3.tclass;
		}
        ;

derived_type_keyword    :   tok_TYPE 
        ;

end_derived_type_stmt:   tok_ENDTYPE EOS
        |   tok_ENDTYPE symbolic_name
        ;

/*
structure_component:	primary
		|	structure_component '%' primary
		;

part_ref	:	part_name
		;

part_name	:	array_element_name
		;
		*/

access_spec	:	tok_PUBLIC
	    		{
			  generic_spec_allowed = TRUE;
			}
        	|	tok_PRIVATE
			{
			  generic_spec_allowed = TRUE;
			}
        	;

sequence_stmt	:	tok_SEQUENCE
			/* sequence attribute is mutually exclusive of
			 * private attribute in derived type definitions
			 */
			{
			  if (get_curr_block_class() != tok_TYPE) {
			    syntax_error($1.line_num,$1.col_num,
			      "SEQUENCE statement is only allowed in derived type definitions");
			  }
			  else if (private_dtype) {
			    syntax_error($1.line_num,$1.col_num,
			      "PRIVATE statement already seen for current derived type definition");
			  }
			  else {
			    if (sequence_dtype) {
			      syntax_error($1.line_num,$1.col_num,
			        "SEQUENCE statement already seen for current derived type definition");
			    }
			    else {
			      sequence_dtype = TRUE;
			      int h = hash_lookup(get_curr_block_name());
			      Lsymtab *symt = hashtab[h].loc_symtab;
			      symt->sequence = TRUE;
			    }
			  }
			}
		;

access_stmt	:	access_spec EOS /* PUBLIC or PRIVATE statement */
			{
			/* This production is NOT used for the
			   accessibility attribute in an attribute-based 
			   type declaration. */

			  /* access statement in a type declaration */
			  if( get_curr_block_class() == tok_TYPE ) {
			    /* record access spec in dtype symtab entry */
			    if (curr_stmt_class == tok_PUBLIC) {
			      syntax_error($1.line_num,$1.col_num,
			      	     "PUBLIC statement is not allowed in derived type definition");

			    }
			    else if (sequence_dtype) {
			      syntax_error($1.line_num,$1.col_num,
					   "SEQUENCE statement already seen for current derived type definition");
			    }
			    else if (private_dtype) {
			      syntax_error($1.line_num,$1.col_num,
					   "PRIVATE statement already seen for current derived type definition");
			    }
			    else {
			      if (curr_stmt_class == tok_PRIVATE) {
			        private_dtype = TRUE;
				privatize_components(get_curr_block_name());
			      }
			    }
			  }
			  else if( get_type(hashtab[current_prog_unit_hash].loc_symtab) != type_MODULE) {
			    syntax_error($1.line_num,$1.col_num,
					 "Accessibility statement not allowed here");
			  }
			  else if( module_accessibility != 0 ) { /* second declaration */
			    syntax_error($1.line_num,$1.col_num,"Module accessibility");    
			    if(module_accessibility != $1.tclass)
			      msg_tail("conflicts with previous declaration");
			    else
			      msg_tail("redeclared");
			  }
			  else {
			    module_accessibility = $1.tclass;
			  }
			}
		|	access_spec
			access_id_part { generic_spec_allowed = FALSE;} EOS
		;

access_id_part	:	access_id_list
	      	|	tok_double_colon access_id_list
		;

access_id_list	:	access_id
	       	|	access_id_list ',' access_id
		;

access_id	:	access_generic_spec
	  		{
			  apply_attr(&($1),curr_stmt_class);
			}
		;

access_generic_spec:	symbolic_name
        	|	tok_OPERATOR '(' operator ')'
        	|	tok_ASSIGNMENT '(' '=' ')'
		;

derived_type_name:	tok_TYPE '(' symbolic_name ')' 
			{
			  int in_dtype_def = (block_stack[block_depth-1].sclass == tok_TYPE);
			  /* Give hint to lexer to continue taking
			     attrs as keywords despite non-initial
			     position */
			  if(see_double_colon())
			    in_attrbased_typedecl = TRUE;

			  /* Get the index of the type.  If this is a
			     forward reference, a new datatype will be
			     assigned.  This is an error outside of a
			     type definition, or inside one if
			     variable is not a pointer.  Here we treat
			     all forward references alike, leaving
			     error diagnostics for later when all the
			     facts are known.
			   */
			  current_datatype = find_dtype(&($3),in_dtype_def);
			  current_kind = default_kind(current_datatype);
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			  $$ = $3;	/* pass the type name up */
			}
		;


/*---------------------------------------------------------------*/



/* 21 */
				/* implicit_flag helps is_keyword's work */
implicit_handle	:	tok_IMPLICIT {implicit_flag=TRUE;}
		;

implicit_stmt	:	implicit_handle implicit_decl_list EOS
			{
			    implicit_flag=FALSE;
			    /* if(implicit_none) { */
			    if(implicit_info.implicit_none) {
				syntax_error($1.line_num,$1.col_num,
				     "conflicts with IMPLICIT NONE");
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			    check_f90_stmt_sequence(&($1),F90_SEQ_IMPLICIT);
			}
				/* IMPLICIT NONE statement */
		|	implicit_handle tok_NONE EOS
			{
			    implicit_flag=FALSE;
			    if(implicit_type_given) {
			      syntax_error($1.line_num,$1.col_num,
				   "conflicts with IMPLICIT statement");
			    }
			    else {
			      if(f77_implicit_none)
				      nonstandard($2.line_num,$2.col_num,0,0);
				  set_implicit_none();
			    }
			    check_f90_stmt_sequence(&($1),F90_SEQ_IMPLICIT_NONE);
			}
		;

implicit_decl_list:	implicit_decl_item
		|	implicit_decl_list ',' {initial_flag = TRUE;}
				       implicit_decl_item
		;

		/* implicit_letter_flag tells lexer to treat letters as letters,
			   not as identifiers */
implicit_decl_item:	type_name '('  {implicit_letter_flag = TRUE;}
				letter_list ')'  {implicit_letter_flag = FALSE;}
		;

letter_list	:	letter_list_item
		|	letter_list ',' letter_list_item
		;

letter_list_item:	tok_letter
			{
			  int c1 = (int)$1.tsubclass;
				/* kluge to allow other non-alpha chars:
				   treate anything except _ as $.
				 */
			  if( !isalpha(c1) && c1 != '_' ) c1 = '$';

			  if( ((f77_dollarsigns||f90_dollarsigns) && c1=='$')
			   || (f77_underscores && c1=='_') ) {
			    nonstandard($1.line_num,$1.col_num,
				f90_dollarsigns&&c1=='$',0);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c1);
			}
		|	tok_letter '-' tok_letter
			{
			  int c1 = (int)$1.tsubclass,
			      c2 = (int)$3.tsubclass;

			  if( !isalpha(c1) && c1 != '_' ) c1 = '$';
			  if( !isalpha(c2) && c2 != '_' ) c2 = '$';

			  if( ((f77_dollarsigns||f90_dollarsigns) && (c1 == '$' || c2 == '$'))
			   || (f77_underscores && (c1 == '_' || c2 == '_')))
			  {
			    if(!isalpha(c1))
			      nonstandard($1.line_num,$1.col_num,
				  f90_dollarsigns&&(c1=='$'||c2=='$'),0);
			    else
			      nonstandard($3.line_num,$3.col_num,
				  f90_dollarsigns&&(c1=='$'||c2=='$'),0);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c2);
			}
		;


/* 22 */
len_specification:	nonneg_unsigned_int_const
			{
			     $$.value.integer = $1.value.integer;
			     $$.size_is_adjustable = 0;
			     $$.size_is_expression = 0;
			}

		|	'(' len_spec_expr ')'
			{
			    $$.value.integer = $2.value.integer;
			    $$.size_is_adjustable = $2.size_is_adjustable;
			    $$.size_is_expression = $2.size_is_expression;
				/* Store as a parenthesized expr tree */
			    $$.left_token = add_tree_node(&($1),
							  &($2),
							  (Token*)NULL);
			}
		;

		/* To keep grammar simple, the syntax rules for CHARACTER
		   length-selector are relaxed, leaving checking to the
		   semantic processing.  Legal variations are
		      CHARACTER*(length)
		      CHARACTER*(LEN=length)
		      CHARACTER*(length,kind)
		      CHARACTER*(length,KIND=kind)
		      CHARACTER*(LEN=length,KIND=kind)
		      CHARACTER*(KIND=kind,LEN=length)
		   where length is * or a specification-expr, and kind is
		   a scalar-int-initialization-expr.  The * can be omitted
		   in all of these.
		   The grammar rules here accept anything of the form
		      CHARACTER*([KEYWORD=]value[, ...])
		   where value is * or int_constant_expr.  When the LEN
		   keyword is seen, the len_spec_item token is copied to
		   len_spec_token to be used in higher-level productions.
		 */

len_spec_list	:	len_spec_item
			{
			  ++len_spec_item_count;
			}
		|	len_spec_list ',' len_spec_item
			{
			  ++len_spec_item_count;
			}
		;

len_spec_item	:	len_spec_expr
			{
				/* Non-keyword form: 1st item is LEN */
			  if(len_spec_item_count == 0) {
			    len_spec_token = $1;
			    len_selector_given = TRUE;
			  }
				/* 2nd item is KIND */
			  else if(len_spec_item_count == 1) {
			    /* If keyword was given earlier then it must be supplied later.
			     */
			    if( charspec_keywd_given ) {
			      syntax_error($1.line_num,$1.col_num,
				   "keyword");
			      if(len_selector_given) msg_tail("KIND");
			      else                   msg_tail("LEN");
			      msg_tail("required here");
			    }

			    if( len_selector_given ) {
			      current_kind = int_expr_value(&($1));
			    }
			    else {
			      len_spec_token = $1;
			      len_selector_given = TRUE;
			    }
			  } 
			  else if(len_spec_item_count == 2) { /* only complain once */
			    syntax_error($1.line_num,$1.col_num,
					 "too many specifiers in list");
			  }
			}
		|	symbolic_name '=' len_spec_expr
			{
			  int erroneous=FALSE;
			  if( strcmp(hashtab[$1.value.integer].name,"LEN")
			      == 0 ) {
			    len_spec_token = $3;
			    len_selector_given = TRUE;
			  }
			  else if( strcmp(hashtab[$1.value.integer].name,"KIND")
			      == 0 ) {
			    current_kind = int_expr_value(&($3));
			  }
			  else {
			    syntax_error($1.line_num,$1.col_num,
					 "unrecognized keyword");
			    msg_tail(hashtab[$1.value.integer].name);
			    erroneous=TRUE;
			  }
			  if(!erroneous && f77_attrbased_typedecl) {
			    nonstandard($2.line_num, $2.col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			  charspec_keywd_given = TRUE; /* once given, must use keywords */
			}
		;

len_spec_expr	:	'*'
			{
			     $$.left_token = (Token *)NULL;
			     $$.value.integer = size_ADJUSTABLE;
			     $$.size_is_adjustable = 1;
			     $$.size_is_expression = 0;
			}
		|	int_constant_expr
			{
			     $$.size_is_adjustable = 0;
			     $$.size_is_expression = 1;
			     if($1.value.integer <= 0) {
			      if($1.value.integer < 0) {
			        if(misc_warn || f77_char_extension) { 
				  warning($1.line_num,$1.col_num,
				  "non-negative integer value expected");
				  msg_tail(": substituting 0");
				}
				$$.value.integer = 0;
			      }
			      else if(f77_char_extension) {
				   warning($1.line_num,$1.col_num,
				   "nonzero integer value expected");
			      }
			     }
			}
		;

/* 23 */
parameter_stmt	:	std_parameter_stmt
		|	parenless_parameter_stmt
		;

std_parameter_stmt:	tok_PARAMETER '(' parameter_defn_list ')' EOS
		;

parenless_parameter_stmt:tok_PARAMETER {param_noparen=TRUE;}
			parameter_defn_list {param_noparen=FALSE;} EOS
			{
			  if(f77_param_noparen || f90_param_noparen) {
				nonstandard($1.line_num,$1.col_num,f90_param_noparen,0);
				msg_tail(" : PARAMETER declaration without parentheses");
			  }
			}
   		;

parameter_defn_list:	parameter_defn_item
		|	parameter_defn_list ',' parameter_defn_item
		;

parameter_defn_item:	symbolic_name {complex_const_allowed = TRUE;}
				'=' parameter_expr
			{
			     def_parameter(&($1),&($4),param_noparen);
			     primary_id_expr(&($1),&($$));
			     set_attr_flags(&($1),&($$));
			     check_initializer_type(&($$),&($3),&($4));
			     complex_const_allowed = FALSE;
			}
		;

/* 24 */
external_stmt	:	external_handle external_name_list EOS
		;

external_handle:	tok_EXTERNAL
		|	tok_EXTERNAL tok_double_colon
		;

external_name_list:	symbolic_name
			{
			     def_ext_name(&($1));
			}
		|	external_name_list ',' symbolic_name
			{
			     def_ext_name(&($3));
			}
		;

/* 25 */
intrinsic_stmt	:	intrinsic_handle intrinsic_name_list EOS
		;

intrinsic_handle :	tok_INTRINSIC
		|	tok_INTRINSIC tok_double_colon
		;


intrinsic_name_list:	symbolic_name
			{
			     def_intrins_name(&($1));
			}
		|	intrinsic_name_list ',' symbolic_name
			{
			     def_intrins_name(&($3));
			}
		;

        /* construct for allocatable statement */
allocatable_stmt:       tok_ALLOCATABLE attr_decl_list EOS
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": ALLOCATABLE statement");
			     }
			}
                ;

        /* construct for f90 target statement */
target_stmt     :       tok_TARGET attr_decl_list EOS
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": TARGET statement");
			     }
			}
                ;

        /* construct for f90 pointer statement */
pointer_stmt    :       tok_POINTER attr_decl_list EOS
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": POINTER statement");
			     }
			}
                ;

attr_decl_list	:	attr_decl_item
		|	tok_double_colon attr_decl_item
		|	attr_decl_list ',' attr_decl_item
		;

attr_decl_item	:	symbolic_name
			{
			     apply_attr(&($1),curr_stmt_class);
			}
		|	array_declarator
			{
			     apply_attr(&($1),curr_stmt_class);
			}
                ;



        /* constructs for nonstd Cray POINTER(pointer=pointee) statement */
cray_pointer_stmt:      tok_POINTER cray_pointer_item_list EOS
		{
		  if(f77_cray_pointers || f90_cray_pointers) {
		    nonstandard($1.line_num,$1.col_num,f90_cray_pointers,0);
		  }
		}
		;

cray_pointer_item_list: cray_pointer_item
		|       cray_pointer_item_list ',' cray_pointer_item
		;

cray_pointer_item:      '(' pointer_name ',' pointee_name ')'
		;

pointer_name    :       symbolic_name
			{
			     declare_type(&($1),
			                  type_INTEGER,
			                  default_kind(type_INTEGER),
					  local_ptrsize,
					  NULL);
			}
		;

pointee_name    :       symbolic_name
		        {
				/* Suppress set/used warnings since
				   often is accessed only via pointer */
		             use_lvalue(&($1));
		             use_variable(&($1));
		        }
		|       array_declarator
		        {
		             use_lvalue(&($1));
		             use_variable(&($1));
		        }
		;

allocate_stmt   :       tok_ALLOCATE {allocatable_flag = TRUE;} '(' allocate_item_list ')' EOS
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": ALLOCATE statement");
			     }
                             do_allocate(&($4));
                             allocatable_flag = FALSE;
			}
                ;

deallocate_stmt :       tok_DEALLOCATE '(' deallocate_item_list ')' EOS
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": DEALLOCATE statement");
			     }
                             do_deallocate (&($3));
			}
                ;

allocate_item_list:     allocate_item
                |       allocate_item_list ',' allocate_item
                        {
                            $$.next_token = append_token($1.next_token,&($3));
                        }
                |       allocate_item_list ',' allocate_stat_item
                ;

allocate_item	:	data_object
		;


			/* productions for ALLOCATE( ..., STAT=variable) */
allocate_stat_item:	symbolic_name '=' variable_name
			{
			     if(is_true(LVALUE_EXPR,$3.TOK_flags)) {
				  use_lvalue(&($3));
			     }
			}

                |       symbolic_name '=' array_element_name
			{
			     if(is_true(LVALUE_EXPR,$3.TOK_flags)) {
				  use_lvalue(&($3));
			     }
			}
                ;

deallocate_item_list:   allocate_item
                |       deallocate_item_list ',' allocate_item
                        {
                            $$.next_token = append_token($1.next_token,&($3));
                        }
                |       deallocate_item_list ',' allocate_stat_item
                ;

nullify_stmt    :       tok_NULLIFY '(' nullify_item_list ')' EOS
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": NULLIFY statement");
			     }
			     do_nullify(&($3));
			}
                ;

nullify_item_list:	nullify_item
			{
			    $$.next_token = append_token((Token *)NULL,&($1));
			}
		|	nullify_item_list ',' nullify_item
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

nullify_item	:	data_object
		;

			    /* where_handle can only be of the form
			       tok_WHERE
			    */
where_stmt	:	where_handle '(' log_expr ')' assignment_stmt
	   		{
			    if(is_true(ID_EXPR,$3.TOK_flags)){
				use_variable(&($3));
			    }
			}
	   	;

where_construct_stmt:	where_handle '(' log_expr ')' EOS
	   		{
			    if(is_true(ID_EXPR,$3.TOK_flags)){
				use_variable(&($3));
			    }
			}
		    ;

where_handle	:	tok_WHERE
	     		{
			    curr_stmt_name = (char *)NULL;
			}
	     	|	construct_spec tok_WHERE
			{
			    /* make check fails if
			       construct_name ':' is used instead
			    */
			}
		;

elsewhere_stmt	:	elsewhere_handle EOS
			{
			    /* To allow construct names to match
			       when there are nested where constructs.
			    */
			    curr_stmt_name = get_curr_block_name();
			}
	        |	elsewhere_handle tok_identifier
			{
			    curr_stmt_name = hashtab[$2.value.integer].name;
			}
		;

elsewhere_handle:	tok_ELSE tok_WHERE
			{
			    /* To match where construct block */
			    $$.tclass = tok_ELSEWHERE;
			}
		|	tok_ELSE tok_WHERE '(' log_expr ')'
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    /* To match where construct block */
			    $$.tclass = tok_ELSEWHERE;
			}
		;

end_where_stmt	:	tok_ENDWHERE EOS
	     		{
			    curr_stmt_name = (char *)NULL;
			}
		|	tok_ENDWHERE tok_identifier
			{
			    curr_stmt_name = hashtab[$2.value.integer].name;
			}
		;

forall_stmt	:	forall_handle forall_header assignment_stmt
	     		{
			    process_forall_construct(&($3));
			    pop_loc_scope();
			}
	    	;

forall_construct_stmt:	forall_handle forall_header EOS 
		     ;

forall_handle	:	tok_FORALL
	     		{
			    curr_stmt_name = (char *)NULL;
			    push_loc_scope();
			}
	      	|	construct_spec tok_FORALL
	     		{
			    push_loc_scope();
			}
	      	;

forall_header	:	'(' forall_triplet_list ')'
	      	|	'(' forall_triplet_list ',' log_expr ')'
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			}
	      	;

forall_triplet_list:	forall_triplet
		   |	forall_triplet_list ',' forall_triplet
		   ;

forall_triplet	:	tok_identifier '=' forall_subscript_list
	       		{
			    def_forall_index(&($1));
			}
	       	;

forall_subscript_list:	bounds_expr ':' bounds_expr
		     |	bounds_expr ':' bounds_expr ':' bounds_expr
		     ;

end_forall_stmt	:	tok_ENDFORALL EOS
	     		{
			    curr_stmt_name = (char *)NULL;
			}
		|	tok_ENDFORALL tok_identifier
			{
			    curr_stmt_name = hashtab[$2.value.integer].name;
			}
		;

/* 26 */
save_stmt	:	tok_SAVE EOS
			{
			  global_save = TRUE;
			}
		|	tok_SAVE save_list EOS
		;

save_list	:	save_item
		|	save_list ',' save_item
		;

save_item	:	symbolic_name
			{
			     apply_attr(&($1),tok_SAVE);
			}
		|	'/' symbolic_name '/'
			{
/***			     def_com_block(&($2),(Token*)NULL);***/
			     save_com_block(&($2));
			}
		;

/* 27 */
data_stmt	:	tok_DATA data_defn_list EOS
   		;

data_defn_list	:	data_defn_item
		|	data_defn_list data_defn_item
		|	data_defn_list ',' data_defn_item
		;

data_defn_item	:	data_defn_assignee_list '/'
				{complex_const_allowed=TRUE;}
					data_value_list
				{complex_const_allowed=FALSE;}  '/'
		;

data_defn_assignee_list
		:	data_defn_assignee
		|	data_defn_assignee_list ',' data_defn_assignee
		;

data_defn_assignee:	lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_value_list:	data_value
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	data_value_list ',' data_value
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

data_value	:	data_constant_value
			{
			    $$.left_token = (Token*)NULL;
			}
		|	data_repeat_factor '*' data_constant_value
			{
				/* Save data repeat factor in a permanent token
				   pointed to by left_token.
				 */
			    Token *tcopy = new_token();
			    *tcopy = $1; /* copy the repeat factor token */
			    $$ = $3; /* pass data_value up the parse tree */
			    $$.left_token = tcopy;
			}
		;

data_repeat_factor:	nonzero_unsigned_int_const
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;

data_constant_value:	data_constant
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;


data_dlist	:	data_dlist_item
		|	data_dlist ',' data_dlist_item
		;

data_dlist_item	:	array_element_lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_implied_do_list:  '(' data_dlist ',' implied_do_control ')'
		;

implied_do_control: symbolic_name '=' implied_do_loop_bounds
			{
			   $$.value.integer = $3.value.integer;
			   use_implied_do_index(&($1));
			   $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		;

implied_do_loop_bounds:	bounds_expr ',' bounds_expr
			{
			  if( $1.value.integer == size_UNKNOWN ||
			      $3.value.integer == size_UNKNOWN ) {
			    $$.value.integer = size_UNKNOWN;
			  }
			  else {
			    $$.value.integer = $3.value.integer - $1.value.integer + 1;
			  }
			  $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	bounds_expr ',' bounds_expr ',' bounds_expr
			{
			  if( $1.value.integer == size_UNKNOWN ||
			      $3.value.integer == size_UNKNOWN ||
			      $5.value.integer == size_UNKNOWN ) {
			    $$.value.integer = size_UNKNOWN;
			  }
			  else {
			    int span = ($3.value.integer-$1.value.integer+$5.value.integer)
			      /$5.value.integer;
			    if(span < 0)
			      span = 0;
			    $$.value.integer = span;
			  }
			  $3.left_token = add_tree_node(&($4),&($3),&($5));
			  $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		;


/* 29 */
assignment_stmt	:	lvalue assignment_op {complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;} expr
			{
			  if( ! (is_true(LVALUE_EXPR,$1.TOK_flags)
			       || is_true(STMT_FUNCTION_EXPR,$1.TOK_flags) )) {
			    syntax_error($1.line_num,$1.col_num,
					 "left side is not assignable");
			    if(is_true(CONST_EXPR,$1.TOK_flags))
				msg_tail(": it is a constant");
			  }
			  else {
			    int array_lhs, array_rhs;
			    array_lhs =
			      (($1.TOK_flags&(ARRAY_EXPR|ARRAY_ELEMENT_EXPR)) == ARRAY_EXPR);
			    array_rhs =
			      (($4.TOK_flags&(ARRAY_EXPR|ARRAY_ELEMENT_EXPR)) == ARRAY_EXPR);
			    if( array_lhs || array_rhs ) {
			      if( (! array_lhs) && misc_warn) {
				warning($1.line_num,$1.col_num,
					"array assigned to scalar");
			      }
			      else if( f77_assignment ) {
				nonstandard($2.line_num,$2.col_num,0,0);
				msg_tail(": assignment involving whole array");
			      }
			    }

			    assignment_stmt_type(&($1),&($2),
					&($4));
			  }
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			}
				 EOS
			{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, $1.TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&($1));
		        }
		;

lvalue		:	variable_name
		|	array_element_lvalue
		|	component /* derived type component */
			{
			    ref_component(&($1),&($$),TRUE);
			}
		|	substring_lvalue
		|	stmt_function_handle
		;

assignment_op	: '='				/* normal assignment */
		| tok_rightarrow	/* module rename assignment */
			{
			     if( f77_pointers ) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(": pointer assignment");
			     }
			}
		;

/* array-element_lvalue is at 88 */

assign_stmt	:    	tok_ASSIGN pre_label label tok_TO variable_name EOS
			{
			    do_ASSIGN(&($5));
			    if( f95_assign ) {
			      nonstandard($1.line_num,$1.col_num,0,f95_assign);
			      msg_tail(": ASSIGN statement");
			    }

			    ref_label(&($3),LAB_ASSIGN);

			}
		;


/* 31 */
unconditional_goto:	goto pre_label label EOS
			{

			  ref_label(&($3),LAB_GOTO);

			}
		;

/* 32 */
computed_goto	:	goto '(' goto_list ')' integer_expr EOS
		|	goto '(' goto_list ')' ',' integer_expr EOS
		;

/* 33 */
assigned_goto	:	goto symbolic_name EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name ',' '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		;

goto		:	tok_GOTO
			{
			    integer_context=TRUE;
				/* Warn if GOTO considered harmful */
			    if( style_goto ) {
			      warning($1.line_num,$1.col_num,
				      "obsolescent feature: GOTO statement");
			    }
			}
		;

goto_list	:	pre_label label
                        {
                            ref_label(&($2), LAB_GOTO);
			}
		|	goto_list ',' pre_label label
                        {
			    ref_label(&($4), LAB_GOTO);
			}
		;

/* 34 */
arithmetic_if_stmt:	if_handle pre_label label ',' pre_label label
				 ',' pre_label label EOS
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_INTEGER && t != type_REAL
			     && t != type_DP && t != type_ERROR ) {
			    syntax_error($1.line_num,$1.col_num,
		  "integer, real, or double precision expression required");
			  }
			  ref_label(&($3), LAB_GOTO);
			  ref_label(&($6), LAB_GOTO);
			  ref_label(&($9), LAB_GOTO);

			}
		;

/* 35 */
logical_if_stmt	:	if_handle executable_stmt
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error($1.line_num,$1.col_num,
					  "logical expression required");
			}
		;

/* 36 */
block_if_stmt	:	if_handle tok_THEN EOS
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error($1.line_num,$1.col_num,
					  "logical expression required");

		/* In picky mode warn if no name tag on block construct.
		   By this time $1 is the expr, not tok_IF, so line and
		   column must be those saved at lower-level productions.
		 */
			  if(curr_stmt_name == NULL &&
			     style_req_construct_name) {
			      warning(if_line_num,if_col_num,
				"Construct name missing from IF statement");
			  }
			}
		;

if_handle	:	f77_if_handle
			{
			    curr_stmt_name = NULL;
			}
		|	construct_spec f77_if_handle
			{
			  if(f77_construct_name) {
			    nonstandard($1.line_num,$1.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			  construct_name_seen=FALSE;
			  $$ = $2;
			}
		;

f77_if_handle	:	tok_IF '(' {complex_const_allowed = TRUE;}  expr ')'
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    if_line_num = $1.line_num; /* save location */
			    if_col_num = $1.col_num; /* for picky warnings */
			    $$ = $4; /* Inherit expr for type checking above */
			}
		;

/* 37 */
else_if_stmt	:	tok_ELSE tok_IF '(' {complex_const_allowed = TRUE;} expr ')'
			{
			    int t=datatype_of($5.TOK_type);

			    if(t != type_LOGICAL && t != type_ERROR)
				syntax_error($5.line_num,$5.col_num,
					  "logical expression required");

			    if(is_true(ID_EXPR,$5.TOK_flags)){
				use_variable(&($5));
			    }

			    complex_const_allowed = FALSE;
			    initial_flag = TRUE;
			}
			else_if_then
		;

else_if_then	:	tok_THEN EOS
			{
			    curr_stmt_name = NULL;
			}
		|	tok_THEN construct_name EOS
			{
			  if(f77_construct_name) {
			    nonstandard($2.line_num,$2.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
		;

/* 38 */
else_stmt	:	tok_ELSE EOS
			{
			    curr_stmt_name = NULL;
			}
		|	tok_ELSE construct_name EOS
			{
			  if(f77_construct_name) {
			    nonstandard($2.line_num,$2.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
		;

/* 39 */
end_if_stmt	:	tok_ENDIF EOS
			{
			    curr_stmt_name = NULL;
			}
		|	tok_ENDIF construct_name EOS
			{
			  if(f77_construct_name) {
			    nonstandard($2.line_num,$2.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
		;

                      /* F90 CASE construct:
                         SELECT CASE ( expr )
                         [ CASE ( case-value [, case-value ...] ) 
                            [ ... ] ]
                         [ CASE DEFAULT
                            [ ... ] ]
                         END SELECT
                      */
select_case_stmt:	select_handle '(' {complex_const_allowed = TRUE;} expr ')' EOS
			{
			    int t = datatype_of ($4.TOK_type);
			    if (t != type_ERROR) {
			        if (!is_case_type(t)) {
			            syntax_error($4.line_num,$4.col_num,
			"integer, character, or logical expression required");
			        }
			    }
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;
			    push_block(&($1),$1.tclass,construct,curr_stmt_name,NO_LABEL);
			}
		;

select_handle	:	tok_SELECTCASE
			{
			    curr_stmt_name = NULL;
			    if (f77_case_construct) {
				nonstandard($1.line_num,$1.col_num,0,0);
			    }
			    if( style_req_construct_name ) {
			      warning($1.line_num,$1.col_num,
			       "Construct name missing from SELECT statement");
			    }
			}
		|	construct_spec tok_SELECTCASE
			{
			    if (f77_case_construct) {
				nonstandard($2.line_num,$2.col_num,0,0);
			    }
			    $$ = $2;
			}
		;

case_stmt	:	case_handle EOS
		|	case_handle construct_name EOS
		;

case_handle	:	tok_CASE '(' case_values ')'
		;

case_values	:	case_value
		|	case_values ',' case_value
		;

case_value	:	case_value_primary
		|	case_value_primary ':' case_value_primary
			{
			    int t1 = datatype_of($1.TOK_type),
			        t2 = datatype_of($3.TOK_type);
			    if (t1 == type_LOGICAL || t2 == type_LOGICAL) {
			        syntax_error($2.line_num,$2.col_num,
			            "ranges of type LOGICAL not allowed here");
			    }
			    if (t1 != t2) {
			        syntax_error($3.line_num,$3.col_num,
			            "range boundaries must have the same type");
			    }
			}
		|	':' case_value_primary
			{
			    int t = datatype_of($2.TOK_type);
			    if (t == type_LOGICAL) {
			        syntax_error($2.line_num,$2.col_num,
			            "ranges may not have type LOGICAL bounds");
			    }
			}
		|	case_value_primary ':'
			{
			    int t = datatype_of($2.TOK_type);
			    if (t == type_LOGICAL) {
			        syntax_error($2.line_num,$2.col_num,
			            "ranges may not have type LOGICAL bounds");
			    }
			}
		;

case_value_primary:	expr
			{
			    int t = datatype_of($1.TOK_type);
			    if (t != type_ERROR) {
			        if (!is_case_type(t)) {
			            syntax_error($1.line_num,$1.col_num,
			"integer, character, or logical expression required");
			        }
			    }
			    if (!is_true(CONST_EXPR, $1.TOK_flags)) {
			        syntax_error($1.line_num,$1.col_num,
			"expression must evaluate to a compile-time constant");
			    }
			    $$ = $1;
			}
		;

case_default_stmt:	tok_CASEDEFAULT EOS
		|	tok_CASEDEFAULT construct_name EOS
		;

end_select_stmt	:	tok_ENDSELECT EOS
		|	tok_ENDSELECT construct_name EOS
		;


/* 40 */
			/* Allow F90 extensions:
			   DO [label [,]] var = expr , expr [,expr]
			   DO [label [,]] WHILE ( expr )
			      ...
			   ENDDO
			*/

do_stmt		:	do_handle variable_name
				'=' do_loop_bounds EOS
			{
			  if( ! is_true(LVALUE_EXPR,$2.TOK_flags) ) {
			    syntax_error($2.line_num,$2.col_num,
					 "index is not assignable");
			    if(is_true(CONST_EXPR,$2.TOK_flags))
				msg_tail(": it is a constant");
			    $$.value.integer = -1; /* no hash entry */
			  }
			  else {
			     def_do_variable(&($2));
				/* Store hash index of DO index in token for
				   use when pushing block on stack. The
				   value field is not used by keywords, so it
				   is OK to use it this way. */
			     $$.value.integer = $2.value.integer;
			  }

				/* Check for non-integer DO index or bounds */
			     if(datatype_of($2.TOK_type) == type_INTEGER
				&& datatype_of($4.TOK_type) != type_INTEGER) {
			       if( f95_real_do ) {
				 nonstandard($4.line_num,$4.col_num,0,f95_real_do);
				 msg_tail(": DO loop bounds not integer");
			       }
			       else if(misc_warn) {
				 warning($3.line_num,$3.col_num,
				  "type mismatch between DO index and bounds");
			       }
			     }
			     else if(datatype_of($2.TOK_type) != type_INTEGER) {
			       if( f95_real_do ) {
				 nonstandard($2.line_num,$2.col_num,0,f95_real_do);
				 msg_tail(": DO index is not integer");
			       }
			       else if(datatype_of($4.TOK_type) != type_INTEGER) {
				 if(port_real_do)
				   nonportable($4.line_num,$4.col_num,
					       "non-integer DO loop");
			       }
			       else {
				 if(trunc_real_do_index) {
				   warning($2.line_num,$2.col_num,
					   "DO index is not integer");
				 }
			       }
			     }
			}
		|	tok_DOWHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    $$.value.integer = -1; /* no DO index */
			    curr_stmt_name = NULL;
			}
		/* Normally, the lexer glues DO WHILE together and yields
		   tok_DOWHILE.  The following production is needed, however,
		   for e.g. DO 100 WHILE and constructname : DOWHILE */
		|	do_handle tok_WHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$5.TOK_flags)){
				use_variable(&($5));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    $$.value.integer = -1; /* no DO index */
			}
		|	do_handle EOS
			{
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    $$.value.integer = -1; /* no DO index */
			}
		;

do_handle	:	f77_do_handle
			{
		/* In picky mode warn if no name tag on block construct. */
			  if( style_req_construct_name ) {
			      warning($1.line_num,$1.col_num,
				"Construct name missing from DO statement");
			  }
			  curr_stmt_name = NULL;
			}
		|	construct_spec f77_do_handle
			{
			  if(f77_construct_name) {
			    nonstandard($1.line_num,$1.col_num,0,0);
			    msg_tail(": DO construct name");
			  }
			  construct_name_seen=FALSE;
			  $$ = $2;
			}
		;

f77_do_handle	:	tok_DO pre_label label
                        {
			    ref_label(&($3), LAB_DO);
			    def_do_label(&($3));
				/* Save label in subclass for push_block */
			    $$.tsubclass = $3.value.integer;
			}
		|	tok_DO pre_label label ','
                        {
                            ref_label(&($3), LAB_DO);
			    def_do_label(&($3));
			    $$.tsubclass = $3.value.integer;
			}
		|	tok_DO pre_label
			{
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    integer_context=FALSE;
			    $$.tsubclass = (long)NO_LABEL;
			}
		;

do_loop_bounds	:	int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.TOK_type=do_bounds_type(&($1),&($3),&($3));
			}
		|   int_real_dp_expr ',' int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.TOK_type=do_bounds_type(&($1),&($3),&($5));
			}
		;

enddo_stmt	:	tok_ENDDO EOS
			{
			  curr_stmt_name = NULL;
			}
		|	tok_ENDDO construct_name EOS
		;

/* 41 */
continue_stmt	:	tok_CONTINUE EOS
		;

/* F90 CYCLE and EXIT statements.  Note: at this time, the
   optional do-construct-name is not supported. */
cycle_or_exit_stmt:	cycle_stmt
		{
			   if( f77_cycle_exit ) {
			     nonstandard($1.line_num,$1.col_num,0,0);
			     msg_tail(": CYCLE statement");
			   }
		}
		|	exit_stmt
		{
			   if( f77_cycle_exit ) {
			     nonstandard($1.line_num,$1.col_num,0,0);
			     msg_tail(": EXIT statement");
			   }
		}
		;

cycle_stmt	:	tok_CYCLE EOS
		|	tok_CYCLE construct_name EOS
		;

exit_stmt	:	tok_EXIT EOS
		|	tok_EXIT construct_name EOS
		;

/* 42 */
stop_stmt	:	tok_STOP stop_info EOS
		;

/* 43 */
pause_stmt	:	tok_PAUSE stop_info EOS
			{
			  if( f95_pause ) {
			    nonstandard($1.line_num,$1.col_num,0,f95_pause);
			    msg_tail(": PAUSE statement");
			  }
			}
		;

stop_info	:	/* empty */
		|	tok_integer_const
		|	symbolic_name
			{
			     use_variable(&($1));
			}
		|	tok_string
		;

/* 44 */
write_stmt	:	write_handle
				{complex_const_allowed = FALSE;} EOS
		|	write_handle io_list
				{complex_const_allowed = FALSE;} EOS
		;

write_handle	:	tok_WRITE {init_io_ctrl_list();}
				'(' control_info_list ')'
				{complex_const_allowed = TRUE;}
		;

/* 45 */
		/* Note that parenthesized format_id's will end up in
		   control_info_list. Disambiguation left to semantic phase.
		   This is why we need the optional comma */
read_stmt	:	read_handle '(' control_info_list ')' EOS
		|	read_handle '(' control_info_list ')' io_list EOS
		|	read_handle '(' control_info_list ')' ',' io_list EOS
		|	read_handle format_id EOS
			{
			    record_default_io();
			}
		|	read_handle format_id ',' io_list EOS
			{
			    record_default_io();
			}
		;
read_handle	:	tok_READ {init_io_ctrl_list();}
		;

accept_stmt	:	tok_ACCEPT format_id EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
		|	tok_ACCEPT format_id ',' io_list EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
		;

/* 46 */
print_stmt	:	tok_PRINT format_id EOS
			{
			    record_default_io();
			}
   		|	tok_PRINT format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
			{
			    record_default_io();
			}
		;

type_output_stmt:	tok_TYPE type_format_id EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
   		|	tok_TYPE type_format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
		;

/* Because a general format_id raises conflicts with F90 (derived)
   TYPE statement, in TYPE output statements we only support format_id
   that is * or a literal constant (e.g. a format label or character
   string containing a format).
 */

type_format_id: '*'
		|	type_format_literal_const
		;


type_format_literal_const: tok_integer_const
			{
				ref_label(&($1),LAB_IO);
			}
		|	tok_string
		;

/* 47 */
control_info_list:	control_info_item
			{
			    ++control_item_count;
			}
		|	control_info_list ',' control_info_item
			{
			    ++control_item_count;
			    if(! io_warning_given) {
			      if( io_internal_file ) {
				if( (curr_stmt_class == tok_WRITE ||
				     curr_stmt_class == tok_READ) &&
				    io_list_directed ) {
				  if(f77_internal_list_io) {
				    nonstandard($3.line_num,$3.col_num,0,0);
	    msg_tail(": internal file cannot be used with list-directed I/O");
				  }
				  io_warning_given = TRUE;
				}
			      }
			    }
			}
		;

	/* Note that unit id is not distinguished from format id
	   by the grammar. Use sequence no. to tell which is which.
	 */
control_info_item:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	unit_id
			{
			  if(control_item_count == 0) /* unit id */
			  {
					/* Handle special cases */
			    if( datatype_of($1.TOK_type) == type_STRING ) {
					/* unit id=char variable is
					   an internal file.  I/O goes in
					   and out of the variable. */
			      if( is_true(ID_EXPR,$1.TOK_flags) ) {
				 io_internal_file = TRUE;
				 if(curr_stmt_class == tok_WRITE) {
				      use_lvalue(&($1));
				 }
			      }
			      else { /* internal unit must be a variable */
				syntax_error($1.line_num,$1.col_num,
					"internal file must be a variable");
			      }
			    }
			    else { /* Otherwise it is a normal external file unit id */
				 record_io_unit_id(&$1);
			    }
			  }
			  else if(control_item_count == 1) /* format id */
			  {
			    if( $1.tclass == '*' )
			    {
				 io_list_directed = TRUE;
			    }
			    else if( is_true(ID_EXPR,$1.TOK_flags)){
				 if(datatype_of($1.TOK_type) == type_NAMELIST) {
				   ref_namelist(&($1),curr_stmt_class);
				 }
				 else
				     /* format id=integer variable is assigned format */
				   if( datatype_of($1.TOK_type) == type_INTEGER) {
				     if( f95_assign ) {
				       nonstandard($1.line_num,$1.col_num,0,f95_assign);
				       msg_tail(": assigned format");
				     }
				   }
			    }
				/* An integer at this point is a format label */
			    else if ( is_true(LIT_CONST,$1.TOK_flags) &&
				      $1.TOK_type == type_pack(class_VAR,type_INTEGER))
			    {
				 ref_label(&($1),LAB_IO);
			    }
			    current_io_form = IO_FORM_FORMATTED;
  			  }
					/* Handle use of variable */
			  if( is_true(ID_EXPR,$1.TOK_flags)){
			       use_variable(&($1));
			  }
			}
		;

			/* OPEN stmt needs its own control list defn to
			   allow for VMS READONLY and similar keywords.
			   Special prodn for unit_id as optional 1st item
			   needed to avoid reduce/reduce conflict with
			   later-occurring symbolic_name items.   */
open_info_list	:	unit_id
			{
			    if( $1.tclass != '*'
			       && is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if(control_item_count == 0)
			    {
			       record_io_unit_id(&($1));
			    }
			    ++control_item_count;
			}
		|	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			    ++control_item_count;
			}
		|	open_info_list ',' open_info_item
			{
			    ++control_item_count;
			}
		;

open_info_item	:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	symbolic_name	/* NOSPANBLOCKS, READONLY or SHARED */
			{
			    use_special_open_keywd(&($1));
			}
		;

/* 48 */
io_list		:	io_item
		|	io_list ',' io_item
		;

io_item		:	expr
			{
			    if( curr_stmt_class == tok_READ ||
				curr_stmt_class == tok_ACCEPT ) { /* Input */
				if(is_true(LVALUE_EXPR,$1.TOK_flags)) {
				    use_lvalue(&($1));
				}
				else {
				    syntax_error($1.line_num,$1.col_num,
						 "item is not assignable");
				/* Give hint if it is a parameter */
				    if(is_true(ID_EXPR,$1.TOK_flags) &&
				       is_true(CONST_EXPR,$1.TOK_flags))
					msg_tail(": it is a constant");
				}
			    }
			    else {				 /* Output */
				if(is_true(ID_EXPR,$1.TOK_flags)){
				    use_variable(&($1));
				}
			    }
			}
		|	io_implied_do_list
		;

/* 49 */
io_implied_do_list:	'(' io_list ',' implied_do_control ')'
		;

/* 50 */
open_stmt	:	tok_OPEN {init_io_ctrl_list();}
				 '(' open_info_list ')' EOS
		;

/* 51 */
close_stmt	:	tok_CLOSE {init_io_ctrl_list();}
				'(' control_info_list ')' EOS
		;

/* 52 */
inquire_stmt	:	tok_INQUIRE {init_io_ctrl_list();}
				'(' control_info_list ')' EOS
		;

/* 53 */
backspace_stmt	:	backspace_handle unit_id EOS
			{
			    if( $2.tclass != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			    record_io_unit_id(&$2);
			}
		|	backspace_handle '(' control_info_list ')' EOS
		;
backspace_handle:	tok_BACKSPACE {init_io_ctrl_list();}
		;

/* 54 */
endfile_stmt	:	endfile_handle unit_id EOS
			{
			    if( $2.tclass != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			    record_io_unit_id(&$2);
			}
		|	endfile_handle '(' control_info_list ')' EOS
		;
endfile_handle	:	tok_ENDFILE {init_io_ctrl_list();}
		;

/* 55 */
rewind_stmt	:	rewind_handle unit_id EOS
			{
			    if( $2.tclass != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			    record_io_unit_id(&$2);
			}
		|	rewind_handle '(' control_info_list ')' EOS
		;
rewind_handle	:	tok_REWIND {init_io_ctrl_list();}
		;


/* 56 */
		/* "expr" causes shift/reduce conflict on ')' between
		   red'n  unit_id: expr_  and shift  primary: ( expr_ ).
		   Use "associativity" rule to force reduction */
unit_id		:	expr		%prec REDUCE
		|	'*'
		;

/* 57 */
format_id	:	char_expr
			{
			  if(is_true(ID_EXPR,$1.TOK_flags)){
			    use_variable(&($1));
				/* If integer, format_id is assigned format */
			    if( datatype_of($1.TOK_type) == type_INTEGER ) {
			      if( f95_assign ) {
				nonstandard($1.line_num,$1.col_num,0,f95_assign);
				msg_tail(": assigned format");
			      }
			    }
			  }
			     /* A format label appears here as integer const */
			  else if(is_true(LIT_CONST,$1.TOK_flags) &&
			    $1.TOK_type == type_pack(class_VAR,type_INTEGER)){
			      ref_label(&($1),LAB_IO);
			  }
			}
		|	'*'
		;

/* 58,59 */
format_stmt	:	tok_FORMAT {inside_format=TRUE;} '(' format_spec ')' EOS
			{
			  inside_format=FALSE;
			}
		;

/* 60-69 */
format_spec	:		/* EMPTY */
		|	nonempty_format_spec
		;


nonempty_format_spec:	fmt_spec_item
		|	nonempty_format_spec fmt_spec_item
		;

fmt_spec_item	:	repeatable_fmt_item
		|	unrepeatable_fmt_item
		|	fmt_item_separator
		;

repeatable_fmt_item:	'(' nonempty_format_spec ')'
		|	tok_edit_descriptor
		;

unrepeatable_fmt_item:	tok_string
		|	tok_hollerith
			{
			  if( f95_Hedit ) {
			    nonstandard($1.line_num,$1.col_num,0,f95_Hedit);
			    msg_tail(": H edit descriptor");
			  }
			}
		|	repeat_spec
		|	variable_fmt_item
		;

fmt_item_separator:	','
		|	'/'
		|	tok_concat	/* since lexer spots "//" */
		|	':'
		|	tok_double_colon /* not likely but not illegal */
		|	'.'		/* Occurs when variable w.d is used */
		|	nonstandard_fmt_item
			{
			  if(f77_format_dollarsigns || f90_format_dollarsigns)
			     nonstandard($1.line_num,$1.col_num,f90_format_dollarsigns,0);
			}
		;

nonstandard_fmt_item: '$'	/* VMS uses this */
		;

repeat_spec	:	tok_integer_const
		|	'-' tok_integer_const	/* for kP descriptor */
		|	'+' tok_integer_const	/* for +kP descriptor */
		;

		/* VMS-style variable format size or repeat spec*/
variable_fmt_item:	'<' {inside_format=FALSE;} integer_expr
				{inside_format=TRUE;} '>'
			{
			  if(f77_variable_format || f90_variable_format)
			     nonstandard($1.line_num,$1.col_num,f90_variable_format,0);
			  msg_tail(": variable format");
			}
		;

/* 70 handle only: complete defn handled as assignment stmt */

stmt_function_handle:	scalar_name '(' stmt_function_dummy_list ')'
			{
			  check_stmt_sequence(&($1),SEQ_STMT_FUN);
			  check_f90_stmt_sequence(&($1),F90_SEQ_SPECIF);

				def_stmt_function(&($1),&($3));
					/* make token info */
				primary_id_expr(&($1),&($$));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&($3));
#endif
			}
		;

stmt_function_dummy_list: /* empty list */
			{
			    $$.next_token = (Token*)NULL;
			}
		| nonempty_stmt_fun_dummy_list
		;

nonempty_stmt_fun_dummy_list:	  stmt_function_dummy_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	  nonempty_stmt_fun_dummy_list ','
					stmt_function_dummy_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

stmt_function_dummy_arg:  variable_name	/* for now: later, handle correctly */
		;

/* 71 */
call_stmt	:	call_handle
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' ')'
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' subr_arg_list ')'
			{
			     call_subr(&($1),&($3));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&($3));
#endif
			     complex_const_allowed = FALSE;
			} EOS
		;

call_handle	:	tok_CALL symbolic_name
			{
			     complex_const_allowed = TRUE;
			     $$ = $2;
			}
		;
subr_arg_list:		subr_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *)NULL;
			}
		|	subr_arg_list ',' subr_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

subr_arg	:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				 use_actual_arg(&($1));
				 use_variable(&($1));
			    }
			}
		|	'*' pre_label label
			{
			    ref_label(&($3), LAB_CALL);  
			  $$ = $3;
			  $$.left_token = (Token *)NULL;
			}
		|	symbolic_name '=' expr
			{
			    /* Currently passing expr token to prevent
			    unwanted error messages. Further work
			    needs to be done for proper semantic
			    checking.
			    */

			    $$ = $3;
			}
		;

/* 72 */
return_stmt	:	tok_RETURN EOS
			{
			  (void)do_RETURN(current_prog_unit_hash,&($1));
			}
		|	tok_RETURN integer_expr EOS
			{
			  if( do_RETURN(current_prog_unit_hash,&($1)) ) {

				/* Warn if alternate return value is a constant
				   that is not between 0 and the number of
				   labels that are dummy-arguments.
				 */
			     if( pretty_alt_return &&
				(is_true(EVALUATED_EXPR,$2.TOK_flags) &&
			        ($2.value.integer < 1 ||
				  $2.value.integer > label_dummy_arg_count)) ){
				 warning($2.line_num,$2.col_num,
					 "alternate return value");
				 msg_tail(ulongtostr($2.value.integer));
				 if( $2.value.integer < 0 ) {
				   msg_tail("is negative");
				 }
				 else {
				   msg_tail("exceeds");
				   msg_tail(ulongtostr(label_dummy_arg_count));
				   msg_tail("= number of alternative return points");
				 }
			     }
			     else {
				/* Style warning is under goto rubric */
			       if( style_goto ) {
				 warning($1.line_num,$1.col_num,
				     "obsolescent feature: alternate return");
			       }
			     }
			  }
			}
		;

/* 73 */
function_reference:	fun_or_substr_handle '(' fun_arg_list ')'
			{
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,$1.TOK_flags))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,$1.TOK_flags))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if($3.next_token == NULL)
				  call_func(&($1),(Token *)NULL);
				else
				  call_func(&($1),&($3));
							/* make token info */
				func_ref_expr(&($1),&($3),&($$));
				/* Substitute empty token for null arglist */
				$$.left_token = add_tree_node(
						   &($2),&($1),
						   ($3.next_token == NULL?
						    empty_token(&($3)) :
						    $3.next_token) );
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&($3));
#endif
			}
		;

fun_or_substr_handle:	scalar_name
			{
			  if(complex_const_allowed)/* save context */
			    make_true(COMPLEX_FLAG,$$.TOK_flags);
			  complex_const_allowed=TRUE;
			  if(in_assignment_stmt)
			    make_true(IN_ASSIGN,$$.TOK_flags);
			  in_assignment_stmt = FALSE;
			}
		;
fun_arg_list	:	/* empty */
			{
				$$.tclass = 0;
				$$.next_token = (Token *)NULL;
				$$.left_token = (Token *)NULL;
			}
		|	nonempty_fun_arg_list
		;

nonempty_fun_arg_list:	fun_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *)NULL;
			}
		|	nonempty_fun_arg_list ',' fun_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}

		;

fun_arg		:	expr
		|	symbolic_name '=' expr
			{
			    /* Currently passing expr token to prevent
			    unwanted error messages. Further work
			    needs to be done for proper semantic
			    checking.
			    */

			    $$ = $3;
			}
		;

/* 74 not present: type checking not done at this level */

/* 75 was constant_expr, but only used by PARAMETER and initializers */
parameter_expr	:	/* arith, char, or logical */ expr
			{
			  int t=datatype_of($1.TOK_type);
			  if( t != type_ERROR){
			    if( !is_param_type(t) ) {
			      syntax_error($1.line_num,$1.col_num,
		      "arithmetic, char, logical, or structured type expression expected");
			    }
			    else {
			      if( !is_true(PARAMETER_EXPR,$1.TOK_flags) && 
				  !is_derived_type(t)) {
				syntax_error($1.line_num,$1.col_num,
					   "constant expression expected");
			      }
			    /* Here we allow, with some warnings, expr
			       containing intrins func or **REAL in
			       PARAMETER defn. */
			      else if( !is_true(CONST_EXPR,$1.TOK_flags) ) {
				if(f77_param_intrinsic) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(
			 "intrinsic function or **REAL in PARAMETER defn");
				}
			      }
			    }
			  }
			}
		;

/* 76 following the text of the standard, not the diagrams */
expr		:	log_expr
			{
#ifdef DEBUG_PARSER
			    if(debug_parser) {
				(void)fprintf(list_fd,
					"\nexpr: class=0x%lx subclass=0x%lx",
					$1.tclass,
					$1.tsubclass);
			    }
#endif
			}
		;

log_expr	:	log_disjunct

		|	expr tok_EQV log_disjunct
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		|	expr tok_NEQV log_disjunct
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_disjunct	:	log_term

		|	log_disjunct tok_OR log_term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_term	:	log_factor

		|	log_term tok_AND log_factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_factor	:	log_primary

		|	tok_NOT log_primary
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		;

log_primary	:	arith_expr

		|	log_primary tok_relop log_primary
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;


arith_expr	:	term

		|	'-' term
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		|	'+' term
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		|	arith_expr '+' term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		|	arith_expr '-' term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

term		:	factor

		|	term '/' factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			    if(div_check &&
			       !is_true(CONST_EXPR,$3.TOK_flags)){
				warning($2.line_num,$2.col_num,
					"Possible division by zero");
			    }
			}
		|	term '*' factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

factor		:	char_expr

		|	char_expr tok_power factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

char_expr	:	primary

		|	char_expr tok_concat primary
			{
			  do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

primary		:	data_object
		|	compound_object
		;

/* items that can be allocated, nullified etc.  (Used in other productions) */
data_object	:	variable_name
			{
			    make_true(DIM_BOUND_EXPR,$$.TOK_flags);
			}
		|	array_element_name

		|	component /* derived type component */
			{
			    ref_component(&($1),&($$),FALSE);
			}
		;

/* primaries that are not allocatable things */
compound_object	:	function_reference

		|	substring_name
			{
			    check_token(&($1));
			}

		|	literal_const
			{
			    $$.TOK_flags = 0;
			    $$.left_token = (Token *)NULL;
			    make_true(CONST_EXPR,$$.TOK_flags);
			    make_true(PARAMETER_EXPR,$$.TOK_flags);
			    make_true(LIT_CONST,$$.TOK_flags);
			    make_true(EVALUATED_EXPR,$$.TOK_flags);
			    make_true(DIM_BOUND_EXPR,$$.TOK_flags);
			    $$.array_dim = array_dim_info(0,0);
			}

		|	array_constructor
			{
			    /* array constructor is an array of rank 1 */
			    make_true(ARRAY_EXPR,$$.TOK_flags);
			}

		|	'(' expr ')'
			{
			    $$ = $2;
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,$2.TOK_flags)) {
				if(pretty_parens) {
				  ugly_code($2.line_num,$2.col_num,
					  "Extraneous parentheses");
				}
				use_variable(&($2));
				make_false(LVALUE_EXPR,$$.TOK_flags);
				make_false(ARRAY_EXPR,$$.TOK_flags);
				make_false(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				make_false(ID_EXPR,$$.TOK_flags);
				make_false(DO_VARIABLE,$$.TOK_flags);
			    }
				/* (expr) becomes tree node with root = '(' */
			    $$.left_token = add_tree_node(&($1),&($2),
							  (Token*)NULL);
			}
		;

array_constructor:	tok_l_ac_delimiter ac_value_list tok_r_ac_delimiter
			{
			    $$.array_dim = $1.array_dim = $2.array_dim;
			    $2.next_token = reverse_tokenlist($2.next_token);
			    $$.left_token = add_tree_node(&($1),$2.next_token,(Token*)NULL);
			    $$.TOK_type = $2.TOK_type;
			    $$.size = $2.size;
			    $$.kind = $2.kind;
			    $$.TOK_flags = 0;
			    copy_flag(CONST_EXPR,$$.TOK_flags,$2.TOK_flags);
			    copy_flag(PARAMETER_EXPR,$$.TOK_flags,$2.TOK_flags);
			}

		|	'[' ac_value_list ']' /* F2010 alternative form */
			{
			    $$.array_dim = $1.array_dim = $2.array_dim;
			    $2.next_token = reverse_tokenlist($2.next_token);
			    $1.tclass = tok_l_ac_delimiter; /* use same as above */
			    $$.left_token = add_tree_node(&($1),$2.next_token,(Token*)NULL);
			    $$.TOK_type = $2.TOK_type;
			    $$.size = $2.size;
			    $$.kind = $2.kind;
			    $$.TOK_flags = 0;
			    copy_flag(CONST_EXPR,$$.TOK_flags,$2.TOK_flags);
			    copy_flag(PARAMETER_EXPR,$$.TOK_flags,$2.TOK_flags);
			}
		;

			    /* Each ac_value in ac_value_list shall have
			    same type and kind.  If a character type,
			    must also have same length.
			    */

ac_value_list	:	ac_value
	      		{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
	 	|	ac_value_list ',' ac_value
			{
			    if( array_size_is_unknown($1.array_dim) ||
			        array_size_is_unknown($3.array_dim) ) {
				$$.array_dim = array_dim_info_unk_size(1);
			    }
			    else {
				$$.array_dim = array_dim_info(1,
				    array_size($1.array_dim) + 
				    array_size($3.array_dim));
			    }

			    $$.next_token = append_token($1.next_token,&($3));
			    $2.tsubclass = tok_l_ac_delimiter; /* hint for warnings */
			    binexpr_type(&($1),&($2),&($3),&($$));
			}
		;

ac_value	:	make_ac_value
	 		{
			    /* change scalar to 1 dimensional array */
			    if( array_dims($1.array_dim) == 0 ) {
				$$.array_dim = array_dim_info(1,1);
			    }
			    else {
				if( array_size_is_unknown($1.array_dim) ) {
				    $$.array_dim =
				        array_dim_info_unk_size(1);
				}
				else {
				    $$.array_dim = array_dim_info(1,
					array_size($1.array_dim));
				}
			    }
			}

make_ac_value	:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
			      use_variable(&($1));
			    }
			}
	 	|	ac_implied_do
	 	;

ac_implied_do	:	'(' ac_value_list ',' implied_do_control ')'
	      		{
			    if( array_size_is_unknown($2.array_dim) ||
				$4.value.integer == size_UNKNOWN ) {
			      $$.array_dim = array_dim_info_unk_size(1);
			    }
			    else {
			      $$.array_dim = array_dim_info(1,
				    array_size($2.array_dim)*$4.value.integer);
			    }
			    $2.next_token = reverse_tokenlist($2.next_token);
			    /* change token class to tok_DO for tree */
			    $$.tclass = $3.tclass = tok_DO;
			    $$.left_token = add_tree_node(&($3),&($2),&($4));
			    $$.TOK_type = $2.TOK_type;
			    $$.size = $2.size;
			    $$.TOK_flags = 0;
			    copy_flag(CONST_EXPR,$$.TOK_flags,$2.TOK_flags);
			    copy_flag(PARAMETER_EXPR,$$.TOK_flags,$2.TOK_flags);
			}
	      	;


				/* Literal constants are numbers, strings
				   holleriths, and logical constants */
literal_const	:	numeric_const
			    /* (class, size set in numeric_const productions) */
		|	char_literal_const
			{
			    $$.TOK_type = type_pack(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			}
		|	tok_hollerith
			{
			    $$.TOK_type = type_pack(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    $$.kind = default_kind(type_INTEGER);
			    if(port_hollerith) {
				warning($1.line_num,$1.col_num,
				"hollerith constant may not be portable");
			    }
			}
		|	tok_logical_const non_char_kind_param
			{
			    $$.TOK_type = type_pack(class_VAR,type_LOGICAL);
			    $$.size = size_DEFAULT;
			    $$.kind = ($2.kind == kind_DEFAULT_UNKNOWN) ?
			    	default_kind(type_LOGICAL) : $2.kind;
			}
		;

numeric_const	:	numeric_literal_const
		|	complex_const
		;

numeric_literal_const:	tok_integer_const non_char_kind_param
			{
			    $$.TOK_type = type_pack(class_VAR,type_INTEGER);
			    $$.size = size_DEFAULT;
			    $$.kind = ($2.kind == kind_DEFAULT_UNKNOWN) ?
			    	default_kind(type_INTEGER) : $2.kind;
			}
		|	tok_real_const non_char_kind_param
			{
			    $$.TOK_type = type_pack(class_VAR,type_REAL);
			    $$.size = size_DEFAULT;
			    $$.kind = ($2.kind == kind_DEFAULT_UNKNOWN) ?
			    	default_kind(type_REAL) : $2.kind;
			}
		|	tok_dp_const
			{
			    $$.TOK_type = type_pack(class_VAR,type_DP);
			    $$.size = size_DEFAULT;
			    $$.kind = default_kind(type_DP); /* no kind param allowed */
			}
		|	tok_quad_const
			{
			    $$.TOK_type = type_pack(class_VAR,type_QUAD);
			    $$.size = size_QUAD;
			    $$.kind = kind_DEFAULT_QUAD;
                            if(f77_quad_constants || f90_quad_constants) {
                              nonstandard($1.line_num,$1.col_num,f90_quad_constants,0);
                              msg_tail(": quad precision constant");
                            }
			}
		;

complex_const	:	tok_lparen signed_numeric_literal_const ',' signed_numeric_literal_const ')'
			{
			    int real_type, imag_type;
			    int token_class, final_type, final_size;
			    kind_t real_kind, imag_kind, final_kind;

			    /* datatype is stored in tclass as
			     * token class.
			     */
			    real_type = $2.tclass;
			    imag_type = $4.tclass; 
			    real_kind = $2.kind;
			    imag_kind = $4.kind;

			    /* integer is promoted to default real */
			    if( real_type == tok_integer_const ) {
				real_type = tok_real_const;
				real_kind = kind_DEFAULT_REAL;
			    }
			    if( imag_type == tok_integer_const ) {
				imag_type = tok_real_const;
				real_kind = kind_DEFAULT_REAL;
			    }

			    /* if same types then complex constant gets
			    kind of the part with greater decimal
			    precision */
			    if (real_type == imag_type) {
				/* Note that quad precision is
				 * type_REAL with its own default
				 * kind.  The next step works if both
				 * parts are default kinds.  The std
				 * does not address quad.  We decree
				 * that it beats all other real
				 * non-default kinds.
				 */
				if( real_kind == kind_DEFAULT_QUAD ||
				    imag_kind == kind_DEFAULT_QUAD ) {
				    final_kind = kind_DEFAULT_QUAD;
				}
				else {
				/* Either default or specified real
				 * kind.  Note that if precision is
				 * not specified it comes back -1.
				 * Both parts d.p. is also handled here:
				 * they will be both default dp kind.
				 */
				    final_kind =
					( kind_precision(real_kind) >=
					  kind_precision(imag_kind) ) ?
					real_kind : imag_kind;
				}
				/* Warn if const mixes concrete & not,
				   or different concrete kinds.
				 */
				if(port_mixed_kind) {
				    if( real_kind != imag_kind &&
				       (real_kind >= 0 || imag_kind >= 0) ){
					nonportable($3.line_num,$3.col_num,
						"complex constant mixes concrete kind with");
					if(real_kind >= 0 && imag_kind >= 0)
					    msg_tail("different concrete kind");
					else
					    msg_tail("non-concrete kind");
				    }
				}
			    }
			    else {/* one & only one part double precision */
				/* Result has the higher precision,
				 * but we do not presume to know what
				 * the precision of d.p. is, so if
				 * other part is specified real kind,
				 * warn under -port.  Assume dp wins. */
			        final_kind = kind_DEFAULT_DP;
				if( port_mixed_kind ) {
				    if( real_kind != kind_DEFAULT_REAL &&
					imag_kind != kind_DEFAULT_REAL ) {
					nonportable($3.line_num,$3.col_num,
						"complex constant mixes default D.P. and specified real kinds");
					msg_tail(": assuming D.P. more precise");
				    }
				}
			    }

			    token_class = tok_complex_const;
			    final_type = type_COMPLEX;
			    final_size = size_DEFAULT;

			    /* now promote if either part is higher */
			    if( real_type == tok_dp_const ||
				imag_type == tok_dp_const ) {
				token_class = tok_dcomplex_const;
				final_type = type_DCOMPLEX;
				/* F77 disallows d.p. complex.  We don't
				 * however warn about quad complex.  It
				 * is warned separately under quad consts. */
				if(f77_double_complex) {
				  warning($1.line_num,$1.col_num,
			    	    "nonstandard double precision complex constant");
				}
			    }
			    if( real_type == tok_quad_const ||
				imag_type == tok_quad_const ) {
				final_type = type_COMPLEX;
				final_size = size_QUAD;
			    }

			    $$ = $2;	/* save value of real part only */
			    $$.tclass = token_class;
			    $$.TOK_type = type_pack(class_VAR,final_type);
			    $$.kind = final_kind;
			    $$.size = final_size;

			    /* Reconstitute entire complex constant string in
			       source text for error messages.  (Do
			       not save as a tree.)
			    */
			    $$.src_text = new_src_text_alloc( 3 +
			    			strlen($2.src_text) + 
						strlen($4.src_text) + 1 );
			    strcpy($$.src_text, $1.src_text);
			    strcat($$.src_text, $2.src_text);
			    strcat($$.src_text, $3.src_text);
			    strcat($$.src_text, $4.src_text);
			    strcat($$.src_text, $5.src_text);
			}
		;


signed_numeric_literal_const: numeric_literal_const
		|	 '+' numeric_literal_const
			{
			    $$ = $2;

			    /* create src_text field for the numeric
			       constant token which will include
			       the plus sign
			    */
			    $$.src_text = new_src_text_alloc( 2 + 
			    			strlen($2.src_text) );
			    strcpy($$.src_text, $1.src_text);
			    strcat($$.src_text, $2.src_text);
			}
		|	 '-' numeric_literal_const
			{
			    $$ = $2;
			    $$.value.dbl = -($$.value.dbl);

			    /* create src_text field for the numeric
			       constant token which will include
			       the minus sign
			    */
			    $$.src_text = new_src_text_alloc( 2 + 
			    			strlen($2.src_text) );
			    strcpy($$.src_text, $1.src_text);
			    strcat($$.src_text, $2.src_text);
			}
		;

char_literal_const:	char_kind_param tok_string
			{
			    $$ = $2;
			    $$.kind = $1.kind;
			}
		;

char_kind_param	:	kind_param tok_underscore
			{
			    /* used in character constants */
			}
		|
			{
			    /* empty production */
			    $$.kind = kind_DEFAULT_CHARACTER;
			}
		;

non_char_kind_param:	'_' kind_param
			{
			    /* used in numeric and logical constants */
			    $$ = $2;
			}
		|
			{
			    /* empty production */
			    $$.kind = kind_DEFAULT_UNKNOWN;
			}
		;

kind_param	:	tok_integer_const
	   		{
			    /* has to be nonnegative integer */
			    $$.kind = $1.value.integer;
			}
	   	|	tok_identifier
			{
			    primary_id_expr(&($1),&($$));
			    /* has to be scalar nonnegative integer */
			    /* the identifier could be undefined */
			    $$.kind = int_expr_value(&($$));
			}
	   	;

/* 77 */
integer_expr	:	/* integer */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if(datatype_of($1.TOK_type) != type_INTEGER) {
				syntax_error(
					$1.line_num,$1.col_num,
					"expression must be integer type");
			    }
			}
		;

/* 78 */
int_real_dp_expr:	/* integer, real, or double */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    {
				int t=datatype_of($1.TOK_type);
				    if(t != type_INTEGER && t != type_REAL
					&& t != type_DP ) {
					syntax_error(
					  $1.line_num,$1.col_num,
		"expression must be integer, real, or double precision type");
			    	    }
			    }
			}
		;

/* 79 absent */

/* 80 */
int_constant_expr:	/* integer const */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if( ! is_true(CONST_EXPR,$1.TOK_flags) ) {
				syntax_error(
					$1.line_num,$1.col_num,
					"constant expression expected");
			    }
			    else {
			      if(datatype_of($1.TOK_type) != type_INTEGER){
				syntax_error(
					$1.line_num,$1.col_num,
					"integer expression expected");
			      }
			      else {
				$$.value.integer = int_expr_value(&($1));
			      }
			    }
			}
		;

/* 81 */
dim_bound_expr	:       /* integer */  arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }

			    if(f77_array_bounds) {	/* Section 5.1.1.1 */
			      if( !is_true(DIM_BOUND_EXPR,$1.TOK_flags) ) {
				nonstandard($1.line_num,$1.col_num,0,0);
				msg_tail(
		": array bounds expr cannot have array or function reference");
			      }
			    }

			    if( datatype_of($1.TOK_type) != type_INTEGER ){
				syntax_error(
					$1.line_num,$1.col_num,
					"integer dimension expected");
				$$.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,$1.TOK_flags) )
				$$.value.integer =
				  int_expr_value(&($1));
			      else		/* must be dummy */
				$$.value.integer = 0;
			    }
			}
		;

/* 82-85 absent: no type checking here */
/* 86-87 absent: see 76 */

/* 88 */

array_element_lvalue:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&($3));
#endif
				$$.array_dim = subarray_size(&($1),&($3));
					/* array now becomes scalar */
				if (array_dims($$.array_dim) == 0) {
				    make_false(ARRAY_EXPR,$$.TOK_flags);
				    make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				}

				$$.left_token = add_tree_node(
					   &($2),&($1),$3.next_token);
				$$.next_token = (Token *) NULL;
			}
		;

array_element_name:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&($3));
#endif
				$$.array_dim = subarray_size(&($1),&($3));
					/* array now becomes scalar */
				if (array_dims($$.array_dim) == 0) {
				    make_false(ARRAY_EXPR,$$.TOK_flags);
				    make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				}
				
				$$.left_token = add_tree_node(
					   &($2),&($1),$3.next_token);
				$$.next_token = (Token *) NULL;
			}
		;


subscript_list	:	subscript
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *) NULL;/* not meaningful in list header */
			}
		|	subscript_list ',' subscript
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		     ;

subscript	:	bounds_expr  /* array element */
		|	bounds_range /* array section */
	  		{
			/* the left token of bounds_range contains
			   a tree whose root is ':' and children
			   are the bounds values
			 */
	  		}

		|	bounds_range ':' bounds_expr /* array section with
							stride */
	  		{
			    $$.left_token = add_tree_node(&($2),&($1),&($3));
			    $$.next_token = (Token *) NULL;
	  		}

		;

/* 89 */
substring_name	:	fun_or_substr_handle  '(' bounds_range ')'
			{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,$1.TOK_flags))
				  complex_const_allowed=FALSE;
				/* set flag to keep more than just id for
				   arg list text */
			    if(is_true(ID_EXPR,$1.TOK_flags))
			       make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
			    $$.size=substring_size(&($1),&($3));
			    $$.left_token = add_tree_node(
					       &($2),&($1),&($3));
			    $$.next_token = (Token *) NULL;
			}

		|	function_reference  '(' bounds_range ')'
			{
			    $$.size=substring_size(&($1),&($3));
			    $$.left_token = add_tree_node(
					       &($2),&($1),&($3));
			    $$.next_token = (Token *) NULL;
			}

		|	array_element_name '(' bounds_range ')'
			{
			    $$.size=substring_size(&($1),&($3));
			    $$.left_token = add_tree_node(
					       &($2),&($1),&($3));
			    $$.next_token = (Token *) NULL;
			}
		;

substring_lvalue:	scalar_name '(' bounds_range ')'
			{
			    ref_variable(&($1));
			    $$.TOK_flags = $1.TOK_flags;
			    $$.size=substring_size(&($1),&($3));
			    $$.left_token = add_tree_node(
					       &($2),&($1),&($3));
			    $$.next_token = (Token *) NULL;
			}
		|	array_element_lvalue '(' bounds_range ')'
			{
			    $$.size=substring_size(&($1),&($3));
			    $$.left_token = add_tree_node(
					       &($2),&($1),&($3));
			    $$.next_token = (Token *) NULL;
			}
		;


bounds_range	:	':'
			{
			    Token empty = $1;

			    (void)empty_token(&empty);
			    $$.left_token =
			      add_tree_node(&($1),&empty,&empty);
				/* Nullify next_token so it looks like
				   a tokenlist */
			    $$.next_token = (Token *)NULL;
			}

		  |	bounds_expr ':'
			{
			    Token empty = $2;

			    (void)empty_token(&empty);
			    $$.left_token =
			      add_tree_node(&($2),&($1),&empty);
			    $$.next_token = (Token *)NULL;
			}
		  |	':' bounds_expr
			{
			    Token empty = $1;

			    (void)empty_token(&empty);
			    $$.left_token =
			      add_tree_node(&($1),&empty,&($2));
			    $$.next_token = (Token *)NULL;
			}
		  |	bounds_expr ':' bounds_expr
			{
			    $$.left_token = add_tree_node(&($2),&($1),&($3));
			    $$.next_token = (Token *)NULL;
			}
		  ;

bounds_expr	:	integer_expr
	    		{
			  int bounds_val;
				/* store value, replacing nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,$1.TOK_flags)) {
			    bounds_val=int_expr_value(&($1));
			  }
			  else { /* (no longer need ID hash index) */
			    bounds_val=size_UNKNOWN;
			  }
			  if( $$.left_token == NULL ) {
			    /* bounds expr is a primary */
			    $$.value.integer = bounds_val;
			    make_false(ID_EXPR,$$.TOK_flags);
			  }
			  else {
			    /* non-primary, need to store value in
			     * what will be the root when tied to : */
			    $$.left_token->value.integer = bounds_val;
			    make_false(ID_EXPR,$$.left_token->TOK_flags);
			  }
			}
		;

/* 90-98 absent: name categories not distinguished */

/* 99 */
variable_name	:	scalar_name
			{
			    ref_variable(&($1));
			    $$.TOK_flags = $1.TOK_flags;
			}
		|	array_name
		;

scalar_name	:	tok_identifier
			{
			    ref_identifier(&($1));
			    primary_id_expr(&($1),&($$));
			    set_attr_flags(&($1),&($$));
			}
		;

array_name	:	tok_array_identifier
			{
			    ref_variable(&($1));
			    primary_id_expr(&($1),&($$));
			    set_attr_flags(&($1),&($$));
			}
		;

/* Components of derived type.  Base case needs to include the '%'
   to avoid shift-reduce conflict with function reference. */
component	:	variable_name '%' component_name
			{
			    $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	base_array_name '%' component_name
			{
			    $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	component '%' component_name
			{
			    $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		;

base_array_name :	array_name '(' subscript_list ')'
			{
			  /* list is built in reverse order, restore original */
			    $3.next_token = reverse_tokenlist($3.next_token);
			    $$.left_token = add_tree_node(
				    &($2),&($1),$3.next_token);
			    $$.next_token = (Token *) NULL;
			}
		;

component_name	:	tok_identifier
		|	tok_identifier '(' subscript_list ')'
			{
			    $3.next_token = reverse_tokenlist($3.next_token);
			    $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	tok_identifier '(' subscript_list ')'
			'(' bounds_range ')'
			{
			    $3.next_token = reverse_tokenlist($3.next_token);
			    $4.left_token = add_tree_node(&($2),&($1),&($3));
			    $$.left_token = add_tree_node(&($5),&($4),&($6));
			}
		;

/* symbolic_name refers to a name without making it into an id expr */
symbolic_name	:	tok_identifier
		|	tok_array_identifier
		;


construct_spec	:	construct_name ':'
			{
			  construct_name_seen=TRUE;
				/* remember the name for block balancing */
			  curr_stmt_name = hashtab[$1.value.integer].name;
			}
		;

construct_name	:	tok_identifier
			{
			  curr_stmt_name = hashtab[$1.value.integer].name;
			}
		|	tok_array_identifier
			{
			  curr_stmt_name = hashtab[$1.value.integer].name;
			}
		;


/* 100 */
data_constant	:	numeric_const
		|	'-' numeric_const { $$ = $2; }
		|	'+' numeric_const { $$ = $2; }
		|	tok_logical_const
			{
			    $$.TOK_type = type_pack(class_VAR,type_LOGICAL);
			    $$.size = size_DEFAULT;
			}
   		|	char_literal_const
			{
			    $$.TOK_type = type_pack(class_VAR,type_STRING);
			    $$.size = size_DEFAULT;
			}
		|	tok_hollerith
			{
			    $$.TOK_type = type_pack(class_VAR,type_HOLLERITH);
			    $$.size = size_DEFAULT;
			    $$.kind = default_kind(type_INTEGER);
			}
		;

/* 101-102 absent */

/* 103 */
nonzero_unsigned_int_const:
			tok_integer_const
			{
			  if($1.value.integer == 0) {
			    if(misc_warn) {
			      warning($1.line_num,$1.col_num,
				    "nonzero integer expected");
			      msg_tail(": substituting 1");
			    }
			    $$.value.integer = 1;
			  }
			  $$.left_token = (Token *)NULL;
			}
		;

			/* F77 requires char length spec > 0 but in F90
			   a length spec 0 is allowed, and negative length
			   is turned into 0.  This production handles this.
			   We warn about a negative value just in case.
			 */
nonneg_unsigned_int_const:
			tok_integer_const
			{
			  if($1.value.integer <= 0) {
			    if($1.value.integer < 0) {
			      if(misc_warn || f77_char_extension) { 
				  warning($1.line_num,$1.col_num,
					  "non-negative integer expected");
				  msg_tail(": substituting 0");
			      }
			      $$.value.integer = 0;
			    }
			    else if(f77_char_extension) {
			      warning($1.line_num,$1.col_num,
				    "nonzero integer expected");
			    }
			  }
			  $$.left_token = (Token *)NULL;
			}
		;

/* 104-109 absent: lexer handles these */
	/* pre_label prepares for an expected label by setting flag
	   so that lexer won't look for E-format number.  All grammar
	   rules that have "label" precede it with "pre_label" */
pre_label	:	/* NOTHING */
			{
			    integer_context=TRUE;
			}
		;

/* 110 */
label		:	tok_integer_const
			{
			    if( $$.value.integer > 99999 && misc_warn) {
				syntax_error($1.line_num,$1.col_num,
				      "statement label exceeds 5 digits");
			    }
				integer_context=FALSE;
				$$.TOK_type = type_pack(class_LABEL,type_LABEL);
				$$.size = size_DEFAULT;
				$$.TOK_flags = 0;
			}
		;

/* 111-116 absent: lexer handles these */

%%

void
init_parser(VOID)			/* Initialize various flags & counters */
{
	initial_flag = TRUE;	/* set flag for keyword test */
	implicit_flag=FALSE;	/* clear flags for IMPLICIT stmt */
	implicit_letter_flag = FALSE;
	implicit_type_given = FALSE;
	global_save = FALSE;
	module_accessibility = 0; /* default */
	prev_token_class = EOS;
	complex_const_allowed = FALSE;
    use_keywords_allowed = FALSE;
    generic_spec_allowed = FALSE;
	stmt_sequence_no = 0;
	f90_stmt_sequence_no = 0;
	true_prev_stmt_line_num = 0;
	{
	  int i;		/* Reset *_this_file flags for project files */
	  for(i=0; i<glob_symtab_top; i++) {
	    glob_symtab[i].used_this_file =
	      glob_symtab[i].set_this_file =
	      glob_symtab[i].invoked_as_func_this_file =
	      glob_symtab[i].declared_external_this_file = FALSE;
	  }
	}
	push_loc_scope();	/* enter outermost scope */
}

				/* Handle unary expressions: link
				   into a tree and propagate type.
				 */
PRIVATE void
#if HAVE_STDC
do_unexpr(Token *op, Token *expr, Token *result)
#else /* K&R style */
do_unexpr(op,expr,result)
     Token *op,*expr,*result;
#endif /* HAVE_STDC */
{
  unexpr_type(op,expr,result);

  result->left_token = add_tree_node(op, expr, (Token*)NULL);
}
				/* Handle binary expressions: link
				   into a tree and propagate type.
				 */
PRIVATE void
#if HAVE_STDC
do_binexpr(Token *l_expr, Token *op, Token *r_expr, Token *result)
#else /* K&R style */
do_binexpr(l_expr,op,r_expr,result)
     Token *l_expr,*op,*r_expr,*result;
#endif /* HAVE_STDC */
{
  binexpr_type(l_expr,op,r_expr,result); /* Propagate the type */

  result->left_token = add_tree_node(op, l_expr, r_expr);
}


			/* Changes a token to empty and replaces
			   src_text by null string, value by 0.  Other
			   info (line, col, etc.)  unchanged. */

PRIVATE Token *
#if HAVE_STDC
empty_token(Token *t)
#else /* K&R style */
empty_token(t)
     Token *t;
#endif /* HAVE_STDC */
{
#ifdef DEBUG_EMPTY_TOKEN
  static char *nullstring="(empty)"; /* for debugging.  */
#else
  static char *nullstring=""; /* for operation.  */
#endif
  t->tclass = tok_empty;
  t->tsubclass = 0;
  t->value.integer = 0;
  t->left_token = (Token *) NULL;
  t->src_text = nullstring;

  return t;
}

		/* Propagate non-integer type if any of DO loop
		   bounds are non-integer. */
PRIVATE type_t
#if HAVE_STDC
do_bounds_type(Token *t1, Token *t2, Token *t3)
#else /* K&R style */
do_bounds_type(t1,t2,t3)
     Token *t1, *t2, *t3;
#endif /* HAVE_STDC */
{
  type_t result_type;
       if(datatype_of(t1->TOK_type) != type_INTEGER)result_type = t1->TOK_type;
  else if(datatype_of(t2->TOK_type) != type_INTEGER)result_type = t2->TOK_type;
  else if(datatype_of(t3->TOK_type) != type_INTEGER)result_type = t3->TOK_type;
  else result_type = t1->TOK_type;
  return result_type;
}


/* Debugging routine: prints the expression list of various productions */
#ifdef DEBUG_PARSER
PRIVATE void
print_exprlist(s,t)
	char *s;
	Token *t;
{

	(void)fprintf(list_fd,"\n%s arglist: ",s);

	if(t == NULL)
		(void)fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name(datatype_of(t->TOK_type)));
		  if( is_true(ID_EXPR,t->TOK_flags) )
			(void)fprintf(list_fd,"(%s) ",token_name(t));
	    }
	}
}

PRIVATE void
print_comlist(s,t)
	char *s;
	Token *t;
{

	(void)fprintf(list_fd,"\n%s varlist: ",s);

	if(t == NULL)
		(void)fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name(datatype_of(t->TOK_type)));
		  if( is_true(ID_EXPR,t->TOK_flags) )
			(void)fprintf(list_fd,"(%s) ",token_name(t));
		}
	  }
}
#endif

/* After having parsed prog_stmt, function_stmt, subroutine_stmt,
   block_data_stmt, the stmt_sequence_no is set to the value SEQ_HEADER.
   This routine is also called from finish_scan in advance.c upon EOF,
   with t set to NULL.
*/

void
#if HAVE_STDC
check_seq_header(Token *t)
#else /* K&R style */
check_seq_header(t)
     Token *t;
#endif /* HAVE_STDC */
{
	if(stmt_sequence_no >= SEQ_HEADER) { /* missing END statement */
	   Token fake_end_token;
	   if( t == (Token *) NULL ) { /* occurring at EOF */
	     t = &fake_end_token;
	     t->tclass = EOF;	/* construct an EOF token to use */
	     t->line_num = line_num;
	     t->col_num = NO_COL_NUM;
	   }
	   syntax_error( t->line_num, NO_COL_NUM,
			"missing END statement inserted");
	   msg_tail( (t->tclass == EOF? "at end of file":
		      "prior to statement") );

	   pop_block(t,tok_END,(char *)NULL,NO_LABEL);

	   END_processing(t);
	}
	stmt_sequence_no = SEQ_HEADER;
	f90_stmt_sequence_no = F90_SEQ_HEADER;
}

PRIVATE void
#if HAVE_STDC
check_stmt_sequence(Token *t, int seq_num)
#else /* K&R style */
check_stmt_sequence(t,seq_num)
     Token *t;
     int seq_num;
#endif /* HAVE_STDC */
{
    if(stmt_sequence_no > seq_num) {
      if(f77_stmt_order) {
	nonstandard(t->line_num, NO_COL_NUM,0,0);
	msg_tail(": Statement out of order.");
      }
    }
			/* If no error, sequence number is updated to new
			   value.  If error, it is rolled back to prevent
			   cascades of error messages.  */
    stmt_sequence_no = seq_num;
}

PRIVATE void
#if HAVE_STDC
check_f90_stmt_sequence(Token *t, int f90_seq_num)
#else /* K&R style */
check_f90_stmt_sequence(t,f90_seq_num)
     Token *t;
     int f90_seq_num;
#endif /* HAVE_STDC */
{
    if(f90_stmt_sequence_no > f90_seq_num) {
      if(f90_stmt_order) {
	nonstandard(t->line_num, NO_COL_NUM,f90_stmt_order,0);
	msg_tail(": Statement out of order.");
      }
    }
			/* If no error, sequence number is updated to new
			   value.  If error, it is rolled back to prevent
			   cascades of error messages.  */
    f90_stmt_sequence_no = f90_seq_num;
}

PRIVATE void
init_io_ctrl_list(VOID)
{
  control_item_count = 0;
  io_internal_file = FALSE;
  io_list_directed = FALSE;
  io_warning_given = FALSE;
  current_io_unit_no = IO_UNIT_UNKNOWN;
  current_io_unit_id = IO_UNIT_UNKNOWN;
  current_io_access = IO_ACCESS_DEFAULT;
  current_io_form = IO_FORM_DEFAULT;
}


		/* Remember the name or number of unit_id for current I/O stmt.
		   This routine is only called by parser productions that have a
		   bare unit_id, so less checking is done than for unit=unit_id
		   specifiers that can refer to other than external files.
		 */
void
record_io_unit_id(Token *id)
{
     if( id->tclass == '*' )
     {
	  current_io_unit_id = IO_UNIT_DEFAULT;
     }
     else if( is_true(ID_EXPR,id->TOK_flags)){
	  current_io_unit_id = id->value.integer; /* get hash code of identifier */
     }
     else if( is_true(LIT_CONST,id->TOK_flags) &&
	      id->TOK_type == type_pack(class_VAR,type_INTEGER))
     {
	  current_io_unit_no = id->value.integer; /* get literal int value */
     }
}

		/* Set I/O usage parameters for default formatted sequential I/O
		   statement like READ *, X  */
PRIVATE void
record_default_io(VOID)
{
  current_io_unit_no = IO_UNIT_UNKNOWN;
  current_io_unit_id = IO_UNIT_DEFAULT;
  current_io_access = IO_ACCESS_SEQUENTIAL;
  current_io_form = IO_FORM_FORMATTED;
}

		/* This routine applies the attributes of attr-based type
		   declaration.  Set dim_bounds to current_dim_bound_list
		   or to NULL if item has its own array bounds declarator.
		 */
PRIVATE void
process_attrs(Token *t,Token *dim_bounds)
{
    if(current_external_attr)
	def_ext_name(t);
    if(current_intrinsic_attr)
	def_intrins_name(t);

    if(current_allocatable_attr)
	apply_attr(t,tok_ALLOCATABLE);
    if(current_pointer_attr)
	apply_attr(t,tok_POINTER);
    if(current_save_attr)
	apply_attr(t,tok_SAVE);
    if(current_target_attr)
	apply_attr(t,tok_TARGET);
    if(current_public_attr)
        apply_attr(t,tok_PUBLIC);
    if(current_private_attr)
        apply_attr(t,tok_PRIVATE);
    if(current_intent_in_attr || current_intent_out_attr)
	apply_intent_attr(t);

    if(dim_bounds != NULL)
	def_array_dim(t,dim_bounds);

}

			/* Routine to apply attributes of subprogram
			   specified in prefix.
			 */
PRIVATE void
process_prefix_attrs(Token *t)
{
    if(current_elemental_attr)
	apply_attr(t,tok_ELEMENTAL);
    if(current_pure_attr)
	apply_attr(t,tok_PURE);
    if(current_recursive_attr)
	apply_attr(t,tok_RECURSIVE);
}

/* Routine to do appropriate checking before applying INTENT */
PRIVATE
void apply_intent_attr(Token *t)
{
    if( ! hashtab[t->value.integer].loc_symtab->argument ) {
	syntax_error(t->line_num,t->col_num,
		     "declaring INTENT attribute of non-argument");
    }
    else {
	if(current_intent_in_attr) {
	    if(current_intent_out_attr)
		apply_attr(t,tok_INOUT);
	    else
		apply_attr(t,tok_IN);
	}
	else if(current_intent_out_attr)
	    apply_attr(t,tok_OUT);
    }
}


	/* After having parsed end_stmt, common block lists and
	   subprogram argument lists are copied over into global symbol
	   table, the local symbol table is printed out and then cleared,
	   and stmt_sequence_no is set to zero for start of next prog_unit.
	*/

PRIVATE void
#if HAVE_STDC
END_processing(Token *t)
#else /* K&R style */
END_processing(t)
	Token *t;
#endif /* HAVE_STDC */
{
  ++tot_prog_unit_count;


  if(current_prog_unit_hash != -1) {
    int current_prog_unit_type =
    		get_type(hashtab[current_prog_unit_hash].loc_symtab);
    if(exec_stmt_count == 0 && !interface_block) {
	  if(misc_warn) {
	    char *prog_unit_type_name = NULL;
	    switch (current_prog_unit_type) {
		case type_PROGRAM:
		  prog_unit_type_name = "Program";
		  break;
		case type_SUBROUTINE:
		  prog_unit_type_name = "Subroutine";
		  break;
	        default:	/* all other types, including functions */
		  if (inside_function)
		    prog_unit_type_name = "Function";
		  break;
	    }
	    if( prog_unit_type_name ) {
	    	warning(t == (Token *)NULL? line_num: t->line_num, NO_COL_NUM,
			prog_unit_type_name);
		msg_tail("contains no executable statements");
	    }
	  }
	}

	if(do_list && t != (Token *)NULL) {
	    (void)flush_end_stmt(t->line_num);
	}

	doing_end_proc = TRUE;	/* Set flag for special error message mode */

			/* Catch things that had to wait till now */
	check_loose_ends(current_prog_unit_hash);
			/* Put arg and com lists into global table */
	process_lists(current_prog_unit_hash);
			/* Print symbol table for debug */
	debug_symtabs();
			/* Print local symbol table and do local warnings */
	if(! interface_block) {	/* but not for INTERFACE declaration */
	  print_loc_symbols();
	}

	/* At END MODULE, need to check internal usage of module subprograms
	   and then save module info to a file.  [LATTER NOT DONE YET] */
	if (current_prog_unit_type == type_MODULE) {
	  if(contains_ended) {
	    visit_children(/*wrapup=*/FALSE);
	    check_arglists(current_prog_unit_hash,module_subprog);
	  }

				/* write out module contents to a module file */
	  write_module_file(current_prog_unit_hash);

	  if(contains_ended) {
	    clean_globals(current_prog_unit_hash,module_subprog);
  	    module_accessibility = 0; /* default */
	  }
	}
	/* At END of a non-module subprogram with a CONTAINS section,
	   need to check internal usage of internal subprograms and
	   then clear the valid flags of same. */
	else if(contains_ended) {
	  visit_children(/*wrapup=*/FALSE);
	  check_arglists(current_prog_unit_hash,internal_subprog);
	  clean_globals(current_prog_unit_hash,internal_subprog);
	}
			/* Reset local symbol table */
	init_symtab();

	doing_end_proc = FALSE;
  }
  exec_stmt_count = 0;
  stmt_sequence_no = 0;
  f90_stmt_sequence_no = 0;

  /* Now exit the symbol table scope just processed */
  current_prog_unit_hash = pop_loc_scope();

  /* exiting a scope, return to enclosing scope if any */
  if( block_depth > 0 ) {
#ifdef DEBUG_SCOPE
	  if(debug_latest)
	      fprintf(list_fd,"\n(block is not empty)");
#endif
  }

  implicit_type_given = FALSE;
  true_prev_stmt_line_num = 0;
  integer_context = FALSE;
  global_save = FALSE;
  label_dummy_arg_count = 0;
  num_io_unit_usages = 0;

  push_loc_scope();		/* get ready for next scope unit */

}

		/* Routine to create a node for an expr tree.  Returns
		   a pointer to the newly created node.
		 */
PRIVATE Token *
#if HAVE_STDC
add_tree_node(Token *node, Token *left, Token *right)
#else /* K&R style */
add_tree_node(node,left,right)
     Token *node,*left,*right;
#endif /* HAVE_STDC */
{
  Token *new_node, *new_left, *new_right;

  new_node=new_token();

  *new_node = *node;		/* Make a permanent copy of root */

		/* Add the children.  If child's left_token pointer is
		   null, then that expression is a primary.  Otherwise
		   it is the root node of a subtree.
		 */
  if( left->left_token == (Token *)NULL 
      || node->tclass == tok_DO/* implied-do has expr list as left child */
      || node->tclass == tok_l_ac_delimiter/*array constructor has expr
      					     list as left child */
    ) {
     new_left=new_token();
    *new_left = *left;			/* Copy primary to permanent space */
  }
  else {
    new_left = left->left_token;	/* No copying needed in this case */
  }

  if(right == (Token *)NULL) {
    new_right = (Token *)NULL;		/* No right child */
  }
  else if(right->left_token == (Token *)NULL
	  || node->tclass == '(') { /* Paren means right child is expr list */
    new_right=new_token();
    *new_right = *right;		/* Copy primary to permanent space */
  }
  else {
    new_right = right->left_token;	/* No copying needed in this case */
  }

  new_node->left_token = new_left;	/* Link children onto the new root */
  new_node->next_token = new_right;
  return new_node;
}

		/* Routine to add token t to the front of a token list. */
Token *
#if HAVE_STDC
append_token(Token *tlist, Token *t)
#else /* K&R style */
append_token(tlist,t)
     Token *tlist, *t;
#endif /* HAVE_STDC */
{
	Token *tcopy;

	tcopy=new_token();

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->next_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}

			/* Routine to pop closing statement of block off
			   the stack.  Note: label should be NO_LABEL even
			   if the statement has a label, except for terminal
			   statement of a labeled DO loop.
			 */
PRIVATE void pop_block(Token *t, int stmt_class, char *name, LABEL_t label)
{

	/* Define lookup table for block matches.  This table is generated
	   from tokdefs.h by the Perl script make_blockmatch.pl.  This script
	   also generates 4 macro definitions that go here.  It
	   defines MIN_BLOCK_TOKEN and MAX_BLOCK_TOKEN used to bound range
	   of valid keytok_name arguments for error messages.  It also defines
	   MIN_CLOSER and MAX_CLOSER giving the range of token numbers for
	   closing tokens.  The array block_match contains the token numbers
	   for the matching openers.  Look up the matching opener class as
	     block_match[closer_class-MIN_CLOSER]
	   Plain END is handled specially since it matches many things.
	   Likewise closing labeled-DO range is handled specially.  (ENDDO
	   is handled there if loop is labeled, handled with structured
	   block closers otherwise.)
	*/

  static int block_match[] = {
#include "block_match.h"
  };

		/* Macro for names of all block keyword tokens.  Although no
		   error messages should print "identifier" (only occurs
		   as DO terminator, and mismatch is handled specially),
		   include it for possible debug use.  Any other
		   token classes in block_stack mean a bug in ftnchek.
		 */
#define TOKEN_NAME(CLASS) ((CLASS)>=MIN_BLOCK_TOKEN && (CLASS)<=MAX_BLOCK_TOKEN? \
	keytok_name(CLASS):((CLASS) == tok_identifier?"identifier":"bogus"))

		/* Define DBG_TOKNAME for debugging.  If possible,
		   use yytname to allow for bogus tokens showing up.
		   This works only for bison parser generator.  It would
		   be easy to fix up for byacc too if need be.
		*/
#ifdef DEBUG_BLOCKCHECK
#ifdef USE_YYTNAME
#define DBG_TOKNAME(CLASS) (char *)(yytname[YYTRANSLATE(CLASS)])
#else
#define DBG_TOKNAME(CLASS) TOKEN_NAME(CLASS)
#endif
#endif

#ifdef DEBUG_BLOCKCHECK
  if(debug_latest) {
    fprintf(list_fd,"\npopping token %s name %s label %d type %d at line %d", 
	    DBG_TOKNAME(stmt_class),name,label,
		block_stack[block_depth-1].subprogtype,
		t->line_num);

	fprintf(list_fd,"\nCURRENT_PROG_UNIT_HASH=%d", current_prog_unit_hash);
  }
#endif

  contains_ended = contains_sect; /* remember if CONTAINS was set */

  if(block_depth == 0) {
      syntax_error(t->line_num,t->col_num,"no construct to end here");
  }
  else {
    int opener_class, must_check_name=FALSE;
    BLOCK_TYPE blocktype;

    --block_depth;

    opener_class = block_stack[block_depth].sclass;
    blocktype = block_stack[block_depth].blocktype;

#ifdef DEBUG_BLOCKCHECK
    if(debug_latest) {
      fprintf(list_fd,"\n  opener was token %s name %s label %d type %d at line %d",
	      DBG_TOKNAME(opener_class),
	      block_stack[block_depth].name,
	      block_stack[block_depth].label,
	      block_stack[block_depth].subprogtype,
	      block_stack[block_depth].first_line);
    }
#endif
				/* DO loop terminator */
    if( label != NO_LABEL) {
      int shared_warning = TRUE;
				/* Mark DO index variable as inactive */
      int h=block_stack[block_depth].do_var_hash;
      if(h != -1)
	undef_do_variable(h);

      if( stmt_class == tok_ENDDO ) {
	must_check_name = TRUE;
      }
      else {
			/* issue picky warnings if not modern DO construct */
	if(style_req_enddo) {
	  warning(t->line_num,t->col_num,
		  "DO loop not terminated by ENDDO");
	}
	else if( stmt_class != tok_CONTINUE ) {
	  if(style_req_do_construct) {
	    warning(t->line_num,t->col_num,
		    "DO loop not terminated by CONTINUE or ENDDO");
	  }
	}
      }
      if( opener_class != tok_DO ) {
	syntax_error(t->line_num,t->col_num,keytok_name(opener_class));
	msg_tail("block not closed when DO loop terminating statement encountered");
      }
      else if( label != block_stack[block_depth].label ) {
	syntax_error(t->line_num,t->col_num,"Label");
	msg_tail(ulongtostr(label));
	msg_tail("on DO loop terminator does not match");
	if(block_stack[block_depth].label == NO_LABEL ) {
	  msg_tail("label-less DO statement");
	}
	else {
	  msg_tail("corresponding DO label");
	  msg_tail(ulongtostr(block_stack[block_depth].label));
	}
	msg_tail("above");
      }
		/* If terminator is shared by other DOs, pop them too */
      else {
	while(block_depth > 0 && label == block_stack[block_depth-1].label) {
	  --block_depth;

				/* Mark DO index variable as inactive */
	  {
	    int hh=block_stack[block_depth].do_var_hash;
	    if(hh != -1)
	      undef_do_variable(hh);
	  }
#ifdef DEBUG_BLOCKCHECK
	  if(debug_latest) {
	      fprintf(list_fd,"\n  opener was token %s name %s label %d type %d at line %d",
		      DBG_TOKNAME(opener_class),
		      block_stack[block_depth].name,
		      block_stack[block_depth].label,
		      block_stack[block_depth].subprogtype,
		      block_stack[block_depth].first_line);
	  }
#endif
	  if(style_shared_do_terminator && shared_warning) {
	    warning(t->line_num,t->col_num,
		    "Obsolescent feature: shared DO terminator");
	    shared_warning = FALSE; /* don't repeat the warning */
	  }
	}
      }
    }
    else if( stmt_class == tok_END ) {	/* plain END statement */
      if(style_req_structured_end) {
	  warning(t->line_num,t->col_num,"Old-style END statement");
      }
			/* Check that END statement ends subprog block */
      if(opener_class != tok_SUBROUTINE
	 && opener_class != tok_FUNCTION
	 && opener_class != tok_PROGRAM
	 && opener_class != tok_MODULE
	 && opener_class != tok_BLOCKDATA) {
	  syntax_error(t->line_num,t->col_num,
		       "Block not closed when END statement encountered");
      }
    }
    else {			/* structured block closers */
      int matching_class;

      must_check_name = TRUE;

				/* Look up the correct matching opener class */
      if( stmt_class < MIN_CLOSER || stmt_class > MAX_CLOSER ||
	  (matching_class = block_match[stmt_class-MIN_CLOSER]) == 0 ) {
	fprintf(list_fd,"%d=%s ",stmt_class,keytok_name(stmt_class));
	oops_message(OOPS_FATAL,t->line_num,t->col_num,
		     "pop_block called for unrecognized closing token");
      }
      else {			/* check if opener matches expected */
	if( opener_class != matching_class ) {
		/* If possible, use token names for better error messages */
	  syntax_error(t->line_num,t->col_num,
		       keytok_name(stmt_class));
	  msg_tail("statement does not match");
	  msg_tail(keytok_name(opener_class));
	  msg_tail("block it closes");

	  must_check_name = FALSE; /* name mismatch probably bogus here */
	}
				/* If unlabeled END DO of loop with index
				   variable, mark it inactive.
				 */
	if( stmt_class == tok_ENDDO ) {
	  int h=block_stack[block_depth].do_var_hash;
	  if(h != -1)
	    undef_do_variable(h);
	}
      }
    }

/*--------------------------addition-----------------------------------*/
	if (block_stack[block_depth].subprogtype == internal_subprog ||
			block_stack[block_depth].subprogtype == module_subprog) {
	  /* If we have hit END of containing module subprogram, need
	     to pop the (empty) scope that was inserted to get ready for
	     next internal or module subprogram.
	   */
	  if(block_stack[block_depth].subprogtype == module_subprog
	     && contains_sect ) {
	    current_prog_unit_hash = pop_loc_scope();
	  }
	  contains_sect = TRUE; /* turns CONTAINS block flag on after exiting
				   either of these */
#ifdef DEBUG_BLOCKCHECK
		if(debug_latest) fprintf(list_fd,"\npop_block setting contains_sect=TRUE");
#endif
	}
	else {
	  /* If we have hit END of containing module or external subprogram, need
	     to pop the (empty) scope that was inserted to get ready for
	     next internal or module subprogram.
	   */
	  if( contains_sect ) {
	    current_prog_unit_hash = pop_loc_scope();
	    contains_sect = FALSE;
#ifdef DEBUG_BLOCKCHECK
		if(debug_latest) printf("\npop_block setting contains_sect=FALSE");
#endif
	  }
	}

/*---------------------------------------------------------------------*/

		/* Issue syntax error if name missing from a component of
		   a named construct.  In picky mode warn if no name tag on
		   structured END.
		*/
    if( must_check_name ) {
      if( name == (char *)NULL ) {
	if( blocktype == construct ) {	/* IF, DO, SELECT */
	  if( block_stack[block_depth].name != (char *)NULL ) {
	    syntax_error(t->line_num,t->col_num,
			 "Construct name");
	    msg_tail(block_stack[block_depth].name);
	    msg_tail("missing");
	  }
	}
	else {		/* structured END of subprogram */
	  if(style_req_end_name) {
	    warning(t->line_num,t->col_num,
		    "Name of subprogram missing from");
	    msg_tail(keytok_name(stmt_class));
	    msg_tail("statement");
	  }
	}
      }
				/* Otherwise check name tag if present */
      else {
			/* OK to compare pointers due to name storage method */
	if(name != block_stack[block_depth].name) {
			/* Opener name can only be missing from a construct,
			   but handle subprog case anyway.
			*/
	  if(block_stack[block_depth].name == (char *)NULL) {
	    syntax_error(t->line_num,t->col_num,
			   "name found on closing statement of unnamed");
	    msg_tail(blocktype == construct? "construct": "subprogram");
	  }
	  else {
	    syntax_error(t->line_num,t->col_num,
			 "Name");
	    msg_tail(name);
	    msg_tail("does not match");
	    msg_tail(blocktype == construct? "construct": "subprogram");
	    msg_tail("name");
	    msg_tail(block_stack[block_depth].name);
	  }
	}
      }
    }
  }
}

			/* Check CYCLE and EXIT statements for agreement
			   with enclosing DO loop.
			*/

PRIVATE void check_construct_name_match(Token *t, char *name)
{

				/* If no name on statement, it must at
				   least be inside range of a DO.
				 */
  if( name == (char *)NULL ) {
    int depth;
    for(depth = block_depth-1; depth >= 0; depth--) {
      if( block_stack[depth].sclass == tok_DO )
	return;
    }
    syntax_error(t->line_num,t->col_num,
		 "statement not within range of any DO loop");
  }
				/* If name tag present, it must belong to
				   an enclosing DO.
				*/
  else {
    int depth;
    for(depth = block_depth-1; depth >= 0; depth--) {
      if( block_stack[depth].sclass == tok_DO &&
	  block_stack[depth].name == name )
	  return;
    }
    syntax_error(t->line_num,t->col_num,
		 "construct name");
    msg_tail(name);
    msg_tail("does not match name of any enclosing DO construct");
  }
}

PRIVATE void push_block(Token *t, int stmt_class, BLOCK_TYPE blocktype,
			char *name, LABEL_t label)
{
  if(block_depth == MAX_BLOCK_DEPTH) {
    oops_message(OOPS_FATAL,t->line_num,t->col_num,
		     "blocks nested too deeply");
  }
  else {
    /* This is better later.  See below
#ifdef DEBUG_BLOCKCHECK
    if(debug_latest) {
      fprintf(list_fd,"\npushing token %s name %s label %d at line %d",
	      DBG_TOKNAME(stmt_class),name,label,t->line_num);
    }
#endif
    */

				/* ELSE and ELSE IF masquerade here as IF,
				   and CASE and CASEDEFAULT as SELECTCASE,
				   and ELSEWHERE as WHERE, to
				   simplify match code in pop_block. This
				   masquerading is done in action code above.
				*/
    block_stack[block_depth].sclass = stmt_class;
    block_stack[block_depth].name = name;
    block_stack[block_depth].label = label;
    block_stack[block_depth].first_line = t->line_num;
    block_stack[block_depth].blocktype = blocktype;
    block_stack[block_depth].do_var_hash = -1; /* undefined at this time */

/*--------------------------addition------------------------------------*/

    if (!contains_sect) {
		block_stack[block_depth].subprogtype = not_subprog;
    }
	else { /* A subprogram inside a contains block is either a
		  module subprogram or an internal subprogram.  Note
		  we warn but process anyway if it is not a procedure
		  (subroutine or function) that can legitimately go
		  inside a CONTAINS block, since not treating it so
		  will mess up management of the global symbol table.
	       */
		if(  stmt_class != tok_SUBROUTINE &&
		     stmt_class != tok_FUNCTION ) {
		  syntax_error(t->line_num,t->col_num,
			       "program unit not allowed in CONTAINS section");
		}
		if (block_depth > 0 && block_stack[block_depth-1].sclass
				== tok_MODULE)
			block_stack[block_depth].subprogtype = module_subprog;
		else
			block_stack[block_depth].subprogtype = internal_subprog;
	}
	contains_sect = FALSE;

#ifdef DEBUG_BLOCKCHECK
    if(debug_latest) {
	printf("\npush_block setting contains_sect=FALSE");
      fprintf(list_fd,"\npushing token %s name %s label %d type %d at line %d",
	      DBG_TOKNAME(stmt_class),name,label,
		  block_stack[block_depth].subprogtype,t->line_num);
	fprintf(list_fd,"\nCURRENT_PROG_UNIT_HASH=%d", current_prog_unit_hash);
    }
#endif

/*---------------------------------------------------------------------*/

    ++block_depth;
  }
}

/* Routine to set flags for attributes into result from symtab
   entry for t.  Needed for nonterminals that are data objects,
   i.e. array elements and derived type components.  For terminals,
   the symbol table should be consulted instead of the token.
 */
PRIVATE void set_attr_flags( Token *t, Token *result )
{
  if( is_true(ID_EXPR,t->TOK_flags) ) {	/* value.integer is hashtable index */
    Lsymtab *symt = hashtab[t->value.integer].loc_symtab;
    if( symt ) {

      if(symt->pointer)
	make_true(POINTER_EXPR,result->TOK_flags);
      else
	make_false(POINTER_EXPR,result->TOK_flags);

      if(symt->target)
	make_true(TARGET_EXPR,result->TOK_flags);
      else
	make_false(TARGET_EXPR,result->TOK_flags);

      if(symt->associated_flag)
	make_true(ASSOCIATED_EXPR,result->TOK_flags);
      else
	make_false(ASSOCIATED_EXPR,result->TOK_flags);

      if(symt->allocatable)
	make_true(ALLOCATABLE_EXPR,result->TOK_flags);
      else
	make_false(ALLOCATABLE_EXPR,result->TOK_flags);

      /* non-allocatable targets are treated as allocated */
      if(symt->allocated_flag || (symt->target && !symt->allocatable))
	make_true(ALLOCATED_EXPR,result->TOK_flags);
      else
	make_false(ALLOCATED_EXPR,result->TOK_flags);
    }
  }
}



SUBPROG_TYPE find_subprog_type(int stmt_class)
{
	if (!contains_sect || (stmt_class != tok_SUBROUTINE &&
		stmt_class != tok_FUNCTION))
		return not_subprog;
	else { /* a subprogram inside a contains block is either
			  a module subprogram or an internal subprogram */
		if (block_depth > 0 && block_stack[block_depth-1].sclass
				== tok_MODULE)
			return module_subprog;
		else
			return internal_subprog;
	}
}

/* routine to return the statement class of the current block */
int get_curr_block_class()
{
  return block_stack[block_depth-1].sclass;
}

/* Routine to return the construct name of the current block.  For
   derived type blocks it is the type name. */
char *get_curr_block_name()
{
  return block_stack[block_depth-1].name;
}

/* Routine to swap the top two entries on block stack.  This is
   called when first line of a program is opener of a block
   construct.  That block construct opener was already pushed,
   and the %MAIN opener needs to come first.
 */
void block_stack_top_swap()
{
  BlockStack temp = block_stack[block_depth-1];
  block_stack[block_depth-1] = block_stack[block_depth-2];
  block_stack[block_depth-2] = temp;
}

/* Used as a hook for DDD */
PRIVATE void check_token(Token *id)
{
}

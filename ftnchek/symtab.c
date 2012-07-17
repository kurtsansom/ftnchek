/* $Id: symtab.c,v 1.45 2003/08/07 19:37:48 moniot Exp $

  Definitions of symbol table maintenance routines and
  hash table functions


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

/*
  I. Symtab


		Symbol table routines for Fortran program checker.

	  Shared functions defined:


	   call_func(id,arg)	 Handles function invocations.
	   call_subr(id,arg)	 Handles CALL statements.
	   declare_type(id,datatype,size) Handles TYPE statements.
	   def_arg_name(id)	 Handles func/subr argument lists.
	   def_array_dim(id,arg) Handles dimensioning declarations.
	   def_com_block(id)	 Handles common blocks and SAVE stmts.
	   def_com_variable(id)	 Handles common block lists.
       int def_curr_prog_unit(id)	 Identifies symbol as current prog unit.
     	   def_equiv_name(id)	 Initializes equivalence list items.
	   def_ext_name(id)	 Handles external lists.
	   def_function(datatype,size,size_text,id,args)
	   		Installs function name in global table.
	   def_intrins_name(id)  Handles intrinsic lists.
	   def_parameter(id,value) Handles parameter_defn_item
	   def_stmt_function(id) Declares a statement function.
	   do_ASSIGN(id)	 Handles ASSIGN stmts.
	   do_assigned_GOTO(id)	 Handles assigned GOTO.
	   do_ENTRY(id,args,hashno) Processes ENTRY statement.
	   do_RETURN(hashno,keyword) Processes RETURN statement.
	   do_bind_spec(p,subprogtype) Processes BIND spec
	   do_suffix(class,subprogtype,hashno,suffix) Processes subprogram suffixes
	   equivalence(id1,id2)	 equivalences two variables
       int get_type(symt)	 Finds out data type of symbol, or uses implicit
				 typing to establish its type.
       int get_size(symt,type)	 Finds out size of symbol's datatype.
	unsigned hash_lookup(s)	 Looks up identifier in hashtable.
	   init_globals()	 Initializes global symbol info.
	   init_symtab()	 Clears local symbol table & removes locals
				 from stringspace. Also restores default
				 implicit data typing.
 Gsymtab* install_global(t,datatype,storage_class) Installs indentifier in
				global symbol table.
 Lsymtab* install_local(t,datatype,storage_class) Installs indentifier in
				local symbol table.
	   ref_array(id,subscrs) Handles array references
	   ref_variable(id)	 Handles accessing variable name.
	   set_implicit_type(type,size,c1,c2) Processes IMPLICIT statement.
	   stmt_function_stmt(id) Finishes processing stmt func defn.
    char * token_name(t)	 Returns ptr to token's symbol's name.
	   use_actual_arg(id)	 Handles using a variable as actual arg.
	   use_io_keyword(id_keywd,id_val,class) Handles i/o control specifier.
	   use_inq_arg(id)	 Handles arguments passed to inquiry intrinsic.
	   use_lvalue(id)	 Handles assignment to a variable.
	   use_parameter(id)	 Handles data_constant_value &
				 data_repeat_factor.
	   use_variable(id)	 Sets used-flag for a variable used in expr.

*/

/*  private functions defined:
 call_external(symt,id,arg)	places token list of args into local symtab
 check_intrins_args(arg, defn) Checks call seq of intrinsic functions
 check_stmt_function_args(symt,id,arg)  ditto for statement functions
*/

#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#define SYMTAB
#include "ftnchek.h"
#include "symtab.h"
#include "symspace.h"
#include "symutils.h"
#include "intrins.h"
#include "tokdefs.h"
#include "dtypes.h"

#ifdef DEVELOPMENT		/* for maintaining the program */
#define DEBUG_SIZES
#endif


PROTO(PRIVATE void call_external,( Lsymtab *symt, Token *id, Token *arg ));
PROTO(PRIVATE void check_intrins_args,( Token *id, Token *arg ));
PROTO(PRIVATE void check_stmt_function_args,( const Lsymtab *symt, Token *id, Token *arg ));
/*
PROTO(PRIVATE Lsymtab* install_local,( int h, int datatype, int storage_class ));
*/
PROTO(PRIVATE void use_function_arg,( Token *id ));
PROTO(PRIVATE void use_inq_arg,( Token *id ));
PRIVATE Lsymtab *inherit_local(int h, Lsymtab *enclosing_symt);
PRIVATE void make_equivalent(Lsymtab *symt1, Lsymtab *symt2);
PROTO(PRIVATE void copy_interface_to_symtab,(Lsymtab *symt, const ProcInterface *interface));

#ifdef DEBUG_SIZES
PROTO(extern void print_sizeofs,( void ));	/* in symtab.c */
#endif


/* Apply an attribute to a variable */
/* N.B. Legality checking deferred to END */
/* N. extra B.: allocatable, pointer, and target flags ignored at present */

void
apply_attr(Token *id,		/* token of variable to apply attr to */
	   int attr)		/* token class of attr to apply */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL || !in_curr_scope(symt) ) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

	/* macro to set an attribute bit in symtab: if already set, flag
	   the error, otherwise set the bit in all equivalenced entries.
	 */
#define check_and_set_attr( ATTRBIT ) \
	if( (symt->ATTRBIT) ) {					 \
	     syntax_error(id->line_num,id->col_num,"redundant"); \
	     msg_tail(keytok_name(attr)); msg_tail("declaration"); } \
	else { \
	    symt->ATTRBIT = TRUE; \
	} 
	    
	  

	/* Same but also checks whether a mutually exclusive bit is
	 * set. 
	 */
#define check_and_set_attr2( ATTRBIT, EXCLUDEBIT ) if(symt->EXCLUDEBIT) { \
		  syntax_error(id->line_num,id->col_num,"conflicting"); \
		  msg_tail(keytok_name(attr)); msg_tail("declaration"); \
	      } \
	else \
	    check_and_set_attr( ATTRBIT )

	switch( attr )
	{
	  case tok_ALLOCATABLE:
	       check_and_set_attr(allocatable);
	       break;
	  case tok_POINTER:
	       check_and_set_attr2(pointer,target);
	       break;
	  case tok_SAVE:
	       check_and_set_attr(saved);
	       break;
	  case tok_TARGET:
	       check_and_set_attr2(target,pointer);
	       break;
	  case tok_PUBLIC:
	       check_and_set_attr2(public,private);
	       break;
	  case tok_PRIVATE:
	       check_and_set_attr2(private,public);
	       break;
	  case tok_IN:
	       check_and_set_attr2(intent_in,intent_out);
	       break;
	  case tok_OUT:
	       check_and_set_attr2(intent_out,intent_in);
	       break;
	  case tok_INOUT:
	       if(symt->intent_in || symt->intent_out) {
		   syntax_error(id->line_num,id->col_num,"conflicting or redundant");
		   msg_tail(keytok_name(attr)); msg_tail("declaration");
	       }
	       else {		/* OK to use macro; error can't happen  */
		   check_and_set_attr(intent_in);
		   check_and_set_attr(intent_out);
	       }
	       break;
	  case tok_ELEMENTAL:
	       check_and_set_attr2(elemental,recursive);
	       break;
	  case tok_PURE:
	       check_and_set_attr(pure);
	       break;
	  case tok_RECURSIVE:
	       check_and_set_attr2(recursive,elemental);
	       break;
	  case tok_OPTIONAL:
	       check_and_set_attr(optional);
	       break;
	}
#undef check_and_set_attr
#undef check_and_set_attr2
}
			/* This routine handles the saving of arg lists which
			   is done by call_func and call_subr.  Also called
			   by def_namelist to save its variable list. */
PRIVATE void
#if HAVE_STDC
call_external(Lsymtab *symt, Token *id, Token *arg)
#else /* K&R style */
call_external(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
#endif /* HAVE_STDC */
{
       	TokenListHeader *TH_ptr;

		/* Insert the new list onto linked list of token lists */
      	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (arg == NULL ? NULL: arg->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;
#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\nSubprogram %s :: ",symt->name);
	  if(arg != NULL)
	    print_expr_list(arg->next_token);
	}
#endif
} /*call_external*/

void
#if HAVE_STDC
call_func(Token *id, Token *arg)	/* Process function invocation */
#else /* K&R style */
call_func(id,arg)	/* Process function invocation */
	Token *id, *arg;
#endif /* HAVE_STDC */
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	IntrinsInfo *defn;
	int symt_inherited = FALSE;

	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
       	   symt->info.toklist = NULL;
	}
	else if (!in_curr_scope(symt)) {
	   /* copy appropriate fields to new masking entry */
	   symt = inherit_local(h,symt);
	   hashtab[h].loc_symtab = symt;
	   symt_inherited = TRUE;
	}
	else {			/* protect ourself against nonsense */
	   if( symt->parameter ) {
	      syntax_error(id->line_num,id->col_num,
		   "identifier was previously declared a non-function:");
	      msg_tail(symt->name);
	      symt->array_var = symt->parameter = FALSE;
	      symt->info.toklist = NULL;
	   }
	}

	/* check recursive calls on non-recursive subprograms */
	if( h == current_prog_unit_hash && !symt->recursive) {
	    syntax_error(id->line_num,id->col_num,
		    "subprogram is not recursive:");
	    msg_tail(symt->name);
	}

	t = datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if(storage_class_of(symt->type) == class_VAR) {
	    symt->type = type_pack(class_SUBPROGRAM,t);
	    symt->info.toklist = NULL;
	}


		/* See if intrinsic.  If so, set flag, save info */
	if(!symt->external && !symt->intrinsic && !symt->argument
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
		if( defn->intrins_flags&I_PTR ) { /* returns a pointer */
		  symt->pointer = TRUE;
		}
		if( defn->intrins_flags&I_ELEM ) { /* is elemental */
		  symt->elemental = TRUE;
		}
	}

		/* Update set/used status of variables in arg list.  This
		   is deferred to now to allow intrinsics to be treated
		   as pure functions regardless of pure_function flag. */

	if(arg != NULL) {
	    Token *a=arg;
	    intrins_flags_t
	        nonpure,	/* flag if function may modify arg */
	        i_inq;		/* special handling for intrinsic inquiry functions */
	    if(symt->intrinsic) {
	      nonpure = symt->info.intrins_info->intrins_flags&I_NONPURE;
	      i_inq = symt->info.intrins_info->intrins_flags&I_INQ;
	    }
	    else {
	      nonpure = !(pure_args || pure_common);
	      i_inq = FALSE;
	    }

			/* Token list is in reverse order.  Restore
			   args to original order. */
	    arg->next_token = reverse_tokenlist(arg->next_token);

  	    while( (a=a->next_token) != NULL) {
	      if(is_true(ID_EXPR,a->TOK_flags)){
		if( nonpure ) {
			     /* Treat impure function like subroutine call */
		  use_actual_arg(a);
		  use_variable(a);
		}
		else {
		  if(i_inq)
		       use_inq_arg(a); /* Inquiry function args need special handling */
		  else
			     /* Pure-function invocation checks u-b-s */
		    use_function_arg(a);
		}
	      }
	    }
	}

		/* If intrinsic, do checking now.  Otherwise, save arg list
		   to be checked later. */

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	if(misc_warn)
	  check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
      int lookahead_result = LOOKAHEAD_NOTFOUND;
      switch(storage_class_of(symt->type)) {
	case class_SUBPROGRAM:
	  if(!symt->argument) {

	/* Lookahead kluge: check if this is the first invocation seen
	   in local scope, and if it may be an internal procedure,
	   look ahead to find its interface.  If symt entry is
	   inherited from host, info may be from lookahead, whereas by
	   now perhaps it was parsed and a defn put into global
	   symtab, with better info than lookahead.
	*/
	    if( symt->info.toklist == (TokenListHeader *)NULL ||
		symt_inherited ) {
#ifdef DEBUG_LOOKAHEAD
	      if(debug_latest) {
		fprintf(list_fd,"\ninvoke func %s: file_has_contains=%d",
			symt->name,file_has_contains);
	      }
#endif
	      if( file_has_contains ) {
		ProcInterface interface;
		if( (lookahead_result = search_for_internal(symt->name,&interface)) != LOOKAHEAD_NOTFOUND ) {
#ifdef DEBUG_LOOKAHEAD
		  if(debug_latest) {
		    fprintf(list_fd,"\n %s %s found",
		      lookahead_result==LOOKAHEAD_INTERNAL?"internal":"module",
		      symt->name);
		  }
#endif
	       /* Fill in local symbol table entry with info about function */
		  copy_interface_to_symtab(symt,&interface);

		}
	      }
	    }
	  }
	  symt->external = TRUE;
	  if((!symt->argument) &&
	     ((gsymt=(hashtab[h].glob_symtab)) == NULL ||
	      (lookahead_result == LOOKAHEAD_INTERNAL && !gsymt->internal_subprog) ||
	      (lookahead_result == LOOKAHEAD_MODULE && !gsymt->module_subprog)) ) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
		/* If lookahead found it, set appropriate flag */
		if( lookahead_result == LOOKAHEAD_INTERNAL)
		  gsymt->internal_subprog = TRUE;
		else if( lookahead_result == LOOKAHEAD_MODULE )
		  gsymt->module_subprog = TRUE;
	  }
			/* store arg list in local table */
	  call_external(symt,id,arg);
	  break;
	case class_STMT_FUNCTION:
	  symt->external = TRUE;
	  if(misc_warn)
	    check_stmt_function_args(symt,id,arg);
	  break;
      }
    }

    /* For elemental function, args must be conformable.  Result takes
       shape of array args.
     */
    if( symt->elemental ) {
      check_elemental_args(id,arg);
    }

    if(! symt->used_flag) { /* record first line where used */
	symt->line_used = id->line_num;
	symt->file_used = inctable_index;
    }

    symt->used_flag = TRUE;
    symt->invoked_as_func = TRUE;

} /*call_func*/

void
#if HAVE_STDC
call_subr(Token *id, Token *arg)	/* Process call statements */
#else /* K&R style */
call_subr(id,arg)	/* Process call statements */
	Token *id, *arg;
#endif /* HAVE_STDC */
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	IntrinsInfo *defn;
	int symt_inherited = FALSE;
	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(h,type_SUBROUTINE,class_SUBPROGRAM);
   	   symt->info.toklist = NULL;
	}
	else if (!in_curr_scope(symt)) {
	   /* copy appropriate fields to new masking entry */
	   symt = inherit_local(h,symt);
	   hashtab[h].loc_symtab = symt;
	   symt_inherited = TRUE;
	}
	else {			/* protect ourself against nonsense */
	   if( symt->array_var || symt->parameter ) {
	      syntax_error(id->line_num,id->col_num,
		   "identifier was previously declared a non-subroutine:");
	      msg_tail(symt->name);
	      symt->array_var = symt->parameter = FALSE;
	      symt->info.toklist = NULL;
	   }
	}

	/* check recursive calls on non-recursive subprograms */
	if( h == current_prog_unit_hash && !symt->recursive) {
	    syntax_error(id->line_num,id->col_num,
		    "subprogram is not recursive:");
	    msg_tail(symt->name);
	}

	t = datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if( (storage_class_of(symt->type) == class_VAR
	     || symt->external ) && t == type_UNDECL) {
		t = type_SUBROUTINE;
		symt->info.toklist = NULL;
	}
	symt->type = type_pack(class_SUBPROGRAM,t);
	symt->kind = 0;

	/* Since nonstandard intrinsics include some subroutines,
	   see if it is in intrinsic list.  Or
	   if declared intrinsic, then accept it as such and
	   do checking now.  Otherwise, save arg list
	   to be checked later. */
    if(!symt->external && !symt->intrinsic
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
    }

			/* Token list is in reverse order.  Restore
			   args to original order. */
    if(arg != NULL)
	arg->next_token = reverse_tokenlist(arg->next_token);

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	if(misc_warn)
	  check_intrins_args(id,arg);
    }
    else {
      int lookahead_result = LOOKAHEAD_NOTFOUND;
	
      if( !symt->argument ) {

	/* Lookahead kluge: check if this is the first invocation seen
	   in local scope, and if it may be an internal procedure,
	   look ahead to find its interface. */
       if( symt->info.toklist == (TokenListHeader *)NULL ||
		symt_inherited ) {
#ifdef DEBUG_LOOKAHEAD
	if(debug_latest) {
	  fprintf(list_fd,"\ncall subr %s: file_has_contains=%d",
		  symt->name,file_has_contains);
	}
#endif
	if( file_has_contains ) {
	  ProcInterface interface;
	  if( (lookahead_result = search_for_internal(symt->name,&interface)) ) {
#ifdef DEBUG_LOOKAHEAD
	    if(debug_latest) {
	      fprintf(list_fd,"\n %s %s found",
		      lookahead_result==LOOKAHEAD_INTERNAL?"internal":"module",
		      symt->name);
	    }
#endif
	       /* Fill in local symbol table entry with info about subroutine.
		  It may actually be a function invoked by CALL so copy the
		  interface data as is.
		*/

	    copy_interface_to_symtab(symt,&interface);

	  }
	}
       }
      }	/* end look for internal */

		/* It is not intrinsic: install in global table */
      symt->external = TRUE;
      if((!symt->argument) &&
	     ((gsymt=(hashtab[h].glob_symtab)) == NULL ||
	      (lookahead_result == LOOKAHEAD_INTERNAL && !gsymt->internal_subprog) ||
	      (lookahead_result == LOOKAHEAD_MODULE && !gsymt->module_subprog)) ) {
	gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
	gsymt->info.arglist = NULL;
	gsymt->kind = 0;
		/* If lookahead found it, set appropriate flag */
	if( lookahead_result == LOOKAHEAD_INTERNAL)
	  gsymt->internal_subprog = TRUE;
	else if( lookahead_result == LOOKAHEAD_MODULE )
	  gsymt->module_subprog = TRUE;
      }

      call_external(symt,id,arg);

    }

    /* For elemental function, args must be conformable.  Result takes
       shape of array args.
     */
    if( symt->elemental ) {
      check_elemental_args(id,arg);
    }

    if(! symt->used_flag) { /* record first line where used */
	symt->line_used = id->line_num;
	symt->file_used = inctable_index;
    }

    symt->used_flag = TRUE;

}/*call_subr*/


    /* Copies information from a procedure interface to symtab.
       Used by lookahead kluge.
     */
PRIVATE
void copy_interface_to_symtab(Lsymtab *symt, const ProcInterface *interface)
{
  symt->type = type_pack(class_SUBPROGRAM,interface->datatype);
  symt->size = interface->size;
  symt->kind = interface->kind;
  symt->kind_is_bogus = interface->kind_is_bogus;
  symt->array_var = interface->array;
  symt->array_dim = interface->array_dim;
  symt->pointer = interface->pointer;
  symt->elemental = interface->elemental;
  symt->pure = interface->pure;
  symt->recursive = interface->recursive;
}
		/* check out consistency of intrinsic argument list */
PRIVATE
void
#if HAVE_STDC
check_intrins_args(Token *id, Token *arg)
#else /* K&R style */
check_intrins_args(id, arg)
	Token *id;
	Token *arg;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	IntrinsInfo *defn=symt->info.intrins_info;
	unsigned args_given = ((arg == NULL)?0:arg_count(arg->next_token));
	int numargs;
	unsigned short flags;
	Token *t;

	numargs = defn->num_args;
	flags = defn->intrins_flags;

			/* positive numargs: must agree */
	if( (numargs >= 0 && (args_given != (unsigned)numargs))
			/* 1 or 2 arguments allowed */
	 || (numargs == I_1or2 && (args_given != 1 && args_given != 2))
			/* numargs == -2: 2 or more */
	 || (numargs == I_2up && (args_given < 2))
			/* 1 2 or 3 arguments allowed */
	 || (numargs == I_1to3 && (args_given < 1 || args_given > 3))
			/* 1 2 3 or 4 arguments allowed */
	 || (numargs == I_1to4 && (args_given < 1 || args_given > 4))
			/* 0 or 1 argument allowed */
	 || (numargs == I_0or1 && (args_given != 0 && args_given != 1)) ){
		LINENO_t linenum;
		COLNO_t colnum;
		if(arg==NULL) {linenum=id->line_num; colnum=id->col_num;}
		else {linenum = arg->line_num; colnum = arg->col_num;}

		syntax_error(linenum,colnum,
		  "wrong number of arguments for intrinsic function");
		msg_tail(defn->name);
	}
#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\nIntrinsic %s :: ",defn->name);
	  if(arg != NULL)
	    print_expr_list(arg->next_token);
	}
#endif
	if(arg != NULL && numargs != 0) {

	  Token *prev_t,	/* one operand in type propagation  */
	         fake_op;	/* operator in binexpr_type call */

	  t = arg->next_token;
				/* Copy type & size info into result */
	  arg->tclass = t->tclass;
	  arg->tsubclass = t->tsubclass;
#ifndef TOK_type
	  arg->TOK_type = t->TOK_type;
#endif
#ifndef TOK_flags
	  arg->TOK_flags = t->TOK_flags;
#endif
	  arg->size = t->size;
	  prev_t = t;

	  while(t != NULL) {
	    if(intrins_arg_cmp(defn,t)) {
				/* Propagate data type thru the list.
				   Resulting type info is stored in
				   args token.  */
	      if(prev_t != t && ! (flags & I_MIXED_ARGS) ) {
				/* Set up a pretend expr term for binexpr */
		fake_op.tclass = ',';
		fake_op.line_num = prev_t->line_num;
		fake_op.col_num = prev_t->col_num;
		fake_op.src_text = ",";

		binexpr_type(prev_t,&fake_op,t,arg);
	      }
	      prev_t = t;
	    }
	    t = t->next_token;
	  }/* end while */

	}/* end arg != NULL */
}/* check_intrins_args */



PRIVATE
void
#if HAVE_STDC
check_stmt_function_args(const Lsymtab *symt, Token *id, Token *arg)
#else /* K&R style */
check_stmt_function_args(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
#endif /* HAVE_STDC */
{
	unsigned n1,n2,n;
	unsigned i;
	Token *t1,*t2;

	t1 = symt->info.toklist->tokenlist;
	t2 = ((arg==NULL)? NULL: arg->next_token);

	n1 = arg_count(t1);
	n2 = arg_count(t2);

	if(n1 != n2) {
	    syntax_error(id->line_num,id->col_num,
		"function invoked with incorrect number of arguments");
	}

	n = (n1 < n2? n1: n2);
	for(i=0; i<n; i++) {
#ifdef OLDSTUFF
	    if( t1->TOK_type != t2->TOK_type) {
		syntax_error(t2->line_num,t2->col_num,
		  "function argument is of incorrect datatype");
	    }
#else
	    stmt_fun_arg_cmp(symt,t1,t2);
#endif
	    t1 = t1->next_token;
	    t2 = t2->next_token;
	}
}


void
declare_type(Token *id, int datatype, kind_t kind, long int size, char *size_text)
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

	if( (symt) == NULL) {
	   symt = install_local(h,datatype,class_VAR);
	   symt->kind = kind;
	   symt->size = size;
	   symt->size_is_adjustable = id->size_is_adjustable;
	   symt->size_is_expression = id->size_is_expression;
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */

			/* Intrinsic: see if type is consistent */
	  if( symt->intrinsic ) {
	    IntrinsInfo *defn = symt->info.intrins_info;
	    int rettype = defn->result_type,
		argtype = defn->arg_type;
			/* N.B. this test catches many but not all errors */
	    if( (rettype != type_GENERIC && datatype != rettype)
	     || (rettype == type_GENERIC && !((1<<datatype) & argtype)) ){
	      if(misc_warn) {
		    warning(id->line_num,id->col_num,
				"Declared type ");
		    msg_tail(type_name(datatype));
		    msg_tail(" is invalid for intrinsic function: ");
		    msg_tail(symt->name);
	      }
	    }
	  }

	  /* If symbol is not in current scoping unit, then this declaration
	     masks the existing one.  Install a new symtab entry.
	   */
	  {int new_here = ! in_curr_scope(symt);
	  if ( new_here ) {
	      symt = install_local(h,datatype,class_VAR);
	      symt->kind = kind;
	  }

	  if(!new_here && datatype_of(symt->type) != type_UNDECL) {
	      syntax_error(id->line_num,id->col_num,
		"symbol redeclared: ");
	  	msg_tail(symt->name);
	  }
	  else {
			/* Now give it the declared type */
	      symt->type = type_pack(storage_class_of(symt->type),datatype);

	      symt->kind = kind;
	      symt->size = size;
	      symt->size_is_adjustable = id->size_is_adjustable;
	      symt->size_is_expression = id->size_is_expression;
				/* Type declaration overrides implicit
				   declarations as line where declared.
				 */
	      symt->line_declared = id->line_num;
	      symt->file_declared = inctable_index;


	      /* If function has a RESULT variable that is later
	       * declared, function gets same type as RESULT variable.
	       * (ENTRY is handled differently: if it has a RESULT
	       * variable, that needs to be declared in specifications
	       * prior to ENTRY, so it gets type from RESULT variable in
	       * do_result_spec.)
	       */
	      if(symt->result_var && !symt->entry_point) {
		      /* Find the function this variable is in */
		  Lsymtab *func = hashtab[current_prog_unit_hash].loc_symtab;
		     /* Give function the variable's type, and it has status
		      * of a subprog. */
		  func->type = type_pack(class_SUBPROGRAM, datatype);
		  func->size = size;
		  func->kind = kind;
	      }
	  }
	  }

			/* Issue error if already defined as a parameter */
	  if( symt->parameter ) {
	    syntax_error(id->line_num,id->col_num,
			 "type declaration must precede PARAMETER definition");
	  }
	}

		/* If character type, save the source text of the size
		   specification.  If it is an array already
		   dimensioned, add size_text to tail of src.textvec,
		   otherwise place size_text in src.text if it is
		   character type, except for parameter, which
		   shouldn't happen.
		 */

	if( datatype_of(symt->type) == type_STRING ) {
	  if(symt->array_var) {
	    int i, dims = array_dims(symt->array_dim);
	    char **tvec = new_textvec(dims+1);

	    for(i=0; i<dims; i++)	/* Copy the old list over */
	      tvec[i] = symt->src.textvec[i];

	    tvec[dims] = size_text; /* Copy size text to new last element */

	    free_textvec(symt->src.textvec); /* Free the old list */

	    symt->src.textvec = tvec; /* Replace old list with new */
	  }
	  else if( ! symt->parameter ) {
	    symt->src.text = size_text;
	  }
	}

#ifdef DEBUG_EXPRTREES
	      if(debug_latest) {
		fprintf(list_fd,"\n      %s",type_name(datatype));
		size_text = get_size_text(symt,0);
		if(size_text != NULL) {
		  fprintf(list_fd," * %s",size_text);
		}
		else {
		  if(symt->size != size_DEFAULT)
		  fprintf(list_fd," * %ld",symt->size);
		}
		fprintf(list_fd," %s",symt->name);
	      }
#endif

			/* Under -port=long-string warn if char size > 255 */
	if(port_long_string) {
	  if(datatype == type_STRING && size > 255)
	    nonportable(id->line_num,id->col_num,
			"character variable length exceeds 255");
	}
}/*declare_type*/

void
#if HAVE_STDC
def_arg_name(Token *id)		/* Process items in argument list */
#else /* K&R style */
def_arg_name(id)		/* Process items in argument list */
#endif /* HAVE_STDC */

#if HAVE_STDC
#else /* K&R style */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;


			/* Dummy args mask names in enclosing scope. */
	if( (symt=hashtab[h].loc_symtab) == NULL || ! in_curr_scope(symt)) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->kind = kind_DEFAULT_UNKNOWN;
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	symt->argument = TRUE;
}/*def_arg_name*/


void
#if HAVE_STDC
def_array_dim(Token *id, Token *arg)	/* Process dimension lists */
	               	     /* arg previously defined as int */
#else /* K&R style */
def_array_dim(id,arg)	/* Process dimension lists */
	Token *id,*arg;	     /* arg previously defined as int */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;


	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */
	   if(storage_class_of(symt->type) != class_VAR) {
	      syntax_error(id->line_num,id->col_num,
		"Entity cannot be dimensioned: ");
		msg_tail(symt->name);
	      return;
	   }
	}

	symt->array_var = TRUE;
	
	if(!equivalence_flag && !allocatable_flag){      /* some checking should be done here */
	  if(array_dims(symt->array_dim) != 0 || array_dims(symt->array_dim) != 0)
	      syntax_error(id->line_num,id->col_num,
		"Array redimensioned");
	   else {
	      symt->array_dim = array_dim_info(arg->TOK_dims,
						    arg->TOK_elts);
	   }

	}

		/* Save text of dimension exprs in a list of strings
		   in symtab entry.  If array is of type character,
		   the text of size expression is already in src.text,
		   and is saved at tail of the list of dim strings. */

	{
	  int i, dims=arg->TOK_dims,
	      is_char = (datatype_of(symt->type) == type_STRING);
	  char **tvec;
	  char *size_text=symt->src.text;
	  Token *t;
	  int auto_array=FALSE;	/* records whether automatic array */
				/* Restore dim list to correct order */
	  arg->next_token = reverse_tokenlist(arg->next_token);

	  symt->src.textvec = tvec = new_textvec(is_char?dims+1:dims);

				/* Store dimension expr text in list */
	  for(i=0, t=arg->next_token; i<dims; i++, t=t->next_token) {
	    tvec[i] = ( t->left_token == NULL ?
		       new_tree_text(t):
		       new_tree_text(t->left_token) );

				/* Do some -f77 checking while we're here */
	    if( !symt->argument && !is_true(PARAMETER_EXPR,t->TOK_flags) ) {
				/* Section 5.1.2.1 */
		auto_array = TRUE;
		if(f77_automatic_array ) {
		    nonstandard(t->line_num,t->col_num,0,0);
		    msg_tail(": local array cannot have variable size");
		}
	    }
	  }
				/* Novices sometimes put FUNCTION decl late.
				   Only likely in a type declaration stmt.
				 */
	  if(auto_array && novice_help && curr_stmt_class != tok_DIMENSION
	     && curr_stmt_class != tok_COMMON
	     && strncmp(symt->name,"FUNCTION",8) == 0) {
	      warning(id->line_num,id->col_num,
		"Possible intended function declaration is not first line of prog unit");
	  }
				/* If character type, store size expr
				   text in tail of list. */
	  if(is_char)
	    tvec[dims] = size_text;

#ifdef DEBUG_EXPRTREES
	  if(debug_latest) {
	    int type=datatype_of(symt->type);
	    fprintf(list_fd,"\n      %s",
		    (type == type_UNDECL)?"DIMENSION":type_name(type));
	    if(is_char)
	      fprintf(list_fd," * %s",symt->src.textvec[dims]);

	    fprintf(list_fd," %s ( ",symt->name);
	    for(i=0; i<dims; i++) {
	      fprintf(list_fd,"%s",symt->src.textvec[i]);
	      if(i < dims-1)
		fprintf(list_fd," , ");
	    }
	    fprintf(list_fd," )");
	  }
#endif

	}

}/*def_array_dim*/


void
#if HAVE_STDC
def_com_block(Token *id, Token *comlist)	/* Process common blocks and save_stmt */
#else /* K&R style */
def_com_block(id,comlist)	/* Process common blocks and save_stmt */
	Token *id, *comlist;
#endif /* HAVE_STDC */

{
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
   	TokenListHeader *TH_ptr;
	extern LINENO_t true_prev_stmt_line_num;/* set by fortran.y */

		/* Install name in global symbol table */
	if( (gsymt=hashtab[h].com_glob_symtab) == NULL) {
	   gsymt = install_global(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   gsymt->info.comlist = NULL;
	}


	if( (symt = hashtab[h].com_loc_symtab) == NULL || !in_curr_scope(symt)){
	   symt = install_local(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
	}

				/* Record 1st location of declaration */
	if( symt->info.toklist == NULL ) {
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}

	if(pretty_multiple_common) {

		/* Flag declarations of same block in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if( symt->info.toklist != NULL
	   && symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num) {
	    ugly_code(id->line_num,id->col_num,
		"Common block declared in more than one statement");
	  }
	}

		/* Insert the new list onto linked list of token lists */
	if(comlist != NULL) {
	  	/* Will be NULL only for SAVE, in which case skip */
	    TH_ptr= make_TL_head(id);

 	    TH_ptr->tokenlist = comlist->next_token;
	    TH_ptr->next = symt->info.toklist;
            symt->info.toklist = TH_ptr;
	    {
			/* For each variable in the list, record a pointer
			   to this common block and variable's index in
			   the block. Note that token list is still in
			   reverse order at this time, so we count backwards.
			*/
	      Token *c = comlist->next_token;
	      int indx;
				/* Add to the block's total count, and
				   start indexing there. */
	      indx = (symt->common_index += arg_count(c));
	      while(c != NULL) {
		Lsymtab *com_var = hashtab[c->value.integer].loc_symtab;
		com_var->common_block = gsymt;
		com_var->common_index = indx--;
		c = c->next_token;
	      }
	    }
	}

	if(! symt->used_flag) { /* record first line where used */
	    symt->line_used = id->line_num;
	    symt->file_used = inctable_index;
	}

	if(! symt->set_flag) { /* record first line where set */
	    symt->line_set = id->line_num;
	    symt->file_set = inctable_index;
	}

   	symt->set_flag = TRUE;
	symt->used_flag = TRUE;
}/*def_com_block*/


void
#if HAVE_STDC
def_com_variable(Token *id)		/* Process items in common block list */
#else /* K&R style */
def_com_variable(id)		/* Process items in common block list */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */
	    if(symt->common_var	/* Already in common? */
				/* But if it is equivalenced, suppress
				   the warning.  Equivs in common are not
				   handled in present version. */
	       && symt->equiv_link == symt ) {
		syntax_error(id->line_num,id->col_num,
		     "Variable cannot be in common twice");
	    }
	    else if(symt->entry_point || symt->parameter ||
		    symt->argument || symt->external || symt->intrinsic) {
		syntax_error(id->line_num,id->col_num,
		     "Item cannot be placed in common");
		return;
	    }
	    if(symt->size == size_ADJUSTABLE) {	/* CHARACTER *(*) */
	      syntax_error(id->line_num,id->col_num,
		    "Common variable cannot have adjustable size");
	      symt->size = 1;
	    }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->common_var = TRUE; /* set the flag even if not legit */
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*def_com_variable*/


	/* This guy sets the flag in symbol table saying the id is the
	   current prog unit.  It returns the hash code for later reference.
	   Also bookmarks the source line so the declaration can be found
	   in src buffer (currently only used by mkhtml).
	 */
int
#if HAVE_STDC
def_curr_prog_unit(Token *id)
#else /* K&R style */
def_curr_prog_unit(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int hashno = id->value.integer;
	hashtab[hashno].loc_symtab->is_current_prog_unit = TRUE;

	mark_prog_unit_srcline(id->line_num);	/* save mkhtml_bookmark */

	return hashno;
}/*def_curr_prog_unit*/


void
def_do_variable(Token *id)	/* Treat DO index variable in DO stmt */
{
	int h=id->value.integer;
	Lsymtab *symt;
	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}
	else {
	   if(symt->active_do_var) {
	      syntax_error(id->line_num,id->col_num,
		   "DO variable is already in use in an enclosing DO loop");
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv->used_flag = TRUE;
	equiv->active_do_var = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}


void
#if HAVE_STDC
def_equiv_name(Token *id)		/* Process equivalence list elements */
#else /* K&R style */
def_equiv_name(id)		/* Process equivalence list elements */
	Token *id;
#endif /* HAVE_STDC */
{
  ref_variable(id);		/* Put it in symtab */
	/* No other action needed: processing of equiv pairs is
	   done by equivalence() */
}/*def_equiv_name*/



void
#if HAVE_STDC
def_ext_name(Token *id)		/* Process external lists */
#else /* K&R style */
def_ext_name(id)		/* Process external lists */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
	   symt->info.toklist = NULL;
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
        }
	else if(symt->entry_point){ /* protect ourself from nonsense */
	    syntax_error(id->line_num,id->col_num,
		"Subprogram cannot declare itself external:");
	    msg_tail(symt->name);
	    return;
	}
	else if(symt->parameter){ /* worse nonsense */
	    syntax_error(id->line_num,id->col_num,
		"Identifier was previously declared non-external:");
	    msg_tail(symt->name);
	    return;
	}
	else {
			/* Symbol seen before: check it & change class */

	    if(storage_class_of(symt->type) == class_VAR) {
	      symt->info.toklist = NULL;
	    }
	    symt->type = type_pack(class_SUBPROGRAM,datatype_of(symt->type));
	}

	if(symt->intrinsic){
	    syntax_error(id->line_num,id->col_num,
		"Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	    symt->external = TRUE;
	    if(!symt->argument){
	        TokenListHeader *TH_ptr;
		Gsymtab *gsymt;
		if( (gsymt=hashtab[h].glob_symtab) == NULL) {
	   	    gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
	   	    gsymt->info.arglist = NULL;
		}
		TH_ptr=make_TL_head(id);

		TH_ptr->external_decl = TRUE;
		TH_ptr->next = symt->info.toklist;
		symt->info.toklist = TH_ptr;
	    }
	}
	symt->declared_external = TRUE;

}/*def_ext_name*/



void def_function(int datatype, long int size, char *size_text, kind_t kind,
		  Token *id, Token *args, SUBPROG_TYPE subprogtype)
{
	int storage_class;
	int h=id->value.integer;
	Lsymtab *symt, *old_symt;
	Gsymtab *gsymt;
	TokenListHeader *TH_ptr;
   	storage_class = class_SUBPROGRAM;

   	if((symt = (hashtab[h].loc_symtab)) == NULL || !(in_curr_scope(symt)) ) {
	   old_symt = symt;	/* save masked entry */
			/* Symbol is new to local symtab: install it.
			   Since this is the current routine, it has
			   storage class of a variable. */
	   symt = install_local(h,datatype,class_VAR);
	   symt->kind = kind;
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	   symt->size = size;
	   symt->src.text = size_text;


	   /*  Masked entry, if in enclosing scope, must be a reference
	       internal subprogram by containing prog unit or a
	       declaration (e.g. PRIVATE) by containing module.  In
	       latter case, module did not know this is a subprogram,
	       so we need to update its symtab entry.
	   */
	   if (subprogtype == module_subprog && in_enclosing_scope(old_symt)) {

	      	/* Symbol seen before: check it & change class */

	      if(storage_class_of(old_symt->type) == class_VAR) {
	          old_symt->type = type_pack(class_SUBPROGRAM,datatype);
	          old_symt->info.toklist = NULL;

	           /* copy accessibility attribute to this entry */
	          symt->private = old_symt->private;
	          symt->public = old_symt->public;
	     }
	   }
	}

	if(! symt->entry_point)	/* seen before but not as entry */
	   symt->info.toklist = NULL;

	if(symt->external) {	/* warn if entry point was declared external */
	    syntax_error(id->line_num,id->col_num,
		"Entry point was declared external:");
	    msg_tail(symt->name);
				/* try to undo the damage */
	    symt->type = type_pack(class_VAR,datatype_of(symt->type));
	    symt->external = FALSE;
	}

			/* Symbol is new to global symtab: install it.  If it is
			   an internal or module subprog that masks an external
			   of same name, install it.
			 */
	if((gsymt = (hashtab[h].glob_symtab)) == NULL ||
	   (subprogtype == module_subprog && !gsymt->module_subprog) ||
	   (subprogtype == internal_subprog && !gsymt->internal_subprog)) {
	  gsymt = install_global(h,datatype,storage_class);
	  gsymt->size = size;
	  gsymt->kind = kind;
	  gsymt->info.arglist = NULL;
	}
	else {
			/* Symbol is already in global symtab. Put the
			   declared datatype into symbol table. */
	  gsymt->type = type_pack(storage_class,datatype);
	  gsymt->size = size;
	  gsymt->kind = kind;
	}

	/* Test to turn on internal or module subprogram flags */
	if (subprogtype == module_subprog) {
		gsymt->module_subprog = TRUE;
		gsymt->internal_subprog = FALSE;
	}
	else if (subprogtype == internal_subprog) {
		gsymt->internal_subprog = TRUE;
		gsymt->module_subprog = FALSE;
	}
	else {
		gsymt->internal_subprog = FALSE;
		gsymt->module_subprog = FALSE;
	}

				/* Restore args list to original order */
	if(args != NULL)
	  args->next_token = reverse_tokenlist(args->next_token);

		/* Insert the new list onto linked list of token lists */
   	TH_ptr=make_TL_head(id);

			/* If this is an implied PROGRAM statement it may
			   occur in an include file, which we do not want
			   to appear in diagnostic messages about it. */
	if(top_filename != current_filename && datatype == type_PROGRAM) {
	  TH_ptr->filename = top_filename;
	  TH_ptr->line_num = top_file_line_num;
	}

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	TH_ptr->is_defn = TRUE;

	symt->info.toklist = TH_ptr;

	symt->entry_point = TRUE;

		/* library mode: set the flag so no complaint will
		   be issued if function never invoked. */
	if(library_mode)
		symt->library_prog_unit = TRUE;
	if(datatype == type_PROGRAM) {
#ifdef VCG_SUPPORT		/* Get name of file containing main prog unit */
		main_filename = top_filename;
#endif
	}

	/* Test if it is a function to turn on result_var flag.
	   This may be changed later if there is a RESULT spec.
	 */
	switch(datatype) {
	    case type_MODULE:
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
	    case type_SUBROUTINE:	/* Subroutine return: OK */
		break;
	    default:
		symt->result_var = TRUE;
		break;
	}
}/*def_function*/



void
#if HAVE_STDC
def_intrins_name(Token *id)		/* Process intrinsic lists */
#else /* K&R style */
def_intrins_name(id)		/* Process intrinsic lists */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	   symt->info.toklist = NULL;
        }
	else {
			/* Symbol seen before: check it & change class */
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }

	  symt->type = type_pack(class_SUBPROGRAM,datatype_of(symt->type));
	}

		/* Place info about intrinsic datatype in local symtab.
		   If not found, it will be treated as external.
		 */

	if(symt->external){
	    syntax_error(id->line_num,id->col_num,
	       "Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	  IntrinsInfo *defn;
	  symt->declared_intrinsic = TRUE;
	  if( (defn=find_intrinsic(symt->name)) == NULL ) {
	     if(misc_warn) {
	       warning(id->line_num,id->col_num,
			"Unknown intrinsic function: ");
	       msg_tail(symt->name);
	       msg_tail("Treated as if user-defined");
	     }
				/* Here treat as if EXTERNAL declaration */
	     def_ext_name(id);
	     return;
	   }
	   else {
			/* Found in info table: set intrins flag and store
			   pointer to definition info. */
	     symt->intrinsic = TRUE;
	     symt->info.intrins_info = defn;
	     if( INTRINS_ID(defn->intrins_flags) == I_NULL ) {
	       symt->pointer = TRUE;
	     }
	   }
	}
	symt->declared_external = TRUE;
}/*def_intrins_name*/

/* Create a symbol table entries for a module that was inherited by a
 * USE statement.  Note that inheritance may be implicit, i.e. if
 * statement is USE B, and module B contains USE A, then A is also
 * inherited.
 */
void def_module(Token *id, Token *only, int only_list_mode)
{
    int h = id->value.integer;
    Lsymtab *symt = NULL;
    Gsymtab *gsymt = NULL;
    TokenListHeader *TH_ptr;

    if( (symt = hashtab[h].loc_symtab) == NULL || !in_curr_scope(symt) ) {
	symt = install_local(h, type_MODULE, class_MODULE);
	symt->size = size_DEFAULT;
	symt->kind = 0;
	symt->line_declared = id->line_num;
	symt->file_declared = inctable_index;
    }

    if( (gsymt = hashtab[h].glob_symtab) == NULL ) {
	gsymt = install_global(h, type_MODULE, class_MODULE);
	gsymt->size = size_DEFAULT;
	gsymt->kind = 0;
	gsymt->info.arglist = NULL;	/* not used */
	gsymt->modvarlist = NULL;
	gsymt->internal_subprog = FALSE;
	gsymt->module_subprog = FALSE;
    }

    /* create a stub token list */
    TH_ptr=make_TL_head(id);

    TH_ptr->tokenlist = NULL;
    TH_ptr->next = symt->info.toklist;

    symt->info.toklist = TH_ptr;
}

void
#if HAVE_STDC
def_namelist(Token *id, Token *list)		/* Process NAMELIST declaration */
#else /* K&R style */
def_namelist(id,list)		/* Process NAMELIST declaration */
     Token *id,*list;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;
	extern LINENO_t true_prev_stmt_line_num;/* set by fortran.y */

	if( (symt=hashtab[h].loc_symtab) == NULL) {
				/* First encounter: install in local symtab */
	  symt = install_local(h,type_NAMELIST,class_NAMELIST);
	  symt->line_declared = id->line_num;
	  symt->file_declared = inctable_index;
	  symt->info.toklist = NULL;
	}
			/* protect ourself against nonsense */
	else if( symt->array_var || symt->parameter || symt->entry_point ) {
	  syntax_error(id->line_num,id->col_num,
		       "identifier was previously declared a non-namelist");
	  return;
	}
	else if(pretty_multiple_namelist) {

		/* Flag declarations of same namelist in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if((symt->info.toklist != NULL) && 
	    (symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num)) {
	    ugly_code(id->line_num,id->col_num,
		"Namelist declared in more than one statement");
	  }
	}

	call_external(symt,id,list); /* attach list to symt->info.toklist */

}/*def_namelist*/


void
#if HAVE_STDC
def_namelist_item(Token *id)		/* Process NAMELIST list elements */
#else /* K&R style */
def_namelist_item(id)		/* Process NAMELIST list elements */
	Token *id;
#endif /* HAVE_STDC */
{
  ref_variable(id);		/* Put it in symtab */
}/*def_namelist_name*/


void
#if HAVE_STDC
def_parameter(Token *id, Token *val, int noparen)/* Process parameter_defn_item */
#else /* K&R style */
def_parameter(id,val,noparen)	/* Process parameter_defn_item */
	Token *id,*val;
	int noparen;		/* parenthesis-less form */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {			/* protect ourself against nonsense */
	   if( symt->external || symt->intrinsic 
	       || symt->entry_point ) {
	      syntax_error(id->line_num,id->col_num,
		   "identifier cannot be a parameter:");
	      msg_tail(symt->name);
	      return;
	   }
	}

	if(! symt->set_flag) { /* record first line where set */
	    symt->line_set = id->line_num;
	    symt->file_set = inctable_index;
	}

	symt->set_flag = TRUE;
	symt->parameter = TRUE;
	symt->info.param = new_param_info();
	symt->info.param->seq_num = ++parameter_count;

		/* If parameter type is not declared, then if it is DEC
		   parenthesis-less form (and -source=dec-param not given)
		   or if standard form and -source=parameter-implicit option
		   is given, get type from value.  Warn about it under -f77,
		   or under -port if the data type is not same as F77 default.
		*/
	if( ((noparen && !source_dec_param_std_type)
	   ||(!noparen && source_param_implicit)) &&
	    (datatype_of(symt->type) == type_UNDECL) ) {
	  int val_type = datatype_of(val->TOK_type);
	  if( f77_param_implicit_type || f90_param_implicit_type ) {
	    nonstandard(id->line_num,id->col_num,f90_param_implicit_type,0);
	    msg_tail(": PARAMETER implicitly typed");
	    if( get_type(symt) != val_type )
	      msg_tail("differently from default type");
	  }
	  else if( port_param_implicit_type && 
		   get_type(symt) != val_type ) {
	    nonportable(id->line_num,id->col_num,
	      ": PARAMETER implicitly typed differently from default type");
	  }
	  symt->type = type_pack(class_VAR,val_type);
	  symt->size = val->size;
	}

		/* Integer parameters: save value in symtab entry.  Other
		   types not saved.  Need these since used in array dims */
	switch(get_type(symt)) {
		case type_INTEGER:
			symt->info.param->value.integer = int_expr_value(val);
#ifdef DEBUG_PARAMETERS
if(debug_latest)
(void)fprintf(list_fd,"\nPARAMETER %s = %d",
	      symt->name,symt->info.param->value.integer);
#endif
			break;
			/* Character parameter: if declared adjustable
			   i.e. *(*) then inherit size of const */
		case type_STRING:
			if(symt->size == size_ADJUSTABLE
			   && datatype_of(val->TOK_type) == type_STRING)
			  symt->size = val->size;
			symt->info.param->value.string = char_expr_value(val);
			break;
		case type_REAL:
		case type_DP:
		case type_COMPLEX:
			symt->info.param->value.dbl = float_expr_value(val);
		default:
			break;
	}

			/* Save the source text of value for declaration */

	symt->info.param->src_text = new_tree_text(
		(val->left_token == NULL?
			val:			/* Primary */
			val->left_token)	/* Expr tree */
	        );

#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\n      PARAMETER ( %s = %s ) ",
		  symt->name,
		  symt->info.param->src_text);
	}
#endif

}/*def_parameter*/


void def_result_name(Token *id)		/* Place result name in symtab */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL
		|| ! in_curr_scope(symt)) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	/* result_var flag is not set here.  It is set by do_result_spec
	   when entry name is available to have its flag unset.
	 */
}/*def_result_name*/


void    	       /* Installs statement function name in local table */
#if HAVE_STDC
def_stmt_function(Token *id, Token *args)
#else /* K&R style */
def_stmt_function(id, args)
	Token *id, *args;
#endif /* HAVE_STDC */
{
	int t,h=id->value.integer;
	Lsymtab *symt;
   	TokenListHeader *TH_ptr;

   	if((symt = (hashtab[h].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it. */

	   symt = install_local(h,type_UNDECL,class_STMT_FUNCTION);
	   symt->info.toklist = NULL;
	}
	else {
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }
	}
	symt->line_declared = id->line_num;
	symt->file_declared = inctable_index;

		/* Restore args to original order for sake of checking phase */
	if(args != NULL)
	  args->next_token = reverse_tokenlist(args->next_token);

		/* Save dummy arg list in symbol table */
    	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

		/* check, check, check ... */
	if(storage_class_of(symt->type) == class_VAR)
	   symt->type = type_pack(class_STMT_FUNCTION,t);

	symt->external = TRUE;
}/*def_stmt_function*/




void
#if HAVE_STDC
do_ASSIGN(Token *id)		/* Process ASSIGN statement */
#else /* K&R style */
do_ASSIGN(id)		/* Process ASSIGN statement */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	   if(symt->active_do_var) {
	      syntax_error(id->line_num,id->col_num,
			   "Cannot assign label to active DO index");
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}/*do_ASSIGN*/




void
#if HAVE_STDC
do_assigned_GOTO(Token *id)		/* Process assigned_goto */
#else /* K&R style */
do_assigned_GOTO(id)		/* Process assigned_goto */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	if(! equiv->set_flag)
	   equiv->used_before_set = TRUE;
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*do_assigned_GOTO*/


void
#if HAVE_STDC
do_ENTRY(Token *id, Token *args, int hashno)	/* Processes ENTRY statement */
#else /* K&R style */
do_ENTRY(id,args,hashno)	/* Processes ENTRY statement */
	Token *id,*args;
	int hashno;
#endif /* HAVE_STDC */
{
	int datatype;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
	    case type_COMMON_BLOCK:
	        syntax_error(id->line_num,NO_COL_NUM,
			"You cannot have an entry statement here");
		break;
	    case type_SUBROUTINE:	/* Subroutine entry */
		def_function(type_SUBROUTINE,size_DEFAULT,(char *)NULL,
			     (kind_t)0,id,args,tok_SUBROUTINE);
		break;
	    default:		/* Function entry */
		def_function(type_UNDECL,size_DEFAULT,(char *)NULL,
			     default_kind(datatype),id,args,tok_FUNCTION);
		break;
	}

	hashtab[id->value.integer].loc_symtab->result_var = TRUE;
}/*do_ENTRY*/




	/* This routine checks whether a RETURN statement is valid at
	   the present location, and if it is, looks for possible
	   failure to assign return value of function.  Returns 1
	   if RETURN is valid here, 0 if not.
	*/
int
#if HAVE_STDC
do_RETURN(int hashno, Token *keyword)
	           	/* current prog unit hash number */
	               	/* tok_RETURN, or tok_END if implied RETURN */
#else /* K&R style */
do_RETURN(hashno,keyword)
	int hashno;	/* current prog unit hash number */
	Token *keyword;	/* tok_RETURN, or tok_END if implied RETURN */
#endif /* HAVE_STDC */
{
	int i,datatype, valid=1;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_MODULE:
	    case type_BLOCK_DATA:
		if(keyword->tclass == tok_RETURN) {
		    syntax_error(keyword->line_num,keyword->col_num,
		    	"You cannot have a RETURN statement here!");
		    valid = 0;
		}
		break;
	    case type_SUBROUTINE:	/* Subroutine return: OK */
		break;
	    default:		/* Function return: check whether result
				   variable has been assigned a value.
				   If function has ENTRY stmts, all
				   result variables are storage
				   associated, so only one needs to be set.
				*/
	      if(misc_warn &&
		 curr_scope_bottom < loc_symtab_top) { /* can't happen but... */
		int result_set=FALSE;
		for(i=curr_scope_bottom; i<loc_symtab_top; i++) {
		  if (loc_symtab[i].result_var && loc_symtab[i].set_flag) {
		    result_set = TRUE;
		    break;
		  }
		}
		if(! result_set) {
		  int result_var_count=0;
		  /* Function may have multiple result variables: report all */
		  for(i=curr_scope_bottom; i<loc_symtab_top; i++) {
		    if (loc_symtab[i].result_var) {
		      if(++result_var_count == 1) { /* first encountered */
			warning(keyword->line_num,keyword->col_num,
				"Result variable");
		      }
		      else {
			msg_tail("or");
		      }
		      msg_tail(loc_symtab[i].name);
		    }
		  }
		  msg_tail("not set when RETURN encountered");
		}
	      }

		break;
	}
	return valid;
}/*do_RETURN*/


/* Equivalence two symbol table entries by swapping equiv_links.
   Do not call this on entries that are already equivalenced.
   Equivalence items form a ring.
 */
void make_equivalent(Lsymtab *symt1, Lsymtab *symt2)
{
  Lsymtab *temp;

  if (symt1 == NULL || symt2 == NULL || symt1 == symt2)
    return;

  temp = symt1->equiv_link;
  symt1->equiv_link = symt2->equiv_link;
  symt2->equiv_link = temp;

  return;
}

/* Make result variables of function and entry point equivalent.
 * If function/entry has no RESULT spec then the function/entry
 * variable itself is the result variable.
 */
void equivalence_result_vars(int result_hashno)
{
  int i;
  Lsymtab *result = hashtab[result_hashno].loc_symtab;

  /* Search for first result variable and equiv this new one to it. */
  for (i = curr_scope_bottom; i < loc_symtab_top; i++) {
    if( loc_symtab[i].result_var ) {
      make_equivalent(result, &loc_symtab[i]);
      break;
    }
  }

  return;
}


void do_result_spec(Token *p, int hashno, int entry_hashno)
{
  Lsymtab *func = hashtab[hashno].loc_symtab; /* prog unit */
  Lsymtab *entry = hashtab[entry_hashno].loc_symtab; /* entry */
  Lsymtab *result = hashtab[p->value.integer].loc_symtab; /* result var */
  int entry_datatype = datatype_of(entry->type);
  int result_datatype = datatype_of(result->type);
  long entry_size = entry->size;
  long result_size = result->size;
  kind_t entry_kind = entry->kind;
  kind_t result_kind = result->kind;

  if( (strcmp(result->name, func->name) == 0) || 
      (strcmp(result->name, entry->name) == 0) ) {
      syntax_error(p->line_num, p->col_num,
	      "Result name must not be the same as entry name");
  }

  /* If function has type spec, the result variable gets its type */
  if (entry_datatype != type_UNDECL) {
      /* It is not legally possible for both entry and result to
	 have declared types.  Can only happen for ENTRY point.
       */
      if( result_datatype != type_UNDECL ) {
	  syntax_error(p->line_num,NO_COL_NUM,
		       "ENTRY name has a declared type");
      }
  }
  /* propagate RESULT type to ENTRY name */
  else if (result_datatype != type_UNDECL) {
      entry_datatype = result_datatype;
      entry_size = result_size;
      entry_kind = result_kind;
  }

  /* Turn the function/entry name into an external name in local symbol
   * table. Type is currently UNDECL but will get RESULT type
   */
  entry->result_var = FALSE;
  entry->type = type_pack(class_SUBPROGRAM, entry_datatype); 
  entry->size = entry_size;
  entry->kind = entry_kind;
  entry->external = TRUE;

  /* propagate return type of function if available
   * to RESULT variable 
   */
  result->result_var = TRUE;
  result->type = type_pack(class_VAR, entry_datatype);
  result->size = entry_size;
  result->kind = entry_kind;

#ifdef DEBUG_SUFFIX
    if(debug_latest) {
	fprintf(list_fd,"\nFUNCTION name = %s",
		func->name);
	fprintf(list_fd,"\nENTRY name = %s",
		entry->name);
	fprintf(list_fd,"\nRESULT variable = %s",
		result->name);
    }
#endif
}

/* check syntax errors in suffix */
void do_bind_spec(Token *p, SUBPROG_TYPE subprogtype) 
{
    Token *currToken = NULL;
    
    for (currToken = p; currToken; currToken = currToken->next_token) {
        /* C and NAME are type tok_identifier */
        if (currToken->left_token &&
            currToken->left_token->tclass == tok_identifier) {
            if (currToken->tclass == tok_BIND) { 

#ifdef DEBUG_SUFFIX
if (debug_latest) {
    fprintf(list_fd, "\nLanguage binding spec, language=%s",
	    hashtab[currToken->left_token->value.integer].name);
}
#endif

            /* binding language, only C */

		if (strcmp(hashtab[currToken->left_token->value.integer].name, "C") != 0) {
		    syntax_error(currToken->left_token->line_num, 
				 currToken->left_token->col_num,
				 "invalid binding language, must be C");
		}
            }

            /* NAME = character expression */
            else /* if (currToken->tclass != tok_BIND) */ {
#ifdef DEBUG_SUFFIX
if(debug_latest) {
    fprintf(list_fd, "\nLanguage binding spec, %s %s %s",
	    hashtab[currToken->left_token->value.integer].name, /* keywd NAME*/
	    currToken->src_text, /* = */
	    (datatype_of(currToken->next_token->TOK_type) == type_STRING
	     && is_true(EVALUATED_EXPR,currToken->next_token->TOK_flags))?
	     currToken->next_token->value.string:"<expr>"); /* bound name stored as string */
}
#endif
                if (strcmp(hashtab[currToken->left_token->value.integer].name, "NAME") != 0)
                    syntax_error(currToken->left_token->line_num, 
				 currToken->left_token->col_num,
				 "invalid language binding spec");
                else {
                    if (subprogtype == internal_subprog)
                        syntax_error(currToken->left_token->line_num, 
				     currToken->left_token->col_num,
				     "language binding spec with NAME not permitted in internal procedure");
                }
            }
        }
    }
}


/* process suffix part of subprogram declaration */
void
do_suffix(int class, SUBPROG_TYPE subprogtype, int hashno, Token *suffix, int entry_hashno)
{

    Token *currToken;
    for (currToken = suffix->next_token; currToken != NULL;
            currToken = currToken->next_token) {
        switch(currToken->tclass) {
        case tok_BIND:
            if (currToken->left_token != NULL) {
            do_bind_spec(currToken->left_token, subprogtype);
            }
            break;
        case tok_identifier:
            do_result_spec(currToken, hashno, entry_hashno);
            break;
        default:
            oops_message(OOPS_FATAL,currToken->line_num,currToken->col_num,
                 "unknown subprogram suffix item");
            break;
        }
    }
}

void
#if HAVE_STDC
equivalence(Token *id1, Token *id2)
#else /* K&R style */
equivalence(id1,id2)
     Token *id1, *id2;
#endif /* HAVE_STDC */
{
	int h1=id1->value.integer, h2=id2->value.integer;
	Lsymtab *symt1,*symt2;

		/* install the variables in symtab if not seen before */
	if( (symt1=hashtab[h1].loc_symtab) == NULL) {
	   symt1 = install_local(h1,type_UNDECL,class_VAR);
	   symt1->line_declared = id1->line_num;
	   symt1->file_declared = inctable_index;
	}
	if( (symt2=hashtab[h2].loc_symtab) == NULL) {
	   symt2 = install_local(h2,type_UNDECL,class_VAR);
	   symt2->line_declared = id2->line_num;
	   symt2->file_declared = inctable_index;
	}
			/* Check for legality.  Ought to do complementary
			   checks elsewhere.
			 */
	if(symt1 == symt2
	   || symt1->parameter || symt2->parameter
	   || symt1->entry_point || symt2->entry_point
	   || symt1->argument || symt2->argument
	   || symt1->external || symt2->external) {

		syntax_error(id1->line_num,id1->col_num,
			     "illegal to equivalence these");
	}
		/* now swap equiv_links so their equiv lists are united */
	else {
	   make_equivalent(symt1,symt2);
	}

		/* If either guy is in common, both are in common */
	if(symt1->common_var || symt2->common_var) {
	    Lsymtab *equiv=symt1;
	    do {
		equiv->common_var = TRUE;
		equiv = equiv->equiv_link;
	    } while(equiv != symt1);
	}
}

int
#if HAVE_STDC
get_size(const Lsymtab *symt, int type)			/* ARGSUSED1 */
#else /* K&R style */
get_size(symt,type)			/* ARGSUSED1 */
#endif /* HAVE_STDC */
		    /* Returns size of symbol if explicitly declared
		       or declared using IMPLICIT type*size statement.
		       Otherwise returns size_DEFAULT. */
#if HAVE_STDC
              			/* Evaluated datatype: not used at present */
#else /* K&R style */
     Lsymtab *symt;
     int type;			/* Evaluated datatype: not used at present */
#endif /* HAVE_STDC */
{
  int datasize=symt->size;
  int datatype = datatype_of(symt->type);
  if(datatype != type_UNDECL) /* Declared? */
    return datasize;		/* if declared, use it */
  else {
    int first_char=(int)symt->name[0];

    if(first_char == '$')  first_char = 'Z'+1;
    if(first_char == '_')  first_char = 'Z'+2;

	if (in_curr_scope(symt))
	{
	 	return implicit_info.size[first_char - 'A'];
	}
	else { /* Get implicit type declaration from previous scope */
		int s = find_scope(symt);
		return loc_scope[s].implicit.size[first_char - 'A'];
	}
  }
}

char *
#if HAVE_STDC
get_size_text(const Lsymtab *symt, int type)		/* ARGSUSED1 */
              			/* Evaluated datatype: not used at present */
#else /* K&R style */
get_size_text(symt,type)		/* ARGSUSED1 */
     Lsymtab *symt;
     int type;			/* Evaluated datatype: not used at present */
#endif /* HAVE_STDC */
{
  int datatype = datatype_of(symt->type);
  if(datatype != type_UNDECL) {
				/* Declared: use text in symtab entry */
    if(symt->array_var)
      return symt->src.textvec[array_dims(symt->array_dim)];
    else
      return symt->src.text;
  }
  else {
				/* Undeclared: use implicit value */
    int first_char=(int)symt->name[0];

    if(first_char == '$')  first_char = 'Z'+1;
    if(first_char == '_')  first_char = 'Z'+2;

	if (in_curr_scope(symt))
	{
	   return implicit_info.len_text[first_char - 'A'];
	}
	else { /* Get implicit type declaration from previous scope */
		int s = find_scope(symt);
		return loc_scope[s].implicit.len_text[first_char - 'A'];
	}
  }
}

/* Returns kind of symbol.  Argument type must be resolved type,
   having used implicit typing if necessary.
 */
kind_t
get_kind(const Lsymtab *symt, int type)
{
  kind_t id_kind;

	/* If variable was not declared, its kind parameter is set
	 * to the default kind parameter for its implicit type
	 */
  if( symt->type == type_UNDECL )
    id_kind = default_kind(type);
  else
    id_kind = symt->kind;

  if( id_kind == kind_DEFAULT_UNKNOWN ) /* if kind not set till now, */
    id_kind = default_kind(type);  /* set to default for type */

  return id_kind;
}

int
#if HAVE_STDC
get_type(const Lsymtab *symt)	/* Returns data type of symbol, using implicit if necessary */
#else /* K&R style */
get_type(symt)	/* Returns data type of symbol, using implicit if necessary */
	Lsymtab *symt;
#endif /* HAVE_STDC */
{
	int datatype = datatype_of(symt->type);

	if(datatype != type_UNDECL)	/* Declared? */
	   return datatype;		/*   Yes: use it */
	else if(storage_class_of(symt->type) == class_SUBPROGRAM
	     && !symt->invoked_as_func )
				/* Function never invoked: assume subr */
	   return type_SUBROUTINE;
	else if (symt->invoked_as_func && symt->intrinsic)
	{
	    IntrinsInfo *defn;

	    defn = find_intrinsic(symt->name);
	    if (defn != (IntrinsInfo *)NULL)
		return defn->result_type;
	}

	/* Fell through, so type must be determined by first letter of name */

	  int first_char=(int)symt->name[0];

			/* kluge: treat any nonalpha chars other than _
			   as if they are $.
			 */
	  if( !isalpha(first_char) && first_char != '_' )
	      first_char = 'Z'+1;
	  if(first_char == '_')  first_char = 'Z'+2;

	  if (in_curr_scope(symt))
	  {
	     return implicit_info.type[first_char - 'A'];
	  }
	  else { /* Get implicit type declaration from previous scope */
	  	int s = find_scope(symt);
	  	return loc_scope[s].implicit.type[first_char - 'A'];
	  }
}/*get_type*/


	/* hash_lookup finds identifier in hashtable and returns its
	   index.  If not found, a new hashtable entry is made for it,
	   and the identifier string s is copied to local stringspace.
	*/
unsigned
#if HAVE_STDC
hash_lookup(const char *s)
#else /* K&R style */
hash_lookup(s)
	char *s;
#endif /* HAVE_STDC */
{
        unsigned h;
	unsigned long hnum;

	hnum = hash(s);

	while(h = hnum%HASHSZ, hashtab[h].name != NULL
	          && strcmp(hashtab[h].name,s) != 0) {
			  hnum = rehash(hnum);	/* Resolve clashes */
	}

	if(hashtab[h].name == NULL) {
		    hashtab[h].name = new_local_string(s);
		    hashtab[h].loc_symtab = NULL;
		    hashtab[h].glob_symtab = NULL;
		    hashtab[h].com_loc_symtab = NULL;
		    hashtab[h].com_glob_symtab = NULL;
        }
	return h;
}/*hash_lookup*/

void
init_tables(VOID)			/* Allocates table space */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
	if( ((loc_symtab=(Lsymtab*)calloc(LOCSYMTABSZ,sizeof(Lsymtab)))
		== (Lsymtab*)NULL) ||
	    ((glob_symtab=(Gsymtab*)calloc(GLOBSYMTABSZ,sizeof(Gsymtab)))
		== (Gsymtab*)NULL) ||
	    ((hashtab=(HashTable*)calloc(HASHSZ,sizeof(HashTable)))
		== (HashTable*)NULL)
	  ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for tables");
	}
#endif
}




Gsymtab*
#if HAVE_STDC
install_global(int h, int datatype, int storage_class)	/* Install a global symbol */
	      			/* hash index */
#else /* K&R style */
install_global(h,datatype,storage_class)	/* Install a global symbol */
	int h;			/* hash index */
	int datatype,storage_class;
#endif /* HAVE_STDC */
{
	Gsymtab *gsymt = &glob_symtab[glob_symtab_top];

	if(glob_symtab_top == GLOBSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in global symbol table\n\
Recompile me with larger GLOBSYMTABSZ value\n"
#else
"out of space in global symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
	    clear_symtab_entry(gsymt);

			/* Store symtab pointer in hash table */
	    if(storage_class == class_COMMON_BLOCK) {
		gsymt->mask = hashtab[h].com_glob_symtab;
		hashtab[h].com_glob_symtab = gsymt;
	    }
	    else {
		gsymt->mask = hashtab[h].glob_symtab;
		hashtab[h].glob_symtab = gsymt;
	    }


	 		/* Duplicate copy of string into global stringspace */
	    gsymt->name = new_global_string(hashtab[h].name);

			/* Set symtab info fields */
	    gsymt->valid = TRUE;
	    gsymt->type = type_pack(storage_class,datatype);
	    gsymt->size = type_size[datatype];
	    gsymt->kind = default_kind(datatype);
	    if(storage_class == class_COMMON_BLOCK)
		gsymt->info.comlist = NULL;
	    else
		gsymt->info.arglist = NULL;

	    gsymt->link.child_list = NULL;

	    ++glob_symtab_top;
	}
	return (gsymt);
}/*install_global*/


Lsymtab*
#if HAVE_STDC
install_local(int h, int datatype, int storage_class)	/* Install a local symbol */
	      			/* hash index */
#else /* K&R style */
install_local(h,datatype,storage_class)	/* Install a local symbol */
	int h;			/* hash index */
	int datatype,storage_class;
#endif /* HAVE_STDC */
{
	Lsymtab *symt = &loc_symtab[loc_symtab_top];
	if(loc_symtab_top == LOCSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in local symbol table\n\
Recompile me with larger LOCSYMTABSZ value\n"
#else
"out of space in local symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {

	    clear_symtab_entry(symt);

				/* Save pointer to masked entry if any,
				   then set hashtable to point to this new one.
				 */
	    if(storage_class == class_COMMON_BLOCK) {
		symt->mask = hashtab[h].com_loc_symtab;
		hashtab[h].com_loc_symtab = symt;
	    }
	    else {
		symt->mask = hashtab[h].loc_symtab;
		hashtab[h].loc_symtab = symt;
	    }

	    symt->name = hashtab[h].name;
				/* all items start life as scalars */
	    symt->array_dim = array_dim_info(0,0);

		      /* Set symtab info fields */
	    symt->type = type_pack(storage_class,datatype);
	    symt->size = is_derived_type(datatype)?
	      size_DEFAULT:		/* derived types are default size */
	      type_size[datatype];
	    symt->kind = default_kind(datatype);
	    symt->src.text = NULL;
	    symt->equiv_link = symt;	/* equivalenced only to self */
	    symt->common_block = (Gsymtab*)NULL;
	    symt->common_index = 0;
	    if(current_prog_unit_hash == -1 || /* prog unit, hash not yet set */
	       storage_class == class_MODULE ) /* module's home is module */
	      symt->home_unit = hashtab[h].name;
	    else		/* variables, home is containing prog unit */
	      symt->home_unit = hashtab[current_prog_unit_hash].name;
	    if(incdepth > 0)
	      symt->defined_in_include = TRUE;
	    symt->line_declared = symt->line_set = symt->line_used = NO_LINE_NUM;
				/* initialize indices in incfile table */
	    symt->file_declared = symt->file_set = symt->file_used = -1;
	    symt->file_allocd = -1;
	    ++loc_symtab_top;
	}
	return symt;
}/*install_local*/


		/* Get value specified by an integer-expression token.
		   This will be either an identifier, which should be a
		   parameter whose value is in the symbol table, or else
		   an expression token as propagated by exprtype.c
		   routines, with value stored in the token.
		*/
int
#if HAVE_STDC
int_expr_value(Token *t)
#else /* K&R style */
int_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(!is_true(EVALUATED_EXPR,t->TOK_flags)) {/* something bogus */
				/* warn if error message not already given */
    if(is_true(PARAMETER_EXPR,t->TOK_flags))
      if(misc_warn)
	warning(t->line_num,t->col_num,
	      "Constant not evaluated: value of 0 assumed");
  }
  else {
	if( is_true(ID_EXPR,t->TOK_flags) ) {
		/* Identifier: better be a parameter */
	    int h=t->value.integer;
	    Lsymtab *symt = hashtab[h].loc_symtab;
	    if(symt == NULL || !(symt->parameter) ) {
		syntax_error(t->line_num,t->col_num,
			"symbolic constant required");
	    }
	    else {
		return symt->info.param->value.integer;
	    }
	}
		/* Otherwise, it is a const or expr, use token.value.integer */
	else {
	    return t->value.integer;
	}
  }
				/* Unsuccessful: return value of 0 */
  return 0;
}/*int_expr_value*/

DBLVAL
#if HAVE_STDC
float_expr_value(Token *t)
#else /* K&R style */
float_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(is_true(LIT_CONST,t->TOK_flags))
    return t->value.dbl;
  else
    return (DBLVAL)0;		/* float values are not propagated */
}

char *
#if HAVE_STDC
char_expr_value(Token *t)
#else /* K&R style */
char_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(is_true(LIT_CONST,t->TOK_flags))
    return t->value.string;
  else
    return NULL;		/* char values are not propagated */
}



	/* note_filename():  This routine is called by main prog to give
	   symbol table routines access to current input file name, to be
	   stored in function arg list headers and common list headers, for
	   the use in diagnostic messages. Since filenames are from argv,
	   they are permanent, so pointer is copied, not the string.
	*/
void
#if HAVE_STDC
note_filename(char *s)
#else /* K&R style */
note_filename(s)
	char *s;
#endif /* HAVE_STDC */
{
	current_filename = s;
	top_filename = s;
}/* note_filename */

		/* Routine to output expression tree via msg_tail.  For use
		   in error/warning routines.
		 */
void
msg_expr_tree(const Token *t)
{
    char textbuf[25];
    int ncopied = cp_tree_src_text(textbuf,
				   t->left_token == NULL?t:t->left_token,
				   sizeof(textbuf)-1);
    msg_tail(textbuf);
    if( ncopied == sizeof(textbuf)-1 )
	msg_tail("..");
}

#ifdef DEVELOPMENT		/* Routines to print out expr tree src text */
void
print_src_text(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_tok_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}

void
print_expr_tree(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_tree_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}

void
print_expr_list(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_list_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}
#endif




void
#if HAVE_STDC
ref_array(Token *id, Token *subscrs)   /* Array reference: install in symtab */
#else /* K&R style */
ref_array(id,subscrs)   /* Array reference: install in symtab */
	Token *id, *subscrs;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

				/* Restore subscripts to original order */
	subscrs->next_token = reverse_tokenlist(subscrs->next_token);

	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		       "undeclared variable has dim info:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else{    /* check that subscrs match dimension info */


	  if(arg_count(subscrs->next_token)!=array_dims(symt->array_dim)){
	      syntax_error(subscrs->line_num,subscrs->col_num,
			"array");
	      msg_tail(symt->name);
	      msg_tail("referenced with wrong no. of subscripts");
	  }
	}

}/* ref_array */

void
#if HAVE_STDC
ref_namelist(Token *id, int stmt_class)
#else /* K&R style */
ref_namelist(id,stmt_class)
     Token *id;
     int stmt_class;
#endif /* HAVE_STDC */
{
	Token *t;
	TokenListHeader *toklist;
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
			"undeclared identifier is a namelist:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(h,type_NAMELIST,class_NAMELIST);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	   symt->info.toklist = NULL;
	}

			/* Go thru token list of namelist variables,
			   setting flags appropriately. We can't use
			   use_lvalue or use_variable here, since the
			   line number of token in list is that of the
			   namelist declaration, so we use our own code here.
			*/
	toklist = symt->info.toklist;
	if (toklist != NULL){
	    t = toklist->tokenlist;
	    while(t != NULL){
		/* set flags for all equivalenced vars */

	      int th=t->value.integer;
	      Lsymtab *tsymt,*equiv;
	      if((tsymt=hashtab[th].loc_symtab) == NULL) { /* can't happen */
		  tsymt = install_local(th,type_UNDECL,class_VAR);
		  tsymt->line_declared = id->line_num;
		  tsymt->file_declared = inctable_index;
	      }
	      equiv=tsymt;
	      if(stmt_class == tok_READ) /* code like use_lvalue */
		do{
		  if(! equiv->set_flag) { /* record first line where set */
		      equiv->line_set = id->line_num;
		      equiv->file_set = inctable_index;
		  }
		  equiv->set_flag = TRUE;
		  equiv->assigned_flag = TRUE;
		  equiv = equiv->equiv_link;
		} while(equiv != tsymt);
	      else		/* tok_WRITE: code like use_variable */
		do{
		  if(! equiv->used_flag) { /* record first line where used */
		    equiv->line_used = id->line_num;
		    equiv->file_used = inctable_index;
		  }
		  if(! equiv->set_flag) {
		    equiv->used_before_set = TRUE;
		  }
		  equiv->used_flag = TRUE;
		  equiv = equiv->equiv_link;
		} while(equiv != tsymt);
	      t = t->next_token;
	    }
	}
}

void
#if HAVE_STDC
ref_identifier(Token *id)	/* Identifier reference: install in symtab */
#else /* K&R style */
ref_identifier(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt = hashtab[h].loc_symtab;
	if( symt == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

}/*ref_identifier*/

void
#if HAVE_STDC
ref_variable(Token *id)	/* Variable reference: install in symtab */
#else /* K&R style */
ref_variable(id)	/* Variable reference: install in symtab */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt = hashtab[h].loc_symtab;
	if( symt == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	if(symt->line_declared == NO_LINE_NUM) {
	    symt->line_declared = id->line_num; /* implicit declaration */
	    symt->file_declared = inctable_index;
	}
	/* transfer pointer/target attributes to token */
      if(symt->pointer) {
	make_true(POINTER_EXPR,id->TOK_flags);
      }
      if(symt->target)
	make_true(TARGET_EXPR,id->TOK_flags);

}/*ref_variable*/


void
#if HAVE_STDC
save_com_block(Token *id)	/* Process SAVEing of a common block */
	          	/* N.B. Legality checking deferred to END */
#else /* K&R style */
save_com_block(id)	/* Process SAVEing of a common block */
	Token *id;	/* N.B. Legality checking deferred to END */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

			/* N.B. SAVE does not create a global table entry */
	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
				/* record location in case never declared */
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else
	  symt->saved = TRUE;
}


	/* Following routine sets the implicit typing of characters in
	   range c1 to c2 to the given type. */
void
#if HAVE_STDC
set_implicit_type(int type, long int size, char *len_text, int c1, int c2)
	         		/* Data type of IMPLICIT declaration */
                  		/* Type size or size_DEFAULT if not given */
	               		/* Source text of length spec */
	       			/* First character of range */
	       			/* Last character of range */
#else /* K&R style */
set_implicit_type(type,size,len_text,c1,c2)
	int type;		/* Data type of IMPLICIT declaration */
        long size;		/* Type size or size_DEFAULT if not given */
	char *len_text;		/* Source text of length spec */
	int c1;			/* First character of range */
	int c2;			/* Last character of range */
#endif /* HAVE_STDC */
{
	int c;

	if(c1 == '$')  c1 = 'Z'+1;
	if(c2 == '$')  c2 = 'Z'+1;

	if(c1 == '_')  c1 = 'Z'+2;
	if(c2 == '_')  c2 = 'Z'+2;

	if(c2 < c1) {
		yyerror("IMPLICIT range must be in alphabetical order");
	}
	else {
	  implicit_info.implicit_none = FALSE;
		/* Fill in the lookup table for the given range of chars */
	  for(c=c1; c<=c2; c++) {
		implicit_info.type[c-'A'] = type;
		implicit_info.size[c-'A'] = size;
		implicit_info.len_text[c-'A'] = len_text;
	  }
		/*
	  for(c=c1; c<=c2; c++) {
		implicit_type[c-'A'] = type;
		implicit_size[c-'A'] = size;
		implicit_len_text[c-'A'] = len_text;
	  }
	  */
	}
}/*set_implicit_type*/

void set_implicit_none()
{
  implicit_info.implicit_none = TRUE;
}

		/* Finish processing statement function.
		   Clears all used-before-set flags of ordinary
		   variables. Reason: statement functions are processed
		   like assignment to an array element, setting ubs flags.
		   At this point, no valid setting of ubs flags should
		   be possible, so clearing them will elim false messages.*/
void
#if HAVE_STDC
stmt_function_stmt(Token *id)			/* ARGSUSED0 */
               			/* Not used at present */
#else /* K&R style */
stmt_function_stmt(id)			/* ARGSUSED0 */
     Token *id;			/* Not used at present */
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; i<loc_symtab_top; i++) {
	if(storage_class_of(loc_symtab[i].type) == class_VAR &&
	   ! loc_symtab[i].parameter )
	  loc_symtab[i].used_before_set = FALSE;
    }
}/*stmt_function_stmt(id)*/

char *
#if HAVE_STDC
token_name(Token *t)
#else /* K&R style */
token_name(t)
	Token *t;
#endif /* HAVE_STDC */
{
	return hashtab[t->value.integer].name;
}/*token_name*/


void
undef_do_variable( int h )	/* Make DO index variable inactive */
{
    Lsymtab *symt=hashtab[h].loc_symtab;
    if( symt != NULL )	/* Just in case: it should always be defined */
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->active_do_var = FALSE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}


void
#if HAVE_STDC
use_actual_arg(Token *id)	/* like use_lvalue except does not set assigned_flag */
#else /* K&R style */
use_actual_arg(id)	/* like use_lvalue except does not set assigned_flag */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}
	else {
			/* If an external other than an intrinsic, set up
			   tokenlist for "call".  If intrinsic, check
			   legality of this usage.) */
	  if(storage_class_of(symt->type) == class_SUBPROGRAM) {
	    if(symt->intrinsic) {
	      IntrinsInfo *defn = symt->info.intrins_info;
	      if( !(symt->declared_intrinsic) ) {
		if(misc_warn) {
		  warning(id->line_num,id->col_num,
				defn->name);
		  msg_tail("not declared INTRINSIC");
		}
	      }
	      if( (defn->intrins_flags&I_NOTARG) ) {
		syntax_error(id->line_num,id->col_num,
				defn->name);
		msg_tail("intrinsic function cannot be a subprogram argument");
	      }
	    }
	    else {		/* External subprogram as actual arg */
	      TokenListHeader *TH_ptr;
	      TH_ptr= make_TL_head(id);

	      TH_ptr->actual_arg = TRUE;
	      TH_ptr->next = symt->info.toklist;
	      symt->info.toklist = TH_ptr;
	    }
	  }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_actual_arg*/


PRIVATE void
#if HAVE_STDC
use_function_arg(Token *id)	/* Like use_variable but invokes use_actual_arg
			   if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
#else /* K&R style */
use_function_arg(id)	/* Like use_variable but first invokes use_actual_arg
			   only if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

	if(storage_class_of(symt->type) == class_SUBPROGRAM)
	  use_actual_arg(id);

	use_variable(id);

}/*use_function_arg*/

void
#if HAVE_STDC
use_implied_do_index(Token *id)
#else /* K&R style */
use_implied_do_index(id)
	Token *id;
#endif /* HAVE_STDC */
{
		/* Like use_lvalue and use_variable but clears ubs flag.
	           This is because we cannot handle used-before-set
		   properly in this case, and the odds are that ubs
		   was set in the preceding I/O list. */
	int h=id->value.integer;
	Lsymtab *symt;

	use_lvalue(id);
	use_variable(id);
	symt=hashtab[h].loc_symtab;

	symt->used_before_set = FALSE;
}/*use_implied_do_index*/


PRIVATE void
#if HAVE_STDC
use_inq_arg(Token *id)	/* intrinsic inquiry function eg LEN, ALLOCATED */
#else /* K&R style */
use_inq_arg(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
/* do not set any usage flags since inquiry about a variable
   does not constitute usage. */

}/*use_inq_arg*/

void
#if HAVE_STDC
use_lvalue(Token *id)	/* handles scalar lvalue */
#else /* K&R style */
use_lvalue(id)	/* handles scalar lvalue */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;
	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}
	else {
	  /*   check match to previous invocations and update  */
	}

			/* F77 standard section 11.10.5 prohibits modifying
			   DO variable except thru loop mechanism.
			 */
	if(symt->active_do_var) {
	  if(usage_do_var_modified) {
	      syntax_error(id->line_num,id->col_num, "active");
	      msg_tail(symt->forall_var ? "FORALL" : "DO");
	      msg_tail("index is modified");
	  }
	}
	/* else, if inside a FORALL we should warn that forall index
	   isn't being used.
	 */

			/* check for intent.  Ok to assign to target
			   of intent(in) pointer.
			 */
	if (symt->intent_in && !symt->pointer && !symt->intent_out) {
	  syntax_error(id->line_num,id->col_num,
	    "argument with intent IN must not be set:");
	  msg_tail(symt->name);
	}
                        /* handle allocatable variable here */
        if (symt->allocatable || symt->pointer){
	   if (!symt->allocated_flag){
		symt->line_allocd = id->line_num;
	        symt->file_allocd = inctable_index;
		symt->used_before_allocation = TRUE;
	   }
	}
                        /* handle pointer variable here */
        if (symt->pointer){
	   if (!symt->associated_flag){
		symt->line_assocd = id->line_num;
	        symt->file_assocd = inctable_index;
		symt->used_before_associated = TRUE;
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_lvalue*/



void                    /* Process data_constant_value & data_repeat_factor */
#if HAVE_STDC
use_parameter(Token *id)
#else /* K&R style */
use_parameter(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	if(! symt->parameter) {
		syntax_error(id->line_num,id->col_num,
			"must be a parameter");
/***		symt->parameter = TRUE;**/  /*oops: must define info etc.*/
	}

	if(! symt->set_flag) {
	   symt->used_before_set = TRUE;
	}

	if(! symt->used_flag) { /* record first line where used */
	    symt->line_used = id->line_num;
	    symt->file_used = inctable_index;
	}

	symt->used_flag = TRUE;

}/*use_parameter*/


void
#if HAVE_STDC
use_variable(Token *id)		/* Set the use-flag of variable. */
#else /* K&R style */
use_variable(id)		/* Set the use-flag of variable. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

                 /*** handle pointer/allocatable variables ***/ 
    if (symt->pointer)
    {
      make_true(POINTER_EXPR,id->TOK_flags);
      use_pointer(id);
    }
                        /* handle allocatable variable here */
        if (symt->allocatable || symt->pointer){
	   if (!symt->allocated_flag){
		symt->line_allocd = id->line_num;
	        symt->file_allocd = inctable_index;
		symt->used_before_allocation = TRUE;
	   }
	}
                        /* handle pointer variable here */
        if (symt->pointer){
	   if (!symt->associated_flag){
		symt->line_assocd = id->line_num;
	        symt->file_assocd = inctable_index;
		symt->used_before_associated = TRUE;
	   }
	}


    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	if(! equiv->set_flag) {
	   equiv->used_before_set = TRUE;
	}
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_variable*/

/*** Handle case when pointer variable is used.  This is called
     from use_variable, which updates used, used-before-set.  Here we
     only update used-before-assoc/alloc.
 ***/
void
#if HAVE_STDC
use_pointer(Token *id) 
#else /* K&R style */
use_pointer(id)        
        Token *id;
#endif /* HAVE_STDC */
{
		/* Is this a variable? (could be pointer function ref) */
	if( is_true(ID_EXPR,id->TOK_flags) ) {
	  int h=id->value.integer;
	  Lsymtab *symt;

	  symt=hashtab[h].loc_symtab;

	  if(!(symt->associated_flag || symt->allocated_flag)) {
	    symt->used_before_associated = TRUE;
	    symt->used_before_allocation = TRUE;
	  }

	}

}/*use_pointer*/


/* Handle pointer on left side of pointer assignment, p => q
   where q may be a target or a pointer.
 */
void
use_pointer_lvalue(Token *id, Token *rhs)
{

  int h=id->value.integer;
  Lsymtab *symt;
  if((symt=hashtab[h].loc_symtab) == NULL) {
    symt = install_local(h,type_UNDECL,class_VAR);
    symt->line_declared = id->line_num;
    symt->file_declared = inctable_index;
  }

  if (symt->intent_in && !symt->intent_out) {
    syntax_error(id->line_num,id->col_num,
		 "argument with intent IN must not be set:");
    msg_tail(symt->name);
  }

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

	
      /* propagate pointer association status to lvalue */
  if(  (is_true(POINTER_EXPR,rhs->TOK_flags) || is_true(TARGET_EXPR,rhs->TOK_flags)) ) {
    symt->associated_flag = is_true(ASSOCIATED_EXPR,rhs->TOK_flags)&&1;
    symt->allocated_flag =  is_true(ALLOCATED_EXPR,rhs->TOK_flags)&&1;
    
					/* record 1st place assoc'd */
    if( symt->associated_flag && symt->line_assocd == NO_LINE_NUM ) {
      symt->line_assocd = id->line_num;
    }
					/* record 1st place alloc'd */
    if( symt->allocated_flag && symt->line_allocd == NO_LINE_NUM ) {
      symt->line_allocd = id->line_num;
    }
  }


}/*use_pointer_lvalue*/

/* Process TARGET or POINTER rhs variable in pointer assignment statements.
   This does not imply "usage" i.e. accessing value of the variable.
   Just record the fact that it was assigned as a pointee, so that
   used-before-set warnings can be suppressed.
 */
void use_target(Token *id)
{
    int h = id->value.integer;
    Lsymtab *symt;

    if( (symt = hashtab[h].loc_symtab) == NULL ) {
	symt = install_local(h,type_UNDECL,class_VAR);
	symt->line_declared = id->line_num;
	symt->file_declared = inctable_index;
    }

    symt->assigned_as_target = TRUE;

}

void
#if HAVE_STDC
do_allocate(Token *id)		/* Process ALLOCATE statement */
#else /* K&R style */
do_allocate(id)		/* Process ALLOCATE statement */
	Token *id;
#endif /* HAVE_STDC */
{
	int h;
        Token *next_id = id;
	Lsymtab *symt;

  while (next_id != NULL){
	   if( !is_true(ID_EXPR, next_id->TOK_flags) ||
	       !( is_true(POINTER_EXPR, next_id->TOK_flags) ||
		  is_true(ALLOCATABLE_EXPR, next_id->TOK_flags) ) ) {
	      syntax_error(next_id->line_num,next_id->col_num,
		"Variable must be an allocatable/pointer array: ");
	      msg_expr_tree(next_id);
	   }
           else{
	        h = next_id->value.integer;
	        symt=hashtab[h].loc_symtab;

                if (!symt->allocated_flag){
	           symt->allocated_flag = TRUE;
				/* record first line where set */
		   symt->line_allocd = next_id->line_num;
		   symt->file_allocd = inctable_index;
                }
                else{
                   warning(next_id->line_num,next_id->col_num,
                     "Reallocating an allocated variable: ");
		   msg_expr_tree(next_id);
                }

                if (!symt->associated_flag){
	           symt->associated_flag = TRUE;
				/* record first line where set */
		   symt->line_assocd = next_id->line_num;
		   symt->file_assocd = inctable_index;
                }
                else{
                   warning(next_id->line_num,next_id->col_num,
                     "Reassociating an associated pointer: ");
		   msg_expr_tree(next_id);
                }
	   }
  next_id = next_id->next_token;
  }
}/*do_allocate*/

void
#if HAVE_STDC
do_deallocate(Token *id)		/* Process ALLOCATE statement */
#else /* K&R style */
do_deallocate(id)		/* Process ALLOCATE statement */
	Token *id;
#endif /* HAVE_STDC */
{
	int h;
        Token *next_id = id;
	Lsymtab *symt;

  while (next_id != NULL){
	   if( !is_true(ID_EXPR, next_id->TOK_flags) ||
	       !( is_true(POINTER_EXPR, next_id->TOK_flags) ||
		  is_true(ALLOCATABLE_EXPR, next_id->TOK_flags) ) ) {
	      syntax_error(next_id->line_num,next_id->col_num,
		"Variable must be an allocatable/pointer array: ");
	      msg_expr_tree(next_id);
	   }
           else{
	        h = next_id->value.integer;
	        symt=hashtab[h].loc_symtab;

                if (symt->allocated_flag){
	           symt->allocated_flag = FALSE;
                }
                else{
                   warning(next_id->line_num,next_id->col_num,
                     "Deallocating a possibly unallocated variable: ");
		   msg_expr_tree(next_id);
                }

		symt->associated_flag = FALSE;
	   }
  next_id = next_id->next_token;
  }
}/*do_deallocate*/

void
#if HAVE_STDC
do_nullify(Token *id)		/* Process NULLIFY statement */
#else /* K&R style */
do_nullify(id)		/* Process NULLIFY statement */
	Token *id;
#endif /* HAVE_STDC */
{
	int h;
	Lsymtab *symt;
        Token *next_id = id->next_token; /* skip first duplicate token */

  while (next_id != NULL){
	   if( !is_true(ID_EXPR, next_id->TOK_flags) ||
	       !is_true(POINTER_EXPR, next_id->TOK_flags) ) {
	      syntax_error(next_id->line_num,next_id->col_num,
		"Variable must be a pointer: ");
	      msg_expr_tree(next_id);
	   }
           else{
	       h = next_id->value.integer;
	       symt=hashtab[h].loc_symtab;

	       if (symt->allocated_flag){
		   warning(next_id->line_num,next_id->col_num,
			   "Disassociating an allocated pointer: ");
		   msg_expr_tree(next_id);
	       }
	       symt->associated_flag = FALSE;
	   }
  next_id = next_id->next_token;
  }
}/*do_nullify*/


/* Routines to calculate internal kind parameters for
   selected_int_kind and selected_real_kind.  We use a scheme that
   ensures no internal kind parameters match explicit numeric kind
   parameters while allowing independent kind parameter declarations
   using SELECTED_KIND intrinsics to match.  To keep int and real
   kinds distinct, int kinds are odd and real kinds even.
   For range == 0 and precision == 0 return kind_DEFAULT.
 */

/* Set default kind for given type.  For complex types, kind is the
   kind of the component reals.  For others, kind is negative of
   type number.  Since quad has no type of its own, its default kind
   is defined by a special function.
*/
kind_t
default_kind(int type)
{
  kind_t kind;

  switch(type)
    {
    case type_COMPLEX:
      kind = kind_DEFAULT_REAL;
      break;
    case type_DCOMPLEX:
      kind = kind_DEFAULT_DP;
      break;
    case type_INTEGER:
    case type_REAL:
    case type_DP:
    case type_LOGICAL:
    case type_STRING:
      kind = -type;
      break;
    case type_UNDECL:
      kind = kind_DEFAULT_UNKNOWN;
      break;
    default:			/* bogus: non-kindable type */
      kind = 0;
      break;
    }
  return kind;
}

/* -k = (R+1)*R_factor + (P+1)*P_factor + int?1:0, for P+R > 0.
   For uniqueness require P+1 < R_factor/P_factor.
   IEEE 754 quad precision has precision 34 digits and range 4932.  Our
   limits on P and R must allow these, but we peg very large values to
   prevent overflow.
 */
#define R_factor (10000)
#define P_factor (10)		/* 2*P_factor must exceed max abs default kind no. */
#define MAX_P ((R_factor/P_factor)-2) /* so (P+1)*P_factor < R_factor */
#define MAX_R (9999)      /* limit to ensure max kind does not overflow */


/* F2010 specifies a minimum range of 5 digits for default integer
 * type.  It recommends a minimum range of 37 and a precision of at
 * least 6 digits for default real kind.  If user specifies range
 * and/or precision within these values, yield default kind.
 * Otherwise encode in internal form.
 */
#define DEFAULT_INT_R (5)
#define DEFAULT_REAL_R (37)
#define DEFAULT_REAL_P (6)

kind_t
selected_int_kind( int range ) 
{
  kind_t kind;

  /* enforce sane values */
  if( range < 0 ) range = 0;
  if( range > MAX_R ) range = MAX_R;

  if( range <= DEFAULT_INT_R )
    kind = kind_DEFAULT_INTEGER;
  else
    kind = -((range+1)*R_factor+1);

  return kind;
  
}


kind_t
selected_real_kind_r( int range ) 
{
  kind_t kind;

  /* enforce sane values */
  if( range < 0 ) range = 0;
  if( range > MAX_R ) range = MAX_R;

  if( range <= DEFAULT_REAL_R )
    kind = kind_DEFAULT_REAL;
  else
    kind = -((range+1)*R_factor);

  return kind;
  
}

kind_t
selected_real_kind_p( int precision )
{
  kind_t kind;

  /* enforce sane values */
  if( precision < 0 ) precision = 0;
  if( precision > MAX_P ) precision = MAX_P;

  if( precision <= DEFAULT_REAL_P )
    kind = kind_DEFAULT_REAL;
  else
    kind = -((precision+1)*P_factor);

  return kind;
}

kind_t
selected_real_kind_p_r( int precision, int range ) /* SELECTED_REAL_KIND(P,R) */
{
  kind_t kind;

  /* enforce sane values */
  if( range < 0 ) range = 0;
  if( range > MAX_R ) range = MAX_R;
  if( precision < 0 ) precision = 0;
  if( precision > MAX_P ) precision = MAX_P;

  if( range <= DEFAULT_REAL_R  && precision <= DEFAULT_REAL_P )
    kind = kind_DEFAULT_REAL;
  else
    kind = -((range+1)*R_factor + (precision+1)*P_factor);

  return kind;
}

/* Return data type of the given kind parameter. */
int kind_type( kind_t kind )
{
  if( kind >= 0 ) {		/* user-defined value */
    return type_UNDECL;			/* unknown type */
  }
  else {

    if( kind == kind_DEFAULT_QUAD )	/* special case */
      return type_REAL;			/* QUAD is just real type */

    kind = -kind;		/* get absolute value */
    if( kind < P_factor ) {	/* a default kind */
      return kind;		/* default kind = -type */
    }
    else {			/* a selected int or real kind */
      if( kind%2 == 1 )		/* integer kinds are odd */
	return type_INTEGER;
      else
	return type_REAL;
    }
  }
}

/* Return range of the given kind parameter.  If range was not
   specified, value of -1 is returned. */
int
kind_range(kind_t kind)
{
  if( kind >= 0 || kind == kind_DEFAULT_QUAD ) {/* user-defined value or quad */
    return -1;
  }
  else {
    kind = -kind;		/* get absolute value */
    if( kind < P_factor ) {	/* a default kind */
      return 0;
    }
    else {
      return (kind/R_factor)-1;	/* decode range */
    }
  }
}

/* Return precision of the given kind parameter.  If precision was not
   specified, value of -1 is returned. */
int
kind_precision(kind_t kind)
{
  if( kind >= 0 || kind == kind_DEFAULT_QUAD ) {/* user-defined value or quad */
    return -1;
  }
  else {
    kind = -kind;		/* get absolute value */
    if( kind < P_factor ) {	/* a default kind */
      return 0;
    }
    else {
      return (kind%R_factor)/P_factor-1; /* decode precision */
    }
  }
}

/* Return TRUE if kind is a default kind, FALSE if not.
   All default kinds -1 down to kind_DEFAULT_UNKNOWN.
 */
int
kind_is_default(kind_t kind)
{
  return kind < 0 && kind >= kind_DEFAULT_UNKNOWN;
}

#undef R_factor
#undef P_factor
#undef MAX_P
#undef MAX_R
#undef DEFAULT_INT_R
#undef DEFAULT_REAL_R
#undef DEFAULT_REAL_P

	/* Routine to provide a string with type followed by one of: "",
	   "*n" where n is the declared size of an item, "(l)" where
	   l is the declared array length of an item, or "*n(l)".
	   Note: cannot be used twice in same statement, since
	   it uses a static buffer for the result.
	*/
char *
#if HAVE_STDC
typespec(int t, int has_size, long size, int has_len, long len)
#else /* K&R style */
typespec(t,has_size, size, has_len, len)
    int t;			/* data type code */
    int  has_size,		/* whether it has *size spec */
	 has_len;		/* whether it has (len) spec */
    long size,			/* value of size */
	 len;			/* value of len */
#endif /* HAVE_STDC */
{
			/* Size of buffer allows 3 digits for each byte,
			   which is slightly more than necessary.
			 */
    static char buf[MAX_TYPESPEC];
    strncpy(buf,type_name(t),4); buf[4] = '\0';
    if(has_size) {
	(void) sprintf(buf+strlen(buf),"*%ld",size);
    }
    if(has_len) {
	(void) sprintf(buf+strlen(buf),"(%ld)",len);
    }
    
    return buf;
}

	/* Used by global routines to report derived type names as they
	 * were originally defined as of reporting the renamed alias
	 */
char *
global_typespec(int t, int has_size, long size, int has_len, long len)
{
			/* Size of buffer allows 3 digits for each byte,
			   which is slightly more than necessary.
			 */
    static char buf[MAX_TYPESPEC];
    strncpy(buf,global_type_name(t),4); buf[4] = '\0';
    if(has_size) {
	(void) sprintf(buf+strlen(buf),"*%ld",size);
    }
    if(has_len) {
	(void) sprintf(buf+strlen(buf),"(%ld)",len);
    }
    
    return buf;
}

/* Routine to print out kind qualifier that goes before type spec in warnings. */
void
report_kind(kind_t k)
{
  if( k >= 0 ) {			/* concrete kind */
    msg_tail("kind=");
    msg_tail(ulongtostr((unsigned long)k));
  }
  else {				/* default or selected kind */
    if( kind_is_default(k) ) {
      msg_tail("default");
      if( k == kind_DEFAULT_QUAD ) msg_tail("quad");
    }
    else
      msg_tail("selected");
  }
}

/* Index variables in subscript lists of FORALL constructs have to be
 * defined as INTEGER and already set.  They piggyback on the
 * active_do_var mechanism to catch illicit modification within the
 * loop.  They also have their own flag forall_var so error messages
 * can say the right thing.
 */
void def_forall_index(Token *t)
{
    int h=t->value.integer;
    Lsymtab *symt = hashtab[h].loc_symtab;

    if( symt == NULL || !in_curr_scope(symt) ) {
	symt = install_local(h,type_INTEGER,class_VAR);

	/* flags to initiate and enable messages */
	symt->set_flag = TRUE; 
	symt->active_do_var = TRUE;
	symt->forall_var = TRUE;
    }
    else {
	syntax_error(t->line_num,t->col_num,"Index Variable already used");
    }
}

/* The local scope of FORALL construct is scanned for any scalars that are
 * not FORALL index variables and moved out of the scope.
 */
void process_forall_construct(Token *t)
{
    int i;

    for (i = curr_scope_bottom; i < loc_symtab_top; i++) {
	/* Upon encountering a variable declared (implicitly) in the
	 * FORALL scope that is not a FORALL index variable, move it
	 * to enclosing scope.  Such variables will normally be
	 * erroneous, but warnings about them will be given elsewhere,
	 * e.g. used before set.
	 */
	if (!loc_symtab[i].forall_var){
	    move_outside_scope(&loc_symtab[i]);
	}
	else {
	    if( !loc_symtab[i].used_flag ) {
		warning(t->line_num,NO_COL_NUM,
			"FORALL index variable");
		msg_tail(loc_symtab[i].name);
		msg_tail("not used in construct");
	    }
	}
    }
}


/*  End of symtab.c */

/*

 II. Hash

*/

/*    hash.c:
 	performs a hash function

This was formerly a separate file.

*/

extern int sixclash;	/* flag to check clashes in 1st 6 chars of name */

unsigned long
#if HAVE_STDC
hash(const char *s)
#else /* K&R style */
hash(s)
    char *s;
#endif /* HAVE_STDC */
{
    unsigned long sum = 0, wd;
    unsigned j;

    if(sixclash) {		/* special hashing for six-char limit */
      unsigned i = 0;
      while (i < 6 && s[i] != '\0') {
         wd = 0;
         for(j=1; j <= sizeof(long) && i < 6 && s[i] != '\0'; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (sizeof(long) - j) * 8;}

	sum ^= wd;}
    }
    else {			/* the usual case */
      while( *s != '\0' ) {
         wd = 0;
         for(j=1; j <= sizeof(long) && *s != '\0'; j++) {
            wd += (unsigned long)(*s++ & 0xff) << (sizeof(long) - j) * 8;}

	sum ^= wd;}
    }
    return sum;
}



/*    rehash
        performs a rehash for resolving clashes.
*/

#ifdef COUNT_REHASHES
unsigned long rehash_count=0;
#endif

unsigned long
#if HAVE_STDC
rehash(unsigned long hnum)
#else /* K&R style */
rehash(hnum)
    unsigned long hnum;
#endif /* HAVE_STDC */
{
#ifdef COUNT_REHASHES
    rehash_count++;
#endif
    return hnum+1;
}


/*  End of hash */





#ifdef DEBUG_SIZES
void print_sizeofs()			/* For development: print sizeof for
				   various data structures */
{
#ifdef __STDC__
#define PrintObjSize(OBJ) (void)fprintf(list_fd,#OBJ " size = %ld\n",(long)sizeof(OBJ))
#else			/* K&R form */
#define PrintObjSize(OBJ) (void)fprintf(list_fd,"OBJ size = %ld\n",sizeof(OBJ))
#endif
  PrintObjSize(char *);
  PrintObjSize(Token);
  PrintObjSize(Lsymtab);
  PrintObjSize(Gsymtab);
  PrintObjSize(HashTable);
  PrintObjSize(ArgListHeader);
  PrintObjSize(ArgListElement);
  PrintObjSize(ComListHeader);
  PrintObjSize(ComListElement);
  PrintObjSize(TokenListHeader);
  PrintObjSize(InfoUnion);
  PrintObjSize(IntrinsInfo);
  PrintObjSize(ParamInfo);
  PrintObjSize(ChildList);
}
#endif



int empty_scope() {
  return (curr_scope_bottom == loc_symtab_top);
}

void push_loc_scope(void)
{

#ifdef DEBUG_SCOPE
if (debug_latest) {
    fprintf(list_fd, "\npush: loc_scope_top=%d", loc_scope_top);
    fprintf(list_fd, "\npush: curr_scope_bottom=%d", curr_scope_bottom);
    fprintf(list_fd, "\npush: curr_scope_top   =%d", loc_symtab_top);
}
#endif

    if (loc_scope_top >= MAXSCOPES) {
      oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		   "Local scope stack overflow");
    }
    else {
#ifdef DEBUG_SCOPE
if (debug_latest) {int hashno = current_prog_unit_hash;
    fprintf(list_fd, "\nsaving current_prog_unit_hash of %s",
	    ((hashno == -1)?"-1":hashtab[hashno].name));
}
#endif
      loc_scope[loc_scope_top].hash_num = current_prog_unit_hash;
      loc_scope[loc_scope_top].exec_stmt_count = exec_stmt_count; 
      loc_scope[loc_scope_top].implicit = implicit_info;
      loc_scope[loc_scope_top++].symt_index = curr_scope_bottom;
      curr_scope_bottom = loc_symtab_top;
    }
}

/* Pop the symbols defined in the current scoping unit off the symbol
  table, so the enclosing unit becomes the current scope.  The top of
  the symbol table is set back to the bottom of the current unit, and
  the bottom of the scoping unit is restored to that of the enclosing
  scope, saved on scope stack.
 */
int pop_loc_scope(void)
{
#ifdef DEBUG_SCOPE
if (debug_latest) {
    fprintf(list_fd, "\npop: loc_scope_top=%d", loc_scope_top);
    fprintf(list_fd, "\npop: curr_scope_bottom=%d", curr_scope_bottom);
    fprintf(list_fd, "\npop: curr_scope_top   =%d", loc_symtab_top);
}
#endif
    if (loc_scope_top <= 0) {
      oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		   "Tried to pop empty local scope stack");
      return -1;
    }
    else  {
      int i, h;
		      /* Clear the hash table of local symbol refs */
      for(i=curr_scope_bottom ; i<loc_symtab_top; i++) {
	h=hash_lookup(loc_symtab[i].name);
	/* point hashtable at masked entry in outer scope if any */
	if(hashtab[h].loc_symtab)
	  hashtab[h].loc_symtab = hashtab[h].loc_symtab->mask;
	if(hashtab[h].com_loc_symtab)
	  hashtab[h].com_loc_symtab = hashtab[h].com_loc_symtab->mask;
      }


      loc_symtab_top = curr_scope_bottom;
      curr_scope_bottom = loc_scope[--loc_scope_top].symt_index;

#ifdef DEBUG_SCOPE
if (debug_latest) {
    int h = loc_scope[loc_scope_top].hash_num;
    fprintf(list_fd, "\nrestoring current_prog_unit_hash of %s",
	    ((h == -1)?"-1":hashtab[h].name));
}
#endif
	/* Restore counters for enclosing scope */
      exec_stmt_count = loc_scope[loc_scope_top].exec_stmt_count;
      implicit_info = loc_scope[loc_scope_top].implicit;

    /* Return value of current_prog_unit_hash of enclosing scope. */
      return loc_scope[loc_scope_top].hash_num;
    }
}

/* Bubbles the given entry in the local symbol table to bottom of
   current scope, and adjusts bottom so the entry is now in the
   enclosing scope.  This is used when an entry gets created inside a
   scope but belongs in the enclosing scope.  So far this happens for these
   cases: (1) a statement implying pushing of local scope (e.g. a TYPE
   statement) occurs as first statement of a program, i.e. an implied
   PROGRAM statement, which gets put into the local symbol table after
   the said statement has been processed.  (2) a forward reference to
   a derived type occurs inside a derived type definition, which will
   eventually become a defined derived type but initially gets put
   into the scope of the TYPE definition.  (3) an erroneous reference
   to an undeclared variable or function inside a TYPE definition
   (e.g. as an initializer or an array dimension), which becomes a
   local symtab entry that would be taken as one of the components.

   This routine must not be called if any of swapped entries
   is a common block.  This cannot occur if it is used only in the
   above cases, so no check of this restriction is done.
 */
void move_outside_scope(Lsymtab *symt)
{
  if( curr_scope_bottom < 0 ) {	/* just in case */
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		 "symtab_top_swap called when it shouldn't be");
  }

  else {
  /* bubble top symtab entry to bottom */
    Lsymtab temp;		/* holder for swap */
    int i,top;
    top = symt - loc_symtab;	/* index of entry to move out of scope */
    temp = loc_symtab[top];
    for(i=top; i>curr_scope_bottom; i--) {
      loc_symtab[i] = loc_symtab[i-1];
    }
    loc_symtab[curr_scope_bottom] = temp;

  /* Find the hash table entries of the symtab entries
     and change their symtab pointers to the new values.
   */
    for(i=curr_scope_bottom; i<=top; i++) {
      int h;
      h = hash_lookup(loc_symtab[i].name);
      hashtab[h].loc_symtab = &loc_symtab[i];
    }

    /* Fix the curr_scope_bottom to correct value,
     to account for moving the entry from local to enclosing scope. */
    curr_scope_bottom++;

  }
}


/* Function returns true if symbol table pointer points to entry in
   the current scoping unit, between curr_scope_bottom and loc_symtab_top.
 */
int in_curr_scope(const Lsymtab *entry)
{
    if (entry >= &loc_symtab[curr_scope_bottom] &&
            entry < &loc_symtab[loc_symtab_top])
        return TRUE;
    else
        return FALSE;
}

/* Function returns true if symbol table pointer points to entry in
   the current scoping unit or enclosing scoping unit, between
   previous scope's bottom and loc_symtab_top.
 */
int in_enclosing_scope(const Lsymtab *entry)
{
    int prev_scope_bottom;

    if (loc_scope_top >= 2)
        prev_scope_bottom = loc_scope[loc_scope_top-1].symt_index;
    else
	prev_scope_bottom = 0;  /* would be -1 instead */

    if (entry >= &loc_symtab[prev_scope_bottom] &&
            entry < &loc_symtab[loc_symtab_top])
        return TRUE;
    else
        return FALSE;
}

/* Function returns -1 if entry is in current scope else returns index of
 * loc_scope entry for scope of entry
 */
int find_scope(const Lsymtab *entry)
{
	int i;
	if (in_curr_scope(entry)) return -1;

	for (i = loc_scope_top - 1; i >= 1; i--) {
		if (entry >= &loc_symtab[loc_scope[i].symt_index])
			return i;
	}
	return 0;		/* should not happen */

}

/* Function to clear the valid flags and hashtable pointers of
   internal or module subprograms, called after exiting the scope in
   which they are valid.  The entries in the global symbol table are
   not deleted, to avoid the need to update pointers in various data
   structures that point to the remaining valid entries.
 */
void clean_globals(int hashno, SUBPROG_TYPE limit)
{
	int i, curr_i;

#ifdef DEBUG_GLOBAL
if (debug_latest) {
	fprintf(list_fd,"\nCleaning global entries of %s subprograms",
	  limit == internal_subprog?"internal":"module");
	fprintf(list_fd,"\n:: Global entries are ::");

	for (i = 0; i < glob_symtab_top; i++) {

	if (glob_symtab[i].valid) {
		fprintf(list_fd,"\n %s ", glob_symtab[i].name);
		if(glob_symtab[i].internal_subprog) fprintf(list_fd,"internal subprog");
		else if(glob_symtab[i].module_subprog) fprintf(list_fd,"module subprog");
		else fprintf(list_fd,"other");
		}
	}
}
#endif

/* start the scan from index of current program unit, whose hashtable
   index is in hashno
 */
	curr_i = hashtab[hashno].glob_symtab - &glob_symtab[0];
	for (i = curr_i; i < glob_symtab_top; i++) {
	  if(glob_symtab[i].valid) {
	    if( (limit == internal_subprog && glob_symtab[i].internal_subprog) ||
		(limit == module_subprog && glob_symtab[i].module_subprog) ||
		(limit == from_module && glob_symtab[i].from_module) ) {
	      int h;
#ifdef DEBUG_GLOBAL
if (debug_latest) {
	fprintf(list_fd,"\nInvalidated subprog %s ", glob_symtab[i].name);
}
#endif
			glob_symtab[i].valid = FALSE;

			/* clear the hashtable entry too */
			h = hash_lookup(glob_symtab[i].name);
			if(datatype_of(glob_symtab[i].type) == type_COMMON_BLOCK)
			  hashtab[h].com_glob_symtab = glob_symtab[i].mask;
			else
			  hashtab[h].glob_symtab = glob_symtab[i].mask;
		}
	  }
	}
}

/* Routine to create a new local symbol table entry for a subprogram
 * invocation in an internal subprogram.  Because process_lists works
 * by scanning the local symbol table within the current scope, if a
 * new entry is not made, then these subprogram invocations will not
 * be found till the enclosing program unit's scope is scanned.  That
 * is too late, since argument lists need to be analyzed in the scope
 * of the internal subprogram.  Therefore we create a new, masking
 * local symbol table entry which inherits the info already set by the
 * enclosing program unit, except for info.toklist and mask fields.
 *
 * This is only to be called for subprogram invocations, and
 * enclosing_symt must not be NULL.
 */
PRIVATE Lsymtab *inherit_local(int h, Lsymtab *enclosing_symt)
{
	Lsymtab *symt = &loc_symtab[loc_symtab_top];
	if(loc_symtab_top == LOCSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in local symbol table\n\
Recompile me with larger LOCSYMTABSZ value\n"
#else
"out of space in local symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
	  (*symt) = (*enclosing_symt); /* copy the masked entry */
	  symt->info.toklist = NULL;	 /* clear argument list */

	  symt->mask = enclosing_symt;	 /* mask the outer entry */
	  hashtab[h].com_loc_symtab = symt;

	  ++loc_symtab_top;
	}
	return symt;
}

/* Compare array dim info of two arrays.  Returns 0 if match, 1 if differ. */
int array_dim_cmp(array_dim_t a, array_dim_t b)
{
  return (array_size_is_unknown(a) || array_size_is_unknown(b)) ||
    (array_dims(a) != array_dims(b)) ||
    (array_size(a) != array_size(b));
}

/* $Id: argcheck.c,v 1.10 2002/12/15 19:43:15 moniot Rel $

	Routines to check subprogram type and argument agreement

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

/*

	Shared functions defined:

		check_arglists()  Scans global symbol table for subprograms
				  and finds subprogram defn if it exists.
*/
#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "pgsymtab.h"
#include "dtypes.h"

				/* Local functions defined */

PROTO(PRIVATE void arg_array_cmp,( char *name, ArgListHeader *args1,
			   ArgListHeader *args2 ));
PROTO(PRIVATE void report_dtype_line_declared, (int line_declared));
PROTO(PRIVATE void index_keyword_args,( const char *name, ArgListHeader *defn_head,
	    ArgListHeader *call_head ));

     		/* Compares subprogram calls with definition */
PRIVATE void
#if HAVE_STDC
arg_array_cmp(char *name, ArgListHeader *args1, ArgListHeader *args2)
#else /* K&R style */
arg_array_cmp(name,args1,args2)
	char *name;
	ArgListHeader *args1, *args2;
#endif /* HAVE_STDC */
{
	int i;
	int  n,
	     n1 = args1->numargs,
	     n2 = args2->numargs;
	ArgListElement *a1 = args1->arg_array,
		       *a2 = args2->arg_array;

	n = (n1 > n2) ? n2: n1;		/* n = min(n1,n2) */

	if (argcheck_argnumber && n1 != n2){
	  cmp_error_count = 0;
	  (void) argcmp_error_head(name,args1,"varying number of arguments:");

	  sub_error_report(args1,args1->is_defn? "Defined":"Invoked");
	  msg_tail("with");
	  msg_tail(ulongtostr((unsigned long)n1));
	  msg_tail(n1==1?"argument":"arguments");

	  sub_error_report(args2,args2->is_defn? "Defined":"Invoked");
	  msg_tail("with");
	  msg_tail(ulongtostr((unsigned long)n2));
	  msg_tail(n2==1?"argument":"arguments");
	}

	if(argcheck_argtype)
	{	/* Look for type and kind mismatches */
	    cmp_error_count = 0;
	    for (i=0; i<n; i++) {
	      int c1 = storage_class_of(a1[i].type),
	          c2 = storage_class_of(a2[i].type),
		  t1 = datatype_of(a1[i].type),
	          t2 = datatype_of(a2[i].type);
	      kind_t kind1 = a1[i].kind,
		     kind2 = a2[i].kind;
	      long
		  s1 = a1[i].size,
		  s2 = a2[i].size;

	      int defsize1, defsize2;
	      int cmptype1, cmptype2;

	      int k = i;

	      /* for keyword argument, find the dummy argument which
	       * matches the keyword name
	       */
	      if (a2[i].keyword != NULL) {
		  /* if no matching dummy argument, skip it
		   */
		  if (a2[i].keyword_index == -1)
		      continue;
		  k = a2[i].keyword_index;
		  c1 = storage_class_of(a1[k].type);
		  t1 = datatype_of(a1[k].type);
		  kind1 = a1[k].kind;
		  s1 = a1[k].size;
	      }

	      defsize1 = (s1==size_DEFAULT);
	      defsize2 = (s2==size_DEFAULT);
				/* cmptype is type to use for mismatch test.
				   Basically cmptype=type but DP matches
				   REAL, DCPX matches CPLX, and hollerith
				   matches any numeric or logical type
				   but not  character.  The single/double
				   match will be deferred to size check. */

		/* If -portability, do not translate default sizes so
		   they will never match explicit sizes. */
	      if( port_mixed_size || local_wordsize==0 ) {
		cmptype1 = (t1==type_HOLLERITH && t2!=type_STRING)?
				t2:t1;
		cmptype2 = (t2==type_HOLLERITH && t1!=type_STRING)?
				t1:t2;
	      }
	      else {
		cmptype1 = (t1==type_HOLLERITH && t2!=type_STRING)?
				t2:type_category[t1];
		cmptype2 = (t2==type_HOLLERITH && t1!=type_STRING)?
				t1:type_category[t2];
		if(defsize1)
		  s1 = type_size[t1];
		if(defsize2)
		  s2 = type_size[t2];
	      }


	      if(s1 < 0 || s2 < 0) { /* char size_ADJUSTABLE or UNKNOWN */
		s1 = s2 = size_DEFAULT;	/* suppress warnings on size */
		defsize1 = defsize2 = TRUE;
	      }

			 /* Require exact match between storage classes and
			    compatible data type.  If that is OK, then for
			    non-char args require exact size match.  For char
			    and hollerith defer size check to other section.
			  */
	    if( (c1 != c2) || (cmptype1 != cmptype2) || ( (s1 != s2) &&
			is_num_log_type(t1) && is_num_log_type(t2) ) ) {
		if(argcmp_error_head(name,args1,"argument data type mismatch"))
		  break;

		arg_error_report(args1,args1->is_defn? "Dummy arg": "Actual arg",k,
				 "is type", i);
		if( t1 != type_LABEL ) /*label arg: only print storage class*/
		  msg_tail(global_typespec(t1,!defsize1,(long)s1,FALSE,0));
		if( is_derived_type(t1) ) {/*print line number for derived types */
		  report_dtype_line_declared(dtype_table[t1]->line_declared);
		}
		msg_tail(class_name[storage_class_of(a1[i].type)]);
				     
		arg_error_report(args2,args2->is_defn? "Dummy arg": "Actual arg",i,
				 "is type", i);
		if( t2 != type_LABEL ) /*label arg: only print storage class*/
		  msg_tail(global_typespec(t2,!defsize2,(long)s2,FALSE,0L));
		if( is_derived_type(t2) ) {/*print line number for derived types */
		  report_dtype_line_declared(dtype_table[t2]->line_declared);
		}
		msg_tail(class_name[storage_class_of(a2[i].type)]);
				 
		if(args1->is_defn
			&& storage_class_of(a1[i].type) == class_SUBPROGRAM
			&& storage_class_of(a2[i].type) != class_SUBPROGRAM
			&& datatype_of(a1[i].type) != type_SUBROUTINE
			&& ! a1[i].declared_external )
		  (void)fprintf(list_fd,
		     "\n    (possibly it is an array which was not declared)");
	      }
				/* If no class/type/elementsize clash,
				   and if comparing dummy vs. actual,
				   check character and hollerith sizes */
	      else if(args1->is_defn) {
				/* Character: check size but skip *(*)
				   and dummy array vs. actual array element.
				 */
		if(t1 == type_STRING && s1 > 0 && s2 > 0 &&
		  !(a1[i].array_var && a2[i].array_element)) {
		    long
		      dims1,dims2,size1,size2;

		    if(a1[i].array_var) {
		      dims1 = array_dims(a1[i].array_dim);
		      size1 = array_size(a1[i].array_dim);
		    }
		    else {
		      dims1 = 0;
		      size1 = 1;
		    }
		    if(a2[i].array_var && !a2[i].array_element) {
		      dims2 = array_dims(a2[i].array_dim);
		      size2 = array_size(a2[i].array_dim);
		    }
		    else {
		      dims2 = 0;
		      size2 = 1;
		    }

				/* standard requires dummy <= actual size.
			         */
		  if( (s1*size1 > s2*size2 &&
		      (dims1==0 || size1>1) && (dims2==0 || size2>1)) ) {

		    if(argcmp_error_head(name,args1,"argument mismatch"))
				break;

		    arg_error_report(args1,"Dummy arg",k,"is type",i);
		    msg_tail(global_typespec(t1,TRUE,(long)s1,dims1>0,size1));

		    arg_error_report(args2,"Actual arg",i,"is type",i);
		    msg_tail(global_typespec(t2,TRUE,(long)s2,dims2>0,size2));

		  }/*end if char size mismatch*/
		}/*end if type==char*/

		else if(t2 == type_HOLLERITH) {
			/* Allow hollerith to match any noncharacter type of
			   at least equal aggregate size.  */
		    long dims1,size1;
		    if(a1[i].array_var) {
		      dims1 = array_dims(a1[i].array_dim);
		      size1 = array_size(a1[i].array_dim);
		    }
		    else {
		      dims1 = 0;
		      size1 = 1;
		    }
		    if(s2 > s1*size1 && (dims1==0 || size1>1)) {
		      if(argcmp_error_head(name,args1,"argument mismatch"))
				break;

		      arg_error_report(args1,"Dummy arg",k,"is type",i);
		      msg_tail(typespec(t1,!defsize1,(long)s1,dims1>0,size1));

		      arg_error_report(args2,"Actual arg",i,"is type",i);
		      msg_tail(typespec(t2,TRUE,(long)s2,FALSE,0L));

		    }/*end if holl size mismatch*/
		}/*end if type==holl*/

		
		/* Check kind parameter only if kind was defined on
		 * one or both.  Mismatch is for sure if differing
		 * kinds with at least one concrete.  Otherwise warn
		 * only for -port=mixed-kind.
		 */
		else if ( (kind1 != kind2) &&
			  (port_mixed_kind || kind1 >= 0 || kind2 >= 0) ) {
		    if(argcmp_error_head(name,args1,"argument kind mismatch"))
		      break;
		    arg_error_report(args1,"Dummy arg",k,"is",i);
		    if(! kind_is_default(kind1))
		      report_kind(kind1);
		    msg_tail(typespec(t1,!defsize1,(long)s1,FALSE,0L));
		    arg_error_report(args2,"Actual arg",i,"is",i);
		    if(! kind_is_default(kind2))
		      report_kind(kind2);
		    msg_tail(typespec(t2,!defsize2,(long)s2,FALSE,0L));
		}

	      }/*if(args1->is_defn)*/
	    }/*end for i*/
	}/* end look for type && size mismatches */


		 /* Check arrayness of args only if defn exists */
	if(argcheck_arrayness && args1->is_defn ) {
	    int has_array_arg = FALSE; /* for checking INTENT(OUT) args of elemental */
	    int array_arg_index = -1;
	    cmp_error_count = 0;
	    for (i=0; i<n; i++) {
			/* Skip if class or datatype mismatch.  This
			   also skips holleriths which were checked above.
			   Do not process externals.
			 */
	      int k = i;

	      /* for keyword argument, find the dummy argument which
	       * matches the keyword name
	       */
	      if (a2[i].keyword != NULL) {
		  /* if no matching dummy argument, skip it
		   */
		  if (a2[i].keyword_index == -1)
		      continue;
		  k = a2[i].keyword_index;
	      }

	      if(datatype_of(a2[i].type) != type_HOLLERITH &&
		 storage_class_of(a1[k].type) == class_VAR &&
		 storage_class_of(a2[i].type) == class_VAR) {

		if( a2[i].array_var ) {
		  has_array_arg = TRUE; /* keep track of seeing array args */
		  if( array_arg_index == -1 )
		    array_arg_index = i; /* remember where first occurrence */
		}

		if( a1[k].array_var ) {	/* I. Dummy arg is array */
		    if( a2[i].array_var ) {
			if( a2[i].array_element ) {
					/*   A. Actual arg is array elt */
					/*	Warn on arraycheck_dims. */
			    if(arraycheck_dims) {

			      if(argcmp_error_head(
				      name,args1,"argument arrayness mismatch"))
				break;

			      arg_error_report(args1,"Dummy arg",k,"is whole array",i);
			      arg_error_report(args2,"Actual arg",i,"is array element",i);
			    }
			}/* end case I.A. */

			else {
					/*   B. Actual arg is whole array */
					/*	Warn if dims or sizes differ */
			  array_dim_t
			    diminfo1,diminfo2;
			  long dims1,dims2,size1,size2,
			    cmpsize1,cmpsize2;
			  diminfo1 = a1[k].array_dim;
			  diminfo2 = a2[i].array_dim;
			  dims1 = array_dims(diminfo1);
			  dims2 = array_dims(diminfo2);
			  cmpsize1 = size1 = array_size(diminfo1);
			  cmpsize2 = size2 = array_size(diminfo2);
				/* For char arrays relevant size is no. of
				   elements times element size. But use
				   no. of elements if *(*) involved. */
			  if(datatype_of(a1[i].type) == type_STRING
			     && a1[k].size > 0 && a2[i].size > 0) {
			    cmpsize1 *= a1[k].size;
			    cmpsize2 *= a2[i].size;
			  }

			/* size = 0 or 1 means variable-dim: OK to differ */
			  if( (arraycheck_size &&
				  (size1>1 && size2>1 && cmpsize1 != cmpsize2))
			     || (arraycheck_dims &&
				  (dims1 != dims2)) ) {
			        char sizebuf[2+2*MAX_ULONGTOSTR];
				if(argcmp_error_head(
					name,args1,"argument arrayness mismatch"))
				      break;

				arg_error_report(args1,"Dummy arg",k,"has",i);
				msg_tail(ulongtostr((unsigned long)dims1));
				msg_tail(dims1==1?"dim":"dims");
				if(size1 > 1) { /* report only known size */
				  msg_tail("size");
				  strcpy(sizebuf,ulongtostr((unsigned long)size1));
				  if(datatype_of(a1[k].type) == type_STRING
				     && a1[k].size > 0) {
				    strcat(sizebuf,"*");
				    strcat(sizebuf,ulongtostr((unsigned long)(a1[i].size)));
				  }
				  msg_tail(sizebuf);
				}
				arg_error_report(args2,"Actual arg",i,"has",i);
				msg_tail(ulongtostr((unsigned long)dims2));
				msg_tail(dims2==1?"dim":"dims");
				if(size2 > 1) {
				  msg_tail("size");
				  strcpy(sizebuf,ulongtostr((unsigned long)size2));
				  if(datatype_of(a2[i].type) == type_STRING
				     && a2[i].size > 0) {
				    strcat(sizebuf,"*");
				    strcat(sizebuf,ulongtostr((unsigned long)(a2[i].size)));
				  }
				  msg_tail(sizebuf);
				}
			  }/* end if size mismatch */
			}/* end case I.B. */
		    }
		    else {
					/*   C. Actual arg is scalar */
					/*	Warn in all cases */

		      	if(argcmp_error_head(
				name,args1,"argument arrayness mismatch"))
			  break;

			arg_error_report(args1,"Dummy arg",k,"is array",i);
			arg_error_report(args2,"Actual arg",i,"is scalar",i);
		    }/* end case I.C. */
		} /* end dummy is array, case I. */

		else {			/* II. Dummy arg is scalar */
		    if( a2[i].array_var ) {
			if( a2[i].array_element ) {
					/*   A. Actual arg is array elt */
					/*	OK */
			}
			else {
					/*   B. Actual arg is whole array */
					/*	Warn except if elemental */

			 if( !args1->prog_unit->elemental ) {
			  if(argcmp_error_head(
				   name,args1,"argument arrayness mismatch"))
			    break;

			  arg_error_report(args1,"Dummy arg",k,"is scalar",i);
			  arg_error_report(args2,"Actual arg",i,"is whole array",i);
			 }

			}/* end case II.B. */
		    }
		    else {
					/*   C. Actual arg is scalar */
					/*	OK */
		    }

		} /* end dummy is scalar, case II */

	      } /* end if class_VAR */
	    }/* end for (i=0; i<n; i++) */

	    /* Check whether elemental procedure has scalar actual arg
	       for INTENT(OUT) dummy arg when some arg is array. */
	    if( args1->prog_unit->elemental && has_array_arg ) {
	      for (i=0; i<n; i++) {
		int k = i;

		if (a2[i].keyword != NULL) {
		  /* if no matching dummy argument, skip it
		  */
		  if (a2[i].keyword_index == -1)
		    continue;
		  k = a2[i].keyword_index;
		}

		if(datatype_of(a2[i].type) != type_HOLLERITH && /* stuff disallowed */
		   storage_class_of(a1[k].type) == class_VAR && /* in elemental */
		   storage_class_of(a2[i].type) == class_VAR) { /* but check anyway */
		  if( a1[k].intent_out && !a2[i].array_var ) {
		    if(argcmp_error_head(
			     name,args1,"argument arrayness mismatch"))
		      break;
		    arg_error_report(args1,"Elemental procedure dummy argument",k,
				     "is INTENT(OUT)", i);
		    arg_error_report(args2,"Actual arg",i,"is scalar",i);
		    arg_error_report(args2,"But another actual arg",array_arg_index,
				     "is array", array_arg_index);
		    break;	/* one warning is sufficient */
		  }
		}
	      }
	    }

	}/* if( args1->is_defn ) */


		 /* Check usage of args only if defn exists.  Arg array
		    1 is dummy args, array 2 is actual args.  */
	if( (usage_arg_modified || usage_arg_alias_modified ||
	     usage_array_alias_modified || usage_var_uninitialized ||
	     usage_arg_common_modified || usage_array_common_modified ||
	     usage_do_var_modified)
					&& args1->is_defn ) {
	    cmp_error_count = 0;
	    for (i=0; i<n; i++) {
	      int k = i;

	      if (a2[i].keyword != NULL) {
		  /* if no matching dummy argument, skip it
		   */
		  if (a2[i].keyword_index == -1)
		      continue;
		  k = a2[i].keyword_index;
	      }

	      if(storage_class_of(a1[k].type) == class_VAR &&
		 storage_class_of(a2[i].type) == class_VAR ) {
		int nonlvalue_out = (a1[k].assigned_flag && !a2[i].is_lvalue);
		int nonset_in = (a1[k].used_before_set && !a2[i].set_flag);
		int alias_modified = (a1[k].set_flag && (a2[i].same_as != i));
		int arg_alias_modified = (alias_modified && !a2[i].array_var);
		int array_alias_modified = (alias_modified && a2[i].array_var);
		int do_var_modified = (a1[k].set_flag && a2[i].active_do_var);
		int common_modified_as_arg, common_modified_as_com, /*maybe*/
		    common_assigned_as_arg, common_assigned_as_com; /*for sure*/
		int arg_common_modified, /*nonarray arg*/
		    array_common_modified; /*array arg*/
		char *common_alias_name = NULL;
				/* See if arg aliased to common variable, and
				   if so, is either one modified. */
		common_modified_as_arg = common_modified_as_com = FALSE;
		common_assigned_as_arg = common_assigned_as_com = FALSE;
		if( a2[i].common_block != 0 ) {
			/* Find out if block is defined in called routine */
		  ComListHeader *clist = a2[i].common_block->info.comlist;
		  while( clist != NULL ) {
		    if( clist->prog_unit == args1->prog_unit ) {
		      break;	/* found it */
		    }
		    clist = clist->next;
		  }
		  if( clist != NULL ) {	/* block is defined in called prog unit */
		      if( comcheck_exact ) { /* Exact common: find the var */
				/* It is not yet an error unless the block
				   is also long enough to include the variable
				   and the variable is modified in either
				   place.
				*/
  			if( a2[i].common_index <= clist->numargs ) {
			/* Don't forget that index goes from 1 to numargs.*/
			  int j = a2[i].common_index - 1;
			  common_alias_name = clist->com_list_array[j].name;
			  common_modified_as_arg = a1[k].set_flag;
			  common_assigned_as_arg = a1[k].assigned_flag;
			  common_modified_as_com = clist->com_list_array[j].set;
			  common_assigned_as_com = clist->com_list_array[j].assigned;
			}
		      }
		      else {	/* Inexact common: just see if block or
				   variable is modified.  Don't set
				   assigned_as_com to always say "may be".
				*/
			common_modified_as_arg = a1[k].set_flag;
			common_assigned_as_arg = a1[k].assigned_flag;
			common_modified_as_com = clist->any_set;
			common_assigned_as_com = FALSE;
		      }
		  } /* clist != NULL */
		} /* a2[i].common_block != 0 */
		arg_common_modified =
		  ((common_modified_as_arg || common_modified_as_com) && !a2[i].array_var);
		array_common_modified =
		  ((common_modified_as_arg || common_modified_as_com) && a2[i].array_var);

#ifdef DEBUG_PGSYMTAB
if(debug_latest) {
(void)fprintf(list_fd,
"\nUsage check: %s[%d] dummy asgnd %d ubs %d  actual lvalue %d set %d do %d",
args1->prog_unit->name,
i+1,
a1[k].assigned_flag,
a1[k].used_before_set,
a2[i].is_lvalue,
a2[i].set_flag,
a2[i].active_do_var);
}
#endif

		if( (usage_arg_modified && nonlvalue_out) ||
		    (usage_var_uninitialized && nonset_in)||
		    (usage_arg_alias_modified && arg_alias_modified)||
		    (usage_array_alias_modified && array_alias_modified)||
		    (usage_arg_common_modified && arg_common_modified)||
		    (usage_array_common_modified && array_common_modified)||
		    (usage_do_var_modified && do_var_modified) ) {

		  if(argcmp_error_head(name,args1,"argument usage mismatch"))
		     break;

			/* Usage case 1: Modifying arg that is constant or
			   expression.
			*/
		  if(usage_arg_modified && nonlvalue_out) {
		    if (a1[k].intent_out) {
		    arg_error_report(args2,"Actual arg",i,"is const or expr when dummy arg is INTENT out",i);
		    }
		    else {
		    arg_error_report(args1,"Dummy arg",k,"is modified",i);
		    arg_error_report(args2,"Actual arg",i,"is const or expr",i);
		    }

		  }

			/* Usage case 2: Using arg that is not set.
			*/
		  if(usage_var_uninitialized && nonset_in) {

		    arg_error_report(args1,"Dummy arg",k,"is used before set",i);
		    if (a1[k].intent_in)
		    arg_error_report(args2,"Actual arg",i,"is not set when dummy arg in callee is INTENT in",i);
		    else
		    arg_error_report(args2,"Actual arg",i,"is not set",i);
		  }

			/* Usage case 3: Modifying arg that is the same as
			   another arg.
			*/
		  if((usage_arg_alias_modified && arg_alias_modified)||
		    (usage_array_alias_modified && array_alias_modified)) {
		    arg_error_report(args1,"Dummy arg",k,
			     a1[i].assigned_flag?
				     "is modified":
				     "may be modified",i);
		    arg_error_report(args2,"Actual arg",i,
				     a2[i].array_var?
					"may be same as arg":
					"same as arg",i);
		    msg_tail(ulongtostr((unsigned long)(long)(a2[i].same_as+1)));
		    msg_tail(":");
		    msg_tail(a2[a2[i].same_as].name);

		  }

			/* Usage case 4: Modifying arg that is the same as
			   a variable in common.
			*/
		  if((usage_arg_common_modified && arg_common_modified)||
		     (usage_array_common_modified && array_common_modified)) {
		    char locspec[10+3*sizeof(a2[i].common_index)];
		    if( comcheck_exact ) {
		      (void)sprintf(locspec,"%ld:",a2[i].common_index);
		    }
		    else {
		      (void)sprintf(locspec,"somewhere");
		    }
		    arg_error_report(args1,"Dummy arg",k,"is aliased to common var",i);
		    msg_tail(locspec);
		    msg_tail(comcheck_exact?common_alias_name: "");
		    msg_tail("in block");
		    msg_tail(a2[i].common_block->name);
		    msg_tail(common_modified_as_com?
				   (common_assigned_as_com?
				     "which is modified":
				     "which may be modified"):
				   "");

                    if( common_modified_as_arg ) {
		      arg_error_report(args1,"Dummy arg",k,
				       common_assigned_as_arg?
					"is modified":
					"may be modified",i);
                    }

		    arg_error_report(args2,"Actual arg",i,"is in common block",i);
		    msg_tail(a2[i].common_block->name);
		  }

			/* Usage case 5: Modifying arg that is an active
			   DO index variable.
			 */
		  if( usage_do_var_modified && do_var_modified ) {
		    arg_error_report(args1,"Dummy arg",k,
			     a1[i].assigned_flag?
				     "is modified":
				     "may be modified",i);
		    arg_error_report(args2,"Actual arg",i,
				     "is active DO index",i);
		  }
		}
	      }
	    }
	}/*end if( (usage_arg...) && args->is_defn) */

}/* arg_array_cmp */


PRIVATE void
report_dtype_line_declared(int line_declared)
{
	char msg[] = "declared on line 000000";
	if( line_declared != NO_LINE_NUM ) {
	  sprintf(msg,"declared on line %-6d",line_declared);
	  msg_tail(msg);
	}
}

void
check_arglists(int hashno, SUBPROG_TYPE limit)	/* Scans global symbol table for subprograms */
{                       /* and finds subprogram defn if it exists */
	int i, curr_i;
	ArgListHeader *defn_list, *alist;

#ifdef DEBUG_GLOBALS
if(debug_latest) {
  (void)fprintf(list_fd,"\nGlobal symbol table (limit=%s):",
		(limit==module_subprog?"module":
		 (limit==internal_subprog?"internal":"not subprog"))
		);
}
#endif

/* start the scan from index of current program unit, whose hashtable
   index is in hashno
 */
	curr_i = ((hashno == -1) ? 0 : hashtab[hashno].glob_symtab - &glob_symtab[0]);
	for (i=curr_i; i<glob_symtab_top; i++){

#ifdef DEBUG_GLOBALS
if(debug_latest) {
(void)fprintf(list_fd,
	      "\n%3d%10s %8s %2svalid %5s %7s",
	      i,
	      glob_symtab[i].name,
	      (glob_symtab[i].module_subprog?"module":
	       (glob_symtab[i].internal_subprog?"internal":"subprog")),
	      (glob_symtab[i].valid?"":"in"),
	      class_name[storage_class_of(glob_symtab[i].type)],
	      (glob_symtab[i].visited?"visited":"")
	      );
}
#endif
		/***************************** added ****************************/
	  if (glob_symtab[i].valid) {
		if ((limit == module_subprog && glob_symtab[i].module_subprog) ||
		  (limit == internal_subprog && glob_symtab[i].internal_subprog) ||
		  (limit == from_module && glob_symtab[i].from_module) ||
		  (limit == not_subprog && !glob_symtab[i].internal_subprog
		   	&& !glob_symtab[i].module_subprog)) {
		/****************************************************************/

				/* Skip common blocks */
	    if(storage_class_of(glob_symtab[i].type) != class_SUBPROGRAM)
		continue;

				/* Skip unvisited library prog units */
	    if(glob_symtab[i].library_prog_unit && !glob_symtab[i].visited)
		continue;


	    if((alist=glob_symtab[i].info.arglist) == NULL){
	      oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		      "global symbol has no argument lists:");
	      oops_tail(glob_symtab[i].name);
	    }
	    else{	/* alist != NULL */
		int num_defns= 0;
		ArgListHeader *list_item;

			/* use 1st invocation instead of defn if no defn */
		defn_list = alist;

				/* Find a definition in the linked list of
				   usages.  Count how many defns found. */
		list_item = alist;
		while(list_item != NULL){
#ifdef DEBUG_GLOBALS
if(debug_latest) {
 int a;
 (void)fprintf(list_fd,
	      "\n\tArglist %s",
	      (list_item->is_defn?"defn":(list_item->is_call?"call":(list_item->actual_arg?"arg ":"ext ")))
	      );
 for(a=0; a<list_item->numargs; a++) {
   (void)fprintf(list_fd,
		 ", %s %s",
		 list_item->arg_array[a].name,
		 type_name(datatype_of(list_item->arg_array[a].type))
		 );
 }
}
#endif
		    if(list_item->is_defn){
					/* report multiple defns */
			if(usage_ext_multiply_defined && num_defns > 0) {
			    if(num_defns == 1) {
				cmp_error_count = 0;
				(void)argcmp_error_head(glob_symtab[i].name,
					       defn_list,
					       "multiply defined");
				sub_error_report(defn_list,"Defined");
			    }
			    sub_error_report(list_item,"Defined");
			}
			++num_defns;
			defn_list = list_item;	/* Use last defn found */
		    }
		    else { /* ! list_item->is_defn */
				/* Here treat use as actual arg like call */
			if(list_item->is_call || list_item->actual_arg){
				 /* Use last call by a visited or nonlibrary
				    prog unit as defn if no defn found */
			  if(!defn_list->is_defn
			     && !irrelevant(list_item) )
			    defn_list = list_item;
		        }
		    }

		    list_item = list_item->next;
		}
		if(num_defns == 0){
				/* If no defn found, and all calls are
				   from unvisited library prog units, skip. */
		  if(irrelevant(defn_list))
		    continue;

				/* If no definitions found, report error */
		   if( (usage_ext_undefined && glob_symtab[i].used_flag)
		    || (usage_ext_declared_only && !glob_symtab[i].used_flag) ) {
		     cmp_error_count = 0;
		     (void)argcmp_error_head(glob_symtab[i].name,
				defn_list,
				glob_symtab[i].used_flag?
					     "never defined":
					     "never defined nor invoked");
		     sub_error_report(defn_list,
				(defn_list->external_decl)?"Declared":"Invoked");

			/* Warn if it seems it may just be an array they
			   forgot to declare */
		      if(defn_list->numargs != 0
			 && datatype_of(defn_list->type) != type_SUBROUTINE
			 && ! glob_symtab[i].declared_external) {
			if(novice_help)
			  (void)fprintf(list_fd,
	    "\n    (possibly it is an array which was not declared)");
		      }
		   }
		}
				/* If definition is found but prog unit is
				   not in call tree, report it unless -lib
				   or it is a module subprogram
				*/
		else{	/* num_defns != 0 */
		    if(!glob_symtab[i].visited
		       && datatype_of(glob_symtab[i].type) != type_BLOCK_DATA
		       && datatype_of(glob_symtab[i].type) != type_MODULE
		       && !(glob_symtab[i].library_prog_unit || (glob_symtab[i].module_subprog && !glob_symtab[i].private))
		       && usage_ext_unused) {
			cmp_error_count = 0;
			(void)argcmp_error_head(glob_symtab[i].name,
				   defn_list,
/*
				   glob_symtab[i].used_flag?"not in call tree":"never invoked"
*/
				    "never invoked"
						);
			sub_error_report(defn_list,"Defined");
		    }
		}

			/* Now check defns/invocations for consistency.  If
			   no defn, 1st invocation will serve. Here treat
			   use as actual arg like call.  Ignore calls & defns
			   in unvisited library prog units. */
		if( argcheck_functype &&
		   (defn_list->is_defn || !defn_list->external_decl)) {
		  cmp_error_count = 0;
		  while(alist != NULL){
			if(alist != defn_list && !alist->external_decl
			   && !irrelevant(alist)) {
			  int c1 = storage_class_of(defn_list->type),
			      c2 = storage_class_of(alist->type),
			      t1 = datatype_of(defn_list->type),
			      t2 = datatype_of(alist->type),
			      s1 = defn_list->size,
			      s2 = alist->size,
			      k1 = defn_list->kind,
			      k2 = alist->kind,
			      defsize1 = (s1 == size_DEFAULT),
			      defsize2 = (s2 == size_DEFAULT),
			      cmptype1= type_category[t1],
			      cmptype2= type_category[t2];
		/* If -portability, do not translate default sizes so
		   they will never match explicit sizes. */
			  if(!(port_mixed_size || local_wordsize==0)) {
			    if(defsize1)
			      s1 = type_size[t1];
			    if(defsize2)
			      s2 = type_size[t2];
			  }

			  if(s1 < 0 || s2 < 0){ /*size_ADJUSTABLE or UNKNOWN*/
			    s1 = s2 = size_DEFAULT;/* suppress size warnings */
			    defsize1 = defsize2 = TRUE;
			  }
				/* Check class, type, and size */
			  if( (c1 != c2) || (cmptype1 != cmptype2) ||
			      /* If selected/default kind params don't match, it could
			       * still be OK on a specific processor,
			       * so only warn under -port */
			      (k1 != k2 && (port_mixed_kind || k1 >= 0 || k2 >= 0) ) ||
			     ( (s1 != s2) &&
				/*exclude char size-only mismatch betw calls */
			      (t1 != type_STRING ||
			        defn_list->is_defn || alist->is_defn )) ){

			    	if(argcmp_error_head(glob_symtab[i].name,
					      defn_list,
					      "invoked inconsistently"))
				    break;
				if(cmp_error_count == 1) {
				  sub_error_report(defn_list,
						   defn_list->is_defn?
						     "Defined":
						     "Invoked");
				  msg_tail("as type");
				  if( k1 != k2 && !kind_is_default(k1) ) report_kind(k1);
				  msg_tail(global_typespec(t1,!defsize1,(long)s1,
					   FALSE, 0L));
				}

				sub_error_report(alist,
					       alist->is_defn?
						 "Defined":
						 "Invoked");
				msg_tail("as type");
				if( k1 != k2 && !kind_is_default(k2) ) report_kind(k2);
				msg_tail(global_typespec(t2,!defsize2,(long)s2,
					 FALSE, 0L));
			  }
			}
			alist = alist->next;

		  }/* end while(alist != NULL) */
	        }/* end if(defn) */

		alist = glob_symtab[i].info.arglist;
		while(alist != NULL){
		  /* Here we require true call, not use as actual arg.
		     Also, do not compare multiple defns against each
		     other. */
		    if(alist != defn_list &&
		       (defn_list->is_defn || defn_list->is_call) &&
		       (alist->is_call && !irrelevant(alist)) ){
		            index_keyword_args(glob_symtab[i].name,defn_list,alist);
			    arg_array_cmp(glob_symtab[i].name,defn_list,alist);
			}
			alist = alist->next;

		}/* end while(alist != NULL) */
	    }/* end else <alist != NULL> */
	}
    }
	}/* end for (i=0; i<glob_symtab_top; i++) */
}

/* for keyword arguments, store the index of the occurrence of the keyword
 * name in the definition list for checking later
 */
PRIVATE void 
index_keyword_args(const char *subprog_name, ArgListHeader *defn_head, ArgListHeader *call_head)
{
    int i, j, first_keyword = -1;
    char *keyword_name;
    ArgListElement *defn_list = defn_head->arg_array,
		   *call_list = call_head->arg_array;
    int n1 = defn_head->numargs,
	n2 = call_head->numargs;

    for (i = 0; i < n2; i++) {
	/* for keyword argument, find the dummy argument which
	 * matches the keyword name
	 */
	if (call_list[i].keyword != NULL) {
	    if (first_keyword == -1) first_keyword = i;
	    /* Linear search through definition list for a dummy name that
	     * matches keyword name.  Note this will give incorrect
	     * warning "no such dummy argument" if keyword is provided
	     * for previously non-keyword arg, e.g. function f(a,b)
	     * invoked with y=f(1,a=0) [which is illegal].  Maybe fix
	     * this someday.
	     */
	    for (j = first_keyword; j < n1; j++) {
		keyword_name = defn_head->is_call && defn_list[j].keyword ?
		    defn_list[j].keyword : defn_list[j].name;
		if (strcmp(keyword_name, call_list[i].keyword) == 0) {
		    call_list[i].keyword_index = j;
		    break;
		}
	    }
	    if (j == n1) {
		call_list[i].keyword_index = -1;
		argcmp_error_head(subprog_name,call_head,
			": keyword use");
		arg_error_report(call_head,"Actual argument",i,
			"no such dummy argument",i);
		msg_tail(call_list[i].keyword);
	    }
	}
	else if (first_keyword == -1) {
	    call_list[i].keyword_index = i;
	}
	else {
	    call_list[i].keyword_index = -1;
	    argcmp_error_head(subprog_name,call_head,
		    ": keyword use");
	    arg_error_report(call_head,"Actual argument",i,
		    "requires keyword",i);
	}
    }
}

#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "pgsymtab.h"

/* private routines */
PROTO(PRIVATE void mod_var_usage,( char *name, ModVarListHeader *mvl ));
PROTO(PRIVATE void print_marked_mod_vars,(ModVar *mv, int n));

/* check usage of module variables */

void check_mod_usage()
{
    int i;

    for(i = 0; i < glob_symtab_top; i++) {

	if (datatype_of(glob_symtab[i].type) == type_MODULE){
	    /* only check if there are module variables */
	    if (glob_symtab[i].modvarlist &&
		    glob_symtab[i].modvarlist->numargs > 0) {
		mod_var_usage(glob_symtab[i].name, glob_symtab[i].modvarlist);
	    }
	}
    }

}

/* check usage of module variables */

PRIVATE void mod_var_usage(char *name, ModVarListHeader *mvl_head)
{
    ModVarListHeader *cur_head;
    int mod_any_used = FALSE, 		/* global usage flags */
	mod_any_set = FALSE,
	mod_unused_somewhere = FALSE;

	/* check each list which has a header */
    for (cur_head = mvl_head; cur_head != NULL; cur_head = cur_head->next) {
	if (cur_head->any_used)
	    mod_any_used = TRUE;
	if (cur_head->any_set)
	    mod_any_set = TRUE;
	if(!cur_head->in_module &&
	   !(cur_head->any_used || cur_head->any_set) )
	    mod_unused_somewhere = TRUE;
    }

    if (usage_mod_var_unused) {
	/* none of the module variables are used in any prog unit */
	if ( !(mod_any_used || mod_any_set) ) {
	    cmp_error_count = 0;
	    modcmp_error_head(name,mvl_head,"unused in any prog unit");
	}
	else {
	    /* module not used in some prog units: identify them.
	     * Don't report non-usage in module itself since that is
	     * often normal.

	     */
	    if (mod_unused_somewhere) {
		cmp_error_count = 0;
		modcmp_error_head(name,mvl_head,"unused in the following prog units:");

		
		for (cur_head = mvl_head; cur_head != NULL; cur_head = cur_head->next) {
		    if(!cur_head->in_module &&
		       ! (cur_head->any_used || cur_head->any_set) ) {
			mod_error_report(cur_head,"");
		    }
		   
		}
	    }
	}
    }

    /* Now go thru the lists of variables to find any that are not
     * used/set anywhere.  Skip if it was already reported that none
     * are used/set.  Use the first list to accumulate OR of usage
     * flags of the variables */
    if( mod_any_used || mod_any_set ) {
	int n=mvl_head->numargs;
	int i, var_count;
	ModVar *mv = mvl_head->mod_var_array;
	for(cur_head = mvl_head->next; cur_head != NULL; cur_head = cur_head->next) {
	    ModVar *cur_mv = cur_head->mod_var_array;
	    for(i=0; i<n; i++) {
		if( cur_mv[i].used )
		    mv[i].used = TRUE;
		if( cur_mv[i].set )
		    mv[i].set = TRUE;
	    }
	}

	/* Flag variables that are unused and unset in any prog unit */
	if( usage_mod_var_unused ) {
	  var_count = 0;
	  for(i=0; i<n; i++) {
	    if( !(mv[i].used || mv[i].set) && mv[i].check_usage) {
		mv[i].marked = TRUE;
		var_count++;
	    }
	  }
	  if(var_count > 0) {
	    cmp_error_count = 0;
	    modcmp_error_head(name,mvl_head,
			      "unused/unset in prog units that USE it:");
	    print_marked_mod_vars(mv, n);
	  }
	  for(i=0; i<n; i++) {
	    mv[i].marked = FALSE;
	  }
	}

	/* Flag variables set but not used in any prog unit */
	if( usage_mod_var_set_unused ) {
	  var_count = 0;
	  for(i=0; i<n; i++) {
	    if( mv[i].set && !mv[i].used && mv[i].check_usage) {
		mv[i].marked = TRUE;
		var_count++;
	    }
	  }
	  if(var_count > 0) {
	    cmp_error_count = 0;
	    modcmp_error_head(name,mvl_head,
			      "set but not used in prog units that USE it:");
	    print_marked_mod_vars(mv, n);
	  }

	  for(i=0; i<n; i++) {
	    mv[i].marked = FALSE;
	  }
	}

	/* Flag variables used but not set in any prog unit*/
	if( usage_mod_var_uninitialized ) {
	  var_count = 0;
	  for(i=0; i<n; i++) {
	    if( mv[i].used && !mv[i].set && mv[i].check_usage) {
		mv[i].marked = TRUE;
		var_count++;
	    }
	  }
	  if(var_count > 0) {
	    cmp_error_count = 0;
	    modcmp_error_head(name,mvl_head,
			      "used but not set in prog units that USE it:");
	    print_marked_mod_vars(mv, n);
	  }
	}
    }/*if( mod_any_used || mod_any_set )*/
}/*mod_var_usage*/

		/* Routine to print a list of module variables whose
		   marked flag has been set.  Print usename, not local name.
		 */
PRIVATE void
print_marked_mod_vars(ModVar *mv, int n)
{
    int i;
    COLNO_t col;
    for (i=0,col=78; i<n; i++){
	if (mv[i].marked){
	    if( (col += 1+(int)strlen(mv[i].usename)) > 78 ) {
		(void)fprintf(list_fd,"\n   ");
		col = 4+(int)strlen(mv[i].usename);
	    }
	    (void)fprintf(list_fd, " %s",
			  mv[i].usename);
	}
    }
}

#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "pgsymtab.h"

/* private routines */
PROTO(PRIVATE void mod_var_usage,( char *name, ModVarListHeader *mvl ));


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
    ModVarListHeader *cur_head = mvl_head;
    ModVar *mv;
    int i, 
	mod_any_used = FALSE, 
	mod_any_set = FALSE,
	mod_unused_somewhere = FALSE;

    while (cur_head != NULL) {	/* check each list which has a header */
	/* skip usage checking in the module itself */
	if (cur_head->in_module) {
	    if (cur_head->any_set)
		mod_any_set = TRUE;
	    cur_head = cur_head->next;
	    continue;
	}
	if (cur_head->any_used)
	    mod_any_used = TRUE;
	if (cur_head->any_set)
	    mod_any_set = TRUE;
	if( !(cur_head->any_used || cur_head->any_set) )
	    mod_unused_somewhere = TRUE;

	cur_head = cur_head->next;
    }

    if (usage_mod_var_unused) {
	/* none of the module variables are used in any prog unit */
	if ( !mod_any_used ) {
	    cmp_error_count = 0;
	    modcmp_error_head(name,mvl_head,"unused in any prog unit");
	}
	else {
	    /* module not used in some prog units */
	    if (mod_unused_somewhere) {
		cmp_error_count = 0;
		modcmp_error_head(name,mvl_head,"unused in the following prog units:");

		cur_head = mvl_head;
		while (cur_head != NULL) {
		    if (cur_head->in_module) {
			cur_head = cur_head->next;
			continue;
		    }

		    if(! (cur_head->any_used || cur_head->any_set) ) {
			mod_error_report(cur_head,"");
		    }
		    cur_head = cur_head->next;
		}
	    }

	    /* finally report on individual module variables */
	    fprintf(list_fd,"\n");
	    global_warning(NULL, NO_LINE_NUM, "Some variables of module");
	    msg_tail(name);
	    msg_tail("unused");

	    cur_head = mvl_head;
	    while (cur_head != NULL) {
		if ( cur_head->in_module ||
			!(cur_head->any_used || cur_head->any_set)) {
		    cur_head = cur_head->next;
		    continue;
		}

		mv = cur_head->mod_var_array;
		mod_error_report(cur_head, "");
		msg_tail(":");

		for (i = 0; i < cur_head->numargs; i++) {
		    if (!mv[i].used) {
			msg_tail(mv[i].name);
		    }
		}

		cur_head = cur_head->next;
	    }
	}
    }

    /* Warning if none of the module variables were set in the module 
     * itself */
    if (usage_mod_var_unset && !mod_any_set) {
	cmp_error_count = 0;
	modcmp_error_head(name,mvl_head,"not set in the module");
    }

}

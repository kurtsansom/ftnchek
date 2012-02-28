#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define DTYPE				/* own DTYPE_SHARED variables */
#include "ftnchek.h"
#include "symtab.h"
#include "symspace.h"
#include "dtypes.h"

/* Looks for a derived type with matching name and returns its index from
 * Dtype_table
 */
int find_Dtype(Token *t){
  int i;
  char *name = hashtab[t->value.integer].name;

  /*
  if (dtype_table_top == MIN_DTYPE_ID) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
                 "No derived type definitions have been created");
  }
  */

  /* start from top which is MIN_DTYPE_ID so latest masking derived type is returned */
  for (i = dtype_table_top - 1; i >= MIN_DTYPE_ID; i++) {
    if (strcmp(name, dtype_table[i]->name) == 0) {
      return i;
    }
  }
  return -1;  /* not found */
}


/* Installs a derived type in symbol table.  Components will be parsed
   later and then put into definition by get_dtype_components.
 */
void def_dtype(Token *id)
{
  int h = id->value.integer;
  Lsymtab *symt = hashtab[h].loc_symtab;

  if (dtype_table_top == MAX_DTYPES) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
                 "Reached max limit for derived types");
    return;
  }

  if (symt != NULL && in_curr_scope(symt)){
  	syntax_error(id->line_num,id->col_num,"Type name is in use");
  }
  else { /* install name in loc symtab, masking if in outer scope */
      /* data type is index in the dtype_table */
    symt = install_local(h,dtype_table_top,class_DTYPE);
  }
}

/* Scans the current scope (inside derived type definition) to get
   the components of the type and store them in the type table entry.
*/
void process_dtype_components(char *name)
{
  int i, j, h;
  int num_components = loc_symtab_top - curr_scope_bottom;
  Lsymtab *symt = NULL;
  DtypeComponent *curr; 
  Dtype *dtype;

  if ( (dtype = (Dtype *)malloc(sizeof(Dtype))) == (Dtype *)NULL) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
        "Cannot alloc space for derived type definition");
    return;
  }

  if ( (dtype->components =
	(DtypeComponent *)malloc(num_components*sizeof(DtypeComponent)))
          == (DtypeComponent *)NULL) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
        "Cannot alloc space for derived type component");
    return;
  }

  /* Find the type in local symbol table */
  h = hash_lookup(name);
  symt = hashtab[h].loc_symtab;
  /* If name looked up is not a derived type, it is a component with
   * same name as type.  In that case fetch the masked entry
   */
  if( storage_class_of(symt->type) != class_DTYPE ) {
    symt = symt->mask;
  }

  /* store information about the derived type definition */
  dtype->name = new_global_string(symt->name);
  dtype->num_components = num_components;
  dtype->line_declared = symt->line_declared;
  dtype->file_declared = symt->file_declared;
  dtype->private = symt->private;
  dtype->sequence = symt->sequence;

  /* store information about the components */
  curr = dtype->components;
  for (i=curr_scope_bottom; i<loc_symtab_top; i++) {
    curr->name = new_global_string(loc_symtab[i].name);
    curr->info = loc_symtab[i].info;
    curr->size = loc_symtab[i].size;
    curr->type = loc_symtab[i].type;
    curr->array = loc_symtab[i].array_var;
    curr->pointer = loc_symtab[i].pointer;
    curr->private = loc_symtab[i].private;
    curr++;
  }
  dtype_table[dtype_table_top++] = dtype;
}

/* Routine to return the standard type name of elementary types or
   the name of derived types.
 */
char *type_name(type_t t)
{
   if(t < MIN_DTYPE_ID) {		/* elementary type */
      return elementary_type_name[t];
   }
   else {
      return dtype_table[t]->name;	/* derived type */
   }
}

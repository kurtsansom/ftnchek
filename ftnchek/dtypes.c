#include <stdio.h>
#include <stdlib.h>
#include "ftnchek.h"
#include "symtab.h"
#include "symspace.h"
#include "dtypes.h"

PRIVATE
int dtype_table_top = MIN_DTYPE_ID; 	/* available index in Dtype_table */


/* 0 <= dtype_start < loc_symtab_top
 * dtype_start is the index where local symbol entry for derived type
 * is entered.
 * This function needs to called when END TYPE is encountered */
void new_Dtype(int dtype_start)
{
  if (dtype_table_top >= MAX_DTYPES) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
                 "Reached max limit for derived types");
    return;
  }

  int i;
  /* subtract one to ignore symbol table entry for derived type itself */
  int nc = loc_symtab_top - dtype_start - 1;
  Dtype *new_dtype = (Dtype *) malloc(sizeof(Dtype));
  DtypeComponent *new_components = (DtypeComponent *) calloc(nc, sizeof(DtypeComponent)); 
  if ((new_dtype == (Dtype *) NULL) ||
      (new_components == (DtypeComponent *) NULL)) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
                 "Cannot alloc space for derived type definition");
    return;
  }

  /* Collect information about derived type itself */
  new_dtype->name = new_local_string(loc_symtab[dtype_start].name);
  new_dtype->components = new_components;
  new_dtype->line_declared = loc_symtab[dtype_start].line_declared;
  new_dtype->file_declared = loc_symtab[dtype_start].file_declared;

  /* Collect information about components */
  /* Entries for components will fill the local symbol table up to
   * loc_symtab_top - 1 */
  for (i = dtype_start + 1; i < loc_symtab_top; i++) {
  }

  dtype_table[dtype_table_top++] = new_dtype;
}

/* Looks for a derived type with matching name and returns its index from
 * Dtype_table
 */
int find_Dtype(char *name){
  if (dtype_table_top == 0) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
                 "No derived type definitions have been created");
  }

  int i;

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
}

/* Scans the current scope (inside derived type definition) to get
   the components of the type and store them in the type table entry.
*/
void get_dtype_components(char *name)
{
}


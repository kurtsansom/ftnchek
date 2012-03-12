#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define DTYPE				/* own DTYPE_SHARED variables */
#include "ftnchek.h"
#include "symtab.h"
#include "symspace.h"
#include "dtypes.h"
#include "tokdefs.h"

PRIVATE int duplicate_dtype( int type_id );  /* check if derived type definition already
    exists in dtype_table */
PRIVATE int is_same_components(int s_id,int t_id,
       const DtypeComponent *source, const DtypeComponent *target, int num_components);



/* Looks for a derived type with matching name and returns its index from
 * Dtype_table.  If not found, it is a forward reference so a new type
 * is defined and a symbol table entry is created.  This may be an error
 * but we can't tell here.
 */
int find_dtype(Token *t, int in_dtype_def){
  int h = t->value.integer;
  Lsymtab *symt = hashtab[h].loc_symtab;

  /* If type not seen before, then this is a forward reference.  This
     may be an error but for now we give it a type. */
  if( (symt = hashtab[h].loc_symtab) == NULL ) {
    symt = def_dtype(t,/*access_spec=*/0,/*dtype_def=*/FALSE);

    /* If forward ref occurred inside type definition, it needs
       to move outside scope of type.
     */
    if( in_dtype_def ) {
      move_outside_scope(symt);
      symt = hashtab[h].loc_symtab; /* get changed value */
    }
  }

  if( storage_class_of(symt->type) != class_DTYPE ){
    if(in_dtype_def)
      symt = symt->mask;	/* case of component with same name as type */
      /* If masked entry still is not a data type then this is an error */
    if( symt == NULL ||
	(storage_class_of(symt->type) != class_DTYPE) ){
      symt = hashtab[h].loc_symtab;
      syntax_error(t->line_num,t->col_num,symt->name);
      msg_tail("is not a data type");
      return 0;  /* not found => undeclared type */
    }
  }

  return datatype_of(symt->type);

}


/* Installs a derived type in symbol table.  Components will be parsed
   later and then put into definition by get_dtype_components.  This
   is usually called when parsing a TYPE typename statement (dtype_def == TRUE)
   but may also be called when parsing a TYPE(typename)::var declaration
   (dtype_def == FALSE) which is a forward reference that we take to imply
   creating a new type.  If this is a type defn statement, there may be
   an PRIVATE or PUBLIC access_spec; otherwise access_spec=0.
 */
Lsymtab * def_dtype(Token *id, int access_spec, int dtype_def)
{
  int type_id;
  Dtype *dtype;
  int h = id->value.integer;
  Lsymtab *symt = hashtab[h].loc_symtab;

  if ( symt != NULL && in_curr_scope(symt) ) {
    /* In case of second defn of same name, issue warning but do not
     * enforce.  The new defn will overwrite the old one: so be it.
     * Too much trouble to enforce.  */

    if(dtype_def && symt->line_declared != NO_LINE_NUM) { /* not a forward reference */
      syntax_error(id->line_num,id->col_num,"Type name is in use");
    }
  }
  else { /* install name in loc symtab, masking if in outer scope */

#ifdef DEBUG_DTYPE
  if(debug_latest) {
    if (dtype_def) {
      fprintf(list_fd,"\nInstalling new derived type definition %s",
              hashtab[h].name); 
    }
    else {
      fprintf(list_fd,"\nInstalling forward reference to derived type definition %s",
              hashtab[h].name); 

    }
  }
#endif 


    if (dtype_table_top == MAX_DTYPES) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Reached max limit for derived types");
      return NULL;		/* NOTREACHED */
    }

    if ( (dtype = (Dtype *)malloc(sizeof(Dtype))) == (Dtype *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for derived type definition");
      return NULL;		/* NOTREACHED */
    }

  /* create node to store preliminary information about the derived
   * type definition */

    type_id = dtype_table_top++; /* take next available index */
    symt = install_local(h,type_id,class_DTYPE);
    /* If this is a definition, record where.  If forward ref, leave undefined */
    symt->line_declared = dtype->line_declared = (dtype_def?id->line_num:NO_LINE_NUM);
    symt->file_declared = dtype->file_declared = inctable_index;

    dtype->name = new_global_string(id->src_text);
    dtype->num_components = 0;		/* no components defined yet */
    dtype->components = NULL;
    /* Zero out flags that will be set later */
    dtype->private_components = 0;
    dtype->sequence = 0;

    switch( access_spec ) {	/* access spec declared in TYPE stmt */
    case tok_PUBLIC:
      symt->public = dtype->public = TRUE;
      symt->private = dtype->private = FALSE;
      break;
    case tok_PRIVATE:
      symt->private = dtype->private = TRUE;
      symt->public = dtype->public = FALSE;
      break;
    default:				/* no spec declared */
      symt->private = dtype->private = FALSE;
      symt->public = dtype->public = FALSE;
      break;
    }

    /* Store ptr to the node in type table */
    dtype_table[type_id] = dtype;
  }

#ifdef DEBUG_DTYPE
  if(debug_latest) {
    fprintf(list_fd,"\nDEF_DTYPE: Number of derived type definitions : %d",
            dtype_table_top - MIN_DTYPE_ID);
  }
#endif 

  return symt;
}

/* This routine processes PRIVATE statement within body of a derived
   type definition, where it signifies that the components of the type
   are private.
 */
void privatize_components(const char *name)
{
  int h = hash_lookup(name);
  Lsymtab *symt = hashtab[h].loc_symtab;
  symt->private_components = TRUE;
}

/* Scans the current scope (inside derived type definition) to get
   the components of the type and store them in the type table entry.
*/
void process_dtype_components(const char *name)
{
  int i, h, d;
  int type_id;
  int num_components;
  Lsymtab *symt;
  Dtype *dtype;
  DtypeComponent *curr; 

  /* Find the type in local symbol table */
  h = hash_lookup(name);
  symt = hashtab[h].loc_symtab;

  /* If name looked up is not a derived type, it is a component with
   * same name as type.  In that case fetch the masked entry
   */
  if( storage_class_of(symt->type) != class_DTYPE ) {
    symt = symt->mask;
      /* If masked entry still is not a data type then this is an error */
    if( symt == NULL ||
	(storage_class_of(symt->type) != class_DTYPE) ){
      /* error was reported earlier as "type name is in use" */
      return;  /* not found => do not process */
    }
  }

  /* Get information about the derived type definition from
     its symtab entry, which may have been updated, e.g. by
     PRIVATE statement.
   */
  num_components = loc_symtab_top - curr_scope_bottom;
  type_id = datatype_of(symt->type);
  dtype = dtype_table[type_id];
  dtype->num_components = num_components;
  dtype->private_components = symt->private_components;
  dtype->sequence = symt->sequence;

  if ( num_components > 0 && (dtype->components =
	(DtypeComponent *)malloc(num_components*sizeof(DtypeComponent)))
          == (DtypeComponent *)NULL) {
    oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
        "Cannot alloc space for derived type components");
    return;
  } /* else (empty defn) dtype->components remains NULL */

  /* Get information about the components from contents of type def scope.
   */
  curr = dtype->components;
  for (i=curr_scope_bottom; i<loc_symtab_top; i++) {
    curr->name = new_global_string(loc_symtab[i].name);
    curr->type = get_type(&loc_symtab[i]);
    curr->size = loc_symtab[i].size;
    curr->pointer = loc_symtab[i].pointer;
    /* Component is private if PRIVATE statement appears and component
       is not explicitly declared PUBLIC; otherwise it is public.
     */
    curr->private = symt->private_components && !loc_symtab[i].public;

    curr->array = loc_symtab[i].array_var;
    curr->array_dim = loc_symtab[i].info.array_dim;
    curr++;
  }
			    /* see if it is a duplicate derived type */
  if ( (d = duplicate_dtype(type_id)) != -1 ) {
    /* This is normal if two independent prog units declare type identically */
    /* Change type in local symbol table entry to existing definition */
    symt->type = type_pack(class_DTYPE,d);
    if(dtype->components)
      free(dtype->components);		/* clean up unneeded space */
    free(dtype);
    if( type_id == dtype_table_top+1 ) /* took latest avail type */
    	--dtype_table_top;  /* recover the slot, otherwise it's wasted */
  }
  else {				/* it is new; accept defn */

  }

#ifdef DEBUG_DTYPE
  if(debug_latest) {
    fprintf(list_fd,"\nPROCESS_DTYPE_COMPONENTS: Number of derived type definitions : %d",
            dtype_table_top - MIN_DTYPE_ID);
    if (d != 1) {
      fprintf(list_fd,"\nNumber of components for %s : %d",
              dtype_table[type_id]->name, num_components);
    }
  }
#endif 

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

/* Returns index of duplicate dtype in dtype_table if derived type
 * definition already exists.  This is called for a derived type whose
 * definition has been processed and an entry in dtype_table created,
 * located at the top of the table.
 *
 * "Data entities in different scoping units also have the same type if
 * they are declared with reference to different derived-type
 * definitions that have the same name, have the SEQUENCE property,
 * and have components that do not have PRIVATE accessibility and
 * agree in order, name, and attributes.  Otherwise, they are of
 * different derived types." (J3/97-007R2, section 4.4.2)
 *
 */

PRIVATE int duplicate_dtype( int type_id )
{
  int i;
  Dtype *source, *target;
  int sequence_warning_given=FALSE;	/* to limit warnings to once */

  source = dtype_table[type_id];

  /* check each derived type definition */
  for (i = MIN_DTYPE_ID; i < dtype_table_top; i++) {
    if( i == type_id )			/* skip self */
      continue;

    target = dtype_table[i];
    if ( strcmp(source->name, target->name) != 0 )
      continue;
	/* test below may not be necessary due to mutual exclusivity of
	 * SEQUENCE and PRIVATE attributes */
    if( source->private || target->private ) 
      continue;
    if( source->num_components != target->num_components )
      continue;
      /* now test agreement of components */
    if( is_same_components(dtype_table_top,i,
			   source->components,
			   target->components,
			   source->num_components) ) {
      if (source->sequence && target->sequence) {
#ifdef DEBUG_DTYPE
  if(debug_latest) {
    fprintf(list_fd,"\nDuplicate derived type at index : %d", i);
  }
#endif 
	return i;			/* all matches up */
      }
      else {				/* all but sequence attr */
	if (!sequence_warning_given) {
	  warning(source->line_declared, NO_COL_NUM,
		  "Derived type");
	  msg_tail(source->name);
	  msg_tail("agrees with another definition except for SEQUENCE attribute");
	  sequence_warning_given = TRUE;
	}
	continue;			/* fails due to nonsequence */
      }
    }
  }
  return -1; /* not duplicate */
}

/* Returns TRUE if components are same
 * for components: order, name, attributes 
 */
PRIVATE
int is_same_components(int s_id,int t_id,
       const DtypeComponent *source, const DtypeComponent *target,
       int num_components)
{
  int i;

  for (i = 0; i < num_components; i++) {

    if ( strcmp(source[i].name, target[i].name) != 0 ) /* names match */
      return FALSE;
    if ( source[i].pointer != target[i].pointer ) /* attrs match */
      return FALSE;
    if ( source[i].private || target[i].private ) /* no private components */
      return FALSE;
    if ( (source[i].array != target[i].array) || /* arrayness match */
	 (source[i].array && (source[i].array_dim != target[i].array_dim)) )
      return FALSE;
      /* types must match except if pointers to own type */
    if ( (source[i].type != target[i].type) &&
	 ( !(source[i].pointer &&
	     (datatype_of(source[i].type) == s_id &&
	      datatype_of(target[i].type) == t_id)) ) )
      return FALSE;
  }

  return TRUE;
}

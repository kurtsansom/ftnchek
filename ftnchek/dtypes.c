#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define DTYPE				/* own DTYPE_SHARED variables */
#include "ftnchek.h"
#include "symtab.h"
#include "symspace.h"
#include "symutils.h"
#include "dtypes.h"
#include "tokdefs.h"

PRIVATE int duplicate_dtype( int type_id );  /* check if derived type definition already
    exists in dtype_table */
PRIVATE int is_same_components(int s_id,int t_id,
       const DtypeComponent *source, const DtypeComponent *target, int num_components);
PRIVATE void ref_component_list(Token *comp, Token *result, Lsymtab *base, int d, int s, int lvalue);

PRIVATE void make_error_dtype_token( Token *result );
PRIVATE DtypeComponent *find_component(int d, const char *s);
PRIVATE void replace_type(int dup, int prev);

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
    dtype->filename = current_filename;

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
  int is_fwd_ref;			/* watch for forward refs */
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
  if (num_components == 0) {
    warning(symt->line_declared, NO_COL_NUM,
            "No components in derived type definition");
  }
  type_id = datatype_of(symt->type);
  dtype = dtype_table[type_id];
  dtype->num_components = num_components;
  dtype->private_components = symt->private_components;
  dtype->sequence = symt->sequence;
  is_fwd_ref = (dtype->line_declared == NO_LINE_NUM);
  dtype->line_declared = symt->line_declared;

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
    /* This is normal if two independent prog units declare type
     * identically and include SEQUENCE attribute.
     */

    symt->type = type_pack(class_DTYPE,d);

    if(is_fwd_ref)
      replace_type(type_id,d);	/* change refs to this type to pre-existing number  */

    if(dtype->components)
      free(dtype->components);		/* clean up unneeded space */
    free(dtype);
    if( type_id == dtype_table_top-1 ) /* took latest avail type */
    	--dtype_table_top;  /* recover the slot */
    else
        dtype_table[type_id] = NULL;       /* otherwise it's wasted, mark it as a hole */
  }
  else {				/* it is new; accept defn */

  }

#ifdef DEBUG_DTYPE
  if(debug_latest) {
    fprintf(list_fd,"\nPROCESS_DTYPE_COMPONENTS: Number of derived type definitions : %d",
            dtype_table_top - MIN_DTYPE_ID);
    if (d != -1 && dtype_table[type_id] != NULL) {
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
  if(is_elementary_type(t)) {		/* elementary type */
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
    if( i == type_id || dtype_table[i] == (Dtype *)NULL )	/* skip self, holes */
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

/* Checks if a derived type definition stored in dtype_table['d'] has a
 * component with the name 's'.  If found then it returns ptr to the
 * component else it returns NULL.
 */
PRIVATE
DtypeComponent *find_component(int d, const char *s)
{
  int i;
  int n = dtype_table[d]->num_components;
  DtypeComponent *comp = dtype_table[d]->components;

  for (i = 0; i < n; i++ ) {
    if ( strcmp(s, comp->name) == 0 )
      return comp; /* found component of same name */
    comp++;
  }

  return NULL;
}

/* Takes an arranged list of components and recursively checks if a
 * component which is an elementary type exists in previous component
 * which was a derived type. Upon reaching the last component, its
 * type is transferred to the 'result' argument.
 *
 * Note : Argument 'd' is type number of previous component.
 * If comp != NULL, d must be a derived type.
 */
PRIVATE void ref_component_list(Token *comp, Token *result, Lsymtab *base, int d, int size, int lvalue)
{
  DtypeComponent *comp_dtype;


  if(comp == NULL) {			/* end of list */
    result->TOK_type = type_pack(class_VAR, d);
    result->size = size;

    /* Set appropriate usage flags both in base variable symtab and in
     * token, for use if item is a subroutine argument.
     * NOTE: usage of components of a variable is not tracked: all
     * usage is imputed to the variable itself (as with arrays vs
     * array elements).
     */
    make_true(LVALUE_EXPR,result->TOK_flags);
    make_false(ID_EXPR,result->TOK_flags);
    if (lvalue) {
      base->line_set = result->line_num;
      base->set_flag = TRUE;
      base->assigned_flag = TRUE;
      make_true(SET_FLAG,result->TOK_flags);
      make_true(ASSIGNED_FLAG,result->TOK_flags);
    }
    else {
      if(!base->set_flag) {
	base->used_before_set = TRUE;
        make_true(USED_BEFORE_SET,result->TOK_flags);
      }
      base->line_used = result->line_num;
      base->used_flag = TRUE;
    }

  }
  else {			   /* recursively do next component */
    char *component_name = hashtab[comp->value.integer].name;

    /* Look up the component in the derived type */
    if ((comp_dtype=find_component(d, component_name)) == NULL) {
      make_error_dtype_token( result );
      syntax_error(comp->line_num,comp->col_num,component_name);
      msg_tail("is not a component of type");
      msg_tail(type_name(d));
    }
    else {
      /* If there is a next component, make sure this component is derived type */
      if (!is_derived_type(comp_dtype->type) && comp->next_token != NULL) {
	make_error_dtype_token( result );
	syntax_error(comp->line_num,comp->col_num,component_name);
	msg_tail("is not a derived type");
	return;
      }
    
      /* Set POINTER attribute in result to match component.  Last component
       * wins. */
      if(comp_dtype->pointer)
	make_true(POINTER_EXPR,result->TOK_flags);
      else
	make_false(POINTER_EXPR,result->TOK_flags);

      make_false(TARGET_EXPR,result->TOK_flags);	/* components cannot be targets */

      ref_component_list(comp->next_token, result, base, comp_dtype->type, comp_dtype->size, lvalue);
    }
  }
}

/* Routine to follow a dtype component reference out to the end
   to determine its type.  The result is updated to be suitable
   for use as a primary in an expression.
*/
void ref_component(Token *comp, Token *result, int lvalue)
{
  int h, d, s;
  Token *curr;
  Lsymtab *symt;

  curr = reverse_tokenlist(comp->next_token); /* Put list into left-right order */

#ifdef DEBUG_DTYPE
  if(debug_latest) {
    Token *item;
    fprintf(list_fd,"\nComponents: ");
    for(item=curr; item != NULL; item = item->next_token)
      fprintf(list_fd,"%s ",hashtab[item->value.integer].name);
    fprintf(list_fd,"\n");
  }
#endif
  h = curr->value.integer;
  symt = hashtab[h].loc_symtab;

  d = get_type(symt);
  s = symt->size;

  if( storage_class_of(symt->type) != class_VAR ) { /* base is not a variable?? */
    make_error_dtype_token( result );
    syntax_error(curr->line_num,curr->col_num,symt->name);
    msg_tail("is not a variable");
    return;
  }

  if (!is_derived_type(d)) {		/* base not a derived type?? */
    make_error_dtype_token( result );
    syntax_error(curr->line_num,curr->col_num,symt->name);
    msg_tail("is not a derived type");
    return;
  }

  /*  Here next_token is always non-NULL since ref_component is called
   *  only for base%component[%component...] references. */
  ref_component_list(curr->next_token, result, symt, d, s, lvalue);
}

PRIVATE
void make_error_dtype_token( Token *result )
{
    result->TOK_type = type_pack(class_VAR, type_ERROR);
    result->size = size_DEFAULT;
    result->TOK_flags = 0;
}

/* Routine to fix references to a type that went into symbol table due
   to a forward ref, using newly assigned number, which then turned
   out to be a duplicate type.  Strictly speaking, this should repeat,
   finding types now recognized as duplicates, until no changes are
   made.  That is for a future enhancement.
 */
PRIVATE void replace_type(int dup, int prev)
{
  int i;
  for(i=0; i<loc_symtab_top; i++) {
    if( storage_class_of(loc_symtab[i].type) == class_DTYPE ) {
      int d = get_type(&loc_symtab[i]);
      Dtype *dtype = dtype_table[d];
      int n = dtype->num_components;
      DtypeComponent *comp = dtype->components;
      int c;
      for (c = 0; c < n; c++ ) {
	if(comp[c].type == dup)
	  comp[c].type = prev;
      }
    }
  }
}

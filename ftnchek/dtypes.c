/*

Copyright (c) 2012 by Robert K. Moniot.

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
PRIVATE DtypeComponent *last_component(Token *comp_token);

PRIVATE void make_error_dtype_token( Token *result );
PRIVATE DtypeComponent *find_component(int d, const char *s);
PRIVATE void replace_type(int dup, int prev);
PRIVATE void set_defn_context(Lsymtab *symt, Dtype *dtype, int tok_line_num);

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
    symt = def_dtype(h,t->line_num,t->col_num,/*access_spec=*/0,/*dtype_def=*/FALSE);

    /* If forward ref occurred inside type definition, it needs
       to move outside scope of type.
     */
    if( in_dtype_def ) {
      move_outside_scope(symt);
      symt = hashtab[h].loc_symtab; /* get changed value */
      /* update pointer in dtype_table */
      dtype_table[datatype_of(symt->type)]->symt = symt;
    }
  }

  if( storage_class_of(symt->type) != class_DTYPE ){
    if(in_dtype_def)
      symt = symt->mask;	/* case of component with same name as type */
      /* If masked entry still is not a data type then this is an error */
    if( symt == NULL ||
	(storage_class_of(symt->type) != class_DTYPE) ){
      if (misc_warn) {
	symt = hashtab[h].loc_symtab;
	syntax_error(t->line_num,t->col_num,symt->name);
	msg_tail("is not a data type");
	return 0;  /* not found => undeclared type */
      }
    }
  }

  /* A variable of the derived type was created */
  symt->used_flag = TRUE;

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
Lsymtab * def_dtype(int h, int tok_line_num, int tok_col_num, int access_spec, int dtype_def)
{
  int type_id;
  Dtype *dtype;
  Lsymtab *symt = hashtab[h].loc_symtab;

  if ( symt != NULL && in_curr_scope(symt) ) {
    /* In case of second defn of same name, issue warning but do not
     * enforce.  The new defn will overwrite the old one: so be it.
     * Too much trouble to enforce.  */

    if( !is_derived_type(datatype_of(symt->type)) ||
       (dtype_def && symt->line_declared != NO_LINE_NUM)) { /* not a forward reference */
      if (misc_warn) {
	syntax_error(tok_line_num,tok_col_num,"Type name is in use");
      }
    }
    else if(dtype_def) {
	    /* If this is a type defn for a type defined
	       initially thru a forward reference, fill in info now to
	       say it was defined, and where.
	     */
      dtype = dtype_table[datatype_of(symt->type)];
      set_defn_context(symt, dtype, tok_line_num);
    }
  }
  else { /* install name in loc symtab, masking if in outer scope */

#ifdef DEBUG_DTYPE
if(debug_latest) {
    if (dtype_def) {
      fprintf(list_fd,"\nInstalling new derived type definition %d = %s",
              dtype_table_top,hashtab[h].name); 
    }
    else {
      fprintf(list_fd,"\nInstalling forward reference to derived type definition %d = %s",
              dtype_table_top,hashtab[h].name); 

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
    dtype->symt = symt = install_local(h,type_id,class_DTYPE);
    /* If this is a definition, record where.  If forward ref, leave undefined */
    dtype->filename = current_filename;
    dtype->name = new_global_string(hashtab[h].name);

      /* If this is a true definition, record line_declared and module
       * association if any.  If it is a forward reference, set those
       * to none, to be filled in (see above) when definition is
       * encountered.
       */
    if( dtype_def ) {
      set_defn_context(symt, dtype, tok_line_num);
    }
    else {
      dtype->module_name = NULL;
      symt->line_declared = dtype->line_declared = NO_LINE_NUM;
    }
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

  /* first pass through local symbol table to weed out non-components
     that can be installed due to erroneous conditions */
  for (i=curr_scope_bottom; i<loc_symtab_top; i++) {
    if (datatype_of(loc_symtab[i].type) == type_UNDECL) {
      move_outside_scope(&loc_symtab[i]);
    }
  }

  /* Get information about the derived type definition from
     its symtab entry, which may have been updated, e.g. by
     PRIVATE statement.
   */
  num_components = loc_symtab_top - curr_scope_bottom;
  if (type_empty && num_components == 0) {
    warning(symt->line_declared, NO_COL_NUM,
            "No components in derived type definition");
  }
  type_id = datatype_of(symt->type);
  dtype = dtype_table[type_id];
  dtype->num_components = num_components;
  dtype->private_components = symt->private_components;
  dtype->sequence = symt->sequence;
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
    curr->kind = loc_symtab[i].kind;
    curr->size = loc_symtab[i].size;
    curr->pointer = loc_symtab[i].pointer;
    /* Component is private if PRIVATE statement appears and component
       is not explicitly declared PUBLIC; otherwise it is public.
     */
    curr->private = symt->private_components && !loc_symtab[i].public;

    curr->array = loc_symtab[i].array_var;
    curr->array_dim = loc_symtab[i].array_dim;
    curr->allocatable = loc_symtab[i].allocatable;
    curr++;
  }
			    /* see if it is a duplicate derived type */
  if ( (d = duplicate_dtype(type_id)) != -1 ) {
    /* This is normal if two independent prog units declare type
     * identically and include SEQUENCE attribute.
     */
#ifdef DEBUG_DTYPE
  if(debug_latest) {
      fprintf(list_fd,"\nWithdrawing type %d",type_id);
  }
#endif
    symt->type = type_pack(class_DTYPE,d);
    symt->line_declared = dtype->line_declared;
    dtype_table[d]->symt = symt;

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
   the name of derived types.  Since derived types obtained via a module
   may be renamed, use name in symbol table, not dtype table.
 */
char *type_name(int t)
{
  if(is_elementary_type(t)) {		/* elementary type */
      return elementary_type_name[t];
   }
   else {
       Lsymtab *symt = dtype_table[t]->symt;
       if( symt != NULL && in_curr_scope(symt) )
	   return symt->name;	/* locally use possibly renamed derived type */
       else
	   return dtype_table[t]->name;	/* in wrapup: use global name */
   }
}

/* Routine used in global checking to return type name. For derived types,
 * it always returns the name in dtype table.
 */
char *global_type_name(int t)
{
  if(is_elementary_type(t)) {		/* elementary type */
      return elementary_type_name[t];
   }
   else {
       return dtype_table[t]->name;
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
	if (type_sequence && !sequence_warning_given) {
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
	 (source[i].array && array_dim_cmp(source[i].array_dim,target[i].array_dim) != 0) )
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
  DtypeComponent *comp_dtype = dtype_table[d]->components;

  for (i = 0; i < n; i++ ) {
    if ( strcmp(s, comp_dtype->name) == 0 ) {
      /* ignore private components if this is a module-defined type */
      if(comp_dtype->private) {
	/* find the type entry in local symbol table */
	int h = hash_lookup(dtype_table[d]->name);
	Lsymtab *symt = hashtab[h].loc_symtab;
	if( symt->defined_in_module )
	  return NULL;
      }
      return comp_dtype; /* found component of same name */
    }
    comp_dtype++;
  }

  return NULL;
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

/* Routine to return the last component in a reference to a datatype
   variable component.  It is OK for the token not to be for such a
   variable, in which case it simply returns the same token.  NOTE:
   this is intended only to get to line number, column number, and
   name for reporting purposes.  It does *not* check validity of
   components but just follows out the token list to the end.
 */
const Token *ultimate_component(const Token *t)
{
#if 0
  if(is_true(DTYPE_COMPONENT,t->TOK_flags)) { /* linked list is component train */
    while(t->next_token)		      /* run out to last component */
      t = t->next_token;
  }
#endif

  return t;
}

/* Routine checks if dtype_table has an entry with the same name and
 * module_name for a derived type.  Returns the found index.  Returns -1 if
 * not found */
int find_type_use_assoc(const char *name, const char *module_name)
{
  int i;

  for (i = MIN_DTYPE_ID; i < dtype_table_top; i++) {
    if (strcmp(name, dtype_table[i]->name) == 0 &&
        strcmp(module_name, dtype_table[i]->module_name) == 0) {
      return i;
    }
  }

  return -1;  /* not duplicate name */
}

/* Routine to perform some actions that need to be done in two
 * situations: (mainly) when defining a new type in a TYPE definition
 * or via a module; and (occasionally) when reaching the TYPE
 * definition of a forward-referenced type.  It sets line_declared so
 * we can tell the type was in fact defined, and if the definition is
 * in a module subprogram, it associates the module name with the type
 * for USE association.
 */

void set_defn_context(Lsymtab *symt, Dtype *dtype, int tok_line_num)
{
  Gsymtab *curr_prog_unit = hashtab[current_prog_unit_hash].glob_symtab;
#ifdef DEBUG_DTYPE
if(debug_latest) {
    fprintf(list_fd,"\nSet_defn_context(%s,%s,%d)",symt->name,dtype->name,tok_line_num);
}
#endif
  if( curr_prog_unit != NULL && /* need: see type-first-stmt.f90 */
      curr_prog_unit->type == type_pack(class_SUBPROGRAM,type_MODULE) )
    dtype->module_name = curr_prog_unit->name;

  symt->line_declared = dtype->line_declared = tok_line_num;
}

/* Returns pointer to the data type defn of the ultimate component
 * of a derived type object reference such as A%B...
 *
 * Argument: comp = token for the next component (must be non NULL)
 *
 * Operates recursively, checking as it goes to make sure that the
 * data object of which a component is sought is of a derived type.
 */

/* shared variables for determining array shape and type size */
PRIVATE array_dim_t result_dim;
PRIVATE int result_size;

PRIVATE DtypeComponent *last_component(Token *comp_token)
{
  int h, d;
  DtypeComponent *comp_dtype;
  Token *base = comp_token->left_token;	      /* e.g. A%B in A%B%C */
  Token *component = comp_token->next_token;  /* e.g. C in A%B%C */
  Token *base_id, *component_id;	      /* id tokens, e.g. A in A(1)%B */

  /* If base is an array element, we got the '(' token that is root of
   * array expression instead of variable.  Jog down one level to get
   * variable.
   */
  base_id = base;			/* get id tokens */
  component_id = component;
  if( base->tclass == '(' ) {
    base_id = base_id->left_token;
  }

  if( component->tclass == '(' ) {
      /* last component may be character type in which case may need to
       * jog down one more level for C(subscript_list)(substring_range)*/
    while (component_id->left_token != NULL)
      component_id = component_id->left_token;
  }

  if( base_id->left_token == NULL ) {	/* base variable */
    Lsymtab *symt;
    h = base_id->value.integer;	/* handle has base variable's hash index */
    symt = hashtab[h].loc_symtab;
    d = get_type(symt);
#ifdef DEBUG_DTYPE
    if(debug_latest) {
	printf("\nbase variable %s type %d=%s",symt->name,d,dtype_table[d]->name);
	fflush(stdout);
    }
#endif

					/* do some basic checking */
    if( storage_class_of(symt->type) != class_VAR ) { /* base is not a variable?? */
      syntax_error(base_id->line_num,base_id->col_num,symt->name);
      msg_tail("is not a variable");
      return NULL;
    }

    if (!is_derived_type(d)) {		/* base variable not a derived type?? */
      syntax_error(base_id->line_num,base_id->col_num,symt->name);
      msg_tail("is not a structure object");
      return NULL;
    }

    /* reference to an array */
    if( symt->array_var ) {
      /* if base is a subscripted array, get its dim info */
      result_dim = 
	base_id->array_dim = symt->array_dim; /* update the token */
      if (base->tclass == '(' ) {
	result_dim = subarray_size(base_id,base);
      }

#ifdef DEBUG_DTYPE
    if(debug_latest) {
      printf("\n dims %d size %ld",array_dims(result_dim),array_size(result_dim));
      fflush(stdout);
    }
#endif
    }
  }
  else {				/* base % component */
    DtypeComponent *base_dtype = last_component(base); /* find last component's type */
    if( base_dtype == NULL ) {
      return NULL;
    }
    d = base_dtype->type;		/* get type number */
#ifdef DEBUG_DTYPE
    if(debug_latest) {
	printf("\nbase component %s type %d=%s",
	       base_dtype->name,d,dtype_table[d]->name);
	fflush(stdout);
    }
#endif
  }
  h = component_id->value.integer;

    /* Look up the component in the derived type table entry */
  if ((comp_dtype=find_component(d, hashtab[h].name)) == NULL) {
    syntax_error(comp_token->line_num,comp_token->col_num,"Type");
    msg_tail(type_name(d));
    msg_tail("has no component named");
    msg_tail(hashtab[h].name);
    return NULL;
  }
  else {
	  /* If this is the last component, it can have character type
	     and can have a substring expression.  If also an array,
	     it can have subcript list.  The grammar cannot
	     differentiate in C(1:n) between array section and
	     substring, so it creates a subscript list.  If it is in
	     fact a substring, we need to take first (and only)
	     subscript expression as bounds.  This will be corrected
	     for C(1:n)(1:m) below.
	   */
    Token *substring_bounds;
    if( component->tclass == '(' )
      substring_bounds = component->next_token->next_token; /* correct for C(1:n) as substring */
    else 
      substring_bounds = (Token *) NULL;

    /* fetch info from derived type table */
    component_id->size = comp_dtype->size;
    component_id->TOK_type = comp_dtype->type;
    component_id->array_dim = comp_dtype->array_dim;

				/* Handle array component */
    if (comp_dtype->array) {
      /* Subscript_list will point to array indexing.  Will need
	 to jog down if component is also a substring.
       */
      Token *subscript_list = component->next_token; /* For C(1:n) as array */

      /* if no subscripting, component gets its declared dimensions */
      array_dim_t component_dim = component_id->array_dim;

	/* if component expr is a subscripted array, get its dim info */
      if (component->tclass == '(' ) {
	if (component->left_token->tclass == '(') {  /* for C(1:n)(1:m) */
	  subscript_list = component->left_token->next_token;
	  substring_bounds = component->next_token;
	}
	else {			/* C(1:n) is array ref, not substring bounds */
	  substring_bounds = (Token *) NULL;
	}
	component_dim = subarray_size(component_id,subscript_list);
      }

      /* Replace current result_dim by component dim if the latter
	 is of nonzero rank.  Catch multiple nonzero-rank components.
      */
      if (array_dims(component_dim) > 0) {
	if (array_dims(result_dim) > 0 ) {
	  if (misc_warn) {
	    syntax_error(component_id->line_num,component_id->col_num,
		"only one component may be of nonzero rank");
	  }
	}
	else {
	  result_dim = component_dim;	/* result gets component's shape */
	}
      }

    }	/* end if (comp_dtype->array) */

    /* Now deal with possible substring as last component */
    if (datatype_of(comp_dtype->type) == type_STRING && substring_bounds) {
      result_size = substring_size(component_id,substring_bounds);
    }
    else {
      /* all except substring take size of component */
      result_size = comp_dtype->size;	/* last component wins */
    }

#ifdef DEBUG_DTYPE
    if(debug_latest) {
	printf("\nlast component %s type %d=%s",
	       comp_dtype->name,datatype_of(comp_dtype->type),type_name(datatype_of(comp_dtype->type)));
	fflush(stdout);
    }
#endif
    return comp_dtype;
  }

}

/* Routine to follow a dtype component reference out to the end
   to determine its type.  The result is updated to be suitable
   for use as a primary in an expression.
*/
void ref_component(Token *comp_token, Token *result, int lvalue)
{
  DtypeComponent *comp_dtype;
  result_size = 0;
  result_dim = array_dim_info(0,0);

  comp_dtype = last_component(comp_token->left_token);

  if(comp_dtype == NULL) {			/* not found */
    make_error_dtype_token( result );
    /* error message was given by last_component */
  }
  else {
    result->TOK_type = type_pack(class_VAR, datatype_of(comp_dtype->type));
    result->size = result_size;
    result->kind = comp_dtype->kind;
    result->array_dim = result_dim;
#ifdef DEBUG_DTYPE
  if(debug_latest) {
    fprintf(list_fd,"\nResult has type %d",datatype_of(result->TOK_type));
    fprintf(list_fd," dims %d size %ld",array_dims(result_dim),array_size(result_dim));
    fprintf(list_fd,"\n");
  }
#endif
    /* Set appropriate usage flags both in base variable symtab and in
     * token, for use if item is a subroutine argument.
     * NOTE: usage of components of a variable is not tracked: all
     * usage is imputed to the variable itself (as with arrays vs
     * array elements).
     */
    make_true(LVALUE_EXPR,result->TOK_flags); /* result is assignable */
    make_true(ID_EXPR,result->TOK_flags); /* value.integer is hashtable index */
    make_true(DTYPE_COMPONENT,result->TOK_flags); /* is component of derived type var */
    if (array_dims(result->array_dim) != 0) {
	make_true(ARRAY_EXPR,result->TOK_flags);
	make_false(ARRAY_ELEMENT_EXPR,result->TOK_flags);
    }

#if 0					  /* all this is done elsewhere? */
    if (lvalue) {
      symt->line_set = result->line_num;
      symt->set_flag = TRUE;
      symt->assigned_flag = TRUE;
      make_true(SET_FLAG,result->TOK_flags);
      make_true(ASSIGNED_FLAG,result->TOK_flags);
    }
    else {
      if(!symt->set_flag) {
	symt->used_before_set = TRUE;
        make_true(USED_BEFORE_SET,result->TOK_flags);
      }
      if(!symt->used_flag) {
        symt->line_used = result->line_num;
	symt->file_used = inctable_index;
      }
      symt->used_flag = TRUE;
    }
#endif

      /* Set POINTER attribute in result to match component. */
    if(comp_dtype->pointer) {
      make_true(POINTER_EXPR,result->TOK_flags);
    }
    else {
      make_false(POINTER_EXPR,result->TOK_flags);
    }

      /* Set ALLOCATABLE attribute in result to match component. */
    if(comp_dtype->allocatable) {
      make_true(ALLOCATABLE_EXPR,result->TOK_flags);
    }
    else {
      make_false(ALLOCATABLE_EXPR,result->TOK_flags);
    }

    make_false(TARGET_EXPR,result->TOK_flags);	/* components cannot be targets */

  }
  

}

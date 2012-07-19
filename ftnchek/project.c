/*  $Id: project.c,v 1.12 2002/08/24 15:54:17 moniot Rel $

    project.c:

	Project-file and module-file I/O routines.


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

  Routines included:

	Shared routines:
	   void proj_file_out() writes data from symbol table to project file.
	   void proj_file_in() reads data from project file to symbol table.
	   void write_module_file() writes data to module file
	   void read_module_file() reads data from module file
	Private routines:
		int has_defn()	    TRUE if external has defn in current file
		int has_call()	    TRUE if external has call in current file
		int count_com_defns() Counts multiple common defns.
		void alist_out() Outputs argument lists
		void clist_out() Outputs common lists
		void arg_info_in()  Inputs argument lists
		void com_info_in()  Inputs common lists
*/

#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#define PROJECT
#include "symtab.h"
#include "symspace.h"
#include "dtypes.h"
#include <string.h>
#if HAVE_STRINGS_H
#include <strings.h>	/* we use strncasecmp */
#endif
#include "utils.h"

/* Two options, proj_trim_calls and proj_trim_common, control whether
   Ftnchek creates project files with partial or complete global
   symbol table information.  If these options are TRUE (the usual
   case), the action is: in library mode, keep only subprogram
   definitions, those external references not defined in the current
   file, and only one instance of each common block.  In non-library
   mode, keep, besides the above, one call of a given routine from
   each program unit, and all common block declarations.  Setting
   proj_trim_calls to FALSE causes all definitions and calls to be
   kept.  Setting proj_trim_common to FALSE causes all common block
   instances to be kept.  (In this case the action is the same whether
   or not in library mode.)  These options formerly were controlled by
   compile-time define PROJ_KEEPALL.  They are useful mainly for
   debugging ftnchek and for using the project files for purposes
   other than ftnchek.  */

#define PROJFILE_COOKIE "FTNCHEK_" /* first part of magic cookie */
#define MODULEFILE_COOKIE "FTNCHEK_MODULE"

PROTO(PRIVATE char *make_module_filename,(const char *module_name));
PROTO(PRIVATE int count_com_defns,( ComListHeader *clist ));
PROTO(PRIVATE char *getstrn,(char s[], int n, FILE *fd));
PROTO(PRIVATE int for_mod,( ArgListHeader *alist ));
PROTO(PRIVATE int has_call,( ArgListHeader *alist ));
PROTO(PRIVATE int has_defn,( ArgListHeader *alist ));
PROTO(PRIVATE int nil,( void ));
PROTO(PRIVATE void alist_out,( Gsymtab *gsymt, FILE *fd, int mode ));
PROTO(PRIVATE void arg_info_in,( FILE *fd, char *filename, int is_defn, int module, Token *item_list, int only_list_mode));
PROTO(PRIVATE int find_types, (Lsymtab *sym_list[]));
PROTO(PRIVATE int find_variables,(Lsymtab *sym_list[]));
PROTO(PRIVATE void mod_type_out,(Lsymtab *symt,FILE *fd));
PROTO(PRIVATE void mod_var_out,(Lsymtab *symt,FILE *fd));
PROTO(PRIVATE int find_prog_units,(Gsymtab *sym_list[], int (*has_x)(ArgListHeader *alist),int module_mode));
PROTO(PRIVATE int trim_calls,(int orig_num, Gsymtab *sym_list[]));
PROTO(PRIVATE void prog_unit_out,(Gsymtab* gsymt, FILE *fd, int mode));
PROTO(PRIVATE void find_comblocks, (Gsymtab *sym_list[], Gsymtab *module, int *blocks, int *defns ));
PROTO(PRIVATE void comblocks_out, (FILE *fd, Gsymtab *sym_list[], Gsymtab *module, int numblocks, int numdefns));
PROTO(PRIVATE void clist_out,( Gsymtab *gsymt, ComListHeader *c, FILE *fd ));
PROTO(PRIVATE void com_info_in,( FILE *fd, const char *filename, const char *modulename, Token *item_list, int only_list_mode ));
PROTO(PRIVATE void mod_type_in,(FILE *fd, const char *module_name, const char *filename, Token * item_list, int only_list_mode));
PROTO(PRIVATE void mod_var_in,(FILE *fd, const char *filename, Token *item_list, int only_list_mode, ModVar *modvar, int in_module));
PROTO(PRIVATE ComListHeader *comblock_used_by,(ComListHeader *clist, Gsymtab *module));
PROTO(PRIVATE void initialize_typemap, (VOID));
PROTO(PRIVATE int map_type, (int t_in));
PROTO(int name_in_only_list, (const char *name, const Token *tlist, char **local_name));
PROTO(void print_token_list, (const Token *tlist));


PRIVATE int
for_mod(ArgListHeader *alist)		/* Returns TRUE if list has defns */
{
  /* use this routine in module mode to test if item has defn, ignoring
     file where it was defined.
   */
  while( alist != NULL ) {
    if(alist->is_defn)
      return TRUE;
    alist = alist->next;
  }
  return FALSE;
}

PRIVATE int
#if HAVE_STDC
has_defn(ArgListHeader *alist)			/* Returns TRUE if list has defns */
#else /* K&R style */
has_defn(alist)			/* Returns TRUE if list has defns */
   ArgListHeader *alist;
#endif /* HAVE_STDC */
{
  while( alist != NULL && alist->topfile == top_filename ) {
    if(alist->is_defn)
      return TRUE;
    alist = alist->next;
  }
  return FALSE;
}


PRIVATE int
#if HAVE_STDC
has_call(ArgListHeader *alist)		/* Returns TRUE if list has calls or defns  */
#else /* K&R style */
has_call(alist)		/* Returns TRUE if list has calls or defns  */
   ArgListHeader *alist;
#endif /* HAVE_STDC */
{
  while( alist != NULL && alist->topfile == top_filename) {
    if( alist->is_call || alist->actual_arg )
	return TRUE;
    alist = alist->next;
  }
  return FALSE;
}

PRIVATE int
#if HAVE_STDC
count_com_defns(ComListHeader *clist)		/* Returns number of common decls in list  */
#else /* K&R style */
count_com_defns(clist)		/* Returns number of common decls in list  */
   ComListHeader *clist;
#endif /* HAVE_STDC */
{
  int count=0;
  while( clist != NULL && clist->topfile == top_filename ) {
    ++count;
    clist = clist->next;
  }
  return count;
}


	/* proj_file_out: writes data from symbol table to project file. */

#define WRITE_STR(LEADER,S)	(void)(fprintf(fd,LEADER), fprintf(fd," %s",S))
#define WRITE_ARG(LEADER,S)	(void)(fprintf(fd,LEADER), fprintf(fd," %s",S))
#define WRITE_NUM(LEADER,NUM)	(void)(fprintf(fd,LEADER), fprintf(fd," %ld",(long)(NUM)))
#define NEXTLINE		(void)fprintf(fd,"\n")

/* macros for prog_unit_out mode */
#define MODE_PROJ_CALLS 0	/* project file, calls */
#define MODE_PROJ_DEFNS 1	/* project file, defns */
#define MODE_MODULE 2		/* module file, defns */

/* Routine to create filename of form [path/]module_name.fkm, allocating
   permanent space for it.  Module name is lowercased in filename.
 */

PRIVATE char *
make_module_filename(const char *module_name)
{
  char *path_end;
  char *module_filename;
  int path_len;

			/* source file path component is prepended if present */
  if( (path_end = strrchr(current_filename,PATH_END_CHAR)) != (char *)NULL) {
    path_len = path_end - current_filename + 1; /* include the char itself */
  }
  else {
    path_len = 0;
  }
  module_filename =  malloc(path_len + strlen(module_name)
				 + strlen(DEF_MODULE_EXTENSION) + 1);
  if(path_len > 0)
    (void)strncpy(module_filename,current_filename,path_len);
  (void)strcpy(module_filename+path_len,module_name);
  (void)strtolower(module_filename+path_len); /* filename is lowercased version of name */
  (void)strcat(module_filename,DEF_MODULE_EXTENSION);

#ifdef DEBUG_PROJECT
  if(debug_latest) {
    fprintf(list_fd,"\nModule %s: ",module_name);
    fprintf(list_fd,"File %s ",current_filename);
    fprintf(list_fd,"Module filename is %s",module_filename);
  }
#endif

  return module_filename;
}

void
write_module_file(int h)
{
  FILE *fd;
  char *module_filename = make_module_filename(hashtab[h].name);

  if( (fd = fopen(module_filename,"w")) == (FILE *)NULL ) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nERROR: Cannot open module file %s for writing\n",module_filename);
    return;
  }

  if(do_list) {
    (void)fflush(list_fd);
    (void)fprintf(list_fd,"\n\nWriting module file %s\n",module_filename);
  }

  WRITE_STR(MODULEFILE_COOKIE,MODULE_VERSION);
  NEXTLINE;

  WRITE_STR("module",hashtab[h].name); /* record module name & its filename */
  WRITE_STR(" file",top_filename);
  NEXTLINE;

			/* Write derived type definitions */
  {
    Lsymtab *lsym_list[LOCSYMTABSZ]; /* temp. list of local symtab entries to print */
    int i,numtypes;
    numtypes = find_types(lsym_list);
    WRITE_NUM(" types",numtypes);
    NEXTLINE;
    for(i=0; i<numtypes; i++) {
      mod_type_out(lsym_list[i],fd);
    }
    (void)fprintf(fd," end\n");
  }

  NEXTLINE;
			/* Write local variables & parameters */
  {
    Lsymtab *lsym_list[LOCSYMTABSZ]; /* temp. list of local symtab entries to print */
    int i,numvars;
    numvars = find_variables(lsym_list);
    WRITE_NUM(" locals",numvars);
    NEXTLINE;
    for(i=0; i<numvars; i++) {
      mod_var_out(lsym_list[i],fd);
    }
    (void)fprintf(fd," end\n");
  }

  NEXTLINE;

			/* Write interface defns of module subprograms */
  {
    Gsymtab *gsym_list[GLOBSYMTABSZ]; /* temp. list of global symtab entries to print */
    int i,numdefns;
    numdefns = find_prog_units(gsym_list,for_mod,/*module_mode=*/TRUE);
    WRITE_NUM(" entries",numdefns);
    NEXTLINE;
    for(i=0; i<numdefns; i++) {
      prog_unit_out(gsym_list[i],fd,MODE_MODULE);
    }
    NEXTLINE;
  }

  /* Write the common block section of module file */
  {
    Gsymtab *gsym_list[GLOBSYMTABSZ];
    Gsymtab *module = hashtab[h].glob_symtab;
    int numblocks,numdefns;
    find_comblocks(gsym_list,module,&numblocks,&numdefns);
    comblocks_out(fd,gsym_list,module,numblocks,numdefns);
  }

  fclose(fd);			/* Done. */
}

void
#if HAVE_STDC
proj_file_out(FILE *fd)
#else /* K&R style */
proj_file_out(fd)
     FILE *fd;
#endif /* HAVE_STDC */
{
  Gsymtab *sym_list[GLOBSYMTABSZ]; /* temp. list of symtab entries to print */

  if(fd == NULL)
    return;

  WRITE_STR(PROJFILE_COOKIE,PROJECT_VERSION); /* magic cookie */
  NEXTLINE;

  WRITE_STR("file",top_filename);
  NEXTLINE;

  {	/* Make list of subprograms defined or referenced in this file */
      int i,numdefns,numcalls;

		/* List all subprogram defns, then all calls */

      numdefns = find_prog_units(sym_list,has_defn,/*module_mode=*/FALSE);

      WRITE_NUM(" entries",numdefns);
      NEXTLINE;
      for(i=0; i<numdefns; i++) {
	prog_unit_out(sym_list[i],fd,MODE_PROJ_DEFNS);
      }
      NEXTLINE;

      numcalls = find_prog_units(sym_list,has_call,/*module_mode=*/FALSE);

      if(proj_trim_calls)
	numcalls = trim_calls(numcalls,sym_list);

      WRITE_NUM(" externals",numcalls);
      NEXTLINE;
      for(i=0; i<numcalls; i++) {
	prog_unit_out(sym_list[i],fd,MODE_PROJ_CALLS);
      }
      NEXTLINE;

  }

  /* Write the common block section of project file */
  {
    int numblocks,numdefns;
    find_comblocks(sym_list,(Gsymtab *)NULL,&numblocks,&numdefns);
    comblocks_out(fd,sym_list,(Gsymtab *)NULL,numblocks,numdefns);
  }
}


PRIVATE int
find_types(Lsymtab *sym_list[])
{
  int i, numtypes;
  numtypes=0;
  for(i=curr_scope_bottom; i<loc_symtab_top; i++) {
    if( storage_class_of(loc_symtab[i].type) == class_DTYPE &&
	!loc_symtab[i].private ) { /* omit private types */
      sym_list[numtypes++] = &loc_symtab[i];
    }
  }
  return numtypes;
}


PRIVATE int
find_variables(Lsymtab *sym_list[])
{
  int i, numvars;
  numvars=0;
  for(i=curr_scope_bottom; i<loc_symtab_top; i++) {
    if( storage_class_of(loc_symtab[i].type) == class_VAR &&
	datatype_of(loc_symtab[i].type) != type_MODULE && /* skip module's own entry */
	!loc_symtab[i].private ) { /* for module: omit private variables */
      sym_list[numvars++] = &loc_symtab[i];
    }
  }
  return numvars;
}


PRIVATE void
mod_type_out(Lsymtab *lsymt, FILE *fd)
{
  int type_index;
  Dtype *dtype;
  DtypeComponent *curr;
  int i, num_components;

  WRITE_STR(" dtype",lsymt->name);
  WRITE_STR(" home",lsymt->home_unit);
  WRITE_NUM(" type",(type_index=get_type(lsymt)));
  WRITE_STR(" module",dtype_table[type_index]->module_name);
  WRITE_NUM(" size",lsymt->size);
  (void)fprintf(fd," flags %d %d %d %d %d %d %d %d",
		lsymt->public,
		lsymt->private,		/* never present in module file */
		lsymt->private_components,
		lsymt->sequence,
		0,0,0,0);		/* for future use */
  NEXTLINE;

  dtype = dtype_table[type_index];	/* find the type definition */
  num_components = dtype->num_components;

  WRITE_NUM(" components",num_components);
  NEXTLINE;

  curr = dtype->components;
  for(i=0; i<num_components; i++) {
    WRITE_STR(" component",curr[i].name);
    WRITE_NUM(" type",curr[i].type);
    WRITE_NUM(" kind",curr[i].kind);
    WRITE_NUM(" size",curr[i].size);
    (void)fprintf(fd," flags %d %d %d %d %d %d %d %d",
		  curr[i].array,
		  curr[i].pointer,
		  curr[i].private,
		  0,0,0,0,0);		/* for future use */
    if(curr[i].array) {
      NEXTLINE;
      WRITE_NUM(" dims",array_dims(curr[i].array_dim));
      WRITE_NUM(" elts",array_size(curr[i].array_dim));
    }
    NEXTLINE;
  }

}


PRIVATE void
mod_var_out(Lsymtab *lsymt,FILE *fd)
{
  WRITE_STR(" var",lsymt->name);
  WRITE_STR(" home",lsymt->home_unit);
  WRITE_NUM(" type",get_type(lsymt));
  WRITE_NUM(" kind",lsymt->kind);
  WRITE_NUM(" size",lsymt->size);
  (void)fprintf(fd," flags %d %d %d %d %d %d %d %d %d %d",
		lsymt->parameter,
		lsymt->array_var,
		lsymt->common_var,
		lsymt->allocatable,
		lsymt->pointer,
		lsymt->target,
		lsymt->used_flag,
		lsymt->set_flag,
		lsymt->assigned_flag,
		lsymt->used_before_set);
  if(lsymt->array_var) {
    NEXTLINE;
    WRITE_NUM(" dims",array_dims(lsymt->array_dim));
    WRITE_NUM(" elts",array_size(lsymt->array_dim));
  }
  if(lsymt->parameter) {
    NEXTLINE;
    WRITE_STR(" value",lsymt->info.param->src_text);
  }
  NEXTLINE;
}

      /* Routine to pack sym_list array with pointers to global symtab entries
	 having the property satisfying has_x() function.  has_x is has_defn or
	 has_call.
       */
PRIVATE int
find_prog_units(Gsymtab *sym_list[], int (*has_x)(ArgListHeader *alist),
	int module_mode)
{
    int i,num_entries;
    ArgListHeader *alist;
    int start_i;
    /* To avoid storing things defined outside module, start global
     * symtab scan at current module.  For project file, store everything. */
    if(module_mode)
      start_i = hashtab[current_prog_unit_hash].glob_symtab - glob_symtab;
    else
      start_i = 0;
    num_entries=0;
    for(i=start_i;i<glob_symtab_top;i++) {
#ifdef DEBUG_PROJECT
  if(debug_latest) {
      fprintf(list_fd,"\n%d %s",i,glob_symtab[i].name);
      fprintf(list_fd," %svalid",glob_symtab[i].valid?"":"in");
      fprintf(list_fd," %s",glob_symtab[i].private?"private":"public");
  }
#endif
      if(glob_symtab[i].valid &&
	storage_class_of(glob_symtab[i].type) == class_SUBPROGRAM &&
	!glob_symtab[i].private && /* for module: omit private routines */
	(alist=glob_symtab[i].info.arglist) != NULL) {
			/* Look for defns or calls of this guy. */

	if( (*has_x)(alist) != FALSE ) {
	  sym_list[num_entries++] = &glob_symtab[i];
	}

      }
    }
    return num_entries;
}

/* Routine to remove from sym_list any externals satisfied in this
   file.  Called only when sym_list contains calls and proj_trim_calls
   is true.
 */

PRIVATE int
trim_calls(int orig_num, Gsymtab *sym_list[])
{
  int i,new_num;
  for(i=new_num=0; i<orig_num; i++) {
    if(!library_mode || !has_defn(sym_list[i]->info.arglist)) {
      if( new_num != i ) {	/* move down if necessary */
	sym_list[new_num] = sym_list[i];
      }
      new_num++;
    }
  }
  return new_num;
}
      

PRIVATE void
prog_unit_out(Gsymtab* gsymt, FILE *fd, int mode)
{
	  if(mode == MODE_PROJ_DEFNS || mode == MODE_MODULE)
	    WRITE_STR(" entry",gsymt->name);
	  else
	    WRITE_STR(" external",gsymt->name);

	  if(mode == MODE_MODULE) {
	    /* for module files, home is where prog unit defined */
	    char *home;
	    Lsymtab* symt=hashtab[hash_lookup(gsymt->name)].loc_symtab;
	    if( symt != NULL )
	      home = symt->home_unit; /* inherited from subordinate module */
	    else
	      home = hashtab[current_prog_unit_hash].name; /* it's our own */
	    WRITE_STR(" home",home);
	  }
	  else {
	    /* for project files, home is prog unit name */
	    WRITE_STR(" home",gsymt->name);
	  }

	  WRITE_NUM(" class",storage_class_of(gsymt->type)); /* values as in global symtab */
	  WRITE_NUM(" type",datatype_of(gsymt->type));
	  WRITE_NUM(" kind",gsymt->kind);
	  WRITE_NUM(" size",gsymt->size);
		/* Flag values stored are cumulative only for current file
		   so they will not depend on what files were previously
		   read in current run.  When project file is read, flags
		   will be ORed into Gsymtab as is done in process_lists.
		*/
	  (void)fprintf(fd," flags %d %d %d %d %d %d %d %d",
		  gsymt->used_this_file,
		  gsymt->set_this_file,
		  gsymt->invoked_as_func_this_file,
		  gsymt->declared_external_this_file,
		  gsymt->library_prog_unit,
		  /* next flag values only apply to defn */
		  gsymt->elemental,
		  gsymt->pure,
		  0);	/* Flags for possible future use */
	  NEXTLINE;
	  alist_out(gsymt,fd,mode);
}

/* Routine to scan list of common block headers to find a declaration
   from the given module.

   We could get away with just using the front element except that
   module subprograms can declare the same blocks, and these defns
   will come in front of the module's.
 */

PRIVATE
ComListHeader *comblock_used_by(ComListHeader *clist, Gsymtab *module)
{
  while(clist != NULL) {
    if(clist->prog_unit == module)
      return clist;
    clist = clist->next;
  }
  return NULL;
}

/* Routine to make a list of common blocks.
 *
 * For project files, argument "module" is NULL, and all common blocks
 * declared in the current file are selected, unless
 * -project=trim-common and -lib are specified, in which case only one
 * declaration of each common block is kept.
 *
 * For modules, "module" is pointer to the global symtab entry of the
 * module.  Only blocks declared in the module are selected.
 */

PRIVATE void
find_comblocks(Gsymtab *sym_list[], Gsymtab *module, int *blocks, int *defns)
{
    int i,numblocks,numdefns;
    ComListHeader *clist;
    for(i=0,numblocks=numdefns=0;i<glob_symtab_top;i++) {
      if(storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK
	 && (clist=glob_symtab[i].info.comlist) != NULL &&
	 (module?
	  (comblock_used_by(clist,module)!=NULL):
	  (clist->topfile == top_filename)) ) {
			/* -project=trim-common: save only one com decl if
			   -lib mode.  For module, will use only one decl. */
	      if( !module && (proj_trim_common && library_mode) )
		numdefns++;
	else
			/* -project=no-trim-common or -nolib
			   mode: keep all com decls */
	  numdefns += count_com_defns(clist);

	sym_list[numblocks++] = &glob_symtab[i];
      }
    }
    *blocks = numblocks;
    *defns = numdefns;
}

PRIVATE void
comblocks_out(FILE *fd, Gsymtab *sym_list[], Gsymtab *module, int numblocks, int numdefns)
{
    int i;
    WRITE_NUM(" comblocks",numdefns);
    NEXTLINE;
    for(i=0; i<numblocks; i++) {
      Gsymtab *gsymt=sym_list[i];
      ComListHeader *c=gsymt->info.comlist;

	while( c != NULL && c->topfile == top_filename ) {
	  clist_out(gsymt,c,fd);

			/* for project files: -project=no-trim-common
			   or -nolib: loop thru all defns.  Otherwise
			   only keep the first. */
	  if(module == NULL && proj_trim_common && library_mode)
	    break;
	  c = c->next;
	}/* end while c != NULL */
    }
    NEXTLINE;
}



	/* alist_out: writes arglist data from symbol table to
	   project or module file. */

PRIVATE void
alist_out(Gsymtab *gsymt, FILE *fd, int mode)

{
  ArgListHeader *a=gsymt->info.arglist;
  ArgListElement *arg;
  int i,n;
  Gsymtab *last_calling_prog_unit;
  int do_defns = (mode == MODE_PROJ_DEFNS || mode == MODE_MODULE);
  int locally_defined = TRUE;;

  if( mode == MODE_PROJ_CALLS )
    locally_defined = has_defn(a); /* (avoid call if unnecessary) */

		/* This loop runs thru only those arglists that were
		    created in the current top file. */
    last_calling_prog_unit = NULL;
    while( a != NULL && (mode == MODE_MODULE || a->topfile == top_filename)) {
		/* do_defns mode: output only definitions */
    if( (do_defns && a->is_defn) || (!do_defns && !a->is_defn) ) {
		/* keep only externals not satisfied in this file in -lib
		   mode, otherwise keep one actual call from each program unit. */
    if( ! proj_trim_calls || 
	(a->is_defn
       || !locally_defined
       || (!library_mode && (a->is_call || a->actual_arg)
	   && a->prog_unit != last_calling_prog_unit))
	)
      {
      last_calling_prog_unit = a->prog_unit;
      if(a->is_defn)
	 (void)fprintf(fd," defn\n");
      else
	 (void)fprintf(fd," call\n");

      WRITE_STR(" unit",a->prog_unit->name);
      WRITE_STR(" file",a->filename);
      WRITE_NUM(" line",a->line_num);
      WRITE_NUM(" top",a->top_line_num);
      WRITE_NUM(" class",storage_class_of(a->type));
      WRITE_NUM(" type",datatype_of(a->type));
      WRITE_NUM(" kind",a->kind);
      WRITE_NUM(" size",a->size);
      (void)fprintf(fd," flags %d %d %d %d",
	      a->is_defn,
	      a->is_call,
	      a->external_decl,
	      a->actual_arg);
      NEXTLINE;
      n=a->numargs;
      if(a->is_defn || a->is_call) {
	WRITE_NUM(" args",n);
	NEXTLINE;
      }

      /* Next lines, 2 per argument.
	   1st line: position number & name or source text of expr
	   2nd line: type, array dims, array size, flags
       */
      arg = a->arg_array;
      for(i=0; i<n; i++) {
	WRITE_NUM(" arg",i+1);
	WRITE_ARG(" name",arg[i].name);
	NEXTLINE;
	WRITE_NUM(" class",storage_class_of(arg[i].type));
	WRITE_NUM(" type",datatype_of(arg[i].type));
	WRITE_NUM(" kind",arg[i].kind);
	WRITE_NUM(" size",arg[i].size);
	if( ((storage_class_of(arg[i].type) == class_VAR) &&
	     is_computational_type(datatype_of(arg[i].type))) ) {
	  WRITE_NUM(" dims",array_dims(arg[i].array_dim));
	  WRITE_NUM(" elts",array_size(arg[i].array_dim));
	}
	else {
	  WRITE_NUM(" dims",0);
	  WRITE_NUM(" elts",0);
	}
	  
	{ char *cblk;
	  if( arg[i].common_block == (Gsymtab *)NULL )
	    cblk = "-";	/* place holder if no block name */
	  else
	    cblk = arg[i].common_block->name;
	  WRITE_STR(" cblk",cblk);
	}
	WRITE_NUM(" cndx",arg[i].common_index);
	WRITE_NUM(" same",arg[i].same_as);
	(void)fprintf(fd," flags %d %d %d %d %d %d %d %d %d %d",
		arg[i].is_lvalue,
		arg[i].set_flag,
		arg[i].assigned_flag,
		arg[i].used_before_set,
		arg[i].array_var,
		arg[i].array_element,
		arg[i].declared_external,
		arg[i].active_do_var,
		arg[i].intent_in,
		arg[i].intent_out);
	NEXTLINE;
      }
      }/* end if ! proj_trim_calls ...*/
    }/* end if(do_defns...)*/
     a = a->next;
   }/* end while(a!=NULL)*/
   (void)fprintf(fd," end\n");
}/*alist_out*/



	/* clist_out writes common var list data from symbol
	   table to project or module file. */

PRIVATE void
  clist_out(Gsymtab *gsymt, ComListHeader *c, FILE *fd)
{
    ComListElement *cvar;
    int i,n;


      WRITE_STR(" block",gsymt->name);
      WRITE_NUM(" class",storage_class_of(gsymt->type));
      WRITE_NUM(" type",datatype_of(gsymt->type));
      NEXTLINE;
      WRITE_STR(" unit",c->prog_unit->name);
      WRITE_STR(" file",c->filename);
      WRITE_NUM(" line",c->line_num);
      WRITE_NUM(" top",c->top_line_num);
      (void)fprintf(fd," flags %d %d %d %d",
	      c->any_used,
	      c->any_set,
	      c->saved,
	      0);		/* Flag for possible future use */
      NEXTLINE;
      WRITE_NUM(" vars",(n=c->numargs));
      NEXTLINE;

    /* Next lines, 2 per variable.
         1st line: position number, name.
	 2nd line: class, type, array dims, array size
     */
      cvar = c->com_list_array;
      for(i=0; i<n; i++) {
	WRITE_NUM(" var",i+1);
	WRITE_STR(" name",cvar[i].name);
	NEXTLINE;
	WRITE_NUM(" class",storage_class_of(cvar[i].type));
	WRITE_NUM(" type",datatype_of(cvar[i].type));
	WRITE_NUM(" kind",cvar[i].kind);
	WRITE_NUM(" size",cvar[i].size);
	WRITE_NUM(" dims",array_dims(cvar[i].dimen_info));
	WRITE_NUM(" elts",array_size(cvar[i].dimen_info));
	(void)fprintf(fd," flags %d %d %d %d %d %d %d %d",
		cvar[i].used,
		cvar[i].set,
		cvar[i].used_before_set,
		cvar[i].assigned,
		0,		/* possible flags for future use */
		0,
		0,
		0);
      NEXTLINE;
      }
}

#undef WRITE_STR
#undef WRITE_NUM
#undef NEXTLINE


	/* proj_file_in:
	   Reads a project file, storing info in global symbol table.
	   See proj_file_out and its subroutines for the current
	   project file format.
	 */
#define MAXNAME 127 /* Max string that will be read in: see READ_STR below */


			/* Macros for error-flagging input */

PRIVATE int nil(VOID)/* to make lint happy */
{ return 0; }

#define READ_ERROR (oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,\
     "error reading module/project file"),nil())
#define READ_OK nil()

#define READ_FIRST_STR(LEADER,STR) (fscanf(fd,LEADER), \
				    fscanf(fd,"%127s",STR))
#define READ_STR(LEADER,STR) ((void)((fscanf(fd,LEADER)==0 &&\
			       fscanf(fd,"%127s",STR)==1)? READ_OK:READ_ERROR))
#define READ_ARG(LEADER,STR) ((void)((fscanf(fd,LEADER)==0 && fgetc(fd)==' ' &&\
		    (getstrn(STR,MAXNAME+1,fd)!=NULL)==1)? READ_OK:READ_ERROR))
#define READ_NUM(LEADER,NUM) ((void)((fscanf(fd,LEADER)==0 &&\
			       fscanf(fd,"%d",&NUM)==1)? READ_OK:READ_ERROR))
#define READ_LONG(LEADER,NUM) ((void)((fscanf(fd,LEADER)==0 &&\
			       fscanf(fd,"%ld",&NUM)==1)? READ_OK:READ_ERROR))
#if (SIZEOF_INT >= 4)			/* kind_t is int or long depending */
#define READ_KIND READ_NUM
#else
#define READ_KIND READ_LONG
#endif

#define NEXTLINE {int c;while( (c=fgetc(fd)) != EOF && c != '\n') continue;\
		    if(c == EOF) READ_ERROR; else ++proj_line_num;}


PRIVATE unsigned proj_line_num;		
			/* Line number in proj file for diagnostic output */

PRIVATE int type_map[MAX_DTYPES];	/* for mapping module's type ids to user's type ids */

void
#if HAVE_STDC
proj_file_in(FILE *fd)
#else /* K&R style */
proj_file_in(fd)
  FILE *fd;
#endif /* HAVE_STDC */
{
  char buf[MAXNAME+1],*topfilename=NULL;
  int retval;
  unsigned numentries,ientry, numexts,iext, numblocks,iblock;

  proj_line_num = 1;
  initialize_typemap();

  /* Allow project file to contain (manually) concatenated project files.
     These will be processed as if the separate project files were
     sequentially provided as arguments.
  */
 do {

 while( (retval=READ_FIRST_STR(PROJFILE_COOKIE,buf)) == 1) {
   if( strcmp(buf,PROJECT_VERSION) != 0 ) {
     (void)fprintf(stderr,
	 "\nERROR: project file is not current version -- must be re-created\n");
     exit(1);
   }
   NEXTLINE;
		/* Save filename in permanent storage */
   READ_STR("file",buf);
   topfilename = new_global_string(buf);
   NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("\nread file %s\n",topfilename);
#endif


  READ_NUM(" entries",numentries); /* Get no. of entry points */
  NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read entries %d\n",numentries);
#endif
				/* Read defn arglists */
  for(ientry=0; ientry<numentries; ientry++) {
    arg_info_in(fd,topfilename,TRUE,FALSE,(Token *)NULL,FALSE);
  }
  NEXTLINE;

  READ_NUM(" externals",numexts);	/* Get no. of external refs */
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read exts %d\n",numexts);
#endif
  NEXTLINE;

				/* Read invocation & ext def arglists */
  for(iext=0; iext<numexts; iext++) {
    arg_info_in(fd,topfilename,FALSE,FALSE,(Token *)NULL,FALSE);
  }
  NEXTLINE;


			/* Read common block info */

   READ_NUM(" comblocks",numblocks);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read num blocks %d\n",numblocks);
#endif
   NEXTLINE;

   for(iblock=0; iblock<numblocks; iblock++) {
     com_info_in(fd,topfilename,(const char *)NULL,(Token *)NULL,FALSE);
   }
   NEXTLINE;

 }/* end while(retval == 1) */

 init_symtab();		/* Clear out local strspace */

 /* End of a logical project file.  Continue to read any others
    concatenated together. */
 } while(retval != EOF);
}

/* Print items of only token list */
void print_token_list(const Token *tlist)
{
  if (tlist == NULL) return;

  /* skip the first token which is a duplicate */
  Token *t = tlist->next_token;

  printf("\nRENAME ONLY list contains : ");
  while (t != NULL ) {
    if (t->left_token != NULL) {	/* RENAME token */
      printf("%s => %s , ", t->left_token->src_text, t->src_text);
      t = t->next_token;
      continue;
    }
    printf("%s , ", t->src_text);
    t = t->next_token;
  }
  printf("\n");

  return;
}

/* Routine checks if a name appears in a token list, which is either
   an only-list or a rename-list.  If the name has a rename token,
   local_name is set to point to it.  (This will point to the
   hashtable name entry; if it is needed for the longer term it should
   be copied to global string space.)
 */
int name_in_only_list(const char *name, const Token *tlist, char **local_name)
{
  if (tlist == NULL) return FALSE;

  Token *t = tlist->next_token;
  while (t != NULL) {
    if( strcmp(name, t->src_text) == 0 ) {
      if( t->left_token != NULL) {	/* is a rename token */
	(*local_name) = hashtab[t->left_token->value.integer].name;
#ifdef DEBUG_PROJECT
if (debug_latest) {
  printf("%s found as RENAME item, rename to %s\n", name, *local_name);
}
#endif
      }
#ifdef DEBUG_PROJECT
      else {
if (debug_latest) {
  printf("%s found as ONLY item\n", name);
}
      }
#endif
      return TRUE;
    }
    t = t->next_token;
  }

  return FALSE;
}

/* Routine to read in a module file.
 */

void read_module_file(int h, Token *item_list, int only_list_mode)
{
  FILE *fd;
  Lsymtab *symt;		/* symbol table entries for module */
  Gsymtab *gsymt;

  char buf[MAXNAME+1],*topfilename=NULL,*modulename=NULL;
  char *module_filename;

     /* def_module should have created symtab entries if first encounter
      */
  symt = hashtab[h].loc_symtab;
  gsymt = hashtab[h].glob_symtab;
  if( gsymt == (Gsymtab*)NULL ) {
    oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,
		 "module not in symbol table");
    return;
  }
  modulename = gsymt->name;	/* use ptr to permanent stringspace */

  module_filename = make_module_filename(modulename);

  if( (fd = fopen(module_filename,"r")) == (FILE *)NULL ) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nERROR: Cannot open module file %s for reading\n",module_filename);
    return;
  }

  proj_line_num = 1;		/* for oops messages */
  initialize_typemap();		/* derived types may need to be re-indexed */

  if( READ_FIRST_STR(MODULEFILE_COOKIE,buf) != 1 ||
      strcmp(buf,MODULE_VERSION) != 0 ) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,
	"\nERROR: module file %s is not current version -- must be re-created\n",
		  module_filename);
    exit(1);
  }
  NEXTLINE;

  if(do_list) {
    (void)fflush(list_fd);
    (void)fprintf(list_fd,"\n\nReading module file %s\n",module_filename);
  }


		/* Read module name (should be same as USE name: if
		   different, indicates module file was hacked) */
   READ_STR("module",buf);
   if( strcasecmp(modulename,buf) != 0 ) {
     oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,
		  "module name in file differs from USE name");
     return;
   }

		/* Save filename in permanent storage.  If same as current
		   filename, use that pointer.
		 */
   READ_STR(" file",buf);
   if(strcmp(buf,top_filename) == 0)
     topfilename = top_filename;
   else
     topfilename = new_global_string(buf);
   NEXTLINE;
#ifdef DEBUG_PROJECT
   if(debug_latest) { printf("\nModule is %s from file %s\n",modulename,topfilename);
     print_token_list(item_list);
   }
#endif
			/* Read derived type definitions */
   {
     int i,numtypes;
     char sentinel[5];

     READ_NUM(" types",numtypes);
     NEXTLINE;
     for(i=0; i<numtypes; i++) {
       mod_type_in(fd,modulename,topfilename,item_list,only_list_mode);
     }
     fscanf(fd,"%5s",sentinel);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read sentinel %s\n",sentinel);
#endif
     if(strcmp(sentinel,"end") != 0) READ_ERROR;
     NEXTLINE;
   }

   {
     int numvars,ivar;
     char sentinel[5];

     ModVarListHeader *mvl_head;
     ModVar *mod_var;
     int in_mod = FALSE;

     READ_NUM(" locals",numvars);
     NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read locals %d\n",numvars);
#endif

     mvl_head = gsymt->modvarlist;

     /* first reference to module from use statment */
     if (mvl_head == NULL) {
       /* A module variable list header representing the usage information
	* in the module itself is needed.  This information will later
	* enable allow us to check whether a variable from a module was set
	* with a value in the module itself.  This header is created when
	* a USE statement is encountered for the first time.  
	*/
       mvl_head = new_modvarlistheader();
       gsymt->modvarlist = mvl_head;

       mvl_head->prog_unit = hashtab[h].glob_symtab;
       mvl_head->in_module = TRUE;
       in_mod = TRUE;
     }
     else {
       /* A module variable list header representing the usage information
	* in a prog unit is placed in the front of the list of headers.
	*/
       mvl_head = new_modvarlistheader();
       mvl_head->next = gsymt->modvarlist;
       gsymt->modvarlist = mvl_head;

       mvl_head->prog_unit = hashtab[current_prog_unit_hash].glob_symtab;
       mvl_head->in_module = FALSE;
     }

     mvl_head->numargs = numvars;
     mvl_head->line_num = symt->line_declared;
     mvl_head->top_line_num = top_file_line_num;
     mvl_head->mod_var_array = new_modvar(numvars);
     mvl_head->filename = current_filename;
     mvl_head->topfile = top_filename;

     /* Read local variables */
     for(ivar=0; ivar<numvars; ivar++) {
       mod_var = &(mvl_head->mod_var_array[ivar]);
       mod_var_in(fd,topfilename,item_list,only_list_mode,mod_var,in_mod);
     }

     /* if a header for the module was created, we need to create a
      * header for the prog unit
      */
     if (in_mod) {
       ModVarListHeader *mvl_mod = mvl_head;
       int i;

       mvl_head = new_modvarlistheader();
       mvl_head->next = gsymt->modvarlist;
       gsymt->modvarlist = mvl_head;

       mvl_head->prog_unit = hashtab[current_prog_unit_hash].glob_symtab;
       mvl_head->in_module = FALSE;
       mvl_head->numargs = numvars;
       mvl_head->line_num = symt->line_declared;
       mvl_head->top_line_num = top_file_line_num;
       mvl_head->mod_var_array = new_modvar(numvars);
       mvl_head->filename = current_filename;
       mvl_head->topfile = top_filename;

       for (i = 0; i < mvl_mod->numargs; i++) {
	 mvl_head->mod_var_array[i].name = mvl_mod->mod_var_array[i].name;

	 /* setting the any_set and any_used flags for the variables in
	  * the module itself
	  */
	 if (mvl_mod->mod_var_array[i].set)
	     mvl_mod->any_set = TRUE;
	 if (mvl_mod->mod_var_array[i].used)
	     mvl_mod->any_used = TRUE;
       }

     }

     fscanf(fd,"%5s",sentinel);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read sentinel %s\n",sentinel);
#endif
     if(strcmp(sentinel,"end") != 0) READ_ERROR;
     NEXTLINE;
   }

				/* Read subprogram info */

   {
     int numentries,ientry;

     READ_NUM(" entries",numentries); /* Get no. of entry points */
     NEXTLINE;
#ifdef DEBUG_PROJECT
 if (debug_latest) {
   printf("read entries %d\n",numentries);
 }
#endif

				/* Read interface defn arglists */
     for(ientry=0; ientry<numentries; ientry++) {
       arg_info_in(fd,topfilename,TRUE,TRUE,item_list,only_list_mode);
     }

     /* sentinel "end" is checked in arg_info_in */
     NEXTLINE;
   }

			/* Read common block info */
   {
     int iblock, numblocks;

     READ_NUM(" comblocks",numblocks);
     NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read num blocks %d\n",numblocks);
#endif

     for(iblock=0; iblock<numblocks; iblock++) {
       com_info_in(fd,topfilename,modulename,item_list,only_list_mode);
     }
     /* no sentinel "end" after com blocks */
     NEXTLINE;
   }
}


static char *prev_file_name="";/* used to reduce number of callocs */


PRIVATE void
mod_type_in(FILE *fd, const char *module_name, const char *filename, Token *item_list, int only_list_mode)
{
  char dtype_name[MAXNAME+1], 
       id_home[MAXNAME+1],
       type_module[MAXNAME+1],
       component_name[MAXNAME+1];
  int dtype_type, component_type;
  kind_t component_kind;
  long dtype_size, component_size;
  int i, dtype_num_components;
  int dtype_public,		/* dtype flag bits */
    dtype_private,
    dtype_private_components,
    dtype_sequence,
    dtype_dummy1,
    dtype_dummy2,
    dtype_dummy3,
    dtype_dummy4,
    component_array,		/* component flag bits */
    component_pointer,
    component_private,
    component_dummy1,
    component_dummy2,
    component_dummy3,
    component_dummy4,
    component_dummy5;
  int component_array_dims;
  unsigned long component_array_elts;
  int duplicate_dtype = FALSE;
  Dtype *dtype;
  DtypeComponent *curr;
  int h, home_h;
  Lsymtab *symt;
  int mapped_type;		/* type from map_type array */
  int use_this_item, in_list;
  char *local_name;

  READ_STR(" dtype",dtype_name);
  READ_STR(" home",id_home);
  READ_NUM(" type",dtype_type);
  READ_STR(" module",type_module);
  READ_LONG(" size",dtype_size);
  fscanf(fd," flags %d %d %d %d %d %d %d %d",
	 &dtype_public,
	 &dtype_private,
	 &dtype_private_components,
	 &dtype_sequence,
	 &dtype_dummy1,
	 &dtype_dummy2,
	 &dtype_dummy3,
	 &dtype_dummy4);
  NEXTLINE;

  READ_NUM(" components",dtype_num_components);
  NEXTLINE;

  local_name = dtype_name; /* if no rename, name is same as in module */

  if (item_list == NULL) {/* no RENAME ONLY list : use everything */
    use_this_item = TRUE;
  }
  else {	/* use only if in RENAME ONLY list */
    in_list = name_in_only_list(dtype_name, item_list, &local_name);

    if (only_list_mode)
      use_this_item = in_list;
    else	/* RENAME list : use everything */
      use_this_item = TRUE;
  }

  h = hash_lookup(local_name);

#ifdef DEBUG_PROJECT
  if(debug_latest) printf("Read dtype %s %ld %ld\n",dtype_name,dtype_type,dtype_size);
#endif

if (use_this_item) {
  if ((mapped_type = find_type_use_assoc(dtype_name, type_module)) != -1) {
#ifdef DEBUG_PROJECT
  if(debug_latest) {
    printf("%s from %s is a duplicate of %s in dtype_table\n",
           dtype_name, type_module, dtype_table[mapped_type]->name);
  }
#endif
    duplicate_dtype = TRUE;

    home_h = hash_lookup(id_home);

    /* if home module does not exist, create L&G symtab entries for it */
    if (hashtab[home_h].glob_symtab == (Gsymtab*)NULL) {
      Token t;
      implied_id_token(&t,id_home);
      t.line_num = NO_LINE_NUM;
      def_module(&t,(Token *)NULL,FALSE);
    }

			/* Create a symbol table entry for this type.
			 * If duplicate of previously seen module
			 * type, give it the existing type.  If new,
			 * assign this type a new type id.
			 */

    symt = hashtab[h].loc_symtab;
    if( symt == NULL || strcmp(symt->home_unit, id_home) != 0 ) {
      symt = install_local(h,mapped_type,class_DTYPE);
      symt->line_declared = proj_line_num; /* THIS IS WRONG */
      symt->file_declared = inctable_index;
      symt->home_unit = hashtab[home_h].name;
    }
    type_map[dtype_type] = mapped_type;	/* use the pre-existing number in this program */
    dtype = dtype_table[mapped_type];
    dtype->symt = symt;
  }
  else {
    symt = def_dtype(h,proj_line_num,NO_COL_NUM,
                     /*access_spec=*/0,/*dtype_def=*/TRUE);
    mapped_type = get_type(symt);	/* get number assigned by def_dtype */
    type_map[dtype_type] = mapped_type;
    dtype = dtype_table[mapped_type];
#ifdef DEBUG_PROJECT
  if(debug_latest) {
    printf("%s from %s is a new type\n",dtype_name, type_module);
    printf("type in module %ld mapped type %ld\n",(long)dtype_type,(long)mapped_type);
  }
#endif

    /* fill dtype_table with info from module */
    if (dtype_name != local_name)
      dtype->name = new_global_string(dtype_name);	/* if renamed, put module-defined name in table */
    dtype->filename = new_global_string((char *)filename);
    dtype->module_name = new_global_string((char *)type_module);
    dtype->num_components = dtype_num_components;
    dtype->public = dtype_public;
    dtype->private = dtype_private;
    dtype->private_components = dtype_private_components;
    dtype->sequence = dtype_sequence;

    if ( dtype_num_components > 0 && (dtype->components =
          (DtypeComponent *)malloc(dtype_num_components*sizeof(DtypeComponent)))
            == (DtypeComponent *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
          "Cannot alloc space for derived type components");
      return;
    } /* else (empty defn) dtype->components remains NULL */
  }

    /* Copy type info to symbol table entry.  (Probably not used.) */
  symt->size = dtype_size;
  symt->file_declared = inctable_index;	/* NEED TO CARRY THIS INFO OVER */
  symt->public = dtype->public;
  symt->private = dtype->private;
  symt->defined_in_module = TRUE; /* for honoring private components */
}

  /* read in the components */
  for(i = 0; i < dtype_num_components; i++) {
    READ_STR(" component",component_name);
    READ_NUM(" type",component_type);
    READ_KIND(" kind",component_kind);
    READ_LONG(" size",component_size);
    fscanf(fd," flags %d %d %d %d %d %d %d %d",
	   &component_array,
	   &component_pointer,
	   &component_private,
	   &component_dummy1,
	   &component_dummy2,
	   &component_dummy3,
	   &component_dummy4,
	   &component_dummy5);
    if( component_array ) {
      NEXTLINE;
      READ_NUM(" dims",component_array_dims);
      READ_LONG(" elts",component_array_elts);
    }
    else {
      component_array_dims = 0;
      component_array_elts = 0;
    }
    NEXTLINE;

    /* If new, store component info into dtype_table */
    if (!duplicate_dtype && use_this_item) {
      curr = dtype->components;
      curr[i].name = new_global_string(component_name);
      curr[i].type = map_type(component_type);
      curr[i].kind = component_kind;
      curr[i].array_dim = array_dim_info(component_array_dims,component_array_elts);
      curr[i].size = component_size;
      curr[i].array = component_array;
      curr[i].pointer = component_pointer;
      curr[i].private = component_private;
    }
  } /* finished reading in components */
}

PRIVATE void
initialize_typemap(VOID)
{
  int i;
  for(i=0; i<MAX_DTYPES; i++) {
    if( is_elementary_type(i) )
      type_map[i] = i;			/* elem types map to self */
    else
      type_map[i] = type_UNDECL;	/* initially derived types are undefined */
  }
}

/* Function to map from module's derived type numbering to user's.
   Any elementary types are returned unchanged.
 */
PRIVATE int
map_type(int t_in)
{
  if( t_in >= MAX_DTYPES ) {	/* bounds check */
    oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		 "derived type index out of bounds");
    return type_UNDECL;
  }
  else {
    /* To do this right, if map entry is undefined,
       should create an undefined derived type to use. */
    return type_map[t_in];
  }
}


PRIVATE void
mod_var_in(FILE *fd, const char *filename, Token *item_list, int only_list_mode, ModVar *mod_var, int in_module)
{
  char id_name[MAXNAME+1], id_home[MAXNAME+1], id_param_text[MAXNAME+1];
  long id_type;
  kind_t id_kind;
  int mapped_type;		/* type from map_type array */
  long id_size;
  int id_param,			/* flag bits */
    id_array_var,
    id_common_var,
    id_allocatable,
    id_pointer,
    id_target,
    id_used,
    id_set,
    id_assigned,
    id_used_before_set;
  int id_array_dims;
  unsigned long id_array_elts;
  int use_this_item, in_list;
  char *local_name;

  READ_STR(" var",id_name);
  READ_STR(" home",id_home);
  READ_LONG(" type",id_type);
  READ_KIND(" kind",id_kind);
  READ_LONG(" size",id_size);
  fscanf(fd," flags %d %d %d %d %d %d %d %d %d %d",
	 &id_param,
	 &id_array_var,
	 &id_common_var,
	 &id_allocatable,
	 &id_pointer,
	 &id_target,
	 &id_used,
	 &id_set,
	 &id_assigned,
	 &id_used_before_set);

  local_name = id_name;
  if (item_list == NULL) {/* no RENAME ONLY list : use everything */
    use_this_item = TRUE;
  }
  else {	/* use only if in RENAME ONLY list */
    in_list = name_in_only_list(id_name, item_list, &local_name);

    if (only_list_mode)
      use_this_item = in_list;
    else	/* RENAME list : use everything */
      use_this_item = TRUE;
  }

  mapped_type = map_type(id_type);

#ifdef DEBUG_PROJECT
  if(debug_latest) {
	  printf("Read var %s %ld %ld\n",id_name,id_type,id_size);
	  printf("mapped type %d\n",mapped_type);
  }
#endif
 {
  Lsymtab *symt;
  if (use_this_item) {
    int h = hash_lookup(local_name);
    int home_h = hash_lookup(id_home);

    /* if home module does not exist, create L&G symtab entries for it */
    if( hashtab[home_h].glob_symtab == (Gsymtab*)NULL ) {
      Token t;
      implied_id_token(&t,id_home);
      t.line_num = NO_LINE_NUM;
      def_module(&t,(Token *)NULL,FALSE);
    }

    /* Install decl, masking any existing from different home. */
    symt = hashtab[h].loc_symtab;
    if( symt == (Lsymtab*)NULL ||
	strcmp(symt->home_unit,id_home) != 0 ) {
      symt = install_local(h,mapped_type,class_VAR);
      symt->home_unit = hashtab[home_h].name;
      symt->size = id_size;
      symt->line_declared = NO_LINE_NUM;	/* NEED TO CARRY THIS INFO OVER */
      symt->file_declared = inctable_index;	/* NEED TO CARRY THIS INFO OVER */
      symt->defined_in_module = TRUE;	/* to suppress local usage warnings */
    }
    mod_var->name = symt->name;

    /* only copy usage information for header that represents the module
    */
    if (in_module) {
      mod_var->used = id_used;
      mod_var->set = id_set;
      mod_var->assigned = id_assigned;
      mod_var->used_before_set = id_used_before_set;
    }
  }

    if( id_array_var ) {
      NEXTLINE;
      READ_NUM(" dims",id_array_dims);
      READ_LONG(" elts",id_array_elts);
      if (use_this_item) {
        symt->array_var = TRUE;
        symt->array_dim = array_dim_info(id_array_dims,id_array_elts);
      }
    }
    else if( id_param ) {
      NEXTLINE;
      READ_STR(" value",id_param_text);
      if (use_this_item) {
        symt->set_flag = TRUE;
        symt->parameter = TRUE;
        symt->line_set = symt->line_declared;
        symt->file_set = symt->file_declared;
        symt->info.param = new_param_info();
        symt->info.param->seq_num = ++parameter_count;
        switch(mapped_type) {
        case type_INTEGER:
          sscanf(id_param_text,"%ld",&(symt->info.param->value.integer));
          break;
        case type_STRING:
          id_param_text[strlen(id_param_text)] = '\0'; /* remove trailing quote */
          symt->info.param->value.string = new_global_string(id_param_text+1); /* skip leading quote */
          break;
#ifndef NO_FLOATING_POINT
        case type_REAL:
        case type_DP:
          sscanf(id_param_text,"%lf",&(symt->info.param->value.dbl));
          break;
#endif
        default:
          symt->info.param->value.integer = 0;
          break;
        }
      }
    }

    if (use_this_item) {
      symt->common_var = id_common_var;
      symt->allocatable = id_allocatable;
      symt->pointer = id_pointer;
      symt->target = id_target;
    }
 }

  NEXTLINE;
}


			/* Read arglist info */
PRIVATE void
#if HAVE_STDC
arg_info_in(FILE *fd, char *filename, int is_defn, int module, Token *item_list, int only_list_mode)
                   		/* name of toplevel file */
#else /* K&R style */
arg_info_in(fd,filename,is_defn,item_list)
    FILE *fd;
    char *filename;		/* name of toplevel file */
    int is_defn;
    Token *item_list;
#endif /* HAVE_STDC */
{
    char id_name[MAXNAME+1],prog_unit_name[MAXNAME+1],sentinel[6];
    char id_home[MAXNAME+1];
    char file_name[MAXNAME+1];
    char arg_name[MAXNAME+1];
    int new_module=FALSE; 	/* module not seen before this module-file */
    int use_this_item;		/* entity in only_list */
    int seen_before=FALSE;	/* prog unit was in a previously read module */
#ifndef KEEP_ARG_NAMES
    static char var[]="var",	/* text strings to use for now */
	        expr[]="expr";
#endif
    int id_class,id_type;
    kind_t id_kind;
    long id_size;
    int mapped_type;		/* type from map_type array */
    unsigned
	      id_used_flag,
	      id_set_flag,
	      id_invoked,
	      id_declared,
	      id_library_prog_unit,
	      id_elemental,
	      id_pure,
	      future1;

    unsigned h;
    Gsymtab *gsymt, *prog_unit;
    unsigned alist_class,alist_type,alist_is_defn,alist_is_call,
       alist_external_decl,alist_actual_arg;
    int mapped_alist_type;
    unsigned alist_line, alist_topline;
    kind_t alist_kind;
    long alist_size;
    unsigned numargs,iarg,arg_num,arg_class,arg_type;
    kind_t arg_kind;
    int mapped_arg_type;
    int arg_dims;
    unsigned long arg_elts;
    long arg_size;
    char arg_common_block[MAXNAME+1];
    long arg_common_index;
    int arg_same_as;
    unsigned			/* Flags for arguments */
		arg_is_lvalue,
		arg_set_flag,
		arg_assigned_flag,
		arg_used_before_set,
		arg_array_var,
		arg_array_element,
		arg_declared_external,
		arg_active_do_var,
		arg_intent_in,
		arg_intent_out;
    char *local_name;
    int in_list;

    if(is_defn)
	READ_STR(" entry",id_name); /* Entry point name */
    else
	READ_STR(" external",id_name); /* External name */
    READ_STR(" home",id_home);	       /* home prog unit */
    READ_NUM(" class",id_class); /* class as in symtab */
    READ_NUM(" type",id_type); /* type as in symtab */
    READ_KIND(" kind",id_kind); /* kind as in symtab */
    READ_LONG(" size",id_size); /* size as in symtab */
    if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
	      &id_used_flag,
	      &id_set_flag,
	      &id_invoked,
	      &id_declared,
	      &id_library_prog_unit,
	      &id_elemental,
	      &id_pure,
	      &future1) != 8) READ_ERROR;
    NEXTLINE;

    local_name = id_name;
    if (module) {
      if (item_list == NULL) {/* no RENAME ONLY list : use everything */
        use_this_item = TRUE;
      }
      else {	/* use only if in RENAME ONLY list */
        in_list = ( id_type == type_MODULE ||
                    name_in_only_list(id_name, item_list, &local_name));

        if (only_list_mode)
          use_this_item = in_list;
        else	/* RENAME list : use everything */
          use_this_item = TRUE;
      }
    }
    else { /* project file */
      use_this_item = TRUE;
    }

    mapped_type = map_type(id_type);

#ifdef DEBUG_PROJECT
if(debug_latest) {printf("read id name %s class %d type %d\n",
id_name,id_class,id_type);
  printf("mapped type=%d\n",mapped_type);
}
#endif

  if ( !module || use_this_item ) {
    Lsymtab* symt;
				/* Create global symtab entry */
    h = hash_lookup(local_name);
				/* See if this procedure was gotten earlier
				   e.g. from a previously read module.  Must
				   match home unit.
				 */
    if( module ) {
      symt = hashtab[h].loc_symtab;
      if( symt != NULL && strcmp(id_home,symt->home_unit) == 0 )
	seen_before = TRUE;
    }
    if( (gsymt = hashtab[h].glob_symtab) == NULL || (module && !seen_before) ) {
      gsymt = install_global((int)h,mapped_type,class_SUBPROGRAM);
      gsymt->size = id_size;
      gsymt->kind = id_kind;
      new_module = TRUE;
      if (module)
	  gsymt->from_module = TRUE;	/* inherited through USE stmt */
    }
    else if(is_defn) {
      gsymt->size = id_size;
      gsymt->kind = id_kind;
    }
		/* Set library_prog_unit flag if project file was created
		   with -lib mode in effect, or is now taken in -lib mode */
    if(!module && is_defn && (library_mode || id_library_prog_unit)) {
      gsymt->library_prog_unit = TRUE;
    }
    if(is_defn)
      gsymt->defined = TRUE;
    if(id_used_flag)
      gsymt->used_flag = TRUE;
    if(id_set_flag)
      gsymt->set_flag = TRUE;
    if(id_invoked)
      gsymt->invoked_as_func = TRUE;
    if(id_declared)
      gsymt->declared_external = TRUE;
    if(id_elemental)
      gsymt->elemental = TRUE;
    if(id_pure)
      gsymt->pure = TRUE;

  }

   while(   fscanf(fd,"%5s",sentinel),
#ifdef DEBUG_PROJECT
	    (debug_latest? printf("sentinel=[%s]\n",sentinel):0),
#endif
	 strcmp(sentinel,(is_defn?"defn":"call")) == 0) {
      ArgListHeader *ahead;
      ArgListElement *alist;
#ifdef KEEP_ARG_NAMES
      ArgListHeader *prev_ahead;
      ArgListElement *prev_alist;
      unsigned prev_n;
#endif

      NEXTLINE;

      READ_STR(" unit",prog_unit_name);
      READ_STR(" file",file_name);
      READ_NUM(" line",alist_line); /* line number */
      READ_NUM(" top",alist_topline); /* topfile line number */
      READ_NUM(" class",alist_class);	/* class as in ArgListHeader */
      READ_NUM(" type",alist_type); /* type as in ArgListHeader */
      READ_KIND(" kind",alist_kind);/* kind as in ArgListHeader */
      READ_LONG(" size",alist_size); /* size as in ArgListHeader */
      if(fscanf(fd," flags %d %d %d %d",
		&alist_is_defn,
		&alist_is_call,
		&alist_external_decl,
		&alist_actual_arg) != 4) READ_ERROR;
      NEXTLINE;
   mapped_alist_type = map_type(alist_type);
#ifdef DEBUG_PROJECT
   if(debug_latest) {
     printf("read alist class %d type %d line %d\n",
	    alist_class,alist_type,alist_line);
     printf("mapped type %d\n",mapped_alist_type);
   }
#endif

    /* If this is a module, we need to create a local symbol table
       entry defining the subprogram, to hold its type.
     */
    if(module && use_this_item && !seen_before) {
      Lsymtab *symt = install_local(h,mapped_alist_type,class_SUBPROGRAM);
      symt->home_unit = id_home;
      symt->kind = alist_kind;
      symt->size = alist_size;
      symt->line_declared = alist_line;
      symt->file_declared = inctable_index;	/* WRONG */
      symt->elemental = id_elemental;
      symt->pure = id_pure;
    }

		/* Find current program unit in symtab. If not there, make
		   a global symtab entry for it. It will be filled
		   in eventually when processing corresponding entry.
		   If module is known already by having scanned the
		   source earlier in this run, do not create a new
		   defn arglist for it, which would trigger "multiply
		   defined" warning.
		 */
   if(( !module || (new_module && use_this_item))) {
      if (strcmp(local_name, id_name) == 0)
        h = hash_lookup(prog_unit_name);
      else
        h = hash_lookup(local_name);
      if( (prog_unit = hashtab[h].glob_symtab) == NULL) {
	prog_unit = install_global((int)h,type_UNDECL,class_SUBPROGRAM);
      }
      if(prog_unit->internal_entry) {
	warning(NO_LINE_NUM,NO_COL_NUM,
		"entry point redefined as prog unit");
	msg_tail(prog_unit->name);
	msg_tail(": redefinition ignored");
      }
      else {
	if(is_defn) {
	  if(prog_unit != gsymt) {
#ifdef DEBUG_PROJECT
	    if(debug_latest) printf("\nLinking entry %s to prog unit %s",
		   gsymt->name,prog_unit->name);
#endif
	    gsymt->internal_entry = TRUE;
	    gsymt->link.prog_unit=prog_unit; /* interior entry: link it to program unit */
	  }
	}
	else {			/* call: add to child list */
		/* Avoid duplication on child list.  It will have just
		   been placed there on previous project-file entry,
		   so it will be the first child on the list.
		*/
#ifdef DEBUG_PROJECT
	  if(debug_latest) printf("\nChild %s of prog unit %s",
		 gsymt->name,prog_unit->name);
#endif
	  if(prog_unit->link.child_list == NULL
	     || prog_unit->link.child_list->child != gsymt) {
	    ChildList *node=
	      (ChildList *)calloc(1,sizeof(ChildList));
#ifdef DEBUG_PROJECT
	    if(debug_latest) printf(" linked in");
#endif
	    node->child = gsymt;
	    node->next = prog_unit->link.child_list;
	    prog_unit->link.child_list = node;
	  }
#ifdef DEBUG_PROJECT
	  else {
	    if(debug_latest) printf(" (duplicate)");
	  }
#endif
	}
      }
   } /* end if new module */
      if(alist_is_defn || alist_is_call) {
	  READ_NUM(" args",numargs);
	  NEXTLINE;
      }
      else
	numargs = 0;

#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read numargs %d\n",numargs);
#endif
/*
**      if(!is_defn) {
**	gsymt->used_flag = TRUE;
**      }
*/
   if(( !module || (new_module && use_this_item))) {
				/* Create arglist structure */
      if(((ahead=(ArgListHeader *) calloc(1, sizeof(ArgListHeader)))
		 		 == (ArgListHeader *) NULL) ||
	  (numargs != 0 &&
          ((alist=(ArgListElement *) calloc(numargs,sizeof(ArgListElement)))
				 == (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,
			     "out of malloc space for argument list");
      }

			/* Initialize arglist and link it to symtab */
      ahead->type = type_pack(alist_class,mapped_alist_type);
      ahead->size = alist_size;
      ahead->kind = alist_kind;
      ahead->numargs = (short)numargs;
      ahead->arg_array = (numargs==0? NULL: alist);
      ahead->prog_unit = prog_unit;
      ahead->topfile = filename;
			/* try to avoid reallocating space for same name */
      ahead->filename =
	(strcmp(file_name,filename)==0? filename:
	 (strcmp(file_name,prev_file_name)==0? prev_file_name:
	  (prev_file_name=new_global_string(file_name))));

      ahead->line_num = alist_line;
      ahead->top_line_num = alist_topline;
      ahead->is_defn = alist_is_defn;
      ahead->is_call = alist_is_call;
      ahead->external_decl = alist_external_decl;
      ahead->actual_arg = alist_actual_arg;
      ahead->next = prev_ahead = gsymt->info.arglist;
      gsymt->info.arglist = ahead;
      if(prev_ahead != NULL) {
	prev_n = prev_ahead->numargs;
	prev_alist = prev_ahead->arg_array;
      }
   } /* end if new module */

			/* Fill arglist array from project file */
      for(iarg=0; iarg<numargs; iarg++) {
	READ_NUM(" arg",arg_num);	if(arg_num != iarg+1) READ_ERROR;
	READ_ARG(" name",arg_name);
	READ_NUM(" class",arg_class);
	READ_NUM(" type",arg_type);
	READ_KIND(" kind",arg_kind);
	READ_LONG(" size",arg_size);
	READ_NUM(" dims",arg_dims);
	READ_LONG(" elts",arg_elts);
	READ_STR(" cblk",arg_common_block);
	READ_LONG(" cndx",arg_common_index);
	READ_NUM(" same",arg_same_as);
	if(fscanf(fd," flags %d %d %d %d %d %d %d %d %d %d",
		&arg_is_lvalue,
		&arg_set_flag,
		&arg_assigned_flag,
		&arg_used_before_set,
		&arg_array_var,
		&arg_array_element,
		&arg_declared_external,
		&arg_active_do_var,
		&arg_intent_in,
		&arg_intent_out) != 10) READ_ERROR;

	mapped_arg_type = map_type(arg_type);

   if(( !module || (new_module && use_this_item))) {

#ifdef KEEP_ARG_NAMES
			/* Economize storage by re-using previously allocated
			   space for same name in prior call if any */
	alist[iarg].name = (prev_ahead != NULL && iarg < prev_n &&
			  strcmp(arg_name,prev_alist[iarg].name) == 0) ?
			    prev_alist[iarg].name:
			    new_global_string(arg_name);
#else
	if(strcmp(arg_name,expr) == 0) /* For now, just use "var" and "expr" */
	  alist[iarg].name = expr;
	else
	  alist[iarg].name = var;
#endif
	alist[iarg].array_dim = array_dim_info(arg_dims,arg_elts);
	alist[iarg].type = type_pack(arg_class,mapped_arg_type);
	alist[iarg].kind = arg_kind;
	alist[iarg].size = arg_size;
	if( strcmp(arg_common_block,"-") == 0 ) { /* indicator for "none" */
	  alist[iarg].common_block = (Gsymtab *)NULL;
	}
	else {
	  Gsymtab *g_symt;
	  int hh = hash_lookup(arg_common_block);
			/* Block may not have been seen yet since common
			   decls come at end of project file.  There is a
			   risk that block could end up in Gsymtab with no
			   definition, but only if project file is corrupt.
			 */
	  if( (g_symt = hashtab[hh].com_glob_symtab) == NULL)
	    g_symt = install_global(hh,type_COMMON_BLOCK,class_COMMON_BLOCK);
	  alist[iarg].common_block = g_symt;
	}
	alist[iarg].common_index = arg_common_index;
	alist[iarg].same_as = (short)arg_same_as;
	alist[iarg].is_lvalue = arg_is_lvalue;
	alist[iarg].set_flag = arg_set_flag;
	alist[iarg].assigned_flag = arg_assigned_flag;
	alist[iarg].used_before_set = arg_used_before_set;
	alist[iarg].array_var = arg_array_var;
	alist[iarg].array_element = arg_array_element;
	alist[iarg].declared_external = arg_declared_external;
	alist[iarg].active_do_var = arg_active_do_var;
	alist[iarg].intent_in = arg_intent_in;
	alist[iarg].intent_out = arg_intent_out;
   } /* end if new module */
	NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read arg num %d name %s\n",arg_num,arg_name);
#endif
      }/* end for(iarg...*/

    }/* end while( sentinel == "defn"|"call") */

    if(strcmp(sentinel,"end") != 0) READ_ERROR;
    NEXTLINE;
}


PRIVATE void
com_info_in(FILE *fd, const char *filename, const char *modulename, Token *item_list, int only_list_mode)
{
    char id_name[MAXNAME+1],prog_unit_name[MAXNAME+1];
    char file_name[MAXNAME+1];
    char var_name[MAXNAME+1];
    unsigned id_class,id_type;
    int mapped_type;		/* type from map_type array */
    unsigned			/* Flags in ComListHeader */
		clist_any_used,
		clist_any_set,
		clist_saved,
		clist_future;
    unsigned clist_line,clist_topline;
    unsigned numvars,prev_n,ivar,var_num,var_class,var_type;
    kind_t var_kind;
    int var_dims;
    unsigned long var_elts;
    unsigned			/* Flags for common variables */
		var_used,
		var_set,
		var_used_before_set,
		var_assigned,
		var_future_4,
		var_future_3,
		var_future_2,
		var_future_1;
    long var_size;
      int h;
      Gsymtab *gsymt, *prog_unit;
      ComListHeader *chead,*prev_chead;
      ComListElement *clist,*prev_clist;

      /* Items needed for module input */
    Token toklist;		/* header for list of common block elements */
    int from_this_module;
    int use_this_item, in_list;
    char *local_name;

    READ_STR(" block",id_name);
    READ_NUM(" class",id_class);
    READ_NUM(" type",id_type);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read com name %s class %d type %d\n",
id_name,id_class,id_type);
#endif
    NEXTLINE;

    local_name = id_name;
    if (modulename != (const char *)NULL) {
      if (item_list == NULL) {/* no RENAME ONLY list : use everything */
        use_this_item = TRUE;
      }
      else {	/* use only if in RENAME ONLY list */
        in_list = name_in_only_list(id_name, item_list, &local_name);

        if (only_list_mode)
          use_this_item = in_list;
        else	/* RENAME list : use everything */
          use_this_item = TRUE;
      }
    }
    else {
      use_this_item = TRUE;
    }

    mapped_type = map_type(id_type);

    READ_STR(" unit",prog_unit_name);
    READ_STR(" file",file_name);
    READ_NUM(" line",clist_line);
    READ_NUM(" top",clist_topline);
    if(fscanf(fd," flags %d %d %d %d",
		&clist_any_used,
		&clist_any_set,
		&clist_saved,
		&clist_future) != 4) READ_ERROR;
    NEXTLINE;

    READ_NUM(" vars",numvars);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read unit %s file %s",prog_unit_name,file_name);
 if(debug_latest) printf(" flags %d %d %d %d line %d\n",
	clist_any_used,
	clist_any_set,
	clist_saved,
	clist_future,
	clist_line);
#endif
    NEXTLINE;

    /* For module input, for local symbol table entry only take the
     * defn from the module itself, in case module subprogs also
     * declared the block. */
    from_this_module = (modulename != NULL &&
			strcmp(modulename,prog_unit_name) == 0);

 if (use_this_item) {
				/* Create global symtab entry */
    h = hash_lookup(local_name);
    if( (gsymt = hashtab[h].com_glob_symtab) == NULL)
      gsymt = install_global(h,mapped_type,(int)id_class);


				/* Create arglist structure */
    if(((chead=(ComListHeader *) calloc(1, sizeof(ComListHeader)))
		 		 == (ComListHeader *) NULL) ||
	  (numvars != 0 &&
          ((clist=(ComListElement *) calloc(numvars,sizeof(ComListElement)))
				 == (ComListElement *) NULL))){
		oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,
			     "out of malloc space for common list");
      }

		/* Find current program unit in symtab. If not there, make
		   a global symtab entry for it.  This is bogus, since
		   all program units should have been defined previously. */

      h = hash_lookup(prog_unit_name);
      if( (prog_unit = hashtab[h].glob_symtab) == NULL) {
	(void)fprintf(stderr,"\nWarning-- something's bogus in project file\n");
	prog_unit = install_global(h,type_UNDECL,class_SUBPROGRAM);
      }

			/* Initialize arglist and link it to global symtab */
      chead->numargs = (short)numvars;
      chead->line_num = clist_line;
      chead->top_line_num = clist_topline;
      chead->com_list_array = (numvars==0? NULL: clist);
      chead->prog_unit = prog_unit;
      chead->topfile = filename;
      chead->any_used = clist_any_used;
      chead->any_set = clist_any_set;
      chead->saved = clist_saved;
			/* try to avoid reallocating space for same name */
      chead->filename =
	(strcmp(file_name,filename)==0? filename:
	 (strcmp(file_name,prev_file_name)==0? prev_file_name:
	  (prev_file_name=new_global_string(file_name))));

      chead->next = prev_chead = gsymt->info.comlist;
      gsymt->info.comlist = chead;
      if(prev_chead != NULL) {
	prev_n = prev_chead->numargs;
	prev_clist = prev_chead->com_list_array;
      }

      if( from_this_module ) {
	toklist.next_token = NULL; /* initialize header */
      }
 } /*...  if (use_this_item) */

			/* Fill comlist array from project file */
    for(ivar=0; ivar<numvars; ivar++) {
      READ_NUM(" var",var_num); if(var_num != ivar+1) READ_ERROR;
      READ_STR(" name",var_name);
      NEXTLINE;
      READ_NUM(" class",var_class);
      READ_NUM(" type",var_type);
      READ_KIND(" kind",var_kind);
      READ_LONG(" size",var_size);
      READ_NUM(" dims",var_dims);
      READ_LONG(" elts",var_elts);
	if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
		&var_used,
		&var_set,
		&var_used_before_set,
		&var_assigned,
		&var_future_4,
		&var_future_3,
		&var_future_2,
		&var_future_1) != 8) READ_ERROR;
      NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read name %s class %ld type %ld dims %ld size %ld\n",
 var_name,(long)var_class,(long)var_type,(long)var_dims,(long)var_size);
#endif
			/* Economize storage by re-using previously allocated
			   space for same name in prior decl if any */
  if (use_this_item) {
      clist[ivar].name = (prev_chead != NULL && ivar < prev_n &&
			  strcmp(var_name,prev_clist[ivar].name) == 0) ?
			    prev_clist[ivar].name:
			    new_global_string(var_name);

      clist[ivar].dimen_info = array_dim_info(var_dims,var_elts);
      clist[ivar].type = type_pack(var_class,var_type);
      clist[ivar].kind = var_kind;
      clist[ivar].size = var_size;
      clist[ivar].used = var_used;
      clist[ivar].set = var_set;
      clist[ivar].used_before_set = var_used_before_set;
      clist[ivar].assigned = var_assigned;
  }
			/* If reading a module, the common variables
			   have local symbol table entries that need
			   to be associated with the block.
			 */
      if( from_this_module && use_this_item ) {
	int h = hash_lookup(var_name);
	Lsymtab *com_var;
	Token t;
	if( (com_var=hashtab[h].loc_symtab) == NULL || !com_var->common_var ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Common info in module is inconsistent");
	}
	com_var->common_block = gsymt;
	com_var->common_index = ivar;

	/* Conjure up a token for this item.  Note that all items have
	   been seen before when inputting locals.
	 */
	implied_id_token(&t,var_name);
	t.size = var_size;
	t.TOK_type = type_pack(var_class,var_type);
	t.TOK_flags = 0;	/* clear all flags */
	t.line_num = line_num;	/* line number of USE statement */
	t.col_num = NO_COL_NUM;
	
	toklist.next_token = append_token(toklist.next_token,&t);
      }
    } /* end for(ivar=0... */

			/* If this is a module definition, need to
			   create a local symbol table entry for the
			   using program unit to record set/used
			   status etc.  Make tokens as if this were a
			   parsed COMMON declaration.
			 */
    if( from_this_module && use_this_item) {
      Token block_id;
      implied_id_token(&block_id,id_name);
      block_id.line_num = line_num;
      block_id.col_num = NO_COL_NUM;
      def_com_block(&block_id,&toklist);
    }

}/*com_info_in*/

	/*  Function to read n-1 characters, or up to newline, whichever
	 *  comes first.  Differs from fgets in that the newline is replaced
	 *  by null, and characters up to newline (if any) past the n-1st
	 *  are read and thrown away.
	 *  Returns NULL when end-of-file or error is encountered.
	 */
PRIVATE char *
#if HAVE_STDC
getstrn(char *s, int n, FILE *fd)
#else /* K&R style */
getstrn(s,n,fd)
	char s[];
	int n;
	FILE *fd;
#endif /* HAVE_STDC */
{
	int i=0,c;

	while( (c=getc(fd)) != '\n' ) {
		if(c == EOF)
			return NULL;

		if(i < n-1)
			s[i++] = c;
	}
	s[i] = '\0';
	return s;
}

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
		void proj_alist_out() Outputs argument lists
		void proj_clist_out() Outputs common lists
		void proj_arg_info_in()  Inputs argument lists
		void proj_com_info_in()  Inputs common lists
*/

#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#define PROJECT
#include "symtab.h"
#include "symspace.h"
#include <string.h>
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
PROTO(PRIVATE int has_call,( ArgListHeader *alist ));
PROTO(PRIVATE int has_defn,( ArgListHeader *alist ));
PROTO(PRIVATE int nil,( void ));
PROTO(PRIVATE void proj_alist_out,( Gsymtab *gsymt, FILE *fd, int do_defns ));
PROTO(PRIVATE void proj_arg_info_in,( FILE *fd, char *filename, int is_defn ));
PROTO(PRIVATE int find_variables,(Lsymtab *sym_list[]));
PROTO(PRIVATE void mod_var_out,(Lsymtab *symt,FILE *fd));
PROTO(PRIVATE int find_prog_units,(Gsymtab *sym_list[], int (*has_x)(ArgListHeader *alist)));
PROTO(PRIVATE int trim_calls,(int orig_num, Gsymtab *sym_list[]));
PROTO(PRIVATE void proj_prog_unit_out,(Gsymtab* gsymt, FILE *fd, int do_defns));
PROTO(PRIVATE void find_comblocks, (Gsymtab *sym_list[], int *blocks, int *defns ));
PROTO(PRIVATE void proj_comblock_out, (FILE *fd, Gsymtab *sym_list[], int numblocks, int numdefns));
PROTO(PRIVATE void proj_clist_out,( Gsymtab *gsymt, FILE *fd ));
PROTO(PRIVATE void proj_com_info_in,( FILE *fd, const char *filename ));
PROTO(PRIVATE void mod_var_in,(FILE *fd, const char *filename));



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
#define WRITE_NUM(LEADER,NUM)	(void)(fprintf(fd,LEADER), fprintf(fd," %ld",NUM))
#define NEXTLINE		(void)fprintf(fd,"\n")



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

#ifdef DEBUG_MODULES
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

			/* Write local variables & parameters */
  {
    Lsymtab *lsym_list[LOCSYMTABSZ]; /* temp. list of local symtab entries to print */
    int i,numvars;
    numvars = find_variables(lsym_list);
    WRITE_NUM(" locals",(long)numvars);
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
    numdefns = find_prog_units(gsym_list,has_defn);
    WRITE_NUM(" entries",(long)numdefns);
    NEXTLINE;
    for(i=0; i<numdefns; i++) {
      proj_prog_unit_out(gsym_list[i],fd,/*do_defns=*/TRUE);
    }
    NEXTLINE;
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

      numdefns = find_prog_units(sym_list,has_defn);

      WRITE_NUM(" entries",(long)numdefns);
      NEXTLINE;
      for(i=0; i<numdefns; i++) {
	proj_prog_unit_out(sym_list[i],fd,/*do_defns=*/TRUE);
      }
      NEXTLINE;

      numcalls = find_prog_units(sym_list,has_call);

      if(proj_trim_calls)
	numcalls = trim_calls(numcalls,sym_list);

      WRITE_NUM(" externals",(long)numcalls);
      NEXTLINE;
      for(i=0; i<numcalls; i++) {
	proj_prog_unit_out(sym_list[i],fd,/*do_defns=*/FALSE);
      }
      NEXTLINE;

  }

  /* Write the common block section of project file */
  {
    int numblocks,numdefns;
    find_comblocks(sym_list,&numblocks,&numdefns);
    proj_comblock_out(fd,sym_list,numblocks,numdefns);
  }
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
mod_var_out(Lsymtab *lsymt,FILE *fd)
{
  WRITE_STR(" var",lsymt->name);
  WRITE_NUM(" type",get_type(lsymt));
  WRITE_NUM(" size",(long)lsymt->size);
  (void)fprintf(fd," flags %d %d %d %d %d %d %d %d",
		lsymt->parameter,
		lsymt->array_var,
		lsymt->common_var,
		lsymt->allocatable,
		lsymt->pointer,
		lsymt->target,
		0,0);		/* for future use */
  if(lsymt->array_var) {
    NEXTLINE;
    WRITE_NUM(" dims",lsymt->info.array_dim);
  }
  else if(lsymt->parameter) {
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
find_prog_units(Gsymtab *sym_list[], int (*has_x)(ArgListHeader *alist))
{
    int i,num_entries;
    ArgListHeader *alist;
    for(i=num_entries=0;i<glob_symtab_top;i++) {
#ifdef DEBUG_MODULES
  if(debug_latest) {
      fprintf(list_fd,"\n%d %s",i,glob_symtab[i].name);
      fprintf(list_fd," %svalid",glob_symtab[i].valid?"":"in");
      fprintf(list_fd," %s",glob_symtab[i].private?"private":"public");
  }
#endif
      if(glob_symtab[i].valid &&
	storage_class_of(glob_symtab[i].type) == class_SUBPROGRAM &&
	datatype_of(glob_symtab[i].type) != type_MODULE && /* omit modules */
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
proj_prog_unit_out(Gsymtab* gsymt, FILE *fd, int do_defns)
{
	  if(do_defns)
	    WRITE_STR(" entry",gsymt->name);
	  else
	    WRITE_STR(" external",gsymt->name);

	  WRITE_NUM(" class",(long)storage_class_of(gsymt->type));
	  WRITE_NUM(" type",(long)datatype_of(gsymt->type));
	  WRITE_NUM(" size",(long)gsymt->size);
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
		  /* N.B. library_prog_unit included here but is not restored */
		  gsymt->library_prog_unit,
		  0,	/* Flags for possible future use */
		  0,
		  0);
	  NEXTLINE;
	  proj_alist_out(gsymt,fd,do_defns);
}


PRIVATE void
find_comblocks(Gsymtab *sym_list[], int *blocks, int *defns)
{
    int i,numblocks,numdefns;
    ComListHeader *clist;
    for(i=0,numblocks=numdefns=0;i<glob_symtab_top;i++) {
      if(storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK
	 && (clist=glob_symtab[i].info.comlist) != NULL &&
	 clist->topfile == top_filename ) {
			/* No keepall: save only one com decl if -lib mode */
	if( proj_trim_common && library_mode)
	  numdefns++;
	else
			/* keepall or -nolib mode: keep all com decls */
	  numdefns += count_com_defns(clist);

	sym_list[numblocks++] = &glob_symtab[i];
      }
    }
    *blocks = numblocks;
    *defns = numdefns;
}

PRIVATE void
proj_comblock_out(FILE *fd, Gsymtab *sym_list[], int numblocks, int numdefns)
{
    int i;
    WRITE_NUM(" comblocks",(long)numdefns);
    NEXTLINE;
    for(i=0; i<numblocks; i++) {
      proj_clist_out(sym_list[i],fd);
    }
    NEXTLINE;
}



	/* proj_alist_out: writes arglist data from symbol table to
	   project file. */

PRIVATE void
proj_alist_out(Gsymtab *gsymt, FILE *fd, int do_defns)

{
  ArgListHeader *a=gsymt->info.arglist;
  ArgListElement *arg;
  int i,n;
  unsigned long diminfo;
  Gsymtab *last_calling_prog_unit;
  int locally_defined = do_defns || has_defn(a); /* (avoid call if unnecessary) */

		/* This loop runs thru only those arglists that were
		    created in the current top file. */
    last_calling_prog_unit = NULL;
    while( a != NULL && a->topfile == top_filename) {
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
      WRITE_NUM(" line",(long)a->line_num);
      WRITE_NUM(" top",(long)a->top_line_num);
      WRITE_NUM(" class",(long)storage_class_of(a->type));
      WRITE_NUM(" type",(long)datatype_of(a->type));
      WRITE_NUM(" size",(long)a->size);
      (void)fprintf(fd," flags %d %d %d %d",
	      a->is_defn,
	      a->is_call,
	      a->external_decl,
	      a->actual_arg);
      NEXTLINE;
      n=a->numargs;
      if(a->is_defn || a->is_call) {
	WRITE_NUM(" args",(long)n);
	NEXTLINE;
      }

      /* Next lines, 2 per argument.
	   1st line: position number & name or source text of expr
	   2nd line: type, array dims, array size, flags
       */
      arg = a->arg_array;
      for(i=0; i<n; i++) {
	WRITE_NUM(" arg",(long)i+1);
	WRITE_ARG(" name",arg[i].name);
	NEXTLINE;
	WRITE_NUM(" class",(long)storage_class_of(arg[i].type));
	WRITE_NUM(" type",(long)datatype_of(arg[i].type));
	WRITE_NUM(" size",(long)arg[i].size);
	diminfo = (
		   ((storage_class_of(arg[i].type) == class_VAR) &&
		   is_computational_type(datatype_of(arg[i].type))) ?
		     arg[i].info.array_dim: 0 );
	WRITE_NUM(" dims",(long)array_dims(diminfo));
	WRITE_NUM(" elts",(long)array_size(diminfo));
	{ char *cblk;
	  if( arg[i].common_block == (Gsymtab *)NULL )
	    cblk = "-";	/* place holder if no block name */
	  else
	    cblk = arg[i].common_block->name;
	  WRITE_STR(" cblk",cblk);
	}
	WRITE_NUM(" cndx",(long)arg[i].common_index);
	WRITE_NUM(" same",(long)arg[i].same_as);
	(void)fprintf(fd," flags %d %d %d %d %d %d %d %d",
		arg[i].is_lvalue,
		arg[i].set_flag,
		arg[i].assigned_flag,
		arg[i].used_before_set,
		arg[i].array_var,
		arg[i].array_element,
		arg[i].declared_external,
		arg[i].active_do_var);
	NEXTLINE;
      }
      }/* end if ! proj_trim_calls ...*/
    }/* end if(do_defns...)*/
     a = a->next;
   }/* end while(a!=NULL)*/
   (void)fprintf(fd," end\n");
}/*proj_alist_out*/



	/* proj_clist_out writes common var list data from symbol
	   table to project file. */

PRIVATE void
#if HAVE_STDC
proj_clist_out(Gsymtab *gsymt, FILE *fd)
#else /* K&R style */
proj_clist_out(gsymt,fd)
     Gsymtab *gsymt;
     FILE *fd;
#endif /* HAVE_STDC */
{
    ComListHeader *c=gsymt->info.comlist;
    ComListElement *cvar;
    int i,n;

    while( c != NULL && c->topfile == top_filename ) {

      WRITE_STR(" block",gsymt->name);
      WRITE_NUM(" class",(long)storage_class_of(gsymt->type));
      WRITE_NUM(" type",(long)datatype_of(gsymt->type));
      NEXTLINE;
      WRITE_STR(" unit",c->prog_unit->name);
      WRITE_STR(" file",c->filename);
      WRITE_NUM(" line",(long)c->line_num);
      WRITE_NUM(" top",(long)c->top_line_num);
      (void)fprintf(fd," flags %d %d %d %d",
	      c->any_used,
	      c->any_set,
	      c->saved,
	      0);		/* Flag for possible future use */
      NEXTLINE;
      WRITE_NUM(" vars",(long)(n=c->numargs));
      NEXTLINE;

    /* Next lines, 2 per variable.
         1st line: position number, name.
	 2nd line: class, type, array dims, array size
     */
      cvar = c->com_list_array;
      for(i=0; i<n; i++) {
	WRITE_NUM(" var",(long)i+1);
	WRITE_STR(" name",cvar[i].name);
	NEXTLINE;
	WRITE_NUM(" class",(long)storage_class_of(cvar[i].type));
	WRITE_NUM(" type",(long)datatype_of(cvar[i].type));
	WRITE_NUM(" size",(long)cvar[i].size);
	WRITE_NUM(" dims",(long)array_dims(cvar[i].dimen_info));
	WRITE_NUM(" elts",(long)array_size(cvar[i].dimen_info));
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
			/* keepall or -nolib: loop thru all defns.
			   Otherwise only keep the first. */
      if(proj_trim_common && library_mode)
	break;
      c = c->next;
    }/* end while c != NULL */
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
     "error reading project file"),nil())
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
#define NEXTLINE {int c;while( (c=fgetc(fd)) != EOF && c != '\n') continue;\
		    if(c == EOF) READ_ERROR; else ++proj_line_num;}


PRIVATE unsigned proj_line_num;		
			/* Line number in proj file for diagnostic output */

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
      proj_arg_info_in(fd,topfilename,TRUE);
  }
  NEXTLINE;

  READ_NUM(" externals",numexts);	/* Get no. of external refs */
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read exts %d\n",numexts);
#endif
  NEXTLINE;

				/* Read invocation & ext def arglists */
  for(iext=0; iext<numexts; iext++) {
    proj_arg_info_in(fd,topfilename,FALSE);
  }
  NEXTLINE;


			/* Read common block info */

   READ_NUM(" comblocks",numblocks);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read num blocks %d\n",numblocks);
#endif
   NEXTLINE;

   for(iblock=0; iblock<numblocks; iblock++) {
     proj_com_info_in(fd,topfilename);
   }
   NEXTLINE;

 }/* end while(retval == 1) */

 init_symtab();		/* Clear out local strspace */

 /* End of a logical project file.  Continue to read any others
    concatenated together. */
 } while(retval != EOF);
}


/* Routine to read in a module file.
 */

/**** NOTE: ONLY NOT IMPLEMENTED YET: NEED TO BUILD TOKEN LIST IN PARSER ****/
void read_module_file(int h, Token *only)
{
  FILE *fd;
  char buf[MAXNAME+1],*topfilename=NULL,*modulename=NULL;

  char *module_filename = make_module_filename(hashtab[h].name);

  if( (fd = fopen(module_filename,"r")) == (FILE *)NULL ) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nERROR: Cannot open module file %s for reading\n",module_filename);
    return;
  }

  proj_line_num = 1;		/* for oops messages */

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

		/* read module name (should be same as upcased file stem) */
   READ_STR("module",buf);
   modulename = new_global_string(buf);
		/* Save filename in permanent storage */
   READ_STR(" file",buf);
   topfilename = new_global_string(buf);
   NEXTLINE;
#ifdef DEBUG_MODULES
   if(debug_latest) printf("\nModule is %s from file %s\n",modulename,topfilename);
#endif

   {
     int numvars,ivar;
     char sentinel[5];

     READ_NUM(" locals",numvars);
     NEXTLINE;
#ifdef DEBUG_MODULES
 if(debug_latest) printf("read locals %d\n",numvars);
#endif
				/* Read local variables */
     for(ivar=0; ivar<numvars; ivar++) {
       mod_var_in(fd,topfilename);
     }
     fscanf(fd,"%5s",sentinel);
#ifdef DEBUG_MODULES
 if(debug_latest) printf("read sentinel %s\n",sentinel);
#endif
     if(strcmp(sentinel,"end") != 0) READ_ERROR;
     NEXTLINE;
   }

   {
     int numentries,ientry;

     READ_NUM(" entries",numentries); /* Get no. of entry points */
     NEXTLINE;
#ifdef DEBUG_MODULES
 if(debug_latest) printf("read entries %d\n",numentries);
#endif
				/* Read interface defn arglists */
     for(ientry=0; ientry<numentries; ientry++) {
       proj_arg_info_in(fd,topfilename,TRUE);
     }
     NEXTLINE;
   }

}


static char *prev_file_name="";/* used to reduce number of callocs */

PRIVATE void
mod_var_in(FILE *fd, const char *filename)
{
  char id_name[MAXNAME+1], id_param_text[MAXNAME+1];
  long id_type;
  long id_size;
  int id_param,			/* flag bits */
    id_array_var,
    id_common_var,
    id_allocatable,
    id_pointer,
    id_target,
    id_dummy1,
    id_dummy2;
  unsigned long id_array_dim;

  READ_STR(" var",id_name);
  READ_LONG(" type",id_type);
  READ_LONG(" size",id_size);
  fscanf(fd," flags %d %d %d %d %d %d %d %d",
	 &id_param,
	 &id_array_var,
	 &id_common_var,
	 &id_allocatable,
	 &id_pointer,
	 &id_target,
	 &id_dummy1,
	 &id_dummy2);
#ifdef DEBUG_MODULES
  if(debug_latest) printf("Read var %s %ld %ld\n",id_name,id_type,id_size);
#endif
  {
    /* Install decl, masking any existing. */
    int h = hash_lookup(id_name);
    Lsymtab *symt = install_local(h,datatype_of(id_type),class_VAR);
    symt->size = id_size;
    symt->line_declared = NO_LINE_NUM;	/* NEED TO CARRY THIS INFO OVER */
    symt->file_declared = inctable_index;	/* NEED TO CARRY THIS INFO OVER */

    if( id_array_var ) {
      NEXTLINE;
      READ_LONG(" dims",id_array_dim);
      symt->array_var = TRUE;
      symt->info.array_dim = id_array_dim;
    }
    else if( id_param ) {
      NEXTLINE;
      READ_STR(" value",id_param_text);
      symt->set_flag = TRUE;
      symt->parameter = TRUE;
      symt->line_set = symt->line_declared;
      symt->file_set = symt->file_declared;
      symt->info.param = new_param_info();
      symt->info.param->seq_num = ++parameter_count;
      switch(datatype_of(id_type)) {
      case type_INTEGER:
	sscanf(id_param_text,"%ld",&(symt->info.param->value.integer));
	break;
      case type_STRING:
	id_param_text[strlen(id_param_text)] = '\0'; /* remove trailing quote */
	symt->info.param->value.string = new_global_string(id_param_text+1); /* skip leading quote */
	break;
      case type_REAL:
      case type_DP:
	sscanf(id_param_text,"%lf",&(symt->info.param->value.dbl));
	break;
      default:
	symt->info.param->value.integer = 0;
	break;
      }
    }
  }

  NEXTLINE;
}


			/* Read arglist info */
PRIVATE void
#if HAVE_STDC
proj_arg_info_in(FILE *fd, char *filename, int is_defn)
                   		/* name of toplevel file */
#else /* K&R style */
proj_arg_info_in(fd,filename,is_defn)
    FILE *fd;
    char *filename;		/* name of toplevel file */
    int is_defn;
#endif /* HAVE_STDC */
{
    char id_name[MAXNAME+1],prog_unit_name[MAXNAME+1],sentinel[6];
    char file_name[MAXNAME+1];
    char arg_name[MAXNAME+1];

#ifndef KEEP_ARG_NAMES
    static char var[]="var",	/* text strings to use for now */
	        expr[]="expr";
#endif
    int id_class,id_type;
    long id_size;
    unsigned
	      id_used_flag,
	      id_set_flag,
	      id_invoked,
	      id_declared,
	      id_library_prog_unit,
	      future1,future2,future3;

    unsigned h;
    Gsymtab *gsymt, *prog_unit;
    unsigned alist_class,alist_type,alist_is_defn,alist_is_call,
       alist_external_decl,alist_actual_arg;
    unsigned alist_line, alist_topline;
    long alist_size;
    unsigned numargs,iarg,arg_num,arg_class,arg_type,arg_dims;
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
		arg_active_do_var;

    if(is_defn)
	READ_STR(" entry",id_name); /* Entry point name */
    else
	READ_STR(" external",id_name); /* External name */
    READ_NUM(" class",id_class); /* class as in symtab */
    READ_NUM(" type",id_type); /* type as in symtab */
    READ_LONG(" size",id_size); /* size as in symtab */
    if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
	      &id_used_flag,
	      &id_set_flag,
	      &id_invoked,
	      &id_declared,
	      &id_library_prog_unit,
	      &future1,&future2,&future3) != 8) READ_ERROR;
    NEXTLINE;

#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read id name %s class %d type %d\n",
id_name,id_class,id_type);
#endif

				/* Create global symtab entry */
    h = hash_lookup(id_name);
    if( (gsymt = hashtab[h].glob_symtab) == NULL) {
      gsymt = install_global((int)h,id_type,class_SUBPROGRAM);
      gsymt->size = id_size;
    }
    else if(is_defn)
      gsymt->size = id_size;

		/* Set library_prog_unit flag if project file was created
		   with -lib mode in effect, or is now taken in -lib mode */
    if(is_defn && (library_mode || id_library_prog_unit)) {
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

   while(   fscanf(fd,"%5s",sentinel),
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("sentinel=[%s]\n",sentinel),
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
      READ_LONG(" size",alist_size); /* size as in ArgListHeader */
      if(fscanf(fd," flags %d %d %d %d",
		&alist_is_defn,
		&alist_is_call,
		&alist_external_decl,
		&alist_actual_arg) != 4) READ_ERROR;
      NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read alist class %d type %d line %d\n",
alist_class,alist_type,alist_line);
#endif
		/* Find current program unit in symtab. If not there, make
		   a global symtab entry for it. It will be filled
		   in eventually when processing corresponding entry.
		 */

      h = hash_lookup(prog_unit_name);
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
      ahead->type = type_pack(alist_class,alist_type);
      ahead->size = alist_size;
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

			/* Fill arglist array from project file */
      for(iarg=0; iarg<numargs; iarg++) {
	READ_NUM(" arg",arg_num);	if(arg_num != iarg+1) READ_ERROR;
	READ_ARG(" name",arg_name);
	READ_NUM(" class",arg_class);
	READ_NUM(" type",arg_type);
	READ_LONG(" size",arg_size);
	READ_NUM(" dims",arg_dims);
	READ_LONG(" elts",arg_elts);
	READ_STR(" cblk",arg_common_block);
	READ_LONG(" cndx",arg_common_index);
	READ_NUM(" same",arg_same_as);
	if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
		&arg_is_lvalue,
		&arg_set_flag,
		&arg_assigned_flag,
		&arg_used_before_set,
		&arg_array_var,
		&arg_array_element,
		&arg_declared_external,
		&arg_active_do_var) != 8) READ_ERROR;

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
	alist[iarg].info.array_dim = array_dim_info(arg_dims,arg_elts);
	alist[iarg].type = type_pack(arg_class,arg_type);
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
	NEXTLINE;
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read arg num %d name %s\n",arg_num,arg_name);
#endif
      }

    }/* end while( sentinel == "defn"|"call") */

    if(strcmp(sentinel,"end") != 0) READ_ERROR;
    NEXTLINE;
}


PRIVATE void
#if HAVE_STDC
proj_com_info_in(FILE *fd, const char *filename)
#else /* K&R style */
proj_com_info_in(fd,filename)
     FILE *fd;
     char *filename;
#endif /* HAVE_STDC */
{
    char id_name[MAXNAME+1],prog_unit_name[MAXNAME+1];
    char file_name[MAXNAME+1];
    char var_name[MAXNAME+1];
    unsigned id_class,id_type;
    unsigned			/* Flags in ComListHeader */
		clist_any_used,
		clist_any_set,
		clist_saved,
		clist_future;
    unsigned clist_line,clist_topline;
    unsigned numvars,prev_n,ivar,var_num,var_class,var_type,var_dims;
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


    READ_STR(" block",id_name);
    READ_NUM(" class",id_class);
    READ_NUM(" type",id_type);
#ifdef DEBUG_PROJECT
 if(debug_latest) printf("read com name %s class %d type %d\n",
id_name,id_class,id_type);
#endif
    NEXTLINE;

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
				/* Create global symtab entry */
    h = hash_lookup(id_name);
    if( (gsymt = hashtab[h].com_glob_symtab) == NULL)
      gsymt = install_global(h,(int)id_type,(int)id_class);


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

			/* Initialize arglist and link it to symtab */
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

			/* Fill comlist array from project file */
    for(ivar=0; ivar<numvars; ivar++) {
      READ_NUM(" var",var_num); if(var_num != ivar+1) READ_ERROR;
      READ_STR(" name",var_name);
      NEXTLINE;
      READ_NUM(" class",var_class);
      READ_NUM(" type",var_type);
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
 if(debug_latest) printf("read name %s class %d type %d dims %d size %d\n",
var_name,var_class,var_type,var_dims,var_size);
#endif
			/* Economize storage by re-using previously allocated
			   space for same name in prior decl if any */
      clist[ivar].name = (prev_chead != NULL && ivar < prev_n &&
			  strcmp(var_name,prev_clist[ivar].name) == 0) ?
			    prev_clist[ivar].name:
			    new_global_string(var_name);

      clist[ivar].dimen_info = array_dim_info(var_dims,var_elts);
      clist[ivar].type = type_pack(var_class,var_type);
      clist[ivar].size = var_size;
      clist[ivar].used = var_used;
      clist[ivar].set = var_set;
      clist[ivar].used_before_set = var_used_before_set;
      clist[ivar].assigned = var_assigned;
    }
}/*proj_com_info_in*/

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

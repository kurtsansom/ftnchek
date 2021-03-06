/* $Id: calltree.c,v 1.8 2002/11/20 23:13:15 moniot Rel $

   Routines for producing call trees and cross-reference lists

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

	    com_xref_list()	Print cross-reference list of com blocks.
	    visit_children()	traverses the call tree, doing some checks
				and printing tree if requested.

*/


#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "pgsymtab.h"

				/* Local routines defined. */

PROTO(PRIVATE int block_is_volatile,( ComListHeader *clist, Gsymtab *main_prog_unit ));
PROTO(PRIVATE ComListHeader * com_tree_check,( Gsymtab *comblock, Gsymtab
				       *prog_unit, int level ));
PROTO(PRIVATE void visit_child,( Gsymtab *gsymt, int level, int wrapup ));
PROTO(PRIVATE void visit_child_reflist,( Gsymtab *gsymt ));
#ifdef VCG_SUPPORT
PROTO(PRIVATE void visit_child_vcg,( Gsymtab *gsymt, int level ));
#endif
PROTO(PRIVATE ChildList * sort_child_list,( ChildList *child_list ));
PROTO(PRIVATE void print_crossrefs,( void ));
PROTO(PRIVATE void print_cycle_nodes,( Gsymtab gsymt[], int nsym, Gsymtab
			       *node_list[], int node_count, int
			       parent_count[] ));
PROTO(PRIVATE int toposort,( Gsymtab gsymt[], int nsym ));
PROTO(PRIVATE ComListHeader * com_declared_by,( Gsymtab *comblock, Gsymtab *prog_unit ));
PROTO(PRIVATE void print_prog_units,( unsigned n, Gsymtab *list[] ));



				/* Things used for common undef check */
PRIVATE int com_tree_error;
PRIVATE int numvisited;



void
com_xref_list(VOID)	/* Print cross-reference list of com blocks */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    Gsymtab  **gsymlist,**blocklist;
#else
    Gsymtab  *gsymlist[GLOBSYMTABSZ],*blocklist[GLOBSYMTABSZ];
#endif
    int  i,numentries,numblocks;
    ComListHeader  *cmlist;

#ifdef DYNAMIC_TABLES
      if( (gsymlist=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL
	  ||(blocklist=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL
	  ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for common block list");
      }
#endif

	for(i=numblocks=0;i<glob_symtab_top;i++){ /* loop thru global table */
	   if (storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK){
	     blocklist[numblocks++] = &glob_symtab[i];
	   }
	}
	if(numblocks > 0) {

	  sort_gsymbols(blocklist,numblocks); /* Sort the common block list */

	  (void)fprintf(list_fd,
		       "\n        Common block cross-reference list:\n");
	  for(i=0; i<numblocks; i++) {
	     cmlist = blocklist[i]->info.comlist;
	     numentries=0;

#ifdef DEBUG_COM_USAGE
	     (void)fprintf(list_fd, "\n Common Block %s:\n",blocklist[i]->name );
#endif

	     while (cmlist != NULL){ /* loop thru declarations */

	         if(! irrelevant(cmlist)  &&
		    (cmlist->any_used || cmlist->any_set))
		   gsymlist[numentries++] = cmlist->prog_unit;
#ifdef DEBUG_COM_USAGE
		 print_comvar_usage(cmlist);
#endif
		 cmlist = cmlist->next;

	      }  /* end of while */

	     if (numentries >0){ /* print prog units that declare this block*/

	       (void)fprintf(list_fd, "\nCommon Block %s used in:\n" ,
			blocklist[i]->name );

				/* Sort prog units that declare this block */
	       sort_gsymbols(gsymlist,numentries);

	       print_prog_units((unsigned)numentries,gsymlist);

	     }  /* end of if numentries >0 */


	  } /* end of for i = 0 to numblocks */

	  (void)fprintf(list_fd,"\n");

	} /* end of if numblocks > 0*/

}

void
visit_children(int wrapup)
{
  int i,
	num_mains,		/* number of main programs */
	num_roots;		/* number of uncalled nonlibrary prog units */
  Gsymtab* main_prog_unit=NULL;
  
  num_roots =  0;
  for(i=0; i<glob_symtab_top; i++) {
    if(storage_class_of(glob_symtab[i].type) == class_SUBPROGRAM) {

      /* Clear visited flags set in previous calls (for module &
	 internal subprog argchecks). */
      glob_symtab[i].visited = FALSE;

      if( glob_symtab[i].internal_entry ) {
	glob_symtab[i].link.prog_unit->visited_somewhere = FALSE;
      }
      else {
	glob_symtab[i].visited_somewhere = FALSE;

	/* At end, either sort children of each subprog or put in call order */
	if(wrapup) {
	  glob_symtab[i].link.child_list=
	    sort_child_list(glob_symtab[i].link.child_list);
	}


	/* Count defined but uncalled non-library prog units for use later */
	if(glob_symtab[i].defined && !glob_symtab[i].used_flag &&
	   !glob_symtab[i].library_prog_unit)
	  ++num_roots;	/* Count tree roots for use if no mains */
      }
    }
  }

	/* Print heading for call tree output if it was requested */
  if(wrapup) {
   if(print_ref_list)
    (void)fprintf(list_fd,"\nList of subprogram references:");
#ifdef VCG_SUPPORT
  else if(print_vcg_list) {
    if(vcg_fd == stdout)
      (void)fprintf(vcg_fd,"\n");
    (void)fprintf(vcg_fd,"graph: {\ntitle: \"%s\"\n",main_filename);
			/* Global graph options go here.  See ftnchek.h.
			*/
    (void)fprintf(vcg_fd,VCG_GRAPH_OPTIONS);
   }
#endif
   else if(print_call_tree)
    (void)fprintf(list_fd,"\nTree of subprogram calls:");
  }

				/* Visit children of all main progs */
  for(i=0,num_mains=0; i<glob_symtab_top; i++) {
    if(glob_symtab[i].type == type_pack(class_SUBPROGRAM,type_PROGRAM)) {
      main_prog_unit = &glob_symtab[i];
      if(wrapup && print_ref_list)
	visit_child_reflist(main_prog_unit);
#ifdef VCG_SUPPORT
      else if(wrapup && print_vcg_list)
	visit_child_vcg(main_prog_unit,1);
#endif
      else
	visit_child(main_prog_unit,0,wrapup);
      ++num_mains;
    }

    /* visit subprograms inherited from USE stmt */
    if (!wrapup && glob_symtab[i].valid && glob_symtab[i].from_module)
	visit_child(&glob_symtab[i],0,wrapup);
  }
				/* If no main program found at wrapup, give
				   warning unless -noextern was set */
  if(num_mains == 0) {
    if(wrapup) {
      if(print_call_tree || print_ref_list
#ifdef VCG_SUPPORT
	 || print_vcg_list
#endif
	 ) {
	(void)fprintf(list_fd,"\n  (no main program found)");
      }
      else if(usage_ext_undefined) {
	(void)fprintf(list_fd,
		      "\nNo main program found");
      }
    }
		/* If no main, visit trees rooted at uncalled
		   nonlibrary routines, as the next best thing.
		   If there are no uncalled nonlib prog units, use
		   uncalled library routines.  If there are no uncalled
		   routines, then there is a cycle!
		 */
    for(i=0; i<glob_symtab_top; i++) {
      if(storage_class_of(glob_symtab[i].type) == class_SUBPROGRAM
	&& glob_symtab[i].defined && !glob_symtab[i].used_flag &&
	 (num_roots == 0 || !glob_symtab[i].library_prog_unit) ) {
	if(wrapup && print_ref_list)
	  visit_child_reflist(&glob_symtab[i]);
#ifdef VCG_SUPPORT
	else if(wrapup && print_vcg_list)
	  visit_child_vcg(&glob_symtab[i],1);
#endif
	else
	  visit_child(&glob_symtab[i],1,wrapup); /* indent all trees one level */
      }
    }
  }

  /* Remaining stuff is only for final wrapup phase */
  if( !wrapup )
    return;

  if(print_call_tree || print_ref_list)
    (void)fprintf(list_fd,"\n");
#ifdef VCG_SUPPORT
  if(print_vcg_list)
    (void)fprintf(vcg_fd,"}\n");
#endif

			/* Print list of callers of all visited
			   or non-library prog units, if -crossref
			   flag given. */
  if(print_xref_list) {
    print_crossrefs();
  }

			/* Print linkage-order list of prog units. */
  if( print_topo_sort ) {
    (void) toposort(glob_symtab,(int)glob_symtab_top);
  }

			/* Check that common blocks retain definition
			   status between uses. */
  if(check_com_tree || comcheck_volatile){
    if(num_mains != 1) {
      if(check_com_tree)
	(void)fprintf(list_fd,
		"\nCommon definition check requires single main program");
      if(comcheck_volatile)
	(void)fprintf(list_fd,
		"\nCommon volatility check requires single main program");
    }
    else {
      numvisited = 0;		/* need headcount in case of cycle */
      for(i=0; i<glob_symtab_top; i++) {
	if(glob_symtab[i].visited_somewhere)
	  numvisited++;
      }
      for(i=0; i<glob_symtab_top; i++) {
	if(storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK) {
	  if( block_is_volatile(glob_symtab[i].info.comlist,main_prog_unit) ) {
	    if(comcheck_volatile) {
	      (void)fprintf(list_fd,
		   "\nCommon block %s is volatile",
		   glob_symtab[i].name);
	    }
	    if(check_com_tree) {
	      com_tree_error=0;
	      (void)com_tree_check(&glob_symtab[i],main_prog_unit,0);
	    }
	  }
	}
      }
    }
  }
}

	/* Returns TRUE unless block is SAVED by any prog unit, or declared by
	   the actual main program or in a BLOCK DATA subprogram. */
PRIVATE int
#if HAVE_STDC
block_is_volatile(ComListHeader *clist, Gsymtab *main_prog_unit)
#else /* K&R style */
block_is_volatile(clist,main_prog_unit)
     ComListHeader *clist;
     Gsymtab *main_prog_unit;
#endif /* HAVE_STDC */
{
  int t;
  while(clist != NULL) {
    if( clist->saved ||
       (t=datatype_of(clist->prog_unit->type)) == type_BLOCK_DATA
       || (t == type_PROGRAM && clist->prog_unit == main_prog_unit)) {
      return FALSE;
    }
    clist = clist->next;
  }
  return TRUE;
}

 /* If block declared by prog unit, returns pointer to the comlist
    header which describes it.  Otherwise returns NULL. */
PRIVATE ComListHeader *
#if HAVE_STDC
com_declared_by(Gsymtab *comblock, Gsymtab *prog_unit)
#else /* K&R style */
com_declared_by(comblock,prog_unit)
     Gsymtab *comblock,*prog_unit;
#endif /* HAVE_STDC */
{
  ComListHeader *clist=comblock->info.comlist;
  while(clist != NULL) {
    if(clist->prog_unit == prog_unit) {
      if(clist->saved) {
	com_tree_error = TRUE;	/* not so, but causes bailout */
      }
      return clist;
    }
    clist = clist->next;
  }
  return NULL;
}


		/* Checks whether common block can become undefined
		   between activations of some prog unit that declares it.
		   Should only be done for blocks that are volatile, i.e.
		   that are not SAVED or declared in main or block_data.
		   Rules used are:
		     (1) Block is declared in two subtrees whose roots
		         are called by a given prog unit, and not in
			 the given prog unit itself or above.
		     (2) Block is declared and elements accessed in a prog unit
		         called by a given prog unit, and not declared in the
			 prog unit itself or above.  (Prog unit that declares it but
			 does not access elements, can be holding the
			 block active for its children.)
		   Since Rule 2 is likely to be wrong often due to Ftnchek's
		   lack of knowledge about whether a routine is invoked
		   more than once, it is suppressed for now.
		*/
PRIVATE ComListHeader *
#if HAVE_STDC
com_tree_check(Gsymtab *comblock, Gsymtab *prog_unit, int level)
#else /* K&R style */
com_tree_check(comblock,prog_unit,level)
     Gsymtab *comblock,*prog_unit;
     int level;
#endif /* HAVE_STDC */
{
  ComListHeader *clist;

	/* The following only protects against recursion.  It is not
	   a full-fledged cycle detector just a stopper. */
  if(level > numvisited) {
    (void)fprintf(list_fd,
	    "\nWarning: Call tree has a cycle containing prog unit %s\n",
	    prog_unit->name);
    com_tree_error = TRUE;
    return NULL;
  }

		/* If this prog unit declares the block, return its clist */
  if( (clist=com_declared_by(comblock,prog_unit)) != NULL) {
#ifdef DEBUG_SAVE
      (void)fprintf(list_fd,"\n%s declared by %s",comblock->name,prog_unit->name);
#endif
    return clist;
  }
  else {	/* Otherwise see if it is declared in subtree */
    int any_child_declares_it;
    ComListHeader *declaring_clist, *this_clist;
    ChildList *child_list;

    any_child_declares_it=FALSE;
    declaring_clist=NULL;
				/* Scan list of children */
    child_list = (prog_unit->internal_entry?prog_unit->link.prog_unit:prog_unit)
		   ->link.child_list;
    while(child_list != NULL) {
      this_clist = com_tree_check(comblock,child_list->child,level+1);
				/* Error was detected below: bail out */
      if(com_tree_error) {
	return NULL;
      }
      else if(this_clist != NULL) {
				/* Subtree contains the block */
	if(any_child_declares_it			   /* Rule 1 */
#ifdef COMTREE_RULE_2
	   || (this_clist->any_used || this_clist->any_set) /* Rule 2 */
#endif
	){
	  cmp_error_count = 0;
	  (void)comcmp_error_head(comblock->name,this_clist,
		 "may become undefined between activations");
	  com_error_report(this_clist,"Declared");
	  if(declaring_clist != NULL && declaring_clist != this_clist) {
	    com_error_report(declaring_clist,"Declared");
	  }
	  (void)fprintf(list_fd,"\n        ");
	  (void)fprintf(list_fd,
		  "Not declared in parent prog unit %s",
		  prog_unit->name);
	  com_tree_error = TRUE;
	  return NULL;
	}
	else {
	  any_child_declares_it = TRUE;
	  declaring_clist = this_clist;
	}
      }

      child_list = child_list->next;
    }
		/* If any subtree declares it, say so */
    return declaring_clist;
  }
}



				/* Depth-first search of call tree */
PRIVATE void
visit_child(Gsymtab *gsymt, int level, int wrapup)
{
  static char fmt[]="%000s";	/* Variable format for indenting names */
  ChildList *child_list;
  static int terminate_href = 0;
  char *fname=NULL;
  ArgListHeader *arghdr;


  if(wrapup && print_call_tree) {
     if ( htmlcalltree_fd ) {
				/* Look up defn arglist entry to find the
				   filename where this guy is defined.
				 */
	for ( arghdr = gsymt->info.arglist; arghdr; arghdr=arghdr->next )
	{
	   if ( arghdr->is_defn && arghdr->filename )
	   {
	      fname = new_ext( arghdr->filename,
					DEF_HTML_EXTENSION );
	   }
	}
	if ( terminate_href )
            {
            terminate_href = 0;
            ( void ) fprintf( htmlcalltree_fd, "</A>" );
            }
         ( void ) fprintf( htmlcalltree_fd, "\n" );
     }
    (void)fprintf(list_fd,"\n");
    if(level > 0) {
      (void)sprintf(fmt,"%%%ds",level*4); /* indent 4 spaces per nesting level */
      (void)fprintf(list_fd,fmt,"");
      if ( htmlcalltree_fd )
         {           
         if ( fname )
            {
            if ( ! gsymt->internal_entry )
               ( void ) fprintf( htmlcalltree_fd, "%*.*s<A href=\"%s#%s\">",
                                 level * 4, level * 4, " ",
                                 fname, gsymt->name );
            else
               ( void ) fprintf( htmlcalltree_fd, "%*.*s<A href=\"%s#%s\">",
                                 level * 4, level * 4, " ",
                                 fname, gsymt->link.prog_unit->name );
            terminate_href = 1;
            }
         else
            {
            ( void ) fprintf( htmlcalltree_fd, "%*.*s", level * 4, level * 4,
                              " " );
            terminate_href = 0;
            }
         }      
    }
    if(gsymt->internal_entry)
      {
      (void)fprintf(list_fd,"%s entry ",gsymt->link.prog_unit->name);
         if ( htmlcalltree_fd )
            ( void ) fprintf( htmlcalltree_fd, "%s entry ",
                              gsymt->link.prog_unit->name );      
      }
    (void)fprintf(list_fd,"%s",gsymt->name);
    if ( htmlcalltree_fd )
       {
       if ( level == 0 )
          {
          if ( gsymt->internal_entry )
            {
	       if(fname)
		  ( void ) fprintf( htmlcalltree_fd, "<A href=\"%s#%s\">%s",
                              fname, gsymt->link.prog_unit->name, gsymt->name );
            }
          else
            {  
            ( void ) fprintf( htmlcalltree_fd, "<A href=\"%s#%s\">%s",
                              fname, gsymt->name, gsymt->name );
            }
          terminate_href = 1;
          }
       else
          ( void ) fprintf( htmlcalltree_fd, "%s", gsymt->name );
       }

    if(fname)
       free( fname );

  } /* end if wrapup && print_call_tree */

				/* Visit its unvisited children.  Note
				   that children of internal entry are
				   taken as those of its superior prog unit.
				 */
  child_list = (gsymt->internal_entry?gsymt->link.prog_unit:gsymt)
		   ->link.child_list;

				/* If already visited, do not visit its
				   children, but give note to reader if it
				   has some.  Always prune if not wrapup
				   since not printing anything */
  if((!wrapup || call_tree_prune) && gsymt->visited) {
    if(wrapup && print_call_tree && child_list != NULL)
      {
      (void)fprintf(list_fd," (see above)");
         if ( htmlcalltree_fd )
            {
            if ( terminate_href )
               { 
               ( void )fprintf( htmlcalltree_fd, "</A>" ); 
               terminate_href=0; 
               }
            ( void ) fprintf( htmlcalltree_fd, " (see above)" );
            }
      }      
  }
  else {
				/* Mark node as visited */
    gsymt->visited = TRUE;
				/* Record that containing prog unit
				   is visited via this entry point*/
    if(gsymt->internal_entry)
      gsymt->link.prog_unit->visited_somewhere = TRUE;
    else
      gsymt->visited_somewhere = TRUE;

   if ( wrapup && print_call_tree )
      {
      if ( terminate_href )
         {
         ( void ) fprintf( htmlcalltree_fd, "</A>" );
         terminate_href = 0;
         }
      }

    ++level;			/* move to next level */
    while(child_list != NULL) {
      visit_child(child_list->child,level,wrapup);
      child_list = child_list->next;
    }
  }
  if ( terminate_href )
     {
     ( void ) fprintf( htmlcalltree_fd, "</A>" );
     terminate_href = 0;
     }
}

/*** visit_child_reflist

Same as visit_child, except it does a breadth-first search of the call
tree, and prints the results in the form of a who-calls-who list.

Contributed by: Gerome Emmanuel : Esial Troisieme annee
		Projet commun Esial / Ecole des mines
		INERIS
		E-mail: gerome@mines.u-nancy.fr
Date received: 20-APR-1993
Modified slightly to make it compatible as alternative to call-tree and
to make output format consistent.
***/

PRIVATE void
#if HAVE_STDC
visit_child_reflist(Gsymtab *gsymt)
#else /* K&R style */
visit_child_reflist(gsymt)
     Gsymtab *gsymt;
#endif /* HAVE_STDC */
{
  ChildList *child_list;

  child_list = (gsymt->internal_entry?gsymt->link.prog_unit:gsymt)
                   ->link.child_list;

                                /* If already visited, do not visit its
                                   children, but give note to reader if it
                                   has some. */
  if(!gsymt->visited) {
                                /* Mark node as visited */
    gsymt->visited = TRUE;
                                /* Record that containing prog unit
                                   is visited via this entry point*/
    if(gsymt->internal_entry)
      gsymt->link.prog_unit->visited_somewhere = TRUE;
    else
      gsymt->visited_somewhere = TRUE;

    if(print_ref_list)		/* Print callees neatly if desired */
    {
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
      Gsymtab  **gsymlist;
#else
      Gsymtab  *gsymlist[GLOBSYMTABSZ];
#endif
      ChildList *child_list2;
      unsigned numcalls;

#ifdef DYNAMIC_TABLES
      if( (gsymlist=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for reference list");
      }
#endif

      (void)fprintf(list_fd,"\n%s calls:",gsymt->name);

      numcalls = 0;
      child_list2 = child_list;
      while(child_list2 != NULL)
	  {
	    gsymlist[numcalls++] = child_list2->child;
	    child_list2 = child_list2->next;
	  }

      if(numcalls == (unsigned)0)
	    (void)fprintf(list_fd," none");
      else {
	    (void)fprintf(list_fd,"\n");
	    print_prog_units(numcalls,gsymlist);
      }
#ifdef DYNAMIC_TABLES
      (void) cfree(gsymlist);
#endif
    }

    while(child_list != NULL) {
      visit_child_reflist(child_list->child);
      child_list = child_list->next;
    }
  }
}

/* visit_child_vcg:
  
  Same as visit_child_reflist except it provides output suitable for
  visualisation of the call graph, using the vcg graph visualisation
  program.  VCG is freely available from ftp.cs.uni-sb.de and
  elsewhere. It was written by G. Sander of the University of
  Saarland, Germany.

  Contributed by:  P.A.Rubini@cranfield.ac.uk
  Date: 3-APR-1995
*/

#ifdef VCG_SUPPORT
PRIVATE void
#if HAVE_STDC
visit_child_vcg(Gsymtab *gsymt, int level)
#else /* K&R style */
visit_child_vcg(gsymt,level)
     Gsymtab *gsymt;
     int level;
#endif /* HAVE_STDC */
{
  ArgListHeader *arglist;
  ChildList *child_list;

  child_list = (gsymt->internal_entry?gsymt->link.prog_unit:gsymt)
                   ->link.child_list;

                                /* If already visited, do not visit its
                                   children, but give note to reader if it
                                   has some. */
  if(!gsymt->visited) {
                                /* Mark node as visited */
    gsymt->visited = TRUE;
                                /* Record that containing prog unit
                                   is visited via this entry point*/
    if(gsymt->internal_entry)
      gsymt->link.prog_unit->visited_somewhere = TRUE;
    else
      gsymt->visited_somewhere = TRUE;

    if(print_vcg_list)		/* Print callees neatly if desired */
    {
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
      Gsymtab  **gsymlist;
#else
      Gsymtab  *gsymlist[GLOBSYMTABSZ];
#endif
      ChildList *child_list2;
      int j;
      int numcalls;

#ifdef DYNAMIC_TABLES
      if( (gsymlist=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for reference list");
      }
#endif

    numcalls = 0;
    child_list2 = child_list;
    while(child_list2 != NULL)
	  {
	    gsymlist[numcalls++] = child_list2->child;
	    child_list2 = child_list2->next;
	  }

    arglist = gsymt->info.arglist;
    while(arglist != NULL) {
      if ( arglist->is_defn ) {

         (void)fprintf(vcg_fd,"\ngraph: {\ntitle:\"[%s]\"\n",gsymt->name);
         (void)fprintf(vcg_fd,
	      "node: { title: \"%s\" label: \"%s \\n (%s)\" info1:\"%d\" }\n",
                    gsymt->name,gsymt->name,
                    arglist->filename,
                    level );


	  if(numcalls != 0) {
		for (j=0;j<numcalls;j++){
		   arglist = gsymlist[j]->info.arglist;
		   while(arglist != NULL) {
		     if ( arglist->is_defn ) {
			(void)fprintf(vcg_fd,
		 "edge: { sourcename: \"%s\" targetname: \"%s\" class:%d} \n",
			    gsymt->name,gsymlist[j]->name,
                            level );
			break ;
		     }
                     arglist = arglist->next;
		   }
		}
	  }
          break;
      }
      arglist = arglist->next;
    }
#ifdef DYNAMIC_TABLES
      (void) cfree(gsymlist);
#endif

    ++level;			/* move to next level */

/*  while(child_list != NULL) {
      visit_child_vcg(child_list->child,level);
      child_list = child_list->next;
    } */

    for (j=0;j<numcalls;j++){
       arglist = gsymlist[j]->info.arglist;
       while(arglist != NULL) {
          if ( arglist->is_defn ) {
             visit_child_vcg(gsymlist[j],level);
             break ;
          }
          arglist = arglist->next;
       }
    }
    (void)fprintf(vcg_fd,"}\n");
    }
  }
}

#endif /* VCG_SUPPORT */


PRIVATE void
print_crossrefs(VOID)
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
      Gsymtab  **gsymlist, **prog_unitlist;
#else
  Gsymtab  *gsymlist[GLOBSYMTABSZ], *prog_unitlist[GLOBSYMTABSZ];
#endif
  ArgListHeader *args;
  int  i,numentries;
  int numcalls;

#ifdef DYNAMIC_TABLES
      if( (gsymlist=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL ||
	 (prog_unitlist=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for crossref list");
      }
#endif

				/* Gather up all relevant subprograms */
  for(i=0,numentries=0; i<glob_symtab_top; i++) {
    if(storage_class_of(glob_symtab[i].type) == class_SUBPROGRAM
       && (glob_symtab[i].visited || !glob_symtab[i].library_prog_unit)) {
      gsymlist[numentries++] = &glob_symtab[i];
    }
  }

  if(numentries > 0) {
    (void)fprintf(list_fd,"\n\n        Cross-reference list:\n");

				/* Sort the subprograms */
    sort_gsymbols(gsymlist,numentries);

				/* Print their callers */
    for(i=0; i<numentries; i++) {
      (void)fprintf(list_fd,"\n");
      if(gsymlist[i]->internal_entry)
	(void)fprintf(list_fd,"%s entry ",gsymlist[i]->link.prog_unit->name);
      (void)fprintf(list_fd,"%s",gsymlist[i]->name);

      numcalls=0;
      args = gsymlist[i]->info.arglist;
      while(args != NULL) {		/* Gather up callers */
	if(!args->is_defn) {
				/* (eliminate duplicates) */
	  if(numcalls==0 || args->prog_unit != prog_unitlist[numcalls-1])
	    prog_unitlist[numcalls++] = args->prog_unit;
	}
	args = args->next;
      }

      if(numcalls == 0) {
	(void)fprintf(list_fd," not called");
	if(datatype_of(gsymlist[i]->type) == type_PROGRAM)
	  (void)fprintf(list_fd," (main program)");
      }
      else {
	(void)fprintf(list_fd," called by:\n");
	sort_gsymbols(prog_unitlist,numcalls); /* Sort the callers */
	print_prog_units(numcalls,prog_unitlist);
      }
    }
    (void)fprintf(list_fd,"\n");
  }
#ifdef DYNAMIC_TABLES
      (void) cfree(gsymlist);
      (void) cfree(prog_unitlist);
#endif
}


	/* Topological sort of the call tree.  Based closely on algorithm
	   on page 314 of Horowitz and Sahni, Fundamentals of Data
	   Structures.  Returns TRUE if successful, FALSE if failed
	   due to a cycle being detected.
	 */

PRIVATE int
#if HAVE_STDC
toposort(Gsymtab *gsymt, int nsym)
#else /* K&R style */
toposort(gsymt,nsym)
     Gsymtab gsymt[];
     int nsym;
#endif /* HAVE_STDC */
{
  int i,num_nodes, node_count;
  ChildList *child_list;
  Gsymtab *child_prog_unit;	/* Called prog unit's top entry point */
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
  int *parent_count;
  Gsymtab **node_list;
#else
  int parent_count[GLOBSYMTABSZ];
  Gsymtab *node_list[GLOBSYMTABSZ];
#endif

#ifdef DYNAMIC_TABLES
      if( (parent_count=(int *)calloc(glob_symtab_top,sizeof(int)))
	 == (int *)NULL ||
	 (node_list=(Gsymtab **)calloc(glob_symtab_top,sizeof(Gsymtab *)))
	 == (Gsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for prog unit sort");
      }
#endif
			/* Initialize array of links/counts */
  for(i=0; i<nsym; i++)
    parent_count[i] = 0;	/* In-order of prog unit as node */

			/* Traverse child lists, incrementing their
			   parent counts.
			 */
  for(i=0,num_nodes=0; i<nsym; i++) {
    if(gsymt[i].visited_somewhere) { /* skip entry pts and com blocks */
      ++num_nodes;
      child_list = gsymt[i].link.child_list;
      while(child_list != NULL) {
				/* If child is an internal entry, substitute
				   top entry point of its subprogram unit. */
	if( (child_prog_unit=child_list->child)->internal_entry )
	  child_prog_unit = child_prog_unit->link.prog_unit;
	++parent_count[child_prog_unit - gsymt]; /* index into table */
	child_list = child_list->next;
      }
    }
  }

  {				/* Start of the sort */
    int top=0;
    int j,k;

    for(i=0; i<nsym; i++) {
      if(gsymt[i].visited_somewhere && parent_count[i] == 0) {
	parent_count[i] = top;	/* Link now-parentless prog unit into stack */
	top = i+1;
      }
    }
    for(i=0,node_count=0; i<num_nodes; i++) {
      if(top == 0) {
	if(print_topo_sort) {
	  (void)fprintf(list_fd,"\nCall tree has a cycle");
	  print_cycle_nodes(gsymt,nsym,node_list,node_count,parent_count);
	}
	break;
      }
      j = top-1;
      top = parent_count[j];	/* Recover the link */

				/* Print the next prog unit */
      if(print_topo_sort) {
	node_list[node_count++] = &gsymt[j];
	parent_count[j] = -1;
      }
			/* Decrease parent count of its children */
      child_list = gsymt[j].link.child_list;
      while(child_list != NULL) {
	if( (child_prog_unit=child_list->child)->internal_entry )
	  child_prog_unit = child_prog_unit->link.prog_unit;
	k = child_prog_unit - gsymt;
	if(--parent_count[k] == 0) { /* Now parentless? Stack it*/
	  parent_count[k] = top;
	  top = k+1;
	}
	child_list = child_list->next;
      }
    }
  }/*end sort*/

  if(print_topo_sort && node_count > 0) {
    (void)fprintf(list_fd,"\nList of called prog units in prerequisite order:\n");
    print_prog_units(node_count,node_list);
    (void)fprintf(list_fd,"\n");
  }

#ifdef DYNAMIC_TABLES
  (void) cfree(parent_count);
  (void) cfree(node_list);
#endif

  return (node_count==num_nodes);	/* Success = TRUE */
}

		/* Traces back to find nodes not listed in topological
		   sort.  They are the cycle nodes and their descendants.
		 */
PRIVATE void
#if HAVE_STDC
print_cycle_nodes(Gsymtab *gsymt, int nsym, Gsymtab **node_list, int node_count, int *parent_count)
#else /* K&R style */
print_cycle_nodes(gsymt,nsym,node_list,node_count,parent_count)
     Gsymtab gsymt[];
     int nsym;
     Gsymtab *node_list[];
     int node_count;
     int parent_count[];
#endif /* HAVE_STDC */
{
  int i;
  int k=node_count;
  for(i=0; i<nsym; i++) {
    if(gsymt[i].visited_somewhere) {
      if(parent_count[i] != -1)	/* Not tagged */
	node_list[k++] = &gsymt[i];
    }
  }
  if(k > node_count)
    (void)fprintf(list_fd," containing some of the following prog units:\n");
  print_prog_units(k-node_count,node_list+node_count);
}


				/* Insertion sort of child list.
				   Also removes duplicates which
				   can be introduced via multiple
				   defns or via project files. */
PRIVATE ChildList *
#if HAVE_STDC
sort_child_list(ChildList *child_list)
#else /* K&R style */
sort_child_list(child_list)
     ChildList *child_list;
#endif /* HAVE_STDC */
{
 if( call_tree_sort ) {
  ChildList *front,*prev,*next,*cl=child_list;
  Gsymtab *temp;
  prev = NULL;
  while(cl != NULL) {
			/* Scan thru list for lexicographically lowest name */
    front=cl;
    for(next=cl->next; next != NULL; next = next->next) {
      if(strcmp(front->child->name,next->child->name) > 0) {
	front = next;
      }
    }
			/* Swap child pointers so front is first */
    if(front != cl) {
      temp = front->child;
      front->child = cl->child;
      cl->child = temp;
    }
			/* If duplicate, remove from list */
    if(prev != NULL && prev->child == cl->child)
      prev->next = cl->next;
    else
      prev = cl;
    cl = cl->next;
  }
  return child_list;

 }
 else  /* put children in program order, i.e. reverse the list */
 {
  ChildList *curr,*next,*temp;
  if(child_list == NULL)
    return child_list;
  curr = child_list;
  next = curr->next;
  while(next != NULL) {
    temp = next->next;
    next->next = curr;		/* switch the pointers to point in reverse */
    curr = next;
    next = temp;
  }
  child_list->next = NULL;	/* former head is now tail */
  return curr;			/* and curr now points to new head */
 }
}

PRIVATE void
#if HAVE_STDC
print_prog_units(unsigned int n, Gsymtab **list)    /* formatting of prog unit names */
#else /* K&R style */
print_prog_units(n,list)    /* formatting of prog unit names */
	unsigned n;
	Gsymtab *list[];
#endif /* HAVE_STDC */
{
	COLNO_t col=0;
	unsigned len,j;

        for (j=0;j<n;j++){
	  if(list[j]->internal_entry) {
		 len=strlen(list[j]->link.prog_unit->name);
		 col+= len= (len<=10? 10:len) +9;
		 if (col >78){
			fprintf(list_fd, "\n");
			col = len;
		 } /* end of if */
		 fprintf(list_fd,"   %10s entry",list[j]->link.prog_unit->name);
		 len=strlen(list[j]->name)+1;
		 col+= len;
		 if (col >78){
			fprintf(list_fd, "\n");
			col = len;
		 } /* end of if */
		 fprintf(list_fd," %s",list[j]->name);
	   }
	   else {
		 len=strlen(list[j]->name);
		 col+= len= (len<=10? 10:len) +3;
		 if (col >78){
			(void)fprintf(list_fd, "\n");
			col = len;
		 } /* end of if */

		 (void)fprintf(list_fd,"   %10s",list[j]->name);
	   }


	 } /* end of for */
}
/** End of common block and variable usage checks **/

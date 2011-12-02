/*   $Id: labels.c,v 1.22 2004/12/29 19:40:21 moniot Rel $
 *   Handles defs and refs of statement labels
 *
 *   Written by Heba Elsayed
 *   Modified to use an open hash backend by Robert Landrito
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

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "plsymtab.h"

#define TOP_FILE -1               /* not an include file */                  
#define HASHSIZE 50               /* hash table size */


/* Linked-list: describe how references are used */
 
typedef struct lab_ref_list 
{
    short file_ref;               /* file referenced: = TOP_FILE
      				     if not an include file, else =
				     inctable_index */

    LINENO_t line_ref;            /* line referenced */

    int ref_type;                 /* reference type: arg, assign, do,
				     goto, or I/O (format) */

    struct lab_ref_list *next;    /* link to next element */
} Lab_ref_list;



/* A label */

typedef struct label_node {
    LABEL_t lab;                  /* label numeric value */
    short file_def;               /* file where defined; = TOP_FILE if
				     not an include file, else = 
				     inctable_index */


    LINENO_t line_def;            /* line number where defined */
    int stmt_type;                /* statement type: specification
		        	     format, or executable  */
    
    unsigned defined:1;           /* TRUE if label is defined */
    unsigned referenced:1;        /* TRUE if label is referenced */
    unsigned do_label:1;	  /* TRUE if label used in DO statement */
    Lab_ref_list *how_used;       /* list describing each ref to label */
    struct label_node *next;      /* pointer to next label in linked list */
} Lab;


/* >>>>> function prototypes <<<<< */

PRIVATE unsigned int hash_label(LABEL_t label);

PRIVATE int insert_lab(LABEL_t label);

PRIVATE void ref_error_check(char *mod_name, int type1, int type2, int type3,
			     Lab *label); 

PRIVATE void print_msg(LABEL_t label, short file, 
		       LINENO_t line, const char string[]);


PRIVATE void bubble_sort(int n);

PRIVATE void swap(Lab *x, Lab *y);

PRIVATE int compare_labs(const void *a, const void *b);

PRIVATE Lab_ref_list *reverse_ref_list(Lab_ref_list *t);

PRIVATE int find_label(LABEL_t label);

PRIVATE void clear_footnotes(void);

PRIVATE void print_key(int num_footnotes);


/* >>>>> global variables <<<<< */

PRIVATE int num_labels;                  
   /* current number of entries in labtable */  

PRIVATE Lab *labtable;  
   /* table of labels defined or used: mallocked at run-time */

PRIVATE int labtable_size=0;
  /* current size of labtable */

PRIVATE int ref_index;                 
   /* index of ref_lists */

PRIVATE int ref_lists_size = REF_LISTS_SIZE;
   /* current max size of ref_lists */

PRIVATE Lab_ref_list *ref_lists = NULL;     
   /* table of label ref_lists */

PRIVATE unsigned any_lab_defined;        
   /* used to check if any label has been defined */

PRIVATE unsigned any_lab_referenced;        
   /* used to check if any label has been referenced */

PRIVATE Lab *hash_table[HASHSIZE];
   /* array of pointers to label linked list for use in hashing scheme */

PRIVATE unsigned int last_hash_index;
   /* last hash index searched */

/*
 * hash: get a hash index
 */
PRIVATE unsigned int hash_label(LABEL_t label) {
    /* numbers used in the following random number generator obtained from
       Numerical Recipes in C: The Art of Scientific Computing */
    return ( (1861 * label + 49297) % 233280 );
}


/*
 * init_labtable: initialize space for labels in labtable
 */
void init_labtable(void) {
    int i;
    num_labels = ref_index = last_hash_index = 0;
    any_lab_defined = any_lab_referenced = FALSE;

    if ( labtable_size == 0 ) {
        labtable_size = LABTABLE_SIZE;

        if ( NULL == (labtable = (Lab *) malloc(sizeof(Lab) * labtable_size)) )
            oops_message(OOPS_FATAL, NO_LINE_NUM, NO_COL_NUM,
                    "Cannot alloc space for label table");
    } /* end (outer) if */

    /* nullify hash table */
    for (i = 0; i < HASHSIZE; i++)
        hash_table[i] = NULL;

    return;
}

/*
 * grow_labtable: double the size of the hashtable
 */
PRIVATE void grow_labtable(void) {
    Lab *old_labtable = labtable;
    int i;

    labtable_size *= 2;
    if ( NULL == ( labtable =
                (Lab *) realloc(labtable, sizeof(Lab) * labtable_size) ) )
        oops_message(OOPS_FATAL, NO_LINE_NUM, NO_COL_NUM,
                "Cannot realloc space for label tabel");

#ifdef DEBUG_LABELS
    fprintf(stderr, "DEBUG: resizing labtable to %d...", labtable_size);
#endif

    /* fix the pointers in hash_table */
    for (i = 0; i < HASHSIZE; i++)
        if (hash_table[i] != NULL)
            hash_table[i] = labtable + (hash_table[i] - old_labtable);

    /* fix pointers in linked lists */
    for (i = 0; i < num_labels; i++)
        if (labtable[i].next != NULL)
            labtable[i].next = labtable + (labtable[i].next - old_labtable);

#ifdef DEBUG_LABELS
    fprintf(stderr, "DONE\n");
#endif

    return;
}

/*
 * insert_lab: inserts new label into labtable
 */
PRIVATE int insert_lab(LABEL_t label) {

    /* check if we have any space */
    if ( num_labels >= labtable_size )
        grow_labtable();

    /* place label in linked list pointed to in hash_table */
    labtable[num_labels].next = hash_table[last_hash_index];
    hash_table[last_hash_index] = &labtable[num_labels];

    /* set up the label */
    labtable[num_labels].how_used = NULL;

    labtable[num_labels].defined = FALSE;
    labtable[num_labels].referenced = FALSE;
    labtable[num_labels].do_label = FALSE;

    labtable[num_labels].file_def = TOP_FILE;
    labtable[num_labels].line_def = NO_LINE_NUM;
    labtable[num_labels].stmt_type = LAB_NO_TYPE;

    labtable[num_labels].lab = label;

    ++num_labels;

    return (num_labels - 1);
}

/*
 * sort_labtable
 * WARNING: THIS RENDERS THE HASH TABLE USELESS
 *          DO *NOT REFERENCE THE HASH TABLE AFTER
 *          CALLING THIS FUNCTION, UNLESS init_labtable()
 *          IS FIRST CALLED.
 */
void sort_labtable(void)
{
    int j, k;
    int count = 0;  /* number of unsorted pairs */


    for (j = 0; j < num_labels; j++) 
    {
	    /* Put ref lines into ascending order */

	    labtable[j].how_used = 
		reverse_ref_list(labtable[j].how_used);

    }
                     /* assert: i == num_labels */
    

    for (k = 1; k < num_labels; k++)
    {
	/* Count out-of-order table entries */

	if (labtable[k-1].lab > labtable[k].lab)
	{
	    count++;
	}
    }

    /* choose more efficient sorting alg., if nec. */

    if (count == 0) {}
	/* labtable is sorted */

    else if (count > 25)
    {
	qsort(labtable, num_labels, sizeof(Lab), compare_labs);
    }
    else
    {
	bubble_sort(num_labels);
    }
}


/*
 * bubble_sort: sort in ascending order
 */

PRIVATE void bubble_sort(int n)
{
    int i, j, numswaps;

    numswaps = 1;

    for (i = 0; (i < n) && (numswaps != 0); i++)
    {
	numswaps = 0;

	for (j = n - 2; j >= i; j--)
	{
	    if (labtable[j].lab > labtable[j+1].lab)
	    {
		swap(&labtable[j], &labtable[j+1]);
		numswaps++;
	    }
	}
    }
}


/*
 * swap
 */

PRIVATE void swap(Lab *x, Lab *y)
{
    Lab temp = *x;
    *x = *y;
    *y = temp;
}


/*
 * compare_labs: for qsort routine
 */

PRIVATE int compare_labs(const void *v1, const void *v2)
{
    Lab *a, *b;
    
    a = (Lab *)v1;
    b = (Lab *)v2;
    
    return (a->lab - b->lab);
}


/*
 * print_label_refs: print label cross-references
 */

void print_label_refs(void)
{
    int k;                     /* LCV */
    int num_refs;              /* number of refs in how_used */
    int n;                     /* number of refs printed so far */
    int per_line = 4;          /* number of refs to print per line */
    int footnote_num = 0;      /* number of footnotes--for include files */
    char buf[13];              /* holds label enclosed in <> */

    unsigned per_line_decreased = FALSE; /* for output formatting */

    if (!any_lab_referenced)   /* nothing to print! */
    {
	return;
    }

    clear_footnotes();

    (void)fprintf(list_fd, "\n\nLabel cross-references:");

    for (k = 0; k < num_labels; k++)
    {
	if (labtable[k].how_used != NULL)
	{
	    Lab_ref_list *how_used;
	    (void)sprintf(buf, "<%d>:", labtable[k].lab); 
	    (void)fprintf(list_fd, "\n\n%8s", buf); 

	    /* count refs */
	    
	    num_refs = 0;
	    for (how_used = labtable[k].how_used; how_used != NULL; 
		 how_used = how_used->next, num_refs++) {}
	    
	    /* print ref list */
	    
	    n = 0;
	    for (how_used = labtable[k].how_used; how_used != NULL; 
		 how_used = how_used->next, n++)
	    {
		if ((n > 0) && (n < num_refs))
		{
		    (void)fprintf(list_fd, ",");
		}

		/* indent after first line is printed */

		if ((n > 0) && (n % per_line) == 0)
		{
		    (void)fprintf(list_fd, "\n%8s", "");
		
		    if (per_line_decreased)
		    {
			per_line++;
			per_line_decreased = FALSE;
		    }
		}

		(void)fprintf(list_fd, " (line %d", how_used->line_ref);


		/* include file: print footnote */

		if (how_used->file_ref != TOP_FILE)
		{
		    /* is this a new include file? */

		    if (incfile_list[how_used->file_ref].footnote == 0)
		    {
			footnote_num++;
			incfile_list[how_used->file_ref].footnote
			    = footnote_num;
		    }
		
	      	    (void)fprintf(list_fd, "[%d]", footnote_num);

   		    if (!per_line_decreased)
		    {
			per_line--;
			per_line_decreased = TRUE;
		    }
		}

		(void)fprintf(list_fd, ": %s)",
			      lab_type_name[how_used->ref_type]);
	    }
	    
	}
    }

    (void)fprintf(list_fd, "\n");

    if (footnote_num > 0 )
    {
	print_key(footnote_num);
    }
}
		


/*
 * reverse_ref_list: reverse order of items 
 *                   in a Lab_ref_list
 */

PRIVATE Lab_ref_list *reverse_ref_list(Lab_ref_list *t)
{
    Lab_ref_list *curr, *next, *temp;
    
    if (t == NULL)
    {
	return t;
    }
    curr = t;
    next = curr->next;

    while (next != NULL) 
    {
	temp = next->next;
	next->next = curr;
	curr = next;
	next = temp;
    }
    t->next = NULL;	  /* former head is now tail */
    return curr;	  /* curr now points to new head */
}

		    
/*
 * print_labels
 */

void print_labels(void)
{
    int i, k;
    int per_line = 3;       /* number of labels to print per line */
    char buf[13];           /* holds label enclosed in <> */
    int footnote_num = 0;   /* number of footnotes--for include files */

    if (!any_lab_defined)   /* no labels to print */
    {
	return;
    }

    clear_footnotes();

    (void)fprintf(list_fd, "\n\nStatement labels defined:\n\n");

    for (i = 0; (i < per_line) && (i < num_labels); i++)
    {
	(void)fprintf(list_fd, "%4sLabel   Line  StmtType", "");
    }

    for (k = 0; k < num_labels; k++)
    {
	if ((k % per_line) == 0)
	{
	    (void)fprintf(list_fd, "\n");
	}

	if (labtable[k].line_def != NO_LINE_NUM)
	{
	    (void)sprintf(buf, "<%d>", labtable[k].lab);
	    (void)fprintf(list_fd, "%9s", buf);

	    
	     /* include file: print footnote */

	    if (labtable[k].file_def != TOP_FILE) 
                                    
	    {
		/* is this a new include file? */

		if (incfile_list[labtable[k].file_def].footnote == 0)
		{
		    footnote_num++;
		    incfile_list[labtable[k].file_def].footnote = footnote_num;
		}
		(void)fprintf(list_fd, "%4d[%d]", 
			      labtable[k].line_def, footnote_num);
	    }

	    else
	    {
		(void)fprintf(list_fd, "%7d", labtable[k].line_def);
	    }

	    (void)fprintf(list_fd,"%10s", 
			  lab_type_name[labtable[k].stmt_type]);

	}
    }

    (void)fprintf(list_fd, "\n"); 
    
    if (footnote_num > 0)
    {
	print_key(footnote_num);
    }
}

/*
 * find_label
 */

PRIVATE int find_label(LABEL_t label)
{  
    Lab *label_ptr;

    /* index value of linked list where "label" is a part of */
    last_hash_index = (hash_label(label) >> 9) % HASHSIZE;
    label_ptr = hash_table[last_hash_index];

    while ( label_ptr != NULL && label_ptr->lab != label )
        label_ptr = label_ptr->next;

    if ( label_ptr == NULL )    /* not found! */
        return NO_LABEL;

    /* return the index of *labtable* */
    return (label_ptr - labtable);
}

/*
 * def_label.  Called where label is attached to a statement.  Returns
 *    TRUE if the label marks end of a DO range.
 */

int def_label(Token *t, int type)
{
    LABEL_t label = (LABEL_t)(t->value.integer);
    LINENO_t line_number = (LINENO_t)t->line_num;
    
    int i = find_label(label);        /* label's index */
    
    if (i == NO_LABEL)  /* label never defined/used */
    {
	i = insert_lab(label);
    }
    
    any_lab_defined = TRUE;

    /* set appropriate fields */

    labtable[i].file_def = inctable_index;
    labtable[i].line_def = line_number;  
    
    if (!labtable[i].defined)
    {
	labtable[i].defined = TRUE;
    }
    else   /* error--duplicate label */
    {
	syntax_error(line_number, t->col_num, "Label previously defined");
    }    
    
    labtable[i].stmt_type = type;
    return labtable[i].do_label;
}

/*
 * def_do_label
 */

void def_do_label(Token *t)
{
    LABEL_t label = (LABEL_t)(t->value.integer);

    int i = find_label(label);        /* label's index */ 


    if (i == NO_LABEL)  /* label has to be defined by now */
    {
	oops_message(OOPS_FATAL,t->line_num,t->col_num,
		     "def_do_label called for undefined label");
    }

    labtable[i].do_label = TRUE;
}

/*
 * ref_label
 */

void ref_label(Token *t, int type)
{
    LABEL_t label = (LABEL_t)(t->value.integer);
    LINENO_t line_number = (LINENO_t)t->line_num;

    int i = find_label(label);        /* label's index */ 


    if (i == NO_LABEL)  /* label never defined/used */
    {
	i = insert_lab(label);
    }

    any_lab_referenced = TRUE;
    
    
    /* add entry to how_used */
    
    if (ref_lists == NULL)   /* allocate array space for our ref_lists */
    {
	ref_lists = (Lab_ref_list *) malloc(ref_lists_size *
					    sizeof(Lab_ref_list));
	if (ref_lists == NULL)
	{
		oops_message(OOPS_FATAL, NO_LINE_NUM, NO_COL_NUM,
			     "unable to allocate memory for ref_lists");
	}
    }
    
    if (ref_index == ref_lists_size) /* out of ref_list space: 
					double size of memory chunk */
    {
	Lab_ref_list *oldbase=ref_lists; /* for fixing pointers later */
	ref_lists_size *= 2;
	ref_lists = (Lab_ref_list *) realloc(ref_lists,
					 ref_lists_size*sizeof(Lab_ref_list));
	if (ref_lists == NULL)
	{
	    oops_message(OOPS_FATAL, NO_LINE_NUM, NO_COL_NUM,
			 "unable to reallocate memory for ref_lists");
	}
			/* If realloc moved the array, fix all the pointers
			   that refer to it.
			*/
	if(oldbase != ref_lists) {
	  int j;
	  for(j=0; j<ref_index; j++) {
	    if( ref_lists[j].next != NULL ) {
	      ref_lists[j].next = (ref_lists[j].next-oldbase)+ref_lists;
	    }
	  }
	  for(j=0; j<labtable_size; j++ ) {
	    if( labtable[j].lab != NO_LABEL && labtable[j].how_used != NULL ) {
	      labtable[j].how_used = (labtable[j].how_used-oldbase)+ref_lists;
	    }
	  }
	}
    }

    ref_lists[ref_index].file_ref = inctable_index;
    ref_lists[ref_index].line_ref = line_number;
    ref_lists[ref_index].ref_type = type;
    
    ref_lists[ref_index].next = labtable[i].how_used;
    labtable[i].how_used = ref_lists + ref_index;
    
    labtable[i].referenced = TRUE;    

    ref_index++;
}


/* 
 * check_labels: report problems with label usage
 */

void check_labels(char *mod_name)
{
    int i;
    unsigned head_printed;   /* has error header been printed? */
    
		/* does a goto refer to	a non-executable stmt? */
    if (misc_warn)
    {
	for (i = 0; i < num_labels; i++)
	{
	    if ( (labtable[i].stmt_type == LAB_FORMAT) || 
		 (labtable[i].stmt_type == LAB_SPECIFICATION) )
	    {
	    
		ref_error_check(mod_name, LAB_GOTO, LAB_DO, LAB_CALL, 
				&labtable[i]); 
	    }
	}
    }

		/* does a format-id refer to a non format? */
    if (misc_warn)
    {
	for (i = 0; i < num_labels; i++)
	{
	    if( (labtable[i].stmt_type == LAB_SPECIFICATION) ||
		(labtable[i].stmt_type == LAB_EXECUTABLE) )
	    {	
		
		ref_error_check(mod_name, LAB_IO, LAB_NO_TYPE,
				LAB_NO_TYPE, &labtable[i]); 
	    }
	}
    }

		/* does an assign refer to a specification stmt? */
    if (misc_warn)
    {
	for (i = 0; i < num_labels; i++)
	{
	    if (labtable[i].stmt_type == LAB_SPECIFICATION)
	    {

		ref_error_check(mod_name, LAB_ASSIGN, LAB_NO_TYPE,
				LAB_NO_TYPE, &labtable[i]);
	    }
	}
    }


    if (usage_label_undefined)
    {
	head_printed = FALSE;
	for (i = 0; i < num_labels; i++)
	{
	    if (!labtable[i].defined)     /* undefined label is referenced */
	    {
		Lab_ref_list *how_used;

		if (!head_printed)
		{
		    local_err_head(mod_name,
				   choose_filename(labtable[i].how_used,
						   file_ref),
				   labtable[i].how_used->line_ref,
				   (Lsymtab *)NULL, FALSE,
				   "Labels referenced but not defined:");

		    head_printed = TRUE;
		}

		/* print error msg for EACH reference to undefined
		   label */

		for (how_used = labtable[i].how_used; 
		     how_used != NULL; how_used = how_used->next)
		{
		    print_msg(labtable[i].lab, how_used->file_ref,
			      how_used->line_ref, "referenced");
		}
	    }
	}
    }

    if (usage_label_unused)
    {
	head_printed = FALSE;

	for (i = 0; i < num_labels; i++)
	{
	    if (!labtable[i].referenced)  /* defined label is unused */
	    {
		if (!head_printed)
		{
		    local_warn_head(mod_name,
				    choose_filename(&labtable[i], file_def),
				    labtable[i].line_def,
				    (Lsymtab *)NULL, FALSE,
				    "Labels defined but not used:");

		    head_printed = TRUE;
		}
		print_msg(labtable[i].lab, labtable[i].file_def,
			  labtable[i].line_def, "defined");
	    }
	}
    }
}
	

/* 
 * ref_error_check: ref_error_check is called to check whether 
 *   a non-executable stmt is referred to by a goto
 *    
 *   whether a non-format stmt is referred to by a format-id, 
 *  OR
 *   whether an assign refers to a specification statement
 *
 *   type1, type2, and type3 are reference types. 
 *
 *   if we are checking for goto errors, then type1 is LAB_GOTO, type2
 *     is LAB_DO, and type3 is LAB_CALL
 *   otherwise, type1 is either LAB_IO or LAB_ASSIGN, and type2 & type3
 *     are LAB_NO_TYPE
 */
	
PRIVATE void ref_error_check(char *mod_name, int type1, int type2, int type3,
			     Lab *label)
{
    Lab_ref_list *how_used;
    unsigned head_printed = FALSE;   /* has error header been printed? */

    for (how_used = label->how_used; how_used != NULL;  
	 how_used = how_used->next)
    {
	if ((how_used->ref_type == type1) ||
	    (how_used->ref_type == type2) ||
	    (how_used->ref_type == type3))
	{
	    if (type1 == LAB_GOTO) 
	    {
		if (!head_printed)
		{
		    local_err_head(mod_name,
				   choose_filename(how_used, file_ref), 
				   how_used->line_ref,
				   (Lsymtab *)NULL, FALSE,
				   "Goto refers to a non-executable stmt");

		    head_printed = TRUE;
		}
		print_msg(label->lab, how_used->file_ref,
			  how_used->line_ref, "referenced");
	    }
	    else if (type1 == LAB_IO)
	    {
		if (!head_printed)
		{
		    local_err_head(mod_name, 
				   choose_filename(how_used, file_ref),
				   how_used->line_ref,
				   (Lsymtab *)NULL, FALSE,
				   "Format-id refers to a non-format");

		    head_printed = TRUE;
		}
		print_msg(label->lab, how_used->file_ref,
			  how_used->line_ref, "referenced");
				   
	    }

	    else if (type1 == LAB_ASSIGN)
	    {
		if (!head_printed)
		{
		    local_err_head(mod_name, 
				   choose_filename(how_used, file_ref),	
				   how_used->line_ref,
				   (Lsymtab *)NULL, FALSE,
				   "Assign refers to a specification stmt");
		    head_printed = TRUE;
		}
		print_msg(label->lab, how_used->file_ref,
			  how_used->line_ref, "referenced");
	    }
	}
    }
}
    
	
/*
 * print_msg: prints error message detail;
 *            string is either "defined" or "referenced"
 */

PRIVATE void print_msg(LABEL_t label, short file, LINENO_t line, const char string[])
{
    char detail[25];
    (void)sprintf(detail, "    <%d> %s", label, string);
    local_detail(file, line, (char *)NULL, detail);
}


/*
 * clear_footnotes
 */

PRIVATE void clear_footnotes(void)
{
    int i;    

    for (i = 0; i < num_incfiles; i++)
    {
	incfile_list[i].footnote = 0;
    }
}


/* 
 * print_key: correlates bracketed footnote numbers
 *            with include file names
 */        


PRIVATE void print_key(int num_footnotes)
{
    int i;         /* footnote number */
    int j;         /* include file index */
    char buf[6];   /* holds bracketed footnote # */


    (void)fprintf(list_fd, "\n________\n\n");
    for (i = 1; i <= num_footnotes; i++)
    {
	(void)sprintf(buf, "[%d]", i);
        (void)fprintf(list_fd, "%5s -- in include file: ", buf);
	
        for (j = 0; j < num_incfiles; j++)
        {  
            if (incfile_list[j].footnote == i)
            {
                (void)fprintf(list_fd, "%s\n", incfile_list[j].fname); 
            }
        }
    }
}

		/* Keep track of number of statement labels use, max
		   in any subprogram and total.
		 */
void update_label_resources()
{
    if (num_labels > max_labels)
    {
	max_labels = num_labels;
    }
    tot_label_count += num_labels;
}

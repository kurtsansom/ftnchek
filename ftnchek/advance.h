/* $Id: advance.h,v 1.10 2003/10/14 21:41:08 moniot Rel $

   Declarations shared between advance.c and include.c

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

#ifdef ADVANCE
#define ADVANCE_SHARED
#else
#define ADVANCE_SHARED extern
#endif

ADVANCE_SHARED
int
	next_index,		/* Index in line of next_char */
	noncomment_line_count,	/* Number of noncomment lines read so far */
	sticky_EOF;		/* Signal to delay EOF a bit for sake
				   of error messages in include files. */

ADVANCE_SHARED
LINENO_t
	prev_line_num;		/* line number of previous input line */


		/* Standard maximum line length depends on source form. */
ADVANCE_SHARED
COLNO_t
	std_max_stmt_col;

	/* Define tab stops: NXTCOL(c,col_num) is next column after col_num.
	   This formula maps [1-8]->9, [9-16]->17, etc. if C is a tab,
	   otherwise just adds 1 to COL.
	 */
#define NXTCOL(C,COL) ((C)=='\t'?( (((COL)-1)/8+1)*8+1 ):COL+1)
	/* This is the same but counts tabs, for support of -port=tab warning */
#define PORT_NXTCOL(C,COL) ((C)=='\t'?(++tab_count, (((COL)-1)/8+1)*8+1 ):COL+1)

/* Definition of source-line buffer structure.  Source file is read in
   entirely before processing begins.
*/

struct src_line {
     char *line;		/* buffer holding source code of line */
     struct src_line *next,	/* link to next line */
		     *prev;	/* link to prev line */
     LINENO_t line_num;		/* number of line within source file */
     int start_index, end_index; /* span containing interesting source */
     COLNO_t start_col;		/* like start_index but accounts for tabs */
     unsigned
	printed: 1,		/* line has been printed */
		/* Flags indicating categories of lines */
	contin: 1,		/* line is a continuation line */
	comment: 1,		/* line is a comment line */
	blank: 1,		/* line is blank (implies comment) */
	cpp_line_directive: 1,	/* Unix #line directive */
		 /* the next flags are for f77 or f90 warnings */
	d_comment: 1,		/* line is a 'D' comment */
	f90_comment: 1,		/* line is or has an '!' comment */
	overlength: 1,		/* line exceeds std max length */
	empty_contin: 1,	/* line is an '&' on otherwise comment line */
	contin_wo_amp: 1;	/* freeform continued line lacks initial '&' */
};


typedef struct src_line srcLine;

ADVANCE_SHARED
LINENO_t prev_stmt_line_num;	/* line number of previous noncomment */

#if 0	/* this approach would require sharing advance.h just for this */
ADVANCE_SHARED
srcLine *prev_stmt_srcLine;	/* source line of previous noncomment */

/* macro to yield line number of previous noncomment. */
#define prev_stmt_line_num (prev_stmt_srcLine?prev_stmt_srcLine->line_num:(LINENO_t)1)
#endif

ADVANCE_SHARED
srcLine  *curr_srcLine,		/* rec of line being lexed */
	 *next_srcLine,		/* rec of line on deck */
	 *srcBuffer		/* 1st rec of source buffer for current file */
#ifdef ADVANCE
= (srcLine *)NULL
#endif
;

ADVANCE_SHARED
srcLine *mkhtml_bookmark;	/* used by mkhtml to find start of subprog */

PROTO(srcLine * gulp_srcfile,(FILE *fd));
PROTO(void free_srcBuffer, (srcLine *srcBuffer));

#ifdef MACINTOSH
#define ENDL '\r'
#else
#define ENDL '\n'
#endif

/* $Id: advance.c,v 1.30 2005/02/06 21:35:44 moniot Exp $

	Low-level input routines for Fortran program checker.

	Shared functions defined:
		init_scan()	Initializes an input stream.
		finish_scan()	Finishes processing an input stream.
		advance()	Reads next char, removing comments and
				handling continuation lines.
			looking_at_x Handles lookahead up to end of line:
		looking_at_cplx() Identifies complex constant.
		looking_at_keywd() Identifies assgnmt stmts vs keywords.
		looking_at_relop() Distinguishes .EQ. from .Eexp .
		flush_line_out(n) Prints lines up to line n if not already
				printed, so error messages come out looking OK.
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
#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "tokdefs.h"
#include "forlex.h"
#define ADVANCE
#include "advance.h"
#if HAVE_STRINGS_H
#include <strings.h>	/* we use strcasecmp */
#endif
#include "utils.h"

/* Note: beginning with rev 1.15 (July 2003) this code is somewhat
 * schizophrenic.  The source file is now read entirely into an
 * internal buffer before lexing starts.  Much code still assumes the
 * old method of reading just one line ahead, and there is duplicate
 * bookkeeping with respect to line numbers and to identifying comments
 * and continuations.  Hopefully in the course of time we will change
 * over completely so as to take full advantage of the new method.

 * Note added Oct 2003: as of rev 1.26 the changeover is complete.
 */


	/* Lookahead routines that scan the input line for various
	   things.  The ones that need not look ahead past end of line
	   take a string as argument, but most take a srcPosn struct.
	   This struct contains a pointer to the source buffer where
	   the line being scanned is located and an index in the line
	   of the point at which to start looking for the desired
	   entity.  The is_whatever routines return TRUE if the entity
	   at the given srcPosn satisfies the criterion. The
	   skip_whatever routines return index of the next nonspace
	   character after the expected thing, which must be there in
	   a syntactically correct program.  They also update the
	   srcPosn struct.  The given index points at the character
	   after a known lead-in (except for see_a_number, which can
	   be given the index of 1st char of number).  The
	   see_whatever routines are similar but return -1 if the
	   expected thing is not seen, which it need not be. */

typedef struct posn_struct
{
     srcLine *Line;		/* ptr to the source buffer line struct */
     int idx;			/* index of location in Line->line[] */
} srcPosn;

PROTO(PRIVATE void finish_off_srcLine,( srcLine *Line ));

PROTO(PRIVATE COLNO_t colno_of,(char *s, int c, COLNO_t startcol ));

PROTO(PRIVATE srcPosn see_a_number,( srcPosn pos, int can_be_holl ));

PROTO(PRIVATE srcPosn see_dowhile,( srcPosn pos ));

PROTO(PRIVATE srcPosn see_expression,( srcPosn pos ));

PROTO(PRIVATE srcPosn see_keyword,(srcPosn pos, char *matchstr ));

PROTO(PRIVATE srcPosn skip_balanced_parens,( srcPosn pos ));

PROTO(PRIVATE srcPosn skip_idletters,( srcPosn pos ));

PROTO(PRIVATE srcPosn skip_digits,( srcPosn pos ));

PROTO(PRIVATE srcPosn skip_quoted_string,( srcPosn pos ));

PROTO(PRIVATE srcPosn skip_hollerith,( srcPosn pos ));

PROTO(PRIVATE srcPosn skip_dummy_arg_list,( srcPosn pos ));

PROTO(PRIVATE srcPosn read_int_const,(srcPosn pos, int *value));

/* Lookahead kluge routines */

PROTO(PRIVATE void nextStmt,(srcPosn* p));

PROTO(PRIVATE void reset_attrs,(void));

PROTO(PRIVATE srcPosn parse_subprog_stmt,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_subprog_keywd,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_prefix_keywd,(srcPosn pos, int only_types));

PROTO(PRIVATE srcPosn parse_end_subprog,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_result_clause,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_dtype_spec,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_identifier,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_kind_param,(srcPosn pos, int datatype, int keyword_mandatory));

PROTO(PRIVATE srcPosn parse_char_len,(srcPosn pos, int keyword_mandatory));

PROTO(PRIVATE srcPosn parse_kind_selector,(srcPosn pos, int datatype));

PROTO(PRIVATE srcPosn parse_char_selector,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_size_spec,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_array_spec,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_array_bound_expr,(srcPosn pos, int *bound, int *status));

PROTO(PRIVATE srcPosn parse_array_bound,(srcPosn pos, int *size));

PROTO(PRIVATE srcPosn parse_attr_spec_list,(srcPosn pos));

PROTO(PRIVATE srcPosn skip_double_colon,(srcPosn pos));

PROTO(PRIVATE srcPosn parse_entity_decl_item,(const char *name, ProcInterface *interface, srcPosn pos));

PROTO(PRIVATE srcPosn parse_entity_decl_list,(const char *name, ProcInterface *interface, srcPosn pos));

PROTO(PRIVATE srcPosn parse_type_decl,(const char *name, ProcInterface *interface, srcPosn pos));

PROTO(PRIVATE srcPosn parse_attr_decl,(const char *name, ProcInterface *interface, srcPosn pos));

/* next routines do not use srcPosn for sake of efficiency */
PROTO(PRIVATE int skip_label,(const char *line, int i));

PROTO(PRIVATE srcLine *scan_for_contains,(srcLine *start_srcLine, int stop_at_end));

PROTO(PRIVATE srcPosn get_internal_interface,(const char *name, ProcInterface *interface, srcPosn pos));

PROTO(PRIVATE void get_result_decls,(const char *resultname, ProcInterface *interface, srcPosn pos));

PROTO(PRIVATE int get_alhead_defn,(Gsymtab *gsymt));

PROTO(PRIVATE void populate_interface,(ProcInterface *interface));

PROTO(PRIVATE void update_interface,(ProcInterface *interface));

#ifdef ALLOW_UNIX_CPP
PROTO(PRIVATE int take_cpp_line,( srcLine *Buf ));
#endif

void
init_scan(VOID)			/* Starts reading a file */
{
	tab_filename = NULL;
	incdepth = 0;
	inctable_index = -1;
#ifdef ALLOW_UNIX_CPP
	cpp_inc_depth = 0;
	cpp_start_of_file = TRUE;
#endif

			/* Choose source form based on filename extension,
			   unless overridden by -source=fixed or free.
			 */
	if(source_free_form) {
	    free_form = TRUE;
	}
	else if(source_fixed_form) {
	    free_form = FALSE;
	}
	else {
				/* For *.f90, or *.f95 use free form.  For all others,
				   use fixed form.
				 */
	    if( has_extension(top_filename,".f90") ||
		has_extension(top_filename,".f95") ) {
		free_form = TRUE;
	    }
	    else {
		free_form = FALSE;
	    }
	}

		/* Set max line length according to source form */
	if( free_form ) {
	    max_stmt_col = (COLNO_t)132;	/* not adjustable */
	    std_max_stmt_col = (COLNO_t)132;
	}
	else {
	    max_stmt_col = (COLNO_t)fixed_max_stmt_col; /* may be changed by -column */
	    std_max_stmt_col = (COLNO_t)72;
	}

	free_srcBuffer(srcBuffer);	/* free up memory from previous */
	mkhtml_bookmark = (srcLine *)NULL; /* invalidate mkhtml bookmark */

	srcBuffer = gulp_srcfile(input_fd); /* read the input file */

	if( srcBuffer != (srcLine *)NULL )
	    top_file_line_num = srcBuffer->line_num;
	else
	    top_file_line_num = 1;

	init_stream();

	/* Record whether file has a CONTAINS statement.  This will be
	   consulted by call_func and call_subr to decide whether to look
	   ahead for an internal defn to satisfy the call.
	*/
	file_has_contains =
	  (scan_for_contains(next_srcLine,FALSE) != (srcLine *)NULL);
}

void
init_stream(VOID)		/* Initializes a new input stream */
{
	inside_string = FALSE;
	inside_hollerith = FALSE;
	noncomment_line_count = 0;


		/* Start up with next_char pointing at first significant
		   character of source.  If source starts with a comment,
		   act as if at end of a line so readahead will pull in
		   the next noncomment line.  This will insert an extra EOS
		   at start up, but it is not counted as a statement, so
		   it will be OK.  If buffer is empty, treat as EOF.
		 */

	curr_srcLine = (srcLine *)NULL;
	curr_char = EOS;
	line_num = NO_LINE_NUM;
	col_num = NO_COL_NUM;

	next_srcLine = srcBuffer;
	if( next_srcLine == (srcLine *)NULL ) {
	     next_char = EOF;
	     next_line_num = 1;
	}
	else {
	    next_line_num = next_srcLine->line_num;
	    if( next_srcLine->comment ) {
		next_index = next_srcLine->end_index + 1;
		next_char = EOS;
	    }
	    else {			/* the usual case */
		next_index = next_srcLine->start_index;
		next_col_num = next_srcLine->start_col;
		next_char = next_srcLine->line[next_index];
	    }
	}

	prev_stmt_line_num = 0;
	sticky_EOF = TRUE;
	contin_count = 0;

	advance();		/* put 1st char in the pipeline */
}


void
finish_scan(VOID)
{
		/* clean up if no END statement at EOF */
	check_seq_header((Token *)NULL);

	(void) pop_loc_scope();	/* zero scope stack for next input file */
}

	/* next_up(c) is true if next_char == c outside of character or
	   hollerith context.   Must look at curr_char to avoid
	   being fooled by '!' without messing up on 'xxx'! either.
	   Also don't be fooled by '''!''' which is the string '!'
	   Note that inside_string does not yet reflect curr_char.
	   Test is that inside_string is true but about to become false,
	   or false and not about to become true. Think about it. */

#define next_up(c) ( (next_char==(c)) &&\
	(inside_string? (curr_char == quote_char) : !isaquote(curr_char)) &&\
	(!inside_hollerith) )

	/* char_context() is true if next_char is inside character string. */
#define char_context() (inside_string?(curr_char!=quote_char):isaquote(curr_char))


	/* Routine to move forward one character in source.  Invariants
	   that hold on entry and on exit:

	     curr_char = current significant character of source.  On
	                 entry, it is the char just lexed, on exit it is
	                 the next one to be lexed.  (Blanks are considered
	                 significant, but comments are not.)  On startup,
	                 it is EOS as if a statement had just been finished.
			 The actual char in source buffer may be substituted by
	                 something else, e.g. EOS replaces actual stmt-end.
	     curr_srcLine = source line-buffer record containing curr_char.
	     col_num   = column number of curr_char (accounting for tabs).
	     line_num  = line of source containing curr_char.
	     next_char = next significant character following curr_char.
	     next_srcLine = source line-buffer record containing next_char.
	     next_index = cursor into line buffer, location of next_char
	     next_line_num = line of source containing next_char.
	*/

void
advance(VOID)
{
    prev_char = curr_char;

    if(next_char == EOF) {	  /* Don't advance past EOF */
	if(curr_char == EOS || curr_char == EOF) {

			 /* Pause to allow parse actions at end of stmt
			    to have correct file context before popping
			    the include file.  Effect is to send an extra
			    EOS to parser at end of file. */
		  if(sticky_EOF) {
		    sticky_EOF = FALSE;
		    return;
		  }
				/* At EOF: close include file if any,
				   otherwise yield an EOF character. */
		  if( ! pop_include_file() ) {
		    curr_char = EOF;
		  }
		  return;
	}
	else {
		  curr_char = EOS;
		  return;
	}
    }

    if(curr_char == EOS)
	initial_flag = TRUE;

    curr_char = next_char;	  /* Step to next char of input */
    col_num = next_col_num;
    line_num = next_line_num;
    curr_srcLine = next_srcLine;

			/* # line directives take effect here, changing
			   the value of next_filename.  The line number
			   was already set in gulp_srcfile.
			 */
#ifdef ALLOW_UNIX_CPP
    if(next_index == 0 && next_char == '#')
	cpp_handled = take_cpp_line(next_srcLine);
#endif

    next_col_num = NXTCOL(next_char,next_col_num);

    next_index++;		/* advance to next char of input line */

			/* See if end of line has been reached.  If
			   so, then insert EOS for end of statement or
			   EOL if stmt is continued and gap is not
			   escaped by reaching max_stmt_col (fixed
			   form) or by pair of '&' (free form).
			 */
    while( next_index > next_srcLine->end_index ) {

    /* If we were here before, next_char is EOL (for continued
       stmt) or EOS (for end of statement).  In that case, advance to
       next noncomment line.
     */

	if( next_char == EOS || next_char == EOL ) {


	    /* this may not be needed now that EOS is on same line as stmt */
	  if( curr_srcLine != (srcLine *)NULL )
	      prev_stmt_line_num = curr_srcLine->line_num;


	  do {			/* loop till noncomment found */

		/* Print line about to be discarded & handle warnings deferred
		   till now.  Then skip over subsequent comments to next
		   statement line. */


				/* Keep count of consecutive continuations */
	    if( next_srcLine->contin )
	      contin_count++;
	    else
	      contin_count = 0;

	    finish_off_srcLine(next_srcLine);

				/* Get the next line */
	    next_srcLine = next_srcLine->next;

	    if( next_srcLine == (srcLine *)NULL ) { /* End of source buffer reached */
		next_char = EOF;
		return;
	    }

	/* A new source line has been obtained.
	 */
	    next_line_num = next_srcLine->line_num;



	    ++tot_line_count; /* count global lines processed */

	    if( ! next_srcLine->comment ) {

		++tot_stmt_line_count; /* global noncomment lines */
		++noncomment_line_count; /* those in just this file */
	    }
	  } while( next_srcLine->comment );

	  next_index = next_srcLine->start_index;
	  next_col_num = next_srcLine->start_col;


	  /* Obscure case of a blank continuation line */
	  if( next_srcLine->contin &&
	     next_srcLine->line[next_index] == '\0' ) {
	    next_char = EOL;
	    return;
	  }

	} /* end if next_char == EOS or EOL */

	else {
			/* Scan ahead to see if the next noncomment
			   line is a continuation, to tell whether to
			   insert an EOL or an EOS.
			 */

	    srcLine *p = next_srcLine->next;
	    while( p != NULL && p->comment ) {
		p = p->next;
	    }
	    if( p != NULL && p->contin ) {
		int i = p->start_index;

	    /* Here is where we insert an EOL between last char of a
	       stmt line and first char of continuation on next line
	       unless elided by free-form '&' pair or fixed-form
	       running right up to column 72.  The EOL is sent so that
	       there will be an embedded space at that point.
	     */

		next_char = EOL;
		if( free_form ) {
		    if( i <= p->end_index &&
			(i == 0 || p->line[i-1] != '&') ) {
			return;
		    }
		}
		else {		/* fixed form */
		    if( col_num < max_stmt_col ) {
			return;
		    }
		}

		/* This point is reached only if EOL is elided.  The
		   code will now drop to end of the while loop, with
		   next_index unchanged, so it will re-enter the loop
		   instead of sending next_char = EOL to caller.  It
		   will therefore get a new line and next_char will be
		   the first significant character of the continuation
		   line.  In all other situations the while is
		   effectively an if.  */

	    } /* end contin */
	    else { /* not a contin */
		/* If statement is not continued, send an EOS. */
		next_char = EOS;
		return;
	    }
	} /* end else next_char != EOS or EOL */

    } /* end while next_index > end_index */

    /* We now have a significant character to advance to. */

    next_char = next_srcLine->line[next_index];


				/* Handle semicolon statement separator. It
				   has to be looked for ahead and changed to
				   EOS so lexer code watching for end of
				   line coming doesn't need to deal with it.
				 */

/* I'm not sure this is correct */

    if(next_up(';')) {
	next_char = EOS;
	if(f77_semicolon) {
	    nonstandard(next_line_num,next_col_num,0,0);
	    msg_tail("semicolon statement separator");
	}
    }



}/* end advance */


	/* Routine to make sure that line gets printed if -list option
	   in effect, and issue misc warnings about the line that has
	   just finished being lexed.  These are low-level matters
	   that were identified by first pass after gulping the
	   source.
	 */

PRIVATE void
finish_off_srcLine( srcLine *Line )
{
    if( Line != (srcLine *)NULL ) {

	/* When debugging lexer, it is helpful to have lines flushed early.
	   Normally, it is best to defer flushing lines to as late a stage
	   as possible so that errors uncovered late in processing can still
	   be printed next to the source lines that produced them.
	 */
#ifdef DEBUG_FORLEX
if(debug_lexer) {
    flush_line_out(Line->line_num);
}
#endif

	if( ! Line->comment ) {

				/* Warn if stmt field has been extended
				   and the extended part has been used. */
	    if( ((f77_overlength || f90_overlength)
		 && max_stmt_col>std_max_stmt_col)
		&& Line->overlength){
		nonstandard(Line->line_num,(std_max_stmt_col+1),
			    f90_overlength,0);
		msg_tail(": significant characters past");
		if( !free_form ) {
		    msg_tail("72 columns");
		    if(f90_overlength) { /* explain still wrong */
			msg_tail("in fixed source form");
		    }
		}
		else {
		    msg_tail("132 columns in free source form");
		}
	    }
			/* Otherwise warn if any chars past 72 cols,
			   fixed format only */
	    else if(pretty_overlength
		    && !free_form
		    && Line->overlength ) {
		ugly_code(Line->line_num,(COLNO_t)73,
			  "characters past 72 columns");
	    }

			/* Issue warnings related to continuations */

	    if( Line->contin ) {
	      if( free_form ) {	/* free form cases */
		if( Line->contin_wo_amp ) {
		    COLNO_t col = colno_of(Line->line,'&',(COLNO_t)1);
		    syntax_error(Line->line_num,col,
		 "Continuation in character context must resume with '&'");
		}
		if(contin_count == 40) {
		    if(f90_continue) {
			COLNO_t col = colno_of(Line->line,'&',(COLNO_t)1);
			nonstandard(next_line_num,col,0,0);
			msg_tail(": more than 39 continuation lines");
		    }
		}
	      }
	      else {		/* fixed form cases */
		if(noncomment_line_count == 0)
		    warning(Line->line_num,(COLNO_t)6,
		    "Continuation mark found in first line of file");

		if( pretty_contin  &&
		    curr_srcLine->prev != NULL && curr_srcLine->prev->comment )
			    ugly_code(Line->line_num,(COLNO_t)6,
		    "Continuation follows comment or blank line");
				/* DEC tab continuation convention */
		if( source_dec_tab && Line->line[0] == '\t' ) {
		    if(f77_dec_tabs || f90_dec_tabs) {
			nonstandard(Line->line_num,(COLNO_t)7,f90_dec_tabs,0);
			msg_tail(": continuation mark not in column 6");
		    }
		}

		if(contin_count == 20) {
		    if(f77_20_continue || f90_continue) {
			nonstandard(Line->line_num,(COLNO_t)6,0,0);
			msg_tail(": more than 19 continuation lines");
			if(f90_continue)
			    msg_tail("in fixed source form");
		    }
		}
	      }
	    }

	} /* end of handling non-comment line warnings & errors */


			/* Warn under -f77 if ! comment is used. */

	if( f77_inline_comment && Line->f90_comment) {
	    int bang_col_num; /* column of the '!' */
	    if( Line->comment )
		bang_col_num = colno_of(Line->line,'!',(COLNO_t)1);
	    else
		bang_col_num = colno_of(&Line->line[next_index],'!',next_col_num);

	    nonstandard(Line->line_num,bang_col_num,0,0);
	    msg_tail(": inline comment");
	}

    } /* end if Line not NULL */
}

	/*  Function which returns column number of character c in line s.
	    Similar to (strchr(s,c)-s)+1 but accounts for tabs, and
	    returns NO_COL_NUM if c not found.
	 */

PRIVATE COLNO_t
colno_of(char *s, int c, COLNO_t startcol)
{
    int i;
    COLNO_t col;
    for(i=0,col=startcol; s[i] != '\0'; i++) {
	if( s[i] == c )
	    return col;
	col = NXTCOL(s[i],col);
    }
    return NO_COL_NUM;
}


	/* Here we handle Unix preprocessor lines.  The only ones
	   processed now are those that set the line number and filename.
	     Form 1: # line 10 "filename"
	     Form 2: # 10 "filename"
	   We replace next_filename and next_line_num by the
	   given values.
	 */
#ifdef ALLOW_UNIX_CPP
PRIVATE int
take_cpp_line(srcLine *Buf)
{
  int nchars, handled;
  char *filename;
  char *s = Buf->line;

  handled = FALSE;

			/* Deal only with # line directives */
  if( Buf->cpp_line_directive ) {

    handled = TRUE;

    do { ++s; } while( isspace(*s) );	/* Skip space after the '#' */

    if(strncmp(s,"line",4) == 0) {	/* Look for the keyword "line" */
	s += 4;			/* Skip the word "line" */
	while( isspace(*s) ) ++s;	/* Skip space after the word "line" */
    }

    while( isdigit(*s) ) ++s;	/* skip the line number (already used) */

    while( isspace(*s) ) ++s;	/* Skip space after the line number */

			/* Now find the filename */

    filename = (char *)NULL;

    if( *s == '"') {		/* Filename must be preceded by " */

      ++s;			/* Skip the " */

      nchars = 0;		/* Count chars in the filename */
      while( s[nchars] != '"' && s[nchars] != '\0')
	++nchars;

      if( s[nchars] == '"') {	/* Filename must be followed by " */

	s[nchars] = '\0';/* terminate it temporarily */

				/* See if this filename is on cpp_include_stack
				   at the previous level, and if so, re-use it.
				   If not, make new global space for it. */
	if( cpp_inc_depth > 0 &&
	    strcmp(s,cpp_include_stack[cpp_inc_depth-1].filename) == 0 ) {
	  filename = cpp_include_stack[cpp_inc_depth-1].filename;
	}
 	else if( strcmp(s,top_filename) == 0 ) {
 	  filename = top_filename;
	}
	else {
	  filename = new_global_string(s); /* put it in permanent space */
	}

	s[nchars] = '"'; /* restore line as it was */

      }


    }
  }/*end handling #line */

  if(handled) {
    next_top_file_line_num = next_line_num; /* save this in case it's needed */
    next_filename = filename;
  }
  else {
    next_filename = (char *)NULL;
  }


  return handled;	/* Return TRUE if it was interpreted */

}/*take_cpp_line*/
#endif


int
#if HAVE_STDC
flush_line_out(LINENO_t n)	/* Prints lines up to line #n if not yet printed */
               		/* Returns TRUE if line was printed, else FALSE */
#else /* K&R style */
flush_line_out(n)	/* Prints lines up to line #n if not yet printed */
    LINENO_t n;		/* Returns TRUE if line was printed, else FALSE */
#endif /* HAVE_STDC */
{
   srcLine *p = curr_srcLine;
   if( p != (srcLine *)NULL ) {

			/* For some low-level warnings that happen early,
			   the right place to start may be next_srcLine.
			 */
      if( p->line_num < n && p->next != (srcLine *)NULL ) {
	  p = p->next;
      }
			/* First, back up if necessary to the
			 * indicated line.  Preprocessor # line directives
			 * can cause confusion since line numbers not
			 * monotonic: don't back up in that case.
			 */
      while( p->prev != (srcLine *)NULL && p->prev->line_num >= n) {
	 if( p->prev->line_num >= p->line_num ) {
	    break;
	 }
	 p = p->prev;
      }
      if( do_list ) {
			/* Print previous lines only if do_list TRUE.
			 * Back up to last non-printed line.
			 */
	 while( p->prev != (srcLine *)NULL && ! p->prev->printed ) {
	    p = p->prev;
	 }
	 while( p != (srcLine *)NULL && p->line_num < n ) {
	    if( ! p->printed ) {
	       print_a_line(list_fd,p->line,p->line_num);
	       p->printed = TRUE;
	    }
	    p = p->next;
	 }
      }
			/* At this point we have p->line_num <= n, but
			 * line may have been printed already.  If not,
			 * then print it.
			 */
      if( p != (srcLine *)NULL && p->line_num == n ) {
	 if( ! p->printed ) {
	    print_a_line(list_fd,p->line,p->line_num);
	    p->printed = TRUE;
	 }
	 return TRUE;		/* at this point it is definitely printed */
      }
   }

   return FALSE;
}

 /* Routine used by END_processing to print the END statement and also, if
    this is the last statement of the file, any blank and comment lines
    that follow it.
  */

int
flush_end_stmt(LINENO_t n)
{
   srcLine *p = curr_srcLine;
   LINENO_t last_line_num = n;	/* this will be used if END is last line */

			/* Scan ahead to see if only comments from here
			   to end of file.
			 */
   if( p != (srcLine *)NULL ) {
       for(p=p->next; p != (srcLine*)NULL && p->comment; p = p->next) {
	   last_line_num = p->line_num;
       }
   }

   return flush_line_out( p == (srcLine *)NULL? last_line_num: n);
}

	/* Function to free up all memory allocated in a srcLine buffer. */
void
free_srcBuffer(srcLine *srcBuf)
{
    while( srcBuf != (srcLine *)NULL ) {
	srcLine *nextLine = srcBuf->next;
	if(srcBuf->line != (char *)NULL)
	    free(srcBuf->line);
	free(srcBuf);
	srcBuf = nextLine;
    }
}


	/* Functions which look ahead as far as end of stmt to see if input
	   cursor is sitting at start of a token of the given class.  Used
	   to resolve ambiguities that need more than one token of lookahead.
	   */


/* Convenience macro for accessing character at position POS */
#define CHAR_AT(POS) (POS.Line->line[POS.idx])

/* Macro to skip whitespace within significant part of stmt.  It will
   leave pos.idx as index of first nonspace after starting position
   unless only blanks follow.
 */

#define SKIP_SPACE    while(pos.idx <= pos.Line->end_index && isspace(CHAR_AT(pos))) stepPosn(&pos)



/* Function to advance a srcPosn location by one character.  Normally
   this just increments the index into the line.  If end of line
   is reached and statement is continued, the location will move to
   the first significant character of the continuation line. The
   argument p must not be NULL.
 */

PRIVATE void
stepPosn(srcPosn* p)
{
    srcLine *L = p->Line;	/* convenience variable */

    p->idx++;			/* Usual case: step to next character */

    if( p->idx > L->end_index ) { /* Reached end of significant text? */

				/* If stmt is continued, move to next line.
				   First, skip over intervening comments.
				*/
	for( L=L->next; L != (srcLine *)NULL && L->comment; L=L->next )
	    continue;
				/* Now, if it is a continuation, move to
				   beginning of significant text.
				 */
	if( L != (srcLine *)NULL && L->contin ) {
	    p->Line = L;
	    p->idx = L->start_index;
	}
    }
}

int
looking_at_cplx(VOID)
{
    srcPosn pos;

    if( next_char != EOS  &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL ) /* (rare but possible after include file) */
    {
	pos.Line = next_srcLine; /* starting position of lookahead text */
	pos.idx = next_index;

	if( (pos = see_a_number(pos,FALSE)), pos.idx < 0 )
	  return FALSE;

	SKIP_SPACE;

	if( CHAR_AT(pos) != ',' )
	  return FALSE;
	stepPosn(&pos);

	if( (pos = see_a_number(pos,FALSE)), pos.idx < 0 )
	  return FALSE;

	SKIP_SPACE;

	if(CHAR_AT(pos) != ')')
	  return FALSE;
    }
#ifdef DEBUG_IS_KEYWORD
    else {
if(debug_lexer && getenv("VERBOSE")) {
(void)fprintf(list_fd,"\nlooking_at_cplx:");
if(next_char == EOS)(void)fprintf(list_fd," at EOS");
else                (void)fprintf(list_fd," curr_srcLine = NULL");
}
    }
#endif

    return TRUE;	/* passed all the tests */

}


int
#if HAVE_STDC
looking_at_keywd(int token_class)
	                	/* Keyword class to be checked out */
#else /* K&R style */
looking_at_keywd(token_class)
	int token_class;	/* Keyword class to be checked out */
#endif /* HAVE_STDC */
{
				/* Distinguishing identifier from keyword.
				   If not sure, assumes true.   Ambiguity
				   must be resolved in current line. */
    int c;
    srcPosn pos;
    pos.Line = next_srcLine;	/* starting position of lookahead text */
    pos.idx = next_index;

    if( next_char != EOS &&	/* Looking at next line already */
	pos.Line != (srcLine *)NULL )
    {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,"\nlooking_at: curr_char=%c then %c",
curr_char,pos.Line->line[next_index]);
#endif
				/* Skip over leading
				   stuff that could be rest of identifier */

	if(isidletter(curr_char) || isdigit(curr_char) ||
	   isspace(curr_char)){
	  pos = skip_idletters( pos );
	  c = CHAR_AT(pos);	/* Store following character in c */
	  stepPosn(&pos);   /* Leave index pointing at char after c */
	}
	else {
	  c = curr_char;	/* Otherwise next input char is c */
	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," c=%c then %c",c,CHAR_AT(pos));
#endif

				/* An initial identifier followed by single
				   : has to be a construct name, followed
				   by :: has to be a keyword.  However, if
				   we are testing for DO, fail here since it
				   cannot be followed by any colons and is
				   probably DOUBLE which will be matched later.
				 */
	if( c == ':' ) {
	  while( isspace( (c=CHAR_AT(pos)) ) )
	    stepPosn(&pos);
	  return (c == ':' && token_class != tok_DO );
	} 

	if(token_class == tok_DO) {
	  int opt_comma = FALSE;

		/* DO: we must by now have skipped over optional label
		  to optional comma or over optional label and
		  variable name to = sign.  Look for expression and comma.
		  DOWHILE will be found as single keyword, but we have
		  to spot DO label WHILE(expr) here.  DO of END DO
		  is not seen here. */

	  WHILE_expected = FALSE; /* most cases do not use it */

	  if(c == ',' && isdigit(curr_char)) {
				/* Skip optional comma after label.
				   First, back up and check that we saw
				   only digits so far. Do it here since
				   this is rare and not worth cluttering
				   the foregoing code. */
	    srcPosn p;	/* temporary position variable for rescan */
	    p.Line = next_srcLine;
	    p.idx = next_index;
	    while( isdigit(p.Line->line[p.idx]) ||
		   isspace(p.Line->line[p.idx]) )
	      stepPosn(&p);
	    if(p.Line->line[p.idx] != ',')
	      return FALSE;
				/* Checks out OK: */
	    pos = skip_idletters(pos);	/* skip DO index or WHILE */
	    c = CHAR_AT(pos);
	    stepPosn(&pos);
	    opt_comma = TRUE;
	  }

	  if(c == '=') {	/* Traditional DO form */
	    pos = see_expression(pos);
	    return (pos.idx != -1 && CHAR_AT(pos) == ',') || opt_comma;
	  }
	  else {		/* Nonstandard variants */
	    if(c == '(') {
				/* DO label WHILE (expr): rescan from the
				   word DO to see if it fits. */
	      pos.Line = next_srcLine;
	      pos.idx = next_index;
	      if( (pos = see_dowhile(pos)), pos.idx != -1 )
		WHILE_expected = TRUE;
	      return WHILE_expected || opt_comma;
	    }
	    else
	      return opt_comma;	/* The comma is found only in DO forms */
	  }
	}/* end of tok_DO forms */

		/* Otherwise, look for an assignment statement.  If there
		   is no left paren, then must be an equals sign here
		   if it is an assignment statement. */
	if(c != '(') {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,"\n Conclude %s",
	(c != '=')? "keyword": "assignment stmt");
#endif
	      return (c != '=');
	}

	else {			/* sitting at parenthesis */

		/* Skip to end of balancing parenthesis. Then if = sign, it
		   must be an assignment statement.  If ( is found,
		   presumably it is an array substring assignment. So skip
		   once more to check for the = sign.) */


	pos = skip_balanced_parens(pos);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," to %c",CHAR_AT(pos));
#endif

	if(CHAR_AT(pos) == '(') {
	  stepPosn(&pos);		/* Move past the paren */
	  pos = skip_balanced_parens(pos);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," to %c",CHAR_AT(pos));
#endif

	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," conclude %s",CHAR_AT(pos)!= '='?"keyword":"variable");
#endif

	return (CHAR_AT(pos) != '=');
      }
    }
#ifdef DEBUG_IS_KEYWORD
    else {
if(debug_lexer && getenv("VERBOSE")) {
(void)fprintf(list_fd,"\nlooking_at_keywod(%s):",keytok_name(token_class));
if(next_char == EOS)(void)fprintf(list_fd," at EOS");
else                (void)fprintf(list_fd," curr_srcLine = NULL");
}
    }
#endif

				/* End of line: must be a keyword */
    return TRUE;

}/*looking_at_keywd*/

		/* This guy is called when an integer is followed by '.'
		   in cases where a real number or expression is allowed.
		   When an integer is followed by .E, it can either be a real
		   like 1.E10, or a comparison like (1.EQ.I).  We need to
		   be able to identify F90 defined-operators too.  Just
		   look for one or more letters followed by a closing '.'
		   to be sure it is an operator.
		  */
int
looking_at_relop(VOID)
{
    int c=' ';			/* initialize so no compiler warning */
    srcPosn pos;


    if( next_char != EOS &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL )
    {
	 int letter_count=0;
	 pos.Line = next_srcLine; /* starting position of lookahead text */
	 pos.idx = next_index;

	 SKIP_SPACE;		/* move to first nonblank after '.' */
	 while( pos.idx <= pos.Line->end_index &&
		isaletter(c = CHAR_AT(pos)) ) {
	   ++letter_count;
	   stepPosn(&pos);
	   SKIP_SPACE;
	 }
	 return (letter_count > 0 && c == '.');	/* require letters ended by '.' */
    }
#ifdef DEBUG_IS_KEYWORD
    else {
if(debug_lexer && getenv("VERBOSE")) {
(void)fprintf(list_fd,"\nlooking_at_relop:");
if(next_char == EOS)(void)fprintf(list_fd," at EOS");
else                (void)fprintf(list_fd," curr_srcLine = NULL");
}
    }
#endif
				/* If EOS, then it is stmt like x=1. */
    return FALSE;

}


		/* Routine called when an identifier is being taken,
		 * and an underscore is seen.  It checks if the next
		 * thing up is a character literal constant, in which
		 * case the identifier (without the underscore) is a
		 * kind parameter and the underscore is punctuation. */
int
looking_at_string(VOID)
{
    srcPosn pos;

    if( next_char != EOS &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL )
    {
      int c;
      pos.Line = next_srcLine; /* starting position of lookahead text */
      pos.idx = next_index;

      SKIP_SPACE;
      c = CHAR_AT(pos);

    /* If next nonblank thing is a quote mark then we have a hit. */
      return ( c == '\'' || c == '"' );
    }
    else
    {
      return FALSE;
    }
}


		/* This routine is called for an IMPLICIT statement if the
		   type keyword is followed by a left parenthesis, and
		   returns TRUE if what follows is a letter list and not
		   an F90 length-selector or kind-selector.  If insufficient
		   lookahead to tell, it returns TRUE as more likely.
		*/
int
looking_at_implicit_list(VOID)
{
    int c;
    srcPosn pos;

    if( next_char != EOS  &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL )
    {
      int letter_next_up;
      pos.Line = next_srcLine;	/* starting position of lookahead text */
      pos.idx = next_index;

		/* scan across a letter-list inside parentheses */
      letter_next_up = TRUE;
      c = CHAR_AT(pos);
      while( c != ')' && pos.idx <= pos.Line->end_index ) {
	if(! isspace(c) ) {
	  if(letter_next_up) {
	    if(! isaletter(c) )
	      return FALSE;	/* can't be letter-list */
	    letter_next_up = FALSE;
	  }
	  else {
	    if( c == ',' )
	      return TRUE;	/* can't be length or kind selector */
	    if( c != '-' )
	      return FALSE;	/* can't be letter-list */
	    letter_next_up = TRUE;
	  }
	}
	stepPosn(&pos);
	c = CHAR_AT(pos);
      }

      if( c == ')' )		/* skip past closing paren */
	stepPosn(&pos);

		/* Having scanned what might be a letter-list, it still could
		   be an expression e.g. M-N where M and N are parameters.
		   If we are looking at a len or kind spec, then next nonblank
		   must be the left paren of the letter list.
		 */
      SKIP_SPACE;
      c = CHAR_AT(pos);

      if(pos.idx <= pos.Line->end_index) /* c is significant */
	return (c != '(');
    }
#ifdef DEBUG_IS_KEYWORD
    else {
if(debug_lexer && getenv("VERBOSE")) {
(void)fprintf(list_fd,"\nlooking_at_implicit_list:");
if(next_char == EOS)(void)fprintf(list_fd," at EOS");
else                (void)fprintf(list_fd," curr_srcLine = NULL");
}
    }
#endif
    return TRUE;	/* End of line: guess that it isn't len/kind spec */
}

	/* see_a_number returns srcPosn with index -1 if there is
	   no valid numeric constant
	   in stmt starting at index i.  If valid number found, it
	   returns the index of the next character after the constant.
	   Leading whitespace in s is skipped.*/



PRIVATE srcPosn
see_a_number(srcPosn pos,	/* location in source buffer */
	     int can_be_holl)	/* context indication */
{
   int digit_seen = FALSE;
   srcPosn save_pos;

   SKIP_SPACE;

			/* move past optional preceding sign */
   if(CHAR_AT(pos) == '-' || CHAR_AT(pos) == '+' ) {
     stepPosn(&pos);
     SKIP_SPACE;
     can_be_holl = FALSE;
   }
   save_pos = pos;

		/* move past ddd or ddd. or .ddd or ddd.ddd */
   if(isdigit(CHAR_AT(pos)))
     digit_seen = TRUE;

   while(isdigit(CHAR_AT(pos))) {
     stepPosn(&pos);
     SKIP_SPACE;
   }
   if(CHAR_AT(pos) == 'H' && can_be_holl) {
     pos = save_pos;		/* back up to start of number on holl */
     pos = skip_hollerith(pos);
     return pos;
   }
   if(CHAR_AT(pos) == '.') {
     stepPosn(&pos);
     SKIP_SPACE;
     if(isdigit(CHAR_AT(pos)))
       digit_seen = TRUE;
     while(isdigit(CHAR_AT(pos))) {
       stepPosn(&pos);
       SKIP_SPACE;
     }
   }

		/* no digits seen: bail out now */
   if(! digit_seen) {
       pos.idx = -1;
       return pos;
   }

		/* look for exponential part.  The standard does not
		   allow D or Q, but we will, just in case. */
   if(makeupper(CHAR_AT(pos)) == 'E' || makeupper(CHAR_AT(pos)) == 'D' ||
      makeupper(CHAR_AT(pos)) == 'Q') {
     stepPosn(&pos);
     SKIP_SPACE;
     if(CHAR_AT(pos) == '+' || CHAR_AT(pos) == '-') {
       stepPosn(&pos);
       SKIP_SPACE;
     }
     if(!isdigit(CHAR_AT(pos))) {
       pos.idx = -1;
       return pos;
     }
     while(isdigit(CHAR_AT(pos)) || isspace(CHAR_AT(pos)))
       stepPosn(&pos);
   }
		/* look for optional kind parameter. */
   if( CHAR_AT(pos) == '_' ) {
     save_pos = pos;		/* in case of backout */
     stepPosn(&pos);
     SKIP_SPACE;
     if(isidletter(CHAR_AT(pos))) { /* identifier as kind param */
       pos = skip_idletters(pos);
     }
     else if(isadigit(CHAR_AT(pos))) {
       pos = skip_digits(pos);
     }
     else {
	/* if no identifier or integer after underscore, it is not a
	   kind param but we still have seen a number up to before
	   the underscore, so back up to it. */
       pos = save_pos;
     }
   }
   return pos;
}/*see_a_number*/

	/* see_dowhile looks ahead to see if the stuff following the initial
	   DO is a label and the word WHILE followed by a parenthesized expr.
	   Returns position following the DO n WHILE( ... ) if found.
	   If it fails to find that pattern, index of returned position
	   is set to -1.

	   Note that the "DO WHILE" form without a DO label is not accepted
	   here so that DOWHILE will be gotten as a single token later.
	 */

PRIVATE srcPosn
see_dowhile(srcPosn pos)
{
    int c;
				/* Skip over the label */
    while( isdigit(c=CHAR_AT(pos)) || isspace(c) )
      stepPosn(&pos);

    if(c == ',')		/* Skip optional comma */
      stepPosn(&pos);

    pos = see_keyword(pos,"WHILE");

    if( pos.idx == -1 || CHAR_AT(pos) != '(')  /* Look for the opening paren */
    {
      pos.idx = -1;
      return pos;
    }

    stepPosn(&pos);			/* skip the opening paren */
    pos = skip_balanced_parens(pos);
				/* Only = sign can follow the parens if this
				  is not a do-while. */
    if( CHAR_AT(pos) == '=' )
	 pos.idx = -1;

    return pos;

}/*see_dowhile*/

		/* Routine to look ahead for the double colon that signals
		   an F90 form of declaration.
		 */
int
see_double_colon(VOID)
{
    srcPosn pos;
    if( next_char != EOS  &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL )
    {
	pos.Line = next_srcLine;
	pos.idx = next_index;
	while(pos.idx <= pos.Line->end_index) {

				/* Skip past commas */
	  if(CHAR_AT(pos) == ',')
	    stepPosn(&pos);
				/* Check for a double colon */
	  else if(CHAR_AT(pos) == ':') {
	    do {
		stepPosn(&pos);
	    } while(isspace(CHAR_AT(pos)));
	    if(CHAR_AT(pos) == ':')
	      return TRUE;
	  }
				/* Other possible things look like exprs */
	  else {
	    int old_indx = pos.idx;
	    pos = see_expression(pos);
	    if( old_indx == pos.idx ) /* if no expr there, give up */
	      break;
	  }
	}
    }
    return FALSE;
}

	/* Crude routine to scan forward past arithmetic expressions.
	   Function invocations and array or character elements will
	   have their parentheses skipped by skip_balanced_parens;
	   outside parens a comma will cause a halt.  Returns the index
	   of the nonblank character following the expression, or
	   -1 if something non-kosher was found (e.g. a faulty number)
	   It can be confused by holleriths containing significant
	   characters, i.e. ( ) ' !  and occurring outside parentheses.
	 */
PRIVATE srcPosn
see_expression(srcPosn pos)
{
    int c;
    while(pos.idx != -1 && pos.idx <= pos.Line->end_index &&
	  (c=CHAR_AT(pos)) != '=' && c != '\0') {
	if(isidletter(c))
	  pos = skip_idletters(pos);
	else if(isdigit(c))
	  pos = see_a_number(pos,TRUE);
	else if(isspace(c))
	  stepPosn(&pos);
	else if(c == '(') {
	  stepPosn(&pos);
	  pos = skip_balanced_parens(pos);
	}
	else if(c == '+' || c == '-' || c == '/' || c == '*' || c == '.')
	  stepPosn(&pos);
	else if(c == '\'' || c == '"')	/* embedded strings confuse things */
	  pos = skip_quoted_string(pos);
	else break;
    }
    return pos;
}/*see_expression*/

	/* see_keyword returns pos with idx -1 if the line (ignoring blanks and
	   uppercasing alphabetics) does not match the given string
	   matchstr.  If it does match, idx is index of next nonspace
	   character (or stmt-terminating null). Note that given pos.idx must
	   be at start of keyword. */

PRIVATE srcPosn
see_keyword(srcPosn pos, char *matchstr)
{
    int c;
    while(*matchstr != '\0' && pos.idx <= pos.Line->end_index) {
      c=CHAR_AT(pos);
      if(! isspace(c) ) {
	if(makeupper(c) != *matchstr++) {
	  pos.idx = -1;
	  return pos;
	}
      }
      stepPosn(&pos);
    }
    if(*matchstr == '\0') {	/* Match found */
      SKIP_SPACE;
      return pos;
    }
    else {			/* No match */
       pos.idx = -1;
       return pos;
    }
}/*see_keyword*/

		/* skip_balanced_parens returns index of the nonspace character
		   following the closing ')' that balances the opening
		   '(' preceding CHAR_AT(pos), or of final nul if the
		   parentheses are not balanced within the line.
		*/
PRIVATE srcPosn
skip_balanced_parens(srcPosn pos)
{
  int depth=1;		/* nesting depth in parens */
  int prevchar = '+';	/* arbitrary punctuation */
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,"\nskipping ()...");
#endif

  while(pos.idx <= pos.Line->end_index && depth > 0) {
#ifdef INLINE_COMMENT_CHAR
    if(CHAR_AT(pos) == INLINE_COMMENT_CHAR) /* inline comment ends line */
      break;
#endif
    if(CHAR_AT(pos) == '\'' || CHAR_AT(pos) == '"') {	/* embedded strings confuse things */
      pos = skip_quoted_string(pos);
      prevchar = 'X';	/* Arbitrary non punctuation */
    }
    else if(ispunct(prevchar) && isdigit(CHAR_AT(pos))) {
      pos = skip_hollerith(pos); /* Skip hollerith or number */
      prevchar = CHAR_AT(pos);
    }
    else {
				/* Keep track of nesting */
      if     (CHAR_AT(pos) == '(') ++depth;
      else if(CHAR_AT(pos) == ')') --depth;

      if(! isspace(CHAR_AT(pos)) )
	prevchar = CHAR_AT(pos);

      stepPosn(&pos);
    }
  }

				/* We are now past the closing paren.
				   skip trailing space */
  SKIP_SPACE;

  return pos;
}/*skip_balanced_parens*/


		/* skip_idletters returns index of the nonspace character
		   following a string of idletters: alphabetic characters
		   or digits, or underscore or dollar if those options are
		   enabled.  It does not look out for hollerith constants.
		*/
PRIVATE srcPosn
skip_idletters(srcPosn pos)
{
	int c;
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,": skipping letters...");
#endif
	while(c=CHAR_AT(pos),
	      (isidletter(c) || isadigit(c) || isspace(c)))
	  stepPosn(&pos);
	return pos;
}/*skip_idletters*/

		/* skip_digits returns index of the nonspace character
		   following a string of digits. */
PRIVATE srcPosn
skip_digits(srcPosn pos)
{
	int c;
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,": skipping digits...");
#endif
	while(c=CHAR_AT(pos),
	      (isadigit(c) || isspace(c)))
	  stepPosn(&pos);
	return pos;
}/*skip_idletters*/
		/* Returns index of nonspace character following
		   quote mark that closes string whose opening quote
		   mark is before index. */
PRIVATE srcPosn
skip_quoted_string(srcPosn pos)
{
  int c;
  int start_quote_char = CHAR_AT(pos);	/* get opening quote char: ' or " */
  stepPosn(&pos);
  while( pos.idx <= pos.Line->end_index ) {
    c = CHAR_AT(pos);
    if( source_unix_backslash && c == '\\' ) {	/* skip any escaped char */
      stepPosn(&pos);
      if( pos.idx <= pos.Line->end_index )	/* (check just in case) */
	  break;
    }
    if(c == start_quote_char) {	/* Closing quote? */
      stepPosn(&pos);
      if(CHAR_AT(pos) != start_quote_char) /* Quoted quote? */
	break;
    }
    stepPosn(&pos);
  }

				/* We are now past the closing quote mark.
				   Skip trailing space */
  SKIP_SPACE;

  return pos;
}/*skip_quoted_string*/


/* Routine to read an integer constant starting at the current
   position.  Returns pos at next nonspace character after last digit.
   Places value of the integer in the value argument.  This should
   only be called when a digit is at position.
 */
PRIVATE
srcPosn read_int_const(srcPosn pos, int *value)
{
    int v=0;
    int c;
    while(isdigit(c=CHAR_AT(pos))) {
      v = v*10 + BCD(c);
      stepPosn(&pos);
      SKIP_SPACE;
    }
    *value = v;
    return pos;
}


			/* Skips holleriths.  Note: treats tabs within
			   hollerith as single characters. */
PRIVATE srcPosn
skip_hollerith(srcPosn pos)
{
  int len;
  pos = read_int_const(pos, &len); /* read the length */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd,"\nskip_hollerith: %d then %c:",
len,CHAR_AT(pos));
#endif
  if(makeupper(CHAR_AT(pos)) != 'H')
    return pos;

  stepPosn(&pos);				/* Skip the 'H' */

  while(pos.idx <= pos.Line->end_index && len > 0){ /* Move forward len characters */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd,"%c",CHAR_AT(pos));
#endif
    --len; stepPosn(&pos);
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd," to %c",CHAR_AT(pos));
#endif
  return pos;
}/*skip_hollerith*/


/* Skip over a procedure argument list.  Should be called when a '(' has
   been seen and eaten.  Will skip a possibly empty list of
   comma-separated identifiers and leave pos at next nonblank after
   ')'.  Sets pos.idx to -1 if a non-identifier or other bogosity seen.
 */
PRIVATE
srcPosn skip_dummy_arg_list( srcPosn pos )
{
  while( pos.idx >= 0 && CHAR_AT(pos) != ')' ) {
    int c;
    if( !isidletter(c=CHAR_AT(pos)) && /* must start with a letter, */
	c != '*' ) {		       /* or be '*' for subr alternate return */
      pos.idx = -1;
    }
    else {			/* advance past the dummy arg */
      if( c == '*' ) {
	stepPosn(&pos);
      }
      else {

      /* this loop disallows embedded space in arg identifiers */
	while( isidletter(c=CHAR_AT(pos)) || isadigit(c) ) {
	  stepPosn(&pos);
	}
      }
      SKIP_SPACE;
      if( CHAR_AT(pos) == ',' ) { /* comma or close paren must follow */
	stepPosn(&pos);		/* eat the comma */
	SKIP_SPACE;
      }
    }
  }
  if( pos.idx >= 0 ) {		/* reached ')' */
    stepPosn(&pos);		/* eat it */
    SKIP_SPACE;
  }

  return pos;
}

		/* get_comments() in makehtml.c needs to be able to locate
		 * the source line where the current prog unit is defined.
		 * This routine saves a bookmark for it. Called by
		 * def_curr_prog_unit() in symtab.c.  Location can be wrong on
		 * source with #line making nonmonotonic line numbers.
		 */
void mark_prog_unit_srcline( LINENO_t line_num )
{
     mkhtml_bookmark = curr_srcLine;
     /* due to readahead, curr_srcLine may be after declaration */
     while( mkhtml_bookmark != (srcLine *)NULL &&
	    mkhtml_bookmark->line_num > line_num)
	  mkhtml_bookmark = mkhtml_bookmark->prev;
}


/* Routines for the lookahead kluge.  Pending a major rewrite of
 * ftnchek to permit two-pass processing, we solve the problem of
 * explicit context for internal subprograms by a lookahead that is
 * invoked when a call of a previously unseen subprogram occurs.  The
 * goal of the lookahead is to harvest the minimum necessary
 * information about the subprogram interface: whether a local
 * subprogram of that name exists, and what type (possibly subroutine)
 * and attributes it has.  Arguments do not need to be analyzed, since
 * checking of them occurs at wrapup time.
 * Restrictions: CONTAINS must not have embedded space or be split across
 * a contination line.  ENTRY points, allowed in module procedures, are
 * not recognized as satisfying invocations by peers.
 * Bug: evaluating KIND parameters is not done, since it would involve
 * re-implementing too much of the parser.  KIND parameters are set to
 * default kind, and kind_is_bogus flag is set if KIND is declared, so
 * that checking code knows to suppress warnings.

To do:
  find type and attrs of result var declared below FUNCTION stmt
 */

				/* buffer to hold identifier just scanned */
PRIVATE
char parsed_identifier[MAXIDSIZE+1];

/* buffers to hold procedure name and function result variable */
PRIVATE
char parsed_procname[MAXIDSIZE+1];

PRIVATE
char parsed_resultvar[MAXIDSIZE+1];

			
PRIVATE
int parsed_subprog_class,	/* keyword class of subprog that was found */
  parsed_datatype,		/* datatype of subprog */
  parsed_size,			/* size (CHARACTER len or numeric*size) */
  parsed_kind_param,		/* kind of subprog (FUNCTIONs only) */
  kind_is_bogus,		/* set if KIND spec is present */
  parsed_array_attr,		/* has array attributes */
  parsed_pointer_attr,		/* has POINTER attribute */
  parsed_target_attr,		/* has TARGET attribute */
  parsed_recursive_attr,		/* RECURSIVE attribute */
  parsed_pure_attr,			/* PURE attribute */
  parsed_elemental_attr;		/* ELEMENTAL attribute */

PRIVATE
array_dim_t parsed_array_dim;		/* array attrs of function result */

struct keywd_list {
  char* keywd;			/* keyword string */
  int class;			/* token class of keyword */
};

/* Array of subprogram keywords. */
PRIVATE
struct keywd_list subprog_keywd[] = {
  {"FUNCTION",tok_FUNCTION},
  {"SUBROUTINE",tok_SUBROUTINE},
  {"PROGRAM",tok_PROGRAM},
  {"MODULE",tok_MODULE},
  {"BLOCKDATA",tok_BLOCKDATA}
};
#define NUM_SUBPROG_KEYWDS (sizeof(subprog_keywd)/sizeof(subprog_keywd[0]))

/* Array of prefix keywords.  Type names must be first so list
   can also be used to look for just type names.
 */
PRIVATE
struct keywd_list prefix_keywd[] = {
  {"INTEGER",tok_INTEGER},
  {"REAL",tok_REAL},
  {"DOUBLEPRECISION",tok_DOUBLEPRECISION},
  {"COMPLEX",tok_COMPLEX},
  {"CHARACTER",tok_CHARACTER},
  {"LOGICAL",tok_LOGICAL},
  {"TYPE",tok_TYPE},
  {"RECURSIVE",tok_RECURSIVE},
  {"PURE",tok_PURE},
  {"ELEMENTAL",tok_ELEMENTAL},
};
#define NUM_PREFIX_KEYWDS (sizeof(prefix_keywd)/sizeof(prefix_keywd[0]))
#define NUM_TYPE_KEYWDS 7	/* includes TYPE */

/* List of attribute keywords.  Includes only those allowable for a
   function result variable.
 */
PRIVATE
struct keywd_list attr_keywd[] = {
  {"DIMENSION",tok_DIMENSION},
  {"POINTER",tok_POINTER},
  {"TARGET",tok_TARGET},
};
#define NUM_ATTR_KEYWDS (sizeof(attr_keywd)/sizeof(attr_keywd[0]))


/* Function to advance a srcPosn location to next statement.  Mainly
   this advances p->Line to p->Line->next, but also skips continuation lines
   of current statement and comment lines.  Must have p->Line != NULL
   initially, but it may be NULL upon return if end of source is reached.
 */
PRIVATE void
nextStmt(srcPosn* p)
{
    for( p->Line=p->Line->next;
	 p->Line != (srcLine *)NULL && (p->Line->comment || p->Line->contin);
	 p->Line=p->Line->next )
      continue;

    p->idx = p->Line->start_index;
}

/* Routine to initialize attribute values to defaults prior to parsing. */
PRIVATE
void reset_attrs(void)
{
  parsed_datatype = type_UNDECL;
  parsed_size = size_DEFAULT; /* will be changed to 1 if CHARACTER */
  parsed_kind_param = kind_DEFAULT_UNKNOWN;
  kind_is_bogus = FALSE;	/* hope it will be default kind */
  parsed_array_attr = FALSE;
  parsed_array_dim = array_dim_info(0,0); /* scalar */
  parsed_pointer_attr = FALSE;
  parsed_target_attr = FALSE;
  parsed_recursive_attr = FALSE;	/* prefix keywords that may be seen */
  parsed_pure_attr = FALSE;
  parsed_elemental_attr = FALSE;
}

/* Routine that looks for a subprogram keyword (in subprog_keywd list)
   and returns pos at the next nonspace location after it.  Sets
   pos.idx = -1 if no keyword found.  As side effect, sets
   parsed_subprog_class to the token class of the keyword found.
 */
PRIVATE
srcPosn parse_subprog_keywd(srcPosn pos)
{
  srcPosn save_pos = pos;
  int k;
  for(k=0; k<NUM_SUBPROG_KEYWDS; k++) {
    pos = see_keyword(pos,subprog_keywd[k].keywd);
    if( pos.idx >= 0 ) {
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_subprog_keywd found %s",subprog_keywd[k].keywd);
   }
#endif
       parsed_subprog_class = subprog_keywd[k].class;
      return pos;
    }
    else {
      pos = save_pos;		/* back up to try next */
    }
  }
  pos.idx = -1;			/* no match found */
  return pos;
}

/* Routine that looks for a prefix keyword (in prefix_keywd list) and
   returns pos at the next nonspace location after it.  Sets pos.idx =
   -1 if no keyword found.  As side effect, sets parsed_datatype and
   parsed_kind to type and kind if prefix item is a type, or sets one
   of the flags parsed_{recursive,pure,elemental} to TRUE according to
   prefix item.
 */
PRIVATE
srcPosn parse_prefix_keywd(srcPosn pos, int only_types)
{
  srcPosn save_pos;
  int k, max_k;

  /* set scan to look for any prefix or, if only_types=TRUE,
     only type names */
  if( only_types )
    max_k = NUM_TYPE_KEYWDS;
  else
    max_k = NUM_PREFIX_KEYWDS;

  save_pos = pos;
  for(k=0; k < max_k; k++) {
    pos = see_keyword(pos,prefix_keywd[k].keywd);
    if( pos.idx >= 0 ) {	/* prefix keyword seen */
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_prefix_keywd found %s",prefix_keywd[k].keywd);
   }
#endif
      switch(prefix_keywd[k].class) {
      case tok_INTEGER:
	parsed_datatype = type_INTEGER;
	parsed_kind_param = kind_DEFAULT_INTEGER;
	break;
      case tok_REAL:
	parsed_datatype = type_REAL;
	parsed_kind_param =  kind_DEFAULT_REAL;
	break;
      case tok_DOUBLEPRECISION:
	parsed_datatype = type_DP;
	parsed_kind_param =  kind_DEFAULT_DP;
	break;
      case tok_COMPLEX:
	parsed_datatype = type_COMPLEX;
	parsed_kind_param =  kind_DEFAULT_REAL;
	break;
      case tok_CHARACTER:
	parsed_datatype = type_STRING;
	parsed_kind_param =  kind_DEFAULT_CHARACTER;
	break;
      case tok_LOGICAL:
	parsed_datatype = type_INTEGER;
	parsed_kind_param =  kind_DEFAULT_LOGICAL;
	break;
      case tok_TYPE:
	pos = parse_dtype_spec(pos);
	if( pos.idx < 0 )
	  return pos;	/* bail out if it fails */
	break;
      case tok_RECURSIVE:
	parsed_recursive_attr = TRUE;
	break;
      case tok_PURE:
	parsed_pure_attr = TRUE;
	break;
      case tok_ELEMENTAL:
	parsed_elemental_attr = TRUE;
	break;
      default:
	break;		/* can't happen */
      }/* end switch */

		/* For applicable types, look for *size or (kind-selector) */
      switch(prefix_keywd[k].class) {
      case tok_INTEGER:
      case tok_REAL:
      case tok_COMPLEX:
      case tok_LOGICAL:
	if( CHAR_AT(pos) == '*' )
	  pos = parse_size_spec(pos);
	else
	  pos = parse_kind_selector(pos,parsed_datatype); /* will do nothing if absent */
	break;
	/* For CHARACTER type, look for *size or (char-selector) */
      case tok_CHARACTER:
	parsed_size = 1;	/* default size for character */
	if( CHAR_AT(pos) == '*' ) {
	  pos = parse_size_spec(pos);
  /* Standard allows optional comma here after CHARACTER*len but only
     in non-attr-based type decl.  Too much trouble to check context,
     and expect only modern code, so don't accept it.  */
	}
	else
	  pos = parse_char_selector(pos);
	break;
      }/* end switch */

      SKIP_SPACE;
      return pos;
    }
    else {			/* no match on keyword k */
      pos = save_pos;		/* back up & try next keyword */
    }
  }/* for(k=0,k<NUM_PREFIX_KEYWDS; k++)*/

  pos.idx = -1;			/* no match found */
  return pos;
}

/* Routine that looks for a subprogram definition of the form
   [prefix] subprog-class name [( [arg-list] )] [RESULT ( result-var )]
   where prefix is a list of one or more of the keywords in prefix_keywd
   and subprog-class is one of the keywords in subprog_keywd.
   Returns pos with idx >= 0 if found, idx = -1 if not found.

   As side effects, data type and attribute values are put into
   parsed_datatype, parsed_recursive_attr, parsed_pure_attr, and
   parsed_elemental_attr; and subroutine name or function result name is
   stored in parsed_identifier.
 */
PRIVATE
srcPosn parse_subprog_stmt(srcPosn pos)
{
  srcPosn save_pos;

  /* Initialize datatype and attribute values */
  reset_attrs();

#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_subprog_stmt: %s",pos.Line->line);
  }
#endif

  /* Parse as many prefix keywords as are present. */
  do {
    save_pos = pos;		/* remember start point */
    pos = parse_prefix_keywd(pos,FALSE); /* look for a match */
  } while(pos.idx >= 0);	/* keep looking for more matches */

  pos = save_pos;	    /* return to last start point */

  /* Now parse the subprog-class keyword. */
  pos = parse_subprog_keywd(pos);
  if( pos.idx >= 0 ) {    /* match found: verify name next */
      /* for non functions, data type defaults to value appropriate for class */
    switch( parsed_subprog_class ) {
    case tok_PROGRAM:
      parsed_datatype = type_PROGRAM;
      parsed_kind_param = 0;
      break;
    case tok_SUBROUTINE:
      parsed_datatype = type_SUBROUTINE;
      parsed_kind_param = 0;
      break;
    case tok_MODULE:
      parsed_datatype = type_MODULE;
      parsed_kind_param = 0;
      break;
    case tok_BLOCKDATA:
      parsed_datatype = type_BLOCK_DATA;
      parsed_kind_param = 0;
      break;
    }
    if( pos.idx > pos.Line->end_index ) { /* end of statement: OK only for BLOCK DATA */
      if( parsed_subprog_class != tok_BLOCKDATA ) {
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_subprog_stmt: stmt ends here: abort match");
  }
#endif
	pos.idx = -1;
      }
    }
    else {
      /* Now find the subprog name */
      pos = parse_identifier(pos);
      if( pos.idx >= 0 ) {	/* identifier seen */
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_subprog_stmt found name %s",parsed_identifier);
   }
#endif
 	strcpy(parsed_procname,parsed_identifier); /* save the name */
	strcpy(parsed_resultvar,parsed_identifier); /* = default result var */

	/* At this point it is OK for statement to be ended if it is a
	   PROGRAM, MODULE, SUBROUTINE, or BLOCK DATA subprogram but
	   not FUNCTION.  We do not check that these rules are obeyed,
	   since it is impossible for a match that ends stmt to be
	   some other kind of statement.  Next up is ( arg-list )
	   which is optional for SUBROUTINE if arg-list is empty.
	 */
#ifdef DEBUG_LOOKAHEAD
	if(debug_latest && getenv("VERBOSE")) {
	  fprintf(list_fd,"\nparse_subprog_stmt at pos %d of %d",
		  pos.idx, pos.Line->end_index);
	  if( pos.idx <= pos.Line->end_index)
	    fprintf(list_fd,": %s",pos.Line->line+pos.idx);
	}
#endif
	if( pos.idx <= pos.Line->end_index ) { /* not end of statement */
	  if( CHAR_AT(pos) == '(' ) { /* (arg-list) present */
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE") && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_subprog_stmt skipping balanced parens");
   }
#endif
	    stepPosn(&pos);		     /* eat the '(' */
	    SKIP_SPACE;
	    pos = skip_dummy_arg_list(pos); /* we do not harvest dummy args */
	  }
	  if( pos.idx <= pos.Line->end_index ) { /* still not end: RESULT clause next */
	    pos = parse_result_clause(pos); /* this will fail if it is an asgmt stmt after all */
	  }
	}
      }/* end parsing after proc name */
    }/* end parsing after subprog keywd */
  }/* end subprog keywd seen */

  return pos;
}



/* Routine that looks for an END [subprog-class [name]] where
   subprog-class = PROGRAM|MODULE|SUBROUTINE|FUNCTION|BLOCKDATA.  No
   checking done whether the optional subprog name part matches
   opener.  Returns pos with idx = index of optional name (or of
   terminal '\0' if no name) if a match is found, otherwise pos.idx =
   -1.

   This routine will look ahead past end of line if statement is
   continued.  Blank space within keywords is tolerated.
*/

PRIVATE
srcPosn parse_end_subprog(srcPosn pos)
{
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_end_subprog: %s",pos.Line->line);
  }
#endif

  pos = see_keyword(pos, "END");

  if(pos.idx >= 0) {			  /* END was seen */

    if( pos.idx > pos.Line->end_index ) { /* END is all by itself */
      return pos;
    }
    else {		/* END is followed by something: must match a keyword */
      pos = parse_subprog_keywd(pos);

      if( pos.idx >= 0 ) {    /* match found: verify end of stmt or name next */
	if( pos.idx >= pos.Line->end_index ) { /* end of statement */
	  return pos;
	}
	else {		/* not end of stmt: must be followed by identifier */
	  pos = parse_identifier(pos);
	  if( pos.idx <= pos.Line->end_index ) { /* not end of statement: abort match */
	    pos.idx = -1;
	  }
	  return pos;
	}
      }
    }
  }
  return pos;
}

/* Routine to parse the RESULT(result-var) part of a function definition.
   As a side effect, parsed_resultvar is filled with the name of the result-var.
 */
PRIVATE
srcPosn parse_result_clause(srcPosn pos)
{
  pos = see_keyword(pos,"RESULT");
  if( pos.idx >= 0 ) {		/* RESULT keyword seen */
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_result_clause sees RESULT");
  }
#endif
    if( CHAR_AT(pos) != '(' ) { /* look for opening paren */
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd," not followed by (");
  }
#endif
      pos.idx = -1;			   /* bogus */
    }
    else {
      stepPosn(&pos);		   /* eat left paren */
      SKIP_SPACE;
      pos = parse_identifier(pos); /* get the result var name */
      if( pos.idx >= 0 ) {
	if( CHAR_AT(pos) != ')' ) {
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nidentifier not followed by )");
  }
#endif
	  pos.idx = -1;				    /* bogus */
	}
	else {
	  stepPosn(&pos);	   /* eat right paren */
	  SKIP_SPACE;
	  strcpy(parsed_resultvar,parsed_identifier); /* save result var name */
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_result_clause: result var=%s",parsed_resultvar);
  }
#endif
	}
      }
    }
  }
  return pos;
}

/* Routine to parse the (type-name) part of a type-spec of the form
   TYPE(type-name).  As a side effect, parsed_datatype is set to the type
   id of the named type.
 */
PRIVATE
srcPosn parse_dtype_spec(srcPosn pos)
{
  SKIP_SPACE;
  if( CHAR_AT(pos) != '(' ) { /* expect left paren */
    pos.idx = -1;
  }
  else {
    stepPosn(&pos);		/* advance to start of name */
    SKIP_SPACE;
    pos = parse_identifier(pos);	/* this sets parsed_identifier to type-name */
    if( pos.idx >= 0 ) {		/* identifier seen: look up its type id */
      int h = hash_lookup(parsed_identifier);
      Lsymtab *type_symt = hashtab[h].loc_symtab;
      if( type_symt == (Lsymtab *)NULL ) { /* something bogus */
	pos.idx = -1;
	return pos;
      }
      else {		/* (should test that it is class_DTYPE) */
	parsed_datatype = datatype_of(type_symt->type);
      }
      SKIP_SPACE;
      if( CHAR_AT(pos) != ')' ) { /* expect closing paren */
	pos.idx = -1;
      }
      else {
	stepPosn(&pos);		/* step past the ')' and trailing space */
	SKIP_SPACE;
      }
    }
  }
  return pos;
}/*parse_dtype_spec*/


/* Routine to see if an identifier is at the starting position.  If
   found, pos is updated to the next nonspace character, otherwise
   pos.idx is set to -1.  As a side effect, parsed_identifier buffer
   is filled with a copy (uppercased and blanks removed) of the
   identifier.
 */

PRIVATE
srcPosn parse_identifier(srcPosn pos)
{
  int c;
  int idlen=0;
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_identifier: <%s>",pos.Line->line+pos.idx);
  }
#endif
  while( isidletter(c=CHAR_AT(pos)) || isadigit(c) ) {
    if( idlen < MAXIDSIZE ) {
      parsed_identifier[idlen++] = makeupper(c);
    }
    stepPosn(&pos);
  }
  parsed_identifier[idlen] = '\0';

  if( idlen == 0 )		/* no identifier seen */
    pos.idx = -1;
  else
    SKIP_SPACE;			/* move to following text */

  return pos;
}/*parse_identifier*/

/* Routine to parse a size-spec if present.  Called when '*' is seen
   at pos.  For CHARACTER type, it may be CHARACTER*(expr), and if the
   expr is a simple literal constant we will interpret it.
   Alternatively it may be the standard conforming but deprecated
   CHARACTER*len, or the vendor extension for numeric sizes
   e.g. REAL*8.  In either case the size must be an integer constant,
   which we can interpret.
 */
PRIVATE
srcPosn parse_size_spec(srcPosn pos)
{
  stepPosn(&pos);		/* eat the '*' */
  SKIP_SPACE;
  if( CHAR_AT(pos) == '(' ) {
    stepPosn(&pos);
    SKIP_SPACE;
    pos = parse_char_len(pos,FALSE);/* more general than needed here but works */
  }
  else {
    pos = read_int_const(pos, &parsed_size); /* get the size */
  }
  return pos;
}/*parse_size_spec*/


/* Routine that is called inside a kind-selector or char-selector to
   parse and evaluate, if possible without too much trouble, the kind
   parameter.  Sets pos.idx = -1 if it fails, otherwise pos is at next
   nonblank after the parameter.  Set keyword_mandatory = TRUE if
   keyword must be present for this to be a kind.

   As side effect, sets parsed_kind_param to the kind value if found
   and evaluated, or to default kind if found but not evaluated.
*/
PRIVATE
srcPosn parse_kind_param(srcPosn pos, int datatype, int keyword_mandatory)
{
  srcPosn save_pos;
  int c;
      /* first skip over KIND= if present */
  save_pos = pos;
  pos = see_keyword(pos,"KIND=");

  if( pos.idx < 0 ) {	      /* keyword not seen */
    if( keyword_mandatory ) {	/* if KIND= mandatory, do not process */
      return pos;
    }
    else {
      pos = save_pos; /* KIND= optional & not seen, return to start of value */
    }
  }

      /* Now try to read an integer at the position. */
  if( isdigit(CHAR_AT(pos)) ) {
    save_pos = pos;
    pos = read_int_const(pos,&parsed_kind_param);
    /* It is unlikely that kind is a calculated quantity, but make sure
       that no expr follows number.  Param can be followed only by
       ',' or ')' */
    if( (c=CHAR_AT(pos)) != ',' && c != ')' ) {
      parsed_kind_param = default_kind(datatype); /* give up */
      pos = see_expression(save_pos);	/* this will skip to ',' or ')' */
    }
  }
  else {
    parsed_kind_param = default_kind(datatype); /* give up */
    kind_is_bogus = TRUE;
    pos = see_expression(pos);	/* this will skip to ',' or ')' */
  }

  return pos;
}/*parse_kind_param*/

/* Routine that is called inside a char-selector to parse and
   evaluate, if possible without too much trouble, the length
   parameter.  Sets pos.idx = -1 if it fails, otherwise pos is at next
   nonblank after the parameter.  Set keyword_mandatory = TRUE if
   keyword must be present for this to be a length.
 */
PRIVATE
srcPosn parse_char_len(srcPosn pos, int keyword_mandatory)
{
  srcPosn save_pos;
  int c;
      /* first skip over LEN= if present */
  save_pos = pos;
  pos = see_keyword(pos,"LEN=");

  if( pos.idx < 0 ) {	      /* keyword not seen */
    if( keyword_mandatory ) {	/* if LEN= mandatory, do not process */
      return pos;
    }
    else {
      pos = save_pos; /* LEN= optional & not seen, return to start of value */
    }

    pos = see_keyword(pos,"KIND="); /* keyword may be KIND instead */
    if( pos.idx >= 0 ) {	 /* if so this is not a LEN */
      pos.idx = -1;
      return pos;
    }
    else {
      pos = save_pos;		/* go ahead with implied LEN */
    }
  }

      /* Now try to read an integer at the position. */
  if( isdigit(CHAR_AT(pos)) ) {
    save_pos = pos;
    pos = read_int_const(pos,&parsed_size);
    /* Length may be a calculated quantity, so make sure that no expr
       follows.  Length can be followed only by ',' or ')'.  If not,
       then set size unknown and skip.
    */
    if( (c=CHAR_AT(pos)) != ',' && c != ')' ) {
      parsed_size = size_UNKNOWN; /* give up */
      pos = see_expression(save_pos);	/* this will skip to ',' or ')' */
    }
  }
  else {
    if( CHAR_AT(pos) == '*' ) {	/* length-selector= (*) */
      stepPosn(&pos);
      SKIP_SPACE;
      if( CHAR_AT(pos) == ')' ) {
	parsed_size = size_ADJUSTABLE;
	stepPosn(&pos);
	SKIP_SPACE;
      }
      else {
	pos.idx = -1;		/* something is bogus */
	return pos;
      }
    }
    else {
      parsed_size = size_UNKNOWN; /* give up */
      pos = see_expression(pos);	/* this will skip to ',' or ')' */
    }
  }

  return pos;
}/*parse_char_len*/


/* Routine to parse a kind-spec if present.  It is to be called with
   pos at the position that will be '(' if there is a kind-spec.
   NOTE: unlike most parse_whatever routines, this does NOT set
   pos.idx = -1 if kind-spec not seen, but leaves pos unchanged.  If
   kind-spec present, pos is left pointing at the next nonspace after
   the (kind-spec).

   As side effect, sets parsed_kind_param to the kind value if found
   and evaluated, or to default kind if found but not evaluated.

   NOTE FURTHER: because of the effort that would be required to
   evaluate the kind-spec, this kluge simply skips over it unless it
   is a concrete integer, setting kind_is_bogus flag so that checks
   can be aware that true kind value is not known.
 */

PRIVATE
srcPosn parse_kind_selector(srcPosn pos, int datatype)
{
  srcPosn save_pos;
  if( CHAR_AT(pos) == '(' ) { /* kind-selector is present */
    stepPosn(&pos);		/* eat the '(' */
    SKIP_SPACE;
    save_pos = pos;
    pos = parse_kind_param(pos,datatype,FALSE); /* get the parameter */
    if( pos.idx < 0 )				/* failed: something bogus */
      pos = save_pos;
    else {
#ifdef DEBUG_LOOKAHEAD
    if(debug_latest && getenv("VERBOSE")) {
      fprintf(list_fd,"\nparse_kind_selector found kind=%d",parsed_kind_param);
    }
#endif
      stepPosn(&pos);		/* eat the ',' or ')' at end */
      SKIP_SPACE;
    }
  }
  return pos;
}/*parse_kind_selector*/

/* Routine to parse a char-selector if present.  This is of form
   ([LEN=]len[,[KIND=]kind]) or (KIND=kind,LEN=len)

   It is to be called with pos at the position that will be '(' if
   there is a char-selector.  NOTE: unlike most parse_whatever
   routines, this does NOT set pos.idx = -1 if char-selector not seen,
   but leaves pos unchanged.  If char-selector is present, pos is left
   pointing at the next nonspace after the closing paren.

   NOTE FURTHER: because of the effort that would be required to
   evaluate the len or kind, this kluge simply skips over it unless it
   is a concrete integer, setting len to size_UNKNOWN and
   setting kind_is_bogus flag so that checks can be aware that true
   kind value is not known.
 */

PRIVATE
srcPosn parse_char_selector(srcPosn pos)
{
  srcPosn save_pos;
  char c_after_spec;

  if( CHAR_AT(pos) == '(' ) { /* char-selector is present  */
    stepPosn(&pos);		/* eat the '(' */
    SKIP_SPACE;

    /* first look for LEN= */
    save_pos = pos;
    pos = parse_char_len(pos,FALSE); /* LEN= is optional */
    if( pos.idx >= 0 ) {	/* length found */
#ifdef DEBUG_LOOKAHEAD
    if(debug_latest && getenv("VERBOSE")) {
      fprintf(list_fd,"\nparse_char_selector found len=%d",parsed_size);
    }
#endif
      c_after_spec = CHAR_AT(pos);
      stepPosn(&pos);		/* eat the ',' or ')' */
      SKIP_SPACE;

      if( c_after_spec == ',' ) {
	save_pos = pos;		/* look for optional kind after len */
	pos = parse_kind_param(pos,type_STRING,FALSE); /* KIND= is optional */
	if( pos.idx < 0 )
	  pos = save_pos;
	else {
#ifdef DEBUG_LOOKAHEAD
    if(debug_latest && getenv("VERBOSE")) {
      fprintf(list_fd,"\nparse_char_selector found kind=%d",parsed_kind_param);
    }
#endif
	  stepPosn(&pos);	/* eat the ')' */
	  SKIP_SPACE;
	}

      }
    }
    else {		     /* [LEN=]len not found */
				/* can only be KIND=kind[,LEN=len] */
      pos = parse_kind_param(save_pos,type_STRING,TRUE);

      if( pos.idx < 0 ) {		/* not found: give up */
	return save_pos;
      }
      else {
#ifdef DEBUG_LOOKAHEAD
    if(debug_latest && getenv("VERBOSE")) {
      fprintf(list_fd,"\nparse_char_selector found KIND=%d",parsed_kind_param);
    }
#endif
	c_after_spec = CHAR_AT(pos);
	stepPosn(&pos);		/* eat the ',' or ')' */
	SKIP_SPACE;
	if( c_after_spec == ',' ) {
	  save_pos = pos;			/* look for optional length */
	  pos = parse_char_len(pos,TRUE); /* LEN= is mandatory in this case */
	  if( pos.idx < 0 ) {
	    pos = save_pos;	/* not seen: back up */
	  }
	  else{
#ifdef DEBUG_LOOKAHEAD
    if(debug_latest && getenv("VERBOSE")) {
      fprintf(list_fd,"\nparse_char_selector found LEN=%d",parsed_size);
    }
#endif
	    stepPosn(&pos);		/* eat the ')' */
	    SKIP_SPACE;
	  }
	}
      }
    }
  }
  return pos;
}/*parse_char_selector*/


/* Routine to parse a possibly empty array bound expression at pos.
   If empty, sets status=ARRAY_BOUND_ABSENT.  If present, then if it
   can be evaluated, sets status=ARRAY_BOUND_KNOWN and bound=value; if
   it cannot be evaluated, sets status=ARRAY_BOUND_PRESENT.  If bound
   is '*', sets status=ARRAY_BOUND_ASSUMED_SIZE.  Returns pos
   unchanged if no bound expr seen, otherwise pointing at next
   nonblank after bound expr.
 */
#define ARRAY_BOUND_ABSENT 0	   /* no bound expr present */
#define ARRAY_BOUND_PRESENT 1	   /* present but not evaluated */
#define ARRAY_BOUND_KNOWN 2	   /* present and evaluated */
#define ARRAY_BOUND_ASSUMED_SIZE 3 /* present as '*' */
PRIVATE
srcPosn parse_array_bound_expr(srcPosn pos, int *bound, int *status)
{
  srcPosn save_pos;
  int c;
  if( (c=CHAR_AT(pos)) == ',' || c == ')' || c == ':' ) {
    (*status) = ARRAY_BOUND_ABSENT;
  }
  else {
    save_pos = pos;
    if( isdigit(CHAR_AT(pos)) ) { /* attempt to read int literal value */
      pos = read_int_const(pos,bound);
      /* make sure this is end of expr */
      if( (c=CHAR_AT(pos)) == ',' || c == ')' || c == ':' ) {
	(*status) = ARRAY_BOUND_KNOWN;
	return pos;
      }
    }
    /* Bound present but couldn't evaluate it.  Skip to end of bound expr */
    if( CHAR_AT(pos) == '*' ) {
      (*status) = ARRAY_BOUND_ASSUMED_SIZE;
      stepPosn(&pos);
      SKIP_SPACE;
    }
    else {
      (*status) = ARRAY_BOUND_PRESENT;
      save_pos = pos;
      pos = see_expression(save_pos);	/* this will skip to ':' ',' or ')' */
      if(pos.idx < 0) {		/* something bogus: bail out */
	pos = save_pos;
	while(pos.idx < pos.Line->end_index &&
	      (c=CHAR_AT(pos)) != ',' && c != ')' && c != ':' )
	  stepPosn(&pos);
      }
    }
  }
  return pos;
}/*parse_array_bound_expr*/

/* Routine to parse an array bound of form [expr][:[expr]] Returns pos
   at location of comma or right paren following the bound.  The size
   argument is set to length of bound if [both] expr[s] are literal
   integer constants, otherwise one of size_UNKNOWN size_ASSUMED_SIZE
   size_ASSUMED_SHAPE size_DEFERRED.
 */
PRIVATE
srcPosn parse_array_bound(srcPosn pos, int *size)
{
  int lbound, ubound;
  int lbound_status, ubound_status;
  int colon_present=FALSE;
  pos = parse_array_bound_expr(pos,&lbound,&lbound_status);
  if( CHAR_AT(pos) == ':' ) {
    colon_present = TRUE;
    stepPosn(&pos);
    SKIP_SPACE;
    pos = parse_array_bound_expr(pos,&ubound,&ubound_status);
  }
  else {
    ubound_status = ARRAY_BOUND_ABSENT;
  }
  /* Cases:
     A ( [l:]u ) EXPLICIT SHAPE
     A ( [l:]* ) ASSUMED SIZE
     A ( [l]: )  ASSUMED_SHAPE
   Note that if : is absent, u gets stored in lbound.
   */
  if( lbound_status == ARRAY_BOUND_ASSUMED_SIZE || /* no colon */
      ubound_status == ARRAY_BOUND_ASSUMED_SIZE ) { /* with colon */
    (*size) = size_ASSUMED_SIZE;
  }
  else if( lbound_status == ARRAY_BOUND_KNOWN ) {
    if( ubound_status == ARRAY_BOUND_ABSENT ) {
      if( colon_present )
	(*size) = size_ASSUMED_SHAPE;
      else
	(*size) = lbound;	/* explicit shape w/o colon */
    }
    else if( ubound_status == ARRAY_BOUND_KNOWN )
      (*size) = (ubound-lbound+1);
    else
      (*size) = size_UNKNOWN;
  }
  else {
    if( colon_present )		/* (:) form */
      (*size) = size_ASSUMED_SHAPE;
    else			/* (u) form */
      (*size) = size_UNKNOWN;
  }

  return pos;
}/*parse_array_dim*/

/* Routine to parse an array spec dimensioning an array.  Given pos at
   the left parenthesis.
 */
PRIVATE
srcPosn parse_array_spec(srcPosn pos)
{
  int ndims = 0;
  int dim_size, array_size = 1;
  if( CHAR_AT(pos) == '(' ) {
    stepPosn(&pos);		/* eat the '(' */
    SKIP_SPACE;
    for(;;) {			/* loop thru dim bound */
      pos = parse_array_bound(pos,&dim_size);
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest) {
    fprintf(list_fd,"\nparse_array_spec: dim %d size %d",ndims,dim_size);
  }
#endif
      if( array_size > 0 ) {
	if( dim_size > 0 )  /* if size of dim known, figure total */
	  array_size *= dim_size;
	else /* first unknown size dim encountered */
	  array_size = dim_size;	/* size_UNKNOWN or assumed/deferred */
      }
      ndims++;			/* count number of dimensions */
      if( CHAR_AT(pos) == ',' ) {
	stepPosn(&pos);		/* eat the comma */
	SKIP_SPACE;
      }
      else {
	if( CHAR_AT(pos) == ')' ) { /* better be there */
	  stepPosn(&pos);
	  SKIP_SPACE;
	}
	break;			/* end of dims */
      }
    }
    parsed_array_attr = TRUE;
    parsed_array_dim = array_dim_info(ndims,array_size);
  }
  else {
    pos.idx = -1;
  }
  return pos;
}/*parse_array_spec*/

/* Routine to parse an attribute in a type declaration.  Given pos at
   the start of the attribute.  Sets pos.idx = -1 if attribute not
   recognized.  Note that only attributes allowable on function
   results are recognized.  As side effect, sets parsed_xxx_attr = TRUE if
   attribute xxx is seen.
 */
PRIVATE
srcPosn parse_attr_spec(srcPosn pos)
{
  srcPosn save_pos;
  int k;
  for(k=0; k<NUM_ATTR_KEYWDS; k++) {
    save_pos = pos;
    pos = see_keyword(pos,attr_keywd[k].keywd);
    if( pos.idx >= 0 ) {
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_attr_spec found %s",attr_keywd[k].keywd);
  }
#endif
      switch(attr_keywd[k].class) {
      case tok_DIMENSION:
	pos = parse_array_spec(pos); /* sets parsed_array_dim */
	break;
      case tok_POINTER:
	parsed_pointer_attr = TRUE;
	break;
      case tok_TARGET:
	parsed_target_attr = TRUE;
	break;
      }
      return pos;		/* success */
    }
    else {
      pos = save_pos;		/* back up & try next keyword */
    }
  }
  pos.idx = -1;			/* no keyword recognized */
  return pos;
}/*parse_attr_spec*/

/* Routine to parse a list of attributes in a type declaration.  Given
   pos at the start of the first attr after comma that follows
   type-spec.  Sets pos.idx = -1 if list not found.  Otherwise pos is
   set to next nonblank after last attribute keyword.  As side effect,
   parsed_array_attr, parsed_array_dim, parsed_pointer_attr, and
   parsed_target_attr are set if the corresponding attrs are found.
 */
PRIVATE
srcPosn parse_attr_spec_list(srcPosn pos)
{
  for(;;) {
    pos = parse_attr_spec(pos);	/* look for an attr-spec */
    if( pos.idx >= 0 &&		/* found one */
	CHAR_AT(pos) == ',') {	/* check for ',' after attr */
	stepPosn(&pos);		/* eat the ',' */
	SKIP_SPACE;
    }
    else {
      break;			/* end of the list or unrecognized attr */
    }
  }

  /* If last attr was parsed ok, must be a "::" next */
  if( pos.idx >= 0 )
    pos = skip_double_colon(pos);

  return pos;
}/*parse_attr_spec_list*/

PRIVATE
srcPosn skip_double_colon(srcPosn pos)
{
  if(CHAR_AT(pos) == ':') {
    stepPosn(&pos);
    SKIP_SPACE;
    if(CHAR_AT(pos) == ':') {
      stepPosn(&pos);
      SKIP_SPACE;
      return pos;
    }
  }
  pos.idx = -1;
  return pos;
}/*skip_double_colon*/

/* Routine to parse an entity-decl item, which is
   name[(array-spec)][*char-length][initialization] If the entity name
   matches the given name, the interface is updated with the declared
   attributes.  The optional initialization is accepted here although
   invalid for function results, because result may be declared in a
   statement in same list with local variables which may have it.
 */

PRIVATE
srcPosn parse_entity_decl_item(const char *name, ProcInterface *interface, srcPosn pos)
{
  srcPosn save_pos;
  /* Variables to hold attr values declared by statement, which may be
     overridden for individual items. */
  int stmt_array_attr = parsed_array_attr, /* stmt may lack DIMENSION */
    stmt_size = parsed_size;	/* for character type */
  array_dim_t stmt_array_dim = parsed_array_dim;

  pos = parse_identifier(pos);
  if( pos.idx >= 0 ) {
    save_pos = pos;
    /* Attempt to parse array spec */
    pos = parse_array_spec(pos);
    if( pos.idx < 0 )		/* not found: back up */
      pos = save_pos;
    if( CHAR_AT(pos) == '*' ) {	/* see *char-length */
      pos = parse_size_spec(pos);      
    }
    if( pos.idx >= 0 ) {
      if( CHAR_AT(pos) == '=' ) { /* initializer */
	stepPosn(&pos);		  /* eat the '=' */
	SKIP_SPACE;
	if( CHAR_AT(pos) == '>' ) { /* => form */
	    stepPosn(&pos);	   /* eat the '>' */
	SKIP_SPACE;
	}
	pos = see_expression(pos); /* ignore contents of initializer */
      }
      if( strcmp(parsed_identifier,name) == 0 ) {
	update_interface(interface);
      }
    }
  }

  /* Restore statement default attr values */
  parsed_array_attr = stmt_array_attr;
  parsed_size = stmt_size;
  parsed_array_dim = stmt_array_dim;

  return pos;
}

/* Routine to parse an entity-decl-list.
 */
PRIVATE
srcPosn parse_entity_decl_list(const char *name, ProcInterface *interface, srcPosn pos)
{
  for(;;) {
    pos = parse_entity_decl_item(name,interface,pos);
    if( pos.idx >= 0 && CHAR_AT(pos) == ',' ) {
      stepPosn(&pos);		/* eat the ',' */
      SKIP_SPACE;
    }
    else {
      break;			/* end of list */
    }
  }

  return pos;
}/*parse_entity_decl_list*/

/* Routine to attempt to parse a type declaration referring to the
   named variable.  If no type declaration is found, pos.idx is set to
   -1.  If a type declaration is found, pos is advanced to end of
   statement.  If declaration applies to the named variable, the
   interface is updated with the declaration info.
 */
PRIVATE
srcPosn parse_type_decl(const char *name, ProcInterface *interface, srcPosn pos)
{
  srcPosn save_pos;
  reset_attrs();

  pos = parse_prefix_keywd(pos,TRUE); /* look for type-spec */
  if( pos.idx >= 0 ) {		      /* type-spec found */
    if( CHAR_AT(pos) == ',' ) {	      /* Look for ',' indicating attr-spec-list */
      stepPosn(&pos);		/* eat the ',' */
      SKIP_SPACE;
      pos = parse_attr_spec_list(pos); /* get the attributes */
    }
    else {
      save_pos = pos;		/* skip optional :: following type spec */
      pos = skip_double_colon(pos);
      if(pos.idx < 0)
	pos = save_pos;
    }
  }
  if( pos.idx >= 0 ) {
  /* Now look for identifier in entity-decl-list */
    pos = parse_entity_decl_list(name,interface,pos);
  }
  return pos;
}/*parse_type_decl*/

/* Routine to attempt to parse an attr declaration referring to the
   named variable.  If no attr declaration is found, pos.idx is set to
   -1.  If an attr declaration is found, pos is advanced to end of
   statement.  If declaration applies to the named variable, the
   interface is updated with the declaration info.
 */
PRIVATE
srcPosn parse_attr_decl(const char *name, ProcInterface *interface, srcPosn pos)
{
  srcPosn save_pos;
  int k;
  reset_attrs();

  /* Try to parse an attr */
  save_pos = pos;
  for(k=0; k<NUM_ATTR_KEYWDS; k++) {
    pos = see_keyword(save_pos,attr_keywd[k].keywd);
    if( pos.idx >= 0 ) {
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest && getenv("VERBOSE")) {
    fprintf(list_fd,"\nparse_attr_decl found %s",attr_keywd[k].keywd);
  }
#endif
      save_pos = pos;		/* skip optional :: following attr spec */
      pos = skip_double_colon(pos);
      if(pos.idx < 0)
	pos = save_pos;

      switch(attr_keywd[k].class) {
      case tok_DIMENSION:
	/* No action needed: no stmt array dim info, info will be set on
	   decl items */
	break;
      case tok_POINTER:
	parsed_pointer_attr = TRUE;
	break;
      case tok_TARGET:
	parsed_target_attr = TRUE;
	break;
      }
  /* Now look for identifier in entity-decl-list */
      pos = parse_entity_decl_list(name,interface,pos);
      return pos;
    }
  }
  pos.idx = -1;
  return pos;
}/*parse_attr_decl*/

/* Routine to skip a statement label (sequence of digits) if any,
   starting at line[i].  It does not skip prior blank space, so should
   be given index of first nonblank character of line.  It returns
   index of first nonblank character after the label, or given index i
   if no label present.
 */
PRIVATE
int skip_label(const char *line, int i)
{
      while(isadigit(line[i])) /* skip label if present */
	i++;
      while(iswhitespace(line[i])) /* then skip blank space */
	i++;
      return i;
}

/* Routine to find an ArgListHeader record with is_defn set for the
   given global symbol table entry.  If found, the values of interface
   variables are set from those in the definition, and returns TRUE.
   Otherwise returns FALSE.
 */
PRIVATE
int get_alhead_defn(Gsymtab *gsymt)
{
  ArgListHeader *alhead = gsymt->info.arglist;

  /* If procedure was seen by host unit, a global symtab entry was
     created, but it will not have been processed yet, so check to
     make sure there is an arglist header. */
  while( alhead != (ArgListHeader *)NULL ) {

    if( alhead->is_defn ) {
      parsed_datatype = datatype_of(alhead->type);
      parsed_size = alhead->size;
      parsed_kind_param = alhead->kind;
      kind_is_bogus = FALSE;
/* FIXME when functions with array & pointer results supported
      parsed_array_attr = alhead->array;
      parsed_array_dim = alhead->array_dim;
      parsed_pointer_attr = alhead->pointer;
*/
      /* FIXME: next attributes should come from alhead */
      parsed_recursive_attr = gsymt->recursive;
      parsed_pure_attr = gsymt->pure;
      parsed_elemental_attr = gsymt->elemental;
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest) {
    fprintf(list_fd,": defn arglist found");
  }
#endif
      return TRUE;		/* success */
    }
    else {
      alhead = alhead->next;	/* not defn: follow list */
    }
  }
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest) {
    fprintf(list_fd,": defn not found");
  }
#endif
  return FALSE;			/* no defn found */
}/*get_alhead_defn*/

/* Routine called when a possible prefix keyword ELEMENTAL, PURE, or
   RECURSIVE is seen.  It calls parse_subprog_stmt to see if the
   statement is a valid procedure declaration.  Note that there are
   side effects as various parse variables are set, but these will do
   no harm in the present context.  We check the parsed subprog class
   to make sure it is one that allows prefix.

   This is not part of the lookahead kluge but is used by is_keyword()
   to disambiguate prefix keywords from identifiers.  It is located
   here since it uses lookahead kluge routines and variables.
 */

int
looking_at_prefix()
{
  srcPosn pos;
  /* Back up to the beginning of statement */
  pos.Line = next_srcLine;
  pos.idx = skip_label(pos.Line->line,pos.Line->start_index);

  pos = parse_subprog_stmt(pos);
  if( pos.idx >= 0 &&
      (parsed_subprog_class == tok_SUBROUTINE ||
       parsed_subprog_class == tok_FUNCTION) )
    return TRUE;
  else
    return FALSE;
}


/* Routine that searches from given starting line forward
   to find a CONTAINS statement.  It returns the srcLine where found,
   or NULL if not found.  If stop_at_end is true, it will stop the
   scan at the next END of a subprogram.  With stop_at_end = FALSE,
   this is called by init_scan() to set file_has_contains, which is
   used to decide whether to look ahead for internal subprogram
   definition when unknown subprogram invocation is found.  With
   stop_at_end = TRUE, this is called by scan_for_internal() to find
   the starting point from which to scan for a given internal or
   module subprogram.

   Routine is written for efficiency, so it does not use see_keyword
   or look out for odd cases.  It will only find CONTAINS if it is on
   a single line (i.e. not split across a continuation).  It can be
   fooled by a continued statement with CONTAINS by itself on first
   line and e.g. = 0 on continuation line.
 */
PRIVATE srcLine *scan_for_contains(srcLine *start_srcLine, int stop_at_end)
{
  srcLine *Line = start_srcLine;
  int len = strlen("CONTAINS");

  while( Line  != (srcLine *)NULL ) {
    if( !(Line->comment || Line->contin) ) {
      /* begin scan at first significant char after label */
      int i = skip_label(Line->line,Line->start_index);

      /* Look for a line beginning with the string CONTAINS */
      if(strncasecmp(&(Line->line[i]),"CONTAINS",len) == 0 &&
	 i+len > Line->end_index) { /* verify nothing follows */
	  return Line;				/* found */
      }
      /* No success: go to next line unless stop_at_end==TRUE and
         and end subprogram statement is here. */
      if( stop_at_end ) {
	srcPosn pos;
	pos.Line = Line;
	pos.idx = i;
	pos = parse_end_subprog(pos);
	if(pos.idx >= 0)	/* statement matches end subprog */
	  break;
      }
    }
    Line = Line->next;
  }
  return (srcLine *)NULL;			/* not found */
} /* scan_for_contains */

/* Routine to get interface of named internal or module procedure.
   Provide pos.Line = line preceding where function or subroutine
   statement is expected (e.g. on CONTAINS stmt or END of previous
   procedure).  Declaration will be parsed to pick up info, and if
   name agrees with given, the interface arg will be populated.
   Otherwise routine scans to end of procedure and tries the next,
   till end of CONTAINS section is reached.  If success, returns pos
   with pos.idx >= 0.  If end of CONTAINS is reached without finding
   target, pos.idx = -1 with pos.Line containing the END statement closing
   the host subprogram.  If search runs off the end of the source code
   (an error condition), pos has Line=NULL.
 */
PRIVATE
srcPosn get_internal_interface(const char *name, ProcInterface *interface, srcPosn pos)
{
      while( nextStmt(&pos), (pos.Line != (srcLine *)NULL) ) { /* advance to next stmt */
	srcPosn save_pos;
	pos.idx = skip_label(pos.Line->line,pos.Line->start_index);
	save_pos = pos;
	pos = parse_subprog_stmt(pos);
	if( pos.idx >= 0 ) {	/* found a subprog statement*/
	  if( strcmp(name,parsed_procname) == 0 ) { /* matches the one we seek */
	    populate_interface(interface); /* copy parsed values to ProcInterface struct */

	    /* If it is a function, need to scan for specification
	       stmts that may declare type and other attributes of
	       result.
	     */
	    if(parsed_datatype != type_SUBROUTINE)
	      get_result_decls(parsed_resultvar, interface, pos);

	    return pos;	   /* found internal procedure */
	  }
	  else {			/* not the one we seek: scan to END stmt */
#ifdef DEBUG_LOOKAHEAD
	    if(debug_latest && getenv("VERBOSE")) {
	      fprintf(list_fd,"\nsearch_for_internal scanning for END");
	    }
#endif
	    while( nextStmt(&pos), (pos.Line != (srcLine *)NULL) ) {
	      pos.idx = skip_label(pos.Line->line,pos.Line->start_index);
	      pos = parse_end_subprog(pos);
	      if( pos.idx >= 0 )
		break;
	    }
	    if( pos.Line == (srcLine *)NULL ) { /* protect against hitting end of source */
	      break;				/* quit main while loop */
	    }
	  }
	}/* end found subprog stmt */
	else {			/* catch end of CONTAINS */
	  pos = parse_end_subprog(save_pos); /* i.e. see another END not subprog header */
	  if( pos.idx >= 0 ) {
	    break;
	  }
	}
      }	/* end while pos.Line != NULL */

      pos.idx = -1;		/* target not found */
      return pos;
}/*get_internal_interface*/

/* Routine that scans procedure body for type/attribute declarations
   referring to result variable.  Attributes so defined replace those
   in interface.
*/
PRIVATE
void get_result_decls(const char *resultname, ProcInterface *interface, srcPosn pos)
{
  srcPosn save_pos;
#ifdef DEBUG_LOOKAHEAD
	    if(debug_latest && getenv("VERBOSE")) {
	      fprintf(list_fd,"\nscanning for specif stmts...");
	    }
#endif

  while( nextStmt(&pos), (pos.Line != (srcLine *)NULL) ) {
    pos.idx = skip_label(pos.Line->line,pos.Line->start_index);
    save_pos = pos;
    pos = parse_type_decl(resultname,interface,pos);
    if( pos.idx < 0 ) {
      pos = parse_attr_decl(resultname,interface,save_pos);
      if( pos.idx < 0 ) {
	pos = parse_end_subprog(save_pos);
	if( pos.idx >= 0 )
	  break;
      }
    }
  }
}

/* Routine that searches for an internal or module procedure with the
   given name that could satisfy a call at the current source line.
   Returns LOOKAHEAD_NOTFOUND if none found, LOOKAHEAD_INTERNAL if an
   internal procedure is found, and LOOKAHEAD_MODULE if a module
   procedure is found.

   There are three contexts in which the call can occur:

   (1) a program unit (main program, external subroutine, or
   external function) or a module procedure (subroutine or function)
   referencing a subroutine or function in its CONTAINS section.

   (2) an internal or module procedure within a CONTAINS section of an
   enclosing program unit (main program, module, external subroutine
   or external function), referencing a peer (internal or module
   respectively) procedure in the same CONTAINS section.

   (3) an internal procedure of a module procedure referencing a peer
   of the module procedure.

   A call from a module procedure can be of either (1) or (2), in which case
   (1) prevails.
*/

int search_for_internal(const char *name, ProcInterface *interface)
{
  srcPosn pos;
  Gsymtab *host_gsymt;

  /* Internal subprograms cannot have CONTAINS of their own, so skip
     case (1) if context is call from internal subprogram.
   */
  host_gsymt = hashtab[current_prog_unit_hash].glob_symtab;
  if( !host_gsymt->internal_subprog ) {

	/* Case (1) search for proc in CONTAINS section within current unit*/
    pos.Line = scan_for_contains(next_srcLine,TRUE);

    if( pos.Line != (srcLine *)NULL ) { /* CONTAINS was found */

      pos = get_internal_interface(name, interface, pos);

      if( pos.idx >= 0 )	/* success */
	return LOOKAHEAD_INTERNAL;

      /* in case of running off the end */
      if( pos.Line == (srcLine *)NULL )
	return LOOKAHEAD_NOTFOUND;

    }/* end if CONTAINS found */
  }/* end if !internal_subprog */

  /* Case (2): after scan for own internal failed, look for
     internal or module.  This can be only for calls from internal or
     module procedure.*/
  if( host_gsymt->internal_subprog || host_gsymt->module_subprog ) {
    /* First, we may be lucky and the reference is to a previously
       parsed procedure.  In that case we have full interface info in
       global symbol table.  Or, the global entry may be from a
       call from the host.  In that case the gsymt info is from a
       previous lookahead and we cannot do any better. */
    int h = hash_lookup(name);
    Gsymtab *gsymt;
    if( (gsymt = hashtab[h].glob_symtab) != (Gsymtab *)NULL && 
      /* Verify that the found item is a peer. */
       (gsymt->internal_subprog || gsymt->module_subprog) ) {
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest) {
    fprintf(list_fd,"\nglobal symtab entry found");
  }
#endif
				/* Look for ArgListHeader defn and use
				   it if there is one.
				 */
	if( get_alhead_defn(gsymt) ) {
	    populate_interface(interface); /* copy parsed values to ProcInterface struct */
	  /* Success: return code indicating which category */
	  return (gsymt->internal_subprog?
		  LOOKAHEAD_INTERNAL:
		  LOOKAHEAD_MODULE);
	}
    }
    else {

    /* No luck finding global symtab entry.  Search ahead for a peer. */
      if( host_gsymt->internal_subprog ) {
    /* If call is from internal proc, scan to END to align same as above */
	pos.Line = next_srcLine;
	while( pos.Line != (srcLine *)NULL ) {
	  pos.idx = skip_label(pos.Line->line,pos.Line->start_index);
	  pos = parse_end_subprog(pos);
	  if( pos.idx >= 0 )
	    break;
	  nextStmt(&pos);
	}
	if(pos.Line == (srcLine *)NULL) /* oops, hit end of source */
	  return LOOKAHEAD_NOTFOUND;
      }

      if( pos.Line != (srcLine *)NULL ) {

	pos = get_internal_interface(name, interface, pos);

	if( pos.idx >= 0 ) {	/* success */
	  return (host_gsymt->internal_subprog?
		  LOOKAHEAD_INTERNAL:
		  LOOKAHEAD_MODULE);
	}
	/* Check if case (3) applies.  Call is from an internal procedure and
	   scope stack depth is 3 so caller must be in a module procedure.
	   If entry is in global symtab, it was already found, so here
	   just look ahead in rest of module CONTAINS section.
	 */
	else if( host_gsymt->internal_subprog && loc_scope_top == 3 ) {
	  pos = get_internal_interface(name, interface, pos);
	  if( pos.idx >= 0 ) {
	    return LOOKAHEAD_MODULE;
	  }
	}
      }
    }
  }/* end if internal or module subprog */

  return LOOKAHEAD_NOTFOUND;			/* not found */
}


/* Copy parsed subprogram characteristics to interface struct. */
PRIVATE
void populate_interface(ProcInterface *i)
{
#ifdef DEBUG_LOOKAHEAD
  if(debug_latest) {
    fprintf(list_fd,"\ntype %d(%s)",parsed_datatype,type_name(parsed_datatype));
    fprintf(list_fd," kind %d size %d",parsed_kind_param,parsed_size);
  }
#endif
  i->datatype = parsed_datatype;
  i->size = parsed_size;
  if( parsed_kind_param == kind_DEFAULT_UNKNOWN &&
      parsed_datatype != type_UNDECL )
    i->kind = default_kind(parsed_datatype);
  else
    i->kind = parsed_kind_param;
  i->array_dim = parsed_array_dim;
  i->kind_is_bogus = kind_is_bogus;
  i->array = parsed_array_attr;
  i->pointer = parsed_pointer_attr;
  i->target = parsed_target_attr;
  i->elemental = parsed_elemental_attr;
  i->pure = parsed_pure_attr;
  i->recursive = parsed_recursive_attr;
}

/* Routine called by parse_type_decl to update interface attribute
   values with newly acquired ones from type or attribute declaration
   of the function.  Differs from populate_interface in that only
   non-default values, i.e. values parsed from declaration, are copied
   over.  Also, since elemental, pure, recursive can only be declared
   in FUNCTION or SUBROUTINE statement, they are not updated.
 */
PRIVATE
void update_interface(ProcInterface *i)
{
  if(parsed_datatype != type_UNDECL)
    i->datatype = parsed_datatype;
  if(parsed_size != size_DEFAULT)
    i->size = parsed_size;
  if(parsed_kind_param != kind_DEFAULT_UNKNOWN) {
    i->kind = parsed_kind_param;
    i->kind_is_bogus = kind_is_bogus;
  }
  else {			/* set default kind based on datatype */
    if(parsed_datatype != type_UNDECL)
      i->kind = default_kind(parsed_datatype);
  }
  if(parsed_array_attr) {
    i->array = parsed_array_attr;
    i->array_dim = parsed_array_dim;
  }
  if(parsed_pointer_attr)
    i->pointer = parsed_pointer_attr;
  if(parsed_target_attr)
    i->target = parsed_target_attr;
}



/* End of module Advance */

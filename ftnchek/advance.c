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


	/* Functions which look ahead as far as end of line to see if input
	   cursor is sitting at start of a token of the given class.  Used
	   to resolve ambiguities that need more than one token of lookahead.
	   */


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

    if( L->line[p->idx] == '\0' ) { /* Reached end of line? */

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
	while(pos.Line->line[pos.idx] != '\0' && isspace(pos.Line->line[pos.idx]))
	  stepPosn(&pos);

	if( pos.Line->line[pos.idx] != ',' )
	  return FALSE;
	stepPosn(&pos);

	if( (pos = see_a_number(pos,FALSE)), pos.idx < 0 )
	  return FALSE;
	while(pos.Line->line[pos.idx] != '\0' && isspace(pos.Line->line[pos.idx]))
	  stepPosn(&pos);

	if(pos.Line->line[pos.idx] != ')')
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
	  c = pos.Line->line[pos.idx];	/* Store following character in c */
	  stepPosn(&pos);   /* Leave index pointing at char after c */
	}
	else {
	  c = curr_char;	/* Otherwise next input char is c */
	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," c=%c then %c",c,pos.Line->line[pos.idx]);
#endif

				/* An initial identifier followed by single
				   : has to be a construct name, followed
				   by :: has to be a keyword.  However, if
				   we are testing for DO, fail here since it
				   cannot be followed by any colons and is
				   probably DOUBLE which will be matched later.
				 */
	if( c == ':' ) {
	  while( isspace( (c=pos.Line->line[pos.idx]) ) )
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
	    c = pos.Line->line[pos.idx];
	    stepPosn(&pos);
	    opt_comma = TRUE;
	  }

	  if(c == '=') {	/* Traditional DO form */
	    pos = see_expression(pos);
	    return (pos.idx != -1 && pos.Line->line[pos.idx] == ',') || opt_comma;
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
(void)fprintf(list_fd," to %c",pos.Line->line[pos.idx]);
#endif

	if(pos.Line->line[pos.idx] == '(') {
	  stepPosn(&pos);		/* Move past the paren */
	  pos = skip_balanced_parens(pos);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," to %c",pos.Line->line[pos.idx]);
#endif

	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," conclude %s",pos.Line->line[pos.idx]!= '='?"keyword":"variable");
#endif

	return (pos.Line->line[pos.idx] != '=');
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
		   like 1.E10, or a comparison like (1.EQ.I).  This requires
		   looking for the 'Q' after the 'E'.  The other cases,
		   like ... 1.AND. ... are resolved by looking at next_char
		   to see if it is the 'D' of a d.p. constant or not.
		  */
int
looking_at_relop(VOID)
{
    int c;
    srcPosn pos;


    if( next_char != EOS &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL )
    {
	 pos.Line = next_srcLine; /* starting position of lookahead text */
	 pos.idx = next_index;

	while( (c=pos.Line->line[pos.idx]) != '\0' && isspace(c))
	  stepPosn(&pos);

	if( !isaletter( c ) )	/* next char must be letter */
		return FALSE;
	c = makeupper(c);
	if( c == 'D' )	/* D.P. exponent */
	  return FALSE;	/* No dotted keywords start with D */
	if( c == 'Q' )	/* Q.P. exponent */
	  return FALSE;	/* No dotted keywords start with Q */

			/* If next char is any other letter but 'E', cannot be
			    exponent.  If it is 'E', must be EQ or EQV to
			    be a relop.  So look ahead for the 'Q'. */
	else if( c == 'E' ) {
	  do {
	    stepPosn(&pos);
	  } while( (c=pos.Line->line[pos.idx]) != '\0' && isspace(c));

	  c = makeupper(c);
	  return (c == 'Q');
	}
	else		/* Next char not D or E: must be a dotted keyword */
	  return TRUE;
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
    int c;
    srcPosn pos;


    if( next_char != EOS &&	/* Looking at next line already */
	curr_srcLine != (srcLine *)NULL )
    {
      pos.Line = next_srcLine; /* starting position of lookahead text */
      pos.idx = next_index;

      while( (c=pos.Line->line[pos.idx]) != '\0' && isspace(c))
	stepPosn(&pos);
    }

    /* If next nonblank thing is a quote mark then we have a hit. */

    return ( c == '\'' || c == '"' );
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
      while( (c=pos.Line->line[pos.idx]) != '\0' && c != ')' ) {
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
      }

      if( c == ')' )		/* skip past closing paren */
	stepPosn(&pos);

		/* Having scanned what might be a letter-list, it still could
		   be an expression e.g. M-N where M and N are parameters.
		   If we are looking at a len or kind spec, then next nonblank
		   must be the left paren of the letter list.
		 */
      while( (c=pos.Line->line[pos.idx]) != '\0' && isspace(c) )
	stepPosn(&pos);
      if(c != '\0')
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


#define SKIP_SPACE    while(pos.Line->line[pos.idx] != '\0' && isspace(pos.Line->line[pos.idx])) stepPosn(&pos)

PRIVATE srcPosn
see_a_number(srcPosn pos,	/* location in source buffer */
	     int can_be_holl)	/* context indication */
{
   int digit_seen = FALSE;
   srcPosn save_pos;

   while(pos.Line->line[pos.idx] != '\0' && isspace(pos.Line->line[pos.idx]))
     stepPosn(&pos);

			/* move past optional preceding sign */
   if(pos.Line->line[pos.idx] == '-' || pos.Line->line[pos.idx] == '+' ) {
     stepPosn(&pos);
     SKIP_SPACE;
     can_be_holl = FALSE;
   }
   save_pos = pos;

		/* move past ddd or ddd. or .ddd or ddd.ddd */
   if(isdigit(pos.Line->line[pos.idx]))
     digit_seen = TRUE;

   while(isdigit(pos.Line->line[pos.idx])) {
     stepPosn(&pos);
     SKIP_SPACE;
   }
   if(pos.Line->line[pos.idx] == 'H' && can_be_holl) {
     pos = save_pos;		/* back up to start of number on holl */
     pos = skip_hollerith(pos);
     return pos;
   }
   if(pos.Line->line[pos.idx] == '.') {
     stepPosn(&pos);
     SKIP_SPACE;
     if(isdigit(pos.Line->line[pos.idx]))
       digit_seen = TRUE;
     while(isdigit(pos.Line->line[pos.idx])) {
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
   if(makeupper(pos.Line->line[pos.idx]) == 'E' || makeupper(pos.Line->line[pos.idx]) == 'D' ||
      makeupper(pos.Line->line[pos.idx]) == 'Q') {
     stepPosn(&pos);
     SKIP_SPACE;
     if(pos.Line->line[pos.idx] == '+' || pos.Line->line[pos.idx] == '-') {
       stepPosn(&pos);
       SKIP_SPACE;
     }
     if(!isdigit(pos.Line->line[pos.idx])) {
       pos.idx = -1;
       return pos;
     }
     while(isdigit(pos.Line->line[pos.idx]) || isspace(pos.Line->line[pos.idx]))
       stepPosn(&pos);
   }
		/* look for optional kind parameter. */
   if( pos.Line->line[pos.idx] == '_' ) {
     save_pos = pos;		/* in case of backout */
     stepPosn(&pos);
     SKIP_SPACE;
     if(isidletter(pos.Line->line[pos.idx])) { /* identifier as kind param */
       pos = skip_idletters(pos);
     }
     else if(isadigit(pos.Line->line[pos.idx])) {
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
    while( isdigit(c=pos.Line->line[pos.idx]) || isspace(c) )
      stepPosn(&pos);

    if(c == ',')		/* Skip optional comma */
      stepPosn(&pos);

    pos = see_keyword(pos,"WHILE");

    if( pos.idx == -1 || pos.Line->line[pos.idx] != '(')  /* Look for the opening paren */
    {
      pos.idx = -1;
      return pos;
    }

    stepPosn(&pos);			/* skip the opening paren */
    pos = skip_balanced_parens(pos);
				/* Only = sign can follow the parens if this
				  is not a do-while. */
    if( pos.Line->line[pos.idx] == '=' )
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
	while(pos.Line->line[pos.idx] != '\0') {

				/* Skip past commas */
	  if(pos.Line->line[pos.idx] == ',')
	    stepPosn(&pos);
				/* Check for a double colon */
	  else if(pos.Line->line[pos.idx] == ':') {
	    do {
		stepPosn(&pos);
	    } while(isspace(pos.Line->line[pos.idx]));
	    if(pos.Line->line[pos.idx] == ':')
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
    while(pos.idx != -1 && (c=pos.Line->line[pos.idx]) != '=' && c != '\0') {
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

	/* see_keyword returns -1 if the line (ignoring blanks and
	   uppercasing alphabetics) does not match the given string
	   matchstr.  If it does match, returns index of next nonspace
	   character. Note that index must be at start of keyword. */

PRIVATE srcPosn
see_keyword(srcPosn pos, char *matchstr)
{
    int c;
    while(*matchstr != 0 && (c=pos.Line->line[pos.idx]) != '\0') {
      if(! isspace(c) ) {
	if(makeupper(c) != *matchstr++) {
	  pos.idx = -1;
	  return pos;
	}
      }
      stepPosn(&pos);
    }
    if(*matchstr == '\0') {	/* Match found */
      while(isspace(pos.Line->line[pos.idx]))
	stepPosn(&pos);
      return pos;
    }
    else {			/* No match */
       pos.idx = -1;
       return pos;
    }
}/*see_keyword*/

		/* skip_balanced_parens returns index of the nonspace character
		   following the closing ')' that balances the opening
		   '(' preceding pos.Line->line[pos.idx], or of final nul if the
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

  while(pos.Line->line[pos.idx] != '\0' && depth > 0) {
#ifdef INLINE_COMMENT_CHAR
    if(pos.Line->line[pos.idx] == INLINE_COMMENT_CHAR) /* inline comment ends line */
      break;
#endif
    if(pos.Line->line[pos.idx] == '\'' || pos.Line->line[pos.idx] == '"') {	/* embedded strings confuse things */
      pos = skip_quoted_string(pos);
      prevchar = 'X';	/* Arbitrary non punctuation */
    }
    else if(ispunct(prevchar) && isdigit(pos.Line->line[pos.idx])) {
      pos = skip_hollerith(pos); /* Skip hollerith or number */
      prevchar = pos.Line->line[pos.idx];
    }
    else {
				/* Keep track of nesting */
      if     (pos.Line->line[pos.idx] == '(') ++depth;
      else if(pos.Line->line[pos.idx] == ')') --depth;

      if(! isspace(pos.Line->line[pos.idx]) )
	prevchar = pos.Line->line[pos.idx];

      stepPosn(&pos);
    }
  }

				/* We are now past the closing paren */
  while(pos.Line->line[pos.idx] != '\0' && isspace(pos.Line->line[pos.idx]))
    stepPosn(&pos);		/* skip trailing space */

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
	while(c=pos.Line->line[pos.idx],
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
	while(c=pos.Line->line[pos.idx],
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
  int start_quote_char = pos.Line->line[pos.idx];	/* get opening quote char: ' or " */
  stepPosn(&pos);
  while( (c=pos.Line->line[pos.idx]) != '\0') {
    if( source_unix_backslash && c == '\\' ) {	/* skip any escaped char */
      stepPosn(&pos);
      if(pos.Line->line[pos.idx] == '\0')	/* (check just in case) */
	  break;
    }
    if(c == start_quote_char) {	/* Closing quote? */
      stepPosn(&pos);
      if(pos.Line->line[pos.idx] != start_quote_char) /* Quoted quote? */
	break;
    }
    stepPosn(&pos);
  }

				/* We are now past the closing quote mark */
  while(pos.Line->line[pos.idx] != '\0' && isspace(pos.Line->line[pos.idx]))
    stepPosn(&pos);		/* skip trailing space */

  return pos;
}/*skip_quoted_string*/


			/* Skips holleriths.  Note: treats tabs within
			   hollerith as single characters. */
PRIVATE srcPosn
skip_hollerith(srcPosn pos)
{
  int len=0;
  int c;
  while(isdigit(c=pos.Line->line[pos.idx])) {
    len = len*10 + BCD(c);
    stepPosn(&pos);
    SKIP_SPACE;
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd,"\nskip_hollerith: %d then %c:",
len,pos.Line->line[pos.idx]);
#endif
  if(makeupper(pos.Line->line[pos.idx]) != 'H')
    return pos;

  stepPosn(&pos);				/* Skip the 'H' */

  while(pos.Line->line[pos.idx] != '\0' && len > 0){ /* Move forward len characters */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd,"%c",pos.Line->line[pos.idx]);
#endif
    --len; stepPosn(&pos);
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd," to %c",pos.Line->line[pos.idx]);
#endif
  return pos;
}/*skip_hollerith*/



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

/* End of module Advance */

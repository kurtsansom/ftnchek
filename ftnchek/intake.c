#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "tokdefs.h"
#include "forlex.h"
#include "advance.h"

PROTO(PRIVATE void find_comments,(srcLine *Buf));
PROTO(PRIVATE void find_fixed_contins,(srcLine *Buf));
PROTO(PRIVATE void find_free_contins,(srcLine *Buf));
PROTO(PRIVATE int line_is_blank,(char *s));


	/* Function to read in a source file and store it in a linked
	   list of srcLine elements.  Returns pointer to head of the
	   list.  If file is empty, returns NULL.
	 */

srcLine*
gulp_srcfile(FILE *fd)
{
     char linebuf[MAXLINE+1];
     LINENO_t line_num=0;
     int line_len;
     srcLine *firstLine=NULL, *prevLine=NULL, *thisLine;

     while( fgets(linebuf, sizeof(linebuf), fd ) != NULL ) {
	  LINENO_t new_line_num = 0;

	  line_len = strlen(linebuf); /* this counts the \n */

				/* If input line was too long to fit into
				   buffer, gobble what was not read and
				   discard it. */
	  if(line_len == MAXLINE && linebuf[MAXLINE-1] != ENDL) {
	       int c;
	       while( (c = getc(fd)) != ENDL && c != EOF )
		    continue;
	  }
	  else {
			/* Replace \n by null.  line_len>0 is guaranteed */
	       if(linebuf[line_len-1] == ENDL)
		    linebuf[--line_len] = '\0';

				/* handle <CR><LF> */
	       if( line_len > 1 && linebuf[line_len-1] == '\r' ) {
		    linebuf[--line_len] = '\0';
	       }
	  }

				/* Allocate a new struct for this line.
				   Use calloc so srcLine is zeroed.
				 */
	  if( (thisLine = (srcLine *)calloc((size_t)1,sizeof(srcLine))) == (srcLine *)NULL ||
	       (thisLine->line = (char *)malloc(line_len+1)) == (char *)NULL ) {
	       fflush(list_fd);
	       fprintf(stderr,"Oops: Out of memory in gulp_srcfile\n");
	       exit(1);
	  }

				/* Remember the first line as header */
	  if( firstLine == NULL )
	       firstLine = thisLine;

				/* Fill in the struct */
	  strcpy(thisLine->line,linebuf);

	  ++line_num;		/* Advance the line count */

	  thisLine->line_num = line_num;
	  thisLine->printed = FALSE;
	  thisLine->contin  = FALSE; /* this will be set by lexer */
	  thisLine->cpp_line_directive = FALSE;

				/* Link it into the list */
	  if( prevLine != NULL )
	       prevLine->next = thisLine;
	  thisLine->prev = prevLine;
	  thisLine->next = NULL;
	  prevLine = thisLine;

				/* Let # line directives override the above
				   increment of line_num.  The given value
				   will take effect on the line following
				   the # line directive.
				 */
	  if( linebuf[0] == '#' ) {
	       char *s = linebuf;
	       do { ++s; } while( isspace(*s) ); /* Skip space after the '#' */

	       if(strncmp(s,"line",4) == 0) {	/* Look for the keyword "line" */
		    s += 4;			/* Skip the word "line" */
		    while( isspace(*s) ) ++s;	/* Skip space after the word "line" */
	       }

	       if( isdigit(*s) ) {		/* See that we are now looking at a number */
			/* Get the line number */
		    new_line_num=0;
		    while( isdigit(*s) )
			 new_line_num = new_line_num*10 + BCD(*s++);

		    thisLine->cpp_line_directive = TRUE; /* Note we did it */
	       }
	  }

	  if( new_line_num != 0 ) { /* This is a # line directive */
	       line_num = new_line_num-1; /* number it 1 less than next */
	  }

     }
     (void) fclose(fd);

		/* Now go thru and mark comment and continuation lines */
     if( firstLine != (srcLine *)NULL ) {
	 find_comments(firstLine);
	 if( free_form ) {	/* free source form */
	     find_free_contins(firstLine);
	 }
	 else {			/* fixed source form */
	     find_fixed_contins(firstLine);
	 }
     }
#ifdef DEBUG_GULP_SRCFILE
     if(debug_latest) {	/* print the source file marking comments & contins */
	 srcLine *p;
	 for(p = firstLine; p != NULL; p = p->next ) {
	     fprintf(list_fd,"\n%4d%c", p->line_num,
		     p->comment?(p->f90_comment?'!':'C'): (p->contin?'&':' ') );
	     fprintf(list_fd,"%s",p->line);
	     if(! p->comment ) {
		 int i, last_i;
		 last_i = strlen(p->line)-1;
		 if(last_i < p->start_index)
		      last_i = p->start_index;
		 if(last_i < p->end_index)
		      last_i = p->end_index;
		 if(last_i >= MAXLINE) /* just in case something's bogus */
		      last_i = MAXLINE-1;
		 for(i=0; i<=last_i; i++)
		      linebuf[i] = ' ';
		 if( p->start_index >= MAXLINE || p->end_index >= MAXLINE )
		     linebuf[last_i] = '!'; /* signals a bug */
		 else {
		     if(p->start_index == p->end_index)  /* coincident */
			 linebuf[p->start_index] = '%';
		     else {
			 linebuf[p->start_index] = '^';	/* points to start */
			 linebuf[p->end_index] = '$';   /* points to end */
		     }
		 }
		 linebuf[last_i+1] = '\0';
		 fprintf(list_fd,"\n%5s","");
		 fprintf(list_fd,"%s",linebuf);
	     }
#include "PRINT_srcBuf.h"
	     PRINT_srcBuf(p);
	 }
     }
#endif
     return firstLine;
}


/* Function to find lines that are entirely comment (incl blank lines) */

PRIVATE void
find_comments(srcLine *Buf)
{
  while( Buf != (srcLine *)NULL ) {
    char *s = Buf->line;
    int i,c= makeupper(s[0]);
    int allspace;
    COLNO_t col;

    Buf->comment = FALSE;
    Buf->blank = FALSE;
    Buf->d_comment = FALSE;
    Buf->f90_comment = FALSE;
    Buf->empty_contin = FALSE;


    if( !free_form ) {
				/* Handle F77 standard comments here. */
	if( c == 'C' || c == '*' )
	    Buf->comment = TRUE;

				/* Tolerate D comment lines.  There is
				   no provision for optionally
				   treating them as source code lines.
				 */
	if( c == 'D' ) {
	    Buf->comment = TRUE;
	    Buf->d_comment = TRUE;
	}
    }
				/* Now see if line is blank or only contains
				   an inline comment.
				 */
    allspace = TRUE;
    for(i=0,col=1; s[i] != '\0'; i++) {
	if( !isspace(s[i]))
	{
	    allspace = FALSE;
		/* Initial "!" starts a comment, except in col. 6 of
		   fixed form it must be taken as continuation mark */
	    if(s[i]==INLINE_COMMENT_CHAR && (free_form || col != 6) ) {
		Buf->comment = TRUE;
		Buf->f90_comment = TRUE;
	    }
		/* Standard 3.3.1.3 prohibits an & on a line with nothing but
		   blanks or commentary.  We allow it but flag it here.
		   It is treated as a comment rather than a continuation.
		 */
	    else if( s[i] == '&' && free_form ) {
		i++;
		while( s[i] != '\0' && (isspace(s[i])) ) {
		    i++;
		}
		if( s[i] == '\0' || s[i] == INLINE_COMMENT_CHAR ) {
		    Buf->f90_comment = (s[i] == INLINE_COMMENT_CHAR);
		    Buf->comment = TRUE; /* treat as comment line */
		    Buf->empty_contin = TRUE;
		}
		else
		    break;
	    }
	    else
		break;
	}
	else {
	    col = NXTCOL(s[i],col);
	}
    } /* end for */
    if( allspace ) {
	Buf->blank = TRUE;		/* blank line */
	Buf->comment = TRUE;
    }

#if 0
    fprintf(list_fd,"\n%c",Buf->comment? 'C':(Buf->contin?'&':' '));
    fprintf(list_fd,"%s",Buf->line);
#endif
    Buf = Buf->next;		/* advance to next line */
  }
}

	/* Functions to find and mark continuation lines.  Note that
	   first line of continued statement is NOT marked.

	   These functions also set the cursors start_index and
	   end_index of each line to the first and last significant
	   character respectively of line.  (It is possible for
	   start_index to point to '\0' in some cases.  Also,
	   end_index is set to -1 for fixed-form continuation line
	   that is blank.)  For comment lines, the cursors are set to
	   harmless values, but they should not be needed.  The column
	   number start_col corresponding to start_index after
	   accounting for tabs (including -source=dec-tab convention)
	   is also set, but there is no need for an end_col.

	   The main difference between the two functions, besides the
	   fundamental one of the free-form final '&' vs. fixed-form
	   column-6 mark, is that in fixed-form we need to ignore
	   spaces and pay attention to column numbers to avoid running
	   past column 72 (or in general max_stmt_col).

	   These functions would be simple except that a '!'  or '&'
	   must not be treated as ending a line if it is inside a
	   string or a hollerith.  (In this comment, an H edit
	   descriptor is considered a hollerith.)  Strings are pretty
	   easy to deal with, but holleriths cause a lot of trouble.
	   In order not to see a hollerith in a variable like X1H1 or
	   in DO 4 H=1,10 we require it to be preceded by a
	   punctuation character.  It is worth noting that the code
	   allows a hollerith to immediately follow a string, which is
	   improper but accepted by some compilers (and our
	   lexer/parser) in a FORMAT.  I believe that all cases where
	   a hollerith could be matched by this code in a place where
	   no hollerith should be are illegal, so all we need to do is
	   make the parser squawk, which will happen.  Likewise it
	   will always spot a hollerith if it sees an integer preceded
	   by punctuation and followed by 'H', which can never miss
	   a valid one.  In any event a false identification or
	   failure to identify a hollerith can only cause trouble if a
	   '!' or (for free-form) '&' is in the span of the (real or
	   falsely supposed) hollerith.

	   In both functions (free and fixed versions) the variable
	   inside_number is set TRUE while scanning an integer that
	   could be the length part of a hollerith; num_val accumulates
	   the value of this integer; prev_sig_char remembers the previous
	   significant character, which must be punctuation (ispunct)
	   in order for inside_number to be turned on by seeing a digit.

	   Similarly, inside_quote is TRUE while scanning a string,
	   and quoteChar remembers the opening quote type (' or ") to
	   determine when the closing quote is found.
	 */

PRIVATE void
find_free_contins(srcLine *Buf)
{
    int to_be_continued = FALSE; /* remembers '&' of previous line */
    int inside_quote = FALSE;
    int quoteChar;		/* opening quote character */
    int inside_number = FALSE;	/* used for continued hollerith numbers */
    int prev_sig_char = ' ';	/* previous significant character */
    int num_val;		/* value of number recently seen */
    while( Buf != (srcLine *)NULL ) {
	Buf->overlength = FALSE;
	if( Buf->comment) {	/* skip comment lines */
	    Buf->contin = FALSE;
	    Buf->start_index = 0;
	    Buf->end_index = -1;
	    Buf->start_col = 0;
	}
	else {		/* not comment */
	    COLNO_t col;
	    int i, c;
	    char *s = Buf->line;

	    Buf->contin = to_be_continued; /* mark this line */
	    Buf->end_index = -1;

	    /* look for optional initial '&' on continued line */
	    i = 0;
	    col = 1;
	    if( inside_quote ) {
		/* Record the start of significant text.
		   Inside a quote: line must be continued, and
		   these may be overruled by finding a leading
		   '&' later (which std requires but we don't) .
		*/
		Buf->start_index = i;
		Buf->start_col = col;
		Buf->contin_wo_amp = TRUE;
	    }
	    while( isspace(s[i]) ) {/* skip leading space */
		col = NXTCOL(s[i],col);
		i++;
	    }

	    if( to_be_continued ) {	/* deal with continued statements */
		if( s[i] == '&' ) {
		    Buf->contin_wo_amp = FALSE;
		    i++;	/* resume after the '&' */
		    col++;
		    if( inside_quote ) { /* blanks are significant inside strings */
			Buf->start_index = i;
			Buf->start_col = col;
		    }
		}

			/* Handle the unlikely case that a hollerith is split
			   within or just after the initial number.
			   The usual case is to skip over blanks
			   following the initial '&' if any.
			 */
		if( ! inside_number ||
		       !( (i == 0 || s[i-1] == '&') &&
			  (makeupper(s[i]) == 'H' || isdigit(s[i])) ) ) {
		    inside_number = FALSE;
		    while( isspace(s[i]) ) {/* skip space after & */
			col = NXTCOL(s[i],col);
			i++;
		    }
		}
	    }
	    else {
		inside_quote = FALSE;	/* clean up unfinished business */
		inside_number = FALSE;
		prev_sig_char = ' ';
	    }

	    /* Record the start of significant text.
	       Continued quoted strings were handled
	       above; all other situations are done here.
	       We have at all events skipped over all
	       leading space and, if a continuation line,
	       over any initial '&' and following nonquoted space.
	    */
	    if( ! inside_quote ) {
		Buf->start_index = i;
		Buf->start_col = col;
	    }

	    to_be_continued = FALSE;
	    for( ; (c=s[i]) != '\0'; i++ ) {
		if( inside_quote ) {
		    if( c == quoteChar ) {
			if( s[i+1] == quoteChar ) { /* escaped quote */
			    i++;
			    col++;
			}
			else
			    inside_quote = FALSE;
		    }
		    /* inside quote '&' is continuation if rest of line blank */
		    else if( c == '&' && line_is_blank(s+i+1) ) {
			to_be_continued = TRUE;
			break;
		    }
		    /* escaped quote via Unix backslash */
		    else if( source_unix_backslash &&
			     c == '\\' && s[i+1] == quoteChar ) {
			i++;
			col++;
		    }
		}
		else if( c == '\'' || c == '"' ) {
		    inside_quote = TRUE;
		    quoteChar = c;
		    inside_number = FALSE;
		}
		/* Not in a quoted string or starting one */

		else {
		  if( isdigit(c) ) { /* number found: see if hollerith */
		    if( inside_number ) { /* this may be a continued number */
			num_val = num_val*10 + BCD(c);
		    }
		    else {
			/* hollerith can only follow punctuation */
			if( ispunct(prev_sig_char) ) {
			    num_val=BCD(c);	/* start it off */
			    inside_number = TRUE;
			}
		    }
		  }
		  if( inside_number ) {
		    if( ! (isdigit(c) || c == '&') ) {
			inside_number = FALSE; /* cannot be continued */
		    }
		    if( makeupper(c) == 'H' ) { /* skip past hollerith */
			int hollerith_count=0;
			/* We can't just say i += num since line may end
			   before then.  (AFAIK holleriths cannot be
			   continued in free form, so premature end of line
			   can be taken as end of stmt.) */
			i++; col++;	/* move past 'H' */
			for(hollerith_count = num_val;
			    (c=s[i]) != '\0' && hollerith_count>0 ;
			    /* Hmmm: how should tabs in holleriths be counted?
			       As one column or many?  We say many. */
			    hollerith_count--, i++, col = NXTCOL(c,col)) {
			    Buf->end_index = i;
			}
			hollerith_count = 0; /* if line ends before H does */
		    }
		  } /* end of dealing with number and maybe hollerith */

		    /* Start of a comment: quit i loop. */
		  if( c == INLINE_COMMENT_CHAR ) {
		      Buf->f90_comment = TRUE;
		      break;
		  }
		  else if( c == '&' ) {
		    /* Found freeform contin mark.   Check
		     * that naught but comments follow.
		     */
		      to_be_continued = TRUE;

		      for(i++, col++; (c=s[i]) != '\0' && isspace(c);
			  i++, col = NXTCOL(c,col))
			  continue;
		      /* If something does follow the contin mark, set
			 end_index to point to it so the parser will squawk.
			 This error is too rare to deserve specific handling.
		       */
		      if(c == INLINE_COMMENT_CHAR)
			  Buf->f90_comment = TRUE;
		      else if(c != '\0')
			  Buf->end_index = i;

		      break;
		  }
		} /* end of stuff outside quotes */

			/* Re-test for end of stmt needed here due to
			   advancing i above.  This should never happen
			   when inside_quote but source may be illegal.
			*/
		if( c == '\0' )
		    break;

		/* As long as interesting stuff is seen,
		 * update end index.  This should never
		 * leave end_index at -1 since we know line
		 * is not a comment so not all blank.
		 */
		if( inside_quote || ! isspace(c) ) {
		    Buf->end_index = i;
		    prev_sig_char = c;
				/* set up std violation warning if significant
				   chars past std max line length */
		    if( col > std_max_stmt_col )
			Buf->overlength = TRUE;
		}
	    } /* end for i */
	} /* end else not comment */
	Buf = Buf->next;
    } /* end while Buf */
}/*find_free_contins*/

PRIVATE void
find_fixed_contins(srcLine *Buf)
{
    int inside_quote = FALSE;
    int quoteChar;		/* opening quote character */
    int hollerith_count=0; /* used to deal with continued holleriths */
    int inside_number = FALSE;	/* used for continued hollerith numbers */
    int prev_sig_char = ' ';	/* previous significant character */
    int num_val;		/* value of number recently seen */
    int tab_count = 0;		/* tabs seen, for -port=tab option */
    while( Buf != (srcLine *)NULL ) {
	Buf->overlength = FALSE;
	Buf->contin = FALSE;
	if( Buf->comment) {	/* skip comment lines */
	    Buf->start_index = 0;
	    Buf->end_index = -1;
	    Buf->start_col = 0;
	}
	else {
	    COLNO_t col;
	    int i,c;
	    char *s = Buf->line;

	    /* Handle DEC tabs: followed by nonzero digit
	       is a continuation line */
	    if( source_dec_tab && s[0] == '\t' ) {
		++tab_count;
		if( isadigit((int)s[1]) && s[1] != '0' ) {
		    Buf->contin = TRUE;
		    Buf->start_index = 2; /* stmt char after contin mark */
		    Buf->start_col = 8; /* treat start of stmt as col 8 */
		}
	    }
	    else
	    {
		/* skip to col 6 */
		for(i=0,col=1; col < 6 && s[i] != '\0'; i++) {
		    col = PORT_NXTCOL(s[i],col);
		}
		c = s[i];

		if( col == 6 && c != '\0' 
		    && !isspace(c) && c != '0'
#ifdef ALLOW_UNIX_CPP
		    /* Veto if it is a preprocessor line */
		    && s[0] != '#'
#endif
		    ) {
		    Buf->contin = TRUE;
		    Buf->start_index = i+1;	/* record where stmt resumes */
		    Buf->start_col = 7;
		}	
	    }

	    /* Locate the start and end of significant
	     * source text. */


	    if( Buf->contin ) { /* resume after any continuation mark */
		i=Buf->start_index;
		col=Buf->start_col;
	    }
	    else {
		/* For DEC tabs, an initial tab puts us in column 7 */
#ifdef DEC_TABS
		if( source_dec_tab && s[0] == '\t' )
		{
		    i = 1;
		    col = 7;
		}
		else
#endif
		{
		    i=0;	/* otherwise start at column 1 */
		    col=1;
		}
		hollerith_count = 0; /* clear unfinished business */
		inside_quote = FALSE;
		inside_number = FALSE;
		prev_sig_char = ' ';
	    }
	    if( hollerith_count>0 ) {
		for(;	/* Eat any continued hollerith */
		    (c=s[i]) != '\0' && hollerith_count>0 && col<=max_stmt_col;
		    hollerith_count--, i++, col=PORT_NXTCOL(c,col)) {
		    Buf->end_index = i;
		}
		if( hollerith_count>0 ) { /* fill out implied blanks */
		    hollerith_count -= (max_stmt_col - col + 1);
		}
	    }
				/* Skip over leading blank space */
	    else if( ! inside_quote ) {
		for( ; (c=s[i]) != '\0' && isspace(c); i++, col=PORT_NXTCOL(c,col) ) {
		    continue;
		}
		Buf->start_index = i;
		Buf->start_col = col;
	    }

		/* In fixed form, end_index can fail to get set if line
		   consists of no significant characters other than a
		   continuation mark.  This is OK if it is equal to
		   start_index so an EOL will be sent by advance.
		*/
	    Buf->end_index = Buf->start_index;

				/* Now scan rest of line for end of
				   significant text */

	    for( ; (c=s[i]) != '\0'; i++, col=PORT_NXTCOL(c,col) ) {

		if( inside_quote ) {

		    /* Handle escaped quote.  This code does not spot
		       escaped quote split across continuation, but it
		       gets the begin and end markers right anyway.
		       Unix backslash style can't be split since it
		       would escape newline instead.
		     */
		    if( c == quoteChar ) {
			if( s[i+1] == quoteChar ) {/* escaped quote */
			    i++;
			    col++;
			}
			else
			    inside_quote = FALSE;
		    }
		    /* escaped quote via Unix backslash */
		    else if( source_unix_backslash &&
			     c == '\\' && s[i+1] == quoteChar ) {
			i++;
			col++;
		    }
		}
		else if( c == '\'' || c == '"' ) {
		    inside_quote = TRUE;
		    quoteChar = c;
		    inside_number = FALSE;
		}
		/* Not in a quoted string or starting one */

		else {
		  if( isdigit(c) ) { /* number found: see if hollerith */
		    if( inside_number ) { /* this may be a continued number */
			num_val = num_val*10 + BCD(c);
		    }
		    else {
			/* hollerith can only follow punctuation */
			if( ispunct(prev_sig_char) ) {
			    num_val=BCD(c);	/* start it off */
			    inside_number = TRUE;
			}
		    }
		  }
		  if( inside_number ) {
		    if( !(isdigit(c) || isspace(c) || c == INLINE_COMMENT_CHAR) ) {
			inside_number = FALSE; /* cannot be continued */
		    }
		    if( makeupper(c) == 'H' ) { /* skip past hollerith */
			Buf->end_index = i; /* the H might be last thing on line */
			/* We can't just set i = i+num since line may
			   end before then. */
			i++; col++;	/* move past 'H' */

			for(hollerith_count = num_val;
			    (c=s[i]) != '\0' && hollerith_count>0 && col<=max_stmt_col;
			    /* Hmmm: how should tabs in holleriths be counted?
			       As one column or many?  We say many. */
			    hollerith_count--, i++, col = PORT_NXTCOL(c,col)) {
			    Buf->end_index = i;
			}
			if( hollerith_count>0 ) { /* fill out implied blanks */
			    hollerith_count -= (max_stmt_col - col + 1);
			}
		    }
		  } /* end of dealing with number and maybe hollerith */

		    /* Start of a comment: quit i loop. */
		  if( c == INLINE_COMMENT_CHAR ) {
		      Buf->f90_comment = TRUE;
		      break;
		  }

		} /* end of stuff outside quotes */

			/* Re-test for end of stmt needed here due to
			   advancing i above */
		if( c == '\0' )
		    break;

		/* As long as interesting stuff is seen,
		 * update end index.  This should never
		 * leave end_index undefined since we know line
		 * is not a comment so not all blank.
		 */
		if( (inside_quote || ! isspace(c)) ) {
		    if( col <= max_stmt_col ) {
			Buf->end_index = i;
			prev_sig_char = c;
		    }
				/* Set up std violation warning if significant
				   chars past std max line length.  The second
				   test is to avoid warning if nonsignificant
				   blanks follow end of statement inside quote.
				*/
		    if( col > std_max_stmt_col &&
			(col <= max_stmt_col || ! isspace(c)) )
			Buf->overlength = TRUE;
		}
	    } /* end for i */

			/* A continuation line that is blank except for the
			   continuation mark will leave end_index unchanged
			   from initial -1.  This is not illegal in fixed
			   form: f77 std 3.2.3.  So we do not treat them
			   as comments, unlike counterpart in freeform.
			 */
	    if( Buf->contin && Buf->end_index == -1 ) {
		Buf->empty_contin = TRUE;
	    }

	} /* end else not comment */
	Buf = Buf->next;
    } /* end while Buf */
    if(tab_count > 0 && tab_filename == NULL)
	tab_filename = current_filename;	/*  for portability warning */
}/*find_fixed_contins*/


		/* Function to see if line is blank (under freeform rules) */
PRIVATE int
line_is_blank(char *s)
{
    int i, c;
    for(i=0; (c=s[i]) != '\0' && isspace(c); i++)
	continue;
    return (c == '\0');
}


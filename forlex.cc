/* $Id: forlex.c,v 1.47 2006/10/01 20:31:24 moniot Exp $

	Tokenizing routines for Fortran program checker.
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


#include "config.h"		/* Get system-specific information */
#include <stdio.h>
#include <ctype.h>
#include <string.h>

	/* Some older mac compilers need compat.h to use memset.  If
	   needed, add  `` -d MAC_MPW ''  to compilation options.
	 */
#ifdef MAC_MPW
#include <compat.h>
#endif

#include "ftnchek.h"
#define FORLEX
#include "symtab.h"
#include "tokdefs.h"
#include "forlex.h"
#include "utils.h"

/* toascii() is widely supported, but in case it isn't, define it here.
   We need it mainly in order to avoid bounds violation in legal_chars array.
   On EBCDIC systems, toascii() should convert from an EBCDIC code
   to the ASCII code for the same character, because the legal_chars array is
   ordered according to the ASCII collating sequence.
 */
#ifndef toascii
#define toascii(C) ((C) & 0177)
#endif

extern int complex_const_allowed,    /* shared flags operated by fortran.y */
	   inside_format,
	   integer_context;



PROTO(PRIVATE void make_legal_char,( const char *s ));




/*

Part I. yylex()

   Shared functions defined:
	yylex()			Returns next token.  Called from yyparse().
	implied_id_token(t,s)	Creates token for blank common declaration.
	get_binary_const(t, c ) Creates token for binary constant
	get_string(t)		Creates token for a string

*/


/* Evaluation of real constants can cause overflow or other problems
   at run time, and they are not used for anything, so turn off
   evaluation except for debugging purposes.  A value of 0.0 will be
   substituted.
 */
#ifdef EVALUATE_REALS
#include <math.h>		/* Only used for pow() in debug mode */
#endif

PRIVATE int closeup_saw_whitespace;

	/* The following macro says whether a given character is legal,
	 * i.e. one of the stream control chars or a valid ANSI Fortran
	 * character.  Lower case letters are considered legal too.
	 * Nondigits in columns 1-6 (except EOF,EOS) are illegal in fixed form.
	 * Hopefully this works for EBCDIC too.
	 */
#define islegal(C) ( ((C) == EOF) || ((C) == EOS) || \
	( (col_num >= 6 || free_form || isdigit(C)) && \
	 (toascii((int)(C)) >= toascii(' ') && \
	  legal_chars[toascii((int)(C))-toascii(' ')] == (C))) )
		/* Array has x where ASCII character is not valid.
		   This defn is not exactly standard fortran, since it includes
		   $ allowed by some compilers in formats.
		   Since strlen of array is 96, indexing by [toascii(c)-32]
		   is always in bounds.
		 */
PRIVATE char fortran_legal_chars[]=
" x\"x$%x'()*+,-./0123456789:x<=>xx\
ABCDEFGHIJKLMNOPQRSTUVWXYZ[x]x_xabcdefghijklmnopqrstuvwxyzxxxxx";

		/* This is the working copy of list of legal chars, with
                   any chars in idletter_list made legal using the
		   -identifier-chars setting.
		 */
PRIVATE char legal_chars[sizeof(fortran_legal_chars)];

				/* Routine to fix up list of legal chars */
void make_legal_char_list(VOID)
{
    int i;
			/* Start with the standard fortran list */
    (void)strcpy(legal_chars,fortran_legal_chars);
    
			/* Verify idletter_list has only punctuation chars.
			   If violators, reset to default of "$_"
			 */
    for(i=0; idletter_list[i] != '\0'; i++) {
      if( !ispunct(idletter_list[i]) ) {
	(void)fprintf(stderr,
	"\n%cidentifier setting specifies invalid character %c: setting ignored",
#ifdef OPTION_PREFIX_SLASH
			  '/',
#else
			  '-',
#endif
		idletter_list[i]);
	idletter_list = const_strcpy(DEF_IDLETTER_LIST);	/* restore to default */
	break;
      }
    }
			/* Add nonstd nonalpha chars allowed in identifiers */
    make_legal_char(idletter_list);

}




		/* Routines to alter the default status of characters,
		   to support various extensions to f77. */

PRIVATE void
#if HAVE_STDC
make_legal_char(const char *s)
#else /* K&R style */
make_legal_char(s)
     char *s;			/* List of legal chars */
#endif /* HAVE_STDC */
{
  int i;
  while( *s != '\0' ) {
    i = toascii((int)(*s));
    if(i >= toascii(' ') && i <= toascii('~')) {
      legal_chars[i-toascii(' ')] = *s;
    }
    s++;
  }
}

#if 0
		/* Routines to alter the default status of characters,
		   to support various extensions to f77. Not used now.*/

PROTO(void make_illegal_char,( char *s ));


void
make_illegal_char(s)
     char *s;			/* List of illegal chars */
{
  int i;
  while( *s != '\0' ) {
    i = toascii((int)(*s));
    if(i >= toascii(' ') && i <= toascii('~')) {
	legal_chars[i-toascii(' ')] = ( (*s != 'x')? 'x': 'X');
    }
    s++;
  }
}
#endif


		/* local functions defined */


PROTO(PRIVATE void closeup,( void ));


#ifdef ALLOW_UNIX_CPP
PROTO(PRIVATE void get_cpp_directive,( void ));
#endif

PROTO(PRIVATE void get_dot,( Token *token ));

PROTO(PRIVATE void get_dotted_keyword,( Token *token));

PROTO(PRIVATE void get_edit_descriptor,( Token *token ));

PROTO(PRIVATE void get_hollerith,( Token *token, int n ));

PROTO(PRIVATE void get_illegal_token,( Token *token ));

PROTO(PRIVATE void get_label,( Token *token ));

PROTO(PRIVATE void get_letter,( Token *token ));

PROTO(PRIVATE void get_number,( Token *token ));

PROTO(PRIVATE void get_punctuation,( Token *token ));

PROTO(PRIVATE void get_simple_punctuation,( Token *token ));

PROTO(PRIVATE int f90_relop, ( Token *token, int *multichar ) );

/* Define zero_struct to set a struct to zero. It works around the differing availability
   of memset and bzero.   This is used to initialize a token. 
 */
#if HAVE_MEMSET
#define zero_struct(sptr,struct_size)    (void)memset((sptr),0,struct_size)
#else
#if HAVE_BZERO
#define zero_struct(sptr,struct_size)    bzero((char *)(sptr),struct_size)
#else
PROTO(PRIVATE void zero_struct, (void *sptr, int struct_size) );
PRIVATE void
zero_struct(void *sptr, int struct_size)
{
     int i;
     for(i=0; i<struct_size; i++) {
	  ((char *)sptr)[i] = 0;
     }
}
#endif
#endif

		/*  Gets next token for Yacc.  Return value is token.class,
		 *  and a copy of the token is stored in yylval.
		 */
int
yylex(VOID)
{
    Token token;
    extern int in_attrbased_typedecl; /* shared with fortran.y */

		/* Initialize token fields to scratch. */
    zero_struct(&token,sizeof(token));

    src_text_len = 0;

    if(curr_char == EOF) {
	token.tclass = EOF;
	token.line_num = line_num;
	token.col_num = col_num;
    }
    else /* not EOF */ {


		/* Skip leading spaces, and give error message if non-ANSI
		 * characters are found.
		 */

	while(iswhitespace(curr_char) || (! islegal(curr_char))  ) {
	  if(!iswhitespace(curr_char)) {
#ifdef ALLOW_UNIX_CPP
	    if(curr_char == '#' && col_num == 1) {
	       get_cpp_directive();	/* turn # line into EOS */
	       break;
	    }
	    else
#endif
		lex_error("Illegal character");
	  }
	  advance();
	}

	token.line_num = line_num;
	token.col_num = col_num;

	closeup_saw_whitespace = FALSE;

	if(inside_format) {	/* Handle format stuff here to avoid trouble */
	  get_edit_descriptor(&token);
	}
	else if(isadigit(curr_char)) {
			/* Identify label:
			      Fixed form: Number in cols 1-5.
			      Free form:  Number at start of statement.  Be careful:
			      initial_flag normally tells, but it gets turned on in
			      attrbased type decls where dimensions can be, and following
			      an IF(expr) which might be an arithmetic IF (the labels that
			      follow are lexed as integers not label tokens).
			 */
	    if( (free_form)?
		(initial_flag && !(in_attrbased_typedecl || curr_stmt_class == tok_IF)):
		(col_num < 6))
			get_label(&token);      /* Stmt label */
		else
			get_number(&token);     /* Numeric or hollerith const */
	}
	else if(curr_char == '_') {
			/* Underscore character not allowed as initial char
			 * of identifier; instead get it as punctuation */
			get_punctuation(&token);
	}
	else if(isidletter(curr_char)) {
		if(implicit_letter_flag)
			get_letter(&token);	/* letter in IMPLICIT list */
		else
			get_identifier(&token); /* Identifier or keyword */
	}
	else if(isaquote(curr_char)) {
			get_string(&token);	/* Quoted string */
	}
	else if(curr_char == '.') {
			get_dot(&token);	 /* '.' lead-in */
	}
	else {
			get_punctuation(&token);  /* Punctuation character or EOS */
	}
    }/*end not EOF*/

    if(token.tclass == EOS) {
	implicit_flag=FALSE;	/* in case of errors, reset flags */
	implicit_letter_flag = FALSE;
	WHILE_expected = FALSE;
    }


    prev_token_class = token.tclass;

    yylval = token;
    return token.tclass;

} /* yylex */

	/* closeup: Advances input stream till next_char is nonspace.  Fudges
	   things so that curr_char remains as it was.
	*/
PRIVATE void
closeup(VOID)
{
  int
    save_curr_char = curr_char,
    save_prev_char = prev_char;
  LINENO_t
    save_line_num = line_num;
  COLNO_t
    save_col_num = col_num;

  int next_space = iswhitespace(next_char);

  closeup_saw_whitespace = next_space; /* Record for free-format warnings */

  while(next_space) {
    advance();
    next_space = iswhitespace(next_char);
  }

  curr_char = save_curr_char;
  prev_char = save_prev_char;
  line_num = save_line_num;
  col_num = save_col_num;
}


	/* Fills argument with token for an identifer, as if an identifer
	 * with name given by string s had been lexed.  This will
	 * be called by parser when blank common declaration is seen,
	 * and when a main prog without program statement is found,
	 * and when an unnamed block data statement is found,
	 * so processing of named and unnamed cases can be handled uniformly.
	 * Also called when reading module with common declarations to
	 * create tokens for common block and its elements for def_com_block.
	*/
void
#if HAVE_STDC
implied_id_token(Token *t, char *s)
#else /* K&R style */
implied_id_token(t,s)
	Token *t;
	char *s;
#endif /* HAVE_STDC */
{
	int h;
	unsigned long hnum;

	hnum = hash(s);
	while( h=hnum%HASHSZ, hashtab[h].name != NULL &&
		strcmp(hashtab[h].name,s) != 0)
			hnum = rehash(hnum);
	if(hashtab[h].name == NULL) {	/* not seen before */
		hashtab[h].name = s;
		hashtab[h].loc_symtab = NULL;
		hashtab[h].glob_symtab = NULL;
		hashtab[h].com_loc_symtab = NULL;
		hashtab[h].com_glob_symtab = NULL;
	}
	t->tclass = tok_identifier;
	t->value.integer = h;
	t->src_text = new_src_text("",0);
} /* implied_id_token */

#ifdef ALLOW_UNIX_CPP
		/* This does not create a token but just performs the
		   actions needed when a cpp directive is seen.  It
		   advances curr_char to the EOS.  The setting of
		   filename is delayed to this point because it is not
		   stored in tokens but is external, so changing it
		   must wait till the previous statement is fully
		   parsed and any error messages printed and arg or
		   com list headers completed.
		 */
PRIVATE void
get_cpp_directive(VOID)
{
  if(next_filename != (char *)NULL) {

		/* A #line directive on first line of toplevel source file
		   gives name of real original file.  Replace our idea
		   of top_filename with that. But ignore an initial
		   # 1 "" since that means cpp was working with stdin,
		   probably from ftnpp.  Likewise ignore # 1 "stdin"
		   which is a variant form, e.g. from fpp.
		   Nowadays cpp may indicate stdin by "<stdin>" or suchlike,
		   which we also ignore.
		*/
    if( cpp_start_of_file ) {
      if( next_filename[0] != '\0' &&
	  next_filename[0] != '<' &&
	  strcmp(next_filename,"stdin") != 0 ) {
	top_filename = next_filename;
	current_filename = next_filename;
	cpp_start_of_file = FALSE;
      }
    }
    else {
      if( cpp_inc_depth > 0 &&
	  next_filename == cpp_include_stack[cpp_inc_depth-1].filename ) {
	--cpp_inc_depth;
      }
      else {
	if( cpp_inc_depth == 0 ) {
	  top_file_line_num = next_top_file_line_num;
	}
				/* Avoid overrun, but it will recover
				   even if max depth is exceeded.
				*/
	if(cpp_inc_depth < MAX_INCLUDE_DEPTH)
	  cpp_include_stack[cpp_inc_depth++].filename = current_filename;
      }
      current_filename = next_filename;
    }

    
  }
  if(f77_unix_cpp || f90_unix_cpp || !cpp_handled) {
    nonstandard(line_num,col_num,f90_unix_cpp,0);
    msg_tail(": preprocessor directive");
    if(!cpp_handled)
      msg_tail("(not processed)");
  }

  do {			/* Skip to end of directive.  It will become an EOS */
    advance();
  } while( curr_char != EOS);

}/*get_cpp_directive*/
#endif

PRIVATE void
#if HAVE_STDC
get_dot(Token *token)
#else /* K&R style */
get_dot(token)
	Token *token;
#endif /* HAVE_STDC */
{
	if(src_text_len < MAX_SRC_TEXT)
	  src_text_buf[src_text_len++] = curr_char;

	closeup();		/* Advance till nonspace char in next_char */

	if(isadigit(next_char))
		get_number(token);		/* Numeric const */
	else if(isaletter(next_char))
		get_dotted_keyword(token);	/* .EQ. etc. */
	else
		get_simple_punctuation(token);	/* "." out of place */
}


PRIVATE const struct {
	const char *name;
	int tclass,tsubclass;
 } dotted_keywords[]={
			{".EQ.",tok_relop,relop_EQ},
			{".NE.",tok_relop,relop_NE},
			{".LE.",tok_relop,relop_LE},
			{".LT.",tok_relop,relop_LT},
			{".GE.",tok_relop,relop_GE},
			{".GT.",tok_relop,relop_GT},
			{".AND.",tok_AND,0},
			{".OR.",tok_OR,0},
			{".NOT.",tok_NOT,0},
			{".FALSE.",tok_logical_const,FALSE},
			{".TRUE.",tok_logical_const,TRUE},
			{".EQV.",tok_EQV,0},
			{".NEQV.",tok_NEQV,0},
			{NULL,0,0}
		    };


PRIVATE void
#if HAVE_STDC
get_dotted_keyword(Token *token)
#else /* K&R style */
get_dotted_keyword(token)
	Token *token;
#endif /* HAVE_STDC */
{
	int i, h,
	    has_embedded_space,	/* Spaces inside keyword */
	    space_seen_lately;	/* Flag for catching embedded space */
	initial_flag = FALSE;
				/* Watch for embedded space, but not
				   (in fixed form)
				   between dots and letters of keyword.
				   I.e.  ". eq ." is OK, but not ".e q." */
	has_embedded_space = FALSE;

	bi_advance();      /* gobble the initial '.' */

	space_seen_lately = (!free_form? FALSE:
			     closeup_saw_whitespace);

	while(isaletter(curr_char)) {

	   if(src_text_len < MAX_SRC_TEXT)
	     src_text_buf[src_text_len++] = (char)makeupper(curr_char);

	  if(space_seen_lately)
	    has_embedded_space = TRUE;

	   bi_advance();

	   space_seen_lately = iswhitespace(prev_char);
	}

			/* Free form complains about space before last dot */
	if(free_form && space_seen_lately)
	    has_embedded_space = TRUE;

	if(src_text_len < MAX_SRC_TEXT)
	  src_text_buf[src_text_len++] = '.'; /* make it complete */

	if(curr_char != '.') {
	    lex_error("Badly formed logical/relational operator or constant");
	}
	else {
		advance();      /* gobble the final '.' */
	}
	if( (pretty_extra_space || (free_form && f90_freeform_space))
	   && has_embedded_space) {
	      space_violation(token->line_num,token->col_num,
			"keyword has embedded space");
	}

	for(i=0; dotted_keywords[i].name != NULL; i++) {
	  if(strncmp(src_text_buf+1, /* only compare the significant parts */
		     dotted_keywords[i].name+1,
		     src_text_len-2) == 0) {
	    token->tclass = dotted_keywords[i].tclass;
	    token->tsubclass = dotted_keywords[i].tsubclass;
	    token->value.string = token->src_text = const_strcpy(dotted_keywords[i].name);
#ifdef DEBUG_FORLEX
			if(debug_lexer)
			   (void)fprintf(list_fd,"\nDotted keyword:\t\t%s",
						token->src_text);
#endif
			return;
		}
	}

	/* It could be a defined operator in generic spec */
	src_text_buf[src_text_len] = '\0';
	token->tclass = tok_defined_op;
	token->value.integer = h = hash_lookup(src_text_buf);
	token->src_text = hashtab[h].name;

#ifdef DEBUG_FORLEX
			if(debug_lexer)
			   (void)fprintf(list_fd,"\nDefined operator:\t\t%s",
						token->src_text);
#endif
	return;

} /* get_dotted_keyword */

PRIVATE void
#if HAVE_STDC
get_edit_descriptor(Token *token)
#else /* K&R style */
get_edit_descriptor(token)
	Token *token;
#endif /* HAVE_STDC */
{
    int c;
    long repeat_spec;
    int Ee_allowed=FALSE;	/* true if edit descr can have Ee after w.d */

    if(isadigit(curr_char)) {	/* Digit: repeat spec or holl or kP or nX */
      repeat_spec = 0;
      do {
	if(src_text_len < MAX_SRC_TEXT)
	  src_text_buf[src_text_len++] = curr_char;
	repeat_spec = repeat_spec*10L + (long)BCD(curr_char);
	if( makeupper(next_char) == 'H' )
	  inside_hollerith = TRUE;/* get ready for hollerith*/
	bi_advance();
      } while(isadigit(curr_char));

      if( makeupper(curr_char) == 'H' ) {
				/* nH... pass off to hollerith routine */
	get_hollerith(token, (int)repeat_spec);
	return;
      }
      else {
				/* Otherwise it is a repeat spec or the
				   numeric part of kP or nX which we treat
				   as repeat specs too */
	token->tclass = tok_integer_const;
	token->value.integer = repeat_spec;
	token->src_text = new_src_text(src_text_buf,src_text_len);
#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nInteger const:\t\t%ld (from %s)",
	      repeat_spec,
              token->src_text);
#endif
      }
    }/* end if digit */

    else if(isaletter(curr_char)) {
      c = makeupper(curr_char);
      if(src_text_len < MAX_SRC_TEXT)
	src_text_buf[src_text_len++] = c;
      bi_advance();
      switch(c) {

	case 'P':		/* P of kP  k seen previously */
	  if(prev_token_class != tok_integer_const) {
	    if(f77_format_extensions || f90_format_extensions){
	      nonstandard(token->line_num,token->col_num,f90_format_extensions,0);
	      msg_tail(": P must follow a number");
	    }
	  }
	  break;

	case 'X':		/* X or nX */
	  break;

	case 'S':		/* S or SP or SS */
	  c = makeupper(curr_char);
	  if(c == 'S' || c == 'P') {
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = c;
	    bi_advance();
	  }
	  break;

	case 'B':		/* BN or BZ */
	  c = makeupper(curr_char);
	  if(c == 'N' || c == 'Z') {
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = c;
	    bi_advance();
	  }
	  else {
	    if(f77_f90){
	      nonstandard(token->line_num,token->col_num,0,0);
	      msg_tail(": N or Z expected after B");
	    }
	    goto get_w_d;	/* F90 has Bw.d: allow that */
	  }
	  break;

	case 'T':		/* Tc or TLc or TRc */
	  c = makeupper(curr_char);
	  if(c == 'L' || c == 'R') {
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = c;
	    bi_advance();
	  }

	case 'E':		/* In F90, E can be followed by N or S  */
	  c = makeupper(curr_char);
	  if( c == 'N' || c == 'S' ) {
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = c;
	    bi_advance();
	    if(f77_f90){
	      nonstandard(token->line_num,token->col_num,0,0);
	    }
	  }
	  Ee_allowed = TRUE;
	  goto get_w_d;

	    
	case 'O':	/* These are OK in f90 but not f77 */
	case 'Z':
	  if(f77_f90){
	    nonstandard(token->line_num,token->col_num,0,0);
	  }
	  goto get_w_d;

				/* Iw, Fw.c and similar forms */

	case 'G':
	  Ee_allowed = TRUE;	/* OK in F90 to have Ee trailer */
	  /*FALLTHRU*/
	case 'A':
	case 'D':
	case 'F':
	case 'I':
	case 'L':
get_w_d:				/* Get the w field if any */
	  while( isadigit(curr_char) ){
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = curr_char;
	    bi_advance();
	  }
			/* Include any dot followed by number (e.g. F10.5)
			*/
	  if( curr_char == '.' ) {
	    do {
	      if(src_text_len < MAX_SRC_TEXT)
		src_text_buf[src_text_len++] = curr_char;
	      bi_advance();
	    } while( isadigit(curr_char) );
	  }
				/* w.d can sometimes be followed by Ee */
	  if( Ee_allowed && (c=makeupper(curr_char)) == 'E' ) {
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = c;
	    bi_advance();
	    while( isadigit(curr_char) ){
	      if(src_text_len < MAX_SRC_TEXT)
		src_text_buf[src_text_len++] = curr_char;
	      bi_advance();
	    }
	  }
	  break;

	  /* Next case can only occur in illegal code */
        case 'H':
	      syntax_error(token->line_num,token->col_num,
			   "H specifier missing preceding number");
	      break;

	default:
	  if(f77_format_extensions || f90_format_extensions) {
	    nonstandard(token->line_num,token->col_num,f90_format_extensions,0);
	    msg_tail(": edit descriptor");
	    src_text_buf[src_text_len++] = '\0';
	    msg_tail(src_text_buf);
	  }
	  goto get_w_d;
      }/*end switch*/

      token->tclass = tok_edit_descriptor;
      token->value.string = NULL;
      token->src_text = new_src_text(src_text_buf,src_text_len);

#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nEdit descriptor:\t%s",token->src_text);
#endif
    }/*end else if isaletter*/

			/* Apostrophe or quote mark means a string. */
    else if( isaquote(curr_char) ) {
      get_string(token);
    }
				/* Otherwise it is mere punctuation. Handle
				   it here ourself to avoid complications. */
    else {
      src_text_buf[src_text_len++] = curr_char;
      get_simple_punctuation(token);
    }
}

PRIVATE void
#if HAVE_STDC
get_hollerith(Token *token, int n)  /* Gets string of form nHaaaa */
#else /* K&R style */
get_hollerith(token,n)  /* Gets string of form nHaaaa */
	Token *token;
	int n;
#endif /* HAVE_STDC */
{
	int i;
	LINENO_t last_line_num;
	COLNO_t last_col_num;

		/* strsize = length of only the string being defined
		   fullsize = length of whole hollerith const, which includes
		   length spec already stored in src_text_buf plus the
		   H plus the text plus final nul. */
	int strsize=n,
	    leadin=src_text_len+1,
	    fullsize=leadin+strsize+1;
	char *s;

	initial_flag = FALSE;

	s = new_src_text_alloc(fullsize);

	for(i=0; i<src_text_len; i++) /* Copy the leadin already saved */
	  s[i] = src_text_buf[i];
	s[i++] = 'H';		/* store the 'H' */

	if(n==1)
	  inside_hollerith=FALSE;/* turn off flag ahead of next_char */
	advance();/* Gobble the 'H' */

	last_col_num = col_num;
	last_line_num = line_num;

	for(i=0; i<n; i++) {
	  while(curr_char == EOL) {
			/* Treat short line as if extended with blanks */
	    COLNO_t col;
	    for(col=last_col_num; i<n && col<(COLNO_t)max_stmt_col; i++,col++) {
		s[leadin+i] = ' ';
	    }
	    last_col_num = col_num;
	    advance();
	  }
	  if(i==n) break;

	  if(curr_char == EOS || curr_char == EOF) {
	    COLNO_t col;
	    for(col=last_col_num; i<n && col<(COLNO_t)max_stmt_col; i++,col++) {
	      if(i < strsize)
		s[leadin+i] = ' ';
	    }
	    if(i < n) {		/* If it did not fill up */
	      syntax_error((LINENO_t)last_line_num,(COLNO_t)last_col_num,
			   "Hollerith constant ends prematurely");
	      strsize=i;
	    }
	    break;
	  }
	  else {
	    s[leadin+i] = curr_char;
	    last_col_num = col_num;
	    last_line_num = line_num;
	    if(i==n-2)/* turn flag off ahead of next_char*/
	      inside_hollerith = FALSE;
	    advance();
	  }
	}

	if(strsize > 0)
	  s[leadin+strsize] = '\0';

	inside_hollerith = FALSE;
	token->tclass = tok_hollerith;
	token->value.string = s + leadin;
	token->size = n;
	token->src_text = s;
#ifdef DEBUG_FORLEX
	if(debug_lexer)
		(void)fprintf(list_fd,"\nHollerith:\t\t%s (from %s)",
			      token->value.string,
			      token->src_text);
#endif

} /* get_hollerith */



PRIVATE void
#if HAVE_STDC
get_illegal_token(Token *token)	/* Handle an illegal input situation */
#else /* K&R style */
get_illegal_token(token)	/* Handle an illegal input situation */
	Token *token;
#endif /* HAVE_STDC */
{
	token->tclass = tok_illegal;
	token->src_text = new_src_text("",0);
#ifdef DEBUG_FORLEX
	if(debug_lexer)
	     (void)fprintf(list_fd,"\nILLEGAL TOKEN");
#endif

} /* get_illegal_token */



		/* Read a label from label field. */
PRIVATE void
#if HAVE_STDC
get_label(Token *token)
#else /* K&R style */
get_label(token)
	Token *token;
#endif /* HAVE_STDC */
{
    int value=0;
    int space_seen=FALSE, has_embedded_space=FALSE;
    if( !free_form ) {
	while( isadigit(curr_char) && col_num < 6 ) {
	  if(space_seen)
	    has_embedded_space = TRUE;
	  value = value*10 + BCD(curr_char);
	  src_text_buf[src_text_len++] = curr_char;
	  advance();
	  while(curr_char==' ' && col_num < 6) {
	    space_seen = TRUE;
	    advance();
	  }
	}
	if((pretty_extra_space || (free_form && f90_freeform_space))
	   && has_embedded_space) {
	      space_violation(token->line_num,token->col_num,
			"label has embedded space");
	}
    }
    else {			/* free form */
	int numdigits=0;
	while( isadigit(curr_char) ) {
	    value = value*10 + BCD(curr_char);
	    if(src_text_len < MAX_SRC_TEXT)
		src_text_buf[src_text_len++] = curr_char;
	    ++numdigits;
	    advance();
	}
				/* label can have only up to 5 digits */
	if( numdigits > 5 && misc_warn) {
	    syntax_error(token->line_num,token->col_num,
			 "statement label exceeds 5 digits");
	}
    }
    token->tclass = tok_label;
    token->value.integer = value;
    token->src_text = new_src_text(src_text_buf,src_text_len);
#ifdef DEBUG_FORLEX
	if(debug_lexer)
		(void)fprintf(list_fd,"\nLabel:\t\t\t%d (from %s)",
			      value,
			      token->src_text);
#endif

} /* get_label */


PRIVATE void
#if HAVE_STDC
get_letter(Token *token)		/* Gets letter in IMPLICIT list */
#else /* K&R style */
get_letter(token)		/* Gets letter in IMPLICIT list */
	Token *token;
#endif /* HAVE_STDC */
{
	token->tclass = tok_letter;
	token->tsubclass = src_text_buf[src_text_len++] = makeupper(curr_char);
	token->src_text = new_src_text(src_text_buf,src_text_len);

#ifdef DEBUG_FORLEX
    if(debug_lexer)
	(void)fprintf(list_fd,"\nLetter:\t\t\t%s",token->src_text);
#endif

	advance();

} /* get_letter */


	/* get_number reads a number and determines data type: integer,
	 * real, or double precision.
	 */
/* This belongs in ftnchek.h, perhaps.  Defines number of significant
   figures that are reasonable for a single-precision real constant.
   Works out to 9 for wordsize=4, 21 for wordsize=8. These allow
   for a couple of extra digits for rounding. Used in -trunc warning. */
#define REAL_SIGFIGS (local_wordsize==0? 8: (local_wordsize-1)*3)

PRIVATE void
#if HAVE_STDC
get_number(Token *token)
#else /* K&R style */
get_number(token)
	Token *token;
#endif /* HAVE_STDC */
{
	DBLVAL dvalue,leftside,rightside,pwr_of_ten;
	int exponent,datatype,c;
#ifdef EVALUATE_REALS
	int expsign;
#endif
	int numdigits,	/* Count of digits in integer, significant or not */
	    sigfigs;	/* Count of significant digits */

			/* For freeform warnings, this gets set when we
			   arrive here via a leading '.', otherwise is false.
			 */
	int space_seen_lately = closeup_saw_whitespace;
	int has_embedded_space = FALSE;

	initial_flag = FALSE;

	leftside = (DBLVAL)0;
	numdigits = sigfigs = 0;
	datatype = tok_integer_const;
	while(isadigit(curr_char)) {
		if(space_seen_lately)
		  has_embedded_space = TRUE;
		leftside = leftside*(DBLVAL)10 + (DBLVAL)BCD(curr_char);
		++numdigits;
			/* Do not count leading zeroes as significant */
		if(sigfigs > 0 || curr_char != '0')
		  ++sigfigs;
		if( !integer_context && makeupper(next_char) == 'H' )
		  inside_hollerith = TRUE;/* get ready for hollerith*/

		if(src_text_len < MAX_SRC_TEXT)
		  src_text_buf[src_text_len++] = curr_char;
				/* Embedded space is worth preserving since
				   it is often used in long numbers.  Any
				   amount of blanks + tabs -> 1 blank.
				   Exception: integer_context says upcoming
				   item is a label or datatype length spec. */
		if(! integer_context &&
		   (next_char == ' ' || next_char == '\t')) {
		  if(src_text_len < MAX_SRC_TEXT)
		    src_text_buf[src_text_len++] = ' ';
		}

		bi_advance();
		space_seen_lately = iswhitespace(prev_char);
	}

		/* If context specifies integer expected, skip to end.
		   Otherwise scan on ahead for more. */
    if( integer_context) {
        if(numdigits == 0) {
	    lex_error("integer expected");
	    advance();	/* gobble something to avoid infinite loop */
	}
    }
    else {/* not integer_context */
	if( makeupper(curr_char) == 'H' ){      /* nnH means hollerith */
		if(leftside == (DBLVAL)0) {
			lex_error("Zero-length hollerith constant");
			inside_hollerith = FALSE;
			advance();
			get_illegal_token(token);
		}
		else {
			if(src_text_buf[src_text_len-1] == ' ')
			  --src_text_len;
			get_hollerith(token, (int)leftside);
		}
		return;
	}

	rightside = (DBLVAL)0;
	pwr_of_ten = (DBLVAL)1;
	closeup();		/* Pull in the lookahead character */

	if( curr_char == '.' &&
				/* don't be fooled by 1.eq.N or
				   I.eq.1.and. etc */
	   !looking_at_relop() ) {
		datatype = tok_real_const;
		if( space_seen_lately )
		    has_embedded_space = TRUE;
		if(numdigits > 0) /* if dot is initial it is already stored */
		  if(src_text_len < MAX_SRC_TEXT)
		    src_text_buf[src_text_len++] = curr_char;
		bi_advance();
		space_seen_lately = closeup_saw_whitespace || iswhitespace(prev_char);
		closeup_saw_whitespace = FALSE;

		while(isadigit(curr_char)) {
			if( space_seen_lately )
			    has_embedded_space = TRUE;
			rightside = rightside*(DBLVAL)10 + (DBLVAL)BCD(curr_char);
			++numdigits; /* not used past here, but maintain it anyway */
			if(sigfigs > 0 || curr_char != '0')
			  ++sigfigs;
			pwr_of_ten /= (DBLVAL)10;

			if(src_text_len < MAX_SRC_TEXT)
			  src_text_buf[src_text_len++] = curr_char;
			if(next_char == ' ' || next_char == '\t')
			  if(src_text_len < MAX_SRC_TEXT)
			    src_text_buf[src_text_len++] = ' ';

			bi_advance();
			space_seen_lately = iswhitespace(prev_char);
		}
	}
#ifdef EVALUATE_REALS
	dvalue = leftside + rightside*pwr_of_ten;
#else
	dvalue = (DBLVAL)0;
#endif

	exponent = 0;
#ifdef EVALUATE_REALS
	expsign = 1;
#endif
		/* Integer followed by E or D gives a real/d.p constant.
		   We also accept Q for quad (real*16) constants. */

	space_seen_lately = space_seen_lately || closeup_saw_whitespace;

	if( ( (c = makeupper(curr_char)) == 'E' || c == 'D' || c == 'Q') )
	{
		datatype = ((c == 'E')? tok_real_const:
			    ((c == 'D')? tok_dp_const:
			    tok_quad_const));
		if( space_seen_lately )
		  has_embedded_space = TRUE;
		if(src_text_len < MAX_SRC_TEXT)
		  src_text_buf[src_text_len++] = c;
		bi_advance();
		space_seen_lately = iswhitespace(prev_char);
		if(curr_char == '+') {
#ifdef EVALUATE_REALS
			expsign = 1;
#endif
			if(src_text_len < MAX_SRC_TEXT)
			  src_text_buf[src_text_len++] = curr_char;
			bi_advance();
			space_seen_lately = space_seen_lately || iswhitespace(prev_char);
		}
		else if(curr_char == '-') {
#ifdef EVALUATE_REALS
			expsign = -1;
#endif
			if( iswhitespace(prev_char) )
			  has_embedded_space = TRUE;
			if(src_text_len < MAX_SRC_TEXT)
			  src_text_buf[src_text_len++] = curr_char;
			bi_advance();
			space_seen_lately = space_seen_lately || iswhitespace(prev_char);
		}
		if(!isadigit(curr_char)) {
			lex_error("Badly formed real constant");
		}
		else while(isadigit(curr_char)) {
			if( space_seen_lately )
			  has_embedded_space = TRUE;
			exponent = exponent*10 + (curr_char-'0');
			if(src_text_len < MAX_SRC_TEXT)
			  src_text_buf[src_text_len++] = curr_char;
			bi_advance();
			space_seen_lately = iswhitespace(prev_char);
		}

	/*  Compute real value only if debugging. If it exceeds max magnitude,
	    computing it may cause crash. At this time, value of real const
	    is not used for anything. */
#ifdef EVALUATE_REALS
		  dvalue *= pow(10.0, (double)(exponent*expsign));
#else
		  dvalue = (DBLVAL)0;
#endif

	}
    }/* end if(!integer_context) */

        if(src_text_buf[src_text_len-1] == ' ')	/* remove any trailing blank */
	  --src_text_len;

	token->tclass = datatype;

	token->src_text = new_src_text(src_text_buf,src_text_len);

	if( free_form && (pretty_extra_space || f90_freeform_space)
	     && has_embedded_space ) {
	      space_violation(token->line_num,token->col_num,
			"Numeric constant has embedded space");
	}

	switch(datatype) {
	   case tok_integer_const:
		token->value.integer = (long)leftside;
#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nInteger const:\t\t%ld (from %s)",
	      token->value.integer,
	      token->src_text);
#endif
		break;
	   case tok_real_const:
			/* store single as double lest it overflow */
				/*LINTED*/ /*not uninit*/
		token->value.dbl = dvalue;
		if(trunc_sigfigs && sigfigs >= REAL_SIGFIGS) {
		  warning(token->line_num,token->col_num,
	"Single-precision real constant has more digits than are stored");
		}
#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nReal const:\t\t%g (from %s)",
	      (double)token->value.dbl,
	      token->src_text);
#endif
		break;
	   case tok_dp_const:
				/*LINTED*/ /*not uninit*/
		token->value.dbl = dvalue;
#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nDouble const:\t\t%g (from %s)",
	      (double)token->value.dbl,
	      token->src_text);
#endif
		break;
	   case tok_quad_const:
			/* store quad as double in case host doesn't do quad */
				/*LINTED*/ /*not uninit*/
		token->value.dbl = dvalue;
#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nQuad const:\t\t%g (from %s)",
	      (double)token->value.dbl,
	      token->src_text);
#endif
		break;
	}

} /* get_number */


#ifdef ALLOW_TYPELESS_CONSTANTS
		/* Routine to get constants of the forms:
		     B'nnnn' (f90std)  'nnnn'B (nonf90)  -- binary
		     O'nnnn' (f90std)  'nnnn'O (nonf90)  -- octal
		     Z'nnnn' (f90std)  X'nnnn' 'nnnn'X 'nnnn'Z (nonf90) -- hex
		   No check of whether digits are less than base.
		   Warning is issued here instead of in parser since constant
		   looks like a normal integer by the time the parser sees it.
		 */
void
get_binary_const(Token *token, int c, int space_seen_lately)
			/* c is base character: madeupper'ed by caller */
{
  long value=0;
  int base,digit;
  int badly_formed=FALSE;
  int i,j;			/* indices in src_text_buf for repacking */

  if(c == 'O')  base = 8;
  else if(c == 'X' || c == 'Z')  base = 16;
  else if(c == 'B') base = 2;
  else {
    syntax_error(token->line_num,token->col_num,
		 "Unknown base for typeless constant -- octal assumed");
    base = 8;
  }
				/* F90 allows initial B, O, Z but not X */
  if( c == 'X' && (f90_typeless_constants || f77_typeless_constants) ) {
    nonstandard(token->line_num,token->col_num,f90_typeless_constants,0);
  }
  else if(f77_f90) {
    nonstandard(token->line_num,token->col_num,0,0);
  }
  else if( curr_stmt_class != tok_DATA &&
	   (c == 'B' || c == 'O' || c == 'Z') &&
	   (f90_typeless_constants || f77_typeless_constants) ) {
    nonstandard(token->line_num,token->col_num,f90_typeless_constants,0);
    msg_tail("binary, octal, or hex constant allowed only in DATA statement");
  }

				/* Advance i to starting digit */
  i = 0;
  while( ! isaquote(src_text_buf[i]) ) {
    ++i;
  }
  j = ++i;	/* Input = Output to start */

				/* Scan the string, moving chars down
				   to change multi spaces to single
				   blanks, and converting digits. */
  while( ! isaquote(src_text_buf[i]) ) {
    digit=src_text_buf[i++];
    if( ishex(digit) ){
      value = value*base + HEX(digit);
      src_text_buf[j++] = digit;
    }
    else {			/* Anything else should be space */
      if( isspace(digit) ) {
	if( free_form )
	    space_seen_lately = TRUE; /* blanks not OK in free form */
	src_text_buf[j++] = ' ';
	while( isspace(src_text_buf[i]) ) {
	  ++i;
	}
      }
      else {
	  badly_formed = TRUE;
      }
    }
  }
  if( badly_formed ) {
      syntax_error(token->line_num,token->col_num,
		   "badly formed typeless constant");
  }
  else if((pretty_extra_space || (free_form && f90_freeform_space))
	  && space_seen_lately) {
      space_violation(token->line_num,token->col_num,
		"typeless constant has embedded space");
  }

  while(i < src_text_len)
    src_text_buf[j++] = src_text_buf[i++]; /* Copy the rest over */

  src_text_len = j;

  token->tclass = tok_integer_const;
  token->value.integer = value;
  token->src_text = new_src_text(src_text_buf,src_text_len);

#ifdef DEBUG_FORLEX
if(debug_lexer)
(void)fprintf(list_fd,"\nInteger const:\t\t%ld (from %s)",
	      token->value.integer,
	      token->src_text);
#endif

}/*get_binary_const*/

#endif/*ALLOW_TYPELESS_CONSTANTS*/


PRIVATE void
#if HAVE_STDC
get_punctuation(Token *token)
#else /* K&R style */
get_punctuation(token)
	Token *token;
#endif /* HAVE_STDC */
{
	int multichar,	   /* Flags To catch spaces inside multi-char token */
	    space_seen_lately;
	extern int in_attrbased_typedecl;	/* shared with fortran.y */
	extern int disallow_double_colon;	/* shared with fortran.y */
	multichar = FALSE;

	src_text_buf[src_text_len++] = curr_char;

		/* Punctuation turns off initial_flag.  However, if
		   lexing attr-based type decl, turn
		   initial_flag back on if a comma is found.  Turn off
		   the attribute-based decl flag when the double colon
		   is reached.
		 */
	initial_flag = FALSE;
	if( in_attrbased_typedecl ) {
	    if( curr_char == ',' ) {
		initial_flag = TRUE;
	    }
	    else if(curr_char == ':' ) {
		in_attrbased_typedecl = FALSE;
	    }
	}

	space_seen_lately = iswhitespace(next_char);

	closeup();

	if(curr_char == '*' && next_char == '*') {
		token->tclass = tok_power;
		multichar = TRUE;
		advance();
		src_text_buf[src_text_len++] = curr_char;
	}
	else if(curr_char == '/' && next_char == '/' ) {
		/* If this is COMMON / / list, then embedded space is OK */
		if( prev_token_class == tok_COMMON ) {
		    space_seen_lately = FALSE;
		}
			/* Otherwise it is concatenation operator */
		else {
		    extern int in_assignment_stmt;
				/* for obscure rule check */
		    if(in_assignment_stmt)
			make_true(IN_ASSIGN,token->TOK_flags);
		}
		token->tclass = tok_concat;
		multichar = TRUE;
		advance();
		src_text_buf[src_text_len++] = curr_char;
	}
		/* double colon :: treat as single token */
	else if(curr_char == ':' && next_char == ':' && !disallow_double_colon) {
		token->tclass = tok_double_colon;
		multichar = TRUE;
		advance();
		src_text_buf[src_text_len++] = curr_char;
	}
            /* recognize module rename and ptr assignment => */
        else if(curr_char == '=' && next_char == '>') {
                token->tclass = tok_rightarrow;
                multichar = TRUE;
                advance();
		src_text_buf[src_text_len++] = curr_char;
	}
            /* recognize beginning of array constructor (/ */
		/* Don't be fooled by OPERATOR (/) */
        else if(curr_char == '(' && next_char == '/' &&
		prev_token_class != tok_OPERATOR ) {
                token->tclass = tok_l_ac_delimiter;
                multichar = TRUE;
                advance();
		src_text_buf[src_text_len++] = curr_char;
	}
            /* recognize end of array constructor /) */
        else if(curr_char == '/' && next_char == ')' &&
		prev_token_class != '(' ) {
                token->tclass = tok_r_ac_delimiter;
                multichar = TRUE;
                advance();
		src_text_buf[src_text_len++] = curr_char;
	}
	/* Underscore followed by quote char must be for kind
	 * parameter on a character literal.  Return special
	 * underscore to avoid shift/reduce conflict.
	 */
	else if(  curr_char == '_' &&
	         (next_char == '\'' || next_char == '"' ) ) {
		token->tclass = tok_underscore;
	}
	/* Catch closing > of variable format here so it taken as
	   delimiter not f90 relops.   Note that inside_format is
	   turned off while scanning the expr inside < > so we use
	   curr_stmt_class instead to detect this case.  Assumes that
	   a > as relop cannot occur in var fmt expr.  This could be
	   made sure by checking if outside of parens.
	*/
	else if( curr_char == '>'  && curr_stmt_class == tok_FORMAT ) {
	  get_simple_punctuation(token);
	  return;
	}
				/* recognize F90 rel-ops here */
	else if( f90_relop(token,&multichar) ) {
	  token->tclass = tok_relop;
	  if(f77_relops) {
	    nonstandard(token->line_num,token->col_num,0,0);
	    msg_tail("for relational operator");
	  }
	}
		/* paren can be the start of complex constant if everything
		   is just right. Maybe more tests needed here. */
	else if(complex_const_allowed && curr_char == '(' &&
	     (  (prev_token_class<256 && ispunct(prev_token_class))
	      || prev_token_class == tok_relop
	      || prev_token_class == tok_power )
	     && looking_at_cplx()) {
		token->tclass = tok_lparen;
	}
	else {
			/* Provide special left parenthesis to avoid s/r
			   conflict in grammar.
			 */
	  if( need_special_lparen ) {
	      /* ASSERT ( curr_char == '(' ) */
	    token->tclass = tok_lparen;
	    need_special_lparen = FALSE;
	  }
	  else {
	    token->tclass = curr_char;
	  }

	  /* Do not recognize double colons within parenthesis to prevent
	   * them from being parsed as a single token.  This is needed
	   * for array sections with missing subscripts e.g. array(1::10).
	   * Here the occurrence of '::' could be mistaken for that token.
	   */
	  if (curr_char == '(')
	      disallow_double_colon = TRUE;
	  if (curr_char == ')')
	      disallow_double_colon = FALSE;
	}

	token->src_text = new_src_text(src_text_buf,src_text_len);

	if((pretty_extra_space || (free_form && f90_freeform_space))
	   && multichar && space_seen_lately) {
	      space_violation(token->line_num,token->col_num,
			"multi-character operator has embedded space");
	}

	advance();

#ifdef DEBUG_FORLEX
if(debug_lexer) {
	if(token->tclass == EOS)
		(void)fprintf(list_fd,"\n\t\t\tEOS");
	else {
		(void)fprintf(list_fd,"\nPunctuation:\t\t");
		if(token->tclass == tok_lparen || token->tclass == tok_underscore)
		    (void)fprintf(list_fd,"special ");
		(void)fprintf(list_fd,"%s",token->src_text);
	}
 }
#endif
} /* get_punctuation */


PRIVATE void
#if HAVE_STDC
get_simple_punctuation(Token *token)
#else /* K&R style */
get_simple_punctuation(token)
	Token *token;
#endif /* HAVE_STDC */
{
		/* Like get_punctuation but lacks special cases.  Just
		   gets the punctuation character. Text is already in
		   src_text_buf. */

	token->tclass = curr_char;
	token->src_text = new_src_text(src_text_buf,src_text_len);
	advance();
#ifdef DEBUG_FORLEX
if(debug_lexer) {
	if(token->tclass == EOS)
		(void)fprintf(list_fd,"\n\t\t\tEOS");
	else
		(void)fprintf(list_fd,"\nPunctuation:\t\t%s",token->src_text);
}
#endif
} /* get_simple_punctuation */

PRIVATE int
#if HAVE_STDC
f90_relop(Token *token, int *multichar)
#else /* K&R style */
f90_relop(token, multichar)
     Token *token;
     int *multichar;
#endif /* HAVE_STDC */
{
  *multichar = FALSE;
  if( curr_char == '>' ) {
    if( next_char == '=' ) {
      token->tsubclass = relop_GE;
      token->src_text = const_strcpy(">=");
      goto twochar_relop;
    }
    else {
      token->tsubclass = relop_GT;
      token->value.string = const_strcpy(">");
      return TRUE;
    }
  }
  
  if( curr_char == '<' ) {
    if( next_char == '=' ) {
      token->tsubclass = relop_LE;
      token->value.string = const_strcpy("<=");
      goto twochar_relop;
    }
    else {
      token->tsubclass = relop_LT;
      token->value.string = const_strcpy("<");
      return TRUE;
    }
  }

  if( curr_char == '=' && next_char == '=' ) {
      token->tsubclass = relop_EQ;
      token->value.string = const_strcpy("==");
      goto twochar_relop;
  }

  if( curr_char == '/'  && next_char == '=' ) {
      token->tsubclass = relop_NE;
      token->value.string = const_strcpy("/=");
      goto twochar_relop;
  }

  return FALSE;

			/* Two-character relops: need to gobble 2nd char */
twochar_relop:
  *multichar = TRUE;
  advance();
  src_text_buf[src_text_len++] = curr_char;
  return TRUE;
}

void
#if HAVE_STDC
get_string(Token *token)       /* Gets string of form 'aaaa' */
#else /* K&R style */
get_string(token)       /* Gets string of form 'aaaa' */
	Token *token;
#endif /* HAVE_STDC */
{
	int len;
	COLNO_t last_col_num;
	int has_backslash = FALSE; /* for portability check */

	quote_char = curr_char; /* remember the delimiter */
	initial_flag = FALSE;
	inside_string = TRUE;
	last_col_num=col_num;
	src_text_buf[src_text_len++] = curr_char; /* store leading quote */
	advance();      /* Gobble leading quote */
	len = 0;
	for(;;) {
		while(curr_char == EOL) {
			/* Fixed form: treat short line as if extended with
			   blanks to 72 columns.  Free form: line ends at EOL */
		  if( ! free_form ) {
		    COLNO_t col;
		    for(col=last_col_num; col<max_stmt_col; col++) {

		      if(src_text_len < MAX_SRC_TEXT)
			src_text_buf[src_text_len++] = ' ';

		      ++len;
		    }
		  }
		  last_col_num=col_num;
		  advance();
		}
		if(curr_char == EOS || curr_char == EOF) {
			lex_error("Closing quote missing from string");
			break;
		}
		if(curr_char == quote_char) {
			inside_string = FALSE;/* assume so for now */

/* If LEX_RAWSTRINGS defined, stores doubled quotes and final quote.
   Otherwise initial quote is stored and doubled quotes are reduced to one. */
#ifdef LEX_RAWSTRINGS
				/* Store the quote */
			if(src_text_len < MAX_SRC_TEXT)
			  src_text_buf[src_text_len++] = curr_char;
#endif

			last_col_num=col_num;
			advance();

			if(curr_char == quote_char){/* '' becomes ' in string */
				inside_string = TRUE; /* not a closing quote */

				if(src_text_len < MAX_SRC_TEXT)
				  src_text_buf[src_text_len++] = curr_char;

				++len;
				last_col_num=col_num;
				advance();
			}
			else {
				break;  /* It was a closing quote after all */
			}
		}
		else {		/* ordinary character within quotes */
			int value=curr_char;

			if(curr_char == '\\') {
			  if(!has_backslash) {/* only warn once per string */
			    if(port_backslash)
			      nonportable(line_num,col_num,
			   "backslash treated incompatibly by some compilers");
			  }
			  has_backslash = TRUE;

#ifdef ALLOW_UNIX_BACKSLASH	/* This has problems: undigesting
				   a string gets complicated. */
			  if(source_unix_backslash) {
			    if(f77_unix_backslash || f90_unix_backslash) {
			      nonstandard(line_num,col_num,f90_unix_backslash,0);
			      msg_tail(": backslash escape sequence");
			    }
#ifdef LEX_RAWSTRINGS
				/* Store the backslash */
			    if(src_text_len < MAX_SRC_TEXT)
			      src_text_buf[src_text_len++] = curr_char;
#endif
			    inside_string = FALSE;/* so inline_comment works */
			    advance(); /* gobble the backslash */
			    inside_string = TRUE;
#ifdef LEX_RAWSTRINGS
			    value = curr_char;
#else /* !LEX_RAWSTRINGS*/
			    if(isadigit(curr_char)) { /* \octal digits */
			      value = BCD(curr_char);
			      while(isadigit(next_char)) {
				advance();
				value = value*8 + BCD(curr_char);
			      }
			    }
			    else if(curr_char == 'x') {
			      advance(); /* gobble the 'x' */
			      value = HEX(curr_char);
			      while(ishex(next_char)) {
				advance();
				value = value*16 + HEX(curr_char);
			      }
			    }/* end if octal or hex */
			    else switch(curr_char) {
#if __STDC__ + 0
			      case 'a': value = '\a'; break; /* alarm */
#else
			      case 'a': value = '\007'; break; /* alarm */
#endif
			      case 'b': value = '\b'; break; /* backspace */
			      case 'f': value = '\f'; break; /* formfeed */
			      case 'n': value = '\n'; break; /* newline */
			      case 'r': value = '\r'; break; /* carr return */
			      case 't': value = '\t'; break; /* h tab */
			      case 'v': value = '\v'; break; /* v tab */
			      case EOS: value = '\n'; break; /* a no-no */
				/* All others: \c --> c */
			      default:  value = curr_char; break;
			    }
#endif /* !LEX_RAWSTRINGS*/
			  }/* end if source_unix_backslash */
#endif /*ALLOW_UNIX_BACKSLASH*/

			}/* end if curr_char == backslash */

			if(src_text_len < MAX_SRC_TEXT)
			  src_text_buf[src_text_len++] = value;

			++len;
			last_col_num=col_num;
			advance();
		}
	}

#ifdef ALLOW_TYPELESS_CONSTANTS
				/* Watch for const like 'nnn'X */
	if(!inside_format) {
	  int space_seen_lately = iswhitespace(curr_char);
	  while(iswhitespace(curr_char))
	    advance();
	  if(isaletter(curr_char)) {
	    int c=makeupper(curr_char);
#ifndef LEX_RAWSTRINGS
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = quote_char;
#endif
	    if(src_text_len < MAX_SRC_TEXT)
	      src_text_buf[src_text_len++] = c;
	    advance();		/* Gobble the base character */

			/* F90 does not allow forms 'ddd'[BOZ].
			   Suppress message here if letter is not in [BOZ]
			   since that gets a warning in get_binary_const
			*/
	    if( (f77_typeless_constants || f90_typeless_constants) &&
		(c=='Z' || c=='O' || c=='B') ) {
	      nonstandard(token->line_num,token->col_num,f90_typeless_constants,0);
	    }

	    get_binary_const(token,c,space_seen_lately);
	    return;
	  }
	}
#endif /*ALLOW_TYPELESS_CONSTANTS*/

	if(len == 0) {
	  if(f77_char_extension) {
	    nonstandard(line_num,col_num,0,0);
	    msg_tail(": zero-length string");
	  }
	}

	if(quote_char != '\'') { /* Warn if quote is used instead of apost */
	  if(f77_quotemarks) {
	    nonstandard(token->line_num,token->col_num,0,0);
	    msg_tail(": character string should be delimited by apostrophes");
	  }
	}

	inside_string = FALSE;

	token->tclass = tok_string;
	token->size = len;
	token->src_text = new_src_text(src_text_buf,src_text_len);
#ifdef LEX_RAWSTRINGS
	token->value.string = token->src_text; /* Includes the initial quote */
#else
	token->value.string = token->src_text+1; /* Skips the initial quote */
#endif
				/* Under -port warn if char size > 255 */
	if(port_long_string) {
	  if(len > 255)
	    nonportable(line_num,col_num,
			"character constant length exceeds 255");
	}

#ifdef DEBUG_FORLEX
	if(debug_lexer
	   && src_text_buf[0] == quote_char) { /* skip if doing X'nnnn' */
		(void)fprintf(list_fd,"\nString:\t\t\t%s",token->value.string);
		(void)fprintf(list_fd,"\n\t\t(from\t%s)",token->src_text);
	}
#endif

} /* get_string */


		/* This routine is called when -pretty=extra-space or
		   missing-space are in effect, or when in free form
		   mode.  It figures out the right kind of warning to issue.
		*/
void space_violation( LINENO_t lineno, COLNO_t colno, const char *s )
{
    if(free_form && f90_freeform_space) {
	syntax_error(lineno,colno,s);
    }
    else {
	ugly_code(lineno,colno,s);
    }
}
/* End of Forlex module */

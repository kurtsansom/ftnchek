/*  $Id: symutils.h,v 1.2 2001/10/07 22:57:35 moniot Rel $

  Prototypes of routines defined in symutils.c

*/

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

PROTO(unsigned arg_count,( const Token *t ));
PROTO(int cp_tree_src_text,( char *s, const Token *t, int max ));
PROTO(int cp_tok_src_text,( char *s, const Token *t, int max ));
PROTO(int cp_list_src_text,( char *s, const Token *t, int max ));
PROTO(Token * reverse_tokenlist,( Token *t ));

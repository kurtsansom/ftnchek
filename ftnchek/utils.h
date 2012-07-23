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

#ifndef UTILS_H
#define UTILS_H

#if ! HAVE_STRCASECMP
#  if HAVE_STRICMP
#    define strcasecmp(A,B) stricmp(A,B)
#    define strncasecmp(A,B,C) strnicmp(A,B,C)
#  elif HAVE_STRCMPI
#    define strcasecmp(A,B) strcmpi(A,B)
#    define strncasecmp(A,B,C) strncmpi(A,B,C)
#  else /* neither STRICMP nor STRCMPI: roll our own */
#    define USE_OUR_CASECMP
#    define strcasecmp(A,B) our_strcasecmp(A,B)
#    define strncasecmp(A,B,C) our_strncasecmp(A,B,C)
#  endif
#endif

#ifdef USE_OUR_CASECMP
int our_strcasecmp(register const char *s1, register const char *s2);
int our_strncasecmp(const char *s1, const char *s2, size_t n);
#endif

char *strtolower(char *s);
#endif /* UTILS_H */

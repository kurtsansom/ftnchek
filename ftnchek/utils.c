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

#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "config.h"
#include "utils.h"

#ifdef USE_OUR_CASECMP

/* Code contributed by Nelson Beebe */
/**********************************************************************/
/****************************** strcasecmp ****************************/
/**********************************************************************/


/***********************************************************************
 Compare strings (ignoring case), and return:
	s1>s2:	>0
	s1==s2:  0
	s1<s2:	<0
***********************************************************************/

/* toupper() is supposed to work for all letters, but PCC-20 does it
incorrectly if the argument is not already lowercase; this definition
fixes that. */

#define TOUPPER(c) (islower((int)(c)) ? toupper((int)(c)) : (c))

int
our_strcasecmp(
register const char *s1,
register const char *s2
)
{
    while ((*s1) && (TOUPPER(*s1) == TOUPPER(*s2)))
    {
	s1++;
	s2++;
    }
    return((int)(TOUPPER(*s1) - TOUPPER(*s2)));
}

#ifdef TOUPPER
#undef TOUPPER
#endif /* TOUPPER */


/**********************************************************************/
/****************************** strncasecmp ***************************/
/**********************************************************************/


/***********************************************************************
Compare strings ignoring case, stopping after n characters, or at
end-of-string, whichever comes first.
***********************************************************************/

int
our_strncasecmp(
const char	*s1,
const char	*s2,
size_t		n
)
{
    int	   c1;
    int	   c2;
    int	   result;

    for (; (n > 0) && *s1 && *s2; ++s1, ++s2, --n)
    {
	c1 = 0xff & (islower((int)(*s1)) ? (int)*s1 : tolower((int)(*s1)));
	c2 = 0xff & (islower((int)(*s2)) ? (int)*s2 : tolower((int)(*s2)));
	if (c1 < c2)
	    return (-1);
	else if (c1 > c2)
	    return (1);
    }
    if (n <= 0)		   /* first n characters match */
	result = 0;
    else if (*s1 == '\0')
	result = ((*s2 == '\0') ? 0 : -1);
    else /* (*s2 == '\0') */
	result = 1;

    return (result);
}

#endif /* end USE_OUR_CASECMP */

/* Converts all uppercase characters of string s to lowercase, and
 * returns pointer to the converted string.
 */
char *
strtolower(char *s)
{
  while( *s ) {
    if( isupper((int)(*s)) )
      (*s) = (char)tolower((int)(*s));
    s++;
  }
  return s;
}

/* const_strcpy replaces old method of assigning src pointer
   to dest, which is illegal when src is const char *.  It allocates
   new space and copies src to dest.  The contents of src must be
   a null-terminated string.
 */
char *const_strcpy(const char *src)
{
  int numchars = strlen(src);
  char *dest = (char *)malloc(numchars+1);
  return strcpy(dest,src);
}

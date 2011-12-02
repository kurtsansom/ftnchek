/* cmp.c:

   Simple replacement for Unix cmp, for use with the check.bat script.
   This program lacks options such as -l and -s.

*/


/*

Copyright (c) 2000 by Robert K. Moniot.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
ROBERT K. MONIOT OR FORDHAM UNIVERSITY BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of ftnchek shall not be used
in advertising or otherwise to promote the sale, use or other dealings in
this Software without prior written authorization from the author.

*/


#include <stdio.h>
#include <stdlib.h>

#define MAXLINE 1024		/* Big enough for present purposes */

long char_count, line_count;	/* Total byte and line counts */

char *myname;			/* For error messages */

				/* Prototypes */
FILE *fopen_or_fail( char *path, char *mode );
int cmp_lines( char *line1, char *line2);


				/* Open file or fail with error message.
				 */
FILE *fopen_or_fail( char *path, char *mode )
{
    FILE *fd;
    fd = fopen(path,mode);
    if( fd == NULL ) {
	fprintf(stderr,"%s: ",myname);
	perror(path);
	exit(2);
    }
    return fd;
}

			/* Compare two lines, return 0 if equal and 1 if
			   different.  Also keep track of line & char count.
			 */
int cmp_lines( char *line1, char *line2)
{
    int i;
    for(i=0; line1[i] != '\0' && line2[i] != '\0'; i++) {
	if(line1[i] != line2[i])
	    return 1;

	char_count++;
	if(line1[i] == '\n')
	    line_count++;
    }
    return 0;
}

int main( int argc, char *argv[] )
{
    FILE *fd1, *fd2;			 /* input file descriptors */
    char line1[MAXLINE], line2[MAXLINE]; /* buffers for input */
    char *read1, *read2;		 /* return status from fgets */
    int status = 0;			 /* exit status */

    myname = argv[0];
				/* Make sure exactly 2 args are present */
    if( argc != 3 ) {
	fprintf(stderr,"Usage: %s file1 file2\n", myname);
	exit(2);
    }

				/* Open files, die if cannot */
    fd1 = fopen_or_fail( argv[1], "r" );
    fd2 = fopen_or_fail( argv[2], "r" );

				/* Read files and compare line by line */

    line_count = char_count = 1;

    for(;;) {

	read1 = fgets(line1, sizeof(line1), fd1);
	read2 = fgets(line2, sizeof(line2), fd2);

	if( read1 == NULL || read2 == NULL ) {
	    if(read1 != NULL || read2 != NULL) {
		printf("%s: EOF on %s\n", myname,
		       read1 == NULL? argv[1]: argv[2]);
		status = 1;
	    }
	    break;
	}

				/* Compare the two lines */
	if( (status = cmp_lines(line1, line2)) != 0 ) {
	    printf("%s %s differ: char %ld, line %ld\n",argv[1],argv[2],
		   char_count,line_count);
	    break;
	}

    }

    fclose(fd1);
    fclose(fd2);

    exit(status);
}

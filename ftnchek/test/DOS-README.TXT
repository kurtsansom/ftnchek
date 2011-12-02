DOS/Win32 test suite for ftnchek

CHECK.BAT and its associated files provide some checking of FTNCHEK
compiled for DOS/Win32 similar to the Unix makefile.

Some tests had to be skipped, mostly because stderr cannot be redirected
on DOS/Win9x systems and, in particular, cannot be combined with stdout
(at least I don't know how to do this - hints welcome).

Note that some tests require that the HELP.*, INCLUDE.*, and
WILDCARD.* files provided in OKAY.DOS be used instead of the ones in
OKAY.  COMPARE.BAT looks to see if the master file exists in OKAY.DOS
and if so, uses that.  This avoids error messages due to different
standard command-line option prefixes on DOS and UNIX ('/' instead of
'-') and different directory separators ('\' instead of '/').

Furthermore, I left out the DCL2INC test since this uses AWK which is not
available on stock DOS/Windows systems (although ports of GAWK to Win32
are available on the net).  For the same reason, I avoided using TOUCH.

The CMP program to compare the contents of two files was kindly provided
by Robert Moniot.  Make sure CMP.EXE is in the test folder (or compile
and link CMP.C) before starting the test if you do not have a CMP program
on your path.

The test scripts require that FTNCHEK.EXE be located in the main ftnchek
folder (i.e. one level above the TEST folder).

Unfortunately, MSDOS doesn't support GOSUB and RETURN in batch files
like DRDOS6 did.  To avoid proliferating batch files, I decided to
use GOTO and write "spaghetti" code occasionally (in particular in
makedcls.bat).

--
Gunnar Duus

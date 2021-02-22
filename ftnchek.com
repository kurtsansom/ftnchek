$!  Ftnchek setup.  (Fortran program checker)
$! Define FTNCHEK as a DCL foreign command
$ FTNCHEK :== $ UNSP:FTNCHEK
$ write sys$output "FTNCHEK ready to run"
$!
$! Set up the help library This script sets the first available
$! logical of HLP$LIBRARY, HLP$LIBRARY_1, HLP$LIBRARY_2, or
$! HLP$LIBRARY_3 in process table equal to the FTNCHEK help file.  If
$! that doesn't do it for you, you are on your own.
$! 
$! Author: R. Moniot, October 1995
$!
$ if f$trnlnm("hlp$library","LNM$PROCESS_TABLE") .eqs. ""
$ then
$   define hlp$library "UNSP:FTNCHEK.HLB"
$ else
$   if  f$trnlnm("hlp$library","LNM$PROCESS_TABLE") .nes. "UNSP:FTNCHEK.HLB"
$   then
$     if f$trnlnm("hlp$library_1","LNM$PROCESS_TABLE") .eqs. ""
$     then
$       define hlp$library_1 "UNSP:FTNCHEK.HLB"
$     else
$       if  f$trnlnm("hlp$library_1","LNM$PROCESS_TABLE") .nes. "UNSP:FTNCHEK.HLB"
$       then
$         if f$trnlnm("hlp$library_2","LNM$PROCESS_TABLE") .eqs. ""
$         then
$           define hlp$library_2 "UNSP:FTNCHEK.HLB"
$         else
$           if  f$trnlnm("hlp$library_2","LNM$PROCESS_TABLE") .nes. "UNSP:FTNCHEK.HLB"
$           then
$             if f$trnlnm("hlp$library_3","LNM$PROCESS_TABLE") .eqs. ""
$             then
$               define hlp$library_3 "UNSP:FTNCHEK.HLB"
$             else
$               if  f$trnlnm("hlp$library_3","LNM$PROCESS_TABLE") .nes. "UNSP:FTNCHEK.HLB"
$               then
$               write sys$output "Help library not defined: available logical name not found"
$               endif
$             endif
$           endif
$         endif
$       endif
$     endif
$   endif
$ endif


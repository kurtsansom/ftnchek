rem Batch file for processing the LISTOUT group.

rem The first set uses DEFAULT_FLAGS.
for %%f in (average comclash comtest correct iffy labeltest)  do call listout2.bat %%f %1 %2
for %%f in (sequence substr autoarray cycle-exit quad relops) do call listout2.bat %%f %1 %2
for %%f in (embedded-space arg-alias common-alias primes)     do call listout2.bat %%f %1 %2
for %%f in (dec-parameter pure-function case allkeywords)     do call listout2.bat %%f %1 %2

rem These files all use individual flags (see *.opt).
echo   backslash
..\ftnchek.exe -nof77 -source=4 -wrap=65 %2 backslash.f > backslash.%1
call compare.bat backslash.%1
echo   blockerrors
..\ftnchek.exe -list -nof77 -style %2 blockerrors.f > blockerrors.%1
call compare.bat blockerrors.%1
echo   chestnuts
..\ftnchek.exe -nopretty %2 chestnuts.f > chestnuts.%1
call compare.bat chestnuts.%1
echo   comcmp
..\ftnchek.exe -common:3 -nof77 -port -quiet %2 comcmp.f -ref > comcmp.%1
call compare.bat comcmp.%1
echo   complex
..\ftnchek.exe -sym %2 complex.f > complex.%1
call compare.bat complex.%1
echo   comusage
..\ftnchek.exe -common=volatile %2 comusage.f > comusage.%1
call compare.bat comusage.%1
echo   dectab
..\ftnchek.exe -source=dec-tab -sym -div %2 dectab.f > dectab.%1
call compare.bat dectab.%1

rem Skip dims.f/dims.opt. This test will produce differences because stdout
rem and stderr can't be combined on MSDOS/Windows 9x systems.

echo   do_enddo
..\ftnchek.exe -port %2 do_enddo.f > do_enddo.%1
call compare.bat do_enddo.%1

rem The 'help' test requires that the UNIX result files be replaced with the
rem special MSDOS ones.  Otherwise, it will report differences because the
rem standard command-line option prefix for MSDOS/Win32 is '/' instead of '-'.
echo   help
..\ftnchek.exe %2 -help > help.%1
call compare.bat help.%1

rem The 'include' test requires that the UNIX result files be replaced with
rem the special MSDOS ones.  Otherwise, it will report differences because
rem the directory separator for MSDOS/Win32 is '\' instead of '/'.
echo   include
..\ftnchek.exe -list -symt -port -include=.\Include %2 include.f > include.%1
call compare.bat include.%1

echo   namelist
..\ftnchek.exe -sym -lis %2 namelist.f > namelist.%1
call compare.bat namelist.%1
echo   noblanks
..\ftnchek.exe -nopretty %2 noblanks.f > noblanks.%1
call compare.bat noblanks.%1
echo   strings
..\ftnchek.exe -port %2 strings.f > strings.%1
call compare.bat strings.%1
echo   t208f
..\ftnchek.exe -list -symt -sixchar %2 t208f.f > t208f.%1
call compare.bat t208f.%1
echo   unixincluded
..\ftnchek.exe %2 unixincluded.f > unixincluded.%1
call compare.bat unixincluded.%1

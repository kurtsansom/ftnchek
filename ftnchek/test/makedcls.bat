rem Batch file for checking the generation of declarations files.
rem Please note that, for various reasons, this is not an exact
rem representation of the corresponding makefile code.
..\ftnchek.exe -nocheck -makedcls=%2 %1.f > NUL
if exist %1.dcl goto DCL_EXIST
if exist Okay\%1.%3 goto MISSING
goto END

:DCL_EXIST
rem is %1.dcl empty?
dir %1.dcl | find " 0  " > NUL
if not errorlevel 1 goto EMPTY
goto ELSE

:EMPTY
move %1.dcl Vary\%1.%3 > NUL
echo Empty declarations file Vary\%1.%3 should have been deleted.
set CHECK_STATUS=FAILED
goto END

:ELSE
if exist Okay\%1.%3 goto COMPARE
move %1.dcl Vary\%1.%3
echo No master file to compare to.  Presumably, Vary\%1.%3
echo should not have been created.
set CHECK_STATUS=FAILED
goto END

:COMPARE
move %1.dcl %1.%3 > NUL
call compare.bat %1.%3
goto END

:MISSING
rem is Okay\%1.dci empty?
dir Okay\%1.%3 | find " 0 %1"  > NUL
if not errorlevel 1 goto END
echo Non-empty declarations file %1.%3 is missing.
set CHECK_STATUS=FAILED

:END

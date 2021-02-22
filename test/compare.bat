rem Batch file to compare File in the current directory with
rem master file in directory .\Okay.  If there are differences,
rem CHECK_STATUS is set to FAILED, and the file is moved to
rem .\Vary.
rem Note: if master file is found in Okay.dos, use that instead
rem of the one in Okay.
set OKAY_DIR=Okay
if exist Okay.dos\%1 set OKAY_DIR=Okay.dos
cmp %OKAY_DIR%\%1 %1
if errorlevel 1 goto FAIL
if exist %1 del %1
goto END

:FAIL
set CHECK_STATUS=FAILED
echo --------------------------------------------------
echo Differences in %OKAY_DIR%\%1 Vary\%1
move %1 Vary\%1 > NUL

:END

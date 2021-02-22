rem Batch file for looking out for situations that will cause the check to fail.
if not exist ..\ftnchek.exe goto NOEXE

rem It would be a good idea to check if CMP.EXE is present somewhere on the
rem search path.  But how to do this?

rem No need to check for .ftnchekrc since this name is illegal under MSDOS/Win32.

if exist ftnchek.ini goto INI
if exist ..\ftnchek.ini goto INI
goto END

:NOEXE
echo ERROR === Cannot run checks: no ftnchek found ===

echo Use CTRL-C to quit batch processing.
pause
goto END

:INI
echo WARNING === ftnchek.ini exists ===
echo Delete or rename it before running check.

echo Use CTRL-C to quit batch processing.
pause
goto END

:END

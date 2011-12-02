rem Batch file for checking listing output (in novice and expert mode).
echo   %1
..\ftnchek.exe %DEFAULT_FLAGS% %3 %1.f > %1.%2
call compare.bat %1.%2

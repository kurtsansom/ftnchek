rem Batch file for checking -vcg setting.
..\ftnchek.exe -vcg %1.f > NUL
call compare.bat %1.vcg

rem Batch file for checking -args setting.
..\ftnchek.exe -args=%2 %1.f > %1.fc%2
call compare.bat %1.fc%2

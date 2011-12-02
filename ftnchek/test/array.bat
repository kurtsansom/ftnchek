rem Batch file for checking -array setting.
..\ftnchek.exe -array=%2 -port %1.f > %1.fc%2
call compare.bat %1.fc%2

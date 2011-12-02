rem Batch file for checking -common setting.
..\ftnchek.exe -common=%2 -nof77 %1.f > %1.fc%2
call compare.bat %1.fc%2

rem Batch file for checking listing output for various size & type
rem clashes in expressions and assignment statements.
echo   %1
..\ftnchek.exe -notrun -noport -nof77 -lis    %1.f > %1.fc1
call compare.bat %1.fc1
..\ftnchek.exe -notrun -noport   -f77         %1.f > %1.fc2
call compare.bat %1.fc2
..\ftnchek.exe -notrun   -port -nof77         %1.f > %1.fc3
call compare.bat %1.fc3
..\ftnchek.exe -notrun   -port   -f77         %1.f > %1.fc4
call compare.bat %1.fc4
..\ftnchek.exe   -trun -noport -nof77         %1.f > %1.fc5
call compare.bat %1.fc5
..\ftnchek.exe   -trun -noport   -f77         %1.f > %1.fc6
call compare.bat %1.fc6
..\ftnchek.exe   -trun   -port -nof77         %1.f > %1.fc7
call compare.bat %1.fc7
..\ftnchek.exe   -trun   -port   -f77         %1.f > %1.fc8
call compare.bat %1.fc8
..\ftnchek.exe   -trun   -port -nof77 -word=2 %1.f > %1.fcA
call compare.bat %1.fcA
..\ftnchek.exe   -trun   -port -nof77 -word=4 %1.f > %1.fcB
call compare.bat %1.fcB
..\ftnchek.exe   -trun   -port -nof77 -word=8 %1.f > %1.fcC
call compare.bat %1.fcC
..\ftnchek.exe   -trun -noport -nof77 -word=2 %1.f > %1.fcD
call compare.bat %1.fcD
..\ftnchek.exe   -trun -noport -nof77 -word=4 %1.f > %1.fcE
call compare.bat %1.fcE
..\ftnchek.exe   -trun -noport -nof77 -word=8 %1.f > %1.fcF
call compare.bat %1.fcF

rem Batch file for checking standard-compliance warnings.
echo   %1
..\ftnchek.exe -nof77 -nof90 -nof95 %1.f > %1.fc0
call compare.bat %1.fc0
..\ftnchek.exe   -f77 -nof90 -nof95 %1.f > %1.fc1
call compare.bat %1.fc1
..\ftnchek.exe -nof77   -f90 -nof95 %1.f > %1.fc2
call compare.bat %1.fc2
..\ftnchek.exe -nof77 -nof90   -f95 %1.f > %1.fc3
call compare.bat %1.fc3

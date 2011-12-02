@echo off
rem CHECK.BAT - batch file for smoke-testing ftnchek.
rem gunnar.duus@airbus.dasa.de, 12-May-01
rem moniot@fordham.edu silencing initialization of Vary 16-Nov-03
rem
rem See Readme.txt for details.

rem Make sure that the Vary folder exists
if not exist Vary mkdir Vary
rem Make sure that the Vary folder is empty.
if exist Vary\*.fc? del Vary\*.fc?
if exist Vary\*.dc? del Vary\*.dc?
echo.
echo "=========================================================="
echo There should be no file differences reported by these tests.
echo Output files matching master output files will be removed,
echo leaving any erroneous output files in directory Vary
echo for manual examination.  If you built ftnchek with non-
echo standard compilation options, there may be some differences.
echo "=========================================================="
pause
set CHECK_STATUS=OK

rem PRECHECK.BAT looks out for situations that will cause the check to
rem fail: ftnchek not built or ini file present in current directory.
call precheck.bat

rem LISTOUT programs are simple, and all use the default options or else the
rem options given in their .opt file.  These produce listing files *.fcl
rem Note: Those with .opt files may have associated routines in other
rem files that do not appear here.
echo checking listing output:
if exist *.fcl del *.fcl
set DEFAULT_FLAGS=-list -symt -port
set FTNCHEK_F77=YES
call listout.bat fcl
echo done

rem Do the LISTOUT group again, but in expert mode (-nonovice switch).
echo checking listing output in expert mode:
call listout.bat fcx -nonovice
set FTNCHEK_F77=NO
echo done

rem Check free source form
set FTNCHEK_F90=YES
echo checking freeform:
..\ftnchek.exe %DEFAULT_FLAGS% blockcheck.f90 > blockcheck.fcl
call compare.bat blockcheck.fcl
..\ftnchek.exe %DEFAULT_FLAGS% model.f90 > model.fcl
call compare.bat model.fcl
set FTNCHEK_F90=NO

rem Next series of targets checks variations of output as options change.
rem The output files are named *.fc[1-9A-F] for different cases.  Note that
rem in some instances the same source file is also in a set that is used
rem to check source listing as usual, with output file named *.fcl

rem Check standard-compliance warnings.
echo checking standard-compliance warnings:
for %%f in (iokeywords f95deleted) do call checkf90.bat %%f
echo done

rem Check listing output for various size & type clashes in expressions
rem and assignment statements.
echo checking size and type mismatch:
for %%f in (assign wordsize mixed-equiv) do call checkexpr.bat %%f
echo done

rem Check the -args=n setting.
echo checking -args setting:
echo   -args=0
for %%f in (args01 args02 args03 args04 args05) do call checkargs.bat %%f 0
for %%f in (args06 args07 args08 args09 args10) do call checkargs.bat %%f 0
for %%f in (args11 args12 args13 args14 args15) do call checkargs.bat %%f 0
echo   -args=1
for %%f in (args01 args02 args03 args04 args05) do call checkargs.bat %%f 1
for %%f in (args06 args07 args08 args09 args10) do call checkargs.bat %%f 1
for %%f in (args11 args12 args13 args14 args15) do call checkargs.bat %%f 1
echo   -args=2
for %%f in (args01 args02 args03 args04 args05) do call checkargs.bat %%f 2
for %%f in (args06 args07 args08 args09 args10) do call checkargs.bat %%f 2
for %%f in (args11 args12 args13 args14 args15) do call checkargs.bat %%f 2
echo   -args=3
for %%f in (args01 args02 args03 args04 args05) do call checkargs.bat %%f 3
for %%f in (args06 args07 args08 args09 args10) do call checkargs.bat %%f 3
for %%f in (args11 args12 args13 args14 args15) do call checkargs.bat %%f 3
echo done

rem Check the -array=n setting.
echo checking -array setting:
for %%v in (0 1 2 3) do call array.bat arrayclash %%v
echo done

rem Check the -common=n setting.
echo checking -common setting:
echo   -common=0
for %%f in (comcmp comusage) do call common.bat %%f 0
echo   -common=1
for %%f in (comcmp comusage) do call common.bat %%f 1
echo   -common=2
for %%f in (comcmp comusage) do call common.bat %%f 2
echo   -common=3
for %%f in (comcmp comusage) do call common.bat %%f 3
echo done

echo checking -intrinsic setting:
..\ftnchek.exe -intrinsic=none intrinstest.f > intrinstest.fc0
call compare.bat intrinstest.fc0
..\ftnchek.exe -intrinsic=none,extra intrinstest.f > intrinstest.fc1
call compare.bat intrinstest.fc1
..\ftnchek.exe -intrinsic=none,unix intrinstest.f > intrinstest.fc2
call compare.bat intrinstest.fc2
..\ftnchek.exe -intrinsic=none,vms intrinstest.f > intrinstest.fc3
call compare.bat intrinstest.fc3
..\ftnchek.exe -intrinsic=extra,unix,no-vms,no-rand-no-arg intrinstest.f > intrinstest.fc4
call compare.bat intrinstest.fc4
..\ftnchek.exe -intrinsic=extra,unix,no-vms,no-rand-one-arg intrinstest.f > intrinstest.fc5
call compare.bat intrinstest.fc5
..\ftnchek.exe -intrinsic=extra,unix,no-vms,no-iargc-no-arg intrinstest.f > intrinstest.fc6
call compare.bat intrinstest.fc6
..\ftnchek.exe -intrinsic=extra,unix,no-vms,no-iargc-one-arg intrinstest.f > intrinstest.fc7
call compare.bat intrinstest.fc7
..\ftnchek.exe -intrinsic=all intrinstest.f > intrinstest.fc8
call compare.bat intrinstest.fc8
echo done

rem Check the -output=file string setting.
rem average.fc2 can't be created because redirecting stderr does not work
rem on MSDOS/Windows 9x systems. So skip this part of the test.
echo checking -output setting:
..\ftnchek.exe -symt -list -port -out=average.out average.f
call compare.bat average.out
echo done

rem Check the -usage=options setting.
echo checking -usage setting:
..\ftnchek.exe -usage=none usage.f > usage.fc0
call compare.bat usage.fc0
..\ftnchek.exe -usage=none,com-var-uninitialized,var-uninitialized usage.f > usage.fc1
call compare.bat usage.fc1
..\ftnchek.exe -usage=none,com-block-unused,com-var-set-unused,com-var-unused,ext-unused,var-set-unused,var-unused usage.f > usage.fc2
call compare.bat usage.fc2
..\ftnchek.exe -usage=all usage.f > usage.fc3
call compare.bat usage.fc3
echo done

rem Check creation and use of project files.
rem Note that correct.f is used in LISTOUT also, but with default
rem compilation options.  So we make listing file suffix .fc1 here.

rem Skip the comparison of correct.fc1. It will produce differences because
rem stdout and stderr can't be combined on MSDOS/Windows 9x systems.
echo checking project files:
if exist correct.prj del correct.prj
if exist correct.fc1 del correct.fc1
if exist correct.pcl del correct.pcl
..\ftnchek.exe -project correct.f > correct.fc1
..\ftnchek.exe correct.prj > correct.pcl
for %%f in (correct.prj correct.pcl) do call compare.bat %%f
if exist correct.fc1 del correct.fc1
echo done

rem Check the reading of startup file.
echo checking reading rc file:
echo f77=all > ftnchek.ini
echo symtab >> ftnchek.ini
..\ftnchek.exe -f77=no-long-name,no-name-underscore do_enddo.f > rc.fcl
call compare.bat rc.fcl
if exist ftnchek.ini del ftnchek.ini
echo done

rem The 'wildcard' tests require that the UNIX result files be replaced with
rem the special MSDOS ones.  Otherwise, they will report differences because
rem the standard command-line option prefix for MSDOS/Win32 is '/' instead of
rem '-'.

rem Check the interpretation of wildcards in warn-option lists
echo checking interpreting wildcards:
..\ftnchek.exe -port=mixed*        -port=help > wildcard.fc1
call compare.bat wildcard.fc1
..\ftnchek.exe -usage=no-*var*    -usage=help > wildcard.fc2
call compare.bat wildcard.fc2
..\ftnchek.exe -usage=no-var*     -usage=help > wildcard.fc3
call compare.bat wildcard.fc3
..\ftnchek.exe -f77=*array*,format* -f77=help > wildcard.fc4
call compare.bat wildcard.fc4

rem Skip wildcard.fc5. This test will produce differences because stdout
rem and stderr can't be combined on MSDOS/Windows 9x systems.
echo done

rem Check the generation of declarations files.  Listing goes to NUL.
rem The values in VAL are used as 3rd char in .fc? suffix.  They are
rem converted to -makedcls=num values by raising 2 to the power.  Thus
rem file.dc0 contains -makedcls=1, ... file.dca contains -makedcls=1024.
rem There is also a test to be sure that empty .dcl files are removed
rem automatically as they should be.
echo checking -makedcls setting:
echo   -makedcls=1    (=2**0)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 1 dc0
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 1 dc0
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 1 dc0
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 1 dc0

echo   -makedcls=2    (=2**1)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 2 dc1
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 2 dc1
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 2 dc1
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 2 dc1

echo   -makedcls=4    (=2**2)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 4 dc2
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 4 dc2
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 4 dc2
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 4 dc2

echo   -makedcls=8    (=2**3)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 8 dc3
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 8 dc3
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 8 dc3
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 8 dc3

echo   -makedcls=16   (=2**4)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 16 dc4
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 16 dc4
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 16 dc4
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 16 dc4

echo   -makedcls=32   (=2**5)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 32 dc5
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 32 dc5
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 32 dc5
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 32 dc5

echo   -makedcls=64   (=2**6)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 64 dc6
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 64 dc6
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 64 dc6
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 64 dc6

echo   -makedcls=128  (=2**7)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 128 dc7
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 128 dc7
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 128 dc7
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 128 dc7

echo   -makedcls=256  (=2**8)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 256 dc8
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 256 dc8
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 256 dc8
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 256 dc8

echo   -makedcls=512  (=2**9)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 512 dc9
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 512 dc9
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 512 dc9
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 512 dc9

echo   -makedcls=1024 (=2**10)
for %%f in (builtin dcltest dims namelist params)            do call makedcls.bat %%f 1024 dca
for %%f in (t208a t208b t208c t208d t208e t208f t208g t208h) do call makedcls.bat %%f 1024 dca
for %%f in (t208i t208j t208k t208l t208m t208n t208o t208p) do call makedcls.bat %%f 1024 dca
for %%f in (t208q t208r t208s t208t t208u t208v t208w t208x) do call makedcls.bat %%f 1024 dca
echo done

rem Check -vcg switch.
echo checking -vcg setting:
for %%f in (comcmp comusage) do call vcg.bat %%f
echo done

rem Check of dcl2inc processing is not possible under MSDOS/Windows.

rem Print test results.
if %CHECK_STATUS%==OK goto CHECK_OK

echo "=============================================="
echo "==  There were differences.  Check failed.  =="
echo "=============================================="
set CHECK_STATUS=
goto END

:CHECK_OK
echo No differences found.  Check successful.

:END

$! Name: CHECK.COM
$! Author: Bob Wells (wells@atm.ox.ac.uk)
$! Date: 14-Mar-00
$! $Id: check.com,v 1.11 2001/06/02 00:35:54 moniot Rel $
$! Purpose: To provide some checking of the VMS FTNCHEK distribution
$!          as in the unix distribution make check.
$!          Most of the prolems arise from the different way in which
$!          files are named and some effort is needed to to produce output
$!          identical to that of unix systems.
$!          MAKEDCL2INC involves the use of AWK which is not provided in the
$!          standard VMS distribution.  It can be obtained from the Freeware CD
$!          or downloaded from http://www.openvms.digital.com/freeware
$! Usage:  @CHECK
$!
$ Noadwrite: Subroutine ! P1= Text to be output
$! Purpose: perform non-advancing write to screen
$! Special case: terminates with newline if P1 = "done"
$ If P1 .NES. "done"
$ Then
$   Read/Time=0/Error=Next SYS$COMMAND Line/Prompt="''P1'"
$ Else
$   Read/Time=0/Error=Next SYS$COMMAND Line/Prompt="''F$FAO("!AS!/","done")'"
$ Endif
$ Next:
$ EndSubroutine
$!
$ Doedit: Subroutine ! P1=Filename P2=string P3=substitute string
$! Purpose: replace all occurrences of a string in a file
$ Open/Write TMP REPLACE.EDT
$ Write TMP "substitute\" + P2 + "\" + F$Edit(P3,"LOWERCASE") + "\ 1:999999 /notype"
$ Write TMP "ex"
$ Close TMP
$ Assign NLA0: SYS$OUTPUT
$ Edit/EDT/NOJOURNAL/Command=REPLACE.EDT 'P1'
$ Deassign SYS$OUTPUT
$ Delete/Noconfirm REPLACE.EDT;
$ EndSubroutine
$!
$ Doftnchek: Subroutine ! P1=FTNCHEK argument list P2=Output file
$! Purpose: run FTNCHEK with specified arguments and redirect output to a file
$ Open/Write TMP 'P2'
$ Set Noon
$ Assign/User TMP SYS$OUTPUT
$ Assign/User TMP SYS$ERROR
$ Ftnchek 'P1'
$ Close TMP
$ Set On
$ EndSubroutine
$!
$ Compare: Subroutine ! P1=filename P2=Unix extension
$! Purpose: compare two FTNCHEK output files for differences
$ Char = F$Extract ( F$Locate(".",P1)-1, 1, P1 ) + P2
$ Vary = "[.VARY]" + P1
$ Okay = "[.OKAY]" + P1
$ Call Doedit 'Vary' .;9 'Char'
$ Differences/Ignore=(Spacing,Blank)/Output=F1.TMP 'Vary' 'Okay'
$ Open/Read TMP F1.TMP 
$ Read TMP Line
$ Close TMP
$ Delete/Noconfirm F1.TMP;
$ If Line .EQS. "Number of difference sections found: 0"
$ Then
$  Call Noadwrite "."
$  Delete/Noconfirm 'Vary';*
$ Else
$  Write SYS$OUTPUT "====> Differences found in " + P1 + " test <===="
$  Write SYS$OUTPUT "Master: " + F$Search(Okay) 
$  Write SYS$OUTPUT "Test:   " + F$Search(Vary) 
$  Write SYS$OUTPUT "---------------------------------------------------------"
$  Purge 'Vary'
$ EndIf
$ EndSubroutine
$!
$ Check: Subroutine ! P1=file P2=.ext of output P3=options
$! Purpose: run FTNCHEK with specified options and compare output with master
$ Tmpfil  = F$Extract ( 0, F$Length(P1)-1, P1 ) + ".;9"
$ Cffil   = P1 + "." + P2
$ Params  = P3 + " " + Tmpfil
$ IF F$Search ("''P1'.F") .NES. "" Then Rename 'P1'.F 'Tmpfil'
$ Call Doftnchek "''Params'" [.VARY]'Cffil'
$ IF F$Search (Tmpfil) .NES. "" Then Rename 'Tmpfil' 'P1'.F
$ Call Compare 'Cffil' ".f"
$ EndSubroutine
$
$ Checklist: Subroutine ! P1=file
$! Purpose: run FTNCHEK with list checks using the .OPT files
$ Optfil = "''P1'.OPT"
$ If F$Search(Optfil) .NES. ""
$ Then
$  Copy 'Optfil' F1.TMP
$  Call Doedit F1.TMP "''P1'.f" " " 
$  Open/Read TMP F1.TMP
$  Read TMP Params
$  Close TMP
$  Delete/Noconfirm F1.TMP;*
$ Else
$  Params = "-list -symt -port"
$ Endif
$ Call Check 'P1' FCL "''Params'"
$ EndSubroutine
$!------------------------------------------------------------------------------
$ CheckF90: Subroutine ! P1=file P2=.ext of output P3=options
$! Purpose: like Check but for .F90 file extension
$ Tmpfil  = F$Extract ( 0, F$Length(P1)-1, P1 ) + ".;9"
$ Cffil   = P1 + "." + P2
$ Params  = P3 + " " + Tmpfil
$ IF F$Search ("''P1'.F90") .NES. "" Then Rename 'P1'.F90 'Tmpfil'
$ Call Doftnchek "''Params'" [.VARY]'Cffil'
$ IF F$Search (Tmpfil) .NES. "" Then Rename 'Tmpfil' 'P1'.F90
$ Call Compare 'Cffil' ".f90"
$ EndSubroutine
$
$ Checkfreeform: Subroutine ! P1=file
$! Purpose: run FTNCHEK with freeform checks using the .OPT files
$ Optfil = "''P1'.OPT"
$ If F$Search(Optfil) .NES. ""
$ Then
$  Copy 'Optfil' F1.TMP
$  Call Doedit F1.TMP "''P1'.f90" " " 
$  Open/Read TMP F1.TMP
$  Read TMP Params
$  Close TMP
$  Delete/Noconfirm F1.TMP;*
$ Else
$  Params = "-list -symt -port"
$ Endif
$ Call CheckF90 'P1' FCL "''Params'"
$ EndSubroutine
$!------------------------------------------------------------------------------
$ Checkexpert: Subroutine ! P1=file
$! Purpose: run FTNCHEK with list checks using the .OPT files and -nonovice
$! Adapted from Checklist by R. Moniot.
$ Optfil = "''P1'.OPT"
$ If F$Search(Optfil) .NES. ""
$ Then
$  Copy 'Optfil' F1.TMP
$  Call Doedit F1.TMP "''P1'.f" " " 
$  Open/Read TMP F1.TMP
$  Read TMP Params
$  Close TMP
$  Delete/Noconfirm F1.TMP;*
$ Else
$  Params = "-list -symt -port"
$ Endif
$ Params = "-nonovice " + Params
$ Call Check 'P1' FCX "''Params'"
$ EndSubroutine
$!
$!------------------------------------------------------------------------------
$ On Control_Y Then GoTo Finish
$ On Warning Then GoTo Finish
$ On Error Then GoTo Finish
$ Setverify = 'F$Verify(0)'
$ Startdir = F$Environment("DEFAULT")
$ Comma = ","                 ! For use as a list separator
$!
$! Set up FTNCHEK command
$ File = F$Search("FTNCHEK.EXE")
$ If File .EQS. ""
$  Then
$   Write SYS$OUTPUT "ERROR ===> Cannot run checks: no FTNCHEK found <==="
$   GoTo Finish
$  Else
$   FTNCHEK := $'File'
$ EndIf
$!
$ Set Default [.TEST]
$!
$! Check for pre-defined logical (environment) variables
$ Assign F1.TMP SYS$OUTPUT
$ Show Logical
$ Deassign SYS$OUTPUT
$ Search/Nowarning/Output=F2.TMP F1.TMP FTNCHEK_
$ Delete/Noconfirm F1.TMP;
$ Open/Read TMP F2.TMP
$ Read/End_Of_File=Empty_Logical TMP Line
$  Close TMP
$  Write SYS$OUTPUT "WARNING ===> Logical names are defined <===
$  Type F2.TMP
$  Write SYS$OUTPUT "These need to be DEASSIGNed before running CHECK"
$  Delete/Noconfirm F2.TMP;
$  GoTo Finish
$ Empty_Logical:
$ Close TMP
$ Delete/Noconfirm F2.TMP;
$!
$! Check for existing default configuration files
$ If F$Search("SYS$LOGIN:.FTNCHEKRC") .NES. ""
$  Then
$  Write SYS$OUTPUT "WARNING ===> SYS$LOGIN:.FTNCHEKRC exists <==="
$  Write SYS$OUTPUT "delete or rename it before running check"
$  GoTo Finish
$ EndIf
$ If F$Search("SYS$LOGIN:FTNCHEK.INI") .NES. ""
$  Then
$  Write SYS$OUTPUT "WARNING ===> SYS$LOGIN:FTNCHEK.INI exists <==="
$  Write SYS$OUTPUT "delete or rename it before running check"
$  GoTo Finish
$ EndIf
$!
$! Check for [.VARY] subdirectory and create it if it does not exist
$ If F$Search("VARY.DIR") .EQS. ""
$  Then
$  Write SYS$OUTPUT "NOTE: [.VARY] subdirectory does not exist: creating it now."
$  Create/Dir [.VARY]
$ Endif
$!
$ Write SYS$OUTPUT "================================================================"
$ Write SYS$OUTPUT "There should be no file differences reported by these tests."
$ Write SYS$OUTPUT "Output files matching master output files will be removed,"
$ Write SYS$OUTPUT "leaving any erroneous output files in the subdirectory"
$ Write SYS$OUTPUT "[.TEST.VARY] for manual examination.  If you built FTNCHEK with"
$ Write SYS$OUTPUT "non-standard compilation options, there may be some differences."
$ Write SYS$OUTPUT "================================================================"
$!
$!------------------------------------------------------------------------------
$ List: Call Noadwrite "checking listing output: "
$ Filelist = "AVERAGE,BACKSLASH,CHESTNUTS,COMCLASH,COMCMP,COMPLEX,COMTEST," + -
 "COMUSAGE,CORRECT,DIMS,DO_ENDDO,NAMELIST,NOBLANKS,STRINGS,UNIXINCLUDED," + -
 "DECTAB,QUAD,SEQUENCE,SUBSTR,AUTOARRAY,CYCLE-EXIT,RELOPS,EMBEDDED-SPACE," + -
 "ARG-ALIAS,COMMON-ALIAS,DEC-PARAMETER,IFFY,PURE-FUNCTION,PRIMES,LABELTEST," + -
 "CASE,BLOCKERRORS,T208F"
$! HELP omitted as VMS prints / instead of - switches and has different defaults
$! INCLUDE omitted. VMS output is different for include files
$! ALLKEYWORDS omitted. VMS output is different for include files
$ Define FTNCHEK_F77 YES
$ M = 0
$ List1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endlist1
$   Call Checklist 'File' 
$   M = M + 1
$   Goto List1
$ Endlist1: Call Noadwrite "done"
$! Repeat for expert mode
$ Expert: Call Noadwrite "checking expert-mode output: "
$ M = 0
$ Expert1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endexpert1
$   Call Checkexpert 'File' 
$   M = M + 1
$   Goto Expert1
$ Endexpert1: Call Noadwrite "done"
$ Deassign FTNCHEK_F77
$!
$!------------------------------------------------------------------------------
$ Freeform: Call Noadwrite "checking free-form output: "
$ Filelist = "MODEL,BLOCKCHECK"
$ Define FTNCHEK_F90 YES
$ Define FTNCHEK_SOURCE "free"
$ M = 0
$ Freeform1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endfreeform1
$   Call Checkfreeform 'File' 
$   M = M + 1
$   Goto Freeform1
$ Endfreeform1: Call Noadwrite "done"
$ Deassign FTNCHEK_F90
$ Deassign FTNCHEK_SOURCE
$!
$!------------------------------------------------------------------------------
$ Standard: Call Noadwrite "checking standard-compliance warnings: "
$ Filelist = "IOKEYWORDS,F95DELETED"
$ M = 0
$ Standard1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endstandard1
$   Call Check 'File' FC0 "-nof77 -nof90 -nof95"
$   Call Check 'File' FC1 "  -f77 -nof90 -nof95"
$   Call Check 'File' FC2 "-nof77   -f90 -nof95"
$   Call Check 'File' FC3 "-nof77 -nof90   -f95"
$   M = M + 1
$   Goto Standard1
$ Endstandard1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Size: Call Noadwrite "checking size and type mismatch: "
$ Filelist = "ASSIGN,WORDSIZE,MIXED-EQUIV"
$ M = 0
$ Size1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endsize1
$   Call Check 'File' FC1 "-notrun -noport -nof77 -lis"
$   Call Check 'File' FC2 "-notrun -noport   -f77"
$   Call Check 'File' FC3 "-notrun   -port -nof77"
$   Call Check 'File' FC4 "-notrun   -port   -f77"
$   Call Check 'File' FC5 "  -trun -noport -nof77"
$   Call Check 'File' FC6 "  -trun -noport   -f77"
$   Call Check 'File' FC7 "  -trun   -port -nof77"
$   Call Check 'File' FC8 "  -trun   -port   -f77"
$   Call Check 'File' FCA "  -trun   -port -nof77 -word=2"
$   Call Check 'File' FCB "  -trun   -port -nof77 -word=4"
$   Call Check 'File' FCC "  -trun   -port -nof77 -word=8"
$   Call Check 'File' FCD "  -trun -noport -nof77 -word=2"
$   Call Check 'File' FCE "  -trun -noport -nof77 -word=4"
$   Call Check 'File' FCF "  -trun -noport -nof77 -word=8"
$   M = M + 1
$   Goto Size1
$ Endsize1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Args: Call Noadwrite "checking -args setting: "
$ Filelist = "01,02,03,04,05,06,07,08,09,10,11,12,13,14,15"
$ M = 0
$ Args1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endargs1
$   N = 0
$   Args2: Call Check "ARGS''File'" "FC''N'" "-args=''N'" 
$     N = N + 1
$     If N .LT. 4 Then Goto Args2
$   M = M + 1
$   Goto Args1
$ Endargs1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Array: Call Noadwrite "checking -array setting: "
$ Filelist = "ARRAYCLASH"
$ M = 0
$ Array1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endarray1
$   N = 0
$   Array2: Call Check 'File' "FC''N'" "-array=''N' -port"
$     N = N + 1
$     If N .LT. 4 Then Goto Array2
$   M = M + 1
$   Goto Array1
$ Endarray1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Common: Call Noadwrite "checking -common setting: "
$ Filelist = "COMCMP,COMUSAGE"
$ M = 0
$ Common1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endcommon1
$   N = 0
$   Common2: Call Check 'File' "FC''N'" "-common=''N' -nof77"
$     N = N + 1
$     If N .LT. 4 Then Goto Common2
$   M = M + 1
$   Goto Common1
$ Endcommon1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Intrinsic: Call Noadwrite "checking -intrinsic setting: "
$ Filelist = "INTRINSTEST"
$ M = 0
$ Intrinsic1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endintrinsic1
$   Call Check 'File' FC0 "-intrinsic=none"
$   Call Check 'File' FC1 "-intrinsic=none,extra"
$   Call Check 'File' FC2 "-intrinsic=none,unix"
$   Call Check 'File' FC3 "-intrinsic=none,vms"
$   Call Check 'File' FC4 "-intrinsic=no-rand-no-arg,no-vms,unix"
$   Call Check 'File' FC5 "-intrinsic=no-rand-one-arg,no-vms,unix"
$   Call Check 'File' FC6 "-intrinsic=no-iargc-no-arg,no-vms,unix"
$   Call Check 'File' FC7 "-intrinsic=no-iargc-one-arg,no-vms,unix"
$   Call Check 'File' FC8 "-intrinsic=all"
$   M = M + 1
$   Goto Intrinsic1
$ Endintrinsic1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Output: Call Noadwrite "checking -output setting: "
$ Call Check AVERAGE FC2 "-symt -list -port -out=average.out"
$ Rename AVERAGE.OUT [.VARY]
$ Call Compare AVERAGE.OUT ".f"
$ Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Usage: Call Noadwrite "checking -usage setting: "
$ Filelist = "USAGE"
$ M = 0
$ Usage1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endusage1
$   Call Check 'File' FC0 "-usage=none"
$   Call Check 'File' FC1 "-usage=none,com-var-uninitialized,var-uninitialized"
$   Call Check 'File' FC2 "-usage=none,com-block-unused,com-var-set-unused,com-var-unused,ext-unused,var-set-unused,var-unused"
$   Call Check 'File' FC3 "-usage=all"
$   M = M + 1
$   Goto Usage1
$ Endusage1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Project: Call Noadwrite "checking project files: "
$ Rename CORRECT.F CORREC.;9
$ Call Doftnchek  "-project correc.;9" "[.VARY]CORRECT.FC1"
$ Rename CORREC.;9 CORRECT.F
$ Call Doedit [.VARY]CORRECT.FC1 correc.prj correct.prj
$ Call Doftnchek "CORREC.PRJ;1" "[.VARY]CORRECT.PCL"
$ Rename CORREC.PRJ [.VARY]CORRECT.PRJ
$ Call Doedit [.VARY]CORRECT.PCL correc.prj;1 correct.prj
$ Call Compare CORRECT.FC1 ".f"
$ Call Compare CORRECT.PRJ ".f"
$ Call Compare CORRECT.PCL ".f"
$ Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Rc: Call Noadwrite "checking reading rc file: "
$ Open/Write TMP FTNCHEK.INI
$ Write TMP "f77=all"
$ Write TMP "symtab"
$ Close TMP
$ Rename DO_ENDDO.F DO_ENDD.;9
$ Call Doftnchek "-f77=no-long-names,no-name-underscore do_endd.;9" [.VARY]RC.FCL
$ Rename DO_ENDD.;9 DO_ENDDO.F
$ Call Doedit [.VARY]RC.FCL .;9 o.f
$ Call Compare RC.FCL ".f"
$ Delete/Noconfirm FTNCHEK.INI;
$ Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Wildcard: Call Noadwrite "check interpreting wildcards: "
$ Call Doftnchek  "-port=mixed* -port=help" "[.VARY]WILDCARD.FC1"
$ Call Doedit [.VARY]WILDCARD.FC1 "/port"  "-port"
$ Call Compare WILDCARD.FC1 ".f"
$ Call Doftnchek  "-usage=no-*var* -usage=help" "[.VARY]WILDCARD.FC2"
$ Call Doedit [.VARY]WILDCARD.FC2 "/usage"  "-usage"
$ Call Compare WILDCARD.FC2 ".f"
$ Call Doftnchek  "-usage=no-var* -usage=help" "[.VARY]WILDCARD.FC3"
$ Call Doedit [.VARY]WILDCARD.FC3 "/usage"  "-usage"
$ Call Compare WILDCARD.FC3 ".f"
$ Call Doftnchek  "-f77=*array*,format* -f77=help" "[.VARY]WILDCARD.FC4"
$ Call Doedit [.VARY]WILDCARD.FC4 "/f77"  "-f77"
$ Call Compare WILDCARD.FC4 ".f"
$ Call Doftnchek  "-pretty=*ugly* -pretty=help" "[.VARY]WILDCARD.FC5"
$ Call Doedit [.VARY]WILDCARD.FC5 "/pretty"  "-pretty"
$ Call Compare WILDCARD.FC5 ".f"
$ Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Makedcls: Call Noadwrite "checking -makedcls setting: "
$ Filelist = "BUILTIN,DCLTEST,DIMS,NAMELIST,PARAMS,T208A,T208B,T208C,T208D," + -
  "T208E,T208F,T208G,T208H,T208I,T208J,T208K,T208L,T208M,T208N,T208O,T208P," + -
  "T208Q,T208R,T208S,T208T,T208U,T208V,T208W,T208X"
$ M = 0
$ Makedcls1: File = F$element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endmakedcls1
$   Tmpfil = F$Extract ( 0, F$Length(File)-1, File )
$   Rename 'File'.F 'Tmpfil'.;9
$   N = 0
$   P = 1
$   Makedcls2: Call Doftnchek "-nocheck -makedcls=''P' ''Tmpfil'.;9" NLA0:
$     If F$Search("''Tmpfil'.DCL") .NES. ""
$      Then
$        Rename 'Tmpfil'.DCL [.VARY]'File'.DC'N'
$        Open/Read TMP '[.VARY]'File'.DC'N'
$        Read/End_Of_File=Dclsempty TMP Line
$        Close TMP
$        Call Compare 'File'.DC'N' ".f"
$        Goto Makedclsnext
$        Dclsempty:  WRITE SYS$OUTPUT "Empty declarations file "+ F$Search("[.VARY]''Cffil'") + " should have been deleted"
$     Endif
$     Makedclsnext: P = P + P
$     N = N + 1
$     If N .LT. 10 Then Goto Makedcls2
$   Rename 'Tmpfil'.;9 'File'.F
$   M = M + 1
$   Goto Makedcls1
$ Endmakedcls1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$ Vcg: Call Noadwrite "checking -vcg setting: "
$ filelist = "COMCMP,COMUSAGE"
$ M = 0
$ Vcg1: File = F$Element(M,Comma,Filelist)
$   If File .EQS. Comma Then Goto Endvcg1
$   Tmpfil = F$Extract ( 0, F$Length(File)-1, File )
$   Rename 'File'.f 'Tmpfil'.;9
$   Call Doftnchek "-vcg  ''Tmpfil'.;9" NLA0:
$   Rename 'Tmpfil'.;9 'File'.f
$   Rename 'Tmpfil'.VCG [.VARY]'File'.VCG
$   Call Compare 'File'.VCG ".f"
$   M = M + 1
$   Goto Vcg1
$ Endvcg1: Call Noadwrite "done"
$!
$!------------------------------------------------------------------------------
$! Check for GAWK - needed for dcl2inc processing
$ Dcl2inc: Filelist = "T208X"
$ Assign F1.TMP SYS$OUTPUT
$ Show Symbol GAWK
$ Deassign SYS$OUTPUT
$ Open/Read TMP F1.TMP
$ Read TMP Line
$ Close TMP
$ Delete/Noconfirm F1.TMP;
$ If Line  .NES. "%DCL-W-UNDSYM, undefined symbol - check validity and spelling"
$  Then
$   Call Noadwrite "checking dcl2inc processing: "
$   M = 0
$   Dcl2inc1: File = F$Element(M,Comma,Filelist)
$     If File .EQS. Comma Then Goto Enddcl2inc1
$     Tmpfil = F$Extract ( 0, F$Length(File)-1, File ) 
$     Rename 'File'.f 'Tmpfil'.;9
$     Call Doftnchek "-nocheck -makedcl=1 ''Tmpfil'.;9" NLA0:
$     Rename 'Tmpfil'.;9 'File'.f
$     Assign/User 'File'.ERR SYS$ERROR
$     Assign/User [.VARY]'File'.MAK SYS$OUTPUT
$     Gawk -f [-]DCL2INC.AWK 'Tmpfil'.DCL
$     Deassign SYS$OUTPUT
$     Rename 'Tmpfil'.DCL [.VARY]'File'.DCL
$     Call Compare 'File'.DCL ".f"
$     Rename 'Tmpfil'.DCN [.VARY]'File'.DCN
$     Call Compare 'File'.DCN ".f"
$     Call Doedit [.VARY]'File'.MAK 'Tmpfil'. 'File'.
$     Call Compare 'File'.MAK ".f"
$     Rename 'File'.ERR [.VARY]
$     Call Doedit [.VARY]'File'.ERR 'Tmpfil'. 'File'.
$     Call Compare 'File'.ERR ".f"
$     Dcl2inc2: File = F$Search("*.INC")
$       If File .EQS. "" Then Goto Enddcl2inc2
$       Cffil = F$Parse(File,,,"NAME") + ".INC"
$       Rename 'Cffil' [.VARY]
$       Call Compare 'Cffil' ".f"
$       Goto Dcl2inc2
$   Enddcl2inc2: M = M + 1
$   Goto Dcl2inc1
$ Enddcl2inc1: Call Noadwrite "done"
$ Else
$  Write SYS$OUTPUT "WARNING ===> dcl2inc processing not checked <===
$  Write SYS$OUTPUT "Unable to locate GAWK on this system"
$ Endif
$!
$ Finish:
$ Set Default 'Startdir'
$ If Setverify Then Set Verify
$ EXIT

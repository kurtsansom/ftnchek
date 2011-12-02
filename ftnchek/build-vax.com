$! BUILD-VAX.COM
$!    Creates Ftnchek from source code for VAX/VMS systems.
$!    This script uses CC.COM and LINK.COM to check whether target
$!    is up to date, giving effect of a crude make utility.
$!    The CC.COM script is used in place of CC command so that if something
$!    is changed, BUILD can be re-run and only re-compiles what changed.
$!    Invocation: @CC PROG DEPENDENCIES [/CFLAGS...]
$!
$!    This version of the build script is for older VAX systems.  For
$!    more recent Alpha systems, use BUILD.COM instead.
$!
$ On Control_Y Then GoTo The_Exit
$ On Error Then GoTo The_Exit
$! DISABLED at ftnchek 2.8: On Warning Then GoTo The_Exit
$!
$!	First, create the right environment for compilation.
$ IF F$SEARCH("vaxc.opt") .EQS. ""
$ THEN!	Create option file for shareable image: makes executable smaller
$ COPY SYS$INPUT vaxc.opt
sys$library:vaxcrtl/share
$ ENDIF
$ DEFINE lnk$library sys$library:vaxcrtl	! simplest runtime library
$!
$!   Now compile everything and link it all.
$!   If you have xmalloc/xrealloc, remove the /DEFINE from shell_mung
$!   line and add them to the list of objs to link.
$!   If your compiler requires the /VAXC or other option, add it to CC.COM
$!   at line 44.
$!
$ IF F$SEARCH("shell_mung.c") .EQS. ""
$ THEN!	If shell_mung not found, work around it and tell the user
$ WRITE SYS$OUTPUT "SHELL_MUNG.C not found: wildcard expansion will not be"
$ WRITE SYS$OUTPUT "done.  SHELL_MUNG.C is distributed separately."
$ @CC ftnchek config.h,ftnchek.h,intrins.h,options.h
$ ELSE
$ @CC ftnchek config.h,ftnchek.h,intrins.h,options.h /DEFINE=(USE_SHELL_MUNG)
$ @CC shell_mung "" /DEFINE=("xmalloc=malloc","xrealloc=realloc")
$ ENDIF
$ @CC advance config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h,advance.h
$ @CC argcheck config.h,ftnchek.h,pgsymtab.h,symtab.h
$ @CC calltree config.h,ftnchek.h,pgsymtab.h,symtab.h
$ @CC comcheck config.h,ftnchek.h,pgsymtab.h,symtab.h
$ @CC exprtype config.h,ftnchek.h,symtab.h,tokdefs.h
$ @CC forlex config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h,advance.h
$ @CC fortran config.h,ftnchek.h,symtab.h,fortran.c
$ @CC include	config.h,ftnchek.h,symtab.h,forlex.h,advance.h
$ @CC intake config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h,advance.h
$ @CC intrins config.h,ftnchek.h,intrins.h,symtab.h
$ @CC iokeywds config.h,ftnchek.h,symtab.h,tokdefs.h,iokeywds.h
$ @CC keywords config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h
$ @CC labels config.h,ftnchek.h,symtab.h
$ @CC loccheck config.h,ftnchek.h,loccheck.h,plsymtab.h,symtab.h
$ @CC makedcls config.h,ftnchek.h,plsymtab.h,symtab.h
$ @CC makehtml config.h,ftnchek.h,symtab.h,plsymtab.h,tokdefs.h
$ @CC message config.h,ftnchek.h
$ @CC options config.h,ftnchek.h,options.h
$ @CC pgsymtab config.h,ftnchek.h,pgsymtab.h,symtab.h
$ @CC plsymtab config.h,ftnchek.h,plsymtab.h,symtab.h
$ @CC prlists config.h,ftnchek.h,symtab.h,symutils.h
$ @CC prlocsym config.h,ftnchek.h,loccheck.h,plsymtab.h,symtab.h
$ @CC project config.h,ftnchek.h,symtab.h
$ @CC symspace config.h,ftnchek.h,symtab.h,symspace.h,symutils.h
$ @CC symtab config.h,ftnchek.h,iokeywds.h,intrins.h,symtab.h,tokdefs.h
$ @CC symutils config.h,ftnchek.h,symtab.h,symutils.h
$ @CC utils config.h,utils.h
$ IF F$SEARCH("shell_mung.obj") .EQS. ""
$ THEN
$ @LINK ftnchek,advance,argcheck,calltree,comcheck,exprtype,forlex,fortran,include,-
intake,intrins,iokeywds,keywords,labels,loccheck,makedcls,makehtml,message,options,-
pgsymtab,plsymtab,prlists,prlocsym,project,symspace,symtab,symutils,utils,-
vaxc/opt
$ ELSE
$ @LINK ftnchek,advance,argcheck,calltree,comcheck,exprtype,forlex,fortran,include,-
intake,intrins,iokeywds,keywords,labels,loccheck,makedcls,makehtml,message,options,-
pgsymtab,plsymtab,prlists,prlocsym,project,symspace,symtab,symutils,utils,-
shell_mung,vaxc/opt
$ ENDIF
$ WRITE SYS$OUTPUT "Ftnchek created"
$ WRITE SYS$OUTPUT "To make it runnable as a command"
$ WRITE SYS$OUTPUT "say   $ FTNCHEK :== $diskname:[pathname]FTNCHEK"
$ On Control_Y Then GoTo Help_Exit
$ On Warning Then GoTo Help_Exit
$! Create the help library.
$ LIBR/CREATE/HELP FTNCHEK.HLB FTNCHEK.HLP
$ WRITE SYS$OUTPUT "Help library created -- to access it via HELP"
$ WRITE SYS$OUTPUT "say   $ DEFINE HLP$LIBRARY diskname:[pathname]FTNCHEK.HLB"
$ EXIT
$ Help_Exit:
$ Set NoVerify
$ WRITE SYS$OUTPUT "Error- help library not created"
$ EXIT
$ The_Exit:
$ Set NoVerify
$ WRITE SYS$OUTPUT "Error- BUILD failed."

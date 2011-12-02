$! BUILD.COM
$!    Creates Ftnchek from source code for Alpha VMS systems
$!
$ On Control_Y Then GoTo The_Exit
$ On Warning Then GoTo The_Exit
$!
$!
$!   Now compile everything and link it all.
$!   If you have xmalloc/xrealloc, remove the /DEFINE from shell_mung
$!   line and add them to the list of objs to link.
$!   The CC.COM script is used in place of CC command so that if something
$!   is changed, BUILD can be re-run and only re-compiles what changed.
$!   Invocation: @CC PROG DEPENDENCIES [/CFLAGS...]
$ IF F$SEARCH("shell_mung.c") .EQS. ""
$ THEN!	If shell_mung not found, work around it and tell the user
$ WRITE SYS$OUTPUT "SHELL_MUNG.C not found: wildcard expansion will not be"
$ WRITE SYS$OUTPUT "done.  SHELL_MUNG.C is distributed separately."
$ @CC ftnchek config.h,ftnchek.h,intrins.h,options.h  /STANDARD=RELAXED_ANSI89
$ ELSE
$ @CC ftnchek config.h,ftnchek.h,intrins.h,options.h  /STANDARD=RELAXED_ANSI89/DEFINE=(USE_SHELL_MUNG)
$ @CC shell_mung "" /STANDARD=VAXC/DEFINE=("xmalloc=malloc","xrealloc=realloc")
$ ENDIF
$ @CC advance config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h,advance.h  /STANDARD=RELAXED_ANSI89
$ @CC argcheck config.h,ftnchek.h,pgsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC calltree config.h,ftnchek.h,pgsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC comcheck config.h,ftnchek.h,pgsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC exprtype config.h,ftnchek.h,symtab.h,tokdefs.h  /STANDARD=RELAXED_ANSI89
$ @CC forlex config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h,advance.h  /STANDARD=RELAXED_ANSI89
$ @CC fortran config.h,ftnchek.h,symtab.h,fortran.c  /STANDARD=RELAXED_ANSI89
$ @CC include	config.h,ftnchek.h,symtab.h,forlex.h,advance.h  /STANDARD=RELAXED_ANSI89
$ @CC intake config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h,advance.h  /STANDARD=RELAXED_ANSI89
$ @CC intrins config.h,ftnchek.h,intrins.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC iokeywds config.h,ftnchek.h,symtab.h,tokdefs.h,iokeywds.h  /STANDARD=RELAXED_ANSI89
$ @CC keywords config.h,ftnchek.h,symtab.h,tokdefs.h,forlex.h  /STANDARD=RELAXED_ANSI89
$ @CC labels config.h,ftnchek.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC loccheck config.h,ftnchek.h,loccheck.h,plsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC makedcls config.h,ftnchek.h,plsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC makehtml config.h,ftnchek.h,symtab.h,plsymtab.h,tokdefs.h  /STANDARD=RELAXED_ANSI89
$ @CC message config.h,ftnchek.h  /STANDARD=RELAXED_ANSI89
$ @CC options config.h,ftnchek.h,options.h  /STANDARD=RELAXED_ANSI89
$ @CC pgsymtab config.h,ftnchek.h,pgsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC plsymtab config.h,ftnchek.h,plsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC prlists config.h,ftnchek.h,symtab.h,symutils.h  /STANDARD=RELAXED_ANSI89
$ @CC prlocsym config.h,ftnchek.h,loccheck.h,plsymtab.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC project config.h,ftnchek.h,symtab.h  /STANDARD=RELAXED_ANSI89
$ @CC symspace config.h,ftnchek.h,symtab.h,symspace.h,symutils.h  /STANDARD=RELAXED_ANSI89
$ @CC symtab config.h,ftnchek.h,iokeywds.h,intrins.h,symtab.h,tokdefs.h  /STANDARD=RELAXED_ANSI89
$ @CC symutils config.h,ftnchek.h,symtab.h,symutils.h  /STANDARD=RELAXED_ANSI89
$ @CC utils config.h,utils.h  /STANDARD=RELAXED_ANSI89
$ IF F$SEARCH("shell_mung.obj") .EQS. ""
$ THEN
$ @LINK ftnchek,advance,argcheck,calltree,comcheck,exprtype,forlex,fortran,include,-
intake,intrins,iokeywds,keywords,labels,loccheck,makedcls,makehtml,message,options,-
pgsymtab,plsymtab,prlists,prlocsym,project,symspace,symtab,symutils,utils
$ ELSE
$ @LINK ftnchek,advance,argcheck,calltree,comcheck,exprtype,forlex,fortran,include,-
intake,intrins,iokeywds,keywords,labels,loccheck,makedcls,makehtml,message,options,-
pgsymtab,plsymtab,prlists,prlocsym,project,symspace,symtab,symutils,utils,-
shell_mung
$ ENDIF
$ Set NoVerify
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
$ EXIT

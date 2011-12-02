$! name: CC.COM
$! author: R. Moniot
$! date: 14-Apr-87
$! purpose: bring C object module up to date with its source.
$! usage:  @CC PROG DEPENDENCIES [/CFLAGS...]
$!		Where PROG is source filename (OMIT extension .C), and
$!		DEPENDENCIES is comma-separated list (possibly null) of
$!		files (WITH their extensions) on which PROG depends, other
$!		than the source file.  Optional compiler flags must come
$!		last.
$!
$!    Look for .C file and .OBJ file, and do the right things.
$!
$ On Control_Y Then Exit 2
$ On Warning Then Exit 2
$ dependency = p1 + ".C"
$ if f$search(dependency) .eqs. "" then goto NoSuchFile
$ if f$search(p1 + ".OBJ") .eqs. "" then goto Compile
$!
$!	If both exist, look at creation dates to see if compilation needed
$!
$ srcdate = f$cvtime(f$file_attributes(p1+".C","cdt"))
$ objdate = f$cvtime(f$file_attributes(p1+".OBJ","cdt")) 
$ if objdate .lts. srcdate then goto Compile
$!
$!  check dependencies.  rest holds remainder of string.
$	rest = p2
$ Loop:
$	len = f$length(rest)
$	pos = f$locate(",",rest)
$	dependency = f$extract(0,pos,rest)
$	if dependency .eqs. "" then goto Exit
$	if f$search(dependency) .eqs. "" then goto NoSuchFile
$	depdate = f$cvtime(f$file_attributes(dependency,"cdt"))
$	if objdate .lts. depdate then goto Compile
$	if pos .eq. len then goto Exit
$	rest = f$extract(pos+1,len-pos-1,rest)
$ goto Loop
$!
$!	Object does not exist or is older than source: recompile
$!
$ Compile:
$	write sys$output "CC " + p1+" "+p3+p4+p5+p6+p7+p8
$	cc 'p1' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8'
$	exit
$!
$!	Source is older than object: all is OK
$!
$ Exit:
$	write sys$output p1 + ".OBJ is up to date."
$	exit
$!
$ NoSuchFile:
$	write sys$output dependency + " does not exist"
$! exit, and return control to command level
$	stop

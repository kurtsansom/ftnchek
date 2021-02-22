$! name: LINK.COM
$! author: R. Moniot
$! date: 14-Apr-87
$! purpose: bring C image up to date with its objects
$! usage:  @LINK OBJLIST
$!		Where OBJLIST is a comma-separated list of object files,
$!		OMITTING the extension .OBJ.
$!
$! Pick main module name from front of the list.  If .EXE file does not exist,
$! go to link.
$!
$ On Control_Y Then Exit 2
$ On Warning Then Exit 2
$ len = f$length(p1)
$ pos = f$locate(",",p1)
$ main = f$extract(0,pos,p1)
$ if f$search(main + ".EXE") .eqs. "" then goto Link
$!
$ exedate = f$cvtime(f$file_attributes(main + ".EXE","cdt")) 
$!
$!  Check against object module dates
$!
$ rest = p1
$ Loop:
$	len = f$length(rest)
$	pos = f$locate(",",rest)
$	module = f$extract(0,pos,rest)
$	if module .eqs. "" then goto Exit
$	if f$search(module + ".OBJ") .eqs. "" then goto NoSuchFile
$	objdate = f$cvtime(f$file_attributes(module + ".OBJ","cdt"))
$	if exedate .lts. objdate then goto Link
$	rest = f$extract(pos+1,len-pos-1,rest)
$ goto Loop
$!
$!	Image does not exist or is older than some objects: link
$!
$ Link:
$	write sys$output "LINK " + p1
$	on warning then exit
$	link 'p1'
$	exit
$!
$!	object is older than executable: all is OK
$!
$ Exit:
$	write sys$output main + ".EXE is up to date."
$	exit
$!
$ NoSuchFile:
$	write sys$output module + ".OBJ does not exist"
$! exit, and return control to command level
$	stop

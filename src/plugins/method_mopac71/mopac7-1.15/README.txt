Modified 2016 by TGAY:

* Converted subroutines containing STOP statements into logical functions, so MOPAC7 can truly be called as a function from a program without unnecessarily terminating the main program.
*** WORK IN PROGRESS ***

**********************************************************************

Modified 20 Oct 2015 by AKS to compile under Gfortran to produce a standalone executable:-

1) Removed all calls to C routines. Only one was actually used and could easily be replaced with calls to Gfortran intrinsics.
2) Attempted to correct some compile-time warnings - the full impact of these is unknown, but most likely there are other coding mismatches (e.g. complex arguments being posted to a subroutine expecting real arguments, etc.) that may go unnoticed due to the way the program is structured.
3) Removed call to libmopac7 - this was giving grief on OS X Yosemite and later due to bugs in the linker not connecting the common blocks in the main file with those in the library.

**********************************************************************

The objective of this work is to make available both a stand-alone MOPAC7
application and a library containing MOPAC7 code, using the f2c tool.

The original fortran source is included, and some careful modifications are
added there. So far they are well-known bug fixes and some extensions, where
original mopac subroutines copied, renamed and modified.


The list of modifications follows:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

2001-04-18 : We will link libmopac7.a into a C++ program at Linux/UNIX.
Translate MOPAC7 into C using f2c package (version 19991025-1). Build it,
and it should work... Copy mopac.f -> mopaclib.f, and for simplicity add
some comments. We will convert the original program into two subroutines,
which will either start or stop the MOPAC7. The idea is roughly following:

	start-begin
		call GETDAT
		open unit 6 as usual...
		call READMO
		(skip the NATOMS check)...
		(skip the keyword "AUTHOR")...
		call GEOUT (by default!!!)
		call MOLDAT
		(skip the SOLVATION stuff)...
		(skip the EXTERNAL stuff)...
	start-end (ALLOW NO JUMPS OUTSIDE THIS BLOCK!!!)
	
	stop-begin
		(start from the label "40")
		(skip the WRITMO stuff)...
		(skip the POLAR stuff)...
		(skip the ESP stuff)...
		write times to unit 6
	stop-end (ALLOW NO JUMPS OUTSIDE THIS BLOCK!!!)

2001-08-17 : Added a modification to calculate electrostatic potential (ESP)
plots. The original MOPAC contains a keyword "ESP" to fit atomic charges using
ESP. However, it did not work. Found a message at CCL where a patch to MOPAC
was included; other files than esp.f were already modified according to the
message. At esp.f, the /STO6G/ common block sizes were different from those
in the corresponding block at setupg.f; this was fixed. Now the plots are
calculated by setting the point data and then calling elesp_() which calculates
ESP. In elesp_() code, atom positions are first copied by calling a modified
pdgrid_(). I haven't yet checked what the unit of ESP is in mopac, but it seems
to be hartree; the plots look really nice when using the hartree->kJ/mol
conversion factor. The plot calculation process is a bit slow however; perhaps
some unnecessary results are also calculated? Those could be commented out...

2001-08-18 : The old ELESP subroutine is now divided in two parts; first will
initialize the plotting code, and the second will calculate the value. Speed
improvement is significant this way.

2001-09-03 : An idea about how to get rid of global variables: try to combine
all separate common blocks into a single big block. Then that could perhaps be
converted into a struct, and a pointer could be used to access it's elements?
Could the addition of pointer code be done as a search/replace operation???

2001-09-26 : Changes made to mopac.f and minimopac.f; we don't open unit 6
as a file anymore, and this will direct the output to console as text. This
also affected iter.f because unit 6 was also opened there (no idea why).
Also esp.f was slightly changed; unnecessary outputs/dumps were removed.

2001-09-27 : At iter.f, the STOP command at line 518 was removed, and failed
SCF convergence now doesn't close the whole program (this has been a problem
in MOPAC geometry optimizations). Instead of STOP, GOTO 380 is called like
in a case few lines above where the job is continued. THIS CHANGE MIGHT CAUSE
SOME PROBLEMS, BECAUSE WE WILL ACCEPT INACCURATE ENERGIES/FORCES AND WE WILL
FEED THEM TO GEOMETRY OPTIMIZATION AND OTHER ALGORITHMS!!! However, there
seems to be no negative side-effects in practice.

2002-01-31 : It turned out that I had modified etime.c file earlier by adding
#include <time.h> there (thanks Radek! :)  The fortran/etime.c file is still
the original unmodified version.

2003-05-06 : A new build system added, that allows one to build either the app
or the lib. There is a new source file m7MAIN.c that contains both mopac.c and
mopaclib.c sources, separated by some preprocessor macros that select which one
gets compiled.

2003-12-19 : Added the configure scripts etc to build both the app and the lib.

2004-08-13 : Added improved configuration scripts (Jean Brefort). The old file
m7MAIN.c is divided into mopac7app.c/mopac7lib.c files.

2004-11-09 : Some fixes and cleanups added for an initial release.

2005-10-11 : Compilers newer than gcc-3.2 had problems compiling cdiag.c file,
where COMPLEX/REAL type tables were mismatching in subroutine EC08C and ME08B
calls. The subroutines were moved to the top of cdiag.f source file, which
helped resolve the problems. There were no changes in fortran code, just a
rearrangement.


How to update the C source files from fortran source:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It seems to be a good strategy to implement most changes and extensions in
fortran side, and then convert that to C. Some modifications in fortran side
will cause *big* changes in C side, for example changes in SIZES file or any
changes in fortran common blocks.

However, some changes are better to do in C side. For example, both MOPAC and
MPQC seems to use some BLAS routines, which have similar names, and therefore
you can't link them together. In the following, this kind of changes are
listed. If you update the C source from fortran, I guess you have to maintain
these changes manually...

	ef.c : functions "daxpy", "dscal" and "dswap" conflicted with similar
	ones in MPQC; seem to be BLAS routines? Renamed them with mM prefix...

	etime.c : #include <time.h> has been added to this file.

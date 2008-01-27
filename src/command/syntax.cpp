/*
	*** Command syntax
	*** src/command/scriptsyntax.cpp
	Copyright T. Youngs 2007

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "command/commandlist.h"
#include "base/sysfunc.h"
#include <string.h>

// Script command data
const char *CA_syntax[CA_NITEMS] = {

	// Variables
	"character <variables>",
	"integer <variables>",
	"double <variables>",
	"atom <variables>",
	"pattern <variables>",
	"model <variables>",
	"bond <variables>",
	"angle <variables>",
	"torsion <variables>",
	"atomtype <variables>",

	// Root node
	"_ROOTNODE_",

	// Analysis
	"finalise",
	"frameanalyse",
	"modelanalyse",
	"pdens <name> <site1> <site2> <griddelta> <nsteps> <filename>",
	"printjobs",
	"rdf <name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>",
	"savequantities",
	"trajanalyse <startframe> <frameskip> [nframes]",

	// Atoms
	"addatom <element>",
	"addchain <element> [bondtype]",
	"endchain",
	"setcharge <q> [id]",
	"setcoords <x> <y> <z> [id]",
	"setelement <element> [id]",
	"setforces <fx> <fy> <fz> [id]",
	"setfx <fx> [id]",
	"setfy <fy> [id]",
	"setfz <fz> [id]",
	"setid <id> [id]",
	"setrx <rx> [id]",
	"setry <ry> [id]",
	"setrz <rz> [id]",
	"setvelocities <vx> <vy> <vz> [id]",
	"setvx <vx> [id]",
	"setvy <vy> [id]",
	"setvz <vz> [id]",

	// Bonding
	"addbond <id1> <id2> [bondtype]",
	"augment",
	"bondtolerance <tolerance>",
	"bondpatterns",
	"bondselection",
	"clearbonds",
	"rebond",

	// Build commands
	"addhydrogen [atom|id]",
	"delete",
	"locate <x> <y> <z>",
	"move <dx> <dy> <dz>",
	"rotx <angle>",
	"roty <angle>",
	"rotz <angle>",
	"transmute <element>",

	// Cell commands
	"fold",
	"fractoreal",
	"pack",
	"printcell",
	"replicatecell <neg.x> <neg.y> <neg.z> <pos.x> <pos.y> <pos.z>",
	"scalecell <x> <y> <z>",
	"setcell <a> <b> <c> <alpha> <beta> <gamma>",
	"setcellaxes <ax> <ay> <az> <bx> <by> <bz> <cx> <cy> <cz>",
	"setspacegroup <spgrp>",

	// Charge commands
	"chargeatom <id> <q>",
	"chargeff",
	"chargefrommodel",
	"chargepatom <pattern> <id> <q>",
	"chargeselection <q>",
	"chargetype <type> <q>",
	"clearcharges",

	// Disordered Builder Commands
	"addcomponent <name> <model> <nmols>",
	"disorder <nsteps>",
	"printcomponents",
	"setcentre <name> <x> <y> <z>",
	"setgeometry <name> <x> <y> <z> [l]",
	"setoverlap <name> yes|no",
	"setshape <name> <shape>",
	"vdwscale <scale>",

	// Energy commands
	"frameenergy",
	"modelenergy",
	"printelec",
	"printewald",
	"printinter",
	"printintra",
	"printenergy",
	"printsummary",
	"printvdw",

	// Expression commands
	"createexpression",
	"ecut <cutoff>",
	"elec <none|coulomb|ewald|ewaldauto> [ [precision] | [alpha] [kx] [ky] [kz] ]",
	"intra on|off",
	"printexpression",
	"vcut <cutoff>",
	"vdw on|off",

	// Field commands
	"savefield <filename>",
	"savefield2 <format> <filename>",

	// Flow control
	"else",
	"elseif,rxe",
	"end",
	"for <variable> [start] [end]",
	"_GOTO_",
	"_GOTONONIF_",
	"if,rxe",
	"quit",
	"_TERMINATE_",

	// Force commands
	"frameforces",
	"modelforces",
	"printforces",

	// Forcefield commands
	"ffmodel",
	"ffpattern",
	"ffpatternid <patternid>",
	"loadff <filename> [name]",
	"selectff <name>",
	"typemodel",
	"typetest <ffid> <atomid>",

	// Grid commands
	"addgridpoint <ix> <iy> <iz> <value>",
	"addnextgridpoint <value>",
	"finalisegrid",
	"newgrid <title>",
	"setgrid <ax> <ay> <az> <bx> <by> <bz> <cx> <cy> <cz>",
	"setgridcubic <l>",
	"setgridorigin <x> <y> <z>",
	"setgridortho <a> <b> <c>",
	"setgridsize <nx> <ny> <nz>",

	//"image 

	// Labeling commands
	"clearlabels",
	"addlabel <label>",
	"removelabel <label>",

	// MC commands
	"mcaccept <movetype> <energy>",
	"mcallow <movetype> yes|no",
	"mcmaxstep <movetype> <step>",
	"mcntrials <movetype> <ntrials>",
	"printmc",

	// Messaging
	"error <message>",
	"print <message>",
	"warn <message>",

	// Minimisation commands
	"cgminimise",
	"converge <energy> <forces>",
	"linetol <tolerance>",
	"mcminimise <maxsteps>",
	"sdminimise <maxsteps>",
	"simplexminimise",

	// Model commands
	"createatoms",
	"finalisemodel",
	"listmodels",
	"loadmodel <filename> [name]",
	"modeltemplate",
	"newmodel <name>",
	"printmodel",
	"savemodel <format> <filename>",
	"selectmodel <name>",
	"settitle <title>",

	// Pattern commands
	"addpattern <name> <nmols> <natoms>",
	"clearpatterns",
	"createpatterns",
	"printpatterns",
	"selectpattern <name>",

	// Preferences commands
	"atomdetail <n>",
	"bonddetail <n>",
	"colour <colour> <r> <g> <b>",
	"densityunits atomsperang|gpercm",
	"elementambient <element> <r> <g> <b>",
	"elementdiffuse <element> <r> <g> <b>",
	"elementradius <element> <radius>",
	"energyunits j|kj|cal|kcal|ha",
	"gl <option> on|off",
	"key ctrl|shift|alt <action>",
	"mouse left|middle|right|wheel <action>",
	"radius <style> <r>",
	"shininess <n>",
	"show <object> yes|no",
	"style <style>",

	// Read / Write Commands
	"addreadoption <option>",
	"find <string> <resultvar> [linevar]",
	"readchars <variable> <nchars>",
	"readdouble <variable>",
	"readint <variable>",
	"readline <variable>",
	"readnext <variable>",
	"readvar <variable> <format>",
	"removereadoption <option>",
	"rewind",
	"skipchars <nchars>",
	"skipline [nlines]",
	"writeline <string>",

	// Selection commands
	"selectall",
	"selectatom <id>",
	"selectelement <el>",
	"selectfftype <typename>",
	"invert",
	"selectnone",
	"selectoverlaps <tolerance>",
	"selecttype <element> <typedesc>",

	// Site commands
	"addsite <name> <pattern> [atomlist]",
	"printsites",
	"selectsite <name>",
	"setaxes <atomlist> <atomlist>",

	// Trajectory commands
	"firstframe",
	"lastframe",
	"loadtrajectory <filename>",
	"nextframe",
	"prevframe",

	// Transformation commands
	"centre <x> <y> <z>",
	"translate <dx> <dy> <dz>",
	"translateatom <dx> <dy> <dz>",
	"mirror <axis>",

	// Variable commands
	"dec <variable>",
	"eval,r=e",
	"inc <variable>",
	"let,rxe"

	};
const char *syntax_from_CA(command_action ca)
	{ return CA_syntax[ca]; }

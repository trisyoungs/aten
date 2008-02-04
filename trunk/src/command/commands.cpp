/*
	*** Command definitions
	*** src/command/commands.cpp
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
#include "classes/bundle.h"
#include "command/commands.h"
#include "base/sysfunc.h"
#include <string.h>

// Command action keywords
const char *CA_data[CA_NITEMS] = {

	// Variables
	"character",
	"integer",
	"double",
	"atom",
	"pattern",
	"model",
	"bond",
	"angle",
	"torsion",
	"atomtype",

	// Root node
	"_ROOTNODE_",
	"help,V",

	// Analysis commands
	"finalise",
	"frameanalyse",
	"modelanalyse",
	"pdens,VVVVVV",
	"printjobs",
	"rdf,VVVVVVV",
	"savequantities",
	"trajanalyse,VVv",

	// Atom commands
	"addatom,V",
	"addchain,Vv",
	"endchain",
	"setcharge,Vv",
	"setcoords,VVVv",
	"setelement,Vv",
	"setforces,VVVv",
	"setfx,Vv",
	"setfy,Vv",
	"setfz,Vv",
	"setid,Vv",
	"setrx,Vv",
	"setry,Vv",
	"setrz,Vv",
	"setvelocities,VVVv",
	"setvx,Vv",
	"setvy,Vv",
	"setvz,Vv",

	// Bonding commands
	"addbond,VVv",
	"addbondid,VVv",
	"augment",
	"bondtolerance,V",
	"bondpatterns",
	"bondselection",
	"clearbonds",
	"rebond",

	// Build commands
	"addhydrogen,v",
	"delete",
	"locate,VVV",
	"move,VVV",
	"rotx,V",
	"roty,V",
	"rotz,V",
	"transmute,V",

	// Cell commands
	"fold",
	"fractoreal",
	"pack",
	"printcell",
	"replicatecell,VVVVVV",
	"scalecell,VVV",
	"setcell,VVVVVV",
	"setcellaxes,VVVVVVVVV",
	"setspacegroup,V",

	// Charge commands
	"chargeff",
	"chargefrommodel",
	"chargepatom,VV",
	"chargeselection,V",
	"chargetype,VV",
	"clearcharges",

	// Disordered Builder Commands
	"addcomponent,VVV",
	"disorder,V",
	"printcomponents",
	"setcentre,VVVV",
	"setgeometry,VVVVv",
	"setoverlap,VV",
	"setshape,VV",
	"vdwscale,V",

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
	"ecut,V",
	"elec,Vvvvv",
	"intra,V",
	"printsetup",
	"vcut,V",
	"vdw,V",

	// Field commands
	"savefield,VV",

	// Flow control
	"else",
	"elseif,VSE",
	"end",
	"for,Vvv",
	"_GOTO_",
	"_GOTONONIF_",
	"if,VSE",
	"quit",
	"_TERMINATE_",

	// Force commands
	"frameforces",
	"modelforces",
	"printforces",

	// Forcefield commands
	"ffmodel",
	"ffpattern",
	"ffpatternid,V",
	"loadff,Vv",
	"selectff,V",
	"typemodel",
	"typetest,V",

	// Grid commands
	"addgridpoint,VVVV",
	"addnextgridpoint,V",
	"finalisegrid",
	"newgrid,V",
	"setgrid,VVVVVVVVV",
	"setgridcubic,V",
	"setgridorigin,VVV",
	"setgridortho,VVV",
	"setgridsize,VVV",

	// Image commands
	"savebitmap,VV",
	"savevector,VV",

	// Labeling commands
	"clearlabels",
	"addlabel,V",
	"removelabel,V",

	// MC commands
	"mcaccept,VV",
	"mcallow,VV",
	"mcmaxstep,VV",
	"mcntrials,VV",
	"printmc",

	// Messaging
	"error,G",
	"print,G",
	"warn,G",

	// Minimisation commands
	"cgminimise",
	"converge,VV",
	"linetol,V",
	"mcminimise,V",
	"sdminimise,V",
	"simplexminimise",

	// Model commands
	"createatoms",
	"finalisemodel",
	"listmodels",
	"loadmodel,Vv",
	"modeltemplate",
	"newmodel,V",
	"printmodel",
	"savemodel,VV",
	"selectmodel,V",
	"settitle,V",

	// Pattern commands
	"addpattern,VVV",
	"clearpatterns",
	"createpatterns",
	"printpatterns",
	"selectpattern,V",

	// Preferences commands
	"atomdetail,V",
	"bonddetail,V",
	"colour,VVVV",
	"densityunits,V",
	"elementambient,VVVV",
	"elementdiffuse,VVVV",
	"elementradius,VV",
	"energyunits,V",
	"gl,VV",
	"key,VV",
	"mouse,VV",
	"radius,VV",
	"shininess,V",
	"show,VV",
	"style,V",

	// Read / Write Commands
	"addreadoption,V",
	"find,VVv",
	"readchars,VV",
	"readdouble,V",
	"readint,V",
	"readline,F",
	"readnext,V",
	"readvar,VF",
	"removereadoption,V",
	"rewind",
	"skipchars,V",
	"skipline,v",
	"writeline,G",

	// Selection commands
	"selectall",
	"selectatom,V",
	"selectelement,V",
	"selectfftype,V",
	"invert",
	"selectnone",
	"selectoverlaps,V",
	"selecttype,VV",

	// Site commands
	"addsite,VVv",
	"printsites",
	"selectsite,V",
	"setaxes,VV",

	// Trajectory commands
	"firstframe",
	"lastframe",
	"loadtrajectory,V",
	"nextframe",
	"prevframe",

	// Transformation commands
	"centre,VVV",
	"translate,VVV",
	"translateatom,VVV",
	"mirror,V",

	// Variables
	"dec,V",
	"eval,V=E",
	"inc,V",
	"let,V=V",

	// View
	"resetview",
	"rotateview,VV",
	"translateview,VVV",
	"viewalong,VVV",
	"viewalongcell,VVV",
	"zoomview,V",
	"zrotateview <dr>"

	};

command_action CA_from_text(const char* s)
	{ return (command_action) enum_search_data("", CA_NITEMS, CA_data, s); }
const char *text_from_CA(command_action ca)
	{ return get_before_comma(CA_data[ca]); }
const char *vars_from_CA(command_action ca)
	{ return get_after_comma(CA_data[ca]); }

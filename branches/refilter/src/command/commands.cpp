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
	"bond",
	"pattern",
	"model",
	"patbound",

	// Root node
	"_ROOTNODE_",

	// Analysis commands
	"finalise",
	"frameanalyse",
	"modelanalyse",
	"pdens,rrrrrr",
	"printjobs",
	"rdf,rrrrrrr",
	"savequantities",

	// Bonding commands
	"addbond,rro",
	"augment",
	"bondtol,r",
	"bondpatterns",
	"bondselection",
	"clearbonds",
	"rebond",

	// Build commands
	"addhydrogen",
	"addatom,r",
	"addchain,ro",
	"delete",
	"endchain",
	"locate,rrr",
	"move,rrr",
	"rotx,r",
	"roty,r",
	"rotz,r",
	"transmute,r",

	// Cell commands
	"fold",
	"fractoreal",
	"pack",
	"printcell",
	"replicatecell,rrrrrr",
	"scalecell,rrr",
	"setcell,rrrrrr",
	"setcellaxes,rrrrrrrrr",
	"setspacegroup,r",

	// Charge commands
	"chargeatom,rr",
	"chargeff",
	"chargefrommodel",
	"chargepatom,rr",
	"chargeselection,r",
	"chargetype,rr",
	"clearcharges",

	// Disordered Builder Commands
	"addcomponent,rrr",
	"disorder,r",
	"printcomponents",
	"setcentre,rrrr",
	"setgeometry,rrrro",
	"setoverlap,rr",
	"setshape,rr",
	"vdwscale,r",

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
	"ecut,r",
	"elec,roooo",
	"intra,r",
	"printexpression",
	"vcut,r",
	"vdw,r",

	// Field commands
	"savefield,r",
	"savefield2,rr",

	// Flow control
	"else",
	"elseif,rxe",
	"end",
	"for,roo",
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
	"ffpatternid,r",
	"loadff,ro",
	"selectff,r",
	"typemodel",
	"typetest,r",

	// Grid commands
	"addgridpoint,rrrr",
	"addnextgridpoint,r",
	"finalisegrid",
	"newgrid,r",
	"setgrid,rrrrrrrrr",
	"setgridcubic,r",
	"setgridorigin,rrr",
	"setgridortho,rrr",
	"setgridsize,rrr",

	//"image 

	// Labeling commands
	"clearlabels",
	"addlabel,r",
	"removelabel,r",

	// MC commands
	"mcaccept,rr",
	"mcallow,rr",
	"mcmaxstep,rr",
	"mcntrials,rr",
	"printmc",

	// Messaging
	"error,s",
	"print,s",
	"warn,s",

	// Minimisation commands
	"cgminimise",
	"converge,rr",
	"linetol,r",
	"mcminimise,r",
	"sdminimise,r",
	"simplexminimise",

	// Model commands
	"createatoms",
	"finalisemodel",
	"listmodels",
	"loadmodel,ro",
	"modeltemplate",
	"newmodel,r",
	"printmodel",
	"savemodel,rr",
	"selectmodel,r",
	"setatom,r",
	"setfx,rr",
	"setfy,rr",
	"setfz,rr",
	"setrx,rr",
	"setry,rr",
	"setrz,rr",
	"settitle,r",
	"setvx,rr",
	"setvy,rr",
	"setvz,rr",

	// Pattern commands
	"addpattern,rrr",
	"clearpatterns",
	"createpatterns",
	"printpatterns",
	"selectpattern,r",

	// Preferences commands
	"atomdetail,r",
	"bonddetail,r",
	"colour,rrrr",
	"densityunits,r",
	"elementambient,rrrr",
	"elementdiffuse,rrrr",
	"elementradius,rr",
	"energyunits,r",
	"gl,rr",
	"key,rr",
	"mouse,rr",
	"radius,rr",
	"shininess,r",
	"show,rr",
	"style,r",

	// Read / Write Commands
	"addreadoption,r",
	"find,rro",
	"readchars,rr",
	"readdouble,r",
	"readint,r",
	"readline,f",
	"readnext,r",
	"readvar,rf",
	"removereadoption,r",
	"skipchars,r",
	"skipline,o",
	"writeline,f",

	// Selection commands
	"selectall",
	"selectatom,r",
	"selectelement,r",
	"selectfftype,r",
	"invert",
	"selectnone",
	"selectoverlaps,r",
	"selecttype,rr",

	// Site commands
	"addsite,rro",
	"printsites",
	"selectsite,r",
	"setaxes,rr",

	// Trajectory commands
	"firstframe",
	"lastframe",
	"loadtrajectory,r",
	"nextframe",
	"prevframe",

	// Transformation commands
	"centre,rrr",
	"centreselection,rrr",
	"translate,rrr",
	"translateatom,rrr",
	"translate,rrr",
	"mirror,r",

	// Variables
	"dec,r",
	"eval,r=e",
	"inc,r",
	"let,rxe"

	};

command_action CA_from_text(const char* s)
	{ return (command_action) enum_search_data("command", CA_NITEMS, CA_data, s); }
const char *text_from_CA(command_action ca)
	{ return get_before_comma(CA_data[ca]); }
const char *vars_from_CA(command_action ca)
	{ return get_after_comma(CA_data[ca]); }

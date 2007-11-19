/*
	*** Script command definitions
	*** src/script/scriptcommands.cpp
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

#include "script/scriptcommands.h"
#include "base/sysfunc.h"
#include <string.h>

// Script command data
const char *SC_data[SC_NITEMS] = {

	// Analysis
	"finalise",
	"frameanalyse",
	"modelanalyse",
	"pdens,G??GGG",
	"printjobs",
	"rdf,G??GGGG",
	"savequantities",

	// Bonding 
	"augment",
	"rebond",
	"clearbonds",
	"bondtol,G",
	"bondpatterns",
	"bondselection",

	// Build commands
	"addhydrogen",
	"addatom,G",
	"addchain,G|g",
	"delete",
	"endchain",
	"locate,GGG",
	"move,GGG",
	"rotx,G",
	"roty,G",
	"rotz,G",
	"transmute,G",

	// Cell commands
	"printcell",
	"replicatecell,GGGGGG",
	"scalecell,GGG",
	"setcell,GGGGGG",

	// Charge commands
	"chargeatom,GG",
	"chargeff",
	"chargefrommodel",
	"chargepatom,GG",
	"chargeselection,G",
	"chargetype,GG",
	"clearcharges",

	// Disordered Builder Commands
	"addcomponent,GGG",
	"disorder,G",
	"printcomponents",
	"setcentre,GGGG",
	"setgeometry,GGGG|g",
	"setoverlap,GG",
	"setshape,GG",
	"vdwscale,G",

	// Element Commands
	"setelementcolour,GGGG",
	"setelementradius,GG",

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
	"ecut,G",
	"elec,Ggggg",
	"intra,G",
	"printexpression",
	"vcut,G",
	"vdw,G",

	// Forcefield commands
	"ffmodel",
	"ffpattern",
	"ffpatternid,G",
	"loadff,GG",
	"selectff,G",
	"typemodel",
	"typetest,GG",

	// Field commands
	"savefield,G",
	"savefield2,GG",

	// Force commands
	"frameforces",
	"modelforces",
	"printforces",

	//"image 

	// MC commands
	"mcaccept,GG",
	"mcallow,GG",
	"mcmaxstep,GG",
	"mcntrials,GG",
	"printmc",

	// Minimisation commands
	"cgminimise",
	"converge,GG",
	"linetol,G",
	"mcminimise,G",
	"sdminimise,G",
	"simplexminimise",

	// Model commands
	"listmodels",
	"loadmodel,GG",
	"newmodel,G",
	"printmodel",
	"savemodel,GG",
	"selectmodel,G",

	// Pattern commands
	"addpattern,GGG",
	"clearpatterns",
	"createpatterns",
	"printpatterns",
	"selectpattern,G",

	// Preferences commands
	"atomdetail,G",
	"bonddetail,G",
	"colour,GGGG",
	"densityunits,G",
	"energyunits,G",
	"gl,GG",
	"key,GG",
	"mouse,GG",
	"movestyle,G",
	"radius,GG",
	"shininess,G",
	"show,GG",
	"style,G",

	// Selection commands
	"selectall",
	"selectatom,G",
	"selectelement,G",
	"selectfftype,G",
	"invert",
	"selectnone",
	"selectoverlaps,g",
	"selecttype,gg",

	// Site commands
	"addsite,GG|g",
	"printsites",
	"selectsite,G",
	"setaxes,GG",

	// Trajectory commands
	"firstframe",
	"lastframe",
	"loadtrajectory,G",
	"nextframe",
	"prevframe",

	// Transformation commands
	"translateatom,GGG",
	"translate,GGG",
	"mirror,G",

	"quit"

	};
script_command SC_from_text(const char* s)
	{ return (script_command) enum_search_data("script command", SC_NITEMS, SC_data, s); }
const char *text_from_SC(script_command sc)
	{ return get_before_comma(SC_data[sc]); }
const char *vars_from_SC(script_command sc)
	{ return get_after_comma(SC_data[sc]); }

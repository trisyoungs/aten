/*
	*** Script command syntax
	*** src/script/scriptsyntax.cpp
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
const char *SC_syntax[SC_NITEMS] = {

	// Analysis
	"finalise",
	"frameanalyse",
	"modelanalyse",
	"pdens <name> <site1> <site2> <griddelta> <nsteps> <filename>",
	"printjobs",
	"rdf <name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>",
	"savequantities",

	// Bonding 
	"augment",
	"rebond",
	"clearbonds",
	"bondtol <tolerance>",
	"bondpatterns",
	"bondselection",

	// Build commands
	"addhydrogen",
	"addatom <element>",
	"addchain <element> [bondstyle]",
	"delete",
	"endchain",
	"locate <x> <y> <z>",
	"move <dx> <dy> <dz>",
	"rotx <angle>",
	"roty <angle>",
	"rotz <angle>",
	"transmute <element>",

	// Cell commands
	"printcell",
	"replicatecell <neg.x> <neg.y> <neg.z> <pos.x> <pos.y> <pos.z>",
	"scalecell <x> <y> <z>",
	"setcell <a> <b> <c> <alpha> <beta> <gamma>",

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

	// Element Commands
	"setelementcolour <element> <r> <g> <b>",
	"setelementradius <element> <radius>",

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

	// Forcefield commands
	"ffmodel",
	"ffpattern",
	"ffpatternid <patternid>",
	"loadff <name> <filename>",
	"selectff <name>",

	// Field commands
	"savefield <filename>",
	"savefield2 <format> <filename> ",

	// Force commands
	"frameforces",
	"modelforces",
	"printforces",

	//"image 

	// MC commands
	"mcaccept <movetype> <energy>",
	"mcallow <movetype> yes|no",
	"mcmaxstep <movetype> <step>",
	"mcntrials <movetype> <ntrials>",
	"printmc",

	// Minimisation commands
	"cgminimise",
	"converge <energy> <forces>",
	"mcminimise <maxsteps>",
	"sdminimise",
	"simplexminimise",

	// Model commands
	"listmodels",
	"loadmodel <name> <filename>",
	"newmodel <name>",
	"printmodel",
	"savemodel <format> <filename>",
	"selectmodel <name>",

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
	"energyunits j|kj|cal|kcal|ha",
	"gl <option> on|off",
	"key ctrl|shift|alt <action>",
	"mouse left|middle|right|wheel <action>",
	"movestyle <style>",
	"radius <style> <r>",
	"shininess <n>",
	"show <object> yes|no",
	"style <style>",

	// Selection commands
	"selectall",
	"selectatom <id>",
	"selectelement <el>",
	"selectfftype <typename>",
	"invert",
	"selectnone",
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
	"translateatom <dx> <dy> <dz>",
	"translate <dx> <dy> <dz>",
	"mirror <axis>",

	"quit"

	};
const char *syntax_from_SC(script_command sc)
	{ return SC_syntax[sc]; }

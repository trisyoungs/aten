/*
	*** Program commands
	*** src/command/commands.h
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

#ifndef H_COMMANDS_H
#define H_COMMANDS_H

#include "classes/bundle.h"

// Forward declarations
class command;
class command_functions;
class model;
class atom;
class forcefield;
class grid;
class pattern;
class bundle;

// Function return values
enum command_return { CR_SUCCESS, CR_SUCCESSNOMOVE, CR_FAIL, CR_FAILCONTINUE, CR_EXIT };

// Command actions
enum command_action {

	// Variable declaration
	CA_CHAR,
	CA_INT,
	CA_DOUBLE,
	CA_ATOM,
	CA_BOND,
	CA_PATTERN,
	CA_MODEL,
	CA_PATBOUND,

	// Root node
	CA_ROOTNODE,

	// Analyse commands
	CA_FINALISE,
	CA_FRAMEANALYSE,
	CA_MODELANALYSE,
	CA_PDENS,
	CA_PRINTJOBS,
	CA_RDF,
	CA_SAVEQUANTITIES,

	// Atom Commands
	CA_ADDATOM,
	CA_ADDCHAIN,
	CA_ENDCHAIN,
	CA_SETCHARGE,
	CA_SETCOORDS,
	CA_SETELEMENT,
	CA_SETFORCES,
	CA_SETFX,
	CA_SETFY,
	CA_SETFZ,
	CA_SETID,
	CA_SETRX,
	CA_SETRY,
	CA_SETRZ,
	CA_SETVELOCITIES,
	CA_SETVX,
	CA_SETVY,
	CA_SETVZ,

	// Bond commands
	CA_ADDBOND,
	CA_AUGMENT,
	CA_BONDTOLERANCE,
	CA_BONDPATTERNS,
	CA_BONDSELECTION,
	CA_CLEARBONDS,
	CA_REBOND,

	// Build commands
	CA_ADDHYDROGEN,
	CA_DELETE,
	CA_LOCATE,
	CA_MOVE,
	CA_ROTX,
	CA_ROTY,
	CA_ROTZ,
	CA_TRANSMUTE,

	// Cell commands
	CA_FOLD,
	CA_FRACTOREAL,
	CA_PACK,
	CA_PRINTCELL,
	CA_REPLICATECELL,
	CA_SCALECELL,
	CA_SETCELL,
	CA_SETCELLAXES,
	CA_SETSPACEGROUP,

	// Charge commands
	CA_CHARGEATOM,
	CA_CHARGEFF,
	CA_CHARGEFROMMODEL,
	CA_CHARGEPATOM,
	CA_CHARGESELECTION,
	CA_CHARGETYPE,
	CA_CLEARCHARGES,

	// Disordered build commands
	CA_ADDCOMPONENT,
	CA_DISORDER,
	CA_PRINTCOMPONENTS,
	CA_SETCENTRE,
	CA_SETGEOMETRY,
	CA_SETOVERLAP,
	CA_SETSHAPE,
	CA_VDWSCALE,

	// Energy Commands
	CA_FRAMEENERGY,
	CA_MODELENERGY,
	CA_PRINTELEC,
	CA_PRINTEWALD,
	CA_PRINTINTER,
	CA_PRINTINTRA,
	CA_PRINTENERGY,
	CA_PRINTSUMMARY,
	CA_PRINTVDW,

	// Expression Commands
	CA_CREATEEXPRESSION,
	CA_ECUT,
	CA_ELEC,
	CA_INTRA,
	CA_PRINTEXPRESSION,
	CA_VCUT,
	CA_VDW,

	// Field Commands
	CA_SAVEFIELD,
	CA_SAVEFIELD2,

	// Flow control
	CA_ELSE,
	CA_ELSEIF,
	CA_END,
	CA_FOR,
	CA_GOTO,
	CA_GOTONONIF,
	CA_IF,
	CA_TERMINATE,
	CA_QUIT,

	// Force Commands
	CA_FRAMEFORCES,
	CA_MODELFORCES,
	CA_PRINTFORCES,

	// Forcefield Commands
	CA_FFMODEL,
	CA_FFPATTERN,
	CA_FFPATTERNID,
	CA_LOADFF,
	CA_SELECTFF,
	CA_TYPEMODEL,
	CA_TYPETEST,

	// Grid Commands
	CA_ADDGRIDPOINT,
	CA_ADDNEXTGRIDPOINT,
	CA_FINALISEGRID,
	CA_NEWGRID,
	CA_SETGRID,
	CA_SETGRIDCUBIC,
	CA_SETGRIDORIGIN,
	CA_SETGRIDORTHO,
	CA_SETGRIDSIZE,

	//image_

	// Labeling commands
	CA_CLEARLABELS,
	CA_ADDLABEL,
	CA_REMOVELABEL,

	// MC Commands
	CA_MCACCEPT,
	CA_MCALLOW,
	CA_MCMAXSTEP,
	CA_MCNTRIALS,
	CA_PRINTMC,

	// Messaging
	CA_ERROR,
	CA_PRINT,
	CA_WARN,

	// Minimisation Commands
	CA_CGMINIMISE,
	CA_CONVERGE,
	CA_LINETOL,
	CA_MCMINIMISE,
	CA_SDMINIMISE,
	CA_SIMPLEXMINIMISE,
	
	// Model Commands
	CA_CREATEATOMS,
	CA_FINALISEMODEL,
	CA_LISTMODELS,
	CA_LOADMODEL,
	CA_MODELTEMPLATE,
	CA_NEWMODEL,
	CA_PRINTMODEL,
	CA_SAVEMODEL,
	CA_SELECTMODEL,
	CA_SETTITLE,

	// Pattern Commands
	CA_ADDPATTERN,
	CA_CLEARPATTERNS,
	CA_CREATEPATTERNS,
	CA_PRINTPATTERNS,
	CA_SELECTPATTERN,

	// Preferences Commands
	CA_ATOMDETAIL,
	CA_BONDDETAIL,
	CA_COLOUR,
	CA_DENSITYUNITS,
	CA_ELEMENTAMBIENT,
	CA_ELEMENTDIFFUSE,
	CA_ELEMENTRADIUS,
	CA_ENERGYUNITS,
	CA_GL,
	CA_KEY,
	CA_MOUSE,
	CA_RADIUS,
	CA_SHININESS,
	CA_SHOW,
	CA_STYLE,

	// Read / Write Commands
	CA_ADDREADOPTION,
	CA_FIND,
	CA_READCHARS,
	CA_READDOUBLE,
	CA_READINTEGER,
	CA_READLINE,
	CA_READNEXT,
	CA_READVAR,
	CA_REMOVEREADOPTION,
	CA_SKIPCHARS,
	CA_SKIPLINE,
	CA_WRITELINE,

	// Select Commands
	CA_SELECTALL,
	CA_SELECTATOM,
	CA_SELECTELEMENT,
	CA_SELECTFFTYPE,
	CA_SELECTINVERT,
	CA_SELECTNONE,
	CA_SELECTOVERLAPS,
	CA_SELECTTYPE,
	
	// Site Commands
	CA_ADDSITE,
	CA_PRINTSITES,
	CA_SELECTSITE,
	CA_SETAXES,
	
	// Trajectory Commands
	CA_FIRSTFRAME,
	CA_LASTFRAME,
	CA_LOADTRAJECTORY,
	CA_NEXTFRAME,
	CA_PREVFRAME,

	// Transformation Commands
	CA_CENTRE,
	CA_CENTRESELECTION,
	CA_TRANSLATE,
	CA_TRANSLATEATOM,
	CA_TRANSLATESELECTION,
	CA_MIRRORSELECTION,

	// Variables
	CA_DECREASE,
	CA_EVAL,
	CA_INCREASE,
	CA_LET,

	CA_NITEMS
	};

command_action CA_from_text(const char*);
const char *text_from_CA(command_action);
const char *vars_from_CA(command_action);
const char *syntax_from_CA(command_action);

#endif

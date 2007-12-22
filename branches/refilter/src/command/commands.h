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

	// Bond commands
	CA_AUGMENT,
	CA_BONDTOLERANCE,
	CA_BONDPATTERNS,
	CA_BONDSELECTION,
	CA_CLEARBONDS,
	CA_REBOND,

	// Build commands
	CA_ADDHYDROGEN,
	CA_ADDATOM,
	CA_ADDCHAIN,
	CA_DELETE,
	CA_ENDCHAIN,
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

	// Element Commands
	CA_SETELEMENTCOLOUR,
	CA_SETELEMENTRADIUS,

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
	CA_FINALISEGRID,

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
	CA_PRINT,

	// Minimisation Commands
	CA_CGMINIMISE,
	CA_CONVERGE,
	CA_LINETOL,
	CA_MCMINIMISE,
	CA_SDMINIMISE,
	CA_SIMPLEXMINIMISE,
	
	// Model Commands
	CA_FINALISEMODEL,
	CA_LISTMODELS,
	CA_LOADMODEL,
	CA_NEWMODEL,
	CA_PRINTMODEL,
	CA_SAVEMODEL,
	CA_SELECTMODEL,

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
	CA_ENERGYUNITS,
	CA_GL,
	CA_KEY,
	CA_MOUSE,
	CA_RADIUS,
	CA_SHININESS,
	CA_SHOW,
	CA_STYLE,

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

// Function pointer typedef
typedef int (command_functions::*commandfunc)(command *&c, bundle &obj);

// Function return values
enum command_return { CR_SUCCESS, CR_SUCCESSNOMOVE, CR_FAIL, CR_FAILCONTINUE, CR_EXIT };

// Encompassing class for command actions
class command_functions
{
	public:
	// Constructor
	command_functions();
	// Array of pointers to command functions
	commandfunc action[CA_NITEMS];

	/*
	// Command Functions
	*/
	int function_CA_ROOTNODE(command *&c, bundle &obj);

	// Analyse commands
	int function_CA_FINALISE(command *&c, bundle &obj);
	int function_CA_FRAMEANALYSE(command *&c, bundle &obj);
	int function_CA_MODELANALYSE(command *&c, bundle &obj);
	int function_CA_PDENS(command *&c, bundle &obj);
	int function_CA_PRINTJOBS(command *&c, bundle &obj);
	int function_CA_RDF(command *&c, bundle &obj);
	int function_CA_SAVEQUANTITIES(command *&c, bundle &obj);

	// Bond commands
	int function_CA_AUGMENT(command *&c, bundle &obj);
	int function_CA_BONDTOLERANCE(command *&c, bundle &obj);
	int function_CA_BONDPATTERNS(command *&c, bundle &obj);
	int function_CA_BONDSELECTION(command *&c, bundle &obj);
	int function_CA_CLEARBONDS(command *&c, bundle &obj);
	int function_CA_REBOND(command *&c, bundle &obj);

	// Build commands
	int function_CA_ADDHYDROGEN(command *&c, bundle &obj);
	int function_CA_ADDATOM(command *&c, bundle &obj);
	int function_CA_ADDCHAIN(command *&c, bundle &obj);
	int function_CA_DELETE(command *&c, bundle &obj);
	int function_CA_ENDCHAIN(command *&c, bundle &obj);
	int function_CA_LOCATE(command *&c, bundle &obj);
	int function_CA_MOVE(command *&c, bundle &obj);
	int function_CA_ROTX(command *&c, bundle &obj);
	int function_CA_ROTY(command *&c, bundle &obj);
	int function_CA_ROTZ(command *&c, bundle &obj);
	int function_CA_TRANSMUTE(command *&c, bundle &obj);

	// Cell commands
	int function_CA_FOLD(command *&c, bundle &obj);
	int function_CA_FRACTOREAL(command *&c, bundle &obj);
	int function_CA_PACK(command *&c, bundle &obj);
	int function_CA_PRINTCELL(command *&c, bundle &obj);
	int function_CA_REPLICATECELL(command *&c, bundle &obj);
	int function_CA_SCALECELL(command *&c, bundle &obj);
	int function_CA_SETCELL(command *&c, bundle &obj);

	// Charge commands
	int function_CA_CHARGEATOM(command *&c, bundle &obj);
	int function_CA_CHARGEFF(command *&c, bundle &obj);
	int function_CA_CHARGEFROMMODEL(command *&c, bundle &obj);
	int function_CA_CHARGEPATOM(command *&c, bundle &obj);
	int function_CA_CHARGESELECTION(command *&c, bundle &obj);
	int function_CA_CHARGETYPE(command *&c, bundle &obj);
	int function_CA_CLEARCHARGES(command *&c, bundle &obj);

	// Disordered build commands
	int function_CA_ADDCOMPONENT(command *&c, bundle &obj);
	int function_CA_DISORDER(command *&c, bundle &obj);
	int function_CA_PRINTCOMPONENTS(command *&c, bundle &obj);
	int function_CA_SETCENTRE(command *&c, bundle &obj);
	int function_CA_SETGEOMETRY(command *&c, bundle &obj);
	int function_CA_SETOVERLAP(command *&c, bundle &obj);
	int function_CA_SETSHAPE(command *&c, bundle &obj);
	int function_CA_VDWSCALE(command *&c, bundle &obj);

	// Element Commands
	int function_CA_SETELEMENTCOLOUR(command *&c, bundle &obj);
	int function_CA_SETELEMENTRADIUS(command *&c, bundle &obj);

	// Energy Commands
	int function_CA_FRAMEENERGY(command *&c, bundle &obj);
	int function_CA_MODELENERGY(command *&c, bundle &obj);
	int function_CA_PRINTELEC(command *&c, bundle &obj);
	int function_CA_PRINTEWALD(command *&c, bundle &obj);
	int function_CA_PRINTINTER(command *&c, bundle &obj);
	int function_CA_PRINTINTRA(command *&c, bundle &obj);
	int function_CA_PRINTENERGY(command *&c, bundle &obj);
	int function_CA_PRINTSUMMARY(command *&c, bundle &obj);
	int function_CA_PRINTVDW(command *&c, bundle &obj);

	// Expression Commands
	int function_CA_CREATEEXPRESSION(command *&c, bundle &obj);
	int function_CA_ECUT(command *&c, bundle &obj);
	int function_CA_ELEC(command *&c, bundle &obj);
	int function_CA_INTRA(command *&c, bundle &obj);
	int function_CA_PRINTEXPRESSION(command *&c, bundle &obj);
	int function_CA_VCUT(command *&c, bundle &obj);
	int function_CA_VDW(command *&c, bundle &obj);

	// Flow control
	int function_CA_ELSE(command *&c, bundle &obj);
	int function_CA_ELSEIF(command *&c, bundle &obj);
	int function_CA_END(command *&c, bundle &obj);
	int function_CA_FOR(command *&c, bundle &obj);
	int function_CA_GOTO(command *&c, bundle &obj);
	int function_CA_GOTONONIF(command *&c, bundle &obj);
	int function_CA_IF(command *&c, bundle &obj);
	int function_CA_TERMINATE(command *&c, bundle &obj);

	// Forcefield Commands
	int function_CA_FFMODEL(command *&c, bundle &obj);
	int function_CA_FFPATTERN(command *&c, bundle &obj);
	int function_CA_FFPATTERNID(command *&c, bundle &obj);
	int function_CA_LOADFF(command *&c, bundle &obj);
	int function_CA_SELECTFF(command *&c, bundle &obj);
	int function_CA_TYPEMODEL(command *&c, bundle &obj);
	int function_CA_TYPETEST(command *&c, bundle &obj);

	// Field Commands
	int function_CA_SAVEFIELD(command *&c, bundle &obj);
	int function_CA_SAVEFIELD2(command *&c, bundle &obj);

	// Grid Commands
	int function_CA_FINALISEGRID(command *&c, bundle &obj);

	// Force Commands
	int function_CA_FRAMEFORCES(command *&c, bundle &obj);
	int function_CA_MODELFORCES(command *&c, bundle &obj);
	int function_CA_PRINTFORCES(command *&c, bundle &obj);

	//image_

	// Labeling commands
	int function_CA_CLEARLABELS(command *&c, bundle &obj);
	int function_CA_ADDLABEL(command *&c, bundle &obj);
	int function_CA_REMOVELABEL(command *&c, bundle &obj);

	// MC Commands
	int function_CA_MCACCEPT(command *&c, bundle &obj);
	int function_CA_MCALLOW(command *&c, bundle &obj);
	int function_CA_MCMAXSTEP(command *&c, bundle &obj);
	int function_CA_MCNTRIALS(command *&c, bundle &obj);
	int function_CA_PRINTMC(command *&c, bundle &obj);

	// Messaging
	int function_CA_PRINT(command *&c, bundle &obj);

	// Minimisation Commands
	int function_CA_CGMINIMISE(command *&c, bundle &obj);
	int function_CA_CONVERGE(command *&c, bundle &obj);
	int function_CA_LINETOL(command *&c, bundle &obj);
	int function_CA_MCMINIMISE(command *&c, bundle &obj);
	int function_CA_SDMINIMISE(command *&c, bundle &obj);
	int function_CA_SIMPLEXMINIMISE(command *&c, bundle &obj);
	
	// Model Commands
	int function_CA_FINALISEMODEL(command *&c, bundle &obj);
	int function_CA_LISTMODELS(command *&c, bundle &obj);
	int function_CA_LOADMODEL(command *&c, bundle &obj);
	int function_CA_NEWMODEL(command *&c, bundle &obj);
	int function_CA_PRINTMODEL(command *&c, bundle &obj);
	int function_CA_SAVEMODEL(command *&c, bundle &obj);
	int function_CA_SELECTMODEL(command *&c, bundle &obj);

	// Pattern Commands
	int function_CA_ADDPATTERN(command *&c, bundle &obj);
	int function_CA_CLEARPATTERNS(command *&c, bundle &obj);
	int function_CA_CREATEPATTERNS(command *&c, bundle &obj);
	int function_CA_PRINTPATTERNS(command *&c, bundle &obj);
	int function_CA_SELECTPATTERN(command *&c, bundle &obj);

	// Preferences Commands
	int function_CA_ATOMDETAIL(command *&c, bundle &obj);
	int function_CA_BONDDETAIL(command *&c, bundle &obj);
	int function_CA_COLOUR(command *&c, bundle &obj);
	int function_CA_DENSITYUNITS(command *&c, bundle &obj);
	int function_CA_ENERGYUNITS(command *&c, bundle &obj);
	int function_CA_GL(command *&c, bundle &obj);
	int function_CA_KEY(command *&c, bundle &obj);
	int function_CA_MOUSE(command *&c, bundle &obj);
	int function_CA_RADIUS(command *&c, bundle &obj);
	int function_CA_SHININESS(command *&c, bundle &obj);
	int function_CA_SHOW(command *&c, bundle &obj);
	int function_CA_STYLE(command *&c, bundle &obj);

	// Select Commands
	int function_CA_SELECTALL(command *&c, bundle &obj);
	int function_CA_SELECTATOM(command *&c, bundle &obj);
	int function_CA_SELECTELEMENT(command *&c, bundle &obj);
	int function_CA_SELECTFFTYPE(command *&c, bundle &obj);
	int function_CA_SELECTINVERT(command *&c, bundle &obj);
	int function_CA_SELECTNONE(command *&c, bundle &obj);
	int function_CA_SELECTOVERLAPS(command *&c, bundle &obj);
	int function_CA_SELECTTYPE(command *&c, bundle &obj);
	
	// Site Commands
	int function_CA_ADDSITE(command *&c, bundle &obj);
	int function_CA_PRINTSITES(command *&c, bundle &obj);
	int function_CA_SELECTSITE(command *&c, bundle &obj);
	int function_CA_SETAXES(command *&c, bundle &obj);
	
	// Trajectory Commands
	int function_CA_FIRSTFRAME(command *&c, bundle &obj);
	int function_CA_LASTFRAME(command *&c, bundle &obj);
	int function_CA_LOADTRAJECTORY(command *&c, bundle &obj);
	int function_CA_NEXTFRAME(command *&c, bundle &obj);
	int function_CA_PREVFRAME(command *&c, bundle &obj);

	// Transform Commands
	int function_CA_CENTRE(command *&c, bundle &obj);
	int function_CA_CENTRESELECTION(command *&c, bundle &obj);
	int function_CA_TRANSLATE(command *&c, bundle &obj);
	int function_CA_TRANSLATEATOM(command *&c, bundle &obj);
	int function_CA_TRANSLATESELECTION(command *&c, bundle &obj);
	int function_CA_MIRRORSELECTION(command *&c, bundle &obj);

	// Variables
	int function_CA_LET(command *&c, bundle &obj);
	int function_CA_INCREASE(command *&c, bundle &obj);
	int function_CA_DECREASE(command *&c, bundle &obj);
	int function_CA_EVAL(command *&c, bundle &obj);

	int function_CA_QUIT(command *&c, bundle &obj);
};

command_action CA_from_text(const char*);
const char *text_from_CA(command_action);
const char *vars_from_CA(command_action);
const char *syntax_from_CA(command_action);

#endif

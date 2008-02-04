/*
	*** Command function pointers
	*** src/command/functions.h
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

#ifndef H_FUNCTIONS_H
#define H_FUNCTIONS_H

#include "command/commands.h"

// Forward declarations
class command;
class bundle;

// Function pointer typedef
typedef int (command_functions::*commandfunc)(command *&c, bundle &obj);

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
	int function_CA_HELP(command *&c, bundle &obj);

	// Analyse commands
	int function_CA_FINALISE(command *&c, bundle &obj);
	int function_CA_FRAMEANALYSE(command *&c, bundle &obj);
	int function_CA_MODELANALYSE(command *&c, bundle &obj);
	int function_CA_PDENS(command *&c, bundle &obj);
	int function_CA_PRINTJOBS(command *&c, bundle &obj);
	int function_CA_RDF(command *&c, bundle &obj);
	int function_CA_SAVEQUANTITIES(command *&c, bundle &obj);
	int function_CA_TRAJANALYSE(command *&c, bundle &obj);

	// Atom Commands
	int function_CA_ADDATOM(command *&c, bundle &obj);
	int function_CA_ADDCHAIN(command *&c, bundle &obj);
	int function_CA_ENDCHAIN(command *&c, bundle &obj);
	int function_CA_SETCOORDS(command *&c, bundle &obj);
	int function_CA_SETCHARGE(command *&c, bundle &obj);
	int function_CA_SETELEMENT(command *&c, bundle &obj);
	int function_CA_SETFORCES(command *&c, bundle &obj);
	int function_CA_SETFX(command *&c, bundle &obj);
	int function_CA_SETFY(command *&c, bundle &obj);
	int function_CA_SETFZ(command *&c, bundle &obj);
	int function_CA_SETID(command *&c, bundle &obj);
	int function_CA_SETRX(command *&c, bundle &obj);
	int function_CA_SETRY(command *&c, bundle &obj);
	int function_CA_SETRZ(command *&c, bundle &obj);
	int function_CA_SETVELOCITIES(command *&c, bundle &obj);
	int function_CA_SETVX(command *&c, bundle &obj);
	int function_CA_SETVY(command *&c, bundle &obj);
	int function_CA_SETVZ(command *&c, bundle &obj);

	// Bond commands
	int function_CA_ADDBOND(command *&c, bundle &obj);
	int function_CA_ADDBONDID(command *&c, bundle &obj);
	int function_CA_AUGMENT(command *&c, bundle &obj);
	int function_CA_BONDTOLERANCE(command *&c, bundle &obj);
	int function_CA_BONDPATTERNS(command *&c, bundle &obj);
	int function_CA_BONDSELECTION(command *&c, bundle &obj);
	int function_CA_CLEARBONDS(command *&c, bundle &obj);
	int function_CA_REBOND(command *&c, bundle &obj);

	// Build commands
	int function_CA_ADDHYDROGEN(command *&c, bundle &obj);
	int function_CA_DELETE(command *&c, bundle &obj);
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
	int function_CA_SETCELLAXES(command *&c, bundle &obj);
	int function_CA_SETSPACEGROUP(command *&c, bundle &obj);

	// Charge commands
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
	int function_CA_PRINTSETUP(command *&c, bundle &obj);
	int function_CA_VCUT(command *&c, bundle &obj);
	int function_CA_VDW(command *&c, bundle &obj);

	// Field Commands
	int function_CA_SAVEFIELD(command *&c, bundle &obj);

	// Flow control
	int function_CA_ELSE(command *&c, bundle &obj);
	int function_CA_ELSEIF(command *&c, bundle &obj);
	int function_CA_END(command *&c, bundle &obj);
	int function_CA_FOR(command *&c, bundle &obj);
	int function_CA_GOTO(command *&c, bundle &obj);
	int function_CA_GOTONONIF(command *&c, bundle &obj);
	int function_CA_IF(command *&c, bundle &obj);
	int function_CA_QUIT(command *&c, bundle &obj);
	int function_CA_TERMINATE(command *&c, bundle &obj);

	// Force Commands
	int function_CA_FRAMEFORCES(command *&c, bundle &obj);
	int function_CA_MODELFORCES(command *&c, bundle &obj);
	int function_CA_PRINTFORCES(command *&c, bundle &obj);

	// Forcefield Commands
	int function_CA_FFMODEL(command *&c, bundle &obj);
	int function_CA_FFPATTERN(command *&c, bundle &obj);
	int function_CA_FFPATTERNID(command *&c, bundle &obj);
	int function_CA_LOADFF(command *&c, bundle &obj);
	int function_CA_SELECTFF(command *&c, bundle &obj);
	int function_CA_TYPEMODEL(command *&c, bundle &obj);
	int function_CA_TYPETEST(command *&c, bundle &obj);

	// Grid Commands
	int function_CA_ADDGRIDPOINT(command *&c, bundle &obj);
	int function_CA_ADDNEXTGRIDPOINT(command *&c, bundle &obj);
	int function_CA_FINALISEGRID(command *&c, bundle &obj);
	int function_CA_NEWGRID(command *&c, bundle &obj);
	int function_CA_SETGRID(command *&c, bundle &obj);
	int function_CA_SETGRIDCUBIC(command *&c, bundle &obj);
	int function_CA_SETGRIDORIGIN(command *&c, bundle &obj);
	int function_CA_SETGRIDORTHO(command *&c, bundle &obj);
	int function_CA_SETGRIDSIZE(command *&c, bundle &obj);

	// Image Commands
	int function_CA_SAVEBITMAP(command *&c, bundle &obj);
	int function_CA_SAVEVECTOR(command *&c, bundle &obj);

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
	int function_CA_ERROR(command *&c, bundle &obj);
	int function_CA_PRINT(command *&c, bundle &obj);
	int function_CA_WARN(command *&c, bundle &obj);

	// Minimisation Commands
	int function_CA_CGMINIMISE(command *&c, bundle &obj);
	int function_CA_CONVERGE(command *&c, bundle &obj);
	int function_CA_LINETOL(command *&c, bundle &obj);
	int function_CA_MCMINIMISE(command *&c, bundle &obj);
	int function_CA_SDMINIMISE(command *&c, bundle &obj);
	int function_CA_SIMPLEXMINIMISE(command *&c, bundle &obj);
	
	// Model Commands
	int function_CA_CREATEATOMS(command *&c, bundle &obj);
	int function_CA_FINALISEMODEL(command *&c, bundle &obj);
	int function_CA_LISTMODELS(command *&c, bundle &obj);
	int function_CA_LOADMODEL(command *&c, bundle &obj);
	int function_CA_MODELTEMPLATE(command *&c, bundle &obj);
	int function_CA_NEWMODEL(command *&c, bundle &obj);
	int function_CA_PRINTMODEL(command *&c, bundle &obj);
	int function_CA_SAVEMODEL(command *&c, bundle &obj);
	int function_CA_SELECTMODEL(command *&c, bundle &obj);
	int function_CA_SETTITLE(command *&c, bundle &obj);

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
	int function_CA_ELEMENTAMBIENT(command *&c, bundle &obj);
	int function_CA_ELEMENTDIFFUSE(command *&c, bundle &obj);
	int function_CA_ELEMENTRADIUS(command *&c, bundle &obj);
	int function_CA_ENERGYUNITS(command *&c, bundle &obj);
	int function_CA_GL(command *&c, bundle &obj);
	int function_CA_KEY(command *&c, bundle &obj);
	int function_CA_MOUSE(command *&c, bundle &obj);
	int function_CA_RADIUS(command *&c, bundle &obj);
	int function_CA_SHININESS(command *&c, bundle &obj);
	int function_CA_SHOW(command *&c, bundle &obj);
	int function_CA_STYLE(command *&c, bundle &obj);

	// Read / Write Commands
	int function_CA_ADDREADOPTION(command *&c, bundle &obj);
	int function_CA_FIND(command *&c, bundle &obj);
	int function_CA_READCHARS(command *&c, bundle &obj);
	int function_CA_READDOUBLE(command *&c, bundle &obj);
	int function_CA_READINTEGER(command *&c, bundle &obj);
	int function_CA_READLINE(command *&c, bundle &obj);
	int function_CA_READNEXT(command *&c, bundle &obj);
	int function_CA_READVAR(command *&c, bundle &obj);
	int function_CA_REMOVEREADOPTION(command *&c, bundle &obj);
	int function_CA_REWIND(command *&c, bundle &obj);
	int function_CA_SKIPCHARS(command *&c, bundle &obj);
	int function_CA_SKIPLINE(command *&c, bundle &obj);
	int function_CA_WRITELINE(command *&c, bundle &obj);

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
	int function_CA_TRANSLATE(command *&c, bundle &obj);
	int function_CA_TRANSLATEATOM(command *&c, bundle &obj);
	int function_CA_MIRROR(command *&c, bundle &obj);

	// Variables
	int function_CA_LET(command *&c, bundle &obj);
	int function_CA_INCREASE(command *&c, bundle &obj);
	int function_CA_DECREASE(command *&c, bundle &obj);
	int function_CA_EVAL(command *&c, bundle &obj);

	// View
	int function_CA_RESETVIEW(command *&c, bundle &obj);
	int function_CA_ROTATEVIEW(command *&c, bundle &obj);
	int function_CA_TRANSLATEVIEW(command *&c, bundle &obj);
	int function_CA_ZOOMVIEW(command *&c, bundle &obj);
	int function_CA_ZROTATEVIEW(command *&c, bundle &obj);
};

#endif

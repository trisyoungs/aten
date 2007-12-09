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

// Forward declarations
class command;
class command_functions;
class model;
class atom;
class forcefield;
class grid;
class pattern;

// Command actions
enum command_action {
	CA_ROOTNODE,

	CA_CHAR,
	CA_INT,
	CA_DOUBLE,
	CA_ATOM,
	CA_BOND,
	CA_PATTERN,
	CA_MODEL,
	CA_PATBOUND,

	CA_FOR,
	CA_FORATOMS,
	CA_FORBONDS,
	CA_FORPATTERNS,
	CA_FORMOLECULES,
	CA_FORFFBONDS,
	CA_FORFFANGLES,
	CA_FORFFTORSIONS,

	CA_IF,
	CA_ELSEIF,
	CA_ELSE,
	CA_END,

	CA_LET,
	CA_INCREASE,
	CA_DECREASE,
	CA_EVAL,

	CA_PRINT,

	CA_GOTO,
	CA_GOTONONIF,
	CA_TERMINATE,

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
	CA_REBOND,
	CA_CLEARBONDS,
	CA_BONDTOLERANCE,
	CA_BONDPATTERNS,
	CA_BONDSELECTION,

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
	CA_PRINTCELL,
	CA_REPLICATECELL,
	CA_CAALECELL,
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

	// Forcefield Commands
	CA_FFMODEL,
	CA_FFPATTERN,
	CA_FFPATTERNID,
	CA_LOADFF,
	CA_SELECTFF,
	CA_TYPEMODEL,
	CA_TYPETEST,

	// Field Commands
	CA_SAVEFIELD,
	CA_SAVEFIELD2,

	// Force Commands
	CA_FRAMEFORCES,
	CA_MODELFORCES,
	CA_PRINTFORCES,

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

	// Minimisation Commands
	CA_CGMINIMISE,
	CA_CONVERGE,
	CA_LINETOL,
	CA_MCMINIMISE,
	CA_SDMINIMISE,
	CA_SIMPLEXMINIMISE,
	
	// Model Commands
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

	// Translation Commands
	CA_TRANSLATEATOM,
	CA_TRANSLATESELECTION,
	CA_MIRRORSELECTION,

	CA_QUIT,

	CA_NITEMS
	};

// Pointer structure
struct objects
{
	// Convenience structure to pass bundle of object pointers
	model *m;
	pattern *p;
	atom *i;
	forcefield *f;
	grid *g;
};

// Function pointer typedef
typedef int (command_functions::*commandfunc)(command *&c, objects &obj);

// Function return values
enum command_return { CR_SUCCESS, CR_SUCCESSNOMOVE, CR_FAIL, CR_FAILCONTINUE };

// Encompassing class for command actions
class command_functions
{

	// Command Functions
	public:
	int commandfunc[CA_NITEMS];

	int function_CA_ROOTNODE(command *&c, objects &obj);

	int function_CA_REPEAT(command *&c, objects &obj);
	int function_CA_FORATOMS(command *&c, objects &obj);
	int function_CA_FORBONDS(command *&c, objects &obj);
	int function_CA_FORPATTERNS(command *&c, objects &obj);
	int function_CA_FORMOLECULES(command *&c, objects &obj);
	int function_CA_FORFFBONDS(command *&c, objects &obj);
	int function_CA_FORFFANGLES(command *&c, objects &obj);
	int function_CA_FORFFTORSIONS(command *&c, objects &obj);

	int function_CA_IF(command *&c, objects &obj);
	int function_CA_ELSEIF(command *&c, objects &obj);
	int function_CA_ELSE(command *&c, objects &obj);
	int function_CA_END(command *&c, objects &obj);

	int function_CA_LET(command *&c, objects &obj);
	int function_CA_INCREASE(command *&c, objects &obj);
	int function_CA_DECREASE(command *&c, objects &obj);
	int function_CA_EVAL(command *&c, objects &obj);
	int function_CA_EVALI(command *&c, objects &obj);

	int function_CA_PRINT(command *&c, objects &obj);

	int function_CA_GOTO(command *&c, objects &obj);
	int function_CA_GOTONONIF(command *&c, objects &obj);
	int function_CA_TERMINATE(command *&c, objects &obj);

	// Analyse commands
	int function_CA_FINALISE(command *&c, objects &obj);
	int function_CA_FRAMEANALYSE(command *&c, objects &obj);
	int function_CA_MODELANALYSE(command *&c, objects &obj);
	int function_CA_PDENS(command *&c, objects &obj);
	int function_CA_PRINTJOBS(command *&c, objects &obj);
	int function_CA_RDF(command *&c, objects &obj);
	int function_CA_SAVEQUANTITIES(command *&c, objects &obj);

	// Bond commands
	int function_CA_AUGMENT(command *&c, objects &obj);
	int function_CA_REBOND(command *&c, objects &obj);
	int function_CA_CLEARBONDS(command *&c, objects &obj);
	int function_CA_BONDTOLERANCE(command *&c, objects &obj);
	int function_CA_BONDPATTERNS(command *&c, objects &obj);
	int function_CA_BONDSELECTION(command *&c, objects &obj);

	// Build commands
	int function_CA_ADDHYDROGEN(command *&c, objects &obj);
	int function_CA_ADDATOM(command *&c, objects &obj);
	int function_CA_ADDCHAIN(command *&c, objects &obj);
	int function_CA_DELETE(command *&c, objects &obj);
	int function_CA_ENDCHAIN(command *&c, objects &obj);
	int function_CA_LOCATE(command *&c, objects &obj);
	int function_CA_MOVE(command *&c, objects &obj);
	int function_CA_ROTX(command *&c, objects &obj);
	int function_CA_ROTY(command *&c, objects &obj);
	int function_CA_ROTZ(command *&c, objects &obj);
	int function_CA_TRANSMUTE(command *&c, objects &obj);

	// Cell commands
	int function_CA_PRINTCELL(command *&c, objects &obj);
	int function_CA_REPLICATECELL(command *&c, objects &obj);
	int function_CA_SCALECELL(command *&c, objects &obj);
	int function_CA_SETCELL(command *&c, objects &obj);

	// Charge commands
	int function_CA_CHARGEATOM(command *&c, objects &obj);
	int function_CA_CHARGEFF(command *&c, objects &obj);
	int function_CA_CHARGEFROMMODEL(command *&c, objects &obj);
	int function_CA_CHARGEPATOM(command *&c, objects &obj);
	int function_CA_CHARGESELECTION(command *&c, objects &obj);
	int function_CA_CHARGETYPE(command *&c, objects &obj);
	int function_CA_CLEARCHARGES(command *&c, objects &obj);

	// Disordered build commands
	int function_CA_ADDCOMPONENT(command *&c, objects &obj);
	int function_CA_DISORDER(command *&c, objects &obj);
	int function_CA_PRINTCOMPONENTS(command *&c, objects &obj);
	int function_CA_SETCENTRE(command *&c, objects &obj);
	int function_CA_SETGEOMETRY(command *&c, objects &obj);
	int function_CA_SETOVERLAP(command *&c, objects &obj);
	int function_CA_SETSHAPE(command *&c, objects &obj);
	int function_CA_VDWSCALE(command *&c, objects &obj);

	// Element Commands
	int function_CA_SETELEMENTCOLOUR(command *&c, objects &obj);
	int function_CA_SETELEMENTRADIUS(command *&c, objects &obj);

	// Energy Commands
	int function_CA_FRAMEENERGY(command *&c, objects &obj);
	int function_CA_MODELENERGY(command *&c, objects &obj);
	int function_CA_PRINTELEC(command *&c, objects &obj);
	int function_CA_PRINTEWALD(command *&c, objects &obj);
	int function_CA_PRINTINTER(command *&c, objects &obj);
	int function_CA_PRINTINTRA(command *&c, objects &obj);
	int function_CA_PRINTENERGY(command *&c, objects &obj);
	int function_CA_PRINTSUMMARY(command *&c, objects &obj);
	int function_CA_PRINTVDW(command *&c, objects &obj);

	// Expression Commands
	int function_CA_CREATEEXPRESSION(command *&c, objects &obj);
	int function_CA_ECUT(command *&c, objects &obj);
	int function_CA_ELEC(command *&c, objects &obj);
	int function_CA_INTRA(command *&c, objects &obj);
	int function_CA_PRINTEXPRESSION(command *&c, objects &obj);
	int function_CA_VCUT(command *&c, objects &obj);
	int function_CA_VDW(command *&c, objects &obj);

	// Forcefield Commands
	int function_CA_FFMODEL(command *&c, objects &obj);
	int function_CA_FFPATTERN(command *&c, objects &obj);
	int function_CA_FFPATTERNID(command *&c, objects &obj);
	int function_CA_LOADFF(command *&c, objects &obj);
	int function_CA_SELECTFF(command *&c, objects &obj);
	int function_CA_TYPEMODEL(command *&c, objects &obj);
	int function_CA_TYPETEST(command *&c, objects &obj);

	// Field Commands
	int function_CA_SAVEFIELD(command *&c, objects &obj);
	int function_CA_SAVEFIELD2(command *&c, objects &obj);

	// Force Commands
	int function_CA_FRAMEFORCES(command *&c, objects &obj);
	int function_CA_MODELFORCES(command *&c, objects &obj);
	int function_CA_PRINTFORCES(command *&c, objects &obj);

	//image_

	// Labeling commands
	int function_CA_CLEARLABELS(command *&c, objects &obj);
	int function_CA_ADDLABEL(command *&c, objects &obj);
	int function_CA_REMOVELABEL(command *&c, objects &obj);

	// MC Commands
	int function_CA_MCACCEPT(command *&c, objects &obj);
	int function_CA_MCALLOW(command *&c, objects &obj);
	int function_CA_MCMAXSTEP(command *&c, objects &obj);
	int function_CA_MCNTRIALS(command *&c, objects &obj);
	int function_CA_PRINTMC(command *&c, objects &obj);

	// Minimisation Commands
	int function_CA_CGMINIMISE(command *&c, objects &obj);
	int function_CA_CONVERGE(command *&c, objects &obj);
	int function_CA_LINETOL(command *&c, objects &obj);
	int function_CA_MCMINIMISE(command *&c, objects &obj);
	int function_CA_SDMINIMISE(command *&c, objects &obj);
	int function_CA_SIMPLEXMINIMISE(command *&c, objects &obj);
	
	// Model Commands
	int function_CA_LISTMODELS(command *&c, objects &obj);
	int function_CA_LOADMODEL(command *&c, objects &obj);
	int function_CA_NEWMODEL(command *&c, objects &obj);
	int function_CA_PRINTMODEL(command *&c, objects &obj);
	int function_CA_SAVEMODEL(command *&c, objects &obj);
	int function_CA_SELECTMODEL(command *&c, objects &obj);

	// Pattern Commands
	int function_CA_ADDPATTERN(command *&c, objects &obj);
	int function_CA_CLEARPATTERNS(command *&c, objects &obj);
	int function_CA_CREATEPATTERNS(command *&c, objects &obj);
	int function_CA_PRINTPATTERNS(command *&c, objects &obj);
	int function_CA_SELECTPATTERN(command *&c, objects &obj);

	// Preferences Commands
	int function_CA_ATOMDETAIL(command *&c, objects &obj);
	int function_CA_BONDDETAIL(command *&c, objects &obj);
	int function_CA_COLOUR(command *&c, objects &obj);
	int function_CA_DENSITYUNITS(command *&c, objects &obj);
	int function_CA_ENERGYUNITS(command *&c, objects &obj);
	int function_CA_GL(command *&c, objects &obj);
	int function_CA_KEY(command *&c, objects &obj);
	int function_CA_MOUSE(command *&c, objects &obj);
	int function_CA_RADIUS(command *&c, objects &obj);
	int function_CA_SHININESS(command *&c, objects &obj);
	int function_CA_SHOW(command *&c, objects &obj);
	int function_CA_STYLE(command *&c, objects &obj);

	// Select Commands
	int function_CA_SELECTALL(command *&c, objects &obj);
	int function_CA_SELECTATOM(command *&c, objects &obj);
	int function_CA_SELECTELEMENT(command *&c, objects &obj);
	int function_CA_SELECTFFTYPE(command *&c, objects &obj);
	int function_CA_SELECTINVERT(command *&c, objects &obj);
	int function_CA_SELECTNONE(command *&c, objects &obj);
	int function_CA_SELECTOVERLAPS(command *&c, objects &obj);
	int function_CA_SELECTTYPE(command *&c, objects &obj);
	
	// Site Commands
	int function_CA_ADDSITE(command *&c, objects &obj);
	int function_CA_PRINTSITES(command *&c, objects &obj);
	int function_CA_SELECTSITE(command *&c, objects &obj);
	int function_CA_SETAXES(command *&c, objects &obj);
	
	// Trajectory Commands
	int function_CA_FIRSTFRAME(command *&c, objects &obj);
	int function_CA_LASTFRAME(command *&c, objects &obj);
	int function_CA_LOADTRAJECTORY(command *&c, objects &obj);
	int function_CA_NEXTFRAME(command *&c, objects &obj);
	int function_CA_PREVFRAME(command *&c, objects &obj);

	// Translation Commands
	int function_CA_TRANSLATEATOM(command *&c, objects &obj);
	int function_CA_TRANSLATESELECTION(command *&c, objects &obj);
	int function_CA_MIRRORSELECTION(command *&c, objects &obj);

	int function_CA_QUIT(command *&c, objects &obj);
};

command_action CA_from_text(const char*);
const char *text_from_CA(command_action);
const char *vars_from_CA(command_action);
const char *syntax_from_CA(command_action);

#endif

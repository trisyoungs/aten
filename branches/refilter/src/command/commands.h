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

// Command actions
enum command_action {
	CA_ROOTNODE,

	CA_REPEAT,
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
	CA_EVALI,

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
	CA_VDWCAALE,

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

// Function pointer typedef
typedef int (command_functions::*commandfunc)(command*&, model *m, grid *g, pattern *p, forcefield *f);

// Encompassing class for command actions
class command_functions
{

	public:
	int commandfunc[CA_NITEMS];

	int function_CA_ROOTNODE(command*&, model *m, grid *g, pattern *p, forcefield *f);

	int function_CA_REPEAT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORATOMS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORBONDS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORPATTERNS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORMOLECULES(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORFFBONDS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORFFANGLES(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FORFFTORSIONS(command*&, model *m, grid *g, pattern *p, forcefield *f);

	int function_CA_IF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ELSEIF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ELSE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_END(command*&, model *m, grid *g, pattern *p, forcefield *f);

	int function_CA_LET(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_INCREASE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_DECREASE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_EVAL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_EVALI(command*&, model *m, grid *g, pattern *p, forcefield *f);

	int function_CA_PRINT(command*&, model *m, grid *g, pattern *p, forcefield *f);

	int function_CA_GOTO(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_GOTONONIF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_TERMINATE(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Analyse commands
	int function_CA_FINALISE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FRAMEANALYSE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MODELANALYSE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PDENS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTJOBS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_RDF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SAVEQUANTITIES(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Bond commands
	int function_CA_AUGMENT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_REBOND(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CLEARBONDS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_BONDTOLERANCE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_BONDPATTERNS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_BONDSELECTION(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Build commands
	int function_CA_ADDHYDROGEN(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ADDATOM(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ADDCHAIN(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_DELETE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ENDCHAIN(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_LOCATE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MOVE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ROTX(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ROTY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ROTZ(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_TRANSMUTE(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Cell commands
	int function_CA_PRINTCELL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_REPLICATECELL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CAALECELL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETCELL(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Charge commands
	int function_CA_CHARGEATOM(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CHARGEFF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CHARGEFROMMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CHARGEPATOM(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CHARGESELECTION(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CHARGETYPE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CLEARCHARGES(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Disordered build commands
	int function_CA_ADDCOMPONENT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_DISORDER(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTCOMPONENTS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETCENTRE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETGEOMETRY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETOVERLAP(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETSHAPE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_VDWCAALE(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Element Commands
	int function_CA_SETELEMENTCOLOUR(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETELEMENTRADIUS(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Energy Commands
	int function_CA_FRAMEENERGY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MODELENERGY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTELEC(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTEWALD(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTINTER(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTINTRA(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTENERGY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTSUMMARY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTVDW(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Expression Commands
	int function_CA_CREATEEXPRESSION(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ECUT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ELEC(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_INTRA(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTEXPRESSION(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_VCUT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_VDW(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Forcefield Commands
	int function_CA_FFMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FFPATTERN(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_FFPATTERNID(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_LOADFF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTFF(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_TYPEMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_TYPETEST(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Field Commands
	int function_CA_SAVEFIELD(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SAVEFIELD2(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Force Commands
	int function_CA_FRAMEFORCES(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MODELFORCES(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTFORCES(command*&, model *m, grid *g, pattern *p, forcefield *f);

	//image_

	// Labeling commands
	int function_CA_CLEARLABELS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ADDLABEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_REMOVELABEL(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// MC Commands
	int function_CA_MCACCEPT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MCALLOW(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MCMAXSTEP(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MCNTRIALS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTMC(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Minimisation Commands
	int function_CA_CGMINIMISE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CONVERGE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_LINETOL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MCMINIMISE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SDMINIMISE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SIMPLEXMINIMISE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	
	// Model Commands
	int function_CA_LISTMODELS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_LOADMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_NEWMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SAVEMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTMODEL(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Pattern Commands
	int function_CA_ADDPATTERN(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CLEARPATTERNS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_CREATEPATTERNS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTPATTERNS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTPATTERN(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Preferences Commands
	int function_CA_ATOMDETAIL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_BONDDETAIL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_COLOUR(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_DENSITYUNITS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_ENERGYUNITS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_GL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_KEY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MOUSE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_RADIUS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SHININESS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SHOW(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_STYLE(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Select Commands
	int function_CA_SELECTALL(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTATOM(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTELEMENT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTFFTYPE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTINVERT(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTNONE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTOVERLAPS(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTTYPE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	
	// Site Commands
	int function_CA_ADDSITE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PRINTSITES(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SELECTSITE(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_SETAXES(command*&, model *m, grid *g, pattern *p, forcefield *f);
	
	// Trajectory Commands
	int function_CA_FIRSTFRAME(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_LASTFRAME(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_LOADTRAJECTORY(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_NEXTFRAME(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_PREVFRAME(command*&, model *m, grid *g, pattern *p, forcefield *f);

	// Translation Commands
	int function_CA_TRANSLATEATOM(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_TRANSLATESELECTION(command*&, model *m, grid *g, pattern *p, forcefield *f);
	int function_CA_MIRRORSELECTION(command*&, model *m, grid *g, pattern *p, forcefield *f);

	int function_CA_QUIT(command*&, model *m, grid *g, pattern *p, forcefield *f);
};

command_action CA_from_text(const char*);
const char *text_from_CA(command_action);
const char *vars_from_CA(command_action);
const char *syntax_from_CA(command_action);

#endif

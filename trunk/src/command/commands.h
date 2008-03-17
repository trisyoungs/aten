/*
	*** Program commands
	*** src/command/commands.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_COMMANDS_H
#define ATEN_COMMANDS_H

#include "classes/bundle.h"

// Forward declarations
class master_data;
class command;
class commanddata;

// Function pointer typedef and call #define
typedef int (commanddata::*commandfunc)(command *&c, bundle &obj);
#define CALL_COMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

// Function return values
enum command_return { CR_SUCCESS, CR_SUCCESSNOMOVE, CR_FAIL, CR_FAILCONTINUE, CR_EXIT, CR_EXITWITHERROR };

// Command actions
enum command_action {

	// Variable declaration
	CA_CHAR,
	CA_INT,
	CA_DOUBLE,
	CA_ATOM,
	CA_PATTERN,
	CA_MODEL,
	CA_BOND,
	CA_ANGLE,
	CA_TORSION,
	CA_ATOMTYPE,

	// Root node
	CA_ROOTNODE,

	// Analysis commands
	CA_FINALISE,
	CA_FRAMEANALYSE,
	CA_GEOMETRY,
	CA_MODELANALYSE,
	CA_PDENS,
	CA_PRINTJOBS,
	CA_RDF,
	CA_SAVEQUANTITIES,
	CA_TRAJANALYSE,

	// Atom Commands
	CA_CHAIN,
	CA_ENDCHAIN,
	CA_NEWATOM,
	CA_NEWATOMFRAC,
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
	CA_AUGMENT,
	CA_BONDTOLERANCE,
	CA_CLEARBONDS,
	CA_NEWBOND,
	CA_NEWBONDID,
	CA_REBONDPATTERNS,
	CA_REBONDSELECTION,
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
	CA_PRINTSETUP,
	CA_SAVEEXPRESSION,

	// Flow control
	CA_ELSE,
	CA_ELSEIF,
	CA_END,
	CA_FOR,
	CA_GOTO,
	CA_GOTONONIF,
	CA_IF,
	CA_TERMINATE,

	// Force Commands
	CA_FRAMEFORCES,
	CA_MODELFORCES,
	CA_PRINTFORCES,

	// Forcefield Commands
	CA_DEFAULTFF,
	CA_FFMODEL,
	CA_FFPATTERN,
	CA_FFPATTERNID,
	CA_GETFF,
	CA_LOADFF,
	CA_TYPEMODEL,
	CA_TYPETEST,

	// Glyph commands
	CA_NEWGLYPH,
	CA_SETGLYPHATOMF,
	CA_SETGLYPHATOMR,
	CA_SETGLYPHATOMV,
	CA_SETGLYPHATOMSF,
	CA_SETGLYPHATOMSR,
	CA_SETGLYPHATOMSV,
	CA_SETGLYPHDATA,
	CA_SETGLYPHSOLID,

	// Grid Commands
	CA_ADDGRIDPOINT,
	CA_ADDNEXTGRIDPOINT,
	CA_FINALISEGRID,
	CA_NEWGRID,
	CA_SETGRID,
	CA_SETGRIDCUBIC,
	CA_SETGRIDLOOPORDER,
	CA_SETGRIDORIGIN,
	CA_SETGRIDORTHO,
	CA_SETGRIDSIZE,

	// Image Commands
	CA_SAVEBITMAP,
	CA_SAVEVECTOR,

	// Labeling commands
	CA_CLEARLABELS,
	CA_LABEL,
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
	CA_VERBOSE,
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
	CA_GETMODEL,
	CA_INFO,
	CA_LISTMODELS,
	CA_LOADMODEL,
	CA_MODELTEMPLATE,
	CA_NEWMODEL,
	CA_SAVEMODEL,
	CA_SETTITLE,

	// Pattern Commands
	CA_CLEARPATTERNS,
	CA_CREATEPATTERNS,
	CA_GETPATTERN,
	CA_LISTPATTERNS,
	CA_NEWPATTERN,

	// Preferences Commands
	CA_ATOMDETAIL,
	CA_BONDDETAIL,
	CA_COLOUR,
	CA_DENSITYUNITS,
	CA_ECUT,
	CA_ELEC,
	CA_ELEMENTAMBIENT,
	CA_ELEMENTDIFFUSE,
	CA_ELEMENTRADIUS,
	CA_ENERGYUNITS,
	CA_GL,
	CA_INTRA,
	CA_KEY,
	CA_MOUSE,
	CA_RADIUS,
	CA_SHININESS,
	CA_SHOW,
	CA_STYLE,
	CA_VCUT,
	CA_VDW,

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
	CA_REWIND,
	CA_SKIPCHARS,
	CA_SKIPLINE,
	CA_WRITELINE,

	// Script Commands
	CA_LISTSCRIPTS,
	CA_LOADSCRIPT,
	CA_RUNSCRIPT,

	// Select Commands
	CA_SELECTALL,
	CA_SELECTATOM,
	CA_SELECTELEMENT,
	CA_SELECTFFTYPE,
	CA_SELECTINVERT,
	CA_SELECTNONE,
	CA_SELECTOVERLAPS,
	CA_SELECTPATTERN,
	CA_SELECTTYPE,
	
	// Site Commands
	CA_GETSITE,
	CA_LISTSITES,
	CA_NEWSITE,
	CA_SETAXES,

	// System commands
	CA_GUI,
	CA_HELP,
	CA_SEED,
	CA_QUIT,

	// Trajectory Commands
	CA_FIRSTFRAME,
	CA_LASTFRAME,
	CA_LOADTRAJECTORY,
	CA_NEXTFRAME,
	CA_PREVFRAME,

	// Transformation Commands
	CA_CENTRE,
	CA_TRANSLATE,
	CA_TRANSLATEATOM,
	CA_MIRROR,

	// Variables
	CA_DECREASE,
	CA_EVAL,
	CA_INCREASE,
	CA_LET,

	// View
	CA_RESETVIEW,
	CA_ROTATEVIEW,
	CA_SPEEDTEST,
	CA_TRANSLATEVIEW,
	CA_VIEWALONG,
	CA_VIEWALONGCELL,
	CA_ZOOMVIEW,
	CA_ZROTATEVIEW,

	CA_NITEMS
};

class commanddata
{
	/*
	// Command description
	*/
	public:
	// Command keyword
	const char *keyword;
	// Command arguments
	const char *arguments;
	// Command argument names
	const char *argtext;
	// Command syntax
	const char *syntax;

	/*
	// Get
	*/
	public:
	// Get command keyword
	const char *get_keyword() { return keyword; }
	// Get command arguments
	const char *get_arguments() { return arguments; }
	// Get command argument 'names'
	const char *get_argtext() { return argtext; }
	// Get command syntax
	const char *get_syntax() { return syntax; }
	// Return whether command accepts any arguments
	bool has_arguments() { return (!(arguments[0] == '\0')); }

	/*
	// Function
	*/
	private:
	// Provide full access to the master
	friend class master_data;
	// All command functions
	int function_CA_ROOTNODE(command *&c, bundle &obj);
	// Analyse commands
	int function_CA_FINALISE(command *&c, bundle &obj);
	int function_CA_FRAMEANALYSE(command *&c, bundle &obj);
	int function_CA_GEOMETRY(command *&c, bundle &obj);	
	int function_CA_MODELANALYSE(command *&c, bundle &obj);
	int function_CA_PDENS(command *&c, bundle &obj);
	int function_CA_PRINTJOBS(command *&c, bundle &obj);
	int function_CA_RDF(command *&c, bundle &obj);
	int function_CA_SAVEQUANTITIES(command *&c, bundle &obj);
	int function_CA_TRAJANALYSE(command *&c, bundle &obj);
	// Atom Commands
	int function_CA_CHAIN(command *&c, bundle &obj);
	int function_CA_ENDCHAIN(command *&c, bundle &obj);
	int function_CA_NEWATOM(command *&c, bundle &obj);
	int function_CA_NEWATOMFRAC(command *&c, bundle &obj);
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
	int function_CA_AUGMENT(command *&c, bundle &obj);
	int function_CA_BONDTOLERANCE(command *&c, bundle &obj);
	int function_CA_CLEARBONDS(command *&c, bundle &obj);
	int function_CA_NEWBOND(command *&c, bundle &obj);
	int function_CA_NEWBONDID(command *&c, bundle &obj);
	int function_CA_REBONDPATTERNS(command *&c, bundle &obj);
	int function_CA_REBONDSELECTION(command *&c, bundle &obj);
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
	int function_CA_PRINTSETUP(command *&c, bundle &obj);
	int function_CA_SAVEEXPRESSION(command *&c, bundle &obj);
	// Flow control
	int function_CA_ELSE(command *&c, bundle &obj);
	int function_CA_ELSEIF(command *&c, bundle &obj);
	int function_CA_END(command *&c, bundle &obj);
	int function_CA_FOR(command *&c, bundle &obj);
	int function_CA_GOTO(command *&c, bundle &obj);
	int function_CA_GOTONONIF(command *&c, bundle &obj);
	int function_CA_IF(command *&c, bundle &obj);
	int function_CA_TERMINATE(command *&c, bundle &obj);
	// Force Commands
	int function_CA_FRAMEFORCES(command *&c, bundle &obj);
	int function_CA_MODELFORCES(command *&c, bundle &obj);
	int function_CA_PRINTFORCES(command *&c, bundle &obj);
	// Forcefield Commands
	int function_CA_DEFAULTFF(command *&c, bundle &obj);
	int function_CA_FFMODEL(command *&c, bundle &obj);
	int function_CA_FFPATTERN(command *&c, bundle &obj);
	int function_CA_FFPATTERNID(command *&c, bundle &obj);
	int function_CA_GETFF(command *&c, bundle &obj);
	int function_CA_LOADFF(command *&c, bundle &obj);
	int function_CA_TYPEMODEL(command *&c, bundle &obj);
	int function_CA_TYPETEST(command *&c, bundle &obj);
	// Glyph commands
	int function_CA_NEWGLYPH(command *&c, bundle &obj);
	int function_CA_SETGLYPHATOMF(command *&c, bundle &obj);
	int function_CA_SETGLYPHATOMR(command *&c, bundle &obj);
	int function_CA_SETGLYPHATOMV(command *&c, bundle &obj);
	int function_CA_SETGLYPHATOMSF(command *&c, bundle &obj);
	int function_CA_SETGLYPHATOMSR(command *&c, bundle &obj);
	int function_CA_SETGLYPHATOMSV(command *&c, bundle &obj);
	int function_CA_SETGLYPHDATA(command *&c, bundle &obj);
	int function_CA_SETGLYPHSOLID(command *&c, bundle &obj);
	// Grid Commands
	int function_CA_ADDGRIDPOINT(command *&c, bundle &obj);
	int function_CA_ADDNEXTGRIDPOINT(command *&c, bundle &obj);
	int function_CA_FINALISEGRID(command *&c, bundle &obj);
	int function_CA_NEWGRID(command *&c, bundle &obj);
	int function_CA_SETGRID(command *&c, bundle &obj);
	int function_CA_SETGRIDCUBIC(command *&c, bundle &obj);
	int function_CA_SETGRIDLOOPORDER(command *&c, bundle &obj);
	int function_CA_SETGRIDORIGIN(command *&c, bundle &obj);
	int function_CA_SETGRIDORTHO(command *&c, bundle &obj);
	int function_CA_SETGRIDSIZE(command *&c, bundle &obj);
	// Image Commands
	int function_CA_SAVEBITMAP(command *&c, bundle &obj);
	int function_CA_SAVEVECTOR(command *&c, bundle &obj);
	// Labeling commands
	int function_CA_CLEARLABELS(command *&c, bundle &obj);
	int function_CA_LABEL(command *&c, bundle &obj);
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
	int function_CA_VERBOSE(command *&c, bundle &obj);
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
	int function_CA_GETMODEL(command *&c, bundle &obj);
	int function_CA_INFO(command *&c, bundle &obj);
	int function_CA_LISTMODELS(command *&c, bundle &obj);
	int function_CA_LOADMODEL(command *&c, bundle &obj);
	int function_CA_MODELTEMPLATE(command *&c, bundle &obj);
	int function_CA_NEWMODEL(command *&c, bundle &obj);
	int function_CA_SAVEMODEL(command *&c, bundle &obj);
	int function_CA_SETTITLE(command *&c, bundle &obj);
	// Pattern Commands
	int function_CA_CLEARPATTERNS(command *&c, bundle &obj);
	int function_CA_CREATEPATTERNS(command *&c, bundle &obj);
	int function_CA_GETPATTERN(command *&c, bundle &obj);
	int function_CA_LISTPATTERNS(command *&c, bundle &obj);
	int function_CA_NEWPATTERN(command *&c, bundle &obj);
	// Preferences Commands
	int function_CA_ATOMDETAIL(command *&c, bundle &obj);
	int function_CA_BONDDETAIL(command *&c, bundle &obj);
	int function_CA_COLOUR(command *&c, bundle &obj);
	int function_CA_DENSITYUNITS(command *&c, bundle &obj);
	int function_CA_ECUT(command *&c, bundle &obj);
	int function_CA_ELEC(command *&c, bundle &obj);
	int function_CA_ELEMENTAMBIENT(command *&c, bundle &obj);
	int function_CA_ELEMENTDIFFUSE(command *&c, bundle &obj);
	int function_CA_ELEMENTRADIUS(command *&c, bundle &obj);
	int function_CA_ENERGYUNITS(command *&c, bundle &obj);
	int function_CA_INTRA(command *&c, bundle &obj);
	int function_CA_GL(command *&c, bundle &obj);
	int function_CA_KEY(command *&c, bundle &obj);
	int function_CA_MOUSE(command *&c, bundle &obj);
	int function_CA_RADIUS(command *&c, bundle &obj);
	int function_CA_SHININESS(command *&c, bundle &obj);
	int function_CA_SHOW(command *&c, bundle &obj);
	int function_CA_STYLE(command *&c, bundle &obj);
	int function_CA_VCUT(command *&c, bundle &obj);
	int function_CA_VDW(command *&c, bundle &obj);
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
	// Script Commands
	int function_CA_LISTSCRIPTS(command *&c, bundle &obj);
	int function_CA_LOADSCRIPT(command *&c, bundle &obj);
	int function_CA_RUNSCRIPT(command *&c, bundle &obj);
	// Select Commands
	int function_CA_SELECTALL(command *&c, bundle &obj);
	int function_CA_SELECTATOM(command *&c, bundle &obj);
	int function_CA_SELECTELEMENT(command *&c, bundle &obj);
	int function_CA_SELECTFFTYPE(command *&c, bundle &obj);
	int function_CA_SELECTINVERT(command *&c, bundle &obj);
	int function_CA_SELECTNONE(command *&c, bundle &obj);
	int function_CA_SELECTOVERLAPS(command *&c, bundle &obj);
	int function_CA_SELECTPATTERN(command *&c, bundle &obj);
	int function_CA_SELECTTYPE(command *&c, bundle &obj);
	// Site Commands
	int function_CA_GETSITE(command *&c, bundle &obj);
	int function_CA_LISTSITES(command *&c, bundle &obj);
	int function_CA_NEWSITE(command *&c, bundle &obj);
	int function_CA_SETAXES(command *&c, bundle &obj);
	// System Commands
	int function_CA_GUI(command *&c, bundle &obj);
	int function_CA_SEED(command *&c, bundle &obj);
	int function_CA_HELP(command *&c, bundle &obj);
	int function_CA_QUIT(command *&c, bundle &obj);
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
	int function_CA_SPEEDTEST(command *&c, bundle &obj);
	int function_CA_TRANSLATEVIEW(command *&c, bundle &obj);
	int function_CA_ZOOMVIEW(command *&c, bundle &obj);
	int function_CA_ZROTATEVIEW(command *&c, bundle &obj);

	public:
	commandfunc function;
};

command_action CA_from_text(const char*);
 // External definitions
extern commanddata CA_data[CA_NITEMS];

#endif

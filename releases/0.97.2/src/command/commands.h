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
class MasterData;
class Command;
class CommandData;

// Function pointer typedef and call #define
typedef int (CommandData::*CommandFunction)(Command *&c, Bundle &obj);
#define CALL_COMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

// Function return values
enum CommandReturn { CR_SUCCESS, CR_SUCCESSNOMOVE, CR_FAIL, CR_FAILCONTINUE, CR_EXIT, CR_EXITWITHERROR };

// Command actions
enum CommandAction {

	// Variable declaration
	CA_CHAR,
	CA_INT,
	CA_FLOAT,
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
	CA_COPY,
	CA_CUT,
	CA_DELETE,
	CA_LOCATE,
	CA_MOVE,
	CA_PASTE,
	CA_ROTX,
	CA_ROTY,
	CA_ROTZ,
	CA_SHIFTDOWN,
	CA_SHIFTUP,
	CA_TOEND,
	CA_TOSTART,
	CA_TRANSMUTE,

	// Cell commands
	CA_FOLD,
	CA_FOLDMOLECULES,
	CA_FRACTOREAL,
	CA_PACK,
	CA_PRINTCELL,
	CA_REPLICATE,
	CA_SCALE,
	CA_CELL,
	CA_CELLAXES,
	CA_SPACEGROUP,

	// Charge commands
	CA_CHARGEFF,
	CA_CHARGEFROMMODEL,
	CA_CHARGEPATOM,
	CA_CHARGESELECTION,
	CA_CHARGETYPE,
	CA_CLEARCHARGES,

	// Colourscale commands
	CA_LISTSCALES,
	CA_SCALEMAXCOLOUR,
	CA_SCALEMIDCOLOUR,
	CA_SCALEMIDPOINT,
	CA_SCALEMINCOLOUR,
	CA_SCALERANGE,
	CA_SCALETYPE,

	// Disordered build commands
	CA_DISORDER,
	CA_LISTCOMPONENTS,
	CA_NMOLS,
	CA_REGIONCENTRE,
	CA_REGIONGEOMETRY,
	CA_REGIONOVERLAPS,
	CA_REGIONSHAPE,
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
	CA_CLEARMAP,
	CA_DEFAULTFF,
	CA_FFMODEL,
	CA_FFPATTERN,
	CA_FFPATTERNID,
	CA_GETFF,
	CA_LOADFF,
	CA_MAP,
	CA_TYPEMODEL,
	CA_TYPETEST,

	// Glyph commands
	CA_GLYPHATOMF,
	CA_GLYPHATOMR,
	CA_GLYPHATOMV,
	CA_GLYPHATOMSF,
	CA_GLYPHATOMSR,
	CA_GLYPHATOMSV,
	CA_GLYPHDATA,
	CA_GLYPHSOLID,
	CA_GLYPHTEXT,
	CA_NEWGLYPH,

	// Grid Commands
	CA_ADDGRIDPOINT,
	CA_ADDNEXTGRIDPOINT,
	CA_FINALISEGRID,
	CA_GRIDAXES,
	CA_GRIDCOLOUR,
	CA_GRIDCOLOURNEGATIVE,
	CA_GRIDCOLOURSCALE,
	CA_GRIDCUBIC,
	CA_GRIDCUTOFF,
	CA_GRIDLOOPORDER,
	CA_GRIDORIGIN,
	CA_GRIDORTHO,
	CA_GRIDSIZE,
	CA_GRIDSYMMETRIC,
	CA_GRIDTRANSPARENCY,
	CA_NEWGRID,

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
	CA_LOGINFO,
	CA_MODELTEMPLATE,
	CA_NAME,
	CA_NEWMODEL,
	CA_NEXTMODEL,
	CA_PREVMODEL,
	CA_SAVEMODEL,
	CA_TITLE,

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
	CA_REPLICATEFOLD,
	CA_REPLICATETRIM,
	CA_SHININESS,
	CA_SHOW,
	CA_STYLE,
	CA_VCUT,
	CA_VDW,

	// Read / Write Commands
	CA_ADDREADOPTION,
	CA_FIND,
	CA_READCHARS,
	CA_READFLOAT,
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
	CA_DESELECT,
	CA_INVERT,
	CA_SELECT,
	CA_SELECTALL,
	CA_SELECTFFTYPE,
	CA_SELECTNONE,
	CA_SELECTOVERLAPS,
	CA_SELECTPATTERN,
	CA_SELECTTYPE,
	
	// Site Commands
	CA_GETSITE,
	CA_LISTSITES,
	CA_NEWSITE,
	CA_SITEAXES,

	// System commands
	CA_DEBUG,
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
	CA_TRANSLATECELL,
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

class CommandData
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
	const char *argText;
	// Command syntax
	const char *syntax;
	// Return whether command accepts any arguments
	bool hasArguments() { return (!(arguments[0] == '\0')); }

	/*
	// Function
	*/
	private:
	// Provide full access to the master
	friend class Master;
	// All command functions
	int function_CA_ROOTNODE(Command *&c, Bundle &obj);
	// Analyse commands
	int function_CA_FINALISE(Command *&c, Bundle &obj);
	int function_CA_FRAMEANALYSE(Command *&c, Bundle &obj);
	int function_CA_GEOMETRY(Command *&c, Bundle &obj);	
	int function_CA_MODELANALYSE(Command *&c, Bundle &obj);
	int function_CA_PDENS(Command *&c, Bundle &obj);
	int function_CA_PRINTJOBS(Command *&c, Bundle &obj);
	int function_CA_RDF(Command *&c, Bundle &obj);
	int function_CA_SAVEQUANTITIES(Command *&c, Bundle &obj);
	int function_CA_TRAJANALYSE(Command *&c, Bundle &obj);
	// Atom Commands
	int function_CA_CHAIN(Command *&c, Bundle &obj);
	int function_CA_ENDCHAIN(Command *&c, Bundle &obj);
	int function_CA_NEWATOM(Command *&c, Bundle &obj);
	int function_CA_NEWATOMFRAC(Command *&c, Bundle &obj);
	int function_CA_SETCOORDS(Command *&c, Bundle &obj);
	int function_CA_SETCHARGE(Command *&c, Bundle &obj);
	int function_CA_SETELEMENT(Command *&c, Bundle &obj);
	int function_CA_SETFORCES(Command *&c, Bundle &obj);
	int function_CA_SETFX(Command *&c, Bundle &obj);
	int function_CA_SETFY(Command *&c, Bundle &obj);
	int function_CA_SETFZ(Command *&c, Bundle &obj);
	int function_CA_SETID(Command *&c, Bundle &obj);
	int function_CA_SETRX(Command *&c, Bundle &obj);
	int function_CA_SETRY(Command *&c, Bundle &obj);
	int function_CA_SETRZ(Command *&c, Bundle &obj);
	int function_CA_SETVELOCITIES(Command *&c, Bundle &obj);
	int function_CA_SETVX(Command *&c, Bundle &obj);
	int function_CA_SETVY(Command *&c, Bundle &obj);
	int function_CA_SETVZ(Command *&c, Bundle &obj);
	// Bond commands
	int function_CA_AUGMENT(Command *&c, Bundle &obj);
	int function_CA_BONDTOLERANCE(Command *&c, Bundle &obj);
	int function_CA_CLEARBONDS(Command *&c, Bundle &obj);
	int function_CA_NEWBOND(Command *&c, Bundle &obj);
	int function_CA_NEWBONDID(Command *&c, Bundle &obj);
	int function_CA_REBONDPATTERNS(Command *&c, Bundle &obj);
	int function_CA_REBONDSELECTION(Command *&c, Bundle &obj);
	int function_CA_REBOND(Command *&c, Bundle &obj);
	// Build commands
	int function_CA_ADDHYDROGEN(Command *&c, Bundle &obj);
	int function_CA_COPY(Command *&c, Bundle &obj);
	int function_CA_CUT(Command *&c, Bundle &obj);
	int function_CA_DELETE(Command *&c, Bundle &obj);
	int function_CA_LOCATE(Command *&c, Bundle &obj);
	int function_CA_MOVE(Command *&c, Bundle &obj);
	int function_CA_PASTE(Command *&c, Bundle &obj);
	int function_CA_ROTX(Command *&c, Bundle &obj);
	int function_CA_ROTY(Command *&c, Bundle &obj);
	int function_CA_ROTZ(Command *&c, Bundle &obj);
	int function_CA_SHIFTDOWN(Command *&c, Bundle &obj);
	int function_CA_SHIFTUP(Command *&c, Bundle &obj);
	int function_CA_TOEND(Command *&c, Bundle &obj);
	int function_CA_TOSTART(Command *&c, Bundle &obj);
	int function_CA_TRANSMUTE(Command *&c, Bundle &obj);
	// Cell commands
	int function_CA_FOLD(Command *&c, Bundle &obj);
	int function_CA_FOLDMOLECULES(Command *&c, Bundle &obj);
	int function_CA_FRACTOREAL(Command *&c, Bundle &obj);
	int function_CA_PACK(Command *&c, Bundle &obj);
	int function_CA_PRINTCELL(Command *&c, Bundle &obj);
	int function_CA_REPLICATE(Command *&c, Bundle &obj);
	int function_CA_SCALE(Command *&c, Bundle &obj);
	int function_CA_CELL(Command *&c, Bundle &obj);
	int function_CA_CELLAXES(Command *&c, Bundle &obj);
	int function_CA_SPACEGROUP(Command *&c, Bundle &obj);
	// Charge commands
	int function_CA_CHARGEFF(Command *&c, Bundle &obj);
	int function_CA_CHARGEFROMMODEL(Command *&c, Bundle &obj);
	int function_CA_CHARGEPATOM(Command *&c, Bundle &obj);
	int function_CA_CHARGESELECTION(Command *&c, Bundle &obj);
	int function_CA_CHARGETYPE(Command *&c, Bundle &obj);
	int function_CA_CLEARCHARGES(Command *&c, Bundle &obj);
	// Colourscale commands
	int function_CA_LISTSCALES(Command *&c, Bundle &obj);
	int function_CA_SCALEMAXCOLOUR(Command *&c, Bundle &obj);
	int function_CA_SCALEMIDCOLOUR(Command *&c, Bundle &obj);
	int function_CA_SCALEMIDPOINT(Command *&c, Bundle &obj);
	int function_CA_SCALEMINCOLOUR(Command *&c, Bundle &obj);
	int function_CA_SCALERANGE(Command *&c, Bundle &obj);
	int function_CA_SCALETYPE(Command *&c, Bundle &obj);
	// Disordered build commands
	int function_CA_DISORDER(Command *&c, Bundle &obj);
	int function_CA_LISTCOMPONENTS(Command *&c, Bundle &obj);
	int function_CA_NMOLS(Command *&c, Bundle &obj);
	int function_CA_REGIONCENTRE(Command *&c, Bundle &obj);
	int function_CA_REGIONGEOMETRY(Command *&c, Bundle &obj);
	int function_CA_REGIONOVERLAPS(Command *&c, Bundle &obj);
	int function_CA_REGIONSHAPE(Command *&c, Bundle &obj);
	int function_CA_VDWSCALE(Command *&c, Bundle &obj);
	// Energy Commands
	int function_CA_FRAMEENERGY(Command *&c, Bundle &obj);
	int function_CA_MODELENERGY(Command *&c, Bundle &obj);
	int function_CA_PRINTELEC(Command *&c, Bundle &obj);
	int function_CA_PRINTEWALD(Command *&c, Bundle &obj);
	int function_CA_PRINTINTER(Command *&c, Bundle &obj);
	int function_CA_PRINTINTRA(Command *&c, Bundle &obj);
	int function_CA_PRINTENERGY(Command *&c, Bundle &obj);
	int function_CA_PRINTSUMMARY(Command *&c, Bundle &obj);
	int function_CA_PRINTVDW(Command *&c, Bundle &obj);
	// Expression Commands
	int function_CA_CREATEEXPRESSION(Command *&c, Bundle &obj);
	int function_CA_PRINTSETUP(Command *&c, Bundle &obj);
	int function_CA_SAVEEXPRESSION(Command *&c, Bundle &obj);
	// Flow control
	int function_CA_ELSE(Command *&c, Bundle &obj);
	int function_CA_ELSEIF(Command *&c, Bundle &obj);
	int function_CA_END(Command *&c, Bundle &obj);
	int function_CA_FOR(Command *&c, Bundle &obj);
	int function_CA_GOTO(Command *&c, Bundle &obj);
	int function_CA_GOTONONIF(Command *&c, Bundle &obj);
	int function_CA_IF(Command *&c, Bundle &obj);
	int function_CA_TERMINATE(Command *&c, Bundle &obj);
	// Force Commands
	int function_CA_FRAMEFORCES(Command *&c, Bundle &obj);
	int function_CA_MODELFORCES(Command *&c, Bundle &obj);
	int function_CA_PRINTFORCES(Command *&c, Bundle &obj);
	// Forcefield Commands
	int function_CA_CLEARMAP(Command *&c, Bundle &obj);
	int function_CA_DEFAULTFF(Command *&c, Bundle &obj);
	int function_CA_FFMODEL(Command *&c, Bundle &obj);
	int function_CA_FFPATTERN(Command *&c, Bundle &obj);
	int function_CA_FFPATTERNID(Command *&c, Bundle &obj);
	int function_CA_GETFF(Command *&c, Bundle &obj);
	int function_CA_LOADFF(Command *&c, Bundle &obj);
	int function_CA_MAP(Command *&c, Bundle &obj);
	int function_CA_TYPEMODEL(Command *&c, Bundle &obj);
	int function_CA_TYPETEST(Command *&c, Bundle &obj);
	// Glyph commands
	int function_CA_GLYPHATOMF(Command *&c, Bundle &obj);
	int function_CA_GLYPHATOMR(Command *&c, Bundle &obj);
	int function_CA_GLYPHATOMV(Command *&c, Bundle &obj);
	int function_CA_GLYPHATOMSF(Command *&c, Bundle &obj);
	int function_CA_GLYPHATOMSR(Command *&c, Bundle &obj);
	int function_CA_GLYPHATOMSV(Command *&c, Bundle &obj);
	int function_CA_GLYPHDATA(Command *&c, Bundle &obj);
	int function_CA_GLYPHSOLID(Command *&c, Bundle &obj);
	int function_CA_GLYPHTEXT(Command *&c, Bundle &obj);
	int function_CA_NEWGLYPH(Command *&c, Bundle &obj);
	// Grid Commands
	int function_CA_ADDGRIDPOINT(Command *&c, Bundle &obj);
	int function_CA_ADDNEXTGRIDPOINT(Command *&c, Bundle &obj);
	int function_CA_FINALISEGRID(Command *&c, Bundle &obj);
	int function_CA_GRIDAXES(Command *&c, Bundle &obj);
	int function_CA_GRIDCOLOUR(Command *&c, Bundle &obj);
	int function_CA_GRIDCOLOURNEGATIVE(Command *&c, Bundle &obj);
	int function_CA_GRIDCOLOURSCALE(Command *&c, Bundle &obj);
	int function_CA_GRIDCUBIC(Command *&c, Bundle &obj);
	int function_CA_GRIDCUTOFF(Command *&c, Bundle &obj);
	int function_CA_GRIDORTHO(Command *&c, Bundle &obj);
	int function_CA_GRIDLOOPORDER(Command *&c, Bundle &obj);
	int function_CA_GRIDORIGIN(Command *&c, Bundle &obj);
	int function_CA_GRIDSIZE(Command *&c, Bundle &obj);
	int function_CA_GRIDSYMMETRIC(Command *&c, Bundle &obj);
	int function_CA_GRIDTRANSPARENCY(Command *&c, Bundle &obj);
	int function_CA_NEWGRID(Command *&c, Bundle &obj);
	// Image Commands
	int function_CA_SAVEBITMAP(Command *&c, Bundle &obj);
	int function_CA_SAVEVECTOR(Command *&c, Bundle &obj);
	// Labeling commands
	int function_CA_CLEARLABELS(Command *&c, Bundle &obj);
	int function_CA_LABEL(Command *&c, Bundle &obj);
	int function_CA_REMOVELABEL(Command *&c, Bundle &obj);
	// MC Commands
	int function_CA_MCACCEPT(Command *&c, Bundle &obj);
	int function_CA_MCALLOW(Command *&c, Bundle &obj);
	int function_CA_MCMAXSTEP(Command *&c, Bundle &obj);
	int function_CA_MCNTRIALS(Command *&c, Bundle &obj);
	int function_CA_PRINTMC(Command *&c, Bundle &obj);
	// Messaging
	int function_CA_ERROR(Command *&c, Bundle &obj);
	int function_CA_PRINT(Command *&c, Bundle &obj);
	int function_CA_VERBOSE(Command *&c, Bundle &obj);
	int function_CA_WARN(Command *&c, Bundle &obj);
	// Minimisation Commands
	int function_CA_CGMINIMISE(Command *&c, Bundle &obj);
	int function_CA_CONVERGE(Command *&c, Bundle &obj);
	int function_CA_LINETOL(Command *&c, Bundle &obj);
	int function_CA_MCMINIMISE(Command *&c, Bundle &obj);
	int function_CA_SDMINIMISE(Command *&c, Bundle &obj);
	int function_CA_SIMPLEXMINIMISE(Command *&c, Bundle &obj);
	// Model Commands
	int function_CA_CREATEATOMS(Command *&c, Bundle &obj);
	int function_CA_FINALISEMODEL(Command *&c, Bundle &obj);
	int function_CA_GETMODEL(Command *&c, Bundle &obj);
	int function_CA_INFO(Command *&c, Bundle &obj);
	int function_CA_LISTMODELS(Command *&c, Bundle &obj);
	int function_CA_LOADMODEL(Command *&c, Bundle &obj);
	int function_CA_LOGINFO(Command *&c, Bundle &obj);
	int function_CA_MODELTEMPLATE(Command *&c, Bundle &obj);
	int function_CA_NAME(Command *&c, Bundle &obj);
	int function_CA_NEWMODEL(Command *&c, Bundle &obj);
	int function_CA_NEXTMODEL(Command *&c, Bundle &obj);
	int function_CA_PREVMODEL(Command *&c, Bundle &obj);
	int function_CA_SAVEMODEL(Command *&c, Bundle &obj);
	int function_CA_TITLE(Command *&c, Bundle &obj);
	// Pattern Commands
	int function_CA_CLEARPATTERNS(Command *&c, Bundle &obj);
	int function_CA_CREATEPATTERNS(Command *&c, Bundle &obj);
	int function_CA_GETPATTERN(Command *&c, Bundle &obj);
	int function_CA_LISTPATTERNS(Command *&c, Bundle &obj);
	int function_CA_NEWPATTERN(Command *&c, Bundle &obj);
	// Preferences Commands
	int function_CA_ATOMDETAIL(Command *&c, Bundle &obj);
	int function_CA_BONDDETAIL(Command *&c, Bundle &obj);
	int function_CA_COLOUR(Command *&c, Bundle &obj);
	int function_CA_DENSITYUNITS(Command *&c, Bundle &obj);
	int function_CA_ECUT(Command *&c, Bundle &obj);
	int function_CA_ELEC(Command *&c, Bundle &obj);
	int function_CA_ELEMENTAMBIENT(Command *&c, Bundle &obj);
	int function_CA_ELEMENTDIFFUSE(Command *&c, Bundle &obj);
	int function_CA_ELEMENTRADIUS(Command *&c, Bundle &obj);
	int function_CA_ENERGYUNITS(Command *&c, Bundle &obj);
	int function_CA_INTRA(Command *&c, Bundle &obj);
	int function_CA_GL(Command *&c, Bundle &obj);
	int function_CA_KEY(Command *&c, Bundle &obj);
	int function_CA_MOUSE(Command *&c, Bundle &obj);
	int function_CA_RADIUS(Command *&c, Bundle &obj);
	int function_CA_REPLICATEFOLD(Command *&c, Bundle &obj);
	int function_CA_REPLICATETRIM(Command *&c, Bundle &obj);
	int function_CA_SHININESS(Command *&c, Bundle &obj);
	int function_CA_SHOW(Command *&c, Bundle &obj);
	int function_CA_STYLE(Command *&c, Bundle &obj);
	int function_CA_VCUT(Command *&c, Bundle &obj);
	int function_CA_VDW(Command *&c, Bundle &obj);
	// Read / Write Commands
	int function_CA_ADDREADOPTION(Command *&c, Bundle &obj);
	int function_CA_FIND(Command *&c, Bundle &obj);
	int function_CA_READCHARS(Command *&c, Bundle &obj);
	int function_CA_READFLOAT(Command *&c, Bundle &obj);
	int function_CA_READINTEGER(Command *&c, Bundle &obj);
	int function_CA_READLINE(Command *&c, Bundle &obj);
	int function_CA_READNEXT(Command *&c, Bundle &obj);
	int function_CA_READVAR(Command *&c, Bundle &obj);
	int function_CA_REMOVEREADOPTION(Command *&c, Bundle &obj);
	int function_CA_REWIND(Command *&c, Bundle &obj);
	int function_CA_SKIPCHARS(Command *&c, Bundle &obj);
	int function_CA_SKIPLINE(Command *&c, Bundle &obj);
	int function_CA_WRITELINE(Command *&c, Bundle &obj);
	// Script Commands
	int function_CA_LISTSCRIPTS(Command *&c, Bundle &obj);
	int function_CA_LOADSCRIPT(Command *&c, Bundle &obj);
	int function_CA_RUNSCRIPT(Command *&c, Bundle &obj);
	// Select Commands
	int function_CA_DESELECT(Command *&c, Bundle &obj);
	int function_CA_INVERT(Command *&c, Bundle &obj);
	int function_CA_SELECT(Command *&c, Bundle &obj);
	int function_CA_SELECTALL(Command *&c, Bundle &obj);
	int function_CA_SELECTFFTYPE(Command *&c, Bundle &obj);
	int function_CA_SELECTNONE(Command *&c, Bundle &obj);
	int function_CA_SELECTOVERLAPS(Command *&c, Bundle &obj);
	int function_CA_SELECTPATTERN(Command *&c, Bundle &obj);
	int function_CA_SELECTTYPE(Command *&c, Bundle &obj);
	// Site Commands
	int function_CA_GETSITE(Command *&c, Bundle &obj);
	int function_CA_LISTSITES(Command *&c, Bundle &obj);
	int function_CA_NEWSITE(Command *&c, Bundle &obj);
	int function_CA_SITEAXES(Command *&c, Bundle &obj);
	// System Commands
	int function_CA_DEBUG(Command *&c, Bundle &obj);
	int function_CA_GUI(Command *&c, Bundle &obj);
	int function_CA_SEED(Command *&c, Bundle &obj);
	int function_CA_HELP(Command *&c, Bundle &obj);
	int function_CA_QUIT(Command *&c, Bundle &obj);
	// Trajectory Commands
	int function_CA_FIRSTFRAME(Command *&c, Bundle &obj);
	int function_CA_LASTFRAME(Command *&c, Bundle &obj);
	int function_CA_LOADTRAJECTORY(Command *&c, Bundle &obj);
	int function_CA_NEXTFRAME(Command *&c, Bundle &obj);
	int function_CA_PREVFRAME(Command *&c, Bundle &obj);
	// Transform Commands
	int function_CA_CENTRE(Command *&c, Bundle &obj);
	int function_CA_TRANSLATE(Command *&c, Bundle &obj);
	int function_CA_TRANSLATEATOM(Command *&c, Bundle &obj);
	int function_CA_TRANSLATECELL(Command *&c, Bundle &obj);
	int function_CA_MIRROR(Command *&c, Bundle &obj);
	// Variables
	int function_CA_LET(Command *&c, Bundle &obj);
	int function_CA_INCREASE(Command *&c, Bundle &obj);
	int function_CA_DECREASE(Command *&c, Bundle &obj);
	int function_CA_EVAL(Command *&c, Bundle &obj);
	// View
	int function_CA_RESETVIEW(Command *&c, Bundle &obj);
	int function_CA_ROTATEVIEW(Command *&c, Bundle &obj);
	int function_CA_SPEEDTEST(Command *&c, Bundle &obj);
	int function_CA_TRANSLATEVIEW(Command *&c, Bundle &obj);
	int function_CA_VIEWALONG(Command *&c, Bundle &obj);
	int function_CA_VIEWALONGCELL(Command *&c, Bundle &obj);
	int function_CA_ZOOMVIEW(Command *&c, Bundle &obj);
	int function_CA_ZROTATEVIEW(Command *&c, Bundle &obj);

	public:
	CommandFunction function;
};

CommandAction CA_from_text(const char*);
 // External definitions
extern CommandData CA_data[CA_NITEMS];

#endif

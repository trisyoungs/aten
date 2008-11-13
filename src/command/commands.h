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

#include "base/bundle.h"

// Forward declarations
class CommandList;
class CommandNode;
class CommandData;
class Command;

// Function pointer typedef and call #define
typedef int (*CommandFunction)(CommandNode *&c, Bundle &obj);
#define CALL_COMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

class CommandData
{
	public:
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
	bool hasArguments();
};

// Command actions
class Command {

	public:
	// Constructor / Destructor
	Command();
	~Command();

	// Command return values
	enum ReturnValue { Success, SuccessNoMove, Fail, FailContinue, Exit, ExitWithError };
	// Command list
	enum Function {
	
		// Variable declaration
		CA_CHAR,
		CA_INT,
		CA_REAL,
		CA_ATOM,
		CA_BOND,
		CA_PATTERN,
		CA_PATTERNBOUND,
		CA_MODEL,
		CA_GRID,
		CA_FFATOM,
		CA_FFBOUND,
		CA_CELLVAR,
		CA_FORCEFIELD,
		CA_PREFSVAR,
	
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
		CA_ATOMSTYLE,
		CA_GETATOM,
		CA_HIDE,
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
		CA_SHOW,
		CA_SHOWALL,
	
		// Bond commands
		CA_AUGMENT,
		CA_BONDTOLERANCE,
		CA_CLEARBONDS,
		CA_CLEARSELECTEDBONDS,
		CA_NEWBOND,
		CA_NEWBONDID,
		CA_REBOND,
		CA_REBONDPATTERNS,
		CA_REBONDSELECTION,
	
		// Build commands
		CA_ADDHYDROGEN,
		CA_BOHR,
		CA_CHAIN,
		CA_ENDCHAIN,
		CA_LOCATE,
		CA_MOVE,
		CA_NEWATOM,
		CA_NEWATOMFRAC,
		CA_RESETPEN,
		CA_ROTX,
		CA_ROTY,
		CA_ROTZ,
		CA_SHIFTDOWN,
		CA_SHIFTUP,
		CA_TOEND,
		CA_TOSTART,
		CA_TRANSMUTE,
	
		// Cell commands
		CA_ADDGENERATOR,
		CA_ADJUSTCELL,
		CA_CELL,
		CA_CELLAXES,
		CA_FOLD,
		CA_FOLDMOLECULES,
		CA_FRACTOREAL,
		CA_NOCELL,
		CA_PACK,
		CA_PRINTCELL,
		CA_REPLICATE,
		CA_SCALE,
		CA_SCALEMOLECULES,
		CA_SETCELL,
		CA_SPACEGROUP,
	
		// Charge commands
		CA_CHARGEFF,
		CA_CHARGEFROMMODEL,
		CA_CHARGEPATOM,
		CA_CHARGE,
		CA_CHARGETYPE,
		CA_CLEARCHARGES,
	
		// Colourscale commands
		CA_ADDPOINT,
		CA_CLEARPOINTS,
		CA_LISTSCALES,
		CA_REMOVEPOINT,
		CA_SCALENAME,
		CA_SCALEVISIBLE,
		CA_SETPOINT,
		CA_SETPOINTCOLOUR,
		CA_SETPOINTVALUE,
	
		// Disordered build commands
		CA_DISORDER,
		CA_LISTCOMPONENTS,
		CA_NMOLS,
		CA_REGION,
		CA_REGIONCENTRE,
		CA_REGIONCENTREF,
		CA_REGIONF,
		CA_REGIONGEOMETRY,
		CA_REGIONGEOMETRYF,
		CA_REGIONOVERLAPS,
		CA_REGIONSHAPE,
		CA_VDWSCALE,
	
		// Edit commands
		CA_COPY,
		CA_CUT,
		CA_DELETE,
		CA_PASTE,
		CA_REDO,
		CA_UNDO,
	
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
	
		// Flow control
		CA_BREAK,
		CA_CONTINUE,
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
	
		// Forcefield/Expression Commands
		CA_ANGLEDEF,
		CA_BONDDEF,
		CA_CLEARMAP,
		CA_CREATEEXPRESSION,
		CA_DEFAULTFF,
		CA_EQUIVALENT,
		CA_FFMODEL,
		CA_FFPATTERN,
		CA_FFPATTERNID,
		CA_FINALISEFF,
		CA_GENCONVERT,
		CA_GENERATOR,
		CA_GETFF,
		CA_LOADFF,
		CA_MAP,
		CA_NEWFF,
		CA_PRINTSETUP,
		CA_RULES,
		CA_SAVEEXPRESSION,
		CA_TORSIONDEF,
		CA_TYPEDEF,
		CA_TYPEMODEL,
		CA_TYPETEST,
		CA_UNITS,
		CA_VDWDEF,
	
		// Glyph commands
		CA_AUTOELLIPSOIDS,
		CA_AUTOPOLYHEDRA,
		CA_GLYPHATOMF,
		CA_GLYPHATOMR,
		CA_GLYPHATOMV,
		CA_GLYPHATOMSF,
		CA_GLYPHATOMSR,
		CA_GLYPHATOMSV,
		CA_GLYPHCOLOUR,
		CA_GLYPHDATA,
		CA_GLYPHSOLID,
		CA_GLYPHTEXT,
		CA_NEWGLYPH,
	
		// Grid Commands
		CA_ADDGRIDPOINT,
		CA_ADDNEXTGRIDPOINT,
		CA_FINALISEGRID,
		CA_GETGRID,
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
		CA_GRIDSTYLE,
		CA_GRIDSYMMETRIC,
		CA_GRIDTRANSPARENCY,
		CA_GRIDUSEZ,
		CA_LOADGRID,
		CA_NEWGRID,
	
		// Image Commands
		CA_SAVEBITMAP,
		CA_SAVEVECTOR,
	
		// Labeling commands
		CA_CLEARLABELS,
		CA_LABEL,
		CA_REMOVELABEL,
		CA_REMOVELABELS,
	
		// MC Commands
		CA_MCACCEPT,
		CA_MCALLOW,
		CA_MCMAXSTEP,
		CA_MCNTRIALS,
		CA_PRINTMC,
	
		// Measurements
		CA_ANGLE,
		CA_ANGLES,
		CA_CLEARMEASUREMENTS,
		CA_DISTANCE,
		CA_DISTANCES,
		CA_LISTMEASUREMENTS,
		CA_MEASURE,
		CA_TORSION,
		CA_TORSIONS,
	
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
		CA_NEWMODEL,
		CA_NEXTMODEL,
		CA_PREVMODEL,
		CA_SAVEMODEL,
		CA_SETNAME,
	
		// Pattern Commands
		CA_CLEARPATTERNS,
		CA_CREATEPATTERNS,
		CA_GETPATTERN,
		CA_LISTPATTERNS,
		CA_NEWPATTERN,
	
		// Preferences Commands
		CA_ANGLELABEL,
		CA_ATOMDETAIL,
		CA_BONDDETAIL,
		CA_COLOUR,
		CA_COMMONELEMENTS,
		CA_DENSITYUNITS,
		CA_DISTANCELABEL,
		CA_ECUT,
		CA_ELEC,
		CA_ELEMENTAMBIENT,
		CA_ELEMENTDIFFUSE,
		CA_ELEMENTRADIUS,
		CA_ENERGYUNITS,
		CA_GL,
		CA_HDISTANCE,
		CA_INTRA,
		CA_KEY,
		CA_LABELSIZE,
		CA_LIGHT,
		CA_LIGHTAMBIENT,
		CA_LIGHTDIFFUSE,
		CA_LIGHTPOSITION,
		CA_LIGHTSPECULAR,
		CA_MOUSE,
		CA_RADIUS,
		CA_REPLICATEFOLD,
		CA_REPLICATETRIM,
		CA_SCHEME,
		CA_SHININESS,
		CA_SHOWONSCREEN,
		CA_SHOWONIMAGE,
		CA_STYLE,
		CA_USENICETEXT,
		CA_VCUT,
		CA_VDW,
	
		// Read / Write Commands
		CA_ADDREADOPTION,
		CA_FIND,
		CA_GETLINE,
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
		CA_WRITEVAR,
	
		// Script Commands
		CA_LISTSCRIPTS,
		CA_LOADSCRIPT,
		CA_RUNSCRIPT,
	
		// Select Commands
		CA_DESELECT,
		CA_EXPAND,
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
		CA_VERSION,

		// Trajectory Commands
		CA_FINALISEFRAME,
		CA_FIRSTFRAME,
		CA_LASTFRAME,
		CA_LOADTRAJECTORY,
		CA_NEXTFRAME,
		CA_PREVFRAME,
		CA_SEEKFRAME,
	
		// Transformation Commands
		CA_CENTRE,
		CA_TRANSLATE,
		CA_TRANSLATEATOM,
		CA_TRANSLATECELL,
		CA_MIRROR,
	
		// Variables
		CA_DECREASE,
		CA_INCREASE,
		CA_LET,
		CA_LETCHAR,
		CA_LETPTR,
	
		// View
		CA_GETVIEW,
		CA_ORTHOGRAPHIC,
		CA_PERSPECTIVE,
		CA_RESETVIEW,
		CA_ROTATEVIEW,
		CA_SETVIEW,
		CA_SPEEDTEST,
		CA_TRANSLATEVIEW,
		CA_VIEWALONG,
		CA_VIEWALONGCELL,
		CA_ZOOMVIEW,
		CA_ZROTATEVIEW,
	
		CA_NITEMS
	};
	// Return enumerated command id from string
	Command::Function command(const char*);

	/*
	// Function declarations
	*/
	private:
	// All command functions
	static int function_CA_ROOTNODE(CommandNode *&c, Bundle &obj);
	// Analyse commands
	static int function_CA_FINALISE(CommandNode *&c, Bundle &obj);
	static int function_CA_FRAMEANALYSE(CommandNode *&c, Bundle &obj);
	static int function_CA_GEOMETRY(CommandNode *&c, Bundle &obj);	
	static int function_CA_MODELANALYSE(CommandNode *&c, Bundle &obj);
	static int function_CA_PDENS(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTJOBS(CommandNode *&c, Bundle &obj);
	static int function_CA_RDF(CommandNode *&c, Bundle &obj);
	static int function_CA_SAVEQUANTITIES(CommandNode *&c, Bundle &obj);
	static int function_CA_TRAJANALYSE(CommandNode *&c, Bundle &obj);
	// Atom Commands
	static int function_CA_ATOMSTYLE(CommandNode *&c, Bundle &obj);
	static int function_CA_GETATOM(CommandNode *&c, Bundle &obj);
	static int function_CA_HIDE(CommandNode *&c, Bundle &obj);
	static int function_CA_SETCOORDS(CommandNode *&c, Bundle &obj);
	static int function_CA_SETCHARGE(CommandNode *&c, Bundle &obj);
	static int function_CA_SETELEMENT(CommandNode *&c, Bundle &obj);
	static int function_CA_SETFORCES(CommandNode *&c, Bundle &obj);
	static int function_CA_SETFX(CommandNode *&c, Bundle &obj);
	static int function_CA_SETFY(CommandNode *&c, Bundle &obj);
	static int function_CA_SETFZ(CommandNode *&c, Bundle &obj);
	static int function_CA_SETID(CommandNode *&c, Bundle &obj);
	static int function_CA_SETRX(CommandNode *&c, Bundle &obj);
	static int function_CA_SETRY(CommandNode *&c, Bundle &obj);
	static int function_CA_SETRZ(CommandNode *&c, Bundle &obj);
	static int function_CA_SETVELOCITIES(CommandNode *&c, Bundle &obj);
	static int function_CA_SETVX(CommandNode *&c, Bundle &obj);
	static int function_CA_SETVY(CommandNode *&c, Bundle &obj);
	static int function_CA_SETVZ(CommandNode *&c, Bundle &obj);
	static int function_CA_SHOW(CommandNode *&c, Bundle &obj);
	static int function_CA_SHOWALL(CommandNode *&c, Bundle &obj);
	// Bond commands
	static int function_CA_AUGMENT(CommandNode *&c, Bundle &obj);
	static int function_CA_BONDTOLERANCE(CommandNode *&c, Bundle &obj);
	static int function_CA_CLEARBONDS(CommandNode *&c, Bundle &obj);
	static int function_CA_CLEARSELECTEDBONDS(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWBOND(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWBONDID(CommandNode *&c, Bundle &obj);
	static int function_CA_REBOND(CommandNode *&c, Bundle &obj);
	static int function_CA_REBONDPATTERNS(CommandNode *&c, Bundle &obj);
	static int function_CA_REBONDSELECTION(CommandNode *&c, Bundle &obj);
	// Build commands
	static int function_CA_ADDHYDROGEN(CommandNode *&c, Bundle &obj);
	static int function_CA_BOHR(CommandNode *&c, Bundle &obj);
	static int function_CA_CHAIN(CommandNode *&c, Bundle &obj);
	static int function_CA_ENDCHAIN(CommandNode *&c, Bundle &obj);
	static int function_CA_LOCATE(CommandNode *&c, Bundle &obj);
	static int function_CA_MOVE(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWATOM(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWATOMFRAC(CommandNode *&c, Bundle &obj);
	static int function_CA_RESETPEN(CommandNode *&c, Bundle &obj);
	static int function_CA_ROTX(CommandNode *&c, Bundle &obj);
	static int function_CA_ROTY(CommandNode *&c, Bundle &obj);
	static int function_CA_ROTZ(CommandNode *&c, Bundle &obj);
	static int function_CA_SHIFTDOWN(CommandNode *&c, Bundle &obj);
	static int function_CA_SHIFTUP(CommandNode *&c, Bundle &obj);
	static int function_CA_TOEND(CommandNode *&c, Bundle &obj);
	static int function_CA_TOSTART(CommandNode *&c, Bundle &obj);
	static int function_CA_TRANSMUTE(CommandNode *&c, Bundle &obj);
	// Cell commands
	static int function_CA_ADDGENERATOR(CommandNode *&c, Bundle &obj);
	static int function_CA_ADJUSTCELL(CommandNode *&c, Bundle &obj);
	static int function_CA_CELL(CommandNode *&c, Bundle &obj);
	static int function_CA_CELLAXES(CommandNode *&c, Bundle &obj);
	static int function_CA_FOLD(CommandNode *&c, Bundle &obj);
	static int function_CA_FOLDMOLECULES(CommandNode *&c, Bundle &obj);
	static int function_CA_FRACTOREAL(CommandNode *&c, Bundle &obj);
	static int function_CA_NOCELL(CommandNode *&c, Bundle &obj);
	static int function_CA_PACK(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTCELL(CommandNode *&c, Bundle &obj);
	static int function_CA_REPLICATE(CommandNode *&c, Bundle &obj);
	static int function_CA_SCALE(CommandNode *&c, Bundle &obj);
	static int function_CA_SCALEMOLECULES(CommandNode *&c, Bundle &obj);
	static int function_CA_SETCELL(CommandNode *&c, Bundle &obj);
	static int function_CA_SPACEGROUP(CommandNode *&c, Bundle &obj);
	// Charge commands
	static int function_CA_CHARGEFF(CommandNode *&c, Bundle &obj);
	static int function_CA_CHARGEFROMMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_CHARGEPATOM(CommandNode *&c, Bundle &obj);
	static int function_CA_CHARGE(CommandNode *&c, Bundle &obj);
	static int function_CA_CHARGETYPE(CommandNode *&c, Bundle &obj);
	static int function_CA_CLEARCHARGES(CommandNode *&c, Bundle &obj);
	// Colourscale commands
	static int function_CA_ADDPOINT(CommandNode *&c, Bundle &obj);
	static int function_CA_CLEARPOINTS(CommandNode *&c, Bundle &obj);
	static int function_CA_LISTSCALES(CommandNode *&c, Bundle &obj);
	static int function_CA_REMOVEPOINT(CommandNode *&c, Bundle &obj);
	static int function_CA_SCALENAME(CommandNode *&c, Bundle &obj);
	static int function_CA_SCALEVISIBLE(CommandNode *&c, Bundle &obj);
	static int function_CA_SETPOINT(CommandNode *&c, Bundle &obj);
	static int function_CA_SETPOINTCOLOUR(CommandNode *&c, Bundle &obj);
	static int function_CA_SETPOINTVALUE(CommandNode *&c, Bundle &obj);
	// Disordered build commands
	static int function_CA_DISORDER(CommandNode *&c, Bundle &obj);
	static int function_CA_LISTCOMPONENTS(CommandNode *&c, Bundle &obj);
	static int function_CA_NMOLS(CommandNode *&c, Bundle &obj);
	static int function_CA_REGION(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONCENTRE(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONCENTREF(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONF(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONGEOMETRY(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONGEOMETRYF(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONOVERLAPS(CommandNode *&c, Bundle &obj);
	static int function_CA_REGIONSHAPE(CommandNode *&c, Bundle &obj);
	static int function_CA_VDWSCALE(CommandNode *&c, Bundle &obj);
	// Edit commands
	static int function_CA_COPY(CommandNode *&c, Bundle &obj);
	static int function_CA_CUT(CommandNode *&c, Bundle &obj);
	static int function_CA_DELETE(CommandNode *&c, Bundle &obj);
	static int function_CA_PASTE(CommandNode *&c, Bundle &obj);
	static int function_CA_REDO(CommandNode *&c, Bundle &obj);
	static int function_CA_UNDO(CommandNode *&c, Bundle &obj);
	// Energy Commands
	static int function_CA_FRAMEENERGY(CommandNode *&c, Bundle &obj);
	static int function_CA_MODELENERGY(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTELEC(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTEWALD(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTINTER(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTINTRA(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTENERGY(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTSUMMARY(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTVDW(CommandNode *&c, Bundle &obj);
	// Filter Commands
	static int function_CA_EXACT(CommandNode *&c, Bundle &obj);
	static int function_CA_EXTENSION(CommandNode *&c, Bundle &obj);
	static int function_CA_GLOB(CommandNode *&c, Bundle &obj);
	static int function_CA_ID(CommandNode *&c, Bundle &obj);
	static int function_CA_NAME(CommandNode *&c, Bundle &obj);
	static int function_CA_NICKNAME(CommandNode *&c, Bundle &obj);
	static int function_CA_ZMAP(CommandNode *&c, Bundle &obj);
	// Flow control
	static int function_CA_BREAK(CommandNode *&c, Bundle &obj);
	static int function_CA_CONTINUE(CommandNode *&c, Bundle &obj);
	static int function_CA_ELSE(CommandNode *&c, Bundle &obj);
	static int function_CA_ELSEIF(CommandNode *&c, Bundle &obj);
	static int function_CA_END(CommandNode *&c, Bundle &obj);
	static int function_CA_FOR(CommandNode *&c, Bundle &obj);
	static int function_CA_GOTO(CommandNode *&c, Bundle &obj);
	static int function_CA_GOTONONIF(CommandNode *&c, Bundle &obj);
	static int function_CA_IF(CommandNode *&c, Bundle &obj);
	static int function_CA_TERMINATE(CommandNode *&c, Bundle &obj);
	// Force Commands
	static int function_CA_FRAMEFORCES(CommandNode *&c, Bundle &obj);
	static int function_CA_MODELFORCES(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTFORCES(CommandNode *&c, Bundle &obj);
	// Forcefield Commands
	static int function_CA_ANGLEDEF(CommandNode *&c, Bundle &obj);
	static int function_CA_BONDDEF(CommandNode *&c, Bundle &obj);
	static int function_CA_CLEARMAP(CommandNode *&c, Bundle &obj);
	static int function_CA_CREATEEXPRESSION(CommandNode *&c, Bundle &obj);
	static int function_CA_DEFAULTFF(CommandNode *&c, Bundle &obj);
	static int function_CA_EQUIVALENT(CommandNode *&c, Bundle &obj);
	static int function_CA_FINALISEFF(CommandNode *&c, Bundle &obj);
	static int function_CA_FFMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_FFPATTERN(CommandNode *&c, Bundle &obj);
	static int function_CA_FFPATTERNID(CommandNode *&c, Bundle &obj);
	static int function_CA_GENCONVERT(CommandNode *&c, Bundle &obj);
	static int function_CA_GENERATOR(CommandNode *&c, Bundle &obj);
	static int function_CA_GETFF(CommandNode *&c, Bundle &obj);
	static int function_CA_LOADFF(CommandNode *&c, Bundle &obj);
	static int function_CA_MAP(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWFF(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTSETUP(CommandNode *&c, Bundle &obj);
	static int function_CA_RULES(CommandNode *&c, Bundle &obj);
	static int function_CA_SAVEEXPRESSION(CommandNode *&c, Bundle &obj);
	static int function_CA_TORSIONDEF(CommandNode *&c, Bundle &obj);
	static int function_CA_TYPEDEF(CommandNode *&c, Bundle &obj);
	static int function_CA_TYPEMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_TYPETEST(CommandNode *&c, Bundle &obj);
	static int function_CA_UNITS(CommandNode *&c, Bundle &obj);
	static int function_CA_VDWDEF(CommandNode *&c, Bundle &obj);
	// Glyph commands
	static int function_CA_AUTOELLIPSOIDS(CommandNode *&c, Bundle &obj);
	static int function_CA_AUTOPOLYHEDRA(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHATOMF(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHATOMR(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHATOMV(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHATOMSF(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHATOMSR(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHATOMSV(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHCOLOUR(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHDATA(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHSOLID(CommandNode *&c, Bundle &obj);
	static int function_CA_GLYPHTEXT(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWGLYPH(CommandNode *&c, Bundle &obj);
	// Grid Commands
	static int function_CA_ADDGRIDPOINT(CommandNode *&c, Bundle &obj);
	static int function_CA_ADDNEXTGRIDPOINT(CommandNode *&c, Bundle &obj);
	static int function_CA_FINALISEGRID(CommandNode *&c, Bundle &obj);
	static int function_CA_GETGRID(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDAXES(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDCOLOUR(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDCOLOURNEGATIVE(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDCOLOURSCALE(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDCUBIC(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDCUTOFF(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDORTHO(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDLOOPORDER(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDORIGIN(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDSIZE(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDSTYLE(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDSYMMETRIC(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDTRANSPARENCY(CommandNode *&c, Bundle &obj);
	static int function_CA_GRIDUSEZ(CommandNode *&c, Bundle &obj);
	static int function_CA_LOADGRID(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWGRID(CommandNode *&c, Bundle &obj);
	// Image Commands
	static int function_CA_SAVEBITMAP(CommandNode *&c, Bundle &obj);
	static int function_CA_SAVEVECTOR(CommandNode *&c, Bundle &obj);
	// Labeling commands
	static int function_CA_CLEARLABELS(CommandNode *&c, Bundle &obj);
	static int function_CA_LABEL(CommandNode *&c, Bundle &obj);
	static int function_CA_REMOVELABEL(CommandNode *&c, Bundle &obj);
	static int function_CA_REMOVELABELS(CommandNode *&c, Bundle &obj);
	// MC Commands
	static int function_CA_MCACCEPT(CommandNode *&c, Bundle &obj);
	static int function_CA_MCALLOW(CommandNode *&c, Bundle &obj);
	static int function_CA_MCMAXSTEP(CommandNode *&c, Bundle &obj);
	static int function_CA_MCNTRIALS(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINTMC(CommandNode *&c, Bundle &obj);
	// Measurements
	static int function_CA_ANGLE(CommandNode *&c, Bundle &obj);
	static int function_CA_ANGLES(CommandNode *&c, Bundle &obj);
	static int function_CA_CLEARMEASUREMENTS(CommandNode *&c, Bundle &obj);
	static int function_CA_DISTANCE(CommandNode *&c, Bundle &obj);
	static int function_CA_DISTANCES(CommandNode *&c, Bundle &obj);
	static int function_CA_LISTMEASUREMENTS(CommandNode *&c, Bundle &obj);
	static int function_CA_MEASURE(CommandNode *&c, Bundle &obj);
	static int function_CA_TORSION(CommandNode *&c, Bundle &obj);
	static int function_CA_TORSIONS(CommandNode *&c, Bundle &obj);
	// Messaging
	static int function_CA_ERROR(CommandNode *&c, Bundle &obj);
	static int function_CA_PRINT(CommandNode *&c, Bundle &obj);
	static int function_CA_VERBOSE(CommandNode *&c, Bundle &obj);
	static int function_CA_WARN(CommandNode *&c, Bundle &obj);
	// Minimisation Commands
	static int function_CA_CGMINIMISE(CommandNode *&c, Bundle &obj);
	static int function_CA_CONVERGE(CommandNode *&c, Bundle &obj);
	static int function_CA_LINETOL(CommandNode *&c, Bundle &obj);
	static int function_CA_MCMINIMISE(CommandNode *&c, Bundle &obj);
	static int function_CA_SDMINIMISE(CommandNode *&c, Bundle &obj);
	static int function_CA_SIMPLEXMINIMISE(CommandNode *&c, Bundle &obj);
	// Model Commands
	static int function_CA_CREATEATOMS(CommandNode *&c, Bundle &obj);
	static int function_CA_FINALISEMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_GETMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_INFO(CommandNode *&c, Bundle &obj);
	static int function_CA_LISTMODELS(CommandNode *&c, Bundle &obj);
	static int function_CA_LOADMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_LOGINFO(CommandNode *&c, Bundle &obj);
	static int function_CA_MODELTEMPLATE(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_NEXTMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_PREVMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_SAVEMODEL(CommandNode *&c, Bundle &obj);
	static int function_CA_SETNAME(CommandNode *&c, Bundle &obj);
	// Pattern Commands
	static int function_CA_CLEARPATTERNS(CommandNode *&c, Bundle &obj);
	static int function_CA_CREATEPATTERNS(CommandNode *&c, Bundle &obj);
	static int function_CA_GETPATTERN(CommandNode *&c, Bundle &obj);
	static int function_CA_LISTPATTERNS(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWPATTERN(CommandNode *&c, Bundle &obj);
	// Preferences Commands
	static int function_CA_ANGLELABEL(CommandNode *&c, Bundle &obj);
	static int function_CA_ATOMDETAIL(CommandNode *&c, Bundle &obj);
	static int function_CA_BONDDETAIL(CommandNode *&c, Bundle &obj);
	static int function_CA_COLOUR(CommandNode *&c, Bundle &obj);
	static int function_CA_COMMONELEMENTS(CommandNode *&c, Bundle &obj);
	static int function_CA_DENSITYUNITS(CommandNode *&c, Bundle &obj);
	static int function_CA_DISTANCELABEL(CommandNode *&c, Bundle &obj);
	static int function_CA_ECUT(CommandNode *&c, Bundle &obj);
	static int function_CA_ELEC(CommandNode *&c, Bundle &obj);
	static int function_CA_ELEMENTAMBIENT(CommandNode *&c, Bundle &obj);
	static int function_CA_ELEMENTDIFFUSE(CommandNode *&c, Bundle &obj);
	static int function_CA_ELEMENTRADIUS(CommandNode *&c, Bundle &obj);
	static int function_CA_ENERGYUNITS(CommandNode *&c, Bundle &obj);
	static int function_CA_HDISTANCE(CommandNode *&c, Bundle &obj);
	static int function_CA_INTRA(CommandNode *&c, Bundle &obj);
	static int function_CA_GL(CommandNode *&c, Bundle &obj);
	static int function_CA_KEY(CommandNode *&c, Bundle &obj);
	static int function_CA_MOUSE(CommandNode *&c, Bundle &obj);
	static int function_CA_LABELSIZE(CommandNode *&c, Bundle &obj);
	static int function_CA_RADIUS(CommandNode *&c, Bundle &obj);
	static int function_CA_REPLICATEFOLD(CommandNode *&c, Bundle &obj);
	static int function_CA_REPLICATETRIM(CommandNode *&c, Bundle &obj);
	static int function_CA_SCHEME(CommandNode *&c, Bundle &obj);
	static int function_CA_SHININESS(CommandNode *&c, Bundle &obj);
	static int function_CA_SHOWONSCREEN(CommandNode *&c, Bundle &obj);
	static int function_CA_SHOWONIMAGE(CommandNode *&c, Bundle &obj);
	static int function_CA_STYLE(CommandNode *&c, Bundle &obj);
	static int function_CA_USENICETEXT(CommandNode *&c, Bundle &obj);
	static int function_CA_VCUT(CommandNode *&c, Bundle &obj);
	static int function_CA_VDW(CommandNode *&c, Bundle &obj);
	// Read / Write Commands
	static int function_CA_ADDREADOPTION(CommandNode *&c, Bundle &obj);
	static int function_CA_FIND(CommandNode *&c, Bundle &obj);
	static int function_CA_GETLINE(CommandNode *&c, Bundle &obj);
	static int function_CA_READCHARS(CommandNode *&c, Bundle &obj);
	static int function_CA_READFLOAT(CommandNode *&c, Bundle &obj);
	static int function_CA_READINTEGER(CommandNode *&c, Bundle &obj);
	static int function_CA_READLINE(CommandNode *&c, Bundle &obj);
	static int function_CA_READNEXT(CommandNode *&c, Bundle &obj);
	static int function_CA_READVAR(CommandNode *&c, Bundle &obj);
	static int function_CA_REMOVEREADOPTION(CommandNode *&c, Bundle &obj);
	static int function_CA_REWIND(CommandNode *&c, Bundle &obj);
	static int function_CA_SKIPCHARS(CommandNode *&c, Bundle &obj);
	static int function_CA_SKIPLINE(CommandNode *&c, Bundle &obj);
	static int function_CA_WRITELINE(CommandNode *&c, Bundle &obj);
	static int function_CA_WRITEVAR(CommandNode *&c, Bundle &obj);
	// Script Commands
	static int function_CA_LISTSCRIPTS(CommandNode *&c, Bundle &obj);
	static int function_CA_LOADSCRIPT(CommandNode *&c, Bundle &obj);
	static int function_CA_RUNSCRIPT(CommandNode *&c, Bundle &obj);
	// Select Commands
	static int function_CA_DESELECT(CommandNode *&c, Bundle &obj);
	static int function_CA_EXPAND(CommandNode *&c, Bundle &obj);
	static int function_CA_INVERT(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECT(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECTALL(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECTFFTYPE(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECTNONE(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECTOVERLAPS(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECTPATTERN(CommandNode *&c, Bundle &obj);
	static int function_CA_SELECTTYPE(CommandNode *&c, Bundle &obj);
	// Site Commands
	static int function_CA_GETSITE(CommandNode *&c, Bundle &obj);
	static int function_CA_LISTSITES(CommandNode *&c, Bundle &obj);
	static int function_CA_NEWSITE(CommandNode *&c, Bundle &obj);
	static int function_CA_SITEAXES(CommandNode *&c, Bundle &obj);
	// System Commands
	static int function_CA_DEBUG(CommandNode *&c, Bundle &obj);
	static int function_CA_GUI(CommandNode *&c, Bundle &obj);
	static int function_CA_SEED(CommandNode *&c, Bundle &obj);
	static int function_CA_HELP(CommandNode *&c, Bundle &obj);
	static int function_CA_QUIT(CommandNode *&c, Bundle &obj);
	static int function_CA_VERSION(CommandNode *&c, Bundle &obj);
	// Trajectory Commands
	static int function_CA_FINALISEFRAME(CommandNode *&c, Bundle &obj);
	static int function_CA_FIRSTFRAME(CommandNode *&c, Bundle &obj);
	static int function_CA_LASTFRAME(CommandNode *&c, Bundle &obj);
	static int function_CA_LOADTRAJECTORY(CommandNode *&c, Bundle &obj);
	static int function_CA_NEXTFRAME(CommandNode *&c, Bundle &obj);
	static int function_CA_PREVFRAME(CommandNode *&c, Bundle &obj);
	static int function_CA_SEEKFRAME(CommandNode *&c, Bundle &obj);
	// Transform Commands
	static int function_CA_CENTRE(CommandNode *&c, Bundle &obj);
	static int function_CA_TRANSLATE(CommandNode *&c, Bundle &obj);
	static int function_CA_TRANSLATEATOM(CommandNode *&c, Bundle &obj);
	static int function_CA_TRANSLATECELL(CommandNode *&c, Bundle &obj);
	static int function_CA_MIRROR(CommandNode *&c, Bundle &obj);
	// Variables
	static int function_CA_DECREASE(CommandNode *&c, Bundle &obj);
	static int function_CA_INCREASE(CommandNode *&c, Bundle &obj);
	static int function_CA_LET(CommandNode *&c, Bundle &obj);
	static int function_CA_LETCHAR(CommandNode *&c, Bundle &obj);
	static int function_CA_LETPTR(CommandNode *&c, Bundle &obj);
	// View
	static int function_CA_GETVIEW(CommandNode *&c, Bundle &obj);
	static int function_CA_LIGHT(CommandNode *&c, Bundle &obj);
	static int function_CA_LIGHTAMBIENT(CommandNode *&c, Bundle &obj);
	static int function_CA_LIGHTDIFFUSE(CommandNode *&c, Bundle &obj);
	static int function_CA_LIGHTPOSITION(CommandNode *&c, Bundle &obj);
	static int function_CA_LIGHTSPECULAR(CommandNode *&c, Bundle &obj);
	static int function_CA_ORTHOGRAPHIC(CommandNode *&c, Bundle &obj);
	static int function_CA_PERSPECTIVE(CommandNode *&c, Bundle &obj);
	static int function_CA_RESETVIEW(CommandNode *&c, Bundle &obj);
	static int function_CA_ROTATEVIEW(CommandNode *&c, Bundle &obj);
	static int function_CA_SETVIEW(CommandNode *&c, Bundle &obj);
	static int function_CA_SPEEDTEST(CommandNode *&c, Bundle &obj);
	static int function_CA_TRANSLATEVIEW(CommandNode *&c, Bundle &obj);
	static int function_CA_VIEWALONG(CommandNode *&c, Bundle &obj);
	static int function_CA_VIEWALONGCELL(CommandNode *&c, Bundle &obj);
	static int function_CA_ZOOMVIEW(CommandNode *&c, Bundle &obj);
	static int function_CA_ZROTATEVIEW(CommandNode *&c, Bundle &obj);

	/*
	// Function descriptions / syntax etc.
	*/
	private:
	// Function pointers
	CommandFunction pointers_[CA_NITEMS];
	// Dummy CommandList for use with non-flow call() function
	CommandList *dummyCommandList_;
	// Dummy CommandNode (owned by dummyCommandList_)
	CommandNode *dummyCommandNode_;

	public:
	// Function data
	static CommandData data[CA_NITEMS];
	// Initialise function pointers
	void initPointers();
	// Execute specified command
	int call(Command::Function cf, CommandNode *&c);
	int call(Command::Function cf);
	// 	int callFromPointer(Command::Function cf, CommandNode *c);
};

// External declaration
extern Command commands;

#endif

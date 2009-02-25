/*
	*** Command Functions
	*** src/parser/commands.h
	Copyright T. Youngs 2007-2009

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

#ifndef ATEN_NUCOMMANDS_H
#define ATEN_NUCOMMANDS_H

#include "base/bundle.h"
#include "parser/returnvalue.h"

// Forward declarations
class NuCommandList;
class NuCommandNode;
class NuCommandData;
class NuCommand;

// Function pointer typedef and call #define
typedef int (*NuCommandFunction)(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
#define CALL_NUCOMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

class NuCommandData
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
	// Return-value datatype (if definite)
	VTypes::DataType returnType;
	// Command syntax
	const char *syntax;

	// Return whether command accepts any arguments
	bool hasArguments();
};

// Command actions
class NuCommand {

	public:
	// Constructor / Destructor
	NuCommand();
	~NuCommand();

	// Command return values
	enum CommandReturnValue { Success, SuccessNoMove, Fail, FailContinue, Exit, ExitWithError };
	// Command list
	enum Function {
	
		// Variable declaration
		CA_CHARACTER,
		CA_INTEGER,
		CA_REAL,
		CA_CONSTVECTOR,
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
		CA_ELEMENTSVAR,
		CA_VECTOR,
	
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
		CA_INSERTATOM,
		CA_LOCATE,
		CA_MOVE,
		CA_MOVETOEND,
		CA_MOVETOSTART,
		CA_NEWATOM,
		CA_NEWATOMFRAC,
		CA_REORDER,
		CA_RESETPEN,
		CA_ROTX,
		CA_ROTY,
		CA_ROTZ,
		CA_SHIFTDOWN,
		CA_SHIFTUP,
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
		CA_ROTATECELL,
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
		CA_INTERDEF,
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
		CA_GRIDALPHA,
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
		CA_CURRENTMODEL,
		CA_FINALISEMODEL,
		CA_FIRSTMODEL,
		CA_GETMODEL,
		CA_INFO,
		CA_LASTMODEL,
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
		CA_SWAPBUFFERS,
		CA_USENICETEXT,
		CA_VCUT,
		CA_VDW,
		CA_ZOOMTHROTTLE,

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
		CA_DESELECTTYPE,
		CA_EXPAND,
		CA_INVERT,
		CA_SELECT,
		CA_SELECTALL,
		CA_SELECTFFTYPE,
		CA_SELECTIONCOG,
		CA_SELECTIONCOM,
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
		CA_AXISROTATE,
		CA_CENTRE,
		CA_MATRIXCONVERT,
		CA_MATRIXTRANSFORM,
		CA_MIRROR,
		CA_TRANSLATE,
		CA_TRANSLATEATOM,
		CA_TRANSLATECELL,

		// Variables
		Addition,
		CA_AFTERCHAR,
		CA_BEFORECHAR,
		CA_DECREASE,
		CA_INCREASE,
		CA_LET,
		CA_LETCHAR,
		CA_LETPTR,
		CA_LETVECTOR,
		CA_NORMALISE,
		CA_STRIPCHARS,
		Subtraction,

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
	NuCommand::Function command(const char*);

	/*
	// Function declarations
	*/
	private:
	// All command functions
	static int function_CA_ROOTNODE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Analyse commands
	static int function_CA_FINALISE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FRAMEANALYSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GEOMETRY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);	
	static int function_CA_MODELANALYSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PDENS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTJOBS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_RDF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SAVEQUANTITIES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TRAJANALYSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Atom Commands
	static int function_CA_ATOMSTYLE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GETATOM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_HIDE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETCOORDS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETCHARGE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETELEMENT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETFORCES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETFX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETFY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETFZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETRX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETRY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETRZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETVELOCITIES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETVX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETVY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETVZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHOW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHOWALL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Bond commands
	static int function_CA_AUGMENT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_BONDTOLERANCE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CLEARBONDS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CLEARSELECTEDBONDS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWBOND(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWBONDID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REBOND(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REBONDPATTERNS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REBONDSELECTION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Build commands
	static int function_CA_ADDHYDROGEN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_BOHR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CHAIN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ENDCHAIN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_INSERTATOM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOCATE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MOVE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MOVETOEND(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MOVETOSTART(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWATOM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWATOMFRAC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REORDER(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_RESETPEN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ROTX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ROTY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ROTZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHIFTDOWN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHIFTUP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TRANSMUTE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Cell commands
	static int function_CA_ADDGENERATOR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ADJUSTCELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CELLAXES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FOLD(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FOLDMOLECULES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FRACTOREAL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NOCELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PACK(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTCELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REPLICATE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ROTATECELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SCALE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SCALEMOLECULES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETCELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SPACEGROUP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Charge commands
	static int function_CA_CHARGEFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CHARGEFROMMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CHARGEPATOM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CHARGE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CHARGETYPE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CLEARCHARGES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Colourscale commands
	static int function_CA_ADDPOINT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CLEARPOINTS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LISTSCALES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REMOVEPOINT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SCALENAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SCALEVISIBLE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETPOINT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETPOINTCOLOUR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETPOINTVALUE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Disordered build commands
	static int function_CA_DISORDER(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LISTCOMPONENTS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NMOLS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONCENTRE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONCENTREF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONGEOMETRY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONGEOMETRYF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONOVERLAPS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REGIONSHAPE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VDWSCALE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Edit commands
	static int function_CA_COPY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CUT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DELETE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PASTE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REDO(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_UNDO(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Energy Commands
	static int function_CA_FRAMEENERGY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MODELENERGY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTELEC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTEWALD(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTINTER(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTINTRA(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTENERGY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTSUMMARY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTVDW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Filter Commands
	static int function_CA_EXACT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_EXTENSION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLOB(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NICKNAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ZMAP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Flow control
	static int function_CA_BREAK(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CONTINUE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ELSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ELSEIF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_END(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FOR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GOTO(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GOTONONIF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_IF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TERMINATE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Force Commands
	static int function_CA_FRAMEFORCES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MODELFORCES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTFORCES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Forcefield Commands
	static int function_CA_ANGLEDEF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_BONDDEF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CLEARMAP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CREATEEXPRESSION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DEFAULTFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_EQUIVALENT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FINALISEFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FFMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FFPATTERN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FFPATTERNID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GENCONVERT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GENERATOR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GETFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_INTERDEF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOADFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MAP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTSETUP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_RULES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SAVEEXPRESSION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TORSIONDEF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TYPEDEF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TYPEMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TYPETEST(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_UNITS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Glyph commands
	static int function_CA_AUTOELLIPSOIDS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_AUTOPOLYHEDRA(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHATOMF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHATOMR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHATOMV(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHATOMSF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHATOMSR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHATOMSV(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHCOLOUR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHDATA(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHSOLID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GLYPHTEXT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWGLYPH(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Grid Commands
	static int function_CA_ADDGRIDPOINT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ADDNEXTGRIDPOINT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FINALISEGRID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GETGRID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDALPHA(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDAXES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDCOLOUR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDCOLOURNEGATIVE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDCOLOURSCALE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDCUBIC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDCUTOFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDORTHO(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDLOOPORDER(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDORIGIN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDSIZE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDSTYLE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDSYMMETRIC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GRIDUSEZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOADGRID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWGRID(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Image Commands
	static int function_CA_SAVEBITMAP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SAVEVECTOR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Labeling commands
	static int function_CA_CLEARLABELS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LABEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REMOVELABEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REMOVELABELS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// MC Commands
	static int function_CA_MCACCEPT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MCALLOW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MCMAXSTEP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MCNTRIALS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINTMC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Measurements
	static int function_CA_ANGLE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ANGLES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CLEARMEASUREMENTS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DISTANCE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DISTANCES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LISTMEASUREMENTS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MEASURE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TORSION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TORSIONS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Messaging
	static int function_CA_ERROR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PRINT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VERBOSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_WARN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Minimisation Commands
	static int function_CA_CGMINIMISE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CONVERGE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LINETOL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MCMINIMISE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SDMINIMISE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SIMPLEXMINIMISE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Model Commands
	static int function_CA_CREATEATOMS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CURRENTMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FINALISEMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FIRSTMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GETMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_INFO(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LASTMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LISTMODELS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOADMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOGINFO(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MODELTEMPLATE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEXTMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PREVMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SAVEMODEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETNAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Pattern Commands
	static int function_CA_CLEARPATTERNS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CREATEPATTERNS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GETPATTERN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LISTPATTERNS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWPATTERN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Preferences Commands
	static int function_CA_ANGLELABEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ATOMDETAIL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_BONDDETAIL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_COLOUR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_COMMONELEMENTS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DENSITYUNITS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DISTANCELABEL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ECUT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ELEC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ELEMENTAMBIENT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ELEMENTDIFFUSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ELEMENTRADIUS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ENERGYUNITS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_HDISTANCE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_INTRA(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_KEY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MOUSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LABELSIZE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_RADIUS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REPLICATEFOLD(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REPLICATETRIM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SCHEME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHININESS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHOWONSCREEN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SHOWONIMAGE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_STYLE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SWAPBUFFERS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_USENICETEXT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VCUT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VDW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ZOOMTHROTTLE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Read / Write Commands
	static int function_CA_ADDREADOPTION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FIND(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GETLINE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_READCHARS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_READFLOAT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_READINTEGER(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_READLINE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_READNEXT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_READVAR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REMOVEREADOPTION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_REWIND(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SKIPCHARS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SKIPLINE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_WRITELINE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_WRITEVAR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Script Commands
	static int function_CA_LISTSCRIPTS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOADSCRIPT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_RUNSCRIPT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Select Commands
	static int function_CA_DESELECT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DESELECTTYPE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_EXPAND(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_INVERT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTALL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTFFTYPE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTIONCOG(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTIONCOM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTNONE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTOVERLAPS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTPATTERN(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SELECTTYPE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Site Commands
	static int function_CA_GETSITE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LISTSITES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEWSITE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SITEAXES(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// System Commands
	static int function_CA_DEBUG(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_GUI(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SEED(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_HELP(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_QUIT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VERSION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Trajectory Commands
	static int function_CA_FINALISEFRAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_FIRSTFRAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LASTFRAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LOADTRAJECTORY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NEXTFRAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PREVFRAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SEEKFRAME(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Transform Commands
	static int function_CA_AXISROTATE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_CENTRE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MATRIXCONVERT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MATRIXTRANSFORM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_MIRROR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TRANSLATE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TRANSLATEATOM(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TRANSLATECELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Variables
	static int function_Addition(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_AFTERCHAR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_BEFORECHAR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_DECREASE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_INCREASE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LET(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LETCHAR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LETPTR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LETVECTOR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_NORMALISE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_STRIPCHARS(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Subtraction(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// View
	static int function_CA_GETVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LIGHT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LIGHTAMBIENT(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LIGHTDIFFUSE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LIGHTPOSITION(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_LIGHTSPECULAR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ORTHOGRAPHIC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_PERSPECTIVE(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_RESETVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ROTATEVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SETVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_SPEEDTEST(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_TRANSLATEVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VIEWALONG(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_VIEWALONGCELL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ZOOMVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CA_ZROTATEVIEW(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);

	/*
	// Function descriptions / syntax etc.
	*/
	private:
	// Function pointers
	NuCommandFunction pointers_[CA_NITEMS];
	// Dummy CommandList for use with non-flow call() function
	NuCommandList *dummyCommandList_;
	// Dummy CommandNode (owned by dummyCommandList_)
	NuCommandNode *dummyCommandNode_;

	public:
	// Function data
	static NuCommandData data[CA_NITEMS];
	// Initialise function pointers
	void initPointers();
	// Execute specified command
	int call(NuCommand::Function cf, NuCommandNode *node);
};

// External declaration
extern NuCommand nucommands;

#endif

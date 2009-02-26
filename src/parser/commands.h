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
		
		// Dummy node
		NoFunction,

		// Joiner
		Joiner,
	
		// Analysis commands
		Finalise,
		Frameanalyse,
		Geometry,
		Modelanalyse,
		Pdens,
		Printjobs,
		Rdf,
		Savequantities,
		Trajanalyse,
	
		// Atom Commands
		Atomstyle,
		Getatom,
		Hide,
		Setcharge,
		Setcoords,
		Setelement,
		Setforces,
		Setfx,
		Setfy,
		Setfz,
		Setid,
		Setrx,
		Setry,
		Setrz,
		Setvelocities,
		Setvx,
		Setvy,
		Setvz,
		Show,
		Showall,
	
		// Bond commands
		Augment,
		Bondtolerance,
		Clearbonds,
		Clearselectedbonds,
		Newbond,
		Newbondid,
		Rebond,
		Rebondpatterns,
		Rebondselection,
	
		// Build commands
		Addhydrogen,
		Bohr,
		Chain,
		Endchain,
		Insertatom,
		Locate,
		Move,
		Movetoend,
		Movetostart,
		Newatom,
		Newatomfrac,
		Reorder,
		Resetpen,
		Rotx,
		Roty,
		Rotz,
		Shiftdown,
		Shiftup,
		Transmute,
	
		// Cell commands
		Addgenerator,
		Adjustcell,
		Cell,
		Cellaxes,
		Fold,
		Foldmolecules,
		Fractoreal,
		Nocell,
		Pack,
		Printcell,
		Replicate,
		Rotatecell,
		Scale,
		Scalemolecules,
		Setcell,
		Spacegroup,
	
		// Charge commands
		Chargeff,
		Chargefrommodel,
		Chargepatom,
		Charge,
		Chargetype,
		Clearcharges,
	
		// Colourscale commands
		Addpoint,
		Clearpoints,
		Listscales,
		Removepoint,
		Scalename,
		Scalevisible,
		Setpoint,
		Setpointcolour,
		Setpointvalue,
	
		// Disordered build commands
		Disorder,
		Listcomponents,
		Nmols,
		Region,
		Regioncentre,
		Regioncentref,
		Regionf,
		Regiongeometry,
		Regiongeometryf,
		Regionoverlaps,
		Regionshape,
		Vdwscale,
	
		// Edit commands
		Copy,
		Cut,
		Delete,
		Paste,
		Redo,
		Undo,
	
		// Energy Commands
		Frameenergy,
		Modelenergy,
		Printelec,
		Printewald,
		Printinter,
		Printintra,
		Printenergy,
		Printsummary,
		Printvdw,
	
		// Flow control
		Break,
		Continue,
		Else,
		Elseif,
		End,
		For,
		Goto,
		Gotononif,
		If,
		Terminate,
	
		// Force Commands
		Frameforces,
		Modelforces,
		Printforces,
	
		// Forcefield/Expression Commands
		Angledef,
		Bonddef,
		Clearmap,
		Createexpression,
		Defaultff,
		Equivalent,
		Ffmodel,
		Ffpattern,
		Ffpatternid,
		Finaliseff,
		Genconvert,
		Generator,
		Getff,
		Interdef,
		Loadff,
		Map,
		Newff,
		Printsetup,
		Rules,
		Saveexpression,
		Torsiondef,
		Typedef,
		Typemodel,
		Typetest,
		Units,
	
		// Glyph commands
		Autoellipsoids,
		Autopolyhedra,
		Glyphatomf,
		Glyphatomr,
		Glyphatomv,
		Glyphatomsf,
		Glyphatomsr,
		Glyphatomsv,
		Glyphcolour,
		Glyphdata,
		Glyphsolid,
		Glyphtext,
		Newglyph,
	
		// Grid Commands
		Addgridpoint,
		Addnextgridpoint,
		Finalisegrid,
		Getgrid,
		Gridalpha,
		Gridaxes,
		Gridcolour,
		Gridcolournegative,
		Gridcolourscale,
		Gridcubic,
		Gridcutoff,
		Gridlooporder,
		Gridorigin,
		Gridortho,
		Gridsize,
		Gridstyle,
		Gridsymmetric,
		Gridusez,
		Loadgrid,
		Newgrid,
	
		// Image Commands
		Savebitmap,
		Savevector,
	
		// Labeling commands
		Clearlabels,
		Label,
		Removelabel,
		Removelabels,
	
		// MC Commands
		Mcaccept,
		Mcallow,
		Mcmaxstep,
		Mcntrials,
		Printmc,
	
		// Measurements
		Angle,
		Angles,
		Clearmeasurements,
		Distance,
		Distances,
		Listmeasurements,
		Measure,
		Torsion,
		Torsions,
	
		// Messaging
		Error,
		Print,
		Verbose,
		Warn,
	
		// Minimisation Commands
		Cgminimise,
		Converge,
		Linetol,
		Mcminimise,
		Sdminimise,
		Simplexminimise,
		
		// Model Commands
		Createatoms,
		Currentmodel,
		Finalisemodel,
		Firstmodel,
		Getmodel,
		Info,
		Lastmodel,
		Listmodels,
		Loadmodel,
		Loginfo,
		Modeltemplate,
		Newmodel,
		Nextmodel,
		Prevmodel,
		Savemodel,
		Setname,
	
		// Pattern Commands
		Clearpatterns,
		Createpatterns,
		Getpattern,
		Listpatterns,
		Newpattern,
	
		// Preferences Commands
		Anglelabel,
		Atomdetail,
		Bonddetail,
		Colour,
		Commonelements,
		Densityunits,
		Distancelabel,
		Ecut,
		Elec,
		Elementambient,
		Elementdiffuse,
		Elementradius,
		Energyunits,
		Gl,
		Hdistance,
		Intra,
		Key,
		Labelsize,
		Light,
		LightAmbient,
		LightDiffuse,
		LightPosition,
		LightSpecular,
		Mouse,
		Radius,
		Replicatefold,
		Replicatetrim,
		Scheme,
		Shininess,
		Showonscreen,
		Showonimage,
		Style,
		Swapbuffers,
		Usenicetext,
		Vcut,
		Vdw,
		Zoomthrottle,

		// Read / Write Commands
		Addreadoption,
		Find,
		Getline,
		Readchars,
		Readfloat,
		Readinteger,
		Readline,
		Readnext,
		Readvar,
		Removereadoption,
		Rewind,
		Skipchars,
		Skipline,
		Writeline,
		Writevar,

		// Script Commands
		Listscripts,
		Loadscript,
		Runscript,

		// Select Commands
		Deselect,
		Deselecttype,
		Expand,
		Invert,
		Select,
		Selectall,
		Selectfftype,
		Selectioncog,
		Selectioncom,
		Selectnone,
		Selectoverlaps,
		Selectpattern,
		Selecttype,

		// Site Commands
		Getsite,
		Listsites,
		Newsite,
		Siteaxes,

		// System commands
		Debug,
		Gui,
		Help,
		Seed,
		Quit,
		Version,

		// Trajectory Commands
		Finaliseframe,
		Firstframe,
		Lastframe,
		Loadtrajectory,
		Nextframe,
		Prevframe,
		Seekframe,

		// Transformation Commands
		Axisrotate,
		Centre,
		Matrixconvert,
		Matrixtransform,
		Mirror,
		Translate,
		Translateatom,
		Translatecell,

		// Variable Declaration
		Character,
		Integer,
		Real,
		ConstVector,
		Atom,
		Bond,
		Pattern,
		PatternBound,
		Model,
		Grid,
		FFAtom,
		FFBound,
		CellVar,
		Forcefield,
		PrefsVar,
		ElementsVar,
		Vector,

		// Variable Manipulation
		AfterChar,
		BeforeChar,
		Decrease,
		Increase,
		Let,
		LetChar,
		LetPtr,
		LetVector,
		Normalise,
		OperatorAdd,
		OperatorDivide,
		OperatorMultiply,
		OperatorPower,
		OperatorSubtract,
		StripChars,

		// View
		GetView,
		Orthographic,
		Perspective,
		ResetView,
		RotateView,
		SetView,
		SpeedTest,
		TranslateView,
		ViewAlong,
		ViewAlongCell,
		ZoomView,
		ZRotateView,

		nFunctions
	};
	// Return enumerated command id from string
	NuCommand::Function command(const char*);

	/*
	// Function declarations
	*/
	private:
	// Primary commands
	static int function_NoFunction(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Joiner(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Analyse commands
	static int function_Finalise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Frameanalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Geometry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);	
	static int function_Modelanalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Pdens(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printjobs(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rdf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Savequantities(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Trajanalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Atom Commands
	static int function_Atomstyle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Getatom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Hide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setcoords(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setcharge(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setelement(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setfx(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setfy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setfz(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setrx(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setrz(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setvelocities(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setvx(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setvy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setvz(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Show(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Showall(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Bond commands
	static int function_Augment(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Bondtolerance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Clearbonds(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Clearselectedbonds(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newbond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newbondid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rebond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rebondpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rebondselection(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Build commands
	static int function_Addhydrogen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Bohr(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Chain(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Endchain(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Insertatom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Locate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Move(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Movetoend(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Movetostart(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newatom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newatomfrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Reorder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Resetpen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rotx(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Roty(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rotz(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Shiftdown(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Shiftup(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Transmute(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Cell commands
	static int function_Addgenerator(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Adjustcell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Cell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Cellaxes(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Fold(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Foldmolecules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Fractoreal(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Nocell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Pack(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printcell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Replicate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rotatecell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Scale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Scalemolecules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setcell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Spacegroup(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Charge commands
	static int function_Chargeff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Chargefrommodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Chargepatom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Charge(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Chargetype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Clearcharges(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Colourscale commands
	static int function_Addpoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Clearpoints(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Listscales(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Removepoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Scalename(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Scalevisible(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setpoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setpointcolour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setpointvalue(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Disordered build commands
	static int function_Disorder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Listcomponents(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Nmols(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Region(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regioncentre(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regioncentref(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regionf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regiongeometry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regiongeometryf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regionoverlaps(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Regionshape(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Vdwscale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Edit commands
	static int function_Copy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Cut(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Delete(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Paste(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Redo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Undo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Energy Commands
	static int function_Frameenergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Modelenergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printelec(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printewald(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printinter(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printintra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printenergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printsummary(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printvdw(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Filter Commands
	static int function_Exact(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Extension(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glob(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Id(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Name(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Nickname(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Zmap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Flow control
	static int function_Break(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Continue(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Else(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Elseif(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_End(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_For(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Goto(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gotononif(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_If(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Terminate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Force Commands
	static int function_Frameforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Modelforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Forcefield Commands
	static int function_Angledef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Bonddef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Clearmap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Createexpression(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Defaultff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Equivalent(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Finaliseff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Ffmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Ffpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Ffpatternid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Genconvert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Generator(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Getff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Interdef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Loadff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Map(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printsetup(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Saveexpression(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Torsiondef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Typedef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Typemodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Typetest(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Units(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Glyph commands
	static int function_Autoellipsoids(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Autopolyhedra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphatomf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphatomr(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphatomv(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphatomsf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphatomsr(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphatomsv(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphcolour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphdata(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphsolid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Glyphtext(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newglyph(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Grid Commands
	static int function_Addgridpoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Addnextgridpoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Finalisegrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Getgrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridalpha(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridaxes(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridcolour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridcolournegative(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridcolourscale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridcubic(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridcutoff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridortho(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridlooporder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridorigin(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridsize(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridstyle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridsymmetric(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gridusez(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Loadgrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newgrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Image Commands
	static int function_Savebitmap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Savevector(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Labeling commands
	static int function_Clearlabels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Label(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Removelabel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Removelabels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// MC Commands
	static int function_Mcaccept(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Mcallow(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Mcmaxstep(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Mcntrials(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Printmc(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Measurements
	static int function_Angle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Angles(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Clearmeasurements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Distance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Distances(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Listmeasurements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Measure(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Torsion(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Torsions(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Messaging
	static int function_Error(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Print(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Verbose(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Warn(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Minimisation Commands
	static int function_Cgminimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Converge(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Linetol(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Mcminimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Sdminimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Simplexminimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Model Commands
	static int function_Createatoms(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Currentmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Finalisemodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Firstmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Getmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Info(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Lastmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Listmodels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Loadmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Loginfo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Modeltemplate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Nextmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Prevmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Savemodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Setname(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Pattern Commands
	static int function_Clearpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Createpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Getpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Listpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Preferences Commands
	static int function_Anglelabel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Atomdetail(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Bonddetail(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Colour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Commonelements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Densityunits(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Distancelabel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Ecut(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Elec(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Elementambient(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Elementdiffuse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Elementradius(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Energyunits(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Hdistance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Intra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gl(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Key(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Labelsize(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Light(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LightAmbient(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LightDiffuse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LightPosition(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LightSpecular(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Mouse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Radius(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Replicatefold(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Replicatetrim(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Scheme(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Shininess(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Showonscreen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Showonimage(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Style(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Swapbuffers(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Usenicetext(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Vcut(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Vdw(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Zoomthrottle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Read / Write Commands
	static int function_Addreadoption(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Find(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Getline(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Readchars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Readfloat(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Readinteger(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Readline(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Readnext(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Readvar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Removereadoption(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Rewind(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Skipchars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Skipline(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Writeline(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Writevar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Script Commands
	static int function_Listscripts(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Loadscript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Runscript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Select Commands
	static int function_Deselect(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Deselecttype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Expand(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Invert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Select(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectall(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectfftype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectioncog(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectioncom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectnone(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectoverlaps(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selectpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Selecttype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Site Commands
	static int function_Getsite(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Listsites(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Newsite(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Siteaxes(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// System Commands
	static int function_Debug(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Gui(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Seed(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Help(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Quit(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Version(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Trajectory Commands
	static int function_Finaliseframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Firstframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Lastframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Loadtrajectory(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Nextframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Prevframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Seekframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Transform Commands
	static int function_Axisrotate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Centre(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Matrixconvert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Matrixtransform(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Mirror(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Translate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Translateatom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Translatecell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Variable Declaration
	static int function_Character(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Integer(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Real(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ConstVector(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Atom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Bond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Pattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_PatternBound(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Model(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Grid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_FFAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_FFBound(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_CellVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Forcefield(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_PrefsVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ElementsVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Vector(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Variable Manipulation
	static int function_AfterChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_BeforeChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Decrease(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Increase(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Let(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LetChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LetPtr(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_LetVector(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Normalise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_OperatorAdd(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_OperatorDivide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_OperatorMultiply(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_OperatorPower(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_OperatorSubtract(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_StripChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// View
	static int function_GetView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Orthographic(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_Perspective(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ResetView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_RotateView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_SetView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_SpeedTest(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_TranslateView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ViewAlong(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ViewAlongCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ZoomView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static int function_ZRotateView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);

	/*
	// Function descriptions / syntax etc.
	*/
	private:
	// Function pointers
	NuCommandFunction pointers_[NuCommand::nFunctions];
	// Dummy CommandList for use with non-flow call() function
	NuCommandList *dummyCommandList_;
	// Dummy CommandNode (owned by dummyCommandList_)
	NuCommandNode *dummyCommandNode_;

	public:
	// Function data
	static NuCommandData data[NuCommand::nFunctions];
	// Initialise function pointers
	void initPointers();
	// Execute specified command
	int call(NuCommand::Function cf, NuCommandNode *node, NuReturnValue &rv);
};

// External declaration
extern NuCommand nucommands;

#endif

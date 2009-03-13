/*
	*** Command Function Pointers
	*** src/parse/functions.cpp
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

#include "nucommand/commands.h"

// Initialise Command Pointers
void NuCommand::initPointers()
{
	/*
	// Store pointers to all command functions
	*/
	pointers_[NoFunction] = &function_NoFunction;
	pointers_[Joiner] = &function_Joiner;
	pointers_[Initialisations] = &function_Initialisations;

/*
	// Analyse commands
	pointers_[Finalise] = &NuCommand::function_Finalise;
	pointers_[Frameanalyse] = &NuCommand::function_Frameanalyse;
	pointers_[Geometry] = &NuCommand::function_Geometry;
	pointers_[Modelanalyse] = &NuCommand::function_Modelanalyse;
	pointers_[Pdens] = &NuCommand::function_Pdens;
	pointers_[Printjobs] = &NuCommand::function_Printjobs;
	pointers_[Rdf] = &NuCommand::function_Rdf;
	pointers_[Savequantities] = &NuCommand::function_Savequantities;
	pointers_[Trajanalyse] = &NuCommand::function_Trajanalyse;

	// Atom commands
	pointers_[Atomstyle] = &NuCommand::function_Atomstyle;
	pointers_[Getatom] = &NuCommand::function_Getatom;
	pointers_[Hide] = &NuCommand::function_Hide;
	pointers_[Setcoords] = &NuCommand::function_Setcoords;
	pointers_[Setcharge] = &NuCommand::function_Setcharge;
	pointers_[Setelement] = &NuCommand::function_Setelement;
	pointers_[Setforces] = &NuCommand::function_Setforces;
	pointers_[Setfx] = &NuCommand::function_Setfx;
	pointers_[Setfy] = &NuCommand::function_Setfy;
	pointers_[Setfz] = &NuCommand::function_Setfz;
	pointers_[Setid] = &NuCommand::function_Setid;
	pointers_[Setrx] = &NuCommand::function_Setrx;
	pointers_[Setry] = &NuCommand::function_Setry;
	pointers_[Setrz] = &NuCommand::function_Setrz;
	pointers_[Setvelocities] = &NuCommand::function_Setvelocities;
	pointers_[Setvx] = &NuCommand::function_Setvx;
	pointers_[Setvy] = &NuCommand::function_Setvy;
	pointers_[Setvz] = &NuCommand::function_Setvz;
	pointers_[Show] = &NuCommand::function_Show;
	pointers_[Showall] = &NuCommand::function_Showall;

	// Bond commands
	pointers_[Augment] = &NuCommand::function_Augment;
	pointers_[Bondtolerance] = &NuCommand::function_Bondtolerance;
	pointers_[Clearbonds] = &NuCommand::function_Clearbonds;
	pointers_[Clearselectedbonds] = &NuCommand::function_Clearselectedbonds;
	pointers_[Newbond] = &NuCommand::function_Newbond;
	pointers_[Newbondid] = &NuCommand::function_Newbondid;
	pointers_[Rebond] = &NuCommand::function_Rebond;
	pointers_[Rebondpatterns] = &NuCommand::function_Rebondpatterns;
	pointers_[Rebondselection] = &NuCommand::function_Rebondselection;
*/
	// Build commands
	pointers_[AddHydrogen] = &NuCommand::function_AddHydrogen;
	pointers_[Bohr] = &NuCommand::function_Bohr;
	pointers_[Chain] = &NuCommand::function_Chain;
	pointers_[EndChain] = &NuCommand::function_EndChain;
	pointers_[InsertAtom] = &NuCommand::function_InsertAtom;
	pointers_[Locate] = &NuCommand::function_Locate;
	pointers_[Move] = &NuCommand::function_Move;
	pointers_[MoveToEnd] = &NuCommand::function_MoveToEnd;
	pointers_[MoveToStart] = &NuCommand::function_MoveToStart;
	pointers_[NewAtom] = &NuCommand::function_NewAtom;
	pointers_[NewAtomFrac] = &NuCommand::function_NewAtomFrac;
	pointers_[ReOrder] = &NuCommand::function_ReOrder;
	pointers_[ResetPen] = &NuCommand::function_ResetPen;
	pointers_[RotX] = &NuCommand::function_RotX;
	pointers_[RotY] = &NuCommand::function_RotY;
	pointers_[RotZ] = &NuCommand::function_RotZ;
	pointers_[ShiftDown] = &NuCommand::function_ShiftDown;
	pointers_[ShiftUp] = &NuCommand::function_ShiftUp;
	pointers_[Transmute] = &NuCommand::function_Transmute;

/*
	// Cell commands
	pointers_[Addgenerator] = &NuCommand::function_Addgenerator;
	pointers_[Adjustcell] = &NuCommand::function_Adjustcell;
	pointers_[Cell] = &NuCommand::function_Cell;
	pointers_[Cellaxes] = &NuCommand::function_Cellaxes;
	pointers_[Fold] = &NuCommand::function_Fold;
	pointers_[Foldmolecules] = &NuCommand::function_Foldmolecules;
	pointers_[Fractoreal] = &NuCommand::function_Fractoreal;
	pointers_[Nocell] = &NuCommand::function_Nocell;
	pointers_[Pack] = &NuCommand::function_Pack;
	pointers_[Printcell] = &NuCommand::function_Printcell;
	pointers_[Replicate] = &NuCommand::function_Replicate;
	pointers_[Rotatecell] = &NuCommand::function_Rotatecell;
	pointers_[Scale] = &NuCommand::function_Scale;
	pointers_[Scalemolecules] = &NuCommand::function_Scalemolecules;
	pointers_[Setcell] = &NuCommand::function_Setcell;
	pointers_[Spacegroup] = &NuCommand::function_Spacegroup;

	// Charge commands
	pointers_[Chargeff] = &NuCommand::function_Chargeff;
	pointers_[Chargefrommodel] = &NuCommand::function_Chargefrommodel;
	pointers_[Chargepatom] = &NuCommand::function_Chargepatom;
	pointers_[Charge] = &NuCommand::function_Charge;
	pointers_[Chargetype] = &NuCommand::function_Chargetype;
	pointers_[Clearcharges] = &NuCommand::function_Clearcharges;

	// Colourscale commands
	pointers_[Addpoint] = &NuCommand::function_Addpoint;
	pointers_[Clearpoints] = &NuCommand::function_Clearpoints;
	pointers_[Listscales] = &NuCommand::function_Listscales;
	pointers_[Removepoint] = &NuCommand::function_Removepoint;
	pointers_[Scalename] = &NuCommand::function_Scalename;
	pointers_[Scalevisible] = &NuCommand::function_Scalevisible;
	pointers_[Setpoint] = &NuCommand::function_Setpoint;
	pointers_[Setpointcolour] = &NuCommand::function_Setpointcolour;
	pointers_[Setpointvalue] = &NuCommand::function_Setpointvalue;

	// Disordered build commands
	pointers_[Disorder] = &NuCommand::function_Disorder;
	pointers_[Listcomponents] = &NuCommand::function_Listcomponents;
	pointers_[Nmols] = &NuCommand::function_Nmols;
	pointers_[Region] = &NuCommand::function_Region;
	pointers_[Regioncentre] = &NuCommand::function_Regioncentre;
	pointers_[Regioncentref] = &NuCommand::function_Regioncentref;
	pointers_[Regionf] = &NuCommand::function_Regionf;
	pointers_[Regiongeometry] = &NuCommand::function_Regiongeometry;
	pointers_[Regiongeometryf] = &NuCommand::function_Regiongeometryf;
	pointers_[Regionoverlaps] = &NuCommand::function_Regionoverlaps;
	pointers_[Regionshape] = &NuCommand::function_Regionshape;
	pointers_[Vdwscale] = &NuCommand::function_Vdwscale;

	// Edit Commands
	pointers_[Delete] = &NuCommand::function_Delete;
	pointers_[Copy] = &NuCommand::function_Copy;
	pointers_[Cut] = &NuCommand::function_Cut;
	pointers_[Paste] = &NuCommand::function_Paste;
	pointers_[Redo] = &NuCommand::function_Redo;
	pointers_[Undo] = &NuCommand::function_Undo;

	// Energy Commands
	pointers_[Frameenergy] = &NuCommand::function_Frameenergy;
	pointers_[Modelenergy] = &NuCommand::function_Modelenergy;
	pointers_[Printelec] = &NuCommand::function_Printelec;
	pointers_[Printewald] = &NuCommand::function_Printewald;
	pointers_[Printinter] = &NuCommand::function_Printinter;
	pointers_[Printintra] = &NuCommand::function_Printintra;
	pointers_[Printenergy] = &NuCommand::function_Printenergy;
	pointers_[Printsummary] = &NuCommand::function_Printsummary;
	pointers_[Printvdw] = &NuCommand::function_Printvdw;
*/
	// Flow control
	pointers_[If] = &function_If;
	pointers_[Break] = &NuCommand::function_Break;
	pointers_[Continue] = &NuCommand::function_Continue;
// 	pointers_[End] = &NuCommand::function_End;
	pointers_[For] = &NuCommand::function_For;
	pointers_[If] = &NuCommand::function_If;
// 	pointers_[Terminate] = &NuCommand::function_Terminate;
/*
	// Force Commands
	pointers_[Frameforces] = &NuCommand::function_Frameforces;
	pointers_[Modelforces] = &NuCommand::function_Modelforces;
	pointers_[Printforces] = &NuCommand::function_Printforces;

	// Forcefield Commands
        pointers_[Angledef] = &NuCommand::function_Angledef;
	pointers_[Bonddef] = &NuCommand::function_Bonddef;
	pointers_[Clearmap] = &NuCommand::function_Clearmap;
	pointers_[Createexpression] = &NuCommand::function_Createexpression;
	pointers_[Defaultff] = &NuCommand::function_Defaultff;
	pointers_[Equivalent] = &NuCommand::function_Equivalent;
	pointers_[Ffmodel] = &NuCommand::function_Ffmodel;
	pointers_[Ffpattern] = &NuCommand::function_Ffpattern;
	pointers_[Ffpatternid] = &NuCommand::function_Ffpatternid;
	pointers_[Finaliseff] = &NuCommand::function_Finaliseff;
	pointers_[Genconvert] = &NuCommand::function_Genconvert;
	pointers_[Generator] = &NuCommand::function_Generator;
	pointers_[Getff] = &NuCommand::function_Getff;
	pointers_[Interdef] = &NuCommand::function_Interdef;
	pointers_[Loadff] = &NuCommand::function_Loadff;
	pointers_[Map] = &NuCommand::function_Map;
	pointers_[Newff] = &NuCommand::function_Newff;
	pointers_[Printsetup] = &NuCommand::function_Printsetup;
	pointers_[Rules] = &NuCommand::function_Rules;
	pointers_[Saveexpression] = &NuCommand::function_Saveexpression;
	pointers_[Torsiondef] = &NuCommand::function_Torsiondef;
	pointers_[Typedef] = &NuCommand::function_Typedef;
	pointers_[Typemodel] = &NuCommand::function_Typemodel;
	pointers_[Typetest] = &NuCommand::function_Typetest;
	pointers_[Units] = &NuCommand::function_Units;

	// Glyph commands
	pointers_[Autoellipsoids] = &NuCommand::function_Autoellipsoids;
	pointers_[Autopolyhedra] = &NuCommand::function_Autopolyhedra;
	pointers_[Glyphatomf] = &NuCommand::function_Glyphatomf;
	pointers_[Glyphatomr] = &NuCommand::function_Glyphatomr;
	pointers_[Glyphatomv] = &NuCommand::function_Glyphatomv;
	pointers_[Glyphatomsf] = &NuCommand::function_Glyphatomsf;
	pointers_[Glyphatomsr] = &NuCommand::function_Glyphatomsr;
	pointers_[Glyphatomsv] = &NuCommand::function_Glyphatomsv;
	pointers_[Glyphcolour] = &NuCommand::function_Glyphcolour;
	pointers_[Glyphdata] = &NuCommand::function_Glyphdata;
	pointers_[Glyphsolid] = &NuCommand::function_Glyphsolid;
	pointers_[Glyphtext] = &NuCommand::function_Glyphtext;
	pointers_[Newglyph] = &NuCommand::function_Newglyph;

	// Grid Commands
	pointers_[Addgridpoint] = &NuCommand::function_Addgridpoint;
	pointers_[Addnextgridpoint] = &NuCommand::function_Addnextgridpoint;
	pointers_[Finalisegrid] = &NuCommand::function_Finalisegrid;
	pointers_[Gridalpha] = &NuCommand::function_Gridalpha;
	pointers_[Gridaxes] = &NuCommand::function_Gridaxes;
	pointers_[Gridcolour] = &NuCommand::function_Gridcolour;
	pointers_[Gridcolournegative] = &NuCommand::function_Gridcolournegative;
	pointers_[Gridcolourscale] = &NuCommand::function_Gridcolourscale;
	pointers_[Gridcubic] = &NuCommand::function_Gridcubic;
	pointers_[Gridcutoff] = &NuCommand::function_Gridcutoff;
	pointers_[Gridlooporder] = &NuCommand::function_Gridlooporder;
	pointers_[Gridorigin] = &NuCommand::function_Gridorigin;
	pointers_[Gridortho] = &NuCommand::function_Gridortho;
	pointers_[Gridsize] = &NuCommand::function_Gridsize;
	pointers_[Gridstyle] = &NuCommand::function_Gridstyle;
	pointers_[Gridsymmetric] = &NuCommand::function_Gridsymmetric;
	pointers_[Gridusez] = &NuCommand::function_Gridusez;
	pointers_[Loadgrid] = &NuCommand::function_Loadgrid;
	pointers_[Newgrid] = &NuCommand::function_Newgrid;

	// Image Commands
	pointers_[Savebitmap] = &NuCommand::function_Savebitmap;
	pointers_[Savevector] = &NuCommand::function_Savevector;

	// Labeling Commands
	pointers_[Clearlabels] = &NuCommand::function_Clearlabels;
	pointers_[Label] = &NuCommand::function_Label;
	pointers_[Removelabel] = &NuCommand::function_Removelabel;
	pointers_[Removelabels] = &NuCommand::function_Removelabels;

	// MC Commands
	pointers_[Mcaccept] = &NuCommand::function_Mcaccept;
	pointers_[Mcallow] = &NuCommand::function_Mcallow;
	pointers_[Mcmaxstep] = &NuCommand::function_Mcmaxstep;
	pointers_[Mcntrials] = &NuCommand::function_Mcntrials;
	pointers_[Printmc] = &NuCommand::function_Printmc;

	// Measurement Commands
	pointers_[Angle] = &NuCommand::function_Angle;
	pointers_[Angles] = &NuCommand::function_Angles;
	pointers_[Clearmeasurements] = &NuCommand::function_Clearmeasurements;
	pointers_[Distance] = &NuCommand::function_Distance;
	pointers_[Distances] = &NuCommand::function_Distances;
	pointers_[Listmeasurements] = &NuCommand::function_Listmeasurements;
	pointers_[Measure] = &NuCommand::function_Measure;
	pointers_[Torsion] = &NuCommand::function_Torsion;
	pointers_[Torsions] = &NuCommand::function_Torsions;

	// Messaging Commands
	pointers_[Error] = &NuCommand::function_Error;
	pointers_[Print] = &NuCommand::function_Print;
	pointers_[Verbose] = &NuCommand::function_Verbose;
	pointers_[Warn] = &NuCommand::function_Warn;

	// Minimisation Commands
	pointers_[Cgminimise] = &NuCommand::function_Cgminimise;
	pointers_[Converge] = &NuCommand::function_Converge;
	pointers_[Linetol] = &NuCommand::function_Linetol;
	pointers_[Mcminimise] = &NuCommand::function_Mcminimise;
	pointers_[Sdminimise] = &NuCommand::function_Sdminimise;
	pointers_[Simplexminimise] = &NuCommand::function_Simplexminimise;
	
	// Model Commands
	pointers_[Createatoms] = &NuCommand::function_Createatoms;
	pointers_[Currentmodel] = &NuCommand::function_Currentmodel;
	pointers_[Finalisemodel] = &NuCommand::function_Finalisemodel;
	pointers_[Firstmodel] = &NuCommand::function_Firstmodel;
	pointers_[Getmodel] = &NuCommand::function_Getmodel;
	pointers_[Lastmodel] = &NuCommand::function_Lastmodel;
	pointers_[Listmodels] = &NuCommand::function_Listmodels;
	pointers_[Loadmodel] = &NuCommand::function_Loadmodel;
	pointers_[Loginfo] = &NuCommand::function_Loginfo;
	pointers_[Modeltemplate] = &NuCommand::function_Modeltemplate;
	pointers_[Newmodel] = &NuCommand::function_Newmodel;
	pointers_[Nextmodel] = &NuCommand::function_Nextmodel;
	pointers_[Prevmodel] = &NuCommand::function_Prevmodel;
	pointers_[Info] = &NuCommand::function_Info;
	pointers_[Savemodel] = &NuCommand::function_Savemodel;
	pointers_[Setname] = &NuCommand::function_Setname;

	// Pattern Commands
	pointers_[Clearpatterns] = &NuCommand::function_Clearpatterns;
	pointers_[Createpatterns] = &NuCommand::function_Createpatterns;
	pointers_[Getpattern] = &NuCommand::function_Getpattern;
	pointers_[Listpatterns] = &NuCommand::function_Listpatterns;
	pointers_[Newpattern] = &NuCommand::function_Newpattern;

	// Preferences Commands
	pointers_[Anglelabel] = &NuCommand::function_Anglelabel;
	pointers_[Atomdetail] = &NuCommand::function_Atomdetail;
	pointers_[Bonddetail] = &NuCommand::function_Bonddetail;
	pointers_[Colour] = &NuCommand::function_Colour;
	pointers_[Commonelements] = &NuCommand::function_Commonelements;
	pointers_[Densityunits] = &NuCommand::function_Densityunits;
	pointers_[Distancelabel] = &NuCommand::function_Distancelabel;
	pointers_[Ecut] = &NuCommand::function_Ecut;
	pointers_[Elec] = &NuCommand::function_Elec;
	pointers_[Elementambient] = &NuCommand::function_Elementambient;
	pointers_[Elementdiffuse] = &NuCommand::function_Elementdiffuse;
	pointers_[Elementradius] = &NuCommand::function_Elementradius;
	pointers_[Energyunits] = &NuCommand::function_Energyunits;
	pointers_[Gl] = &NuCommand::function_Gl;
	pointers_[Hdistance] = &NuCommand::function_Hdistance;
	pointers_[Intra] = &NuCommand::function_Intra;
	pointers_[Key] = &NuCommand::function_Key;
	pointers_[Labelsize] = &NuCommand::function_Labelsize;
	pointers_[Light] = &NuCommand::function_Light;
	pointers_[LightAmbient] = &NuCommand::function_LightAmbient;
	pointers_[LightDiffuse] = &NuCommand::function_LightDiffuse;
	pointers_[LightPosition] = &NuCommand::function_LightPosition;
	pointers_[LightSpecular] = &NuCommand::function_LightSpecular;
	pointers_[Mouse] = &NuCommand::function_Mouse;
	pointers_[Radius] = &NuCommand::function_Radius;
	pointers_[Replicatefold] = &NuCommand::function_Replicatefold;
	pointers_[Replicatetrim] = &NuCommand::function_Replicatetrim;
	pointers_[Scheme] = &NuCommand::function_Scheme;
	pointers_[Shininess] = &NuCommand::function_Shininess;
	pointers_[Showonscreen] = &NuCommand::function_Showonscreen;
	pointers_[Showonimage] = &NuCommand::function_Showonimage;
	pointers_[Style] = &NuCommand::function_Style;
	pointers_[Swapbuffers] = &NuCommand::function_Swapbuffers;
	pointers_[Usenicetext] = &NuCommand::function_Usenicetext;
	pointers_[Vcut] = &NuCommand::function_Vcut;
	pointers_[Vdw] = &NuCommand::function_Vdw;
	pointers_[Zoomthrottle] = &NuCommand::function_Zoomthrottle;

	// Read / Write Commands
	pointers_[Addreadoption] = &NuCommand::function_Addreadoption;
	pointers_[Find] = &NuCommand::function_Find;
	pointers_[Getline] = &NuCommand::function_Getline;
	pointers_[Readchars] = &NuCommand::function_Readchars;
	pointers_[Readfloat] = &NuCommand::function_Readfloat;
	pointers_[Readinteger] = &NuCommand::function_Readinteger;
	pointers_[Readline] = &NuCommand::function_Readline;
	pointers_[Readnext] = &NuCommand::function_Readnext;
	pointers_[Readvar] = &NuCommand::function_Readvar;
	pointers_[Removereadoption] = &NuCommand::function_Removereadoption;
	pointers_[Rewind] = &NuCommand::function_Rewind;
	pointers_[Skipchars] = &NuCommand::function_Skipchars;
	pointers_[Skipline] = &NuCommand::function_Skipline;
	pointers_[Writeline] = &NuCommand::function_Writeline;
	pointers_[Writevar] = &NuCommand::function_Writevar;

	// Script Commands
	pointers_[Listscripts] = &NuCommand::function_Listscripts;
	pointers_[Loadscript] = &NuCommand::function_Loadscript;
	pointers_[Runscript] = &NuCommand::function_Runscript;

	// Select Commands
	pointers_[Deselect] = &NuCommand::function_Deselect;
	pointers_[Deselecttype] = &NuCommand::function_Deselecttype;
	pointers_[Expand] = &NuCommand::function_Expand;
	pointers_[Invert] = &NuCommand::function_Invert;
	pointers_[Select] = &NuCommand::function_Select;
	pointers_[Selectall] = &NuCommand::function_Selectall;
	pointers_[Selectfftype] = &NuCommand::function_Selectfftype;
	pointers_[Selectioncog] = &NuCommand::function_Selectioncog;
	pointers_[Selectioncom] = &NuCommand::function_Selectioncom;
	pointers_[Selectnone] = &NuCommand::function_Selectnone;
	pointers_[Selectoverlaps] = &NuCommand::function_Selectoverlaps;
	pointers_[Selectpattern] = &NuCommand::function_Selectpattern;
	pointers_[Selecttype] = &NuCommand::function_Selecttype;
	
	// Site Commands
	pointers_[Getsite] = &NuCommand::function_Getsite;
	pointers_[Listsites] = &NuCommand::function_Listsites;
	pointers_[Newsite] = &NuCommand::function_Newsite;
	pointers_[Siteaxes] = &NuCommand::function_Siteaxes;

	// System Commands
	pointers_[Debug] = &NuCommand::function_Debug;
	pointers_[Gui] = &NuCommand::function_Gui;
	pointers_[Help] = &NuCommand::function_Help;
	pointers_[Seed] = &NuCommand::function_Seed;
	pointers_[Quit] = &NuCommand::function_Quit;
	pointers_[Version] = &NuCommand::function_Version;
	
	// Trajectory Commands
	pointers_[Finaliseframe] = &NuCommand::function_Finaliseframe;
	pointers_[Firstframe] = &NuCommand::function_Firstframe;
	pointers_[Lastframe] = &NuCommand::function_Lastframe;
	pointers_[Loadtrajectory] = &NuCommand::function_Loadtrajectory;
	pointers_[Nextframe] = &NuCommand::function_Nextframe;
	pointers_[Prevframe] = &NuCommand::function_Prevframe;
	pointers_[Seekframe] = &NuCommand::function_Seekframe;

	// Transform Commands
	pointers_[Axisrotate] = &NuCommand::function_Axisrotate;
	pointers_[Centre] = &NuCommand::function_Centre;
	pointers_[Translate] = &NuCommand::function_Translate;
	pointers_[Translateatom] = &NuCommand::function_Translateatom;
	pointers_[Translatecell] = &NuCommand::function_Translatecell;
	pointers_[Matrixconvert] = &NuCommand::function_Matrixconvert;
	pointers_[Matrixtransform] = &NuCommand::function_Matrixtransform;
	pointers_[Mirror] = &NuCommand::function_Mirror;

	// Variable Manipulation Commands
	pointers_[AfterChar] = &NuCommand::function_AfterChar;
	pointers_[BeforeChar] = &NuCommand::function_BeforeChar;
	pointers_[Decrease] = &NuCommand::function_Decrease;
	pointers_[Increase] = &NuCommand::function_Increase;
	pointers_[Let] = &NuCommand::function_Let;
	pointers_[LetChar] = &NuCommand::function_LetChar;
	pointers_[LetPtr] = &NuCommand::function_LetPtr;
	pointers_[LetVector] = &NuCommand::function_LetVector;
	pointers_[Normalise] = &NuCommand::function_Normalise;	
	pointers_[StripChars] = &NuCommand::function_StripChars;
*/

	// Variable Operators
	pointers_[OperatorAdd] = &function_OperatorAdd;
	pointers_[OperatorAssignment] = &function_OperatorAssignment;
	pointers_[OperatorAssignmentDivide] = &function_OperatorAssignmentDivide;
	pointers_[OperatorAssignmentMinus] = &function_OperatorAssignmentMinus;
	pointers_[OperatorAssignmentMultiply] = &function_OperatorAssignmentMultiply;
	pointers_[OperatorAssignmentPlus] = &function_OperatorAssignmentPlus;
	pointers_[OperatorDivide] = &function_OperatorDivide;
	pointers_[OperatorEqualTo] = &function_OperatorEqualTo;
	pointers_[OperatorGreaterThan] = &function_OperatorGreaterThan;
	pointers_[OperatorGreaterThanEqualTo] = &function_OperatorGreaterThanEqualTo;
	pointers_[OperatorLessThan] = &function_OperatorLessThan;
	pointers_[OperatorLessThanEqualTo] = &function_OperatorLessThanEqualTo;
	pointers_[OperatorMultiply] = &function_OperatorMultiply;
	pointers_[OperatorNegate] = &function_OperatorNegate;
	pointers_[OperatorNotEqualTo] = &function_OperatorNotEqualTo;
	pointers_[OperatorPower] = &function_OperatorPower;
	pointers_[OperatorSubtract] = &function_OperatorSubtract;

/*
	// View Commands
	pointers_[GetView] = &NuCommand::function_GetView;
	pointers_[Orthographic] = &NuCommand::function_Orthographic;
	pointers_[Perspective] = &NuCommand::function_Perspective;
	pointers_[ResetView] = &NuCommand::function_ResetView;
	pointers_[RotateView] = &NuCommand::function_RotateView;
	pointers_[SetView] = &NuCommand::function_SetView;
	pointers_[SpeedTest] = &NuCommand::function_SpeedTest;
	pointers_[TranslateView] = &NuCommand::function_TranslateView;
	pointers_[ViewAlong] = &NuCommand::function_ViewAlong;
	pointers_[ViewAlongCell] = &NuCommand::function_ViewAlongCell;
	pointers_[ZoomView] = &NuCommand::function_ZoomView;
	pointers_[ZRotateView] = &NuCommand::function_ZRotateView;
*/
}

/*
	*** Command function pointers
	*** src/command/functions.cpp
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

#include "command/commands.h"

// Initialise Command Pointers
void Command::initPointers()
{
	/*
	// Store pointers to all command functions
	*/

	pointers_[CA_ROOTNODE] = &function_CA_ROOTNODE;

	// Analyse commands
	pointers_[CA_FINALISE] = &Command::function_CA_FINALISE;
	pointers_[CA_FRAMEANALYSE] = &Command::function_CA_FRAMEANALYSE;
	pointers_[CA_GEOMETRY] = &Command::function_CA_GEOMETRY;
	pointers_[CA_MODELANALYSE] = &Command::function_CA_MODELANALYSE;
	pointers_[CA_PDENS] = &Command::function_CA_PDENS;
	pointers_[CA_PRINTJOBS] = &Command::function_CA_PRINTJOBS;
	pointers_[CA_RDF] = &Command::function_CA_RDF;
	pointers_[CA_SAVEQUANTITIES] = &Command::function_CA_SAVEQUANTITIES;
	pointers_[CA_TRAJANALYSE] = &Command::function_CA_TRAJANALYSE;

	// Atom commands
	pointers_[CA_ATOMSTYLE] = &Command::function_CA_ATOMSTYLE;
	pointers_[CA_GETATOM] = &Command::function_CA_GETATOM;
	pointers_[CA_HIDE] = &Command::function_CA_HIDE;
	pointers_[CA_SETCOORDS] = &Command::function_CA_SETCOORDS;
	pointers_[CA_SETCHARGE] = &Command::function_CA_SETCHARGE;
	pointers_[CA_SETELEMENT] = &Command::function_CA_SETELEMENT;
	pointers_[CA_SETFORCES] = &Command::function_CA_SETFORCES;
	pointers_[CA_SETFX] = &Command::function_CA_SETFX;
	pointers_[CA_SETFY] = &Command::function_CA_SETFY;
	pointers_[CA_SETFZ] = &Command::function_CA_SETFZ;
	pointers_[CA_SETID] = &Command::function_CA_SETID;
	pointers_[CA_SETRX] = &Command::function_CA_SETRX;
	pointers_[CA_SETRY] = &Command::function_CA_SETRY;
	pointers_[CA_SETRZ] = &Command::function_CA_SETRZ;
	pointers_[CA_SETVELOCITIES] = &Command::function_CA_SETVELOCITIES;
	pointers_[CA_SETVX] = &Command::function_CA_SETVX;
	pointers_[CA_SETVY] = &Command::function_CA_SETVY;
	pointers_[CA_SETVZ] = &Command::function_CA_SETVZ;
	pointers_[CA_SHOW] = &Command::function_CA_SHOW;
	pointers_[CA_SHOWALL] = &Command::function_CA_SHOWALL;

	// Bond commands
	pointers_[CA_AUGMENT] = &Command::function_CA_AUGMENT;
	pointers_[CA_BONDTOLERANCE] = &Command::function_CA_BONDTOLERANCE;
	pointers_[CA_CLEARBONDS] = &Command::function_CA_CLEARBONDS;
	pointers_[CA_CLEARSELECTEDBONDS] = &Command::function_CA_CLEARSELECTEDBONDS;
	pointers_[CA_NEWBOND] = &Command::function_CA_NEWBOND;
	pointers_[CA_NEWBONDID] = &Command::function_CA_NEWBONDID;
	pointers_[CA_REBOND] = &Command::function_CA_REBOND;
	pointers_[CA_REBONDPATTERNS] = &Command::function_CA_REBONDPATTERNS;
	pointers_[CA_REBONDSELECTION] = &Command::function_CA_REBONDSELECTION;

	// Build commands
	pointers_[CA_ADDHYDROGEN] = &Command::function_CA_ADDHYDROGEN;
	pointers_[CA_BOHR] = &Command::function_CA_BOHR;
	pointers_[CA_CHAIN] = &Command::function_CA_CHAIN;
	pointers_[CA_ENDCHAIN] = &Command::function_CA_ENDCHAIN;
	pointers_[CA_LOCATE] = &Command::function_CA_LOCATE;
	pointers_[CA_MOVE] = &Command::function_CA_MOVE;
	pointers_[CA_NEWATOM] = &Command::function_CA_NEWATOM;
	pointers_[CA_NEWATOMFRAC] = &Command::function_CA_NEWATOMFRAC;
	pointers_[CA_RESETPEN] = &Command::function_CA_RESETPEN;
	pointers_[CA_ROTX] = &Command::function_CA_ROTX;
	pointers_[CA_ROTY] = &Command::function_CA_ROTY;
	pointers_[CA_ROTZ] = &Command::function_CA_ROTZ;
	pointers_[CA_SHIFTDOWN] = &Command::function_CA_SHIFTDOWN;
	pointers_[CA_SHIFTUP] = &Command::function_CA_SHIFTUP;
	pointers_[CA_TOEND] = &Command::function_CA_TOEND;
	pointers_[CA_TOSTART] = &Command::function_CA_TOSTART;
	pointers_[CA_TRANSMUTE] = &Command::function_CA_TRANSMUTE;

	// Cell commands
	pointers_[CA_ADDGENERATOR] = &Command::function_CA_ADDGENERATOR;
	pointers_[CA_ADJUSTCELL] = &Command::function_CA_ADJUSTCELL;
	pointers_[CA_CELL] = &Command::function_CA_CELL;
	pointers_[CA_CELLAXES] = &Command::function_CA_CELLAXES;
	pointers_[CA_FOLD] = &Command::function_CA_FOLD;
	pointers_[CA_FOLDMOLECULES] = &Command::function_CA_FOLDMOLECULES;
	pointers_[CA_FRACTOREAL] = &Command::function_CA_FRACTOREAL;
	pointers_[CA_NOCELL] = &Command::function_CA_NOCELL;
	pointers_[CA_PACK] = &Command::function_CA_PACK;
	pointers_[CA_PRINTCELL] = &Command::function_CA_PRINTCELL;
	pointers_[CA_REPLICATE] = &Command::function_CA_REPLICATE;
	pointers_[CA_SCALE] = &Command::function_CA_SCALE;
	pointers_[CA_SETCELL] = &Command::function_CA_SETCELL;
	pointers_[CA_SPACEGROUP] = &Command::function_CA_SPACEGROUP;

	// Charge commands
	pointers_[CA_CHARGEFF] = &Command::function_CA_CHARGEFF;
	pointers_[CA_CHARGEFROMMODEL] = &Command::function_CA_CHARGEFROMMODEL;
	pointers_[CA_CHARGEPATOM] = &Command::function_CA_CHARGEPATOM;
	pointers_[CA_CHARGE] = &Command::function_CA_CHARGE;
	pointers_[CA_CHARGETYPE] = &Command::function_CA_CHARGETYPE;
	pointers_[CA_CLEARCHARGES] = &Command::function_CA_CLEARCHARGES;

	// Colourscale commands
	pointers_[CA_ADDPOINT] = &Command::function_CA_ADDPOINT;
	pointers_[CA_CLEARPOINTS] = &Command::function_CA_CLEARPOINTS;
	pointers_[CA_LISTSCALES] = &Command::function_CA_LISTSCALES;
	pointers_[CA_REMOVEPOINT] = &Command::function_CA_REMOVEPOINT;
	pointers_[CA_SCALENAME] = &Command::function_CA_SCALENAME;
	pointers_[CA_SCALEVISIBLE] = &Command::function_CA_SCALEVISIBLE;
	pointers_[CA_SETPOINT] = &Command::function_CA_SETPOINT;
	pointers_[CA_SETPOINTCOLOUR] = &Command::function_CA_SETPOINTCOLOUR;
	pointers_[CA_SETPOINTVALUE] = &Command::function_CA_SETPOINTVALUE;

	// Disordered build commands
	pointers_[CA_DISORDER] = &Command::function_CA_DISORDER;
	pointers_[CA_LISTCOMPONENTS] = &Command::function_CA_LISTCOMPONENTS;
	pointers_[CA_NMOLS] = &Command::function_CA_NMOLS;
	pointers_[CA_REGION] = &Command::function_CA_REGION;
	pointers_[CA_REGIONCENTRE] = &Command::function_CA_REGIONCENTRE;
	pointers_[CA_REGIONCENTREF] = &Command::function_CA_REGIONCENTREF;
	pointers_[CA_REGIONF] = &Command::function_CA_REGIONF;
	pointers_[CA_REGIONGEOMETRY] = &Command::function_CA_REGIONGEOMETRY;
	pointers_[CA_REGIONGEOMETRYF] = &Command::function_CA_REGIONGEOMETRYF;
	pointers_[CA_REGIONOVERLAPS] = &Command::function_CA_REGIONOVERLAPS;
	pointers_[CA_REGIONSHAPE] = &Command::function_CA_REGIONSHAPE;
	pointers_[CA_VDWSCALE] = &Command::function_CA_VDWSCALE;

	// Edit Commands
	pointers_[CA_DELETE] = &Command::function_CA_DELETE;
	pointers_[CA_COPY] = &Command::function_CA_COPY;
	pointers_[CA_CUT] = &Command::function_CA_CUT;
	pointers_[CA_PASTE] = &Command::function_CA_PASTE;
	pointers_[CA_REDO] = &Command::function_CA_REDO;
	pointers_[CA_UNDO] = &Command::function_CA_UNDO;

	// Energy Commands
	pointers_[CA_FRAMEENERGY] = &Command::function_CA_FRAMEENERGY;
	pointers_[CA_MODELENERGY] = &Command::function_CA_MODELENERGY;
	pointers_[CA_PRINTELEC] = &Command::function_CA_PRINTELEC;
	pointers_[CA_PRINTEWALD] = &Command::function_CA_PRINTEWALD;
	pointers_[CA_PRINTINTER] = &Command::function_CA_PRINTINTER;
	pointers_[CA_PRINTINTRA] = &Command::function_CA_PRINTINTRA;
	pointers_[CA_PRINTENERGY] = &Command::function_CA_PRINTENERGY;
	pointers_[CA_PRINTSUMMARY] = &Command::function_CA_PRINTSUMMARY;
	pointers_[CA_PRINTVDW] = &Command::function_CA_PRINTVDW;

	// Flow control
	pointers_[CA_BREAK] = &Command::function_CA_BREAK;
	pointers_[CA_CONTINUE] = &Command::function_CA_CONTINUE;
	pointers_[CA_ELSE] = &Command::function_CA_ELSE;
	pointers_[CA_ELSEIF] = &Command::function_CA_ELSEIF;
	pointers_[CA_END] = &Command::function_CA_END;
	pointers_[CA_FOR] = &Command::function_CA_FOR;
	pointers_[CA_GOTO] = &Command::function_CA_GOTO;
	pointers_[CA_GOTONONIF] = &Command::function_CA_GOTONONIF;
	pointers_[CA_IF] = &Command::function_CA_IF;
	pointers_[CA_TERMINATE] = &Command::function_CA_TERMINATE;

	// Force Commands
	pointers_[CA_FRAMEFORCES] = &Command::function_CA_FRAMEFORCES;
	pointers_[CA_MODELFORCES] = &Command::function_CA_MODELFORCES;
	pointers_[CA_PRINTFORCES] = &Command::function_CA_PRINTFORCES;

	// Forcefield Commands
        pointers_[CA_ANGLEDEF] = &Command::function_CA_ANGLEDEF;
        pointers_[CA_BONDDEF] = &Command::function_CA_BONDDEF;
	pointers_[CA_CLEARMAP] = &Command::function_CA_CLEARMAP;
	pointers_[CA_CREATEEXPRESSION] = &Command::function_CA_CREATEEXPRESSION;
	pointers_[CA_DEFAULTFF] = &Command::function_CA_DEFAULTFF;
        pointers_[CA_EQUIVALENT] = &Command::function_CA_EQUIVALENT;
	pointers_[CA_FFMODEL] = &Command::function_CA_FFMODEL;
	pointers_[CA_FFPATTERN] = &Command::function_CA_FFPATTERN;
	pointers_[CA_FFPATTERNID] = &Command::function_CA_FFPATTERNID;
        pointers_[CA_FINALISEFF] = &Command::function_CA_FINALISEFF;
        pointers_[CA_GENCONVERT] = &Command::function_CA_GENCONVERT;
        pointers_[CA_GENERATOR] = &Command::function_CA_GENERATOR;
	pointers_[CA_GETFF] = &Command::function_CA_GETFF;
	pointers_[CA_LOADFF] = &Command::function_CA_LOADFF;
	pointers_[CA_MAP] = &Command::function_CA_MAP;
        pointers_[CA_NEWFF] = &Command::function_CA_NEWFF;
	pointers_[CA_PRINTSETUP] = &Command::function_CA_PRINTSETUP;
        pointers_[CA_RULES] = &Command::function_CA_RULES;
	pointers_[CA_SAVEEXPRESSION] = &Command::function_CA_SAVEEXPRESSION;
        pointers_[CA_TORSIONDEF] = &Command::function_CA_TORSIONDEF;
        pointers_[CA_TYPEDEF] = &Command::function_CA_TYPEDEF;
	pointers_[CA_TYPEMODEL] = &Command::function_CA_TYPEMODEL;
	pointers_[CA_TYPETEST] = &Command::function_CA_TYPETEST;
        pointers_[CA_UNITS] = &Command::function_CA_UNITS;
        pointers_[CA_VDWDEF] = &Command::function_CA_VDWDEF;

	// Glyph commands
	pointers_[CA_AUTOELLIPSOIDS] = &Command::function_CA_AUTOELLIPSOIDS;
	pointers_[CA_AUTOPOLYHEDRA] = &Command::function_CA_AUTOPOLYHEDRA;
	pointers_[CA_GLYPHATOMF] = &Command::function_CA_GLYPHATOMF;
	pointers_[CA_GLYPHATOMR] = &Command::function_CA_GLYPHATOMR;
	pointers_[CA_GLYPHATOMV] = &Command::function_CA_GLYPHATOMV;
	pointers_[CA_GLYPHATOMSF] = &Command::function_CA_GLYPHATOMSF;
	pointers_[CA_GLYPHATOMSR] = &Command::function_CA_GLYPHATOMSR;
	pointers_[CA_GLYPHATOMSV] = &Command::function_CA_GLYPHATOMSV;
	pointers_[CA_GLYPHCOLOUR] = &Command::function_CA_GLYPHCOLOUR;
	pointers_[CA_GLYPHDATA] = &Command::function_CA_GLYPHDATA;
	pointers_[CA_GLYPHSOLID] = &Command::function_CA_GLYPHSOLID;
	pointers_[CA_GLYPHTEXT] = &Command::function_CA_GLYPHTEXT;
	pointers_[CA_NEWGLYPH] = &Command::function_CA_NEWGLYPH;

	// Grid Commands
	pointers_[CA_ADDGRIDPOINT] = &Command::function_CA_ADDGRIDPOINT;
	pointers_[CA_ADDNEXTGRIDPOINT] = &Command::function_CA_ADDNEXTGRIDPOINT;
	pointers_[CA_FINALISEGRID] = &Command::function_CA_FINALISEGRID;
	pointers_[CA_GRIDAXES] = &Command::function_CA_GRIDAXES;
	pointers_[CA_GRIDCOLOUR] = &Command::function_CA_GRIDCOLOUR;
	pointers_[CA_GRIDCOLOURSCALE] = &Command::function_CA_GRIDCOLOURSCALE;
	pointers_[CA_GRIDCUBIC] = &Command::function_CA_GRIDCUBIC;
	pointers_[CA_GRIDCUTOFF] = &Command::function_CA_GRIDCUTOFF;
	pointers_[CA_GRIDLOOPORDER] = &Command::function_CA_GRIDLOOPORDER;
	pointers_[CA_GRIDORIGIN] = &Command::function_CA_GRIDORIGIN;
	pointers_[CA_GRIDORTHO] = &Command::function_CA_GRIDORTHO;
	pointers_[CA_GRIDSIZE] = &Command::function_CA_GRIDSIZE;
	pointers_[CA_GRIDSTYLE] = &Command::function_CA_GRIDSTYLE;
	pointers_[CA_GRIDSYMMETRIC] = &Command::function_CA_GRIDSYMMETRIC;
	pointers_[CA_GRIDTRANSPARENCY] = &Command::function_CA_GRIDTRANSPARENCY;
	pointers_[CA_GRIDUSEZ] = &Command::function_CA_GRIDUSEZ;
	pointers_[CA_LOADGRID] = &Command::function_CA_LOADGRID;
	pointers_[CA_NEWGRID] = &Command::function_CA_NEWGRID;

	// Image Commands
	pointers_[CA_SAVEBITMAP] = &Command::function_CA_SAVEBITMAP;
	pointers_[CA_SAVEVECTOR] = &Command::function_CA_SAVEVECTOR;

	// Labeling Commands
	pointers_[CA_CLEARLABELS] = &Command::function_CA_CLEARLABELS;
	pointers_[CA_LABEL] = &Command::function_CA_LABEL;
	pointers_[CA_REMOVELABEL] = &Command::function_CA_REMOVELABEL;
	pointers_[CA_REMOVELABELS] = &Command::function_CA_REMOVELABELS;

	// MC Commands
	pointers_[CA_MCACCEPT] = &Command::function_CA_MCACCEPT;
	pointers_[CA_MCALLOW] = &Command::function_CA_MCALLOW;
	pointers_[CA_MCMAXSTEP] = &Command::function_CA_MCMAXSTEP;
	pointers_[CA_MCNTRIALS] = &Command::function_CA_MCNTRIALS;
	pointers_[CA_PRINTMC] = &Command::function_CA_PRINTMC;

	// Measurement Commands
	pointers_[CA_ANGLE] = &Command::function_CA_ANGLE;
	pointers_[CA_ANGLES] = &Command::function_CA_ANGLES;
	pointers_[CA_CLEARMEASUREMENTS] = &Command::function_CA_CLEARMEASUREMENTS;
	pointers_[CA_DISTANCE] = &Command::function_CA_DISTANCE;
	pointers_[CA_DISTANCES] = &Command::function_CA_DISTANCES;
	pointers_[CA_LISTMEASUREMENTS] = &Command::function_CA_LISTMEASUREMENTS;
	pointers_[CA_MEASURE] = &Command::function_CA_MEASURE;
	pointers_[CA_TORSION] = &Command::function_CA_TORSION;
	pointers_[CA_TORSIONS] = &Command::function_CA_TORSIONS;

	// Messaging Commands
	pointers_[CA_ERROR] = &Command::function_CA_ERROR;
	pointers_[CA_PRINT] = &Command::function_CA_PRINT;
	pointers_[CA_VERBOSE] = &Command::function_CA_VERBOSE;
	pointers_[CA_WARN] = &Command::function_CA_WARN;

	// Minimisation Commands
	pointers_[CA_CGMINIMISE] = &Command::function_CA_CGMINIMISE;
	pointers_[CA_CONVERGE] = &Command::function_CA_CONVERGE;
	pointers_[CA_LINETOL] = &Command::function_CA_LINETOL;
	pointers_[CA_MCMINIMISE] = &Command::function_CA_MCMINIMISE;
	pointers_[CA_SDMINIMISE] = &Command::function_CA_SDMINIMISE;
	pointers_[CA_SIMPLEXMINIMISE] = &Command::function_CA_SIMPLEXMINIMISE;
	
	// Model Commands
	pointers_[CA_CREATEATOMS] = &Command::function_CA_CREATEATOMS;
	pointers_[CA_FINALISEMODEL] = &Command::function_CA_FINALISEMODEL;
	pointers_[CA_GETMODEL] = &Command::function_CA_GETMODEL;
	pointers_[CA_LISTMODELS] = &Command::function_CA_LISTMODELS;
	pointers_[CA_LOADMODEL] = &Command::function_CA_LOADMODEL;
	pointers_[CA_LOGINFO] = &Command::function_CA_LOGINFO;
	pointers_[CA_MODELTEMPLATE] = &Command::function_CA_MODELTEMPLATE;
	pointers_[CA_NEWMODEL] = &Command::function_CA_NEWMODEL;
	pointers_[CA_NEXTMODEL] = &Command::function_CA_NEXTMODEL;
	pointers_[CA_PREVMODEL] = &Command::function_CA_PREVMODEL;
	pointers_[CA_INFO] = &Command::function_CA_INFO;
	pointers_[CA_SAVEMODEL] = &Command::function_CA_SAVEMODEL;
	pointers_[CA_SETNAME] = &Command::function_CA_SETNAME;

	// Pattern Commands
	pointers_[CA_CLEARPATTERNS] = &Command::function_CA_CLEARPATTERNS;
	pointers_[CA_CREATEPATTERNS] = &Command::function_CA_CREATEPATTERNS;
	pointers_[CA_GETPATTERN] = &Command::function_CA_GETPATTERN;
	pointers_[CA_LISTPATTERNS] = &Command::function_CA_LISTPATTERNS;
	pointers_[CA_NEWPATTERN] = &Command::function_CA_NEWPATTERN;

	// Preferences Commands
	pointers_[CA_ANGLELABEL] = &Command::function_CA_ANGLELABEL;
	pointers_[CA_ATOMDETAIL] = &Command::function_CA_ATOMDETAIL;
	pointers_[CA_BONDDETAIL] = &Command::function_CA_BONDDETAIL;
	pointers_[CA_COLOUR] = &Command::function_CA_COLOUR;
	pointers_[CA_COMMONELEMENTS] = &Command::function_CA_COMMONELEMENTS;
	pointers_[CA_DENSITYUNITS] = &Command::function_CA_DENSITYUNITS;
	pointers_[CA_DISTANCELABEL] = &Command::function_CA_DISTANCELABEL;
	pointers_[CA_ECUT] = &Command::function_CA_ECUT;
	pointers_[CA_ELEC] = &Command::function_CA_ELEC;
	pointers_[CA_ELEMENTAMBIENT] = &Command::function_CA_ELEMENTAMBIENT;
	pointers_[CA_ELEMENTDIFFUSE] = &Command::function_CA_ELEMENTDIFFUSE;
	pointers_[CA_ELEMENTRADIUS] = &Command::function_CA_ELEMENTRADIUS;
	pointers_[CA_ENERGYUNITS] = &Command::function_CA_ENERGYUNITS;
	pointers_[CA_GL] = &Command::function_CA_GL;
	pointers_[CA_HDISTANCE] = &Command::function_CA_HDISTANCE;
	pointers_[CA_INTRA] = &Command::function_CA_INTRA;
	pointers_[CA_KEY] = &Command::function_CA_KEY;
	pointers_[CA_LABELSIZE] = &Command::function_CA_LABELSIZE;
	pointers_[CA_LIGHT] = &Command::function_CA_LIGHT;
	pointers_[CA_LIGHTAMBIENT] = &Command::function_CA_LIGHTAMBIENT;
	pointers_[CA_LIGHTDIFFUSE] = &Command::function_CA_LIGHTDIFFUSE;
	pointers_[CA_LIGHTPOSITION] = &Command::function_CA_LIGHTPOSITION;
	pointers_[CA_LIGHTSPECULAR] = &Command::function_CA_LIGHTSPECULAR;
	pointers_[CA_MOUSE] = &Command::function_CA_MOUSE;
	pointers_[CA_RADIUS] = &Command::function_CA_RADIUS;
	pointers_[CA_REPLICATEFOLD] = &Command::function_CA_REPLICATEFOLD;
	pointers_[CA_REPLICATETRIM] = &Command::function_CA_REPLICATETRIM;
	pointers_[CA_SCHEME] = &Command::function_CA_SCHEME;
	pointers_[CA_SHININESS] = &Command::function_CA_SHININESS;
	pointers_[CA_SHOWONSCREEN] = &Command::function_CA_SHOWONSCREEN;
	pointers_[CA_SHOWONIMAGE] = &Command::function_CA_SHOWONIMAGE;
	pointers_[CA_STYLE] = &Command::function_CA_STYLE;
	pointers_[CA_USENICETEXT] = &Command::function_CA_USENICETEXT;
	pointers_[CA_VCUT] = &Command::function_CA_VCUT;
	pointers_[CA_VDW] = &Command::function_CA_VDW;

	// Read / Write Commands
	pointers_[CA_ADDREADOPTION] = &Command::function_CA_ADDREADOPTION;
	pointers_[CA_FIND] = &Command::function_CA_FIND;
	pointers_[CA_GETLINE] = &Command::function_CA_GETLINE;
	pointers_[CA_READCHARS] = &Command::function_CA_READCHARS;
	pointers_[CA_READFLOAT] = &Command::function_CA_READFLOAT;
	pointers_[CA_READINTEGER] = &Command::function_CA_READINTEGER;
	pointers_[CA_READLINE] = &Command::function_CA_READLINE;
	pointers_[CA_READNEXT] = &Command::function_CA_READNEXT;
	pointers_[CA_READVAR] = &Command::function_CA_READVAR;
	pointers_[CA_REMOVEREADOPTION] = &Command::function_CA_REMOVEREADOPTION;
	pointers_[CA_REWIND] = &Command::function_CA_REWIND;
	pointers_[CA_SKIPCHARS] = &Command::function_CA_SKIPCHARS;
	pointers_[CA_SKIPLINE] = &Command::function_CA_SKIPLINE;
	pointers_[CA_WRITELINE] = &Command::function_CA_WRITELINE;
	pointers_[CA_WRITEVAR] = &Command::function_CA_WRITEVAR;

	// Script Commands
	pointers_[CA_LISTSCRIPTS] = &Command::function_CA_LISTSCRIPTS;
	pointers_[CA_LOADSCRIPT] = &Command::function_CA_LOADSCRIPT;
	pointers_[CA_RUNSCRIPT] = &Command::function_CA_RUNSCRIPT;

	// Select Commands
	pointers_[CA_DESELECT] = &Command::function_CA_DESELECT;
	pointers_[CA_EXPAND] = &Command::function_CA_EXPAND;
	pointers_[CA_INVERT] = &Command::function_CA_INVERT;
	pointers_[CA_SELECT] = &Command::function_CA_SELECT;
	pointers_[CA_SELECTALL] = &Command::function_CA_SELECTALL;
	pointers_[CA_SELECTFFTYPE] = &Command::function_CA_SELECTFFTYPE;
	pointers_[CA_SELECTNONE] = &Command::function_CA_SELECTNONE;
	pointers_[CA_SELECTOVERLAPS] = &Command::function_CA_SELECTOVERLAPS;
	pointers_[CA_SELECTPATTERN] = &Command::function_CA_SELECTPATTERN;
	pointers_[CA_SELECTTYPE] = &Command::function_CA_SELECTTYPE;
	
	// Site Commands
	pointers_[CA_GETSITE] = &Command::function_CA_GETSITE;
	pointers_[CA_LISTSITES] = &Command::function_CA_LISTSITES;
	pointers_[CA_NEWSITE] = &Command::function_CA_NEWSITE;
	pointers_[CA_SITEAXES] = &Command::function_CA_SITEAXES;

	// System Commands
	pointers_[CA_DEBUG] = &Command::function_CA_DEBUG;
	pointers_[CA_GUI] = &Command::function_CA_GUI;
	pointers_[CA_HELP] = &Command::function_CA_HELP;
	pointers_[CA_SEED] = &Command::function_CA_SEED;
	pointers_[CA_QUIT] = &Command::function_CA_QUIT;
	pointers_[CA_VERSION] = &Command::function_CA_VERSION;
	
	// Trajectory Commands
	pointers_[CA_FINALISEFRAME] = &Command::function_CA_FINALISEFRAME;
	pointers_[CA_FIRSTFRAME] = &Command::function_CA_FIRSTFRAME;
	pointers_[CA_LASTFRAME] = &Command::function_CA_LASTFRAME;
	pointers_[CA_LOADTRAJECTORY] = &Command::function_CA_LOADTRAJECTORY;
	pointers_[CA_NEXTFRAME] = &Command::function_CA_NEXTFRAME;
	pointers_[CA_PREVFRAME] = &Command::function_CA_PREVFRAME;
	pointers_[CA_SEEKFRAME] = &Command::function_CA_SEEKFRAME;

	// Transform Commands
	pointers_[CA_CENTRE] = &Command::function_CA_CENTRE;
	pointers_[CA_TRANSLATE] = &Command::function_CA_TRANSLATE;
	pointers_[CA_TRANSLATEATOM] = &Command::function_CA_TRANSLATEATOM;
	pointers_[CA_TRANSLATECELL] = &Command::function_CA_TRANSLATECELL;
	pointers_[CA_MIRROR] = &Command::function_CA_MIRROR;

	// Variable Commands
	pointers_[CA_DECREASE] = &Command::function_CA_DECREASE;
	pointers_[CA_INCREASE] = &Command::function_CA_INCREASE;
	pointers_[CA_LET] = &Command::function_CA_LET;
	pointers_[CA_LETCHAR] = &Command::function_CA_LETCHAR;
	pointers_[CA_LETPTR] = &Command::function_CA_LETPTR;

	// View Commands
	pointers_[CA_GETVIEW] = &Command::function_CA_GETVIEW;
	pointers_[CA_ORTHOGRAPHIC] = &Command::function_CA_ORTHOGRAPHIC;
	pointers_[CA_PERSPECTIVE] = &Command::function_CA_PERSPECTIVE;
	pointers_[CA_RESETVIEW] = &Command::function_CA_RESETVIEW;
	pointers_[CA_ROTATEVIEW] = &Command::function_CA_ROTATEVIEW;
	pointers_[CA_SETVIEW] = &Command::function_CA_SETVIEW;
	pointers_[CA_SPEEDTEST] = &Command::function_CA_SPEEDTEST;
	pointers_[CA_TRANSLATEVIEW] = &Command::function_CA_TRANSLATEVIEW;
	pointers_[CA_VIEWALONG] = &Command::function_CA_VIEWALONG;
	pointers_[CA_VIEWALONGCELL] = &Command::function_CA_VIEWALONGCELL;
	pointers_[CA_ZOOMVIEW] = &Command::function_CA_ZOOMVIEW;
	pointers_[CA_ZROTATEVIEW] = &Command::function_CA_ZROTATEVIEW;
}

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

#include "parser/commands.h"

// Initialise Command Pointers
void NuCommand::initPointers()
{
	/*
	// Store pointers to all command functions
	*/
	pointers_[CA_ROOTNODE] = &function_CA_ROOTNODE;

	// Analyse commands
	pointers_[CA_FINALISE] = &NuCommand::function_CA_FINALISE;
	pointers_[CA_FRAMEANALYSE] = &NuCommand::function_CA_FRAMEANALYSE;
	pointers_[CA_GEOMETRY] = &NuCommand::function_CA_GEOMETRY;
	pointers_[CA_MODELANALYSE] = &NuCommand::function_CA_MODELANALYSE;
	pointers_[CA_PDENS] = &NuCommand::function_CA_PDENS;
	pointers_[CA_PRINTJOBS] = &NuCommand::function_CA_PRINTJOBS;
	pointers_[CA_RDF] = &NuCommand::function_CA_RDF;
	pointers_[CA_SAVEQUANTITIES] = &NuCommand::function_CA_SAVEQUANTITIES;
	pointers_[CA_TRAJANALYSE] = &NuCommand::function_CA_TRAJANALYSE;

	// Atom commands
	pointers_[CA_ATOMSTYLE] = &NuCommand::function_CA_ATOMSTYLE;
	pointers_[CA_GETATOM] = &NuCommand::function_CA_GETATOM;
	pointers_[CA_HIDE] = &NuCommand::function_CA_HIDE;
	pointers_[CA_SETCOORDS] = &NuCommand::function_CA_SETCOORDS;
	pointers_[CA_SETCHARGE] = &NuCommand::function_CA_SETCHARGE;
	pointers_[CA_SETELEMENT] = &NuCommand::function_CA_SETELEMENT;
	pointers_[CA_SETFORCES] = &NuCommand::function_CA_SETFORCES;
	pointers_[CA_SETFX] = &NuCommand::function_CA_SETFX;
	pointers_[CA_SETFY] = &NuCommand::function_CA_SETFY;
	pointers_[CA_SETFZ] = &NuCommand::function_CA_SETFZ;
	pointers_[CA_SETID] = &NuCommand::function_CA_SETID;
	pointers_[CA_SETRX] = &NuCommand::function_CA_SETRX;
	pointers_[CA_SETRY] = &NuCommand::function_CA_SETRY;
	pointers_[CA_SETRZ] = &NuCommand::function_CA_SETRZ;
	pointers_[CA_SETVELOCITIES] = &NuCommand::function_CA_SETVELOCITIES;
	pointers_[CA_SETVX] = &NuCommand::function_CA_SETVX;
	pointers_[CA_SETVY] = &NuCommand::function_CA_SETVY;
	pointers_[CA_SETVZ] = &NuCommand::function_CA_SETVZ;
	pointers_[CA_SHOW] = &NuCommand::function_CA_SHOW;
	pointers_[CA_SHOWALL] = &NuCommand::function_CA_SHOWALL;

	// Bond commands
	pointers_[CA_AUGMENT] = &NuCommand::function_CA_AUGMENT;
	pointers_[CA_BONDTOLERANCE] = &NuCommand::function_CA_BONDTOLERANCE;
	pointers_[CA_CLEARBONDS] = &NuCommand::function_CA_CLEARBONDS;
	pointers_[CA_CLEARSELECTEDBONDS] = &NuCommand::function_CA_CLEARSELECTEDBONDS;
	pointers_[CA_NEWBOND] = &NuCommand::function_CA_NEWBOND;
	pointers_[CA_NEWBONDID] = &NuCommand::function_CA_NEWBONDID;
	pointers_[CA_REBOND] = &NuCommand::function_CA_REBOND;
	pointers_[CA_REBONDPATTERNS] = &NuCommand::function_CA_REBONDPATTERNS;
	pointers_[CA_REBONDSELECTION] = &NuCommand::function_CA_REBONDSELECTION;

	// Build commands
	pointers_[CA_ADDHYDROGEN] = &NuCommand::function_CA_ADDHYDROGEN;
	pointers_[CA_BOHR] = &NuCommand::function_CA_BOHR;
	pointers_[CA_CHAIN] = &NuCommand::function_CA_CHAIN;
	pointers_[CA_ENDCHAIN] = &NuCommand::function_CA_ENDCHAIN;
	pointers_[CA_INSERTATOM] = &NuCommand::function_CA_INSERTATOM;
	pointers_[CA_LOCATE] = &NuCommand::function_CA_LOCATE;
	pointers_[CA_MOVE] = &NuCommand::function_CA_MOVE;
	pointers_[CA_MOVETOEND] = &NuCommand::function_CA_MOVETOEND;
	pointers_[CA_MOVETOSTART] = &NuCommand::function_CA_MOVETOSTART;
	pointers_[CA_NEWATOM] = &NuCommand::function_CA_NEWATOM;
	pointers_[CA_NEWATOMFRAC] = &NuCommand::function_CA_NEWATOMFRAC;
	pointers_[CA_REORDER] = &NuCommand::function_CA_REORDER;
	pointers_[CA_RESETPEN] = &NuCommand::function_CA_RESETPEN;
	pointers_[CA_ROTX] = &NuCommand::function_CA_ROTX;
	pointers_[CA_ROTY] = &NuCommand::function_CA_ROTY;
	pointers_[CA_ROTZ] = &NuCommand::function_CA_ROTZ;
	pointers_[CA_SHIFTDOWN] = &NuCommand::function_CA_SHIFTDOWN;
	pointers_[CA_SHIFTUP] = &NuCommand::function_CA_SHIFTUP;
	pointers_[CA_TRANSMUTE] = &NuCommand::function_CA_TRANSMUTE;

	// Cell commands
	pointers_[CA_ADDGENERATOR] = &NuCommand::function_CA_ADDGENERATOR;
	pointers_[CA_ADJUSTCELL] = &NuCommand::function_CA_ADJUSTCELL;
	pointers_[CA_CELL] = &NuCommand::function_CA_CELL;
	pointers_[CA_CELLAXES] = &NuCommand::function_CA_CELLAXES;
	pointers_[CA_FOLD] = &NuCommand::function_CA_FOLD;
	pointers_[CA_FOLDMOLECULES] = &NuCommand::function_CA_FOLDMOLECULES;
	pointers_[CA_FRACTOREAL] = &NuCommand::function_CA_FRACTOREAL;
	pointers_[CA_NOCELL] = &NuCommand::function_CA_NOCELL;
	pointers_[CA_PACK] = &NuCommand::function_CA_PACK;
	pointers_[CA_PRINTCELL] = &NuCommand::function_CA_PRINTCELL;
	pointers_[CA_REPLICATE] = &NuCommand::function_CA_REPLICATE;
	pointers_[CA_ROTATECELL] = &NuCommand::function_CA_ROTATECELL;
	pointers_[CA_SCALE] = &NuCommand::function_CA_SCALE;
	pointers_[CA_SCALEMOLECULES] = &NuCommand::function_CA_SCALEMOLECULES;
	pointers_[CA_SETCELL] = &NuCommand::function_CA_SETCELL;
	pointers_[CA_SPACEGROUP] = &NuCommand::function_CA_SPACEGROUP;

	// Charge commands
	pointers_[CA_CHARGEFF] = &NuCommand::function_CA_CHARGEFF;
	pointers_[CA_CHARGEFROMMODEL] = &NuCommand::function_CA_CHARGEFROMMODEL;
	pointers_[CA_CHARGEPATOM] = &NuCommand::function_CA_CHARGEPATOM;
	pointers_[CA_CHARGE] = &NuCommand::function_CA_CHARGE;
	pointers_[CA_CHARGETYPE] = &NuCommand::function_CA_CHARGETYPE;
	pointers_[CA_CLEARCHARGES] = &NuCommand::function_CA_CLEARCHARGES;

	// Colourscale commands
	pointers_[CA_ADDPOINT] = &NuCommand::function_CA_ADDPOINT;
	pointers_[CA_CLEARPOINTS] = &NuCommand::function_CA_CLEARPOINTS;
	pointers_[CA_LISTSCALES] = &NuCommand::function_CA_LISTSCALES;
	pointers_[CA_REMOVEPOINT] = &NuCommand::function_CA_REMOVEPOINT;
	pointers_[CA_SCALENAME] = &NuCommand::function_CA_SCALENAME;
	pointers_[CA_SCALEVISIBLE] = &NuCommand::function_CA_SCALEVISIBLE;
	pointers_[CA_SETPOINT] = &NuCommand::function_CA_SETPOINT;
	pointers_[CA_SETPOINTCOLOUR] = &NuCommand::function_CA_SETPOINTCOLOUR;
	pointers_[CA_SETPOINTVALUE] = &NuCommand::function_CA_SETPOINTVALUE;

	// Disordered build commands
	pointers_[CA_DISORDER] = &NuCommand::function_CA_DISORDER;
	pointers_[CA_LISTCOMPONENTS] = &NuCommand::function_CA_LISTCOMPONENTS;
	pointers_[CA_NMOLS] = &NuCommand::function_CA_NMOLS;
	pointers_[CA_REGION] = &NuCommand::function_CA_REGION;
	pointers_[CA_REGIONCENTRE] = &NuCommand::function_CA_REGIONCENTRE;
	pointers_[CA_REGIONCENTREF] = &NuCommand::function_CA_REGIONCENTREF;
	pointers_[CA_REGIONF] = &NuCommand::function_CA_REGIONF;
	pointers_[CA_REGIONGEOMETRY] = &NuCommand::function_CA_REGIONGEOMETRY;
	pointers_[CA_REGIONGEOMETRYF] = &NuCommand::function_CA_REGIONGEOMETRYF;
	pointers_[CA_REGIONOVERLAPS] = &NuCommand::function_CA_REGIONOVERLAPS;
	pointers_[CA_REGIONSHAPE] = &NuCommand::function_CA_REGIONSHAPE;
	pointers_[CA_VDWSCALE] = &NuCommand::function_CA_VDWSCALE;

	// Edit Commands
	pointers_[CA_DELETE] = &NuCommand::function_CA_DELETE;
	pointers_[CA_COPY] = &NuCommand::function_CA_COPY;
	pointers_[CA_CUT] = &NuCommand::function_CA_CUT;
	pointers_[CA_PASTE] = &NuCommand::function_CA_PASTE;
	pointers_[CA_REDO] = &NuCommand::function_CA_REDO;
	pointers_[CA_UNDO] = &NuCommand::function_CA_UNDO;

	// Energy Commands
	pointers_[CA_FRAMEENERGY] = &NuCommand::function_CA_FRAMEENERGY;
	pointers_[CA_MODELENERGY] = &NuCommand::function_CA_MODELENERGY;
	pointers_[CA_PRINTELEC] = &NuCommand::function_CA_PRINTELEC;
	pointers_[CA_PRINTEWALD] = &NuCommand::function_CA_PRINTEWALD;
	pointers_[CA_PRINTINTER] = &NuCommand::function_CA_PRINTINTER;
	pointers_[CA_PRINTINTRA] = &NuCommand::function_CA_PRINTINTRA;
	pointers_[CA_PRINTENERGY] = &NuCommand::function_CA_PRINTENERGY;
	pointers_[CA_PRINTSUMMARY] = &NuCommand::function_CA_PRINTSUMMARY;
	pointers_[CA_PRINTVDW] = &NuCommand::function_CA_PRINTVDW;

	// Flow control
	pointers_[CA_BREAK] = &NuCommand::function_CA_BREAK;
	pointers_[CA_CONTINUE] = &NuCommand::function_CA_CONTINUE;
	pointers_[CA_ELSE] = &NuCommand::function_CA_ELSE;
	pointers_[CA_ELSEIF] = &NuCommand::function_CA_ELSEIF;
	pointers_[CA_END] = &NuCommand::function_CA_END;
	pointers_[CA_FOR] = &NuCommand::function_CA_FOR;
	pointers_[CA_GOTO] = &NuCommand::function_CA_GOTO;
	pointers_[CA_GOTONONIF] = &NuCommand::function_CA_GOTONONIF;
	pointers_[CA_IF] = &NuCommand::function_CA_IF;
	pointers_[CA_TERMINATE] = &NuCommand::function_CA_TERMINATE;

	// Force Commands
	pointers_[CA_FRAMEFORCES] = &NuCommand::function_CA_FRAMEFORCES;
	pointers_[CA_MODELFORCES] = &NuCommand::function_CA_MODELFORCES;
	pointers_[CA_PRINTFORCES] = &NuCommand::function_CA_PRINTFORCES;

	// Forcefield Commands
        pointers_[CA_ANGLEDEF] = &NuCommand::function_CA_ANGLEDEF;
	pointers_[CA_BONDDEF] = &NuCommand::function_CA_BONDDEF;
	pointers_[CA_CLEARMAP] = &NuCommand::function_CA_CLEARMAP;
	pointers_[CA_CREATEEXPRESSION] = &NuCommand::function_CA_CREATEEXPRESSION;
	pointers_[CA_DEFAULTFF] = &NuCommand::function_CA_DEFAULTFF;
	pointers_[CA_EQUIVALENT] = &NuCommand::function_CA_EQUIVALENT;
	pointers_[CA_FFMODEL] = &NuCommand::function_CA_FFMODEL;
	pointers_[CA_FFPATTERN] = &NuCommand::function_CA_FFPATTERN;
	pointers_[CA_FFPATTERNID] = &NuCommand::function_CA_FFPATTERNID;
	pointers_[CA_FINALISEFF] = &NuCommand::function_CA_FINALISEFF;
	pointers_[CA_GENCONVERT] = &NuCommand::function_CA_GENCONVERT;
	pointers_[CA_GENERATOR] = &NuCommand::function_CA_GENERATOR;
	pointers_[CA_GETFF] = &NuCommand::function_CA_GETFF;
	pointers_[CA_INTERDEF] = &NuCommand::function_CA_INTERDEF;
	pointers_[CA_LOADFF] = &NuCommand::function_CA_LOADFF;
	pointers_[CA_MAP] = &NuCommand::function_CA_MAP;
	pointers_[CA_NEWFF] = &NuCommand::function_CA_NEWFF;
	pointers_[CA_PRINTSETUP] = &NuCommand::function_CA_PRINTSETUP;
	pointers_[CA_RULES] = &NuCommand::function_CA_RULES;
	pointers_[CA_SAVEEXPRESSION] = &NuCommand::function_CA_SAVEEXPRESSION;
	pointers_[CA_TORSIONDEF] = &NuCommand::function_CA_TORSIONDEF;
	pointers_[CA_TYPEDEF] = &NuCommand::function_CA_TYPEDEF;
	pointers_[CA_TYPEMODEL] = &NuCommand::function_CA_TYPEMODEL;
	pointers_[CA_TYPETEST] = &NuCommand::function_CA_TYPETEST;
	pointers_[CA_UNITS] = &NuCommand::function_CA_UNITS;

	// Glyph commands
	pointers_[CA_AUTOELLIPSOIDS] = &NuCommand::function_CA_AUTOELLIPSOIDS;
	pointers_[CA_AUTOPOLYHEDRA] = &NuCommand::function_CA_AUTOPOLYHEDRA;
	pointers_[CA_GLYPHATOMF] = &NuCommand::function_CA_GLYPHATOMF;
	pointers_[CA_GLYPHATOMR] = &NuCommand::function_CA_GLYPHATOMR;
	pointers_[CA_GLYPHATOMV] = &NuCommand::function_CA_GLYPHATOMV;
	pointers_[CA_GLYPHATOMSF] = &NuCommand::function_CA_GLYPHATOMSF;
	pointers_[CA_GLYPHATOMSR] = &NuCommand::function_CA_GLYPHATOMSR;
	pointers_[CA_GLYPHATOMSV] = &NuCommand::function_CA_GLYPHATOMSV;
	pointers_[CA_GLYPHCOLOUR] = &NuCommand::function_CA_GLYPHCOLOUR;
	pointers_[CA_GLYPHDATA] = &NuCommand::function_CA_GLYPHDATA;
	pointers_[CA_GLYPHSOLID] = &NuCommand::function_CA_GLYPHSOLID;
	pointers_[CA_GLYPHTEXT] = &NuCommand::function_CA_GLYPHTEXT;
	pointers_[CA_NEWGLYPH] = &NuCommand::function_CA_NEWGLYPH;

	// Grid Commands
	pointers_[CA_ADDGRIDPOINT] = &NuCommand::function_CA_ADDGRIDPOINT;
	pointers_[CA_ADDNEXTGRIDPOINT] = &NuCommand::function_CA_ADDNEXTGRIDPOINT;
	pointers_[CA_FINALISEGRID] = &NuCommand::function_CA_FINALISEGRID;
	pointers_[CA_GRIDALPHA] = &NuCommand::function_CA_GRIDALPHA;
	pointers_[CA_GRIDAXES] = &NuCommand::function_CA_GRIDAXES;
	pointers_[CA_GRIDCOLOUR] = &NuCommand::function_CA_GRIDCOLOUR;
	pointers_[CA_GRIDCOLOURNEGATIVE] = &NuCommand::function_CA_GRIDCOLOURNEGATIVE;
	pointers_[CA_GRIDCOLOURSCALE] = &NuCommand::function_CA_GRIDCOLOURSCALE;
	pointers_[CA_GRIDCUBIC] = &NuCommand::function_CA_GRIDCUBIC;
	pointers_[CA_GRIDCUTOFF] = &NuCommand::function_CA_GRIDCUTOFF;
	pointers_[CA_GRIDLOOPORDER] = &NuCommand::function_CA_GRIDLOOPORDER;
	pointers_[CA_GRIDORIGIN] = &NuCommand::function_CA_GRIDORIGIN;
	pointers_[CA_GRIDORTHO] = &NuCommand::function_CA_GRIDORTHO;
	pointers_[CA_GRIDSIZE] = &NuCommand::function_CA_GRIDSIZE;
	pointers_[CA_GRIDSTYLE] = &NuCommand::function_CA_GRIDSTYLE;
	pointers_[CA_GRIDSYMMETRIC] = &NuCommand::function_CA_GRIDSYMMETRIC;
	pointers_[CA_GRIDUSEZ] = &NuCommand::function_CA_GRIDUSEZ;
	pointers_[CA_LOADGRID] = &NuCommand::function_CA_LOADGRID;
	pointers_[CA_NEWGRID] = &NuCommand::function_CA_NEWGRID;

	// Image Commands
	pointers_[CA_SAVEBITMAP] = &NuCommand::function_CA_SAVEBITMAP;
	pointers_[CA_SAVEVECTOR] = &NuCommand::function_CA_SAVEVECTOR;

	// Labeling Commands
	pointers_[CA_CLEARLABELS] = &NuCommand::function_CA_CLEARLABELS;
	pointers_[CA_LABEL] = &NuCommand::function_CA_LABEL;
	pointers_[CA_REMOVELABEL] = &NuCommand::function_CA_REMOVELABEL;
	pointers_[CA_REMOVELABELS] = &NuCommand::function_CA_REMOVELABELS;

	// MC Commands
	pointers_[CA_MCACCEPT] = &NuCommand::function_CA_MCACCEPT;
	pointers_[CA_MCALLOW] = &NuCommand::function_CA_MCALLOW;
	pointers_[CA_MCMAXSTEP] = &NuCommand::function_CA_MCMAXSTEP;
	pointers_[CA_MCNTRIALS] = &NuCommand::function_CA_MCNTRIALS;
	pointers_[CA_PRINTMC] = &NuCommand::function_CA_PRINTMC;

	// Measurement Commands
	pointers_[CA_ANGLE] = &NuCommand::function_CA_ANGLE;
	pointers_[CA_ANGLES] = &NuCommand::function_CA_ANGLES;
	pointers_[CA_CLEARMEASUREMENTS] = &NuCommand::function_CA_CLEARMEASUREMENTS;
	pointers_[CA_DISTANCE] = &NuCommand::function_CA_DISTANCE;
	pointers_[CA_DISTANCES] = &NuCommand::function_CA_DISTANCES;
	pointers_[CA_LISTMEASUREMENTS] = &NuCommand::function_CA_LISTMEASUREMENTS;
	pointers_[CA_MEASURE] = &NuCommand::function_CA_MEASURE;
	pointers_[CA_TORSION] = &NuCommand::function_CA_TORSION;
	pointers_[CA_TORSIONS] = &NuCommand::function_CA_TORSIONS;

	// Messaging Commands
	pointers_[CA_ERROR] = &NuCommand::function_CA_ERROR;
	pointers_[CA_PRINT] = &NuCommand::function_CA_PRINT;
	pointers_[CA_VERBOSE] = &NuCommand::function_CA_VERBOSE;
	pointers_[CA_WARN] = &NuCommand::function_CA_WARN;

	// Minimisation Commands
	pointers_[CA_CGMINIMISE] = &NuCommand::function_CA_CGMINIMISE;
	pointers_[CA_CONVERGE] = &NuCommand::function_CA_CONVERGE;
	pointers_[CA_LINETOL] = &NuCommand::function_CA_LINETOL;
	pointers_[CA_MCMINIMISE] = &NuCommand::function_CA_MCMINIMISE;
	pointers_[CA_SDMINIMISE] = &NuCommand::function_CA_SDMINIMISE;
	pointers_[CA_SIMPLEXMINIMISE] = &NuCommand::function_CA_SIMPLEXMINIMISE;
	
	// Model Commands
	pointers_[CA_CREATEATOMS] = &NuCommand::function_CA_CREATEATOMS;
	pointers_[CA_CURRENTMODEL] = &NuCommand::function_CA_CURRENTMODEL;
	pointers_[CA_FINALISEMODEL] = &NuCommand::function_CA_FINALISEMODEL;
	pointers_[CA_FIRSTMODEL] = &NuCommand::function_CA_FIRSTMODEL;
	pointers_[CA_GETMODEL] = &NuCommand::function_CA_GETMODEL;
	pointers_[CA_LASTMODEL] = &NuCommand::function_CA_LASTMODEL;
	pointers_[CA_LISTMODELS] = &NuCommand::function_CA_LISTMODELS;
	pointers_[CA_LOADMODEL] = &NuCommand::function_CA_LOADMODEL;
	pointers_[CA_LOGINFO] = &NuCommand::function_CA_LOGINFO;
	pointers_[CA_MODELTEMPLATE] = &NuCommand::function_CA_MODELTEMPLATE;
	pointers_[CA_NEWMODEL] = &NuCommand::function_CA_NEWMODEL;
	pointers_[CA_NEXTMODEL] = &NuCommand::function_CA_NEXTMODEL;
	pointers_[CA_PREVMODEL] = &NuCommand::function_CA_PREVMODEL;
	pointers_[CA_INFO] = &NuCommand::function_CA_INFO;
	pointers_[CA_SAVEMODEL] = &NuCommand::function_CA_SAVEMODEL;
	pointers_[CA_SETNAME] = &NuCommand::function_CA_SETNAME;

	// Pattern Commands
	pointers_[CA_CLEARPATTERNS] = &NuCommand::function_CA_CLEARPATTERNS;
	pointers_[CA_CREATEPATTERNS] = &NuCommand::function_CA_CREATEPATTERNS;
	pointers_[CA_GETPATTERN] = &NuCommand::function_CA_GETPATTERN;
	pointers_[CA_LISTPATTERNS] = &NuCommand::function_CA_LISTPATTERNS;
	pointers_[CA_NEWPATTERN] = &NuCommand::function_CA_NEWPATTERN;

	// Preferences Commands
	pointers_[CA_ANGLELABEL] = &NuCommand::function_CA_ANGLELABEL;
	pointers_[CA_ATOMDETAIL] = &NuCommand::function_CA_ATOMDETAIL;
	pointers_[CA_BONDDETAIL] = &NuCommand::function_CA_BONDDETAIL;
	pointers_[CA_COLOUR] = &NuCommand::function_CA_COLOUR;
	pointers_[CA_COMMONELEMENTS] = &NuCommand::function_CA_COMMONELEMENTS;
	pointers_[CA_DENSITYUNITS] = &NuCommand::function_CA_DENSITYUNITS;
	pointers_[CA_DISTANCELABEL] = &NuCommand::function_CA_DISTANCELABEL;
	pointers_[CA_ECUT] = &NuCommand::function_CA_ECUT;
	pointers_[CA_ELEC] = &NuCommand::function_CA_ELEC;
	pointers_[CA_ELEMENTAMBIENT] = &NuCommand::function_CA_ELEMENTAMBIENT;
	pointers_[CA_ELEMENTDIFFUSE] = &NuCommand::function_CA_ELEMENTDIFFUSE;
	pointers_[CA_ELEMENTRADIUS] = &NuCommand::function_CA_ELEMENTRADIUS;
	pointers_[CA_ENERGYUNITS] = &NuCommand::function_CA_ENERGYUNITS;
	pointers_[CA_GL] = &NuCommand::function_CA_GL;
	pointers_[CA_HDISTANCE] = &NuCommand::function_CA_HDISTANCE;
	pointers_[CA_INTRA] = &NuCommand::function_CA_INTRA;
	pointers_[CA_KEY] = &NuCommand::function_CA_KEY;
	pointers_[CA_LABELSIZE] = &NuCommand::function_CA_LABELSIZE;
	pointers_[CA_LIGHT] = &NuCommand::function_CA_LIGHT;
	pointers_[CA_LIGHTAMBIENT] = &NuCommand::function_CA_LIGHTAMBIENT;
	pointers_[CA_LIGHTDIFFUSE] = &NuCommand::function_CA_LIGHTDIFFUSE;
	pointers_[CA_LIGHTPOSITION] = &NuCommand::function_CA_LIGHTPOSITION;
	pointers_[CA_LIGHTSPECULAR] = &NuCommand::function_CA_LIGHTSPECULAR;
	pointers_[CA_MOUSE] = &NuCommand::function_CA_MOUSE;
	pointers_[CA_RADIUS] = &NuCommand::function_CA_RADIUS;
	pointers_[CA_REPLICATEFOLD] = &NuCommand::function_CA_REPLICATEFOLD;
	pointers_[CA_REPLICATETRIM] = &NuCommand::function_CA_REPLICATETRIM;
	pointers_[CA_SCHEME] = &NuCommand::function_CA_SCHEME;
	pointers_[CA_SHININESS] = &NuCommand::function_CA_SHININESS;
	pointers_[CA_SHOWONSCREEN] = &NuCommand::function_CA_SHOWONSCREEN;
	pointers_[CA_SHOWONIMAGE] = &NuCommand::function_CA_SHOWONIMAGE;
	pointers_[CA_STYLE] = &NuCommand::function_CA_STYLE;
	pointers_[CA_SWAPBUFFERS] = &NuCommand::function_CA_SWAPBUFFERS;
	pointers_[CA_USENICETEXT] = &NuCommand::function_CA_USENICETEXT;
	pointers_[CA_VCUT] = &NuCommand::function_CA_VCUT;
	pointers_[CA_VDW] = &NuCommand::function_CA_VDW;
	pointers_[CA_ZOOMTHROTTLE] = &NuCommand::function_CA_ZOOMTHROTTLE;

	// Read / Write Commands
	pointers_[CA_ADDREADOPTION] = &NuCommand::function_CA_ADDREADOPTION;
	pointers_[CA_FIND] = &NuCommand::function_CA_FIND;
	pointers_[CA_GETLINE] = &NuCommand::function_CA_GETLINE;
	pointers_[CA_READCHARS] = &NuCommand::function_CA_READCHARS;
	pointers_[CA_READFLOAT] = &NuCommand::function_CA_READFLOAT;
	pointers_[CA_READINTEGER] = &NuCommand::function_CA_READINTEGER;
	pointers_[CA_READLINE] = &NuCommand::function_CA_READLINE;
	pointers_[CA_READNEXT] = &NuCommand::function_CA_READNEXT;
	pointers_[CA_READVAR] = &NuCommand::function_CA_READVAR;
	pointers_[CA_REMOVEREADOPTION] = &NuCommand::function_CA_REMOVEREADOPTION;
	pointers_[CA_REWIND] = &NuCommand::function_CA_REWIND;
	pointers_[CA_SKIPCHARS] = &NuCommand::function_CA_SKIPCHARS;
	pointers_[CA_SKIPLINE] = &NuCommand::function_CA_SKIPLINE;
	pointers_[CA_WRITELINE] = &NuCommand::function_CA_WRITELINE;
	pointers_[CA_WRITEVAR] = &NuCommand::function_CA_WRITEVAR;

	// Script Commands
	pointers_[CA_LISTSCRIPTS] = &NuCommand::function_CA_LISTSCRIPTS;
	pointers_[CA_LOADSCRIPT] = &NuCommand::function_CA_LOADSCRIPT;
	pointers_[CA_RUNSCRIPT] = &NuCommand::function_CA_RUNSCRIPT;

	// Select Commands
	pointers_[CA_DESELECT] = &NuCommand::function_CA_DESELECT;
	pointers_[CA_DESELECTTYPE] = &NuCommand::function_CA_DESELECTTYPE;
	pointers_[CA_EXPAND] = &NuCommand::function_CA_EXPAND;
	pointers_[CA_INVERT] = &NuCommand::function_CA_INVERT;
	pointers_[CA_SELECT] = &NuCommand::function_CA_SELECT;
	pointers_[CA_SELECTALL] = &NuCommand::function_CA_SELECTALL;
	pointers_[CA_SELECTFFTYPE] = &NuCommand::function_CA_SELECTFFTYPE;
	pointers_[CA_SELECTIONCOG] = &NuCommand::function_CA_SELECTIONCOG;
	pointers_[CA_SELECTIONCOM] = &NuCommand::function_CA_SELECTIONCOM;
	pointers_[CA_SELECTNONE] = &NuCommand::function_CA_SELECTNONE;
	pointers_[CA_SELECTOVERLAPS] = &NuCommand::function_CA_SELECTOVERLAPS;
	pointers_[CA_SELECTPATTERN] = &NuCommand::function_CA_SELECTPATTERN;
	pointers_[CA_SELECTTYPE] = &NuCommand::function_CA_SELECTTYPE;
	
	// Site Commands
	pointers_[CA_GETSITE] = &NuCommand::function_CA_GETSITE;
	pointers_[CA_LISTSITES] = &NuCommand::function_CA_LISTSITES;
	pointers_[CA_NEWSITE] = &NuCommand::function_CA_NEWSITE;
	pointers_[CA_SITEAXES] = &NuCommand::function_CA_SITEAXES;

	// System Commands
	pointers_[CA_DEBUG] = &NuCommand::function_CA_DEBUG;
	pointers_[CA_GUI] = &NuCommand::function_CA_GUI;
	pointers_[CA_HELP] = &NuCommand::function_CA_HELP;
	pointers_[CA_SEED] = &NuCommand::function_CA_SEED;
	pointers_[CA_QUIT] = &NuCommand::function_CA_QUIT;
	pointers_[CA_VERSION] = &NuCommand::function_CA_VERSION;
	
	// Trajectory Commands
	pointers_[CA_FINALISEFRAME] = &NuCommand::function_CA_FINALISEFRAME;
	pointers_[CA_FIRSTFRAME] = &NuCommand::function_CA_FIRSTFRAME;
	pointers_[CA_LASTFRAME] = &NuCommand::function_CA_LASTFRAME;
	pointers_[CA_LOADTRAJECTORY] = &NuCommand::function_CA_LOADTRAJECTORY;
	pointers_[CA_NEXTFRAME] = &NuCommand::function_CA_NEXTFRAME;
	pointers_[CA_PREVFRAME] = &NuCommand::function_CA_PREVFRAME;
	pointers_[CA_SEEKFRAME] = &NuCommand::function_CA_SEEKFRAME;

	// Transform Commands
	pointers_[CA_AXISROTATE] = &NuCommand::function_CA_AXISROTATE;
	pointers_[CA_CENTRE] = &NuCommand::function_CA_CENTRE;
	pointers_[CA_TRANSLATE] = &NuCommand::function_CA_TRANSLATE;
	pointers_[CA_TRANSLATEATOM] = &NuCommand::function_CA_TRANSLATEATOM;
	pointers_[CA_TRANSLATECELL] = &NuCommand::function_CA_TRANSLATECELL;
	pointers_[CA_MATRIXCONVERT] = &NuCommand::function_CA_MATRIXCONVERT;
	pointers_[CA_MATRIXTRANSFORM] = &NuCommand::function_CA_MATRIXTRANSFORM;
	pointers_[CA_MIRROR] = &NuCommand::function_CA_MIRROR;

	// Variable Commands
	pointers_[CA_AFTERCHAR] = &NuCommand::function_CA_AFTERCHAR;
	pointers_[CA_BEFORECHAR] = &NuCommand::function_CA_BEFORECHAR;
	pointers_[CA_DECREASE] = &NuCommand::function_CA_DECREASE;
	pointers_[CA_INCREASE] = &NuCommand::function_CA_INCREASE;
	pointers_[CA_LET] = &NuCommand::function_CA_LET;
	pointers_[CA_LETCHAR] = &NuCommand::function_CA_LETCHAR;
	pointers_[CA_LETPTR] = &NuCommand::function_CA_LETPTR;
	pointers_[CA_LETVECTOR] = &NuCommand::function_CA_LETVECTOR;
	pointers_[CA_NORMALISE] = &NuCommand::function_CA_NORMALISE;	
	pointers_[CA_STRIPCHARS] = &NuCommand::function_CA_STRIPCHARS;

	// View Commands
	pointers_[CA_GETVIEW] = &NuCommand::function_CA_GETVIEW;
	pointers_[CA_ORTHOGRAPHIC] = &NuCommand::function_CA_ORTHOGRAPHIC;
	pointers_[CA_PERSPECTIVE] = &NuCommand::function_CA_PERSPECTIVE;
	pointers_[CA_RESETVIEW] = &NuCommand::function_CA_RESETVIEW;
	pointers_[CA_ROTATEVIEW] = &NuCommand::function_CA_ROTATEVIEW;
	pointers_[CA_SETVIEW] = &NuCommand::function_CA_SETVIEW;
	pointers_[CA_SPEEDTEST] = &NuCommand::function_CA_SPEEDTEST;
	pointers_[CA_TRANSLATEVIEW] = &NuCommand::function_CA_TRANSLATEVIEW;
	pointers_[CA_VIEWALONG] = &NuCommand::function_CA_VIEWALONG;
	pointers_[CA_VIEWALONGCELL] = &NuCommand::function_CA_VIEWALONGCELL;
	pointers_[CA_ZOOMVIEW] = &NuCommand::function_CA_ZOOMVIEW;
	pointers_[CA_ZROTATEVIEW] = &NuCommand::function_CA_ZROTATEVIEW;
}

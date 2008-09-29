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

	data_[CA_ROOTNODE].function = &function_CA_ROOTNODE;

	// Analyse commands
	data_[CA_FINALISE].function = &function_CA_FINALISE;
	data_[CA_FRAMEANALYSE].function = &function_CA_FRAMEANALYSE;
	data_[CA_GEOMETRY].function = &function_CA_GEOMETRY;
	data_[CA_MODELANALYSE].function = &function_CA_MODELANALYSE;
	data_[CA_PDENS].function = &function_CA_PDENS;
	data_[CA_PRINTJOBS].function = &function_CA_PRINTJOBS;
	data_[CA_RDF].function = &function_CA_RDF;
	data_[CA_SAVEQUANTITIES].function = &function_CA_SAVEQUANTITIES;
	data_[CA_TRAJANALYSE].function = &function_CA_TRAJANALYSE;

	// Atom commands
	data_[CA_ATOMSTYLE].function = &function_CA_ATOMSTYLE;
	data_[CA_GETATOM].function = &function_CA_GETATOM;
	data_[CA_HIDE].function = &function_CA_HIDE;
	data_[CA_SETCOORDS].function = &function_CA_SETCOORDS;
	data_[CA_SETCHARGE].function = &function_CA_SETCHARGE;
	data_[CA_SETELEMENT].function = &function_CA_SETELEMENT;
	data_[CA_SETFORCES].function = &function_CA_SETFORCES;
	data_[CA_SETFX].function = &function_CA_SETFX;
	data_[CA_SETFY].function = &function_CA_SETFY;
	data_[CA_SETFZ].function = &function_CA_SETFZ;
	data_[CA_SETID].function = &function_CA_SETID;
	data_[CA_SETRX].function = &function_CA_SETRX;
	data_[CA_SETRY].function = &function_CA_SETRY;
	data_[CA_SETRZ].function = &function_CA_SETRZ;
	data_[CA_SETVELOCITIES].function = &function_CA_SETVELOCITIES;
	data_[CA_SETVX].function = &function_CA_SETVX;
	data_[CA_SETVY].function = &function_CA_SETVY;
	data_[CA_SETVZ].function = &function_CA_SETVZ;
	data_[CA_SHOW].function = &function_CA_SHOW;
	data_[CA_SHOWALL].function = &function_CA_SHOWALL;

	// Bond commands
	data_[CA_AUGMENT].function = &function_CA_AUGMENT;
	data_[CA_BONDTOLERANCE].function = &function_CA_BONDTOLERANCE;
	data_[CA_GETBOND].function = &function_CA_GETBOND;
	data_[CA_NEWBOND].function = &function_CA_NEWBOND;
	data_[CA_NEWBONDID].function = &function_CA_NEWBONDID;
	data_[CA_REBONDPATTERNS].function = &function_CA_REBONDPATTERNS;
	data_[CA_REBONDSELECTION].function = &function_CA_REBONDSELECTION;
	data_[CA_CLEARBONDS].function = &function_CA_CLEARBONDS;
	data_[CA_REBOND].function = &function_CA_REBOND;

	// Build commands
	data_[CA_ADDHYDROGEN].function = &function_CA_ADDHYDROGEN;
	data_[CA_BOHR].function = &function_CA_BOHR;
	data_[CA_CHAIN].function = &function_CA_CHAIN;
	data_[CA_ENDCHAIN].function = &function_CA_ENDCHAIN;
	data_[CA_LOCATE].function = &function_CA_LOCATE;
	data_[CA_MOVE].function = &function_CA_MOVE;
	data_[CA_NEWATOM].function = &function_CA_NEWATOM;
	data_[CA_NEWATOMFRAC].function = &function_CA_NEWATOMFRAC;
	data_[CA_RESETPEN].function = &function_CA_RESETPEN;
	data_[CA_ROTX].function = &function_CA_ROTX;
	data_[CA_ROTY].function = &function_CA_ROTY;
	data_[CA_ROTZ].function = &function_CA_ROTZ;
	data_[CA_SHIFTDOWN].function = &function_CA_SHIFTDOWN;
	data_[CA_SHIFTUP].function = &function_CA_SHIFTUP;
	data_[CA_TOEND].function = &function_CA_TOEND;
	data_[CA_TOSTART].function = &function_CA_TOSTART;
	data_[CA_TRANSMUTE].function = &function_CA_TRANSMUTE;

	// Cell commands
	data_[CA_ADJUSTCELL].function = &function_CA_ADJUSTCELL;
	data_[CA_FOLD].function = &function_CA_FOLD;
	data_[CA_FOLDMOLECULES].function = &function_CA_FOLDMOLECULES;
	data_[CA_FRACTOREAL].function = &function_CA_FRACTOREAL;
	data_[CA_PACK].function = &function_CA_PACK;
	data_[CA_PRINTCELL].function = &function_CA_PRINTCELL;
	data_[CA_REPLICATE].function = &function_CA_REPLICATE;
	data_[CA_SCALE].function = &function_CA_SCALE;
	data_[CA_CELL].function = &function_CA_CELL;
	data_[CA_CELLAXES].function = &function_CA_CELLAXES;
	data_[CA_NOCELL].function = &function_CA_NOCELL;
	data_[CA_SETCELL].function = &function_CA_SETCELL;
	data_[CA_SPACEGROUP].function = &function_CA_SPACEGROUP;

	// Charge commands
	data_[CA_CHARGEFF].function = &function_CA_CHARGEFF;
	data_[CA_CHARGEFROMMODEL].function = &function_CA_CHARGEFROMMODEL;
	data_[CA_CHARGEPATOM].function = &function_CA_CHARGEPATOM;
	data_[CA_CHARGE].function = &function_CA_CHARGE;
	data_[CA_CHARGETYPE].function = &function_CA_CHARGETYPE;
	data_[CA_CLEARCHARGES].function = &function_CA_CLEARCHARGES;

	// Colourscale commands
	data_[CA_ADDPOINT].function = &function_CA_ADDPOINT;
	data_[CA_CLEARPOINTS].function = &function_CA_CLEARPOINTS;
	data_[CA_LISTSCALES].function = &function_CA_LISTSCALES;
	data_[CA_REMOVEPOINT].function = &function_CA_REMOVEPOINT;
	data_[CA_SCALENAME].function = &function_CA_SCALENAME;
	data_[CA_SCALEVISIBLE].function = &function_CA_SCALEVISIBLE;
	data_[CA_SETPOINT].function = &function_CA_SETPOINT;
	data_[CA_SETPOINTCOLOUR].function = &function_CA_SETPOINTCOLOUR;
	data_[CA_SETPOINTVALUE].function = &function_CA_SETPOINTVALUE;

	// Disordered build commands
	data_[CA_DISORDER].function = &function_CA_DISORDER;
	data_[CA_LISTCOMPONENTS].function = &function_CA_LISTCOMPONENTS;
	data_[CA_NMOLS].function = &function_CA_NMOLS;
	data_[CA_REGION].function = &function_CA_REGION;
	data_[CA_REGIONCENTRE].function = &function_CA_REGIONCENTRE;
	data_[CA_REGIONCENTREF].function = &function_CA_REGIONCENTREF;
	data_[CA_REGIONF].function = &function_CA_REGIONF;
	data_[CA_REGIONGEOMETRY].function = &function_CA_REGIONGEOMETRY;
	data_[CA_REGIONGEOMETRYF].function = &function_CA_REGIONGEOMETRYF;
	data_[CA_REGIONOVERLAPS].function = &function_CA_REGIONOVERLAPS;
	data_[CA_REGIONSHAPE].function = &function_CA_REGIONSHAPE;
	data_[CA_VDWSCALE].function = &function_CA_VDWSCALE;

	// Edit Commands
	data_[CA_DELETE].function = &function_CA_DELETE;
	data_[CA_COPY].function = &function_CA_COPY;
	data_[CA_CUT].function = &function_CA_CUT;
	data_[CA_PASTE].function = &function_CA_PASTE;
	data_[CA_REDO].function = &function_CA_REDO;
	data_[CA_UNDO].function = &function_CA_UNDO;

	// Energy Commands
	data_[CA_FRAMEENERGY].function = &function_CA_FRAMEENERGY;
	data_[CA_MODELENERGY].function = &function_CA_MODELENERGY;
	data_[CA_PRINTELEC].function = &function_CA_PRINTELEC;
	data_[CA_PRINTEWALD].function = &function_CA_PRINTEWALD;
	data_[CA_PRINTINTER].function = &function_CA_PRINTINTER;
	data_[CA_PRINTINTRA].function = &function_CA_PRINTINTRA;
	data_[CA_PRINTENERGY].function = &function_CA_PRINTENERGY;
	data_[CA_PRINTSUMMARY].function = &function_CA_PRINTSUMMARY;
	data_[CA_PRINTVDW].function = &function_CA_PRINTVDW;

	// Flow control
	data_[CA_BREAK].function = &function_CA_BREAK;
	data_[CA_CONTINUE].function = &function_CA_CONTINUE;
	data_[CA_ELSE].function = &function_CA_ELSE;
	data_[CA_ELSEIF].function = &function_CA_ELSEIF;
	data_[CA_END].function = &function_CA_END;
	data_[CA_FOR].function = &function_CA_FOR;
	data_[CA_GOTO].function = &function_CA_GOTO;
	data_[CA_GOTONONIF].function = &function_CA_GOTONONIF;
	data_[CA_IF].function = &function_CA_IF;
	data_[CA_TERMINATE].function = &function_CA_TERMINATE;

	// Force Commands
	data_[CA_FRAMEFORCES].function = &function_CA_FRAMEFORCES;
	data_[CA_MODELFORCES].function = &function_CA_MODELFORCES;
	data_[CA_PRINTFORCES].function = &function_CA_PRINTFORCES;

	// Forcefield Commands
        data_[CA_ANGLEDEF].function = &function_CA_ANGLEDEF;
        data_[CA_BONDDEF].function = &function_CA_BONDDEF;
	data_[CA_CLEARMAP].function = &function_CA_CLEARMAP;
	data_[CA_CREATEEXPRESSION].function = &function_CA_CREATEEXPRESSION;
	data_[CA_DEFAULTFF].function = &function_CA_DEFAULTFF;
        data_[CA_EQUIVALENT].function = &function_CA_EQUIVALENT;
	data_[CA_FFMODEL].function = &function_CA_FFMODEL;
	data_[CA_FFPATTERN].function = &function_CA_FFPATTERN;
	data_[CA_FFPATTERNID].function = &function_CA_FFPATTERNID;
        data_[CA_FINALISEFF].function = &function_CA_FINALISEFF;
        data_[CA_GENCONVERT].function = &function_CA_GENCONVERT;
        data_[CA_GENERATOR].function = &function_CA_GENERATOR;
	data_[CA_GETFF].function = &function_CA_GETFF;
	data_[CA_LOADFF].function = &function_CA_LOADFF;
	data_[CA_MAP].function = &function_CA_MAP;
        data_[CA_NEWFF].function = &function_CA_NEWFF;
	data_[CA_PRINTSETUP].function = &function_CA_PRINTSETUP;
        data_[CA_RULES].function = &function_CA_RULES;
	data_[CA_SAVEEXPRESSION].function = &function_CA_SAVEEXPRESSION;
        data_[CA_TORSIONDEF].function = &function_CA_TORSIONDEF;
        data_[CA_TYPEDEF].function = &function_CA_TYPEDEF;
	data_[CA_TYPEMODEL].function = &function_CA_TYPEMODEL;
	data_[CA_TYPETEST].function = &function_CA_TYPETEST;
        data_[CA_UNITS].function = &function_CA_UNITS;
        data_[CA_VDWDEF].function = &function_CA_VDWDEF;

	// Glyph commands
	data_[CA_AUTOELLIPSOIDS].function = &function_CA_AUTOELLIPSOIDS;
	data_[CA_AUTOPOLYHEDRA].function = &function_CA_AUTOPOLYHEDRA;
	data_[CA_GLYPHATOMF].function = &function_CA_GLYPHATOMF;
	data_[CA_GLYPHATOMR].function = &function_CA_GLYPHATOMR;
	data_[CA_GLYPHATOMV].function = &function_CA_GLYPHATOMV;
	data_[CA_GLYPHATOMSF].function = &function_CA_GLYPHATOMSF;
	data_[CA_GLYPHATOMSR].function = &function_CA_GLYPHATOMSR;
	data_[CA_GLYPHATOMSV].function = &function_CA_GLYPHATOMSV;
	data_[CA_GLYPHCOLOUR].function = &function_CA_GLYPHCOLOUR;
	data_[CA_GLYPHDATA].function = &function_CA_GLYPHDATA;
	data_[CA_GLYPHSOLID].function = &function_CA_GLYPHSOLID;
	data_[CA_GLYPHTEXT].function = &function_CA_GLYPHTEXT;
	data_[CA_NEWGLYPH].function = &function_CA_NEWGLYPH;

	// Grid Commands
	data_[CA_ADDGRIDPOINT].function = &function_CA_ADDGRIDPOINT;
	data_[CA_ADDNEXTGRIDPOINT].function = &function_CA_ADDNEXTGRIDPOINT;
	data_[CA_FINALISEGRID].function = &function_CA_FINALISEGRID;
	data_[CA_GRIDAXES].function = &function_CA_GRIDAXES;
	data_[CA_GRIDCOLOUR].function = &function_CA_GRIDCOLOUR;
	data_[CA_GRIDCOLOURSCALE].function = &function_CA_GRIDCOLOURSCALE;
	data_[CA_GRIDCUBIC].function = &function_CA_GRIDCUBIC;
	data_[CA_GRIDCUTOFF].function = &function_CA_GRIDCUTOFF;
	data_[CA_GRIDLOOPORDER].function = &function_CA_GRIDLOOPORDER;
	data_[CA_GRIDORIGIN].function = &function_CA_GRIDORIGIN;
	data_[CA_GRIDORTHO].function = &function_CA_GRIDORTHO;
	data_[CA_GRIDSIZE].function = &function_CA_GRIDSIZE;
	data_[CA_GRIDSTYLE].function = &function_CA_GRIDSTYLE;
	data_[CA_GRIDSYMMETRIC].function = &function_CA_GRIDSYMMETRIC;
	data_[CA_GRIDTRANSPARENCY].function = &function_CA_GRIDTRANSPARENCY;
	data_[CA_GRIDUSEZ].function = &function_CA_GRIDUSEZ;
	data_[CA_LOADGRID].function = &function_CA_LOADGRID;
	data_[CA_NEWGRID].function = &function_CA_NEWGRID;

	// Image Commands
	data_[CA_SAVEBITMAP].function = &function_CA_SAVEBITMAP;
	data_[CA_SAVEVECTOR].function = &function_CA_SAVEVECTOR;

	// Labeling Commands
	data_[CA_CLEARLABELS].function = &function_CA_CLEARLABELS;
	data_[CA_LABEL].function = &function_CA_LABEL;
	data_[CA_REMOVELABEL].function = &function_CA_REMOVELABEL;

	// MC Commands
	data_[CA_MCACCEPT].function = &function_CA_MCACCEPT;
	data_[CA_MCALLOW].function = &function_CA_MCALLOW;
	data_[CA_MCMAXSTEP].function = &function_CA_MCMAXSTEP;
	data_[CA_MCNTRIALS].function = &function_CA_MCNTRIALS;
	data_[CA_PRINTMC].function = &function_CA_PRINTMC;

	// Measurement Commands
	data_[CA_CLEARMEASUREMENTS].function = &function_CA_CLEARMEASUREMENTS;
	data_[CA_LISTMEASUREMENTS].function = &function_CA_LISTMEASUREMENTS;
	data_[CA_MEASURE].function = &function_CA_MEASURE;

	// Messaging Commands
	data_[CA_ERROR].function = &function_CA_ERROR;
	data_[CA_PRINT].function = &function_CA_PRINT;
	data_[CA_VERBOSE].function = &function_CA_VERBOSE;
	data_[CA_WARN].function = &function_CA_WARN;

	// Minimisation Commands
	data_[CA_CGMINIMISE].function = &function_CA_CGMINIMISE;
	data_[CA_CONVERGE].function = &function_CA_CONVERGE;
	data_[CA_LINETOL].function = &function_CA_LINETOL;
	data_[CA_MCMINIMISE].function = &function_CA_MCMINIMISE;
	data_[CA_SDMINIMISE].function = &function_CA_SDMINIMISE;
	data_[CA_SIMPLEXMINIMISE].function = &function_CA_SIMPLEXMINIMISE;
	
	// Model Commands
	data_[CA_CREATEATOMS].function = &function_CA_CREATEATOMS;
	data_[CA_FINALISEMODEL].function = &function_CA_FINALISEMODEL;
	data_[CA_GETMODEL].function = &function_CA_GETMODEL;
	data_[CA_LISTMODELS].function = &function_CA_LISTMODELS;
	data_[CA_LOADMODEL].function = &function_CA_LOADMODEL;
	data_[CA_LOGINFO].function = &function_CA_LOGINFO;
	data_[CA_MODELTEMPLATE].function = &function_CA_MODELTEMPLATE;
	data_[CA_NEWMODEL].function = &function_CA_NEWMODEL;
	data_[CA_NEXTMODEL].function = &function_CA_NEXTMODEL;
	data_[CA_PREVMODEL].function = &function_CA_PREVMODEL;
	data_[CA_INFO].function = &function_CA_INFO;
	data_[CA_SAVEMODEL].function = &function_CA_SAVEMODEL;
	data_[CA_SETNAME].function = &function_CA_SETNAME;

	// Pattern Commands
	data_[CA_CLEARPATTERNS].function = &function_CA_CLEARPATTERNS;
	data_[CA_CREATEPATTERNS].function = &function_CA_CREATEPATTERNS;
	data_[CA_GETPATTERN].function = &function_CA_GETPATTERN;
	data_[CA_LISTPATTERNS].function = &function_CA_LISTPATTERNS;
	data_[CA_NEWPATTERN].function = &function_CA_NEWPATTERN;

	// Preferences Commands
	data_[CA_ANGLELABEL].function = &function_CA_ANGLELABEL;
	data_[CA_ATOMDETAIL].function = &function_CA_ATOMDETAIL;
	data_[CA_BONDDETAIL].function = &function_CA_BONDDETAIL;
	data_[CA_COLOUR].function = &function_CA_COLOUR;
	data_[CA_COMMONELEMENTS].function = &function_CA_COMMONELEMENTS;
	data_[CA_DENSITYUNITS].function = &function_CA_DENSITYUNITS;
	data_[CA_DISTANCELABEL].function = &function_CA_DISTANCELABEL;
	data_[CA_ECUT].function = &function_CA_ECUT;
	data_[CA_ELEC].function = &function_CA_ELEC;
	data_[CA_ELEMENTAMBIENT].function = &function_CA_ELEMENTAMBIENT;
	data_[CA_ELEMENTDIFFUSE].function = &function_CA_ELEMENTDIFFUSE;
	data_[CA_ELEMENTRADIUS].function = &function_CA_ELEMENTRADIUS;
	data_[CA_ENERGYUNITS].function = &function_CA_ENERGYUNITS;
	data_[CA_GL].function = &function_CA_GL;
	data_[CA_INTRA].function = &function_CA_INTRA;
	data_[CA_KEY].function = &function_CA_KEY;
	data_[CA_LABELSIZE].function = &function_CA_LABELSIZE;
	data_[CA_LIGHT].function = &function_CA_LIGHT;
	data_[CA_LIGHTAMBIENT].function = &function_CA_LIGHTAMBIENT;
	data_[CA_LIGHTDIFFUSE].function = &function_CA_LIGHTDIFFUSE;
	data_[CA_LIGHTPOSITION].function = &function_CA_LIGHTPOSITION;
	data_[CA_LIGHTSPECULAR].function = &function_CA_LIGHTSPECULAR;
	data_[CA_MOUSE].function = &function_CA_MOUSE;
	data_[CA_RADIUS].function = &function_CA_RADIUS;
	data_[CA_REPLICATEFOLD].function = &function_CA_REPLICATEFOLD;
	data_[CA_REPLICATETRIM].function = &function_CA_REPLICATETRIM;
	data_[CA_SCHEME].function = &function_CA_SCHEME;
	data_[CA_SHININESS].function = &function_CA_SHININESS;
	data_[CA_SHOWONSCREEN].function = &function_CA_SHOWONSCREEN;
	data_[CA_SHOWONIMAGE].function = &function_CA_SHOWONIMAGE;
	data_[CA_STYLE].function = &function_CA_STYLE;
	data_[CA_USENICETEXT].function = &function_CA_USENICETEXT;
	data_[CA_VCUT].function = &function_CA_VCUT;
	data_[CA_VDW].function = &function_CA_VDW;

	// Read / Write Commands
	data_[CA_ADDREADOPTION].function = &function_CA_ADDREADOPTION;
	data_[CA_FIND].function = &function_CA_FIND;
	data_[CA_GETLINE].function = &function_CA_GETLINE;
	data_[CA_READCHARS].function = &function_CA_READCHARS;
	data_[CA_READFLOAT].function = &function_CA_READFLOAT;
	data_[CA_READINTEGER].function = &function_CA_READINTEGER;
	data_[CA_READLINE].function = &function_CA_READLINE;
	data_[CA_READNEXT].function = &function_CA_READNEXT;
	data_[CA_READVAR].function = &function_CA_READVAR;
	data_[CA_REMOVEREADOPTION].function = &function_CA_REMOVEREADOPTION;
	data_[CA_REWIND].function = &function_CA_REWIND;
	data_[CA_SKIPCHARS].function = &function_CA_SKIPCHARS;
	data_[CA_SKIPLINE].function = &function_CA_SKIPLINE;
	data_[CA_WRITELINE].function = &function_CA_WRITELINE;
	data_[CA_WRITEVAR].function = &function_CA_WRITEVAR;

	// Script Commands
	data_[CA_LISTSCRIPTS].function = &function_CA_LISTSCRIPTS;
	data_[CA_LOADSCRIPT].function = &function_CA_LOADSCRIPT;
	data_[CA_RUNSCRIPT].function = &function_CA_RUNSCRIPT;

	// Select Commands
	data_[CA_DESELECT].function = &function_CA_DESELECT;
	data_[CA_INVERT].function = &function_CA_INVERT;
	data_[CA_SELECT].function = &function_CA_SELECT;
	data_[CA_SELECTALL].function = &function_CA_SELECTALL;
	data_[CA_SELECTFFTYPE].function = &function_CA_SELECTFFTYPE;
	data_[CA_SELECTNONE].function = &function_CA_SELECTNONE;
	data_[CA_SELECTOVERLAPS].function = &function_CA_SELECTOVERLAPS;
	data_[CA_SELECTPATTERN].function = &function_CA_SELECTPATTERN;
	data_[CA_SELECTTYPE].function = &function_CA_SELECTTYPE;
	
	// Site Commands
	data_[CA_GETSITE].function = &function_CA_GETSITE;
	data_[CA_LISTSITES].function = &function_CA_LISTSITES;
	data_[CA_NEWSITE].function = &function_CA_NEWSITE;
	data_[CA_SITEAXES].function = &function_CA_SITEAXES;

	// System Commands
	data_[CA_DEBUG].function = &function_CA_DEBUG;
	data_[CA_GUI].function = &function_CA_GUI;
	data_[CA_HELP].function = &function_CA_HELP;
	data_[CA_SEED].function = &function_CA_SEED;
	data_[CA_QUIT].function = &function_CA_QUIT;
	
	// Trajectory Commands
	data_[CA_FINALISEFRAME].function = &function_CA_FINALISEFRAME;
	data_[CA_FIRSTFRAME].function = &function_CA_FIRSTFRAME;
	data_[CA_LASTFRAME].function = &function_CA_LASTFRAME;
	data_[CA_LOADTRAJECTORY].function = &function_CA_LOADTRAJECTORY;
	data_[CA_NEXTFRAME].function = &function_CA_NEXTFRAME;
	data_[CA_PREVFRAME].function = &function_CA_PREVFRAME;
	data_[CA_SEEKFRAME].function = &function_CA_SEEKFRAME;

	// Transform Commands
	data_[CA_CENTRE].function = &function_CA_CENTRE;
	data_[CA_TRANSLATE].function = &function_CA_TRANSLATE;
	data_[CA_TRANSLATEATOM].function = &function_CA_TRANSLATEATOM;
	data_[CA_TRANSLATECELL].function = &function_CA_TRANSLATECELL;
	data_[CA_MIRROR].function = &function_CA_MIRROR;

	// Variable Commands
	data_[CA_DECREASE].function = &function_CA_DECREASE;
	data_[CA_INCREASE].function = &function_CA_INCREASE;
	data_[CA_LET].function = &function_CA_LET;
	data_[CA_LETCHAR].function = &function_CA_LETCHAR;
	data_[CA_LETPTR].function = &function_CA_LETPTR;

	// View Commands
	data_[CA_GETVIEW].function = &function_CA_GETVIEW;
	data_[CA_ORTHOGRAPHIC].function = &function_CA_ORTHOGRAPHIC;
	data_[CA_PERSPECTIVE].function = &function_CA_PERSPECTIVE;
	data_[CA_RESETVIEW].function = &function_CA_RESETVIEW;
	data_[CA_ROTATEVIEW].function = &function_CA_ROTATEVIEW;
	data_[CA_SETVIEW].function = &function_CA_SETVIEW;
	data_[CA_SPEEDTEST].function = &function_CA_SPEEDTEST;
	data_[CA_TRANSLATEVIEW].function = &function_CA_TRANSLATEVIEW;
	data_[CA_VIEWALONG].function = &function_CA_VIEWALONG;
	data_[CA_VIEWALONGCELL].function = &function_CA_VIEWALONGCELL;
	data_[CA_ZOOMVIEW].function = &function_CA_ZOOMVIEW;
	data_[CA_ZROTATEVIEW].function = &function_CA_ZROTATEVIEW;
}

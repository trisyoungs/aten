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
#include "base/master.h"

// Initialise Command Pointers
void Master::initCommands()
{
	/*
	// Store pointers to all command functions
	*/

	CA_data[CA_ROOTNODE].function = &CommandData::function_CA_ROOTNODE;

	// Analyse commands
	CA_data[CA_FINALISE].function = &CommandData::function_CA_FINALISE;
	CA_data[CA_FRAMEANALYSE].function = &CommandData::function_CA_FRAMEANALYSE;
	CA_data[CA_GEOMETRY].function = &CommandData::function_CA_GEOMETRY;
	CA_data[CA_MODELANALYSE].function = &CommandData::function_CA_MODELANALYSE;
	CA_data[CA_PDENS].function = &CommandData::function_CA_PDENS;
	CA_data[CA_PRINTJOBS].function = &CommandData::function_CA_PRINTJOBS;
	CA_data[CA_RDF].function = &CommandData::function_CA_RDF;
	CA_data[CA_SAVEQUANTITIES].function = &CommandData::function_CA_SAVEQUANTITIES;
	CA_data[CA_TRAJANALYSE].function = &CommandData::function_CA_TRAJANALYSE;

	// Atom Commands
	CA_data[CA_CHAIN].function = &CommandData::function_CA_CHAIN;
	CA_data[CA_ENDCHAIN].function = &CommandData::function_CA_ENDCHAIN;
	CA_data[CA_GETATOM].function = &CommandData::function_CA_GETATOM;
	CA_data[CA_NEWATOM].function = &CommandData::function_CA_NEWATOM;
	CA_data[CA_NEWATOMFRAC].function = &CommandData::function_CA_NEWATOMFRAC;
	CA_data[CA_SETCOORDS].function = &CommandData::function_CA_SETCOORDS;
	CA_data[CA_SETCHARGE].function = &CommandData::function_CA_SETCHARGE;
	CA_data[CA_SETELEMENT].function = &CommandData::function_CA_SETELEMENT;
	CA_data[CA_SETFORCES].function = &CommandData::function_CA_SETFORCES;
	CA_data[CA_SETFX].function = &CommandData::function_CA_SETFX;
	CA_data[CA_SETFY].function = &CommandData::function_CA_SETFY;
	CA_data[CA_SETFZ].function = &CommandData::function_CA_SETFZ;
	CA_data[CA_SETID].function = &CommandData::function_CA_SETID;
	CA_data[CA_SETRX].function = &CommandData::function_CA_SETRX;
	CA_data[CA_SETRY].function = &CommandData::function_CA_SETRY;
	CA_data[CA_SETRZ].function = &CommandData::function_CA_SETRZ;
	CA_data[CA_SETVELOCITIES].function = &CommandData::function_CA_SETVELOCITIES;
	CA_data[CA_SETVX].function = &CommandData::function_CA_SETVX;
	CA_data[CA_SETVY].function = &CommandData::function_CA_SETVY;
	CA_data[CA_SETVZ].function = &CommandData::function_CA_SETVZ;

	// Bond commands
	CA_data[CA_AUGMENT].function = &CommandData::function_CA_AUGMENT;
	CA_data[CA_BONDTOLERANCE].function = &CommandData::function_CA_BONDTOLERANCE;
	CA_data[CA_NEWBOND].function = &CommandData::function_CA_NEWBOND;
	CA_data[CA_NEWBONDID].function = &CommandData::function_CA_NEWBONDID;
	CA_data[CA_REBONDPATTERNS].function = &CommandData::function_CA_REBONDPATTERNS;
	CA_data[CA_REBONDSELECTION].function = &CommandData::function_CA_REBONDSELECTION;
	CA_data[CA_CLEARBONDS].function = &CommandData::function_CA_CLEARBONDS;
	CA_data[CA_REBOND].function = &CommandData::function_CA_REBOND;

	// Build commands
	CA_data[CA_ADDHYDROGEN].function = &CommandData::function_CA_ADDHYDROGEN;
	CA_data[CA_DELETE].function = &CommandData::function_CA_DELETE;
	CA_data[CA_COPY].function = &CommandData::function_CA_COPY;
	CA_data[CA_CUT].function = &CommandData::function_CA_CUT;
	CA_data[CA_LOCATE].function = &CommandData::function_CA_LOCATE;
	CA_data[CA_MOVE].function = &CommandData::function_CA_MOVE;
	CA_data[CA_PASTE].function = &CommandData::function_CA_PASTE;
	CA_data[CA_ROTX].function = &CommandData::function_CA_ROTX;
	CA_data[CA_ROTY].function = &CommandData::function_CA_ROTY;
	CA_data[CA_ROTZ].function = &CommandData::function_CA_ROTZ;
	CA_data[CA_SHIFTDOWN].function = &CommandData::function_CA_SHIFTDOWN;
	CA_data[CA_SHIFTUP].function = &CommandData::function_CA_SHIFTUP;
	CA_data[CA_TOEND].function = &CommandData::function_CA_TOEND;
	CA_data[CA_TOSTART].function = &CommandData::function_CA_TOSTART;
	CA_data[CA_TRANSMUTE].function = &CommandData::function_CA_TRANSMUTE;

	// Cell commands
	CA_data[CA_FOLD].function = &CommandData::function_CA_FOLD;
	CA_data[CA_FOLDMOLECULES].function = &CommandData::function_CA_FOLDMOLECULES;
	CA_data[CA_FRACTOREAL].function = &CommandData::function_CA_FRACTOREAL;
	CA_data[CA_PACK].function = &CommandData::function_CA_PACK;
	CA_data[CA_PRINTCELL].function = &CommandData::function_CA_PRINTCELL;
	CA_data[CA_REPLICATE].function = &CommandData::function_CA_REPLICATE;
	CA_data[CA_SCALE].function = &CommandData::function_CA_SCALE;
	CA_data[CA_CELL].function = &CommandData::function_CA_CELL;
	CA_data[CA_CELLAXES].function = &CommandData::function_CA_CELLAXES;
	CA_data[CA_NOCELL].function = &CommandData::function_CA_NOCELL;
	CA_data[CA_SPACEGROUP].function = &CommandData::function_CA_SPACEGROUP;

	// Charge commands
	CA_data[CA_CHARGEFF].function = &CommandData::function_CA_CHARGEFF;
	CA_data[CA_CHARGEFROMMODEL].function = &CommandData::function_CA_CHARGEFROMMODEL;
	CA_data[CA_CHARGEPATOM].function = &CommandData::function_CA_CHARGEPATOM;
	CA_data[CA_CHARGE].function = &CommandData::function_CA_CHARGE;
	CA_data[CA_CHARGETYPE].function = &CommandData::function_CA_CHARGETYPE;
	CA_data[CA_CLEARCHARGES].function = &CommandData::function_CA_CLEARCHARGES;

	// Colourscale commands
	CA_data[CA_ADDPOINT].function = &CommandData::function_CA_ADDPOINT;
	CA_data[CA_CLEARPOINTS].function = &CommandData::function_CA_CLEARPOINTS;
	CA_data[CA_LISTSCALES].function = &CommandData::function_CA_LISTSCALES;
	CA_data[CA_REMOVEPOINT].function = &CommandData::function_CA_REMOVEPOINT;
	CA_data[CA_SCALENAME].function = &CommandData::function_CA_SCALENAME;
	CA_data[CA_SCALEVISIBLE].function = &CommandData::function_CA_SCALEVISIBLE;
	CA_data[CA_SETPOINT].function = &CommandData::function_CA_SETPOINT;
	CA_data[CA_SETPOINTCOLOUR].function = &CommandData::function_CA_SETPOINTCOLOUR;
	CA_data[CA_SETPOINTVALUE].function = &CommandData::function_CA_SETPOINTVALUE;

	// Disordered build commands
	CA_data[CA_DISORDER].function = &CommandData::function_CA_DISORDER;
	CA_data[CA_LISTCOMPONENTS].function = &CommandData::function_CA_LISTCOMPONENTS;
	CA_data[CA_NMOLS].function = &CommandData::function_CA_NMOLS;
	CA_data[CA_REGIONCENTRE].function = &CommandData::function_CA_REGIONCENTRE;
	CA_data[CA_REGIONGEOMETRY].function = &CommandData::function_CA_REGIONGEOMETRY;
	CA_data[CA_REGIONOVERLAPS].function = &CommandData::function_CA_REGIONOVERLAPS;
	CA_data[CA_REGIONSHAPE].function = &CommandData::function_CA_REGIONSHAPE;
	CA_data[CA_VDWSCALE].function = &CommandData::function_CA_VDWSCALE;

	// Energy Commands
	CA_data[CA_FRAMEENERGY].function = &CommandData::function_CA_FRAMEENERGY;
	CA_data[CA_MODELENERGY].function = &CommandData::function_CA_MODELENERGY;
	CA_data[CA_PRINTELEC].function = &CommandData::function_CA_PRINTELEC;
	CA_data[CA_PRINTEWALD].function = &CommandData::function_CA_PRINTEWALD;
	CA_data[CA_PRINTINTER].function = &CommandData::function_CA_PRINTINTER;
	CA_data[CA_PRINTINTRA].function = &CommandData::function_CA_PRINTINTRA;
	CA_data[CA_PRINTENERGY].function = &CommandData::function_CA_PRINTENERGY;
	CA_data[CA_PRINTSUMMARY].function = &CommandData::function_CA_PRINTSUMMARY;
	CA_data[CA_PRINTVDW].function = &CommandData::function_CA_PRINTVDW;

	// Expression Commands
	CA_data[CA_CREATEEXPRESSION].function = &CommandData::function_CA_CREATEEXPRESSION;
	CA_data[CA_PRINTSETUP].function = &CommandData::function_CA_PRINTSETUP;
	CA_data[CA_SAVEEXPRESSION].function = &CommandData::function_CA_SAVEEXPRESSION;

	// Flow control
	CA_data[CA_ELSE].function = &CommandData::function_CA_ELSE;
	CA_data[CA_ELSEIF].function = &CommandData::function_CA_ELSEIF;
	CA_data[CA_END].function = &CommandData::function_CA_END;
	CA_data[CA_FOR].function = &CommandData::function_CA_FOR;
	CA_data[CA_GOTO].function = &CommandData::function_CA_GOTO;
	CA_data[CA_GOTONONIF].function = &CommandData::function_CA_GOTONONIF;
	CA_data[CA_IF].function = &CommandData::function_CA_IF;
	CA_data[CA_TERMINATE].function = &CommandData::function_CA_TERMINATE;

	// Force Commands
	CA_data[CA_FRAMEFORCES].function = &CommandData::function_CA_FRAMEFORCES;
	CA_data[CA_MODELFORCES].function = &CommandData::function_CA_MODELFORCES;
	CA_data[CA_PRINTFORCES].function = &CommandData::function_CA_PRINTFORCES;

	// Forcefield Commands
	CA_data[CA_CLEARMAP].function = &CommandData::function_CA_CLEARMAP;
	CA_data[CA_DEFAULTFF].function = &CommandData::function_CA_DEFAULTFF;
	CA_data[CA_FFMODEL].function = &CommandData::function_CA_FFMODEL;
	CA_data[CA_FFPATTERN].function = &CommandData::function_CA_FFPATTERN;
	CA_data[CA_FFPATTERNID].function = &CommandData::function_CA_FFPATTERNID;
	CA_data[CA_GETFF].function = &CommandData::function_CA_GETFF;
	CA_data[CA_LOADFF].function = &CommandData::function_CA_LOADFF;
	CA_data[CA_MAP].function = &CommandData::function_CA_MAP;
	CA_data[CA_TYPEMODEL].function = &CommandData::function_CA_TYPEMODEL;
	CA_data[CA_TYPETEST].function = &CommandData::function_CA_TYPETEST;

	// Glyph commands
	CA_data[CA_GLYPHATOMF].function = &CommandData::function_CA_GLYPHATOMF;
	CA_data[CA_GLYPHATOMR].function = &CommandData::function_CA_GLYPHATOMR;
	CA_data[CA_GLYPHATOMV].function = &CommandData::function_CA_GLYPHATOMV;
	CA_data[CA_GLYPHATOMSF].function = &CommandData::function_CA_GLYPHATOMSF;
	CA_data[CA_GLYPHATOMSR].function = &CommandData::function_CA_GLYPHATOMSR;
	CA_data[CA_GLYPHATOMSV].function = &CommandData::function_CA_GLYPHATOMSV;
	CA_data[CA_GLYPHDATA].function = &CommandData::function_CA_GLYPHDATA;
	CA_data[CA_GLYPHSOLID].function = &CommandData::function_CA_GLYPHSOLID;
	CA_data[CA_GLYPHTEXT].function = &CommandData::function_CA_GLYPHTEXT;
	CA_data[CA_NEWGLYPH].function = &CommandData::function_CA_NEWGLYPH;

	// Grid Commands
	CA_data[CA_ADDGRIDPOINT].function = &CommandData::function_CA_ADDGRIDPOINT;
	CA_data[CA_ADDNEXTGRIDPOINT].function = &CommandData::function_CA_ADDNEXTGRIDPOINT;
	CA_data[CA_FINALISEGRID].function = &CommandData::function_CA_FINALISEGRID;
	CA_data[CA_GRIDAXES].function = &CommandData::function_CA_GRIDAXES;
	CA_data[CA_GRIDCOLOUR].function = &CommandData::function_CA_GRIDCOLOUR;
	CA_data[CA_GRIDCOLOURSCALE].function = &CommandData::function_CA_GRIDCOLOURSCALE;
	CA_data[CA_GRIDCUBIC].function = &CommandData::function_CA_GRIDCUBIC;
	CA_data[CA_GRIDCUTOFF].function = &CommandData::function_CA_GRIDCUTOFF;
	CA_data[CA_GRIDLOOPORDER].function = &CommandData::function_CA_GRIDLOOPORDER;
	CA_data[CA_GRIDORIGIN].function = &CommandData::function_CA_GRIDORIGIN;
	CA_data[CA_GRIDORTHO].function = &CommandData::function_CA_GRIDORTHO;
	CA_data[CA_GRIDSIZE].function = &CommandData::function_CA_GRIDSIZE;
	CA_data[CA_GRIDSYMMETRIC].function = &CommandData::function_CA_GRIDSYMMETRIC;
	CA_data[CA_GRIDTRANSPARENCY].function = &CommandData::function_CA_GRIDTRANSPARENCY;
	CA_data[CA_GRIDUSEZ].function = &CommandData::function_CA_GRIDUSEZ;
	CA_data[CA_NEWGRID].function = &CommandData::function_CA_NEWGRID;

	// Image Commands
	CA_data[CA_SAVEBITMAP].function = &CommandData::function_CA_SAVEBITMAP;
	CA_data[CA_SAVEVECTOR].function = &CommandData::function_CA_SAVEVECTOR;

	// Labeling Commands
	CA_data[CA_CLEARLABELS].function = &CommandData::function_CA_CLEARLABELS;
	CA_data[CA_LABEL].function = &CommandData::function_CA_LABEL;
	CA_data[CA_REMOVELABEL].function = &CommandData::function_CA_REMOVELABEL;

	// MC Commands
	CA_data[CA_MCACCEPT].function = &CommandData::function_CA_MCACCEPT;
	CA_data[CA_MCALLOW].function = &CommandData::function_CA_MCALLOW;
	CA_data[CA_MCMAXSTEP].function = &CommandData::function_CA_MCMAXSTEP;
	CA_data[CA_MCNTRIALS].function = &CommandData::function_CA_MCNTRIALS;
	CA_data[CA_PRINTMC].function = &CommandData::function_CA_PRINTMC;

	// Messaging Commands
	CA_data[CA_ERROR].function = &CommandData::function_CA_ERROR;
	CA_data[CA_PRINT].function = &CommandData::function_CA_PRINT;
	CA_data[CA_VERBOSE].function = &CommandData::function_CA_VERBOSE;
	CA_data[CA_WARN].function = &CommandData::function_CA_WARN;

	// Minimisation Commands
	CA_data[CA_CGMINIMISE].function = &CommandData::function_CA_CGMINIMISE;
	CA_data[CA_CONVERGE].function = &CommandData::function_CA_CONVERGE;
	CA_data[CA_LINETOL].function = &CommandData::function_CA_LINETOL;
	CA_data[CA_MCMINIMISE].function = &CommandData::function_CA_MCMINIMISE;
	CA_data[CA_SDMINIMISE].function = &CommandData::function_CA_SDMINIMISE;
	CA_data[CA_SIMPLEXMINIMISE].function = &CommandData::function_CA_SIMPLEXMINIMISE;
	
	// Model Commands
	CA_data[CA_CREATEATOMS].function = &CommandData::function_CA_CREATEATOMS;
	CA_data[CA_FINALISEMODEL].function = &CommandData::function_CA_FINALISEMODEL;
	CA_data[CA_GETMODEL].function = &CommandData::function_CA_GETMODEL;
	CA_data[CA_LISTMODELS].function = &CommandData::function_CA_LISTMODELS;
	CA_data[CA_LOADMODEL].function = &CommandData::function_CA_LOADMODEL;
	CA_data[CA_LOGINFO].function = &CommandData::function_CA_LOGINFO;
	CA_data[CA_MODELTEMPLATE].function = &CommandData::function_CA_MODELTEMPLATE;
	CA_data[CA_NEWMODEL].function = &CommandData::function_CA_NEWMODEL;
	CA_data[CA_NEXTMODEL].function = &CommandData::function_CA_NEXTMODEL;
	CA_data[CA_PREVMODEL].function = &CommandData::function_CA_PREVMODEL;
	CA_data[CA_INFO].function = &CommandData::function_CA_INFO;
	CA_data[CA_SAVEMODEL].function = &CommandData::function_CA_SAVEMODEL;
	CA_data[CA_SETNAME].function = &CommandData::function_CA_SETNAME;

	// Pattern Commands
	CA_data[CA_CLEARPATTERNS].function = &CommandData::function_CA_CLEARPATTERNS;
	CA_data[CA_CREATEPATTERNS].function = &CommandData::function_CA_CREATEPATTERNS;
	CA_data[CA_GETPATTERN].function = &CommandData::function_CA_GETPATTERN;
	CA_data[CA_LISTPATTERNS].function = &CommandData::function_CA_LISTPATTERNS;
	CA_data[CA_NEWPATTERN].function = &CommandData::function_CA_NEWPATTERN;

	// Preferences Commands
	CA_data[CA_ANGLELABEL].function = &CommandData::function_CA_ANGLELABEL;
	CA_data[CA_ATOMDETAIL].function = &CommandData::function_CA_ATOMDETAIL;
	CA_data[CA_BONDDETAIL].function = &CommandData::function_CA_BONDDETAIL;
	CA_data[CA_COLOUR].function = &CommandData::function_CA_COLOUR;
	CA_data[CA_COMMONELEMENTS].function = &CommandData::function_CA_COMMONELEMENTS;
	CA_data[CA_DENSITYUNITS].function = &CommandData::function_CA_DENSITYUNITS;
	CA_data[CA_DISTANCELABEL].function = &CommandData::function_CA_DISTANCELABEL;
	CA_data[CA_ECUT].function = &CommandData::function_CA_ECUT;
	CA_data[CA_ELEC].function = &CommandData::function_CA_ELEC;
	CA_data[CA_ELEMENTAMBIENT].function = &CommandData::function_CA_ELEMENTAMBIENT;
	CA_data[CA_ELEMENTDIFFUSE].function = &CommandData::function_CA_ELEMENTDIFFUSE;
	CA_data[CA_ELEMENTRADIUS].function = &CommandData::function_CA_ELEMENTRADIUS;
	CA_data[CA_ENERGYUNITS].function = &CommandData::function_CA_ENERGYUNITS;
	CA_data[CA_GL].function = &CommandData::function_CA_GL;
	CA_data[CA_INTRA].function = &CommandData::function_CA_INTRA;
	CA_data[CA_KEY].function = &CommandData::function_CA_KEY;
	CA_data[CA_LABELSIZE].function = &CommandData::function_CA_LABELSIZE;
	CA_data[CA_LIGHT].function = &CommandData::function_CA_LIGHT;
	CA_data[CA_LIGHTAMBIENT].function = &CommandData::function_CA_LIGHTAMBIENT;
	CA_data[CA_LIGHTDIFFUSE].function = &CommandData::function_CA_LIGHTDIFFUSE;
	CA_data[CA_LIGHTPOSITION].function = &CommandData::function_CA_LIGHTPOSITION;
	CA_data[CA_LIGHTSPECULAR].function = &CommandData::function_CA_LIGHTSPECULAR;
	CA_data[CA_MOUSE].function = &CommandData::function_CA_MOUSE;
	CA_data[CA_RADIUS].function = &CommandData::function_CA_RADIUS;
	CA_data[CA_REPLICATEFOLD].function = &CommandData::function_CA_REPLICATEFOLD;
	CA_data[CA_REPLICATETRIM].function = &CommandData::function_CA_REPLICATETRIM;
	CA_data[CA_SCHEME].function = &CommandData::function_CA_SCHEME;
	CA_data[CA_SHININESS].function = &CommandData::function_CA_SHININESS;
	CA_data[CA_SHOW].function = &CommandData::function_CA_SHOW;
	CA_data[CA_SHOWONIMAGE].function = &CommandData::function_CA_SHOWONIMAGE;
	CA_data[CA_STYLE].function = &CommandData::function_CA_STYLE;
	CA_data[CA_USENICETEXT].function = &CommandData::function_CA_USENICETEXT;
	CA_data[CA_VCUT].function = &CommandData::function_CA_VCUT;
	CA_data[CA_VDW].function = &CommandData::function_CA_VDW;

	// Read / Write Commands
	CA_data[CA_ADDREADOPTION].function = &CommandData::function_CA_ADDREADOPTION;
	CA_data[CA_FIND].function = &CommandData::function_CA_FIND;
	CA_data[CA_READCHARS].function = &CommandData::function_CA_READCHARS;
	CA_data[CA_READFLOAT].function = &CommandData::function_CA_READFLOAT;
	CA_data[CA_READINTEGER].function = &CommandData::function_CA_READINTEGER;
	CA_data[CA_READLINE].function = &CommandData::function_CA_READLINE;
	CA_data[CA_READNEXT].function = &CommandData::function_CA_READNEXT;
	CA_data[CA_READVAR].function = &CommandData::function_CA_READVAR;
	CA_data[CA_REMOVEREADOPTION].function = &CommandData::function_CA_REMOVEREADOPTION;
	CA_data[CA_REWIND].function = &CommandData::function_CA_REWIND;
	CA_data[CA_SKIPCHARS].function = &CommandData::function_CA_SKIPCHARS;
	CA_data[CA_SKIPLINE].function = &CommandData::function_CA_SKIPLINE;
	CA_data[CA_WRITELINE].function = &CommandData::function_CA_WRITELINE;
	CA_data[CA_WRITEVAR].function = &CommandData::function_CA_WRITEVAR;

	// Script Commands
	CA_data[CA_LISTSCRIPTS].function = &CommandData::function_CA_LISTSCRIPTS;
	CA_data[CA_LOADSCRIPT].function = &CommandData::function_CA_LOADSCRIPT;
	CA_data[CA_RUNSCRIPT].function = &CommandData::function_CA_RUNSCRIPT;

	// Select Commands
	CA_data[CA_DESELECT].function = &CommandData::function_CA_DESELECT;
	CA_data[CA_INVERT].function = &CommandData::function_CA_INVERT;
	CA_data[CA_SELECT].function = &CommandData::function_CA_SELECT;
	CA_data[CA_SELECTALL].function = &CommandData::function_CA_SELECTALL;
	CA_data[CA_SELECTFFTYPE].function = &CommandData::function_CA_SELECTFFTYPE;
	CA_data[CA_SELECTNONE].function = &CommandData::function_CA_SELECTNONE;
	CA_data[CA_SELECTOVERLAPS].function = &CommandData::function_CA_SELECTOVERLAPS;
	CA_data[CA_SELECTTYPE].function = &CommandData::function_CA_SELECTTYPE;
	
	// Site Commands
	CA_data[CA_GETSITE].function = &CommandData::function_CA_GETSITE;
	CA_data[CA_LISTSITES].function = &CommandData::function_CA_LISTSITES;
	CA_data[CA_NEWSITE].function = &CommandData::function_CA_NEWSITE;
	CA_data[CA_SITEAXES].function = &CommandData::function_CA_SITEAXES;

	// System Commands
	CA_data[CA_DEBUG].function = &CommandData::function_CA_DEBUG;
	CA_data[CA_GUI].function = &CommandData::function_CA_GUI;
	CA_data[CA_HELP].function = &CommandData::function_CA_HELP;
	CA_data[CA_SEED].function = &CommandData::function_CA_SEED;
	CA_data[CA_QUIT].function = &CommandData::function_CA_QUIT;
	
	// Trajectory Commands
	CA_data[CA_FINALISEFRAME].function = &CommandData::function_CA_FINALISEFRAME;
	CA_data[CA_FIRSTFRAME].function = &CommandData::function_CA_FIRSTFRAME;
	CA_data[CA_LASTFRAME].function = &CommandData::function_CA_LASTFRAME;
	CA_data[CA_LOADTRAJECTORY].function = &CommandData::function_CA_LOADTRAJECTORY;
	CA_data[CA_NEXTFRAME].function = &CommandData::function_CA_NEXTFRAME;
	CA_data[CA_PREVFRAME].function = &CommandData::function_CA_PREVFRAME;
	CA_data[CA_SEEKFRAME].function = &CommandData::function_CA_SEEKFRAME;

	// Transform Commands
	CA_data[CA_CENTRE].function = &CommandData::function_CA_CENTRE;
	CA_data[CA_TRANSLATE].function = &CommandData::function_CA_TRANSLATE;
	CA_data[CA_TRANSLATEATOM].function = &CommandData::function_CA_TRANSLATEATOM;
	CA_data[CA_TRANSLATECELL].function = &CommandData::function_CA_TRANSLATECELL;
	CA_data[CA_MIRROR].function = &CommandData::function_CA_MIRROR;

	// Variable Commands
	CA_data[CA_DECREASE].function = &CommandData::function_CA_DECREASE;
	CA_data[CA_INCREASE].function = &CommandData::function_CA_INCREASE;
	CA_data[CA_LET].function = &CommandData::function_CA_LET;
	CA_data[CA_LET2].function = &CommandData::function_CA_LET2;
	CA_data[CA_LETCHAR].function = &CommandData::function_CA_LETCHAR;

	// View Commands
	CA_data[CA_GETVIEW].function = &CommandData::function_CA_GETVIEW;
	CA_data[CA_ORTHOGRAPHIC].function = &CommandData::function_CA_ORTHOGRAPHIC;
	CA_data[CA_PERSPECTIVE].function = &CommandData::function_CA_PERSPECTIVE;
	CA_data[CA_RESETVIEW].function = &CommandData::function_CA_RESETVIEW;
	CA_data[CA_ROTATEVIEW].function = &CommandData::function_CA_ROTATEVIEW;
	CA_data[CA_SETVIEW].function = &CommandData::function_CA_SETVIEW;
	CA_data[CA_SPEEDTEST].function = &CommandData::function_CA_SPEEDTEST;
	CA_data[CA_TRANSLATEVIEW].function = &CommandData::function_CA_TRANSLATEVIEW;
	CA_data[CA_VIEWALONG].function = &CommandData::function_CA_VIEWALONG;
	CA_data[CA_VIEWALONGCELL].function = &CommandData::function_CA_VIEWALONGCELL;
	CA_data[CA_ZOOMVIEW].function = &CommandData::function_CA_ZOOMVIEW;
	CA_data[CA_ZROTATEVIEW].function = &CommandData::function_CA_ZROTATEVIEW;
}

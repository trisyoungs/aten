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
void MasterData::initCommands()
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
	CA_data[CA_NEWATOM].function = &CommandData::function_CA_NEWATOM;
	CA_data[CA_NEWATOMFRAC].function = &CommandData::function_CA_NEWATOMFRAC;
	CA_data[CA_SELECTATOM].function = &CommandData::function_CA_SELECTATOM;
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
	CA_data[CA_LOCATE].function = &CommandData::function_CA_LOCATE;
	CA_data[CA_MOVE].function = &CommandData::function_CA_MOVE;
	CA_data[CA_ROTX].function = &CommandData::function_CA_ROTX;
	CA_data[CA_ROTY].function = &CommandData::function_CA_ROTY;
	CA_data[CA_ROTZ].function = &CommandData::function_CA_ROTZ;
	CA_data[CA_TRANSMUTE].function = &CommandData::function_CA_TRANSMUTE;

	// Cell commands
	CA_data[CA_FOLD].function = &CommandData::function_CA_FOLD;
	CA_data[CA_FRACTOREAL].function = &CommandData::function_CA_FRACTOREAL;
	CA_data[CA_PACK].function = &CommandData::function_CA_PACK;
	CA_data[CA_PRINTCELL].function = &CommandData::function_CA_PRINTCELL;
	CA_data[CA_REPLICATECELL].function = &CommandData::function_CA_REPLICATECELL;
	CA_data[CA_SCALECELL].function = &CommandData::function_CA_SCALECELL;
	CA_data[CA_SETCELL].function = &CommandData::function_CA_SETCELL;
	CA_data[CA_SETCELLAXES].function = &CommandData::function_CA_SETCELLAXES;
	CA_data[CA_SETSPACEGROUP].function = &CommandData::function_CA_SETSPACEGROUP;

	// Charge commands
	CA_data[CA_CHARGEFF].function = &CommandData::function_CA_CHARGEFF;
	CA_data[CA_CHARGEFROMMODEL].function = &CommandData::function_CA_CHARGEFROMMODEL;
	CA_data[CA_CHARGEPATOM].function = &CommandData::function_CA_CHARGEPATOM;
	CA_data[CA_CHARGESELECTION].function = &CommandData::function_CA_CHARGESELECTION;
	CA_data[CA_CHARGETYPE].function = &CommandData::function_CA_CHARGETYPE;
	CA_data[CA_CLEARCHARGES].function = &CommandData::function_CA_CLEARCHARGES;

	// Disordered build commands
	CA_data[CA_ADDCOMPONENT].function = &CommandData::function_CA_ADDCOMPONENT;
	CA_data[CA_DISORDER].function = &CommandData::function_CA_DISORDER;
	CA_data[CA_PRINTCOMPONENTS].function = &CommandData::function_CA_PRINTCOMPONENTS;
	CA_data[CA_SETCENTRE].function = &CommandData::function_CA_SETCENTRE;
	CA_data[CA_SETGEOMETRY].function = &CommandData::function_CA_SETGEOMETRY;
	CA_data[CA_SETOVERLAP].function = &CommandData::function_CA_SETOVERLAP;
	CA_data[CA_SETSHAPE].function = &CommandData::function_CA_SETSHAPE;
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
	CA_data[CA_DEFAULTFF].function = &CommandData::function_CA_DEFAULTFF;
	CA_data[CA_FFMODEL].function = &CommandData::function_CA_FFMODEL;
	CA_data[CA_FFPATTERN].function = &CommandData::function_CA_FFPATTERN;
	CA_data[CA_FFPATTERNID].function = &CommandData::function_CA_FFPATTERNID;
	CA_data[CA_GETFF].function = &CommandData::function_CA_GETFF;
	CA_data[CA_LOADFF].function = &CommandData::function_CA_LOADFF;
	CA_data[CA_TYPEMODEL].function = &CommandData::function_CA_TYPEMODEL;
	CA_data[CA_TYPETEST].function = &CommandData::function_CA_TYPETEST;

	// Glyph commands
	CA_data[CA_NEWGLYPH].function = &CommandData::function_CA_NEWGLYPH;
	CA_data[CA_SETGLYPHATOMF].function = &CommandData::function_CA_SETGLYPHATOMF;
	CA_data[CA_SETGLYPHATOMR].function = &CommandData::function_CA_SETGLYPHATOMR;
	CA_data[CA_SETGLYPHATOMV].function = &CommandData::function_CA_SETGLYPHATOMV;
	CA_data[CA_SETGLYPHATOMSF].function = &CommandData::function_CA_SETGLYPHATOMSF;
	CA_data[CA_SETGLYPHATOMSR].function = &CommandData::function_CA_SETGLYPHATOMSR;
	CA_data[CA_SETGLYPHATOMSV].function = &CommandData::function_CA_SETGLYPHATOMSV;
	CA_data[CA_SETGLYPHDATA].function = &CommandData::function_CA_SETGLYPHDATA;
	CA_data[CA_SETGLYPHSOLID].function = &CommandData::function_CA_SETGLYPHSOLID;

	// Grid Commands
	CA_data[CA_ADDGRIDPOINT].function = &CommandData::function_CA_ADDGRIDPOINT;
	CA_data[CA_ADDNEXTGRIDPOINT].function = &CommandData::function_CA_ADDNEXTGRIDPOINT;
	CA_data[CA_FINALISEGRID].function = &CommandData::function_CA_FINALISEGRID;
	CA_data[CA_NEWGRID].function = &CommandData::function_CA_NEWGRID;
	CA_data[CA_SETGRID].function = &CommandData::function_CA_SETGRID;
	CA_data[CA_SETGRIDCUBIC].function = &CommandData::function_CA_SETGRIDCUBIC;
	CA_data[CA_SETGRIDLOOPORDER].function = &CommandData::function_CA_SETGRIDLOOPORDER;
	CA_data[CA_SETGRIDORIGIN].function = &CommandData::function_CA_SETGRIDORIGIN;
	CA_data[CA_SETGRIDORTHO].function = &CommandData::function_CA_SETGRIDORTHO;
	CA_data[CA_SETGRIDSIZE].function = &CommandData::function_CA_SETGRIDSIZE;

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
	CA_data[CA_MODELTEMPLATE].function = &CommandData::function_CA_MODELTEMPLATE;
	CA_data[CA_NEWMODEL].function = &CommandData::function_CA_NEWMODEL;
	CA_data[CA_INFO].function = &CommandData::function_CA_INFO;
	CA_data[CA_SAVEMODEL].function = &CommandData::function_CA_SAVEMODEL;
	CA_data[CA_SETTITLE].function = &CommandData::function_CA_SETTITLE;

	// Pattern Commands
	CA_data[CA_CLEARPATTERNS].function = &CommandData::function_CA_CLEARPATTERNS;
	CA_data[CA_CREATEPATTERNS].function = &CommandData::function_CA_CREATEPATTERNS;
	CA_data[CA_GETPATTERN].function = &CommandData::function_CA_GETPATTERN;
	CA_data[CA_LISTPATTERNS].function = &CommandData::function_CA_LISTPATTERNS;
	CA_data[CA_NEWPATTERN].function = &CommandData::function_CA_NEWPATTERN;

	// Preferences Commands
	CA_data[CA_ATOMDETAIL].function = &CommandData::function_CA_ATOMDETAIL;
	CA_data[CA_BONDDETAIL].function = &CommandData::function_CA_BONDDETAIL;
	CA_data[CA_COLOUR].function = &CommandData::function_CA_COLOUR;
	CA_data[CA_DENSITYUNITS].function = &CommandData::function_CA_DENSITYUNITS;
	CA_data[CA_ECUT].function = &CommandData::function_CA_ECUT;
	CA_data[CA_ELEC].function = &CommandData::function_CA_ELEC;
	CA_data[CA_ELEMENTAMBIENT].function = &CommandData::function_CA_ELEMENTAMBIENT;
	CA_data[CA_ELEMENTDIFFUSE].function = &CommandData::function_CA_ELEMENTDIFFUSE;
	CA_data[CA_ELEMENTRADIUS].function = &CommandData::function_CA_ELEMENTRADIUS;
	CA_data[CA_ENERGYUNITS].function = &CommandData::function_CA_ENERGYUNITS;
	CA_data[CA_GL].function = &CommandData::function_CA_GL;
	CA_data[CA_INTRA].function = &CommandData::function_CA_INTRA;
	CA_data[CA_KEY].function = &CommandData::function_CA_KEY;
	CA_data[CA_MOUSE].function = &CommandData::function_CA_MOUSE;
	CA_data[CA_RADIUS].function = &CommandData::function_CA_RADIUS;
	CA_data[CA_SHININESS].function = &CommandData::function_CA_SHININESS;
	CA_data[CA_SHOW].function = &CommandData::function_CA_SHOW;
	CA_data[CA_STYLE].function = &CommandData::function_CA_STYLE;
	CA_data[CA_VCUT].function = &CommandData::function_CA_VCUT;
	CA_data[CA_VDW].function = &CommandData::function_CA_VDW;

	// Read / Write Commands
	CA_data[CA_ADDREADOPTION].function = &CommandData::function_CA_ADDREADOPTION;
	CA_data[CA_FIND].function = &CommandData::function_CA_FIND;
	CA_data[CA_READCHARS].function = &CommandData::function_CA_READCHARS;
	CA_data[CA_READDOUBLE].function = &CommandData::function_CA_READDOUBLE;
	CA_data[CA_READINTEGER].function = &CommandData::function_CA_READINTEGER;
	CA_data[CA_READLINE].function = &CommandData::function_CA_READLINE;
	CA_data[CA_READNEXT].function = &CommandData::function_CA_READNEXT;
	CA_data[CA_READVAR].function = &CommandData::function_CA_READVAR;
	CA_data[CA_REMOVEREADOPTION].function = &CommandData::function_CA_REMOVEREADOPTION;
	CA_data[CA_REWIND].function = &CommandData::function_CA_REWIND;
	CA_data[CA_SKIPCHARS].function = &CommandData::function_CA_SKIPCHARS;
	CA_data[CA_SKIPLINE].function = &CommandData::function_CA_SKIPLINE;
	CA_data[CA_WRITELINE].function = &CommandData::function_CA_WRITELINE;

	// Script Commands
	CA_data[CA_LISTSCRIPTS].function = &CommandData::function_CA_LISTSCRIPTS;
	CA_data[CA_LOADSCRIPT].function = &CommandData::function_CA_LOADSCRIPT;
	CA_data[CA_RUNSCRIPT].function = &CommandData::function_CA_RUNSCRIPT;

	// Select Commands
	CA_data[CA_SELECTALL].function = &CommandData::function_CA_SELECTALL;
	CA_data[CA_SELECTATOM].function = &CommandData::function_CA_SELECTATOM;
	CA_data[CA_SELECTELEMENT].function = &CommandData::function_CA_SELECTELEMENT;
	CA_data[CA_SELECTFFTYPE].function = &CommandData::function_CA_SELECTFFTYPE;
	CA_data[CA_SELECTINVERT].function = &CommandData::function_CA_SELECTINVERT;
	CA_data[CA_SELECTNONE].function = &CommandData::function_CA_SELECTNONE;
	CA_data[CA_SELECTOVERLAPS].function = &CommandData::function_CA_SELECTOVERLAPS;
	CA_data[CA_SELECTTYPE].function = &CommandData::function_CA_SELECTTYPE;
	
	// Site Commands
	CA_data[CA_GETSITE].function = &CommandData::function_CA_GETSITE;
	CA_data[CA_LISTSITES].function = &CommandData::function_CA_LISTSITES;
	CA_data[CA_NEWSITE].function = &CommandData::function_CA_NEWSITE;
	CA_data[CA_SETAXES].function = &CommandData::function_CA_SETAXES;

	// System Commands
	CA_data[CA_GUI].function = &CommandData::function_CA_GUI;
	CA_data[CA_HELP].function = &CommandData::function_CA_HELP;
	CA_data[CA_SEED].function = &CommandData::function_CA_SEED;
	CA_data[CA_QUIT].function = &CommandData::function_CA_QUIT;
	
	// Trajectory Commands
	CA_data[CA_FIRSTFRAME].function = &CommandData::function_CA_FIRSTFRAME;
	CA_data[CA_LASTFRAME].function = &CommandData::function_CA_LASTFRAME;
	CA_data[CA_LOADTRAJECTORY].function = &CommandData::function_CA_LOADTRAJECTORY;
	CA_data[CA_NEXTFRAME].function = &CommandData::function_CA_NEXTFRAME;
	CA_data[CA_PREVFRAME].function = &CommandData::function_CA_PREVFRAME;

	// Transform Commands
	CA_data[CA_CENTRE].function = &CommandData::function_CA_CENTRE;
	CA_data[CA_TRANSLATE].function = &CommandData::function_CA_TRANSLATE;
	CA_data[CA_TRANSLATEATOM].function = &CommandData::function_CA_TRANSLATEATOM;
	CA_data[CA_MIRROR].function = &CommandData::function_CA_MIRROR;

	// Variable Commands
	CA_data[CA_LET].function = &CommandData::function_CA_LET;
	CA_data[CA_INCREASE].function = &CommandData::function_CA_INCREASE;
	CA_data[CA_DECREASE].function = &CommandData::function_CA_DECREASE;
	CA_data[CA_EVAL].function = &CommandData::function_CA_EVAL;

	// View Commands
	CA_data[CA_RESETVIEW].function = &CommandData::function_CA_RESETVIEW;
	CA_data[CA_ROTATEVIEW].function = &CommandData::function_CA_ROTATEVIEW;
	CA_data[CA_SPEEDTEST].function = &CommandData::function_CA_SPEEDTEST;
	CA_data[CA_TRANSLATEVIEW].function = &CommandData::function_CA_TRANSLATEVIEW;
	CA_data[CA_ZOOMVIEW].function = &CommandData::function_CA_ZOOMVIEW;
	CA_data[CA_ZROTATEVIEW].function = &CommandData::function_CA_ZROTATEVIEW;
}

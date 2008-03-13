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
void master_data::init_commands()
{
	/*
	// Store pointers to all command functions
	*/

	CA_data[CA_ROOTNODE].function = &commanddata::function_CA_ROOTNODE;

	// Analyse commands
	CA_data[CA_FINALISE].function = &commanddata::function_CA_FINALISE;
	CA_data[CA_FRAMEANALYSE].function = &commanddata::function_CA_FRAMEANALYSE;
	CA_data[CA_MODELANALYSE].function = &commanddata::function_CA_MODELANALYSE;
	CA_data[CA_PDENS].function = &commanddata::function_CA_PDENS;
	CA_data[CA_PRINTJOBS].function = &commanddata::function_CA_PRINTJOBS;
	CA_data[CA_RDF].function = &commanddata::function_CA_RDF;
	CA_data[CA_SAVEQUANTITIES].function = &commanddata::function_CA_SAVEQUANTITIES;
	CA_data[CA_TRAJANALYSE].function = &commanddata::function_CA_TRAJANALYSE;

	// Atom Commands
	CA_data[CA_CHAIN].function = &commanddata::function_CA_CHAIN;
	CA_data[CA_ENDCHAIN].function = &commanddata::function_CA_ENDCHAIN;
	CA_data[CA_NEWATOM].function = &commanddata::function_CA_NEWATOM;
	CA_data[CA_NEWATOMFRAC].function = &commanddata::function_CA_NEWATOMFRAC;
	CA_data[CA_SELECTATOM].function = &commanddata::function_CA_SELECTATOM;
	CA_data[CA_SETCOORDS].function = &commanddata::function_CA_SETCOORDS;
	CA_data[CA_SETCHARGE].function = &commanddata::function_CA_SETCHARGE;
	CA_data[CA_SETELEMENT].function = &commanddata::function_CA_SETELEMENT;
	CA_data[CA_SETFORCES].function = &commanddata::function_CA_SETFORCES;
	CA_data[CA_SETFX].function = &commanddata::function_CA_SETFX;
	CA_data[CA_SETFY].function = &commanddata::function_CA_SETFY;
	CA_data[CA_SETFZ].function = &commanddata::function_CA_SETFZ;
	CA_data[CA_SETID].function = &commanddata::function_CA_SETID;
	CA_data[CA_SETRX].function = &commanddata::function_CA_SETRX;
	CA_data[CA_SETRY].function = &commanddata::function_CA_SETRY;
	CA_data[CA_SETRZ].function = &commanddata::function_CA_SETRZ;
	CA_data[CA_SETVELOCITIES].function = &commanddata::function_CA_SETVELOCITIES;
	CA_data[CA_SETVX].function = &commanddata::function_CA_SETVX;
	CA_data[CA_SETVY].function = &commanddata::function_CA_SETVY;
	CA_data[CA_SETVZ].function = &commanddata::function_CA_SETVZ;

	// Bond commands
	CA_data[CA_AUGMENT].function = &commanddata::function_CA_AUGMENT;
	CA_data[CA_BONDTOLERANCE].function = &commanddata::function_CA_BONDTOLERANCE;
	CA_data[CA_NEWBOND].function = &commanddata::function_CA_NEWBOND;
	CA_data[CA_NEWBONDID].function = &commanddata::function_CA_NEWBONDID;
	CA_data[CA_REBONDPATTERNS].function = &commanddata::function_CA_REBONDPATTERNS;
	CA_data[CA_REBONDSELECTION].function = &commanddata::function_CA_REBONDSELECTION;
	CA_data[CA_CLEARBONDS].function = &commanddata::function_CA_CLEARBONDS;
	CA_data[CA_REBOND].function = &commanddata::function_CA_REBOND;

	// Build commands
	CA_data[CA_ADDHYDROGEN].function = &commanddata::function_CA_ADDHYDROGEN;
	CA_data[CA_DELETE].function = &commanddata::function_CA_DELETE;
	CA_data[CA_LOCATE].function = &commanddata::function_CA_LOCATE;
	CA_data[CA_MOVE].function = &commanddata::function_CA_MOVE;
	CA_data[CA_ROTX].function = &commanddata::function_CA_ROTX;
	CA_data[CA_ROTY].function = &commanddata::function_CA_ROTY;
	CA_data[CA_ROTZ].function = &commanddata::function_CA_ROTZ;
	CA_data[CA_TRANSMUTE].function = &commanddata::function_CA_TRANSMUTE;

	// Cell commands
	CA_data[CA_FOLD].function = &commanddata::function_CA_FOLD;
	CA_data[CA_FRACTOREAL].function = &commanddata::function_CA_FRACTOREAL;
	CA_data[CA_PACK].function = &commanddata::function_CA_PACK;
	CA_data[CA_PRINTCELL].function = &commanddata::function_CA_PRINTCELL;
	CA_data[CA_REPLICATECELL].function = &commanddata::function_CA_REPLICATECELL;
	CA_data[CA_SCALECELL].function = &commanddata::function_CA_SCALECELL;
	CA_data[CA_SETCELL].function = &commanddata::function_CA_SETCELL;
	CA_data[CA_SETCELLAXES].function = &commanddata::function_CA_SETCELLAXES;
	CA_data[CA_SETSPACEGROUP].function = &commanddata::function_CA_SETSPACEGROUP;

	// Charge commands
	CA_data[CA_CHARGEFF].function = &commanddata::function_CA_CHARGEFF;
	CA_data[CA_CHARGEFROMMODEL].function = &commanddata::function_CA_CHARGEFROMMODEL;
	CA_data[CA_CHARGEPATOM].function = &commanddata::function_CA_CHARGEPATOM;
	CA_data[CA_CHARGESELECTION].function = &commanddata::function_CA_CHARGESELECTION;
	CA_data[CA_CHARGETYPE].function = &commanddata::function_CA_CHARGETYPE;
	CA_data[CA_CLEARCHARGES].function = &commanddata::function_CA_CLEARCHARGES;

	// Disordered build commands
	CA_data[CA_ADDCOMPONENT].function = &commanddata::function_CA_ADDCOMPONENT;
	CA_data[CA_DISORDER].function = &commanddata::function_CA_DISORDER;
	CA_data[CA_PRINTCOMPONENTS].function = &commanddata::function_CA_PRINTCOMPONENTS;
	CA_data[CA_SETCENTRE].function = &commanddata::function_CA_SETCENTRE;
	CA_data[CA_SETGEOMETRY].function = &commanddata::function_CA_SETGEOMETRY;
	CA_data[CA_SETOVERLAP].function = &commanddata::function_CA_SETOVERLAP;
	CA_data[CA_SETSHAPE].function = &commanddata::function_CA_SETSHAPE;
	CA_data[CA_VDWSCALE].function = &commanddata::function_CA_VDWSCALE;

	// Energy Commands
	CA_data[CA_FRAMEENERGY].function = &commanddata::function_CA_FRAMEENERGY;
	CA_data[CA_MODELENERGY].function = &commanddata::function_CA_MODELENERGY;
	CA_data[CA_PRINTELEC].function = &commanddata::function_CA_PRINTELEC;
	CA_data[CA_PRINTEWALD].function = &commanddata::function_CA_PRINTEWALD;
	CA_data[CA_PRINTINTER].function = &commanddata::function_CA_PRINTINTER;
	CA_data[CA_PRINTINTRA].function = &commanddata::function_CA_PRINTINTRA;
	CA_data[CA_PRINTENERGY].function = &commanddata::function_CA_PRINTENERGY;
	CA_data[CA_PRINTSUMMARY].function = &commanddata::function_CA_PRINTSUMMARY;
	CA_data[CA_PRINTVDW].function = &commanddata::function_CA_PRINTVDW;

	// Expression Commands
	CA_data[CA_CREATEEXPRESSION].function = &commanddata::function_CA_CREATEEXPRESSION;
	CA_data[CA_PRINTSETUP].function = &commanddata::function_CA_PRINTSETUP;
	CA_data[CA_SAVEEXPRESSION].function = &commanddata::function_CA_SAVEEXPRESSION;

	// Flow control
	CA_data[CA_ELSE].function = &commanddata::function_CA_ELSE;
	CA_data[CA_ELSEIF].function = &commanddata::function_CA_ELSEIF;
	CA_data[CA_END].function = &commanddata::function_CA_END;
	CA_data[CA_FOR].function = &commanddata::function_CA_FOR;
	CA_data[CA_GOTO].function = &commanddata::function_CA_GOTO;
	CA_data[CA_GOTONONIF].function = &commanddata::function_CA_GOTONONIF;
	CA_data[CA_IF].function = &commanddata::function_CA_IF;
	CA_data[CA_TERMINATE].function = &commanddata::function_CA_TERMINATE;

	// Force Commands
	CA_data[CA_FRAMEFORCES].function = &commanddata::function_CA_FRAMEFORCES;
	CA_data[CA_MODELFORCES].function = &commanddata::function_CA_MODELFORCES;
	CA_data[CA_PRINTFORCES].function = &commanddata::function_CA_PRINTFORCES;

	// Forcefield Commands
	CA_data[CA_FFMODEL].function = &commanddata::function_CA_FFMODEL;
	CA_data[CA_FFPATTERN].function = &commanddata::function_CA_FFPATTERN;
	CA_data[CA_FFPATTERNID].function = &commanddata::function_CA_FFPATTERNID;
	CA_data[CA_GETFF].function = &commanddata::function_CA_GETFF;
	CA_data[CA_LOADFF].function = &commanddata::function_CA_LOADFF;
	CA_data[CA_TYPEMODEL].function = &commanddata::function_CA_TYPEMODEL;
	CA_data[CA_TYPETEST].function = &commanddata::function_CA_TYPETEST;

	// Glyph commands
	CA_data[CA_NEWGLYPH].function = &commanddata::function_CA_NEWGLYPH;
	CA_data[CA_SETGLYPHATOMF].function = &commanddata::function_CA_SETGLYPHATOMF;
	CA_data[CA_SETGLYPHATOMR].function = &commanddata::function_CA_SETGLYPHATOMR;
	CA_data[CA_SETGLYPHATOMV].function = &commanddata::function_CA_SETGLYPHATOMV;
	CA_data[CA_SETGLYPHATOMSF].function = &commanddata::function_CA_SETGLYPHATOMSF;
	CA_data[CA_SETGLYPHATOMSR].function = &commanddata::function_CA_SETGLYPHATOMSR;
	CA_data[CA_SETGLYPHATOMSV].function = &commanddata::function_CA_SETGLYPHATOMSV;
	CA_data[CA_SETGLYPHDATA].function = &commanddata::function_CA_SETGLYPHDATA;
	CA_data[CA_SETGLYPHSOLID].function = &commanddata::function_CA_SETGLYPHSOLID;

	// Grid Commands
	CA_data[CA_ADDGRIDPOINT].function = &commanddata::function_CA_ADDGRIDPOINT;
	CA_data[CA_ADDNEXTGRIDPOINT].function = &commanddata::function_CA_ADDNEXTGRIDPOINT;
	CA_data[CA_FINALISEGRID].function = &commanddata::function_CA_FINALISEGRID;
	CA_data[CA_NEWGRID].function = &commanddata::function_CA_NEWGRID;
	CA_data[CA_SETGRID].function = &commanddata::function_CA_SETGRID;
	CA_data[CA_SETGRIDCUBIC].function = &commanddata::function_CA_SETGRIDCUBIC;
	CA_data[CA_SETGRIDLOOPORDER].function = &commanddata::function_CA_SETGRIDLOOPORDER;
	CA_data[CA_SETGRIDORIGIN].function = &commanddata::function_CA_SETGRIDORIGIN;
	CA_data[CA_SETGRIDORTHO].function = &commanddata::function_CA_SETGRIDORTHO;
	CA_data[CA_SETGRIDSIZE].function = &commanddata::function_CA_SETGRIDSIZE;

	// Image Commands
	CA_data[CA_SAVEBITMAP].function = &commanddata::function_CA_SAVEBITMAP;
	CA_data[CA_SAVEVECTOR].function = &commanddata::function_CA_SAVEVECTOR;

	// Labeling Commands
	CA_data[CA_CLEARLABELS].function = &commanddata::function_CA_CLEARLABELS;
	CA_data[CA_LABEL].function = &commanddata::function_CA_LABEL;
	CA_data[CA_REMOVELABEL].function = &commanddata::function_CA_REMOVELABEL;

	// MC Commands
	CA_data[CA_MCACCEPT].function = &commanddata::function_CA_MCACCEPT;
	CA_data[CA_MCALLOW].function = &commanddata::function_CA_MCALLOW;
	CA_data[CA_MCMAXSTEP].function = &commanddata::function_CA_MCMAXSTEP;
	CA_data[CA_MCNTRIALS].function = &commanddata::function_CA_MCNTRIALS;
	CA_data[CA_PRINTMC].function = &commanddata::function_CA_PRINTMC;

	// Messaging Commands
	CA_data[CA_ERROR].function = &commanddata::function_CA_ERROR;
	CA_data[CA_PRINT].function = &commanddata::function_CA_PRINT;
	CA_data[CA_VERBOSE].function = &commanddata::function_CA_VERBOSE;
	CA_data[CA_WARN].function = &commanddata::function_CA_WARN;

	// Minimisation Commands
	CA_data[CA_CGMINIMISE].function = &commanddata::function_CA_CGMINIMISE;
	CA_data[CA_CONVERGE].function = &commanddata::function_CA_CONVERGE;
	CA_data[CA_LINETOL].function = &commanddata::function_CA_LINETOL;
	CA_data[CA_MCMINIMISE].function = &commanddata::function_CA_MCMINIMISE;
	CA_data[CA_SDMINIMISE].function = &commanddata::function_CA_SDMINIMISE;
	CA_data[CA_SIMPLEXMINIMISE].function = &commanddata::function_CA_SIMPLEXMINIMISE;
	
	// Model Commands
	CA_data[CA_CREATEATOMS].function = &commanddata::function_CA_CREATEATOMS;
	CA_data[CA_FINALISEMODEL].function = &commanddata::function_CA_FINALISEMODEL;
	CA_data[CA_GETMODEL].function = &commanddata::function_CA_GETMODEL;
	CA_data[CA_LISTMODELS].function = &commanddata::function_CA_LISTMODELS;
	CA_data[CA_LOADMODEL].function = &commanddata::function_CA_LOADMODEL;
	CA_data[CA_MODELTEMPLATE].function = &commanddata::function_CA_MODELTEMPLATE;
	CA_data[CA_NEWMODEL].function = &commanddata::function_CA_NEWMODEL;
	CA_data[CA_INFO].function = &commanddata::function_CA_INFO;
	CA_data[CA_SAVEMODEL].function = &commanddata::function_CA_SAVEMODEL;
	CA_data[CA_SETTITLE].function = &commanddata::function_CA_SETTITLE;

	// Pattern Commands
	CA_data[CA_CLEARPATTERNS].function = &commanddata::function_CA_CLEARPATTERNS;
	CA_data[CA_CREATEPATTERNS].function = &commanddata::function_CA_CREATEPATTERNS;
	CA_data[CA_GETPATTERN].function = &commanddata::function_CA_GETPATTERN;
	CA_data[CA_LISTPATTERNS].function = &commanddata::function_CA_LISTPATTERNS;
	CA_data[CA_NEWPATTERN].function = &commanddata::function_CA_NEWPATTERN;

	// Preferences Commands
	CA_data[CA_ATOMDETAIL].function = &commanddata::function_CA_ATOMDETAIL;
	CA_data[CA_BONDDETAIL].function = &commanddata::function_CA_BONDDETAIL;
	CA_data[CA_COLOUR].function = &commanddata::function_CA_COLOUR;
	CA_data[CA_DENSITYUNITS].function = &commanddata::function_CA_DENSITYUNITS;
	CA_data[CA_ECUT].function = &commanddata::function_CA_ECUT;
	CA_data[CA_ELEC].function = &commanddata::function_CA_ELEC;
	CA_data[CA_ELEMENTAMBIENT].function = &commanddata::function_CA_ELEMENTAMBIENT;
	CA_data[CA_ELEMENTDIFFUSE].function = &commanddata::function_CA_ELEMENTDIFFUSE;
	CA_data[CA_ELEMENTRADIUS].function = &commanddata::function_CA_ELEMENTRADIUS;
	CA_data[CA_ENERGYUNITS].function = &commanddata::function_CA_ENERGYUNITS;
	CA_data[CA_GL].function = &commanddata::function_CA_GL;
	CA_data[CA_INTRA].function = &commanddata::function_CA_INTRA;
	CA_data[CA_KEY].function = &commanddata::function_CA_KEY;
	CA_data[CA_MOUSE].function = &commanddata::function_CA_MOUSE;
	CA_data[CA_RADIUS].function = &commanddata::function_CA_RADIUS;
	CA_data[CA_SHININESS].function = &commanddata::function_CA_SHININESS;
	CA_data[CA_SHOW].function = &commanddata::function_CA_SHOW;
	CA_data[CA_STYLE].function = &commanddata::function_CA_STYLE;
	CA_data[CA_VCUT].function = &commanddata::function_CA_VCUT;
	CA_data[CA_VDW].function = &commanddata::function_CA_VDW;

	// Read / Write Commands
	CA_data[CA_ADDREADOPTION].function = &commanddata::function_CA_ADDREADOPTION;
	CA_data[CA_FIND].function = &commanddata::function_CA_FIND;
	CA_data[CA_READCHARS].function = &commanddata::function_CA_READCHARS;
	CA_data[CA_READDOUBLE].function = &commanddata::function_CA_READDOUBLE;
	CA_data[CA_READINTEGER].function = &commanddata::function_CA_READINTEGER;
	CA_data[CA_READLINE].function = &commanddata::function_CA_READLINE;
	CA_data[CA_READNEXT].function = &commanddata::function_CA_READNEXT;
	CA_data[CA_READVAR].function = &commanddata::function_CA_READVAR;
	CA_data[CA_REMOVEREADOPTION].function = &commanddata::function_CA_REMOVEREADOPTION;
	CA_data[CA_REWIND].function = &commanddata::function_CA_REWIND;
	CA_data[CA_SKIPCHARS].function = &commanddata::function_CA_SKIPCHARS;
	CA_data[CA_SKIPLINE].function = &commanddata::function_CA_SKIPLINE;
	CA_data[CA_WRITELINE].function = &commanddata::function_CA_WRITELINE;

	// Script Commands
	CA_data[CA_LISTSCRIPTS].function = &commanddata::function_CA_LISTSCRIPTS;
	CA_data[CA_LOADSCRIPT].function = &commanddata::function_CA_LOADSCRIPT;
	CA_data[CA_RUNSCRIPT].function = &commanddata::function_CA_RUNSCRIPT;

	// Select Commands
	CA_data[CA_SELECTALL].function = &commanddata::function_CA_SELECTALL;
	CA_data[CA_SELECTATOM].function = &commanddata::function_CA_SELECTATOM;
	CA_data[CA_SELECTELEMENT].function = &commanddata::function_CA_SELECTELEMENT;
	CA_data[CA_SELECTFFTYPE].function = &commanddata::function_CA_SELECTFFTYPE;
	CA_data[CA_SELECTINVERT].function = &commanddata::function_CA_SELECTINVERT;
	CA_data[CA_SELECTNONE].function = &commanddata::function_CA_SELECTNONE;
	CA_data[CA_SELECTOVERLAPS].function = &commanddata::function_CA_SELECTOVERLAPS;
	CA_data[CA_SELECTTYPE].function = &commanddata::function_CA_SELECTTYPE;
	
	// Site Commands
	CA_data[CA_GETSITE].function = &commanddata::function_CA_GETSITE;
	CA_data[CA_LISTSITES].function = &commanddata::function_CA_LISTSITES;
	CA_data[CA_NEWSITE].function = &commanddata::function_CA_NEWSITE;
	CA_data[CA_SETAXES].function = &commanddata::function_CA_SETAXES;

	// System Commands
	CA_data[CA_GUI].function = &commanddata::function_CA_GUI;
	CA_data[CA_HELP].function = &commanddata::function_CA_HELP;
	CA_data[CA_QUIT].function = &commanddata::function_CA_QUIT;
	
	// Trajectory Commands
	CA_data[CA_FIRSTFRAME].function = &commanddata::function_CA_FIRSTFRAME;
	CA_data[CA_LASTFRAME].function = &commanddata::function_CA_LASTFRAME;
	CA_data[CA_LOADTRAJECTORY].function = &commanddata::function_CA_LOADTRAJECTORY;
	CA_data[CA_NEXTFRAME].function = &commanddata::function_CA_NEXTFRAME;
	CA_data[CA_PREVFRAME].function = &commanddata::function_CA_PREVFRAME;

	// Transform Commands
	CA_data[CA_CENTRE].function = &commanddata::function_CA_CENTRE;
	CA_data[CA_TRANSLATE].function = &commanddata::function_CA_TRANSLATE;
	CA_data[CA_TRANSLATEATOM].function = &commanddata::function_CA_TRANSLATEATOM;
	CA_data[CA_MIRROR].function = &commanddata::function_CA_MIRROR;

	// Variable Commands
	CA_data[CA_LET].function = &commanddata::function_CA_LET;
	CA_data[CA_INCREASE].function = &commanddata::function_CA_INCREASE;
	CA_data[CA_DECREASE].function = &commanddata::function_CA_DECREASE;
	CA_data[CA_EVAL].function = &commanddata::function_CA_EVAL;

	// View Commands
	CA_data[CA_RESETVIEW].function = &commanddata::function_CA_RESETVIEW;
	CA_data[CA_ROTATEVIEW].function = &commanddata::function_CA_ROTATEVIEW;
	CA_data[CA_SPEEDTEST].function = &commanddata::function_CA_SPEEDTEST;
	CA_data[CA_TRANSLATEVIEW].function = &commanddata::function_CA_TRANSLATEVIEW;
	CA_data[CA_ZOOMVIEW].function = &commanddata::function_CA_ZOOMVIEW;
	CA_data[CA_ZROTATEVIEW].function = &commanddata::function_CA_ZROTATEVIEW;
}

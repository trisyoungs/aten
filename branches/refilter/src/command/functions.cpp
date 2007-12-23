/*
	*** Command function pointers
	*** src/command/commands.cpp
	Copyright T. Youngs 2007

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

#include "command/functions.h"

// Constructor
command_functions::command_functions()
{
	/*
	// Store pointers to all command functions
	*/

	action[CA_ROOTNODE] = &command_functions::function_CA_ROOTNODE;

	// Analyse commands
	action[CA_FINALISE] = &command_functions::function_CA_FINALISE;
	action[CA_FRAMEANALYSE] = &command_functions::function_CA_FRAMEANALYSE;
	action[CA_MODELANALYSE] = &command_functions::function_CA_MODELANALYSE;
	action[CA_PDENS] = &command_functions::function_CA_PDENS;
	action[CA_PRINTJOBS] = &command_functions::function_CA_PRINTJOBS;
	action[CA_RDF] = &command_functions::function_CA_RDF;
	action[CA_SAVEQUANTITIES] = &command_functions::function_CA_SAVEQUANTITIES;

	// Bond commands
	action[CA_ADDBOND] = &command_functions::function_CA_ADDBOND;
	action[CA_AUGMENT] = &command_functions::function_CA_AUGMENT;
	action[CA_BONDTOLERANCE] = &command_functions::function_CA_BONDTOLERANCE;
	action[CA_BONDPATTERNS] = &command_functions::function_CA_BONDPATTERNS;
	action[CA_BONDSELECTION] = &command_functions::function_CA_BONDSELECTION;
	action[CA_CLEARBONDS] = &command_functions::function_CA_CLEARBONDS;
	action[CA_REBOND] = &command_functions::function_CA_REBOND;

	// Build commands
	action[CA_ADDHYDROGEN] = &command_functions::function_CA_ADDHYDROGEN;
	action[CA_ADDATOM] = &command_functions::function_CA_ADDATOM;
	action[CA_ADDCHAIN] = &command_functions::function_CA_ADDCHAIN;
	action[CA_DELETE] = &command_functions::function_CA_DELETE;
	action[CA_ENDCHAIN] = &command_functions::function_CA_ENDCHAIN;
	action[CA_LOCATE] = &command_functions::function_CA_LOCATE;
	action[CA_MOVE] = &command_functions::function_CA_MOVE;
	action[CA_ROTX] = &command_functions::function_CA_ROTX;
	action[CA_ROTY] = &command_functions::function_CA_ROTY;
	action[CA_ROTZ] = &command_functions::function_CA_ROTZ;
	action[CA_TRANSMUTE] = &command_functions::function_CA_TRANSMUTE;

	// Cell commands
	action[CA_FOLD] = &command_functions::function_CA_FOLD;
	action[CA_FRACTOREAL] = &command_functions::function_CA_FRACTOREAL;
	action[CA_PACK] = &command_functions::function_CA_PACK;
	action[CA_PRINTCELL] = &command_functions::function_CA_PRINTCELL;
	action[CA_REPLICATECELL] = &command_functions::function_CA_REPLICATECELL;
	action[CA_SCALECELL] = &command_functions::function_CA_SCALECELL;
	action[CA_SETCELL] = &command_functions::function_CA_SETCELL;
	action[CA_SETCELLAXES] = &command_functions::function_CA_SETCELLAXES;
	action[CA_SETSPACEGROUP] = &command_functions::function_CA_SETSPACEGROUP;

	// Charge commands
	action[CA_CHARGEATOM] = &command_functions::function_CA_CHARGEATOM;
	action[CA_CHARGEFF] = &command_functions::function_CA_CHARGEFF;
	action[CA_CHARGEFROMMODEL] = &command_functions::function_CA_CHARGEFROMMODEL;
	action[CA_CHARGEPATOM] = &command_functions::function_CA_CHARGEPATOM;
	action[CA_CHARGESELECTION] = &command_functions::function_CA_CHARGESELECTION;
	action[CA_CHARGETYPE] = &command_functions::function_CA_CHARGETYPE;
	action[CA_CLEARCHARGES] = &command_functions::function_CA_CLEARCHARGES;

	// Disordered build commands
	action[CA_ADDCOMPONENT] = &command_functions::function_CA_ADDCOMPONENT;
	action[CA_DISORDER] = &command_functions::function_CA_DISORDER;
	action[CA_PRINTCOMPONENTS] = &command_functions::function_CA_PRINTCOMPONENTS;
	action[CA_SETCENTRE] = &command_functions::function_CA_SETCENTRE;
	action[CA_SETGEOMETRY] = &command_functions::function_CA_SETGEOMETRY;
	action[CA_SETOVERLAP] = &command_functions::function_CA_SETOVERLAP;
	action[CA_SETSHAPE] = &command_functions::function_CA_SETSHAPE;
	action[CA_VDWSCALE] = &command_functions::function_CA_VDWSCALE;

	// Element Commands
	action[CA_SETELEMENTCOLOUR] = &command_functions::function_CA_SETELEMENTCOLOUR;
	action[CA_SETELEMENTRADIUS] = &command_functions::function_CA_SETELEMENTRADIUS;

	// Energy Commands
	action[CA_FRAMEENERGY] = &command_functions::function_CA_FRAMEENERGY;
	action[CA_MODELENERGY] = &command_functions::function_CA_MODELENERGY;
	action[CA_PRINTELEC] = &command_functions::function_CA_PRINTELEC;
	action[CA_PRINTEWALD] = &command_functions::function_CA_PRINTEWALD;
	action[CA_PRINTINTER] = &command_functions::function_CA_PRINTINTER;
	action[CA_PRINTINTRA] = &command_functions::function_CA_PRINTINTRA;
	action[CA_PRINTENERGY] = &command_functions::function_CA_PRINTENERGY;
	action[CA_PRINTSUMMARY] = &command_functions::function_CA_PRINTSUMMARY;
	action[CA_PRINTVDW] = &command_functions::function_CA_PRINTVDW;

	// Expression Commands
	action[CA_CREATEEXPRESSION] = &command_functions::function_CA_CREATEEXPRESSION;
	action[CA_ECUT] = &command_functions::function_CA_ECUT;
	action[CA_ELEC] = &command_functions::function_CA_ELEC;
	action[CA_INTRA] = &command_functions::function_CA_INTRA;
	action[CA_PRINTEXPRESSION] = &command_functions::function_CA_PRINTEXPRESSION;
	action[CA_VCUT] = &command_functions::function_CA_VCUT;
	action[CA_VDW] = &command_functions::function_CA_VDW;

	// Field Commands
	action[CA_SAVEFIELD] = &command_functions::function_CA_SAVEFIELD;
	action[CA_SAVEFIELD2] = &command_functions::function_CA_SAVEFIELD2;

	// Flow control
	action[CA_ELSE] = &command_functions::function_CA_ELSE;
	action[CA_ELSEIF] = &command_functions::function_CA_ELSEIF;
	action[CA_END] = &command_functions::function_CA_END;
	action[CA_FOR] = &command_functions::function_CA_FOR;
	action[CA_GOTO] = &command_functions::function_CA_GOTO;
	action[CA_GOTONONIF] = &command_functions::function_CA_GOTONONIF;
	action[CA_IF] = &command_functions::function_CA_IF;
	action[CA_QUIT] = &command_functions::function_CA_QUIT;
	action[CA_TERMINATE] = &command_functions::function_CA_TERMINATE;

	// Force Commands
	action[CA_FRAMEFORCES] = &command_functions::function_CA_FRAMEFORCES;
	action[CA_MODELFORCES] = &command_functions::function_CA_MODELFORCES;
	action[CA_PRINTFORCES] = &command_functions::function_CA_PRINTFORCES;

	// Forcefield Commands
	action[CA_FFMODEL] = &command_functions::function_CA_FFMODEL;
	action[CA_FFPATTERN] = &command_functions::function_CA_FFPATTERN;
	action[CA_FFPATTERNID] = &command_functions::function_CA_FFPATTERNID;
	action[CA_LOADFF] = &command_functions::function_CA_LOADFF;
	action[CA_SELECTFF] = &command_functions::function_CA_SELECTFF;
	action[CA_TYPEMODEL] = &command_functions::function_CA_TYPEMODEL;
	action[CA_TYPETEST] = &command_functions::function_CA_TYPETEST;

	// Grid Commands
	action[CA_ADDGRIDPOINT] = &command_functions::function_CA_ADDGRIDPOINT;
	action[CA_ADDNEXTGRIDPOINT] = &command_functions::function_CA_ADDNEXTGRIDPOINT;
	action[CA_FINALISEGRID] = &command_functions::function_CA_FINALISEGRID;
	action[CA_NEWGRID] = &command_functions::function_CA_NEWGRID;
	action[CA_SETGRID] = &command_functions::function_CA_SETGRID;
	action[CA_SETGRIDCUBIC] = &command_functions::function_CA_SETGRIDCUBIC;
	action[CA_SETGRIDORIGIN] = &command_functions::function_CA_SETGRIDORIGIN;
	action[CA_SETGRIDORTHO] = &command_functions::function_CA_SETGRIDORTHO;
	action[CA_SETGRIDSIZE] = &command_functions::function_CA_SETGRIDSIZE;

	//image_

	// Labeling commands
	action[CA_CLEARLABELS] = &command_functions::function_CA_CLEARLABELS;
	action[CA_ADDLABEL] = &command_functions::function_CA_ADDLABEL;
	action[CA_REMOVELABEL] = &command_functions::function_CA_REMOVELABEL;

	// MC Commands
	action[CA_MCACCEPT] = &command_functions::function_CA_MCACCEPT;
	action[CA_MCALLOW] = &command_functions::function_CA_MCALLOW;
	action[CA_MCMAXSTEP] = &command_functions::function_CA_MCMAXSTEP;
	action[CA_MCNTRIALS] = &command_functions::function_CA_MCNTRIALS;
	action[CA_PRINTMC] = &command_functions::function_CA_PRINTMC;

	// Messaging
	action[CA_PRINT] = &command_functions::function_CA_PRINT;

	// Minimisation Commands
	action[CA_CGMINIMISE] = &command_functions::function_CA_CGMINIMISE;
	action[CA_CONVERGE] = &command_functions::function_CA_CONVERGE;
	action[CA_LINETOL] = &command_functions::function_CA_LINETOL;
	action[CA_MCMINIMISE] = &command_functions::function_CA_MCMINIMISE;
	action[CA_SDMINIMISE] = &command_functions::function_CA_SDMINIMISE;
	action[CA_SIMPLEXMINIMISE] = &command_functions::function_CA_SIMPLEXMINIMISE;
	
	// Model Commands
	action[CA_CREATEATOMS] = &command_functions::function_CA_CREATEATOMS;
	action[CA_FINALISEMODEL] = &command_functions::function_CA_FINALISEMODEL;
	action[CA_LISTMODELS] = &command_functions::function_CA_LISTMODELS;
	action[CA_LOADMODEL] = &command_functions::function_CA_LOADMODEL;
	action[CA_MODELTEMPLATE] = &command_functions::function_CA_MODELTEMPLATE;
	action[CA_NEWMODEL] = &command_functions::function_CA_NEWMODEL;
	action[CA_PRINTMODEL] = &command_functions::function_CA_PRINTMODEL;
	action[CA_SAVEMODEL] = &command_functions::function_CA_SAVEMODEL;
	action[CA_SELECTMODEL] = &command_functions::function_CA_SELECTMODEL;
	action[CA_SETATOM] = &command_functions::function_CA_SETATOM;
	action[CA_SETTITLE] = &command_functions::function_CA_SETTITLE;

	// Pattern Commands
	action[CA_ADDPATTERN] = &command_functions::function_CA_ADDPATTERN;
	action[CA_CLEARPATTERNS] = &command_functions::function_CA_CLEARPATTERNS;
	action[CA_CREATEPATTERNS] = &command_functions::function_CA_CREATEPATTERNS;
	action[CA_PRINTPATTERNS] = &command_functions::function_CA_PRINTPATTERNS;
	action[CA_SELECTPATTERN] = &command_functions::function_CA_SELECTPATTERN;

	// Preferences Commands
	action[CA_ATOMDETAIL] = &command_functions::function_CA_ATOMDETAIL;
	action[CA_BONDDETAIL] = &command_functions::function_CA_BONDDETAIL;
	action[CA_COLOUR] = &command_functions::function_CA_COLOUR;
	action[CA_DENSITYUNITS] = &command_functions::function_CA_DENSITYUNITS;
	action[CA_ENERGYUNITS] = &command_functions::function_CA_ENERGYUNITS;
	action[CA_GL] = &command_functions::function_CA_GL;
	action[CA_KEY] = &command_functions::function_CA_KEY;
	action[CA_MOUSE] = &command_functions::function_CA_MOUSE;
	action[CA_RADIUS] = &command_functions::function_CA_RADIUS;
	action[CA_SHININESS] = &command_functions::function_CA_SHININESS;
	action[CA_SHOW] = &command_functions::function_CA_SHOW;
	action[CA_STYLE] = &command_functions::function_CA_STYLE;

	// Read / Write Commands
	action[CA_ADDREADOPTION] = &command_functions::function_CA_ADDREADOPTION;
	action[CA_FIND] = &command_functions::function_CA_FIND;
	action[CA_READCHARS] = &command_functions::function_CA_READCHARS;
	action[CA_READDOUBLE] = &command_functions::function_CA_READDOUBLE;
	action[CA_READINTEGER] = &command_functions::function_CA_READINTEGER;
	action[CA_READLINE] = &command_functions::function_CA_READLINE;
	action[CA_READNEXT] = &command_functions::function_CA_READNEXT;
	action[CA_READVAR] = &command_functions::function_CA_READVAR;
	action[CA_REMOVEREADOPTION] = &command_functions::function_CA_REMOVEREADOPTION;
	action[CA_SKIPCHARS] = &command_functions::function_CA_SKIPCHARS;
	action[CA_SKIPLINE] = &command_functions::function_CA_SKIPLINE;
	action[CA_WRITELINE] = &command_functions::function_CA_WRITELINE;

	// Select Commands
	action[CA_SELECTALL] = &command_functions::function_CA_SELECTALL;
	action[CA_SELECTATOM] = &command_functions::function_CA_SELECTATOM;
	action[CA_SELECTELEMENT] = &command_functions::function_CA_SELECTELEMENT;
	action[CA_SELECTFFTYPE] = &command_functions::function_CA_SELECTFFTYPE;
	action[CA_SELECTINVERT] = &command_functions::function_CA_SELECTINVERT;
	action[CA_SELECTNONE] = &command_functions::function_CA_SELECTNONE;
	action[CA_SELECTOVERLAPS] = &command_functions::function_CA_SELECTOVERLAPS;
	action[CA_SELECTTYPE] = &command_functions::function_CA_SELECTTYPE;
	
	// Site Commands
	action[CA_ADDSITE] = &command_functions::function_CA_ADDSITE;
	action[CA_PRINTSITES] = &command_functions::function_CA_PRINTSITES;
	action[CA_SELECTSITE] = &command_functions::function_CA_SELECTSITE;
	action[CA_SETAXES] = &command_functions::function_CA_SETAXES;
	
	// Trajectory Commands
	action[CA_FIRSTFRAME] = &command_functions::function_CA_FIRSTFRAME;
	action[CA_LASTFRAME] = &command_functions::function_CA_LASTFRAME;
	action[CA_LOADTRAJECTORY] = &command_functions::function_CA_LOADTRAJECTORY;
	action[CA_NEXTFRAME] = &command_functions::function_CA_NEXTFRAME;
	action[CA_PREVFRAME] = &command_functions::function_CA_PREVFRAME;

	// Transform Commands
	action[CA_CENTRE] = &command_functions::function_CA_CENTRE;
	action[CA_CENTRESELECTION] = &command_functions::function_CA_CENTRESELECTION;
	action[CA_TRANSLATE] = &command_functions::function_CA_TRANSLATE;
	action[CA_TRANSLATEATOM] = &command_functions::function_CA_TRANSLATEATOM;
	action[CA_TRANSLATESELECTION] = &command_functions::function_CA_TRANSLATESELECTION;
	action[CA_MIRRORSELECTION] = &command_functions::function_CA_MIRRORSELECTION;

	// Variables
	action[CA_LET] = &command_functions::function_CA_LET;
	action[CA_INCREASE] = &command_functions::function_CA_INCREASE;
	action[CA_DECREASE] = &command_functions::function_CA_DECREASE;
	action[CA_EVAL] = &command_functions::function_CA_EVAL;

}

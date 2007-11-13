/*
	*** Script commands
	*** src/script/scriptcommands.h
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

#ifndef H_SCRIPTCMDS_H
#define H_SCRIPTCMDS_H

// Script commands
enum script_command {
	// Analyse commands
	SC_FINALISE,
	SC_FRAMEANALYSE,
	SC_MODELANALYSE,
	SC_PDENS,
	SC_PRINTJOBS,
	SC_RDF,
	SC_SAVEQUANTITIES,

	// Bond commands
	SC_AUGMENT,
	SC_REBOND,
	SC_CLEARBONDS,
	SC_BONDTOLERANCE,
	SC_BONDPATTERNS,
	SC_BONDSELECTION,

	// Build commands
	SC_ADDHYDROGEN,
	SC_ADDATOM,
	SC_ADDCHAIN,
	SC_DELETE,
	SC_ENDCHAIN,
	SC_LOCATE,
	SC_MOVE,
	SC_ROTX,
	SC_ROTY,
	SC_ROTZ,
	SC_TRANSMUTE,

	// Cell commands
	SC_PRINTCELL,
	SC_REPLICATECELL,
	SC_SCALECELL,
	SC_SETCELL,

	// Charge commands
	SC_CHARGEATOM,
	SC_CHARGEFF,
	SC_CHARGEFROMMODEL,
	SC_CHARGEPATOM,
	SC_CHARGESELECTION,
	SC_CHARGETYPE,
	SC_CLEARCHARGES,

	// Disordered build commands
	SC_ADDCOMPONENT,
	SC_DISORDER,
	SC_PRINTCOMPONENTS,
	SC_SETCENTRE,
	SC_SETGEOMETRY,
	SC_SETOVERLAP,
	SC_SETSHAPE,
	SC_VDWSCALE,

	// Element Commands
	SC_SETELEMENTCOLOUR,
	SC_SETELEMENTRADIUS,

	// Energy Commands
	SC_FRAMEENERGY,
	SC_MODELENERGY,
	SC_PRINTELEC,
	SC_PRINTEWALD,
	SC_PRINTINTER,
	SC_PRINTINTRA,
	SC_PRINTENERGY,
	SC_PRINTSUMMARY,
	SC_PRINTVDW,

	// Expression Commands
	SC_CREATEEXPRESSION,
	SC_ECUT,
	SC_ELEC,
	SC_INTRA,
	SC_PRINTEXPRESSION,
	SC_VCUT,
	SC_VDW,

	// Forcefield Commands
	SC_FFMODEL,
	SC_FFPATTERN,
	SC_FFPATTERNID,
	SC_LOADFF,
	SC_SELECTFF,

	// Field Commands
	SC_SAVEFIELD,
	SC_SAVEFIELD2,

	// Force Commands
	SC_FRAMEFORCES,
	SC_MODELFORCES,
	SC_PRINTFORCES,

	//image_

	// MC Commands
	SC_MCACCEPT,
	SC_MCALLOW,
	SC_MCMAXSTEP,
	SC_MCNTRIALS,
	SC_PRINTMC,

	// Minimisation Commands
	SC_CGMINIMISE,
	SC_CONVERGE,
	SC_LINETOL,
	SC_MCMINIMISE,
	SC_SDMINIMISE,
	SC_SIMPLEXMINIMISE,
	
	// Model Commands
	SC_LISTMODELS,
	SC_LOADMODEL,
	SC_NEWMODEL,
	SC_PRINTMODEL,
	SC_SAVEMODEL,
	SC_SELECTMODEL,

	// Pattern Commands
	SC_ADDPATTERN,
	SC_CLEARPATTERNS,
	SC_CREATEPATTERNS,
	SC_PRINTPATTERNS,
	SC_SELECTPATTERN,

	// Preferences Commands
	SC_ATOMDETAIL,
	SC_BONDDETAIL,
	SC_COLOUR,
	SC_DENSITYUNITS,
	SC_ENERGYUNITS,
	SC_GL,
	SC_KEY,
	SC_MOUSE,
	SC_MOVESTYLE,
	SC_RADIUS,
	SC_SHININESS,
	SC_SHOW,
	SC_STYLE,

	// Select Commands
	SC_SELECTALL,
	SC_SELECTATOM,
	SC_SELECTELEMENT,
	SC_SELECTFFTYPE,
	SC_SELECTINVERT,
	SC_SELECTNONE,
	SC_SELECTOVERLAPS,
	SC_SELECTTYPE,
	
	// Site Commands
	SC_ADDSITE,
	SC_PRINTSITES,
	SC_SELECTSITE,
	SC_SETAXES,
	
	// Trajectory Commands
	SC_FIRSTFRAME,
	SC_LASTFRAME,
	SC_LOADTRAJECTORY,
	SC_NEXTFRAME,
	SC_PREVFRAME,

	// Translation Commands
	SC_TRANSLATEATOM,
	SC_TRANSLATESELECTION,
	SC_MIRRORSELECTION,

	SC_QUIT,

	SC_NITEMS
	};
script_command SC_from_text(const char*);
const char *text_from_SC(script_command);
const char *vars_from_SC(script_command);
const char *syntax_from_SC(script_command);
#endif

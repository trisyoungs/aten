/*
	*** Script functions
	*** src/script/script.cpp
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

#include "script/script.h"
#include "script/scriptcommands.h"
#include "base/master.h"
#include "base/prefs.h"
#include "base/sysfunc.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "classes/cell.h"
#include "classes/pattern.h"
#include "file/parse.h"
#include "file/filter.h"

// Constructor
script::script()
{
	activepattern = NULL;
	activeatom = NULL;
	activebond = NULL;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_SCRIPT] ++;
	#endif
}

// Destructor
script::~script()
{
	commands.commandlist.clear();
	#ifdef MEMDEBUG
		memdbg.destroy[MD_SCRIPT] ++;
	#endif
}

// Check active model
model *script::check_activemodel(const char *cmd)
{
	if (master.get_currentmodel() == NULL) msg(DM_NONE,"ERROR: %s require(s) an active model.\n",cmd);
	return master.get_currentmodel();
}

// Check active pattern
bool script::check_activepattern(const char *cmd)
{
	if (activepattern != NULL) return TRUE;
	msg(DM_NONE,"ERROR: %s require(s) an active pattern.\n",cmd);
	return FALSE;
}

// Check active atom
bool script::check_activeatom(const char *cmd)
{
	if (activeatom != NULL) return TRUE;
	msg(DM_NONE,"ERROR: %s require(s) an active atom.\n",cmd);
	return FALSE;
}

// Check active site
site *script::check_activesite(const char *cmd)
{
	if (activesite == NULL)	msg(DM_NONE,"ERROR: %s require(s) an active site.\n",cmd);
	return activesite;
}

// Check active forcefield
forcefield *script::check_activeff(const char *cmd)
{
	if (master.get_currentff() == NULL) msg(DM_NONE,"ERROR: %s require(s) an active forcefield.\n",cmd);
	return master.get_currentff();
}

// Check model trajectory
bool script::check_traj(const char *cmd)
{
	model *m = master.get_currentmodel();
	if ((m == NULL) || (m->get_totalframes() == 0))
	{
		msg(DM_NONE,"ERROR: %s require(s) an associated trajectory.\n",cmd);
		return FALSE;
	}
	return TRUE;
}

// Cache script file
bool script::load(const char *filename)
{
	dbg_begin(DM_CALLS,"script::load");
	int success, i;
	bool result;
	msg(DM_NONE,"Script  : %s...\n",filename);
	// Delete any old commands present and add commandlist to flowstack
	commands.clear();
	ifstream scriptfile(filename,ios::in);
	while (!scriptfile.eof())
	{
		success = parser.get_args_delim(&scriptfile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(DM_NONE,"script::load - Error reading script file.\n");
			dbg_end(DM_CALLS,"script::load");
			return FALSE;
		}
		else if (success == -1) break;
		// Add script command
		if (!cache_command())
		{
			dbg_end(DM_CALLS,"script::load");
			return FALSE;
		}
	}
	// Check the flowstack - it should contain just the BC_ROOTNODE branch
	if (commands.get_topbranch_type() != BC_ROOTNODE)
	{
		i = commands.get_branchstack_size() - 1;
		msg(DM_NONE,"Error within script: %i block%s not been terminated.>>>\n",i ,(i == 1 ? " has" : "s have"));
		dbg_end(DM_CALLS,"script::load");
		return FALSE;
	}
	dbg_end(DM_CALLS,"script::load");
	return TRUE;
}

// Cache script commands from line containing semicolon-separated commands
bool script::cache_line(const char *s)
{
	dbg_begin(DM_CALLS,"script::cache_line");
	// Use a local parser to split up the semi-colon'd line into individual commands
	static line_parser lines;
	// Prepare the new script
	commands.clear();
	lines.get_lines_delim(s);
	for (int n=0; n<lines.get_nargs(); n++)
	{
		// Parse the argument in our local line_parser and call cache_command())
		parser.get_args_delim(lines.argc(n), PO_USEQUOTES+PO_SKIPBLANKS);
		if (!cache_command())
		{
			dbg_end(DM_CALLS,"script::cache_line");
			return FALSE;
		}
	}
	dbg_end(DM_CALLS,"script::cache_line");
	return TRUE;
}

// Cache text command
bool script::cache_command()
{
	dbg_begin(DM_CALLS,"script::cache_command");
	script_command sc;
	basic_command bc;
	command_node<script_command> *fn;
	int success;
	bool result = TRUE;
	// Assume that the main parser object contains the data we require.
	// Check for basic commands (local to command_nodes) first.
	bc = BC_from_text(parser.argc(0));
	if (bc != BC_NITEMS)
	{
		// Add the command to the list
		if (!commands.add_basic(bc))
		{
			msg(DM_NONE,"script::load - Error adding basic command '%s'.\n", parser.argc(0));
			result = FALSE;
		}
	}
	else
	{
		// Find the script command and add this to the command list
		sc = SC_from_text(parser.argc(0));
		if (sc != SC_NITEMS)
		{
			// If add_other() returns FALSE then we encountered an error
			if (!commands.add_other(sc, text_from_SC(sc), vars_from_SC(sc)))
			{
				msg(DM_NONE,"Error adding script command '%s'.\n", parser.argc(0));
				msg(DM_NONE,  "Command usage is '%s'.\n", syntax_from_SC(sc));
				result = FALSE;
			}
		}
		else
		{
			msg(DM_NONE,"Unrecognised command '%s' in script.\n", parser.argc(0));
			result = FALSE;
		}
	}
	dbg_end(DM_CALLS,"script::cache_command");
	return result;
}

// Execute script
void script::run()
{
	// Execute the script commands stored in the structure
	dbg_begin(DM_CALLS,"script::run");
	bool result, finished;
	command_node<script_command> *cmd = commands.commandlist.first();
	// We only show the GUI if there is no 'quit' command (finished == FALSE) and all commands succeeded.
	finished = FALSE;
	prefs.set_showgui(FALSE);
	while (cmd != NULL)
	{
		result = FALSE;
		// Is this a basic command node?
		if (cmd->get_basic_command() != BC_OTHER)
		{
			msg(DM_VERBOSE,"(( Script executing basic command '%s' ))\n", text_from_BC(cmd->get_basic_command()));
			result = commands.do_basic(cmd, master.get_currentmodel(), NULL);
		}
		else
		{
			msg(DM_VERBOSE,"(( Script executing command '%s' ))\n", text_from_SC(cmd->get_command()));
			switch (cmd->get_command())
			{
				case (SC_FINALISE):
				case (SC_FRAMEANALYSE):
				case (SC_MODELANALYSE):
				case (SC_PDENS):
				case (SC_PRINTJOBS):
				case (SC_RDF):
				case (SC_SAVEQUANTITIES):
					result = command_analyse(cmd);
					break;

				case (SC_AUGMENT):
				case (SC_REBOND):
				case (SC_CLEARBONDS):
				case (SC_BONDTOLERANCE):
				case (SC_BONDPATTERNS):
				case (SC_BONDSELECTION):
					result = command_bonds(cmd);
					break;

				case (SC_ADDHYDROGEN):
				case (SC_ADDATOM):
				case (SC_ADDCHAIN):
				case (SC_DELETE):
				case (SC_ENDCHAIN):
				case (SC_LOCATE):
				case (SC_MOVE):
				case (SC_ROTX):
				case (SC_ROTY):
				case (SC_ROTZ):
				case (SC_TRANSMUTE):
					result = command_build(cmd);
					break;
	
				case (SC_PRINTCELL):
				case (SC_REPLICATECELL):
				case (SC_SCALECELL):
				case (SC_SETCELL):
					result = command_cell(cmd);
					break;

				case (SC_CHARGEATOM):
				case (SC_CHARGEFF):
				case (SC_CHARGEFROMMODEL):
				case (SC_CHARGEPATOM):
				case (SC_CHARGESELECTION):
				case (SC_CHARGETYPE):
				case (SC_CLEARCHARGES):
					result = command_charge(cmd);
					break;
	
				case (SC_ADDCOMPONENT):
				case (SC_DISORDER):
				case (SC_PRINTCOMPONENTS):
				case (SC_SETCENTRE):
				case (SC_SETGEOMETRY):
				case (SC_SETOVERLAP):
				case (SC_SETSHAPE):
				case (SC_VDWSCALE):
					result = command_disorder(cmd);
					break;
	
				case (SC_FRAMEENERGY):
				case (SC_MODELENERGY):
				case (SC_PRINTELEC):
				case (SC_PRINTEWALD):
				case (SC_PRINTINTER):
				case (SC_PRINTINTRA):
				case (SC_PRINTENERGY):
				case (SC_PRINTSUMMARY):
				case (SC_PRINTVDW):
					result = command_energy(cmd);
					break;

				case (SC_CREATEEXPRESSION):
				case (SC_ECUT):
				case (SC_ELEC):
				case (SC_INTRA):
				case (SC_PRINTEXPRESSION):
				case (SC_VCUT):
				case (SC_VDW):
					result = command_expr(cmd);
					break;
	
				case (SC_FFMODEL):
				case (SC_FFPATTERN):
				case (SC_FFPATTERNID):
				case (SC_LOADFF):
				case (SC_SELECTFF):
					result = command_ff(cmd);
					break;
				case (SC_SAVEFIELD):
				case (SC_SAVEFIELD2):
					result = command_field(cmd);
					break;
	
				case (SC_FRAMEFORCES):
				case (SC_MODELFORCES):
				case (SC_PRINTFORCES):
					result = command_forces(cmd);
					break;
	
				//case (SR_IMAGE):
				//	result = command_image(cmd);
				//	break;
	
				case (SC_MCACCEPT):
				case (SC_MCALLOW):
				case (SC_MCMAXSTEP):
				case (SC_MCNTRIALS):
				case (SC_PRINTMC):
					result = command_mc(cmd);
					break;

				case (SC_CONVERGE):
				case (SC_LINETOL):
				case (SC_MCMINIMISE):
				case (SC_SIMPLEXMINIMISE):
				case (SC_SDMINIMISE):
				case (SC_CGMINIMISE):
					result = command_minimise(cmd);
					break;

				case (SC_LISTMODELS):
				case (SC_LOADMODEL):
				case (SC_NEWMODEL):
				case (SC_PRINTMODEL):
				case (SC_SAVEMODEL):
				case (SC_SELECTMODEL):
					result = command_model(cmd);
					break;

				case (SC_ADDPATTERN):
				case (SC_CLEARPATTERNS):
				case (SC_CREATEPATTERNS):
				case (SC_PRINTPATTERNS):
				case (SC_SELECTPATTERN):
					result = command_pattern(cmd);
					break;

				case (SC_SELECTALL):
				case (SC_SELECTATOM):
				case (SC_SELECTELEMENT):
				case (SC_SELECTFFTYPE):
				case (SC_SELECTINVERT):
				case (SC_SELECTNONE):
				case (SC_SELECTTYPE):
					result = command_select(cmd);
					break;

				case (SC_ATOMDETAIL):
				case (SC_BONDDETAIL):
				case (SC_COLOUR):
				case (SC_DENSITYUNITS):
				case (SC_ENERGYUNITS):
				case (SC_GL):
				case (SC_KEY):
				case (SC_MOUSE):
				case (SC_MOVESTYLE):
				case (SC_RADIUS):
				case (SC_SHININESS):
				case (SC_SHOW):
				case (SC_STYLE):
					result = command_prefs(cmd);
					break;

				case (SC_ADDSITE):
				case (SC_PRINTSITES):
				case (SC_SELECTSITE):
				case (SC_SETAXES):
					result = command_site(cmd);
					break;
	
				case (SC_FIRSTFRAME):
				case (SC_LASTFRAME):
				case (SC_LOADTRAJECTORY):
				case (SC_NEXTFRAME):
				case (SC_PREVFRAME):
					result = command_traj(cmd);
					break;

				case (SC_TRANSLATEATOM):
				case (SC_TRANSLATESELECTION):
				case (SC_MIRRORSELECTION):
					result = command_transform(cmd);
					break;
	
				case (SC_QUIT):
					finished = TRUE;
					result = TRUE;
					msg(DM_NONE,"Script ended normally.\n");
					break;
				default:
					msg(DM_NONE,"script : Command '%s' hasn't been defined an action.\n", text_from_SC(cmd->get_command()));
					break;
			}
			// Step to next command
			cmd = cmd->next;
		}
		// If last command failed then quit...
		if (!result)
		{
			msg(DM_NONE,"Script ended on error.\n");
			break;
		}
		// If last command succeeded *and* finished == TRUE, stop and quit
		if (finished && result) break;
	}
	// If the last command succeeded *and* we're not finished (no 'quit'), show the GUI
	if (result && !finished) prefs.set_showgui(TRUE);
	// Tidy up here...
	dbg_end(DM_CALLS,"script::run");
}


/*
	*** Script model functions
	*** src/script/model.cpp

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
#include "base/master.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "file/filter.h"

// Model-related script commands (root=SR_MODEL)
bool script::command_model(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_model");
	bool result = TRUE;
	model *m;
	int n;
	filter *f;
	switch (cmd->get_command())
	{
		case (SC_NEWMODEL):	// Create new model ('newmodel <name>')
			m = master.add_model();
			m->set_name(cmd->datavar[0]->get_as_char());
			msg(DM_NONE,"script : Create model '%s'\n",m->get_name());
			break;
		case (SC_LOADMODEL):	// Load model ('loadmodel <name> <filename>')
			f = master.probe_file(cmd->datavar[1]->get_as_char(), FT_MODEL_IMPORT);
			if (f != NULL) m = f->import_model(cmd->datavar[1]->get_as_char());
			else
			{
				result = FALSE;
				break;
			}
			if (m != NULL)
			{
				m->set_name(cmd->datavar[0]->get_as_char());
				activeatom = m->get_atoms();
				msg(DM_NONE,"script : Model '%s' loaded, name '%s'\n",cmd->datavar[1]->get_as_char(),m->get_name());
			}
			else
			{
				msg(DM_NONE,"script : Model '%s' couldn't be loaded.'\n",cmd->datavar[1]->get_as_char());
				result = FALSE;
			}
			break;
		case (SC_SAVEMODEL):	// Save current model ('savemodel <format> <filename>')
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m != NULL)
			{
				// Find filter with a nickname matching that given in argc(0)
				for (f = master.filters[FT_MODEL_EXPORT].first(); f != NULL; f = f->next)
					if (strcmp(f->get_nickname(),cmd->datavar[0]->get_as_char()) == 0) break;
				// Check that a suitable format was found
				if (f == NULL)
				{
					msg(DM_NONE,"script : No model export filter was found that matches the nickname '%s'.\nNot saved.\n",cmd->datavar[0]->get_as_char());
					result = FALSE;
					break;
				}
				m->set_filter(f);
				m->set_filename(cmd->datavar[1]->get_as_char());
				f->export_model(m);
			}
			break;
		case (SC_PRINTMODEL):	// Print all information for model ('printmodel')
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m != NULL) m->print();
			break;
		case (SC_SELECTMODEL):	// Select working model ('selectmodel <name>')
			m = master.find_model(cmd->datavar[0]->get_as_char());
			if (m != NULL) 
			{
				master.set_currentmodel(m);
				//gui.select_model(m);
				activepattern = NULL;
				activeatom = m->get_atoms();
			}
			else result = FALSE;
			break;
		case (SC_LISTMODELS):	// Print loaded models ('listmodels')
			if (master.get_nmodels() != 0) msg(DM_NONE,"Name            NAtoms  Forcefield\n");
			for (m = master.get_models(); m != NULL; m = m->next)
				msg(DM_NONE,"%15s %5i  %15s\n",m->get_name(),m->get_natoms(),(m->get_ff() != NULL ? m->get_ff()->get_name() : "None"));
			break;
		default:	// Not recognised
			printf("Error - missed model command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_model");
	return result;
}


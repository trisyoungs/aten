/*
	*** Variable filter commands
	*** src/file/variables.cpp
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

#include "file/filter.h"
#include "base/master.h"
#include "base/sysfunc.h"
#include "base/elements.h"
#include "model/model.h"

// Variables commands
bool filter::do_variables(command_node<filter_command> *&fn)
{
	dbg_begin(DM_CALLS,"filter::do_variables");
	static vec3<double> vec1, vec2, vec3;
	static variable *v;
	static double q;
	static int atomid, ii, jj, n;
	static mat3<double> mat;
	static bond_type bt;
	static atom *i, *j, **modelatoms;
	static model *parent, *newmodel;
	bool result = FALSE;
	switch (fn->get_command())
	{
		// Create new model (and make it the target)
		case (FC_NEWMODEL):
			newmodel = master.add_model();
			newmodel->set_name(strip_trailing(fn->datavar[0]->get_as_char()));
			set_target(newmodel);
			result = TRUE;
			break;
		// Set data for atom 'n' in model
		case (FC_SETATOM):
			if (activemodel == NULL) break;
			// Must store 'n-1'th atom, since loop runs from 1 - natoms inclusive
			i = activemodel->get_staticatoms()[fn->datavar[0]->get_as_int() - 1];
			// Set variable values
			commands.variables.get_atom_variables(i);
			result = TRUE;
			break;
		// Create new atom in target model
		case (FC_ADDATOM):
			if (activemodel == NULL) break;
			// Set element
			v = commands.variables.find("e");
			if (v == NULL)
			{
				printf("'addatom' - No element 'e' variable set for new atom.\n");
				i = activemodel->add_atom(0);
			}
			else
			{
				if (has_zmapping) i = activemodel->add_atom(elements.find(v->get_as_char(),zmapping));
				else i = activemodel->add_atom(elements.find(v->get_as_char()));
				v->reset();
			}
			// Set variable values
			commands.variables.get_atom_variables(i);
			result = TRUE;
			break;
		// Create 'n' new atoms at once in model
		case (FC_CREATEATOMS):
			if (activemodel == NULL) break;
			for (atomid = 0; atomid < fn->datavar[0]->get_as_int(); atomid++) i = activemodel->add_atom(0);
			result = TRUE;
			break;
		// Use parent model as atom template
		case (FC_MODELTEMPLATE):
			if (activemodel == NULL) break;
			parent = activemodel->get_trajparent();
			if (parent == NULL)
			{
				printf("filter::do_variables <<<< SEVERE - No trajectory parent set for 'modeltemplate' >>>>\n");
				break;
			}
			// Create the atoms template
			for (i = parent->get_atoms(); i != NULL; i = i->next)
			{
				j = activemodel->add_atom(i->get_element());
				j->copy_style(i);
			}
			result = TRUE;
			break;
		// Set unit cell data (from lengths/angles)
		case (FC_SETCELL):
			if (activemodel == NULL) break;
			vec1.set(commands.variables.get_as_double("cell.a"), commands.variables.get_as_double("cell.b"), commands.variables.get_as_double("cell.c"));
			vec2.set(commands.variables.get_as_double("cell.alpha"), commands.variables.get_as_double("cell.beta"), commands.variables.get_as_double("cell.gamma"));
			activecell->set(vec1,vec2);
			activemodel->log_change(LOG_VISUAL);
			result = TRUE;
			break;
		// Set unit cell data (from lengths/angles)
		case (FC_SETCELLA):
			if (activemodel == NULL) break;
			mat.rows[0].set(commands.variables.get_as_double("cell.a.x"), commands.variables.get_as_double("cell.b.x"), commands.variables.get_as_double("cell.c.x"));
			mat.rows[1].set(commands.variables.get_as_double("cell.a.y"), commands.variables.get_as_double("cell.b.y"), commands.variables.get_as_double("cell.c.y"));
			mat.rows[2].set(commands.variables.get_as_double("cell.a.z"), commands.variables.get_as_double("cell.b.z"), commands.variables.get_as_double("cell.c.z"));
			activecell->set(mat);
			activemodel->log_change(LOG_VISUAL);
			result = TRUE;
			break;
		// Set spacegroup
		case (FC_SETSPACEGROUP):
			if (activemodel == NULL) break;
			activemodel->set_spacegroup(fn->datavar[0]->get_as_int());
			result = TRUE;
			break;
		// Config SETs
		case (FC_SETRX):		// Set rx of single atom in model
		case (FC_SETRY):		// Set ry of single atom in model
		case (FC_SETRZ):		// Set rz of single atom in model
		case (FC_SETFX):		// Set fx of single atom in model
		case (FC_SETFY):		// Set fy of single atom in model
		case (FC_SETFZ):		// Set fz of single atom in model
		case (FC_SETVX):		// Set vx of single atom in model
		case (FC_SETVY):		// Set vy of single atom in model
		case (FC_SETVZ):		// Set vz of single atom in model
			if (activemodel == NULL) break;
			// Check range of variable
			atomid = fn->datavar[0]->get_as_int() - 1;
			if ((atomid < 0) || (atomid >= activemodel->get_natoms()))
			{
				printf("filter::do_variables <<<< '%i' is out of range for model >>>>\n", atomid);
				break;
			}
			//printf("ATOMID = %i\n",atomid);
			modelatoms = activemodel->get_staticatoms();
			// Set the variable value
			switch (fn->get_command())
			{
				case (FC_SETRX):
					modelatoms[atomid]->r.set(0,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETRY):
					modelatoms[atomid]->r.set(1,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETRZ):
					modelatoms[atomid]->r.set(2,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETFX):
					modelatoms[atomid]->f.set(0,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETFY):
					modelatoms[atomid]->f.set(1,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETFZ):
					modelatoms[atomid]->f.set(2,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETVX):
					modelatoms[atomid]->v.set(0,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETVY):
					modelatoms[atomid]->v.set(1,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETVZ):
					modelatoms[atomid]->v.set(2,fn->datavar[1]->get_as_double());
					break;
			}
			result = TRUE;
			break;
		// Add bond between atoms
		case (FC_ADDBOND):
			if (activemodel == NULL) break;
			ii = fn->datavar[0]->get_as_int();
			jj = fn->datavar[1]->get_as_int();
			if (fn->datavar[2] == NULL) bt = BT_SINGLE;
			else
			{
				// Attempt to convert the datavar into a bond_type.
				// Try direct conversion from number (bond order) first
				// If that fails, try string conversion. Then, give up.
				n = fn->datavar[2]->get_as_int();
				if ((n < 1) || (n > 3))	bt = BT_from_text(fn->datavar[2]->get_as_char());
				else bt = (bond_type) n;
			}
			// Add the bond
			activemodel->bond_atoms(ii, jj, bt);
			result = TRUE;
			break;
		default:
			break;
	}
	if (result) fn = fn->next;
	dbg_end(DM_CALLS,"filter::do_variables");
	return result;
}


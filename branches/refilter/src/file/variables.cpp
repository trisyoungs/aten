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
	static grid *newgrid;
	bool result = FALSE;
	switch (fn->get_command())
	{




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
					modelatoms[atomid]->r().set(0,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETRY):
					modelatoms[atomid]->r().set(1,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETRZ):
					modelatoms[atomid]->r().set(2,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETFX):
					modelatoms[atomid]->f().set(0,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETFY):
					modelatoms[atomid]->f().set(1,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETFZ):
					modelatoms[atomid]->f().set(2,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETVX):
					modelatoms[atomid]->v().set(0,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETVY):
					modelatoms[atomid]->v().set(1,fn->datavar[1]->get_as_double());
					break;
				case (FC_SETVZ):
					modelatoms[atomid]->v().set(2,fn->datavar[1]->get_as_double());
					break;
			}
			result = TRUE;
			break;

		default:
			break;
	}
	if (result) fn = fn->next;
	dbg_end(DM_CALLS,"filter::do_variables");
	return result;
}


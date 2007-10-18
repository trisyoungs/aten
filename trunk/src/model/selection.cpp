/*
	*** Model selection functions
	*** src/model/selection.cpp

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

#include "model/model.h"

// Move atoms 'up'
void model::shift_selection_up()
{
	dbg_begin(DM_CALLS,"model::shift_selection_up");
	int tempid;
	atom *i, *j;
	// For each selected atom in the model, shift it one place back 'up' in the atom list
	i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected())
		{
			j = atoms.shift_up(i);
			if (j != NULL)
			{
				tempid = j->get_id();
				j->set_id(i->get_id());
				i->set_id(tempid);
				i = j;
			}
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::shift_selection_up");
}

// Move atoms 'down'
void model::shift_selection_down()
{
	dbg_begin(DM_CALLS,"model::shift_selection_down");
	int tempid;
	atom *i, *j;
	// For each selected atom in the model, shift it one place back 'up' in the atom list
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		if (i->is_selected())
		{
			j = atoms.shift_down(i);
			if (j != NULL)
			{
				tempid = j->get_id();
				j->set_id(i->get_id());
				i->set_id(tempid);
			}
		}
	}
	dbg_end(DM_CALLS,"model::shift_selection_down");
}

// Move atoms to start
void model::move_selection_to_start()
{
	dbg_begin(DM_CALLS,"model::move_selection_to_start");
	int tempid;
	atom *oldtail, *i;
	// For each selected atom in the model, shift it to the end of the list
	oldtail = atoms.last();
	i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) atoms.move_to_end(i);
		if (i == oldtail) break;
		i = i->next;
	}
	// Renumber atoms
	renumber_atoms();
	dbg_end(DM_CALLS,"model::move_selection_to_start");
}

// Move atoms to end
void model::move_selection_to_end()
{
	dbg_begin(DM_CALLS,"model::move_selection_to_end");
	int tempid;
	atom *oldhead, *i;
	// For each selected atom in the model, shift it to the end of the list
	oldhead = atoms.last();
	i = atoms.last();
	while (i != NULL)
	{
		if (i->is_selected()) atoms.move_to_end(i);
		if (i == oldhead) break;
		i = i->next;
	}
	// Renumber atoms
	renumber_atoms();
	dbg_end(DM_CALLS,"model::move_selection_to_end");
}

// Get selection cog
vec3<double> model::selection_get_cog()
{
        vec3<double> result;
        atom *i, *first;
        first = get_first_selected();
        if (first != NULL)
	{
		for (i = first; i != NULL; i = i->get_next_selected()) result += cell.mim(i,first);
	}
	result /= nselected;
        return result;
}

// Set selection visibility
void model::selection_set_hidden(bool hidden)
{
	for (atom *i = get_first_selected(); i != NULL; i = i->get_next_selected()) set_hidden(i, hidden);
	log_change(LOG_VISUAL);
}

// Fix selected atom positions
void model::selection_set_fixed()
{
	// Sets 'fixed' values to TRUE
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) i->fixed = TRUE;
		i = i->next;
	}
}

// Free selected atom positions
void model::selection_set_free()
{
	// Sets 'fixed' values to TRUE
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) i->fixed = FALSE;
		i = i->next;
	}
}

// Set selection style
void model::selection_set_style(draw_style ds)
{
	// Sets all atoms currently selected to have the drawing style specified
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) i->set_style(ds);
		i = i->next;
	}
	log_change(LOG_VISUAL);
}


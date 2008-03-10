/*
	*** Model select functions
	*** src/model/select.cpp
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

#include "model/model.h"
#include "classes/pattern.h"
#include "classes/atom.h"
#include "classes/bond.h"
#include "base/prefs.h"
#include "base/master.h"
#include "base/elements.h"
#include "gui/gui.h"

// Select Atom
void model::select_atom(atom *i)
{
	dbg_begin(DM_MORECALLS,"model::select_atom");
	if (!i->is_selected())
	{
		i->set_selected(TRUE);
		nselected ++;
		log_change(LOG_SELECTION);
		// Add the change to the undo state (if there is one)
		if (recordingstate != NULL)
		{
			change *newchange = recordingstate->changes.add();
			newchange->set(UE_SELECT,i->get_id());
		}
	}
	dbg_end(DM_MORECALLS,"model::select_atom");
}

// Deselect Atom
void model::deselect_atom(atom *i)
{
	dbg_begin(DM_MORECALLS,"model::deselect_atom");
	if (i->is_selected())
	{
		i->set_selected(FALSE);
		nselected --;
		log_change(LOG_SELECTION);
		// Add the change to the undo state (if there is one)
		if (recordingstate != NULL)
		{
			change *newchange = recordingstate->changes.add();
			newchange->set(-UE_SELECT,i->get_id());
		}
	}
	dbg_end(DM_MORECALLS,"model::deselect_atom");
}

// Toggle Selection State
void model::selection_toggle(atom *i)
{
	dbg_begin(DM_MORECALLS,"model::selection_toggle");
	i->is_selected() ? deselect_atom(i) : select_atom(i);
	dbg_end(DM_MORECALLS,"model::selection_toggle");
}

// Invert Current Selection
void model::selection_invert()
{
	dbg_begin(DM_CALLS,"model::selection_invert");
	for (atom *i = atoms.first(); i != NULL; i = i->next)
		i->is_selected() ? deselect_atom(i) : select_atom(i);
	dbg_end(DM_CALLS,"model::selection_invert");
}

// Delete Selected Atoms
void model::selection_delete()
{
	dbg_begin(DM_CALLS,"model::selection_delete");
	atom *i, *tempi;
	int count = 0;
	master.initialise_progress("Deleting atoms...", atoms.size());
	i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected())
		{
			tempi = i->next;
			delete_atom(i);
			i = tempi;
		}
		else i = i->next;
		if (!master.update_progress(++count)) break;
	}
	master.cancel_progress();
	dbg_end(DM_CALLS,"model::selection_delete");
}

// Expand Current Selection
void model::selection_expand()
{
	dbg_begin(DM_CALLS,"model::selection_expand");
	atom *i;
	refitem<bond,int> *bref;
	// Store the current selection state in i->tempi
	for (i = atoms.first(); i != NULL; i = i->next) i->tempi = i->is_selected();
	// Now use the temporary state to find atoms where we select atomic neighbours
	for (i = atoms.first(); i != NULL; i = i->next)
		if (i->tempi) for (bref = i->get_bonds(); bref != NULL; bref = bref->next) select_atom(bref->item->get_partner(i));
	dbg_end(DM_CALLS,"model::selection_expand");
}

// Select All Atoms
void model::select_all()
{
	dbg_begin(DM_CALLS,"model::select_all");
	for (atom *i = atoms.first(); i != NULL; i = i->next) if (!i->is_selected()) select_atom(i);
	dbg_end(DM_CALLS,"model::select_all");
}

// Deselect All Atoms
void model::select_none()
{
	dbg_begin(DM_CALLS,"model::select_none");
	for (atom *i = atoms.first(); i != NULL; i = i->next) if (i->is_selected()) deselect_atom(i);
	nselected = 0;
	dbg_end(DM_CALLS,"model::select_none");
}

// Atom at Screen Coordinates
atom *model::atom_on_screen(double x1, double y1)
{
	// See if an atom exists under the coordinates x1,y1
	// Ignore 'hidden' atoms.
	dbg_begin(DM_CALLS,"model::atom_on_screen");
	// Make sure we have a valid projection
	project_all();
	atom *closest = NULL;
	#ifdef HAS_GUI
		static vec3<double> wr, sr;
		static double closestz, dist, nclip;
		closestz = 10000.0;
		nclip = prefs.get_clip_near();
		atom *i = atoms.first();
		y1 = gui.mainview.get_height() - y1;
		while (i != NULL)
		{
			if (i->is_hidden()) { i = i->next; continue; }
			wr = -i->worldr();
			sr = i->screenr();
			if (wr.z > nclip)
			{
				dist = sqrt((sr.x - x1)*(sr.x - x1) + (sr.y - y1)*(sr.y - y1));
				if (dist < i->get_screen_radius())	// Mouse is inside bounding sphere
				{
					if ((closest == NULL) || (wr.z < closestz))
					{
						closest = i;
						closestz = wr.z;
					}
				}
			}
			i = i->next;
		}
	#endif
	dbg_end(DM_CALLS,"model::atom_on_screen");
	return closest;
}

// Select atoms within bounding box
void model::select_box(double x1, double y1, double x2, double y2)
{
	// Box selection - choose all the atoms within the selection area
	dbg_begin(DM_CALLS,"model::select_box");
	#ifdef HAS_GUI
		float t;
		atom *i, *closest;
		y1 = gui.mainview.get_height() - y1;
		y2 = gui.mainview.get_height() - y2;
		// Handle 'reverse ranges' - make sure x1 < x2 and y1 < y2
		if (x1 > x2) { t=x1; x1=x2; x2=t; }
		if (y1 > y2) { t=y1; y1=y2; y2=t; }
		for (atom *i = atoms.first(); i != NULL; i = i->next)
		{
			if (i->is_hidden()) { i = i->next; continue; }
			vec3<double> sr = i->screenr();
			if ((sr.x >= x1) && (sr.x <= x2))
				if ((sr.y >= y1) && (sr.y <= y2)) select_atom(i);
		}
	#endif
	dbg_end(DM_CALLS,"model::select_box");
}

// Tree Select
void model::select_tree(atom* i)
{
	// The passed atom node is the starting point for the algorithm.
	// From here, select all atoms that are bound - if they are already
	// selected then ignore them. If they are not already selected, then
	// recursively call the routine on that atom.
	dbg_begin(DM_CALLS,"model::select_tree");
	select_atom(i);
	refitem<bond,int> *bref = i->get_bonds();
	while (bref != NULL)
	{
		atom *j = bref->item->get_partner(i);
		if (!j->is_selected())
		{
			select_atom(j);
			this->select_tree(j);
		}
		bref = bref->next;
	}
	dbg_end(DM_CALLS,"model::select_tree");
}

// Select by Element
void model::select_element(atom *target)
{
	// Select all atoms which are the same element as the atom i
	dbg_begin(DM_CALLS,"model::select_element");
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->get_element() == target->get_element()) select_atom(i);
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::select_element");
}

// Select by element (from ID)
void model::select_element(int id)
{
	// Select all atoms which are the same element as the atom with id 'target'
	atom *i = get_atom(id);
	if (i != NULL) select_element(i);
}

// Select with bounding Sphere
void model::select_radial(atom *target, double radius)
{
	// Select all atoms which are within the distance 'radius' from atom 'target'
	dbg_begin(DM_CALLS,"model::select_radial");
	atom *i = atoms.first();
	printf("Selection radius is %8.4f Angstroms\n",radius);
	while (i != NULL)
	{
		if (i == target) select_atom(i);
		else if (distance(target,(atom*) i) < radius) select_atom(i);
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::select_radial");
}

// Select Pattern
void model::select_pattern(pattern *p)
{
	// Select all atoms covered by the specified pattern.
	dbg_begin(DM_CALLS,"model::select_pattern");
	atom *i = p->get_firstatom();
	for (int n=0; n<p->get_totalatoms(); n++)
	{
		select_atom(i);
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::select_pattern");
}

// Get first selected
atom *model::get_first_selected()
{
	dbg_begin(DM_CALLS,"model::get_first_selected");
	atom *result = NULL;
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected())
		{
			result = i;
			break;
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::get_first_selected");
	return result;
}

// Select overlapping atoms
void model::select_overlaps(double tolerance)
{
	dbg_begin(DM_CALLS,"model::select_overlaps");
	atom *i, *j;
	double deltar;
	select_none();
	for (i = atoms.first(); i != atoms.last(); i = i->next)
	{
		if (i->is_selected()) continue;
		for (j = i->next; j != NULL; j = j->next)
		{
			deltar = cell.distance(i, j);
			if (deltar < tolerance)
			{
				msg(DM_NONE,"Atom %i (%s) is %f from atom %i (%s).\n", j->get_id()+1, elements.symbol(j), deltar, i->get_id()+1, elements.symbol(i));
				select_atom(j);
			}
		}
	}
	dbg_end(DM_CALLS,"model::select_overlaps");
}

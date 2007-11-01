/*
	*** Model measurement functions
	*** src/model/measure.cpp
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

#include "model/model.h"
#include "classes/measurement.h"
#include "classes/bond.h"
#include <stdarg.h>

// Add distance measurement
void model::measure_distance(atom *i, atom *j)
{
	// Measure distances between atoms
	dbg_begin(DM_CALLS,"model::measure_distance");
	measurement *newdist = find_measurement(GT_DISTANCE,i,j);
	// If this distance isn't currently in the list, add it. Otherwise, delete it
	if (newdist == NULL) add_measurement(GT_DISTANCE,i,j);
	else remove_measurement(newdist);
	dbg_end(DM_CALLS,"model::measure_distance");
}

// Add angle measurement
void model::measure_angle(atom *i, atom *j, atom *k)
{
	// Measure angles between atoms
	dbg_begin(DM_CALLS,"model::measure_angle");
	measurement *newangle = find_measurement(GT_ANGLE,i,j,k);
	// Check that this angle isn't already in the list. If it is, delete it
	if (newangle == NULL) add_measurement(GT_ANGLE,i,j,k);
	else remove_measurement(newangle);
	dbg_end(DM_CALLS,"model::measure_angle");
}

// Add torsion measurement
void model::measure_torsion(atom *i, atom *j, atom *k, atom *l)
{
	// Measure torsions between atoms
	dbg_begin(DM_CALLS,"model::measure_torsion");
	measurement *newtorsion = find_measurement(GT_TORSION,i,j,k,l);
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newtorsion == NULL) add_measurement(GT_TORSION,i,j,k,l);
	else remove_measurement(newtorsion);
	dbg_end(DM_CALLS,"model::measure_torsion");
}

// Clear measurements of specific type
void model::remove_measurements(geom_type gt)
{
	dbg_begin(DM_CALLS,"model::remove_measurements");
	dbg_end(DM_CALLS,"model::remove_measurements");
}

// Delete measurements involving specific atom
void model::remove_measurements(atom *xatom)
{
	// Search the lists of measurements for the supplied atom, and remove any that use it
	dbg_begin(DM_CALLS,"model::remove_measurements[atom]");
	int n;
	bool remove;
	measurement *lastm, *m;
	lastm = NULL;
	static atom **atoms;
	m = measurements.first();
	while (m != NULL)
	{
		remove = FALSE;
		atoms = m->get_atoms();
		for (n=0; n<natoms_from_GT(m->get_type()); n++) if (atoms[n] == xatom) remove = TRUE;
		remove ? measurements.remove_and_get_next(m) : m = m->next;
	}
	dbg_end(DM_CALLS,"model::remove_measurements[atom]");
}

// Add Measurement
void model::add_measurement(geom_type gt, atom* first, ...)
{
	dbg_begin(DM_CALLS,"model::add_measurement");
	atom *i, **atoms;
	measurement *newm = measurements.add();
	newm->set_type(gt);
	newm->set_atom(0, first);
	// Get remaining atoms...
	int nexpected = natoms_from_GT(gt);
	va_list vars;
	va_start(vars,first);
	for (int n=1; n<nexpected; n++)
	{
		// Get argument from list and check it...
		i = va_arg(vars, atom*);
		newm->set_atom(n, i);
		if (i == NULL)
		{
			printf("model::add_measurement <<<< Not enough atoms supplied - needed %i, got %i >>>>\n",nexpected,n);
			measurements.remove(newm);
			newm = NULL;
			break;
		}
	}
	va_end(vars);
	// If we succeeded in adding all the atoms we required, set the value of the measurement
	if (newm != NULL) newm->calculate(&cell);
	dbg_end(DM_CALLS,"model::add_measurement");
}

// Add Measurement (reflist)
void model::add_measurement(geom_type gt, reflist<atom> &rl)
{
	dbg_begin(DM_CALLS,"model::add_measurement[reflist]");
	// Check number of atoms supplied
	if (rl.size() != natoms_from_GT(gt))
	{
		printf("add_measurement : Not enough atoms in supplied list to add measurement - needed %i, got %i.\n",natoms_from_GT(gt),rl.size());
		dbg_end(DM_CALLS,"model::add_measurement[reflist]");
		return;
	}
	refitem<atom> *ri = rl.first();
	// Pass to atom routine
	switch (gt)
	{
		case (GT_DISTANCE):
			add_measurement(gt,ri->item,ri->next->item);
			break;
		case (GT_ANGLE):
			add_measurement(gt,ri->item,ri->next->item,ri->next->next->item);
			break;
		case (GT_TORSION):
			add_measurement(gt,ri->item,ri->next->item,ri->next->next->item, ri->next->next->next->item);
			break;
	}
	dbg_end(DM_CALLS,"model::add_measurement[reflist]");
}

// Add measurements in selection
void model::add_measurements_in_selection(geom_type gt)
{
	dbg_begin(DM_CALLS,"model::add_measurements_in_selection");
	atom *i, *j, *k, *l;
	refitem<bond> *b1, *b2, *b3;
	switch (gt)
	{
		case (GT_DISTANCE):
			i = get_first_selected();
			while (i != NULL)
			{
				b1 = i->get_bonds();
				while (b1 != NULL)
				{
					j = b1->item->get_partner(i);
					if (j->is_selected())
						if (find_measurement(gt,i,j) == NULL)
							if (i->get_id() < j->get_id()) add_measurement(gt,i,j);
					b1 = b1->next;
				}
				i = i->get_next_selected();
			}
			break;
		case (GT_ANGLE):
			j = get_first_selected();
			while (j != NULL)
			{
				// Get bonds to this atom and loop over them again
				b1 = j->get_bonds();
				while (b1 != NULL)
				{
					i = b1->item->get_partner(j);
					if (i->is_selected())
					{
						b2 = b1->next;
						while (b2 != NULL)
						{
							k = b2->item->get_partner(j);
							if (k->is_selected() && (find_measurement(gt,i,j,k) == NULL)) add_measurement(gt,i,j,k);
							b2 = b2->next;
						}
					}
					b1 = b1->next;
				}
				j = j->get_next_selected();
			}
			break;
		case (GT_TORSION):
			// Find bond j-k where both are selected
			j = get_first_selected();
			while (j != NULL)
			{
				b1 = j->get_bonds();
				while (b1 != NULL)
				{
					k = b1->item->get_partner(j);
					if (k->is_selected() && (k > j))
					{
						// b1 = j-k. Loop over bonds on j (b2) and on k (b3) and find selected pairs
						b2 = j->get_bonds();
						while (b2 != NULL)
						{
							// Find selected atom i in torsion i-j-k-l
							i = b2->item->get_partner(j);
							if (i->is_selected() && (b2->item != b1->item))
							{
								// Find selected atom l in torsion i-j-k-l
								b3 = k->get_bonds();
								while (b3 != NULL)
								{
									l = b3->item->get_partner(k);
									if (l->is_selected() && (b3->item != b1->item))
									{
										// Found four selected atoms forming a torsion
										if (find_measurement(gt,i,j,k,l) == NULL) add_measurement(gt,i,j,k,l);
									}
									b3 = b3->next;
								}
							}
							b2 = b2->next;
						}
					}
					b1 = b1->next;
				}
				j = j->get_next_selected();
			}
			break;
	}
	dbg_end(DM_CALLS,"model::add_measurements_in_selection");
}

// Find Measurement
measurement *model::find_measurement(geom_type gt, atom* first, ...)
{
	dbg_begin(DM_CALLS,"model::find_measurement");
	measurement *result, *m;
	int n, matched1, matched2;
	bool proceed;
	atom *searchatoms[4], **atoms;
	searchatoms[0] = first;
	int nexpected = natoms_from_GT(gt);
	va_list vars;
	va_start(vars,first);
	proceed = TRUE;
	for (n=1; n<nexpected; n++)
	{
		// Get argument from list and check it...
		searchatoms[n] = va_arg(vars,atom*);
		if (searchatoms[n] == NULL)
		{
			printf("find_measurement : Not enough atoms supplied for measurement - needed %i, got %i.\n",nexpected,n);
			proceed = FALSE;
			break;
		}
	}
	va_end(vars);
	// Continue to search for measurement in list if we have enough atoms
	result = NULL;
	if (proceed)
	{
		for (m = measurements.first(); m != NULL; m = m->next)
		{
			if (gt == m->get_type())
			{
				atoms = m->get_atoms();
				// Check atoms supplied (forward and back)
				matched1 = 0;
				matched2 = 0;
				for (n=0; n<nexpected; n++)
				{
					if (searchatoms[n] == atoms[n]) matched1 ++;
					if (searchatoms[nexpected-n] == atoms[n]) matched2 ++;
				}
				if ((matched1 == nexpected) || (matched2 == nexpected))
				{
					result = m;
					break;
				}
			}
			if (result != NULL) break;
		}
	}
	dbg_end(DM_CALLS,"model::find_measurement");
	return result;
}

// Find Measurement (reflist)
measurement *model::find_measurement(geom_type gt, reflist<atom> &rl)
{
	dbg_begin(DM_CALLS,"model::find_measurement[reflist]");
	measurement *result;
	// Check number of atoms supplied
	if (rl.size() != natoms_from_GT(gt))
	{
		printf("find_measurement : Not enough atoms in supplied list to add measurement - needed %i, got %i.\n",natoms_from_GT(gt),rl.size());
		dbg_end(DM_CALLS,"model::find_measurement[reflist]");
		return NULL;
	}
	refitem<atom> *ri = rl.first();
	// Pass to atom routine
	switch (gt)
	{
		case (GT_DISTANCE):
			result = find_measurement(gt,ri->item,ri->next->item);
			break;
		case (GT_ANGLE):
			result = find_measurement(gt,ri->item,ri->next->item,ri->next->next->item);
			break;
		case (GT_TORSION):
			result = find_measurement(gt,ri->item,ri->next->item,ri->next->next->item, ri->next->next->next->item);
			break;
	}
	dbg_end(DM_CALLS,"model::find_measurement[reflist]");
	return result;
}

// Calculate angle
double model::calculate_angle(int i, int j, int k)
{
	// Make sure we have a staticatoms array
	atom **temp = get_staticatoms();
	return cell.angle(temp[i], temp[j], temp[k]);
}

// Calculate angle
double model::calculate_torsion(int i, int j, int k, int l)
{
	// Make sure we have a staticatoms array
	atom **temp = get_staticatoms();
	return cell.torsion(staticatoms[i], staticatoms[j], staticatoms[k], staticatoms[l]);
}

// Update measurements
void model::update_measurements()
{
	dbg_begin(DM_CALLS,"model::update_measurements");
	for (measurement *m = measurements.first(); m != NULL; m = m->next) m->calculate(&cell);
	dbg_end(DM_CALLS,"model::update_measurements");
}

/*
	*** Model cell functions
	*** src/model/cell.cpp

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
#include "templates/vector3.h"
#include "templates/matrix3.h"
#include "classes/spacegroup.h"
#include "base/master.h"
#include "base/constants.h"
#include "file/parse.h"
#include <math.h>
#include <iostream>

// Fold All Atoms
void model::fold_all_atoms()
{
	dbg_begin(DM_CALLS,"model::fold_all_atoms");
	// Standard fold - individual atoms
	for (atom *i = atoms.first(); i != NULL; i = i->next) cell.fold(i);
	log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"model::fold_all_atoms");
}

// Fold All Molecules
void model::fold_all_molecules()
{
	dbg_begin(DM_CALLS,"model::fold_all_molecules");
	int n,m;
	atom *i, *first;
	pattern *p;
	// Molecular fold - fold first atom, other in molecule are MIM'd to this point
	if (!autocreate_patterns())
	{
		msg(DM_NONE,"model::fold_all_molecules : Molecular fold cannot be performed without a valid pattern definition.\n");
		dbg_end(DM_CALLS,"model::fold_all_molecules");
		return;
	}
	p = patterns.first();
	i = atoms.first();
	while (p != NULL)
	{
		for (m=0; m<p->get_nmols(); m++)
		{
			for (n=0; n<p->get_natoms(); n++)
			{
				// If its the first atom, fold and store pointer. If not, MIM w.r.t. stored atom
				if (n == 0)
				{
					cell.fold(i);
					first = i;
				}
				else i->r = cell.mim(i,first);
				i = i->next;
			}
		}
		p = p->next;
	}
	log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"model::fold_all_molecules");
}

// Apply symmetry operator
void model::apply_symmop(symmop* so, atom *lastatom)
{
	dbg_begin(DM_CALLS,"model::apply_symmop");
	if (so->is_identity())
	{
		// Ignore this operator since it is the identity
		dbg_begin(DM_CALLS,"model::apply_symmop");
		return;
	}
	mat3<double> rotmat = so->get_rotation();
	vec3<double> trans = so->get_translation(), newr;
	atom *i, *newatom;
	i = atoms.first();
	while (i != NULL)
	{
		// Add a new atom and get the position of the old atom
		newatom = add_atom(i->get_element());
		newr = i->r;
		// Apply the rotation and translation
		newr *= rotmat;
		newr -= trans;
		newatom->r = newr;
		if (i == lastatom) break;
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::apply_symmop");
}

// Apply model's symmetry operators
void model::apply_model_symmops(atom *lastatom)
{
	dbg_begin(DM_CALLS,"model::apply_model_symmops");
	symmop *so;
	if (lastatom == NULL) lastatom = atoms.last();
	so = symmops.first();
	while (so != NULL)
	{
		apply_symmop(so,lastatom);
		so = so->next;
	}
	dbg_end(DM_CALLS,"model::apply_model_symmops");
}

// Apply model spacegroup symmetry operators
void model::apply_spacegroup_symmops(atom *lastatom)
{
	dbg_begin(DM_CALLS,"model::apply_spacegroup_symmops");
	symmop *so;
	if (lastatom == NULL) lastatom = atoms.last();
	so = spacegroups.get_symmops(spgrp);
	while (so != NULL)
	{
		apply_symmop(so,lastatom);
		so = so->next;
	}
	dbg_end(DM_CALLS,"model::apply_spacegroup_symmops");
}

// Scale cell and contents (molecule COGs)
void model::scale_cell(const vec3<double> &scale)
{
	dbg_begin(DM_CALLS,"model::scale_cell");
	vec3<double> oldcog, newcog, newpos;
	unitcell newcell;
	mat3<double> newaxes;
	bool calcenergy;
	double olde, newe;
	int n,m;
	atom *i;
	// First, make sure we have a cell and a valid pattern
	if (cell.get_type() == CT_NONE)
	{
		msg(DM_NONE,"No cell to scale.\n");
		dbg_end(DM_CALLS,"model::scale_cell");
		return;
	}
	if (!autocreate_patterns())
	{
		dbg_end(DM_CALLS,"model::scale_cell");
		return;
	}
	calcenergy = create_expression();
	// Copy original cell, expand and save for later
	newaxes = cell.get_axes();
	newaxes.row_multiply(scale);
	newcell.set(newaxes.transpose());
	// We need a working configuration (for COG calculations)
	fold_all_atoms();
	if (calcenergy) olde = total_energy(this);
	// Cycle over patterns, get COG, convert to old fractional coordinates, then
	// use new cell to get new local coordinates.
	for (pattern *p = patterns.first(); p != NULL; p = p->next)
	{
		i = p->get_firstatom();
		for (n=0; n<p->get_nmols(); n++)
		{
			// Get fractional coordinate COG of this molecule
			oldcog = p->calculate_cog(this,n);
			// Get new COG using new cell
			newcog = newcell.frac_to_real(cell.real_to_frac(oldcog));
			// Set new atom positions
			for (m=0; m<p->get_natoms(); m++)
			{
				newpos = cell.mim(i,oldcog) - oldcog + newcog;
				i->r = newpos;
				i = i->next;
			}
		}
	}
	// Calculate new energy before leaving...
	if (calcenergy)
	{
		newe = total_energy(this);
		msg(DM_NONE,"Energy change was %12.7e %s\n", newe-olde, text_from_EU(prefs.get_internal_units()));
	}
	// Set new cell and update model
	cell.set(newaxes);
	log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"model::scale_cell");
}

// Replicate Cell
void model::replicate_cell(const vec3<double> &neg, const vec3<double> &pos)
{
	dbg_begin(DM_CALLS,"model::cell_replicate");
	int count;
	bool stop;
	vec3<double> tvec;
	mat3<double> newaxes, oldaxes;

	// If this isn't a periodic model, exit
	if (cell.get_type() == CT_NONE)
	{
		msg(DM_NONE,"No cell to replicate.\n");
		dbg_end(DM_CALLS,"model::cell_replicate");
		return;
	}

	// Create a temporary clipboard and copy the original model to it
	clipboard originalclip;
	originalclip.copy_all(this);
	// Centre this clipboard copy to put the atoms at 0,0,0
	originalclip.translate(-cell.get_origin());

	// Create the new unit cell in the original model
	oldaxes = cell.get_axes();
	// Copy model to clipboard ready for pasting
	master.privclip.copy_all(this);
	// Take transpose of old and new axes for convenient multiplication
	newaxes = oldaxes;

	// Set new unit cell dimensions
	tvec.set(pos.x+1.0-neg.x, pos.y+1.0-neg.y, pos.z+1.0-neg.z);
	newaxes.row_multiply(tvec);
	// Un-transpose and set new axes
	newaxes = newaxes.transpose();
	cell.set(newaxes);

	// Clear the original model
	this->clear();

	// Re-centre the clipboard copy so it is at the new cell origin
	originalclip.translate(cell.get_origin());

	// Paste in whole copies of the original cell - don't worry about fractional cells yet
	vec3<int> ineg, ipos;
	int ii, jj, kk;
	ineg.set(int(floor(neg.x)), int(floor(neg.y)), int(floor(neg.z)));
	ipos.set(int(ceil(pos.x)), int(ceil(pos.y)), int(ceil(pos.z)));

	// Set up progress indicator
	count = ( (ipos.x - ineg.x) + 1) * ( (ipos.y - ineg.y) + 1) * ( (ipos.z - ineg.z) + 1);
	master.initialise_progress("Creating cell copies...", count);

	// Create cell copies
	count = 0;
	stop = FALSE;
	for (ii = 0; ii <= (ipos.x - ineg.x); ii++)
	{
		for (jj = 0; jj <= (ipos.y - ineg.y); jj++)
		{
			for (kk = 0; kk <= (ipos.z - ineg.z); kk++)
			{
				// Set base translation vector for this replication
				tvec = oldaxes.rows[0] * ii;
				tvec += oldaxes.rows[1] * jj;
				tvec += oldaxes.rows[2] * kk;
				master.privclip.paste_to_model(this,tvec);
				msg(DM_VERBOSE,"Created copy for vector %8.4f %8.4f %8.4f\n",tvec.x,tvec.y,tvec.z);
				if (!master.update_progress(++count))
				{
					stop = TRUE;
					break;
				}
			}
			if (stop) break;
		}
		if (stop) break;
	}
	master.cancel_progress();

	// Deselect all atoms
	select_none();

	// Now trim off atoms that are outside the new cell
	bool delatom;
	atom *i, *j;
	vec3<double> fracr;
	mat3<double> cellinverse = cell.get_inverse();

	master.initialise_progress("Trimming excess atoms...", atoms.size());
	i = atoms.first();
	count = 0;
	while (i != NULL)
	{
		delatom = FALSE;
		// Convert coordinates to fractional coords and test them
		fracr = cellinverse * i->r;
		if ((fracr.x < 0) || (fracr.x >= 1)) delatom = TRUE;
		else if ((fracr.y < 0) || (fracr.y >= 1)) delatom = TRUE;
		else if ((fracr.z < 0) || (fracr.z >= 1)) delatom = TRUE;
		if (delatom)
		{
			j = i->next;
			delete_atom(i);
			i = j;
		}
		else i = i->next;
		if (!master.update_progress(++count)) break;
	}
	master.cancel_progress();

	log_change(LOG_STRUCTURE);
	dbg_end(DM_CALLS,"model::cell_replicate");
}

// Frac to Real
void model::frac_coords_to_real()
{
	dbg_begin(DM_CALLS,"model::frac_coords_to_real");
	for (atom *i = atoms.first(); i != NULL; i = i->next) i->r = cell.frac_to_real(i->r);
	dbg_end(DM_CALLS,"model::frac_coords_to_real");
}

/*
	*** Model functions
	*** src/model/model.cpp
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
#include "base/master.h"
#include "base/elements.h"

// Constructors
model::model()
{
	next = NULL;
	prev = NULL;
	nselected = 0;
	camr.set(0.0,0.0,-10.0);
	projection_point = -1;
	camrot = 0.0;
	ortho_size = 20.0;
	for (int n=0; n<LOG_NITEMS; n++) logs[n] = 0;
	spgrp = 0;
	spgrpsetting = 1;
	mass = 0.0;
	density = 0.0;
	staticatoms = NULL;
	staticatoms_point = -1;
	ff = NULL;
	plist = NULL;
	save_point = 0;
	patterns_point = -1;
	expression_point = -1;
	constraints = NULL; 
	constraints_tail = NULL; 
	nconstraints = 0;
	filefilter = NULL;
	char newname[32];
	sprintf(newname,"Unnamed%03i",master.get_modelid());
	name = newname;
	lastatomdrawn = NULL;
	trajparent = NULL;
	trajfilefilter = NULL;
	trajfile = NULL;
	trajposfirst = 0;
	trajposlast = 0;
	framesize = 0;
	ncachedframes = 0;
	totalframes = 0;
	render_source = this;
	trajcached = FALSE;
	frameposition = 0;
	trajplaying = FALSE;
	currentframe = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_MODEL] ++;
	#endif
}

// Destructor
model::~model()
{
	atoms.clear();
	patterns.clear();
	measurements.clear();
	if (plist != NULL) delete[] plist;
	if (staticatoms != NULL) delete[] staticatoms;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_MODEL] ++;
	#endif
}

// Log change
void model::log_change(change_log cl)
{
	if (cl >= LOG_TOTAL) printf("Invalid log quantity passed.\n");
	logs[cl] ++;
	// For all logs except LOG_CAMERA we also update the total log
	if (cl != LOG_CAMERA) logs[LOG_TOTAL] ++;
}

// Clear
void model::clear()
{
	clear_atoms();
	patterns.clear();
	frames.clear();
	// Reset logs and log points
	for (int n=0; n<LOG_NITEMS; n++) logs[n] = 0;
	patterns_point = -1;
	expression_point = -1;
	projection_point = -1;
	staticatoms_point = -1;
}

// Calculate mass
void model::calculate_mass()
{
	// Calculate the mass of the atoms in the model.
	dbg_begin(DM_CALLS,"model::calculate_mass");
	mass = 0.0;
	atom *i = atoms.first();
	while (i != NULL)
	{
		mass += elements.mass(i);
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::calculate_mass");
}

/*
// Forcefields
*/

// Assign charges from forcefield
void model::assign_charges(charge_source qs)
{
	// Assign atom-type charges from the currently associated forcefield to the model
	// Perform forcefield typing if necessary
	dbg_begin(DM_CALLS,"model::assign_charges");
	pattern *p;
	atom *i;
	forcefield *xff, *patff;
	switch (qs)
	{
		case (QS_MODEL):
			break;
		case (QS_FF):
			if (!patterns_are_valid())
			{
				msg(DM_NONE,"model::assign_charges - Cannot assign atomic charges without a valid pattern setup.\n");
				break;
			}
			type_all();
			p = patterns.first();
			while (p != NULL)
			{
				// Grab current model (global) forcefield
				xff = ff;	
				patff = p->get_ff();
				// Grab pattern forcefield in preference to model's
				if (patff != NULL) xff = patff;
				if (xff == NULL)
					msg(DM_NONE,"assign_charges : No forcefield is currently assigned to pattern %s. No charges assigned.\n",p->get_name());
				else
				{
					i = p->get_firstatom();
					int ptotalatoms = p->get_totalatoms();
					int count = 0;
					while (count < ptotalatoms)
					{
						i->set_charge(i->get_fftype()->get_charge());
						i = i->next;
						count ++;
					}
					// Charge atoms in representative pattern molecule
					for (i = p->molecule.get_atoms(); i != NULL; i = i->next)
						i->set_charge(i->get_fftype()->get_charge());
				}
				p = p->next;
			}
			break;
		case (QS_GASTEIGER):
		case (QS_QEQ):
			printf("Gasteiger and QEq charges are not currently implemented.\n");
			break;
	}
	dbg_end(DM_CALLS,"model::assign_charges");
}

// Set model's forcefield
void model::set_ff(forcefield *newff)
{
	// Change the associated forcefield of the model to 'newff'
	if (ff != newff)
	{
		invalidate_expression();
		ff = newff;
		msg(DM_NONE,"Forcefield '%s' now associated with model '%s'.\n",ff->get_name(),name.get());
	}
}

// Remove typing from the model
void model::remove_typing()
{
	// Remove all atom typing from the current model
	dbg_begin(DM_CALLS,"model::remove_typing");
	atom *i = atoms.first();
	while (i != NULL)
	{
		i->set_fftype(NULL);
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::remove_typing");
}

/*
// Labelling
*/

// Clear atom labelling
void model::clear_atom_labels()
{
	atom *i = atoms.first();
	while (i != NULL)
	{
		i->clear_labels();
		i = i->next;
	}
}

// Clear all labels in selection
void model::selection_clear_atom_labels()
{
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) i->clear_labels();
		i = i->next;
	}
}

// Clear specific labels in selection
void model::selection_clear_atom_labels(atom_label al)
{
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) i->remove_label(al);
		i = i->next;
	}
}

// Add atom labels
void model::set_atom_labels(atom_label al)
{
	atom *i = atoms.first();
	while (i != NULL)
	{
		i->add_label(al);
		i = i->next;
	}
}

// Add labels to selected atoms
void model::selection_set_atom_labels(atom_label al)
{
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) i->add_label(al);
		i = i->next;
	}
}

/*
// OTHER STUFF
*/

void model::print_coords()
{
	dbg_begin(DM_CALLS,"model::print_coords");
	for (atom *i = atoms.first(); i != NULL; i = i->next)
	{
		printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f\n",i->get_id(),elements.symbol(i),i->r.x,i->r.y,i->r.z,i->get_charge());
	//	printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f  %s\n",i->get_id(),elements.symbol(i),r.x,r.y,r.z,
	//		i->get_charge(),(ff == NULL ? " " : ff->name(i)));
	}
	dbg_end(DM_CALLS,"model::print_coords");
}

// Centre system at 0,0,0
void model::centre()
{
	dbg_begin(DM_CALLS,"model::centre");
	vec3<double> cog;
	atom *i;
	for (i = atoms.first(); i != NULL; i = i->next) cog += i->r;
	// Get the centre of geometry and adjust atomic coordinates...
	cog /= -atoms.size();
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		i->r += cog;
		// BEGIN HACK TODO Remove!
		if (i->get_element() > 118) i->v += cog;
		// END HACK
	}
	dbg_end(DM_CALLS,"model::centre");
}

// Calculate the density of the system (if periodic)
void model::calculate_density()
{
	dbg_begin(DM_CALLS,"model::calculate_density");
	double v = 0.0;
	if (cell.get_type() != CT_NONE)
	{
		// Calculate density in the units specified by prefs.density_internal
		switch (prefs.get_density_units())
		{
			case (DU_GPERCM):
				density = (mass / AVOGADRO) / (cell.get_volume() / 1.0E24);
				break;
			case (DU_ATOMSPERANG):
				density = atoms.size() / cell.get_volume();
				break;
		}
	}
	else density = -1.0;
	dbg_end(DM_CALLS,"model::calculate_density");
}

// Bohr to Angstrom
void model::bohr_to_angstrom()
{
	// Convert coordinates and cell from Bohr to Angstrom
	dbg_begin(DM_CALLS,"model::bohr_to_angstrom");
	// Coordinates
	for (atom *i = atoms.first(); i != NULL; i = i->next) i->r = i->r * 0.529;
	// Cell
	cell_type ct = cell.get_type();
	if (ct != CT_NONE)
	{
		vec3<double> lengths = cell.get_lengths();
		lengths *= 0.529;
		cell.set(lengths,cell.get_angles());
	}
	log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"model::bohr_to_angstrom");
}

// Reset atom tempi's
void model::reset_tempi(int value)
{
	dbg_begin(DM_CALLS,"model::reset_tempi");
	atom *i = atoms.first();
	while (i != NULL)
	{
		i->tempi = value;
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::reset_tempi");
}

// Clear charges
void model::clear_charges()
{
	dbg_begin(DM_CALLS,"model::clear_charges");
	for (atom *i = atoms.first(); i != NULL; i = i->next) i->set_charge(0.0);
	dbg_end(DM_CALLS,"model::clear_charges");
}

// Print
void model::print()
{
	dbg_begin(DM_CALLS,"model::print");
	msg(DM_NONE,"   Name : %s\n",name.get());
	msg(DM_NONE,"   File : %s\n",filename.get());
	msg(DM_NONE,"   Mass : %f\n",mass);
	if (cell.get_type() != CT_NONE) msg(DM_NONE,"   Cell : %s\nDensity : %f %s\n",text_from_CT(cell.get_type()),density,text_from_DU(prefs.get_density_units()));
	msg(DM_NONE,"  Atoms : %i\n",atoms.size());
	msg(DM_NONE," Id     El   FFType         X             Y             Z              Q        S  \n");
	// Print from pattern definition if possible, otherwise just use model atom list
	atom *i;
	int n;
	if (patterns.size() != 0)
		for (pattern *p = patterns.first(); p != NULL; p = p->next)
		{
			i = p->get_firstatom();
			for (n=0; n<p->get_totalatoms(); n++)
			{
				i->print_summary();
				i = i->next;
			}
		}
	else for (i = atoms.first(); i != NULL; i = i->next) i->print_summary();
	dbg_end(DM_CALLS,"model::print");
}

// Print Forces
void model::print_forces()
{
	for (atom *i = atoms.first(); i != NULL; i = i->next)
	{
		printf("%4i %3s  %14.6e  %14.6e  %14.6e\n", i->get_id(), elements.symbol(i), i->f.x, i->f.y, i->f.z);
	}
}

// Copy model
void model::copy(model *srcmodel)
{
	// Clear any current contents of the model
	clear();
	// Copy all atoms with privclip
	master.privclip.copy_all(srcmodel);
	master.privclip.paste_to_model(this);
}

// Copy atom data from specified model
void model::copy_atom_data(model *srcmodel, int dat)
{
	dbg_begin(DM_CALLS,"model::copy_atom_data");
	// Simple failsafe - check atom numbers in each are the same
	if (atoms.size() != srcmodel->atoms.size())
	{
		printf("model::copy_atom_data <<<< Models have different numbers of atoms (%i/%i) >>>>\n", atoms.size(), srcmodel->atoms.size());
		dbg_end(DM_CALLS,"model::copy_atom_data");
		return;
	}
	atom *i, *j;
	j = srcmodel->atoms.first();
	for (i = atoms.first(); i != NULL; i = i->next)
	{
		// Copy data items referenced in 'dat'
		if ((dat&AD_R) || (dat == AD_ALL)) i->r = j->r;
		if ((dat&AD_F) || (dat == AD_ALL)) i->f = j->f;
		if ((dat&AD_V) || (dat == AD_ALL)) i->v = j->v;
		if ((dat&AD_Z) || (dat == AD_ALL)) i->set_element(j->get_element());
		if ((dat&AD_Q) || (dat == AD_ALL)) i->set_charge(j->get_charge());
		if ((dat&AD_FIXFREE) || (dat == AD_ALL)) i->fixed = j->fixed;
		j = j->next;
	}
	//msg(DM_VERBOSE,"Copied data for %i atoms from model '%s' to model '%s'.\n", count);
// get_name(), srcmodel->get_name());
	dbg_end(DM_CALLS,"model::copy_atom_data");
}

// Copy range of atom data from specified model
void model::copy_atom_data(model *srcmodel, int dat, int startatom, int ncopy)
{
	dbg_begin(DM_CALLS,"model::copy_atom_data[range]");
	// Simple failsafe - check atom numbers in each are the same
	int numatoms = atoms.size();
	if (numatoms != srcmodel->atoms.size())
	{
		printf("model::copy_atom_data[range] <<<< Models have different numbers of atoms (%i/%i) >>>>\n", numatoms, srcmodel->atoms.size());
		dbg_end(DM_CALLS,"model::copy_atom_data[range]");
		return;
	}
	// Check limits of requested copy
	int finishatom = startatom + ncopy;
	if (ncopy > 0) 
	{
		if (startatom >= numatoms) printf("model::copy_atom_data[range] <<<< Start atom (%i) is past end of model contents >>>>\n",startatom);
		else if (finishatom > numatoms) printf("model::copy_atom_data[range] <<<< End atom too high (%i c.f. N=%i) >>>>\n",finishatom,numatoms);
		else
		{
			// Get staticatoms arrays from both models
			atom **ii = get_staticatoms();
			atom **jj = srcmodel->get_staticatoms();
			for (int n=startatom; n<finishatom; n++)
			{
				// Copy data items referenced in 'dat'
				if ((dat&AD_R) || (dat == AD_ALL)) ii[n]->r = jj[n]->r;
				if ((dat&AD_F) || (dat == AD_ALL)) ii[n]->f = jj[n]->f;
				if ((dat&AD_V) || (dat == AD_ALL)) ii[n]->v = jj[n]->v;
				if ((dat&AD_Z) || (dat == AD_ALL)) ii[n]->set_element(jj[n]->get_element());
				if ((dat&AD_Q) || (dat == AD_ALL)) ii[n]->set_charge(jj[n]->get_charge());
				if ((dat&AD_FIXFREE) || (dat == AD_ALL)) ii[n]->fixed = jj[n]->fixed;
			}
			msg(DM_VERBOSE,"Copied data for %i atoms starting at %i from model '%s' to model '%s'.\n", ncopy, startatom, name.get(), srcmodel->name.get());
		}
	}
	dbg_end(DM_CALLS,"model::copy_atom_data[range]");
}

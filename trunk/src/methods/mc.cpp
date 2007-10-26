/*
	*** Monte Carlo methods
	*** src/methods/mc.cpp
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

#include "classes/pattern.h"
#include "model/model.h"
#include "methods/mc.h"
#include "base/master.h"
#include "base/sysfunc.h"
#include "base/mathfunc.h"
#include "base/prefs.h"
#include "gui/gui.h"

mc_methods mc;

// Monte Carlo move types
const char *MT_strings[MT_NITEMS] = { "Translation", "Rotation", "Z-Matrix", "Insertion", "Deletion" };
const char *MT_keywords[MT_NITEMS] = { "translate", "rotate", "zmatrix", "insert", "delete" };
const char *text_from_MT(mc_move i)
	{ return MT_strings[i]; };
mc_move MT_from_text(const char *s)
	{ return (mc_move) enum_search("Monte Carlo move",MT_NITEMS,MT_keywords,s); }

// Constructors
mc_methods::mc_methods()
{
	// Set initial values
	maxstep[MT_TRANSLATE] = 5.0;
	maxstep[MT_ROTATE] = 20.0;
	maxstep[MT_ZMATRIX] = 0.0;
	ntrials[MT_TRANSLATE] = 10;
	ntrials[MT_ROTATE] = 10; 
	ntrials[MT_ZMATRIX] = 0;
	ntrials[MT_INSERT] = 10;
	ntrials[MT_DELETE] = 0; 
	allowed[MT_TRANSLATE] = TRUE;
	allowed[MT_ROTATE] = TRUE;
	allowed[MT_ZMATRIX] = FALSE;
	allowed[MT_INSERT] = TRUE;
	allowed[MT_DELETE] = TRUE; 
	eaccept[MT_TRANSLATE] = 0.0;
	eaccept[MT_ROTATE] = 0.0;
	eaccept[MT_ZMATRIX] = 0.0;
	eaccept[MT_INSERT] = 500.0;
	eaccept[MT_DELETE] = 0.0;
	acceptratio = NULL;
	acceptratio_size = 0;
	vdw_radius_scale = 1.0;
	ncycles = 100;
}

component::component()
{
	// Set initial values for component
	compmodel = NULL;
	comppattern = NULL;
	nrequested = 0;
	nfilled = 0;
	prev = NULL;
	next = NULL;
	allowed_moves[MT_INSERT] = TRUE;
	allowed_moves[MT_DELETE] = FALSE;
	allowed_moves[MT_TRANSLATE] = TRUE;
	allowed_moves[MT_ROTATE] = TRUE;
	allowed_moves[MT_ZMATRIX] = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_COMPONENT] ++;
	#endif
}

// Destructor
component::~component()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_COMPONENT] ++;
	#endif
}

// Create ratio acceptance array
void mc_methods::create_ratioarray(int newsize)
{
	int n, m;
	if (acceptratio != NULL)
	{
		for (n=0; n<acceptratio_size; n++) delete[] acceptratio[n];
		delete[] acceptratio;
	}
	acceptratio = new double*[newsize];
	for (n=0; n<newsize; n++) acceptratio[n] = new double[MT_NITEMS];
	acceptratio_size = newsize;
	// Zero the elements
	for (n=0; n<newsize; n++)
		for (m=0; m<MT_NITEMS; m++) acceptratio[n][m] = 0.0;
}

/*
// Component management routines
*/

// Find by ID
component *mc_methods::get_component_by_name(const char *search)
{
	for (component *c = components.first(); c != NULL; c = c->next)
		if (strcmp(c->get_model()->get_name(),search) == 0) return c;
	return NULL;
}

// MC Geometry Minimise
bool mc_methods::minimise(model* srcmodel, double econ, double fcon)
{
	// Monte Carlo energy minimisation.
	// Validity of forcefield and energy setup must be performed before calling and is *not* checked here.
	dbg_begin(DM_CALLS,"mc::minimise");
	int n, cycle, nmoves, move, randmol, randpat, npats;
	double enew, ecurrent, ecurrent_vdw, ecurrent_elec, elastcycle, edelta, phi, theta;
	vec3<double> v;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg(DM_NONE,"Creating expression for target model...\n");
        if (!srcmodel->create_expression())
	{
		dbg_end(DM_CALLS,"mc::minimise");
		return FALSE;
	}
	srcmodel->assign_charges(prefs.get_chargesource());

	// Create coordinate backup model for minimisation
	model bakmodel;
	bakmodel.copy(srcmodel);
	//cfg->initialise(srcmodel,NULL,TRUE,-1);
	//cfg->copy(srcmodel,CFG_R+CFG_Q);
	//srcmodel->draw_from_config(cfg);

	// Create the working configuration backup
	//cfg->backup();

	// Create ratio array (not per-pattern, just per move type)
	create_ratioarray(1);

	msg(DM_NONE,"Beginning Monte Carlo minimise...\n\n");
	msg(DM_NONE," Step     Energy        Delta          VDW          Elec              T%%  R%%  Z%%  I%%  D%%\n");

	// Calculate initial reference energy
	ecurrent = srcmodel->total_energy(srcmodel);
	elastcycle = ecurrent;
	msg(DM_NONE,"         %12.5f                %12.5f  %12.5f\n",ecurrent, srcmodel->energy.get_vdw(), srcmodel->energy.get_elec());
	// Cycle through move types; try and perform ntrials for each; move on.
	// For each attempt, select a random molecule in a random pattern
	nmoves = 0;
	// Make the reference list of pattern pointers for convenience
	npats = srcmodel->make_plist();
	pattern *p = NULL;
	// Loop over MC cycles
	for (cycle=0; cycle<ncycles; cycle++)
	{
		// Loop over MC moves
		for (move=0; move<MT_INSERT; move++)
		{
			acceptratio[0][move] = 0;
			// If this move type isn't allowed then continue onto the next
			if (!allowed[move]) continue;
			for (n=0; n<ntrials[move]; n++)
			{
				// Select random pattern and molecule
				npats != 1 ? randpat = cs_randomi(npats) : randpat = 0;
				p = srcmodel->plist[randpat];
				randmol = cs_randomi(p->get_nmols());
	
				// Copy the coordinates of the current molecule
				if (p->get_nmols() != 0) bakmodel.copy_atom_data(srcmodel, AD_R, p->get_offset(randmol),p->get_natoms());
				// Otherwise, generate the new configuration (in model's cfg space)
				switch (move)
				{
					// Translate COG of molecule
					case (MT_TRANSLATE):
						// Create a random translation vector
						v.random_unit();
						v *= maxstep[MT_TRANSLATE]*cs_random();
						// Translate the coordinates of the molecule in cfg
						srcmodel->translate_molecule(p,randmol,v);
						break;
					// Rotate molecule about COG
					case (MT_ROTATE):
						// To do the random rotation, do two separate random rotations about the x and y axes.
						phi = cs_random() * maxstep[MT_ROTATE];
						theta = cs_random() * maxstep[MT_ROTATE];
						srcmodel->rotate_molecule(p,randmol,phi,theta);
						break;
					// Other moves....
				}
				// Get the energy of this new configuration.
				enew = srcmodel->total_energy(srcmodel);
				// If the energy has gone up, undo the move.
				edelta = enew - ecurrent;
				if (edelta > 0.0)
				{
					// Put the molecules back to where it was before
					srcmodel->copy_atom_data(&bakmodel, AD_R, p->get_offset(randmol), p->get_natoms());
				}
				else
				{
					// Update energy and move counters
					ecurrent = enew;
					ecurrent_vdw = srcmodel->energy.get_vdw();
					ecurrent_elec = srcmodel->energy.get_elec();
					acceptratio[0][move] ++;
				}
			}
			if (ntrials[move] != 0) acceptratio[0][move] /= ntrials[move];
		} // Loop over MC moves
		//cycle%prefs.modelupdate == 0 ? master.gui_flow_and_render(TRUE) : master.gui_flow_and_render(FALSE);
		gui.process_events();
		msg(DM_NONE," %-5i %13.6e %13.6e %13.6e %13.6e   ", cycle+1, ecurrent, ecurrent-elastcycle, ecurrent_vdw, ecurrent_elec);
		for (n=0; n<MT_NITEMS; n++) msg(DM_NONE," %3i",int(acceptratio[0][n]*100.0));
		msg(DM_NONE,"\n");
		//if (prefs.update_energy(cycle))
			//msg(DM_NONE,"  %7i  %15.5f  %15.5f (T:%4.2f R:%4.2f A:%4.2f)\n",cycle,ecurrent,ecurrent-elastcycle, aratio[MT_TRANSLATE], aratio[MT_ROTATE], aratio[MT_ZMATRIX]);
		elastcycle = ecurrent;

	} // Loop over MC cycles

	// Finalise
	//cfg->copy_to(srcmodel,CFG_R);
	//srcmodel->draw_from_self();
	srcmodel->log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"mc::minimise");
	return TRUE;
}

// MC Insertion
bool mc_methods::disorder(model* destmodel)
{
	// Monte Carlo Insertion
	// Validity of forcefield and energy setup must be performed before calling and is *not* checked here.
	dbg_begin(DM_CALLS,"mc::insert");
	int n, m, cycle, move, mol, noldatoms;
	int pnmols;
	component *c;
	double enew, ecurrent, elastcycle, edelta, phi, theta, ecurrent_vdw, ecurrent_elec;
	double penalty;
	unitcell *cell;
	pattern *p, *destlastp;
	region *r;
	vec3<double> v, cog;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg(DM_NONE,"Creating expression for target model...\n");
        if (!destmodel->create_expression())
	{
		dbg_end(DM_CALLS,"mc::disorder");
		return FALSE;
	}
	destlastp = destmodel->get_last_pattern();
	noldatoms = destmodel->get_natoms();
	cell = &destmodel->cell;
	//destmodel->assign_charges(prefs.get_chargesource());
	// Fix all patterns in the destmodel
	destmodel->set_patterns_fixed(destmodel->get_npatterns());

	// Autocreate expressions for component models, paste copies in to the target model, and then add a corresponding pattern node.
	msg(DM_NONE,"Preparing destination model...\n");
	for (c = components.first(); c != NULL; c = c->next)
	{
		// Grab the model pointer for this component and set the number filled to zero
		model *m = c->get_model();
		c->set_nfilled(0);
		// Check that we can create a suitable expression for the component model
		if (!m->create_expression())
		{
			msg(DM_NONE,"Failed to create expression for component model '%s'.\n", m->get_name());
			dbg_end(DM_CALLS,"mc::disorder");
			return FALSE;
		}
		// TODO Autocreation of patterns may not give a 1*N pattern. Add option to force 1*N pattern.
		// Assign charges
		//m->assign_charges(prefs.get_chargesource());
		// Copy the model and paste it 'nrequested' times into destmodel
		master.privclip.copy_all(m);
		for (mol=0; mol<c->get_nrequested(); mol++) master.privclip.paste_to_model(destmodel);
		// Create a new pattern node in the destination model to cover these molecules and set it as the componentn's pattern
		p = destmodel->add_pattern(c->get_nrequested(), m->get_natoms(), m->get_name());
		p->set_expectedmols(c->get_nrequested());
		c->set_pattern(p);
		// Now we cut the pattern from its parent model, log a structure change so we invalidate the old model, and make the destmodel the new owner.
		//p = m->get_patterns();
		//p->set_expectedmols(c->get_nrequested());
		//p->set_nmols(c->get_nrequested());
		//m->cut_pattern(p);
		//destmodel->own_pattern(p,FALSE);
		//c->set_pattern(p);
		//regions.add(&comp->area,0,0);
		//totalatoms += c->get_nrequested() * m->get_natoms();
        }

	// Create master expression for the new (filled) model
	if (!destmodel->create_expression())
	{
		msg(DM_NONE,"Couldn't create master expression for destination model.\n");
		return FALSE;
	}
	// Set starting populations of patterns, add atom space to target model, and print out pattern list info.
	msg(DM_NONE,"Pattern info for insertion to model '%s':\n",destmodel->get_name());
	msg(DM_NONE,"  ID  nmols  atoms  starti  finali  name\n");
	for (p = destmodel->get_patterns(); p != NULL; p = p->next)
	{
		// Set nmols, starti, endi, startatom and endatom in the pattern
		msg(DM_NONE,"  %2i  %5i  %5i  %6i  %6i  %s\n",p->get_id(),p->get_nmols(),p->get_natoms(),
			p->get_startatom(),p->get_startatom() + p->get_totalatoms(),p->get_name());
	}

	// Reset number of molecules in component patterns to zero
	for (p = (destlastp != NULL ? destlastp : destmodel->get_patterns()); p != NULL; p = p->next) p->set_nmols(0);

	// Hide unused atoms to begin with
	atom **modelatoms = destmodel->get_staticatoms();
	for (n = noldatoms; n < destmodel->get_natoms(); n++) modelatoms[n]->set_hidden(TRUE);

	// Create a backup model
	msg(DM_NONE,"Preparing workspace for maximum of %i atoms...\n", destmodel->get_natoms());
	model bakmodel;
	bakmodel.copy(destmodel);

	// Make sure intramolecular energy calculation is off (bound terms only)
	bool intrastatus = prefs.calc_intra();
	prefs.set_calc_intra(FALSE);

	// Create array for move acceptance ratios
	create_ratioarray(destmodel->get_npatterns());

	// Set VDW scale ratio
	prefs.set_vdw_radius_scale(vdw_radius_scale);

	/*
	// Calculation
	*/
	// Cycle through move types; try and perform ntrials for each; move on.
	// For each attempt, select a random molecule in a random pattern
	msg(DM_NONE,"Beginning Monte Carlo insertion...\n\n");
	msg(DM_NONE," Step     Energy        Delta          VDW          Elec         Model    N     Nreq   T%%  R%%  Z%%  I%%  D%%\n");
	// Calculate initial reference energy
	ecurrent = destmodel->total_energy(destmodel);

	elastcycle = ecurrent;
	msg(DM_NONE," %-5i %13.6e %13s %13.6e %13.6e \n", 0, ecurrent, "     ---     ", destmodel->energy.get_vdw(), destmodel->energy.get_elec());
	// Loop over MC cycles
	for (cycle=0; cycle<ncycles; cycle++)
	{
		msg(DM_VERBOSE,"Begin cycle %i...\n",cycle);
		// Loop over patterns and regions together
		//p = destmodel->get_patterns();
		//regref = regions.first();
		for (c = components.first(); c != NULL; c = c->next)
		{
			// Get pointers to variables
			p = c->get_pattern();
			r = &c->area;
			msg(DM_VERBOSE,"Working on pattern '%s'\n",p->get_name());
			// If the pattern is fixed, move on
			if (p->is_fixed())
			{
				msg(DM_VERBOSE,"Pattern '%s' is fixed.\n",p->get_name());
				continue;
			}
			//r = regref->item;
			msg(DM_VERBOSE,"Pattern region is '%s'.\n",text_from_RS(r->get_shape()));

			// Copy the target molecule for MT_INSERT
			//master.privclip.copy_all(&p->molecule);

			// Loop over MC moves in reverse order so we do creation / destruction first
			for (move=MT_DELETE; move>-1; move--)
			{
				acceptratio[p->get_id()][move] = 0;
				// If this move type isn't allowed then continue onto the next
				if (!allowed[move]) continue;
				//if (!comp->get_allowed((type) move)) continue;
				for (n=0; n<ntrials[move]; n++)
				{
					// Reset penalty value
					penalty = 0.0;
					// Grab number of molecules currently in this pattern
					pnmols = p->get_nmols();
					// Perform the move
					switch (move)
					{
						// New molecule
						case (MT_INSERT):
							// Check if we've already filled as many as requested
							msg(DM_VERBOSE,"insert : Pattern %s has %i molecules.\n",p->get_name(),pnmols);
							if (pnmols == p->get_expectedmols()) continue;
							// Paste a new molecule into the working configuration
							msg(DM_VERBOSE,"insert : Pasting new molecule - pattern %s, mol %i\n",p->get_name(),pnmols);
							//master.privclip.paste_to_model(destmodel,p,pnmols);
							// Increase nmols for pattern and natoms for config
							mol = pnmols;		// Points to new molecule, since m-1
							p->set_nmols(mol+1);
							// Set the hidden flag on the new molecule to FALSE
							destmodel->hide_molecule(p,mol,FALSE);
							// Randomise position of new molecule
							v = r->random_coords(cell,components.first());
							destmodel->position_molecule(p,mol,v); 
							// Only rotate the new molecule if the component allows it
							// TODO if (!comp->get_allowed(MT_ROTATE)) break;
							if (allowed[MT_ROTATE])
							{
								phi = cs_random() * 360.0;
								theta = cs_random() * 360.0;
								destmodel->rotate_molecule(p,mol,phi,theta);
							}
							break;
						// Translate COG of molecule
						case (MT_TRANSLATE):
							if (pnmols == 0) continue;
							// Select random molecule, store, and move
							mol = cs_randomi(pnmols-1);
							bakmodel.copy_atom_data(destmodel, AD_R, p->get_offset(mol), p->get_natoms());
							// Create a random translation vector
							v.random_unit();
							v *= maxstep[MT_TRANSLATE]*cs_random();
							// Translate the coordinates of the molecule in cfg
							destmodel->translate_molecule(p,mol,v);
							// Check new COG is inside region
							cog = p->calculate_cog(destmodel,mol);
							if ((!r->check_coords(cog,cell)) || r->check_overlap(cog,cell,components.first())) penalty += 1e6;
							break;
						// Rotate molecule about COG
						case (MT_ROTATE):
							if (pnmols == 0) continue;
							// Select random molecule, store, and rotate
							mol = cs_randomi(pnmols-1);
							bakmodel.copy_atom_data(destmodel, AD_R, p->get_offset(mol), p->get_natoms());
							// Do two separate random rotations about the x and y axes.
							phi = cs_random() * maxstep[MT_ROTATE];
							theta = cs_random() * maxstep[MT_ROTATE];
							destmodel->rotate_molecule(p,mol,phi,theta);
							break;
						// Other moves....
					}

					// Get the energy of this new configuration.
					enew = destmodel->total_energy(destmodel);
					// Add on any penalty value
					enew += penalty;
					// If the energy has gone up, undo the move.
					edelta = enew - ecurrent;
					if (edelta > eaccept[move])
					{
						//printf("REJECTING MOVE : edelta = %20.14f\n",edelta);
						// Revert to the previous state.
						switch (move)
						{
							case (MT_INSERT):
								// Set the hidden flag on the new molecule to TRUE
								destmodel->hide_molecule(p,mol,TRUE);
								p->set_nmols(mol);
								break;
							case (MT_DELETE):
							default:
								destmodel->copy_atom_data(&bakmodel, AD_R, p->get_offset(mol), p->get_natoms());
								break;
						}
					}
					else
					{
						//printf("ACCEPTING MOVE : edelta = %20.14f\n",edelta);
						// Fold the molecule's atoms and recalculate its centre of geometry 
						//cfg->fold_molecule(p,mol);
						//destmodel->set_atom_colours(NULL);
						// Update energy and move counters
						ecurrent = enew;
						ecurrent_vdw = destmodel->energy.get_vdw();
						ecurrent_elec = destmodel->energy.get_elec();
						acceptratio[p->get_id()][move] ++;
					}
					gui.process_events();
				}
				// Get acceptance ratio percentages
				if (ntrials[move] != 0) acceptratio[p->get_id()][move] /= ntrials[move];
			}

			//p = p->next;
			//regref = regref->next;
		}
		if (prefs.update_energy(cycle))
		{
			// Print start of first line (current energy and difference)
			//msg(DM_NONE,"%7i  %13.6e %13.6e %13.6e %13.6e   %6.2f %6.2f %6.2f %6.2f %6.2f\n", cycle+1, ecurrent, ecurrent-elastcycle, ecurrent_vdw,ecurrent_elec, aratio[MT_TRANSLATE]*100.0, aratio[MT_ROTATE]*100.0, aratio[MT_ZMATRIX]*100.0, aratio[MT_INSERT]*100.0, aratio[MT_DELETE]*100.0);
			msg(DM_NONE," %-5i %13.6e %13.6e %13.6e %13.6e   ", cycle+1, ecurrent, ecurrent-elastcycle, ecurrent_vdw, ecurrent_elec);
			p = destmodel->get_patterns();
			while (p != NULL)
			{
				n = p->get_id();
				msg(DM_NONE,"%-8s %-4i (%-4i)", p->get_name(), p->get_nmols(), p->get_expectedmols());
				for (m=0; m<MT_NITEMS; m++)
					msg(DM_NONE," %3i",int(acceptratio[n][m]*100.0));
				p = p->next;
				if (p != NULL) msg(DM_NONE,"\n%65s"," ");
				else msg(DM_NONE,"\n");
			}
		}
		elastcycle = ecurrent;
	}	
	
	// Print out final data
	// Print out pattern list info here

	msg(DM_NONE,"Final populations for model '%s':\n",destmodel->get_name());
	msg(DM_NONE,"  ID  name                 nmols \n");
	p = destmodel->get_patterns();
	while (p != NULL)
	{
		printf("  %2i  %-20s  %6i\n",p->get_id(),p->get_name(),p->get_nmols());
		p = p->next;
	}

	// Reset VDW scale ratio
	prefs.set_vdw_radius_scale(1.0);

	// Remove all hidden atoms in the model (unused molecule space)
	atom *i, *j;
	i = destmodel->get_atoms();
	while (i != NULL)
	{
		if (i->is_hidden())
		{
			j = i;
			i = i->next;
			destmodel->delete_atom(j);
		}
		else i = i->next;
	}
	//bakmodel.copy(destmodel);
	//destmodel->recreate_from_patterns(&bakmodel);
	destmodel->render_from_self();
	destmodel->fold_all_atoms();
	destmodel->calculate_mass();
	destmodel->calculate_density();
	destmodel->log_change(LOG_COORDS);
	gui.refresh();
	dbg_end(DM_CALLS,"mc::insert");
	return TRUE;
}


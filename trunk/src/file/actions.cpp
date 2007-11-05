/*
	*** Filter model actions
	*** src/file/actions.cpp
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
#include "model/model.h"
#include "classes/surface.h"
#include "base/sysfunc.h"

// Read commands
bool filter::do_actions(command_node<filter_command> *&fn)
{
	dbg_begin(DM_CALLS,"filter::do_actions");
	atom *i;
	bool result = FALSE;
	switch (fn->get_command())
	{
		// Recalculate bonding in model
		case (FC_REBOND):
			if (activemodel == NULL) break;
			if (prefs.get_bond_on_load() != PS_NO)
			{
				activemodel->clear_bonding();
				activemodel->calculate_bonding();
			}
			result = TRUE;
			break;
		// Do crystal packing in model
		case (FC_PACK):
			if (activemodel == NULL) break;
			if (prefs.get_pack_on_load() != PS_NO) activemodel->apply_spacegroup_symmops(NULL);
			result = TRUE;
			break;
		// Centre model at 0,0,0
		case (FC_CENTRE):
			if (activemodel == NULL) break;
			if (prefs.get_centre_on_load() != PS_NO) activemodel->centre();
			result = TRUE;
			break;
		// Fold atoms into unit cell
		case (FC_FOLD):
			if (activemodel == NULL) break;
			if (prefs.get_fold_on_load() != PS_NO) activemodel->fold_all_atoms();
			result = TRUE;
			break;
		// Convert fractional coordinates to real coordinates
		case (FC_FRACTOREAL):
			if (activemodel == NULL) break;
			activemodel->frac_coords_to_real();
			result = TRUE;
			break;
		// Finalise current model
		case (FC_FINALISEMODEL):
			if (activemodel == NULL) break;
			if (partner != NULL) activemodel->set_filename(filename.get());
			activemodel->set_filter(partner);
			// Do various necessary calculations
			if (prefs.get_coords_in_bohr()) activemodel->bohr_to_angstrom();
			activemodel->renumber_atoms();
			activemodel->reset_view();
			activemodel->calculate_mass();
			activemodel->calculate_density();
			// Print out some useful info on the model that we've just read in
			msg(DM_NONE,"Atoms  : %i\n",activemodel->get_natoms());
			msg(DM_NONE,"Cell   : %s\n",text_from_CT(activemodel->cell.get_type()));
			if (activemodel->cell.get_type() != 0) activemodel->cell.print();
			// Lastly, reset all the log points and start afresh
			activemodel->reset_logs();
			activemodel->update_save_point();
			result = TRUE;
			break;
		// Finalise current surface
		case (FC_FINALISESURFACE):
			if (activesurface == NULL) break;
			if (prefs.get_coords_in_bohr()) activesurface->bohr_to_angstrom();
			result = TRUE;
			break;
		default:
			break;
	}
	if (result) fn = fn->next;
	dbg_end(DM_CALLS,"filter::do_actions");
	return result;
}

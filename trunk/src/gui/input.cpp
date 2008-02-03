/*
	*** GUI input routines
	*** src/gui/input.cpp
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

#include "base/master.h"
#include "base/prefs.h"
#include "gui/gui.h"
#include "gui/canvas.h"

// Static variables
bool canvas_master::mb[MB_NITEMS];
bool canvas_master::keymod[MK_NITEMS];

// Inform mouse down
void canvas_master::inform_mousedown(mouse_button button, double x, double y)
{
	r_mousedown.set(x,y,0.0);
	r_mouseup.set(x,y,0.0);
	// Determine if there is an atom under the mouse
	atom_hover = displaymodel->atom_on_screen(x,y);
	// If a model is being rendered, perform atom selection (if enabled)
	if (subselect_enabled && (atom_hover != NULL))
	{
		// Don't add the same atom more than once
		if (subsel.search(atom_hover) == NULL)
		{
			subsel.add(atom_hover,0,0);
			msg(DM_VERBOSE,"Adding atom %i to canvas subselection.\n",atom_hover);
		}
		else msg(DM_VERBOSE,"Atom %i is already in canvas subselection.\n",atom_hover);
	}
	// Activate mode...
	begin_mode(button);
}

// Inform mouse up
void canvas_master::inform_mouseup(mouse_button button, double x, double y)
{
	// Only finalise the mode if the button is the same as the one that caused the mousedown event.
	if (mb[button])
	{
		r_mouseup.set(x,y,0.0);
		// Deactivate mode...
		end_mode(button);
	}
	atom_hover = NULL;
}

// Inform mouse move
void canvas_master::inform_mousemove(double x, double y)
{
	// Perform action associated with mode (if any)
	if (activemode != UA_NONE) mode_motion(x,y);
	r_mouselast.set(x,y,0.0);
}

// Inform mouse wheel scroll
void canvas_master::inform_scroll(bool dir)
{
	mode_scroll(dir);
}

// Inform key down
void canvas_master::inform_keydown(key_code key)
{
	// Check datamodel...
	if (displaymodel == NULL) return;
	static model *viewtarget;
	// For view operations when we have a trajectory, apply all movement to the parent model
	viewtarget = displaymodel->get_trajparent();
	if (viewtarget == NULL) viewtarget = displaymodel;
	switch (key)
	{
		case (KC_SHIFT_L):
			keymod[MK_SHIFT] = TRUE;
			break;
		case (KC_SHIFT_R):
			keymod[MK_SHIFT] = TRUE;
			break;
		case (KC_CONTROL_L):
			keymod[MK_CTRL] = TRUE;
			break;
		case (KC_CONTROL_R):
			keymod[MK_CTRL] = TRUE;
			break;
		case (KC_ALT_L):
			keymod[MK_ALT] = TRUE;
			break;
		case (KC_ALT_R):
			keymod[MK_ALT] = TRUE;
			break;
		//case (GDK_Escape): master.check_before_close(); break;
		case (KC_LEFT):
			viewtarget->rotate(-10.0,0.0);
			postredisplay();
			break;
		case (KC_RIGHT):
			viewtarget->rotate(10.0,0.0);
			postredisplay();
			break;
		case (KC_UP):
			viewtarget->rotate(0.0,-10.0);
			postredisplay();
			break;
		case (KC_DOWN):
			viewtarget->rotate(0.0,10.0);
			postredisplay();
			break;
	}
}

// Inform key up
void canvas_master::inform_keyup(key_code key)
{
	switch (key)
	{
		case (KC_SHIFT_L):
			keymod[MK_SHIFT] = FALSE;
			break;
		case (KC_SHIFT_R):
			keymod[MK_SHIFT] = FALSE;
			break;
		case (KC_CONTROL_L):
			keymod[MK_CTRL] = FALSE;
			break;
		case (KC_CONTROL_R):
			keymod[MK_CTRL] = FALSE;
			break;
		case (KC_ALT_L):
			keymod[MK_ALT] = FALSE;
			break;
		case (KC_ALT_R):
			keymod[MK_ALT] = FALSE;
			break;
	}
}

/*
// Canvas Modes
*/

// Set selected mode
void canvas_master::set_selectedmode(user_action ua)
{
	dbg_begin(DM_CALLS,"canvas_master::set_selectedmode");
	selectedmode = ua;
	if (displaymodel == NULL)
	{
		printf("Pointless canvas_master::set_selectedmode - datamodel == NULL.\n");
		dbg_end(DM_CALLS,"canvas_master::set_selectedmode");
		return;
	}
	// Prepare canvas / model depending on the mode
	switch (ua)
	{
		case (UA_GEOMDIST):
		case (UA_GEOMANGLE):
		case (UA_GEOMTORSION):
		case (UA_BONDSINGLE):
		case (UA_BONDDOUBLE):
		case (UA_BONDTRIPLE):
		case (UA_DELBOND):
			subselect_enabled = TRUE;
			subsel.clear();
			break;
		default:
			subselect_enabled = FALSE;
			break;
	}
	gui.mainview.postredisplay();
	dbg_end(DM_CALLS,"canvas_master::set_selectedmode");
}

// Begin Mode
void canvas_master::begin_mode(mouse_button button)
{
	dbg_begin(DM_CALLS,"widgetcanvas::begin_mode");
	static bool manipulate, zrotate;
	static int n;
	static atom *i;
	// Do the requested action as defined in the control panel, but only if another action
	// isn't currently in progress. Set the user_action based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.
	// Set mouse flag and get state of modifier keys
	if (displaymodel == NULL)
	{
		printf("Pointless canvas_master::begin_mode - datamodel == NULL.\n");
		dbg_end(DM_CALLS,"canvas_master::begin_mode");
		return;
	}
	// Note the mouse button pressed
	mb[button] = TRUE;
	// Check for modifier keys
	zrotate = FALSE;
	manipulate = FALSE;
	for (n=0; n<3; n++)
	{
		if (keymod[n])
		{
			switch (prefs.get_keymod_action((modifier_key(n))))
			{
				case (KA_MANIPULATE):
					manipulate = TRUE;
					break;
				case (KA_ZROTATE):
					zrotate = TRUE;
					break;
			}
		}
	}
	// Now prepare for the action
	if (activemode == UA_NONE)
	{
		switch (prefs.get_mb_action(button))
		{
			// Main interactor - selection, sketching, measuring
			case (MA_INTERACT):
				use_selectedmode();
				// Some modes require actions to be done when the button is first depressed
				switch (activemode)
				{
					case (UA_DRAWCHAIN):
						// If there is currently no atom under the mouse, draw one...
						if (atom_hover == NULL)
						{
							displaymodel->begin_undostate("Draw Chain");
							i = displaymodel->add_atom(master.get_sketchelement(), displaymodel->guide_to_model(r_mousedown));
							displaymodel->end_undostate();
							displaymodel->project_atom(i);
							atom_hover = i;
						}
						break;
				}
				break;
			case (MA_VIEWROTATE):
				// Check for multiple key modifiers first.
				if (manipulate && zrotate) activemode = UA_MANIPROTZ;
				else if (manipulate) activemode = UA_MANIPROTXY;
				else if (zrotate) activemode = UA_ROTATEZ;
				else activemode = UA_ROTATEXY;
				// Perform any necessary actions
				if (manipulate) displaymodel->prepare_transform();
				break;
			case (MA_VIEWZOOM):
				activemode = UA_ZOOMCAM;
				break;
			case (MA_VIEWTRANSLATE):
				activemode = UA_MOVECAM;
				manipulate ? activemode = UA_MANIPTRANS : activemode = UA_MOVECAM;
				if (manipulate) displaymodel->prepare_transform();
				break;
		}
	}
	dbg_end(DM_CALLS,"canvas_master::begin_mode");
}

// End Mode
void canvas_master::end_mode(mouse_button button)
{
	// Finalize the current action on the model
	dbg_begin(DM_CALLS,"canvas_master::end_mode");
	bool manipulate;
	double area, radius;
	atom *atoms[4], *i;
	bond *b;
	if (displaymodel == NULL)
	{
		printf("Pointless canvas_master::end_mode - datamodel == NULL.\n");
		dbg_end(DM_CALLS,"canvas_master::end_mode");
		return;
	}
	// Reset mouse button flag
	mb[button] = FALSE;
	// Finalize the action
	switch (activemode)
	{
		// Group all the plain selection modes together (one for each toolbar in a diff. window)
		case (UA_PICKSELECT):
		case (UA_GEOMSELECT):
		case (UA_POSSELECT):
			area = fabs(r_mouseup.x - r_mousedown.x) * fabs(r_mouseup.y - r_mousedown.y);
			displaymodel->begin_undostate("Change Selection");
			// If SHIFT is not held down, deselect the current selection
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			// Do either point select or box select based on the size of the selected area
			if (area < 100.0)
			{
				if (keymod[MK_SHIFT])
				{
					if (atom_hover != NULL) displaymodel->select_atom(atom_hover);
				}
				else if (atom_hover != NULL) displaymodel->select_atom(atom_hover);
			}
			else displaymodel->select_box(r_mousedown.x, r_mousedown.y, r_mouseup.x, r_mouseup.y);
			displaymodel->end_undostate();
			break;
		// Now do the rest
		case (UA_PICKFRAG):
			displaymodel->begin_undostate("Select Molecule");
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			if (atom_hover != NULL) displaymodel->select_tree(atom_hover);
			displaymodel->end_undostate();
			break;
		case (UA_PICKELEMENT):
			displaymodel->begin_undostate("Select Element");
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			if (atom_hover != NULL) displaymodel->select_element(atom_hover);
			displaymodel->end_undostate();
			break;
		case (UA_PICKRADIAL):
			displaymodel->begin_undostate("Select Radial");
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			if (atom_hover != NULL)
			{
				radius = (r_mousedown-r_mouseup).magnitude();
				radius /= ((atom*) atom_hover)->get_screen_radius() * prefs.screenradius((atom*) atom_hover);
				displaymodel->select_radial(atom_hover,radius);
			}
			displaymodel->end_undostate();
			break;
		// Measurements
		case (UA_GEOMDIST):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 2) break;
			displaymodel->begin_undostate("Measure Distance");
			subsel.fill_array(2,atoms);
			displaymodel->measure_distance(atoms[0],atoms[1]);
			displaymodel->end_undostate();
			subsel.clear();
			break;
		case (UA_GEOMANGLE):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 3) break;
			displaymodel->begin_undostate("Measure Angle");
			subsel.fill_array(3,atoms);
			displaymodel->measure_angle(atoms[0],atoms[1],atoms[2]);
			displaymodel->end_undostate();
			subsel.clear();
			break;
		case (UA_GEOMTORSION):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 4) break;
			displaymodel->begin_undostate("Measure Torsion");
			subsel.fill_array(4,atoms);
			displaymodel->measure_torsion(atoms[0],atoms[1],atoms[2],atoms[3]);
			displaymodel->end_undostate();
			subsel.clear();
			break;
		// Draw single atom
		case (UA_DRAWATOM):
			// Make sure we don't draw on top of an existing atom
			if (atom_hover == NULL)
			{
				displaymodel->begin_undostate("Draw Atom");
				atom *i = displaymodel->add_atom(master.get_sketchelement(), displaymodel->guide_to_model(r_mousedown));
				displaymodel->end_undostate();
				displaymodel->project_atom(i);
			}
			break;
		// Draw chains of atoms
		case (UA_DRAWCHAIN):
			// If there is no atom under the mouse we draw one
			i = displaymodel->atom_on_screen(r_mouseup.x,r_mouseup.y);
			if ((atom_hover == i) && (i != NULL)) break;
			displaymodel->begin_undostate("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom
				i = displaymodel->add_atom(master.get_sketchelement(), displaymodel->guide_to_model(r_mouseup));
				displaymodel->project_atom(i);
			}
			// Now bond the atoms, unless atom_hover and i are the same (i.e. the button was clicked and not moved)
			if (atom_hover != i) displaymodel->bond_atoms(i,atom_hover,BT_SINGLE);
			displaymodel->end_undostate();
			break;
		case (UA_TRANSATOM):
			displaymodel->begin_undostate("Transmute");
			displaymodel->transmute_atom(atom_hover, master.get_sketchelement());
			displaymodel->end_undostate();
			break;
		case (UA_DELATOM):
			displaymodel->begin_undostate("Delete Atom");
			displaymodel->delete_atom(atom_hover);
			displaymodel->end_undostate();
			break;
		case (UA_PROBEATOM):
			if (atom_hover != NULL) atom_hover->print();
			break;
		// Bonding
		case (UA_BONDSINGLE):
		case (UA_BONDDOUBLE):
		case (UA_BONDTRIPLE):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 2) break;
			subsel.fill_array(2,atoms);
			b = atoms[0]->find_bond(atoms[1]);
			if (b == NULL)
			{
				displaymodel->begin_undostate("Bond Atoms");
				displaymodel->bond_atoms(atoms[0],atoms[1],bond_type(activemode-UA_BONDSINGLE+1));
				displaymodel->end_undostate();
			}
			else
			{
				displaymodel->begin_undostate("Change Bond");
				displaymodel->change_bond(b,bond_type(activemode-UA_BONDSINGLE+1));
				displaymodel->end_undostate();
			}
			subsel.clear();
			break;
		case (UA_DELBOND):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 2) break;
			subsel.fill_array(2,atoms);
			if (atoms[0]->find_bond(atoms[1]) != NULL)
			{
				displaymodel->begin_undostate("Delete Bond");
				displaymodel->unbond_atoms(atoms[0],atoms[1]);
				displaymodel->end_undostate();
			}
			subsel.clear();
			break;
		// Misc
		case (UA_ATOMADDHYDROGEN):
			if (atom_hover != NULL)
			{
				displaymodel->begin_undostate("Delete Bond"); displaymodel->hydrogen_satisfy(atom_hover);
				displaymodel->end_undostate();
			}
			break;
		// Model transformations
		case (UA_MANIPROTXY):
		case (UA_MANIPROTZ):
		case (UA_MANIPTRANS):
			displaymodel->finalize_transform();
			break;
		// View changes (no action)
		case (UA_ROTATEXY):
		case (UA_ROTATEZ):
		case (UA_MOVECAM):
		case (UA_ZOOMCAM):
			break;
		default:
			printf("No button_up handler defined for user_action %i.\n", activemode);
			break;
	}
	activemode = UA_NONE;
	postredisplay();
	dbg_end(DM_CALLS,"canvas_master::end_mode");
}

void canvas_master::mode_motion(double x, double y)
{
	// Actively update variables when moving the mouse (possibly while performing a given action)
	dbg_begin(DM_CALLS,"canvas_master::mode_motion");
	static vec3<double> delta;
	static model *viewtarget;
	if (displaymodel == NULL)
	{
		printf("Pointless canvas_master::mode_motion - datamodel == NULL.\n");
		dbg_end(DM_CALLS,"canvas_master::mode_motion");
		return;
	}
	// For view operations when we have a trajectory, apply all movement to the parent model
	viewtarget = displaymodel->get_trajparent();
	if (viewtarget == NULL) viewtarget = displaymodel;
	// Calculate new delta.
	delta.set(x,y,0.0);
	delta = delta - r_mouselast;
	// Use activemode to determine what needs to be performed
	switch (activemode)
	{
		case (UA_NONE):
			break;
		case (UA_ROTATEXY):
			viewtarget->rotate(delta.x/2.0,delta.y/2.0);
			break;
		case (UA_ROTATEZ):
			viewtarget->zrotate(delta.x/2.0);
			break;
		case (UA_MOVECAM):
			viewtarget->adjust_camera(delta/15.0,0.0);
			break;
		case (UA_MANIPROTXY):
			displaymodel->rotate_selection_world(delta.x/2.0,delta.y/2.0);
			break;
		case (UA_MANIPROTZ):
			displaymodel->manip_rotate_zaxis(delta.x/2.0);
			break;
		case (UA_MANIPTRANS):
			delta.y = -delta.y;
			delta /= displaymodel->get_translatescale() * 2.0;
			displaymodel->translate_selection_world(delta);
			break;
		case (UA_ZOOMCAM):
			if (prefs.using_perspective()) viewtarget->adjust_camera(0.0,0.0,delta.y,0.0);
			else viewtarget->adjust_ortho_size(delta.y);
			calculate_drawpixelwidth();
			break;
		default:
			break;
	}
	postredisplay();
	dbg_end(DM_CALLS,"canvas_master::mode_motion");
}

void canvas_master::mode_scroll(bool scrollup)
{
	// Handle mouse-wheel scroll events.
	// Do the requested wheel action as defined in the control panel
	dbg_begin(DM_CALLS,"canvas_master::mode_scroll");
	static model *viewtarget;
	if (displaymodel == NULL)
	{
		printf("Pointless canvas_master::mode_scroll - datamodel == NULL.\n");
		dbg_end(DM_CALLS,"canvas_master::mode_scroll");
		return;
	}
	// For view operations when we have a trajectory, apply all movement to the parent model
	viewtarget = displaymodel->get_trajparent();
	if (viewtarget == NULL) viewtarget = displaymodel;
	switch (prefs.get_mb_action(MB_WHEEL))
	{
		case (MA_NONE):
			break;
		case (MA_INTERACT):
			use_selectedmode();
			break;
		case (MA_VIEWROTATE):
			scrollup ? viewtarget->rotate(1.0,0.0) : viewtarget->rotate(-1.0,0.0);
			break;
		case (MA_VIEWTRANSLATE):
			break;
		case (MA_VIEWZOOM):
			if (prefs.using_perspective())
				scrollup ? viewtarget->adjust_camera(0.0,0.0,-5.0,0.0) : viewtarget->adjust_camera(0.0,0.0,5.0,0.0);
			else scrollup ? viewtarget->adjust_ortho_size(1.0) : viewtarget->adjust_ortho_size(-1.0);
			calculate_drawpixelwidth();
			break;
	}
	postredisplay();
	dbg_end(DM_CALLS,"canvas_master::mode_scroll");
}

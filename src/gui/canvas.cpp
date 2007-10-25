/*
	*** Model canvas stub
	*** src/gui/canvas.cpp
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
gl_objects canvas_master::globs;

// Constructor
canvas_master::canvas_master()
{
	valid = FALSE;
	render_point = -1;
	drawing = FALSE;
	displaymodel = NULL;
	activemode = UA_NONE;
	selectedmode = UA_PICKSELECT;
	list_modelcontents = 0;
}

// Destructor
canvas_master::~canvas_master()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_BASICCANVAS] ++;
	#endif
}

// Update Canvas
void canvas_master::postredisplay()
{
	printf("canvas_master::postredisplay - Not defined\n");
}

// Called when context is initialised and ready
void canvas_master::realize()
{
	printf("canvas_master::realize - Not defined\n");
}

// Called when context is resized
void canvas_master::configure()
{
	printf("canvas_master::configure - Not defined\n");
}

// Called when context needs to be redrawn
void canvas_master::expose()
{
	printf("canvas_master::expose - Not defined\n");
}

// Swap buffers
void canvas_master::swap_buffers()
{
	printf("canvas_master::swap_buffers - Not defined\n");
}

// Begin GL
bool canvas_master::begin_gl()
{
	printf("canvas_master::begin_gl - Not defined\n");
	return FALSE;
}

// End GL
void canvas_master::end_gl()
{
	printf("canvas_master::end_gl - Not defined\n");
}

// Set GL options
void canvas_master::init_gl()
{
	if (!valid) return;
	dbg_begin(DM_CALLS,"canvas_master::init_gl");
	if (begin_gl())
	{
		// Create model list (if necessary)
		if (list_modelcontents == 0) list_modelcontents = glGenLists(1);
		// Clear colour (with alpha = 0)
		GLint *clrcol = prefs.get_colour(COL_BG);
		glClearColor(clrcol[0],clrcol[1],clrcol[2],clrcol[3]);
		glClearDepth(1.0);
		// Perspective hint
		glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST);
		// Enable depth buffer
		glEnable(GL_DEPTH_TEST);
		// Smooth shading
		glShadeModel(GL_SMOOTH);
		// Auto-calculate surface normals
		glEnable(GL_NORMALIZE);
		// Set alpha-blending function
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
		//glBlendFunc(GL_SRC_ALPHA_SATURATE, GL_ONE);
		// Set up the light model
		glEnable(GL_LIGHTING);
		glLightiv(GL_LIGHT0,GL_AMBIENT,prefs.get_spotlight(SL_AMBIENT));
		glLightiv(GL_LIGHT0,GL_DIFFUSE,prefs.get_spotlight(SL_DIFFUSE));
		glLightiv(GL_LIGHT0,GL_SPECULAR,prefs.get_spotlight(SL_SPECULAR));
		glLightiv(GL_LIGHT0,GL_POSITION,prefs.get_spotlight(SL_POSITION));
		prefs.get_spotlight_on() ? glEnable(GL_LIGHT0) : glDisable(GL_LIGHT0);
		glDisable(GL_BLEND);
		glDisable(GL_LINE_SMOOTH);
		glDisable(GL_POLYGON_SMOOTH);
		// Configure antialiasing
		if (prefs.get_gl_option(GO_LINEALIASING))
		{
			glEnable(GL_BLEND);
			glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_LINE_SMOOTH);
		}
		if (prefs.get_gl_option(GO_POLYALIASING))
		{
			glEnable(GL_BLEND);
			glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
			glEnable(GL_POLYGON_SMOOTH);
		}
		// Configure fog effects
		if (prefs.get_gl_option(GO_FOG))
		{
			glFogi(GL_FOG_MODE, GL_LINEAR);
			GLfloat fogColor[4]= { prefs.colours[COL_BG][0], prefs.colours[COL_BG][1], prefs.colours[COL_BG][2], 1.0 };
			glFogfv(GL_FOG_COLOR, fogColor);
			glFogf(GL_FOG_DENSITY, 0.35f);
			glHint(GL_FOG_HINT, GL_NICEST);
			glFogf(GL_FOG_START,prefs.gl_fog_near);
			glFogf(GL_FOG_END,prefs.gl_fog_far);
			glEnable(GL_FOG);
		}
		else glDisable(GL_FOG);
		// Configure face culling
		glCullFace(GL_BACK);
		prefs.get_gl_option(GO_BACKCULLING) ? glEnable( GL_CULL_FACE ) : glDisable(GL_CULL_FACE);
		// Test
		// End Test
		end_gl();
	}
	else printf("Failed to set-up OpenGL on canvas.\n");
	dbg_end(DM_CALLS,"canvas_master::setup_gl");
}

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
		else
			msg(DM_VERBOSE,"Atom %i is already in canvas subselection.\n",atom_hover);
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
	// If drawing a model, see if an atom is under this position
	//if (displaymodel != NULL) atom_hover = displaymodel->atom_on_screen(x,y);
	// Perform action associated with mode (if any)
	if (activemode != UA_NONE) mode_motion(x,y);
	postredisplay();
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
// OpenGL Begin/End
*/

// Set valid
void canvas_master::set_valid(bool b)
{
	// Wait until the canvas is not drawing
	while (!valid) gui.process_events();
	// Now disallow drawing before we set the new status
	valid = FALSE;
	drawing = FALSE;
	valid = b;
}

/*
// Configuration
*/

// Calculate Projection
void canvas_master::do_projection()
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"canvas_master::do_projection");
	double pmat[16], bottom, top;
	// Check source
	if (begin_gl())
	{
		// Set the viewport size to the whole area and grab the matrix
		glViewport(0,0,(int)w,(int)h);
		glGetIntegerv(GL_VIEWPORT,VMAT);
		// Calculate and store a projection matrix
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		aspect = w / h;
		if (prefs.render_perspective)
		{
			// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
			bottom = tan(prefs.render_fov / DEGRAD) * prefs.gl_clip_near;
			top = -bottom;
			glFrustum(aspect*top,aspect*bottom,top,bottom,prefs.gl_clip_near,prefs.gl_clip_far);
		}
		else
		{
			bottom = displaymodel->get_ortho_size();
			top = -bottom;
			//glOrtho(aspect*top,aspect*bottom,top,bottom,-bottom*2.0,bottom*2.0);
			glOrtho(aspect*top,aspect*bottom,top,bottom,-prefs.gl_clip_near,prefs.gl_clip_far);
		}
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		PMAT.set_from_column_major(pmat);
		// Rotation globe projection matrix (square)
		glLoadIdentity();
		glFrustum(-1.0, 1.0, -1.0, 1.0, 0.0, 10.0);
		glGetDoublev(GL_PROJECTION_MATRIX,pmat); // Store the resulting projection and
		GlobePMAT.set_from_column_major(pmat);
		glMatrixMode(GL_MODELVIEW);
		end_gl();
	}
	else printf("canvas_master::do_projection <<<< Failed to reset projection matrix >>>>\n");
	dbg_end(DM_CALLS,"canvas_master::do_projection");
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
		case (UA_DRAWCHAIN):
			displaymodel->set_lastatomdrawn(NULL);
		default:
			subselect_enabled = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"canvas_master::set_selectedmode");
}

// Begin Mode
void canvas_master::begin_mode(mouse_button button)
{
	dbg_begin(DM_CALLS,"widgetcanvas::begin_mode");
	static bool manipulate, zrotate;
	static int n;
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
				break;
			case (MA_VIEWROTATE):
				prefs.apply_staticstyle();
				// Check for multiple key modifiers first.
				if (manipulate && zrotate) activemode = UA_MANIPROTZ;
				else if (manipulate) activemode = UA_MANIPROTXY;
				else if (zrotate) activemode = UA_ROTATEZ;
				else activemode = UA_ROTATEXY;
				// Perform any necessary actions
				if (manipulate) displaymodel->prepare_transform();
				break;
			case (MA_VIEWZOOM):
				prefs.apply_staticstyle();
				activemode = UA_ZOOMCAM;
				break;
			case (MA_VIEWTRANSLATE):
				prefs.apply_staticstyle();
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
	atom *atoms[4], *i, *lasti;
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
			break;
		// Now do the rest
		case (UA_PICKFRAG):
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			if (atom_hover != NULL) displaymodel->select_tree(atom_hover);
			break;
		case (UA_PICKELEMENT):
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			if (atom_hover != NULL) displaymodel->select_element(atom_hover);
			break;
		case (UA_PICKRADIAL):
			if (!keymod[MK_SHIFT]) displaymodel->select_none();
			if (atom_hover != NULL)
			{
				radius = (r_mousedown-r_mouseup).magnitude();
				radius /= ((atom*) atom_hover)->get_screen_radius() * prefs.screenradius((atom*) atom_hover);
				displaymodel->select_radial(atom_hover,radius);
			}
			break;
		// Measurements
		case (UA_GEOMDIST):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 2) break;
			subsel.fill_array(2,atoms);
			displaymodel->measure_distance(atoms[0],atoms[1]);
			subsel.clear();
			break;
		case (UA_GEOMANGLE):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 3) break;
			subsel.fill_array(3,atoms);
			displaymodel->measure_angle(atoms[0],atoms[1],atoms[2]);
			subsel.clear();
			break;
		case (UA_GEOMTORSION):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 4) break;
			subsel.fill_array(4,atoms);
			displaymodel->measure_torsion(atoms[0],atoms[1],atoms[2],atoms[3]);
			subsel.clear();
			break;
		// Draw single atom
		case (UA_DRAWATOM):
			// Make sure we don't draw on top of an existing atom
			if (atom_hover == NULL)
			{
				atom *i = displaymodel->add_atom(master.sketchelement);
				i->r = displaymodel->guide_to_model(r_mousedown);
				displaymodel->project_atom(i);
			}
			break;
		// Draw chains of atoms
		case (UA_DRAWCHAIN):
			// Grab last atom added to the model first since it will be overwritten as soon as we draw the next atom
			lasti = displaymodel->get_lastatomdrawn();
			if (atom_hover == NULL)
			{
				// No atom under the mouse, so draw an atom
				atom *i = displaymodel->add_atom(master.sketchelement);
				i->r = displaymodel->guide_to_model(r_mousedown);
				displaymodel->project_atom(i);
				// If lastsketched holds an atom, add a bond as well
				if (lasti != NULL) displaymodel->bond_atoms(i,lasti,BT_SINGLE);
			}
			else
			{
				// An existing atom was clicked.
				// If we have a previously drawn atom, bond them, then set it to NULL. Otherwise, do nothing.
				if (lasti != NULL)
				{
					displaymodel->bond_atoms(lasti, atom_hover, BT_SINGLE);
					displaymodel->set_lastatomdrawn(atom_hover);
				}
			}
			break;
		case (UA_TRANSATOM):
			displaymodel->transmute_atom(atom_hover, master.sketchelement);
			break;
		case (UA_DELATOM):
			displaymodel->delete_atom(atom_hover);
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
			if (atoms[0]->find_bond(atoms[1]) == NULL) displaymodel->bond_atoms(atoms[0],atoms[1],bond_type(activemode-UA_BONDSINGLE+1));
			else
			{
				displaymodel->unbond_atoms(atoms[0],atoms[1]);
				displaymodel->bond_atoms(atoms[0],atoms[1],bond_type(activemode-UA_BONDSINGLE+1));
			}
			subsel.clear();
			break;
		case (UA_DELBOND):
			// Must be two atoms in subselection to continue
			if (subsel.size() != 2) break;
			subsel.fill_array(2,atoms);
			if (atoms[0]->find_bond(atoms[1]) != NULL) displaymodel->unbond_atoms(atoms[0],atoms[1]);
			subsel.clear();
			break;
		case (UA_ROTATEXY):
		case (UA_ROTATEZ):
		case (UA_MOVECAM):
		case (UA_ZOOMCAM):
			break;
		case (UA_MANIPROTXY):
		case (UA_MANIPROTZ):
		case (UA_MANIPTRANS):
			displaymodel->finalize_transform();
			break;
		default:
			printf("No button_up handler defined for user_action %i.\n", activemode);
			break;
	}
	prefs.apply_staticstyle();
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
			displaymodel->translate_selection_world(delta);
			break;
		case (UA_ZOOMCAM):
			if (prefs.using_perspective()) viewtarget->adjust_camera(0.0,0.0,delta.y,0.0);
			else viewtarget->adjust_ortho_size(delta.y);
			break;
		default:
			break;
	}
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
			break;
	}
	postredisplay();
	dbg_end(DM_CALLS,"canvas_master::mode_scroll");
}

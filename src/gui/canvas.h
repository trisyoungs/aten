/*
	*** Qt canvas
	*** src/gui/canvas.h
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

#ifndef H_CANVASQT_H
#define H_CANVASQT_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "base/prefs.h"
#include "render/gl2ps_extra.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// Actions
enum user_action { UA_NONE,
		UA_PICKSELECT, UA_PICKFRAG, UA_PICKELEMENT, UA_PICKRADIAL,
		UA_GEOMSELECT, UA_GEOMDIST, UA_GEOMANGLE, UA_GEOMTORSION, 
		UA_POSSELECT, UA_POSCENTRE, UA_POSTRANSLATE, UA_POSROTATE,
		UA_DRAWATOM, UA_DRAWCHAIN, UA_TRANSATOM, UA_DELATOM, UA_PROBEATOM,
		UA_BONDSINGLE, UA_BONDDOUBLE, UA_BONDTRIPLE, UA_DELBOND,
		UA_ROTATEXY, UA_ROTATEZ, UA_MOVECAM, UA_ZOOMCAM, 
		UA_MANIPROTXY, UA_MANIPROTZ, UA_MANIPTRANS,
		UA_ATOMADDHYDROGEN,
		UA_NITEMS };

// Keyboard Key Codes (translated from GTK/KDE keysyms)
enum key_code { KC_OTHER, KC_ESCAPE, KC_SHIFT_L, KC_SHIFT_R, KC_CONTROL_L, KC_CONTROL_R, KC_ALT_L, KC_ALT_R, KC_LEFT, KC_RIGHT, KC_UP, KC_DOWN, KC_NITEMS };

// GL Objects
enum glob_list { GLOB_STICKATOM, GLOB_TUBEATOM, GLOB_SPHEREATOM, GLOB_UNITATOM, GLOB_WIRETUBEATOM, GLOB_WIRESPHEREATOM, GLOB_WIREUNITATOM, GLOB_CYLINDER, GLOB_WIRECYLINDER, GLOB_SELWIRECYLINDER, GLOB_GLOBE, GLOB_GUIDE, GLOB_CIRCLE, GLOB_CELLAXES, GLOB_SELTUBEATOM, GLOB_SELSPHEREATOM, GLOB_SELUNITATOM, GLOB_WIREUNITCUBE, GLOB_UNITCUBE, GLOB_MODEL, GLOB_NITEMS };

// Forward declarations
class atom;
class bond;
class model;
class geometry;
class subselection;
class unitcell;
class TCanvas;

/*
// Canvas Master Class
// Provides GL rendering functions for a context
*/
class canvas
{
	/*
	// Base rendering context
	*/
	private:
	// Internal name of the canvas for error reporting
	const char *name;
	// Width, height, and aspect ratio of the canvas
	double w, h, aspect;
	// Point at which the stored atom display list was valid (sum of LOG_STRUCTURE and LOG_COORDS points)
	int render_point;
	// Flag to indicate whether we may draw to the canvas
	bool valid;
	// Flag indicating if we are currently drawing to this canvas
	bool drawing;
	// Model 'width' of a single pixel at the current draw depth...
	double drawpixelwidth;
	// Qt Target widget
	TCanvas *context_widget;

	public:
	// Constructor / Destructor
	canvas();
	~canvas();
	// Set the internal name of the canvas
	void set_name(const char *s) { name = s; }
	// Return the current height of the drawing area
	float get_height() { return h; }
	// Return the current width of the drawing area
	float get_width() { return w; }
	// Return whether the canvas is currently drawing
	bool is_drawing() { return drawing; }
	// Return if the canvas is valid
	bool is_valid() { return valid; }
	// Set the validity of the canvas
	void set_valid(bool);
	// Recalculate drawing pixel width
	void calculate_drawpixelwidth();
	// Return the corrent drawing pixel width
	double get_drawpixelwidth();
	// Set up widget for OpenGL drawing
	bool set_widget(TCanvas*);
	// Update Canvas
	void postredisplay();
	// Called when context is initialised and ready
	void realize();
	// Called when context is resized
	void configure();
	// Called when context needs to be redrawn
	void expose();
	// Swap buffers
	void swap_buffers();

	/*
	// Rendering display lists
	*/
	private:
	// Quadric objects
	GLUquadricObj *quadric1, *quadric2;
	// Display list ID's
	GLuint list[GLOB_NITEMS];

	public:
	// Create globs for rendering
	void create_lists();

	/*
	// General Rendering Objects / Calls
	*/
	private:
	// Render text string at specific coordinates
	void textbitmap(double, double, const char*);
	// Render text string at atom's screen coordinates
	void textbitmap(const vec3<double>, const char*);
	// Render vector text at the current position
	void textstroke(const char*);
	// Draw a diamond
	void gl_diamond(double, double, double);
	// Draw a square
	void gl_square(double, double, double);
	// Draw a rectangle
	void gl_rectangle(double, double, double, double);
	// Draw 3d marks for the atoms in the subselection
	void gl_subsel_3d();
	// Draw a circle
	void gl_circle(double, double, double);
	// Draw a cylinder along vector supplied
	void gl_cylinder(const vec3<double> &vec, double length, int style);
	// Draw ellipsoid
	void gl_ellipsoid(const vec3<double>&, const vec3<double>&, const vec3<double>&);
	// Draw the unit cell of the model
	void gl_cell(unitcell*);
	// Draw a line arrow
	void gl_arrow(const vec3<double>&, const vec3<double>&);
	// Manually draw a unit sphere
	void gl_sphere(double radius);

	protected:
	// Last model rendered by canvas (needed for mouse hover etc.)
	model *displaymodel;

	public:
	// Configure OpenGL
	void init_gl();
	// Begin construct for any OpenGL commands
	virtual bool begin_gl();
	// Finish OpenGL commands
	virtual void end_gl();
	// Reset the projection matrix based on the current canvas geometry
	void do_projection();
	// Projection matrices for scene and rotation globe
	mat4<double> PMAT, GlobePMAT;
	// Viewport matrix for canvas
	GLint VMAT[4];
	// Return the current display model
	model *get_displaymodel() { return displaymodel; }

	/*
	// Scene Rendering
	*/
	private:
	// Render the rotation globe
	void render_rotation_globe(double *rotmat, double camrot);
	// Render the model specified
	void render_model_atoms();
	// Render glyphs in the current model
	void render_model_glyphs();
	// Add labels to the model
	void render_model_labels();
	// Add geometry measurements to the model
	void render_model_measurements();
	// Add extra 3D objects
	void render_extra_3d();
	// Add extra 2D objects
	void render_extra_2d();
	// Draw regions specified for MC insertion
	void render_regions();
	// Draw model force arrows
	void render_model_forcearrows();
	// Render model cell
	void render_model_cell();
	// Render surfaces
	void render_surfaces();

	public:
	// Render a scene based on the specified model
	void render_scene(model*);
	// Save scene as vector image
	void save_vector(model *source, vector_format vf, const char *filename);

	/*
	// Selection
	*/
	protected:
	// Atom that the mouse pointer is currently hovering over
	atom *atom_hover;
	// Subselection (list of clicked atoms for interactive tools)
	reflist<atom,int> subsel;
	// Whether we are selecting atoms and placing them in the subsel list	
	bool subselect_enabled;

	public:
	// Returns the atom currently under the mouse
	atom *get_atom_hover() { return atom_hover; }
	// Clears the subsel of atoms
	void clear_subsel() { subsel.clear(); }

	/*
	// Mouse
	*/
	protected:
	// Canvas coordinates of mouse down / mouse up events
	vec3<double> r_mouseup, r_mousedown;
	// Canvas coordinates of mouse cursor
	vec3<double> r_mouselast;

	/*
	// Interaction
	*/
	protected:
	// Active interaction mode of the main canvas
	user_action activemode;
	// Selected interaction mode (from GUI)
	user_action selectedmode;
	// Button flags (uses enum 'mouse_button')
	static bool mb[MB_NITEMS];
	// Key flags (uses enum 'modifier_key')
	static bool keymod[MK_NITEMS];
	// Begin an action on the model (called from mouse_buttondown)
	void begin_mode(mouse_button);
	// Handle mouse motion while performing actions
	void mode_motion(double, double);
	// Handle mousewheel scroll events
	void mode_scroll(bool);
	// End an action on the model (called from mouse_buttonup)
	void end_mode(mouse_button);

	public:
	// Set the active mode to the current user mode
	void use_selectedmode() { activemode = selectedmode; }
	// Sets the currently selected interact mode
	void set_selectedmode(user_action);
	// Return the currently selected mode
	user_action get_selectedmode() { return selectedmode; }
	// Inform the canvas of a mouse down event
	void inform_mousedown(mouse_button, double, double);
	// Inform the canvas of a mouse up event
	void inform_mouseup(mouse_button, double, double);
	// Inform the canvas of a mouse move event
	void inform_mousemove(double, double);
	// Inform the canvas of a mouse wheel scroll event
	void inform_scroll(bool);
	// Inform the canvas of a keydown event
	void inform_keydown(key_code);
	// Inform the canvas of a keydown event
	void inform_keyup(key_code);
};

#endif

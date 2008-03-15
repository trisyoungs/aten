/*
	*** Preferences storage
	*** src/base/prefs.h
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

#ifndef ATEN_PREFS_H
#define ATEN_PREFS_H

#include "energy/forms.h"
#include "templates/vector3.h"
#include "base/constants.h"
#include "classes/atom.h"

#ifdef IS_MAC
	#include "OpenGL/gl.h"
#else
	#include "GL/gl.h"
#endif

// Atom colouring scheme
enum atom_colours { AC_ELEMENT, AC_CHARGE, AC_VELOCITY, AC_FORCE, AC_NITEMS };

// Preferences switches
enum file_prefswitch { PS_ASFILTER=-1, PS_NO, PS_YES };

// View Objects
enum view_object { VO_ATOMS, VO_CELL, VO_CELLAXES, VO_CELLREPEAT, VO_FORCEARROWS, VO_GLOBE, VO_LABELS, VO_MEASUREMENTS, VO_REGIONS, VO_SURFACES, VO_NITEMS };
view_object VO_from_text(const char*);

// GL Options
enum gl_option { GO_FOG=1, GO_LINEALIASING=2, GO_POLYALIASING=4, GO_BACKCULLING=8, GO_DUMMY=16, GO_NITEMS=5 };
gl_option GO_from_text(const char*);

// Mouse buttons
enum mouse_button { MB_LEFT, MB_MIDDLE, MB_RIGHT, MB_WHEEL, MB_NITEMS };
mouse_button MB_from_text(const char*);
const char *text_from_MB(mouse_button);

// Mouse Actions
enum mouse_action { MA_NONE, MA_VIEWROTATE, MA_VIEWTRANSLATE, MA_INTERACT, MA_VIEWZOOM, MA_VIEWZROTATE, MA_NITEMS };
mouse_action MA_from_text(const char*);
const char *text_from_MA(mouse_action);
const char **get_MA_strings();

// Modifier keys
enum modifier_key { MK_SHIFT, MK_CTRL, MK_ALT, MK_NITEMS };
const char **get_MK_strings();
modifier_key MK_from_text(const char*);

// Modifier actions
enum key_action { KA_NONE, KA_MANIPULATE, KA_ZROTATE, KA_NITEMS };
const char **get_KA_strings();
key_action KA_from_text(const char*);

// Colours
enum colour { COL_PEN, COL_BG, COL_SCHEMELO, COL_SCHEMEMID, COL_SCHEMEHI, COL_SPECREFLECT, COL_NITEMS };
const char *text_from_COL(colour);
colour COL_from_text(const char*);

// Density calculation units
enum density_unit { DU_GPERCM, DU_ATOMSPERANG, DU_NITEMS };
const char *text_from_DU(density_unit);
density_unit DU_from_text(const char*);

// Drawing guide geometry
enum guide_geometry { GG_SQUARE, GG_HEXAGONAL, GG_NITEMS };
const char **get_GG_strings();

// Energy Units
enum energy_unit { EU_J, EU_KJ, EU_CAL, EU_KCAL, EU_EV, EU_HARTREE, EU_NITEMS };
const char *text_from_EU(energy_unit);
energy_unit EU_from_text(const char*);

// Name->Z mapping methods
enum zmap_type { ZM_ALPHA, ZM_FIRSTALPHA, ZM_NAME, ZM_NUMERIC, ZM_FORCEFIELD, ZM_AUTO, ZM_NITEMS };
zmap_type ZM_from_text(const char*);
const char **get_ZM_keywords();

// Charge source
enum charge_source { QS_MODEL, QS_FF, QS_GASTEIGER, QS_QEQ, QS_NITEMS };

// Spotlight Components
enum spotlight_component { SL_AMBIENT, SL_DIFFUSE, SL_SPECULAR, SL_POSITION, SL_NITEMS };

// Forward declarations
class unitcell;
class atom;

// Prefs
class prefs_data
{
	public:
	// Constructor / Destructor
	prefs_data();
	~prefs_data();
	// Load prefs from file
	void load(const char*);
	// Set GUI controls to reflect prefs choices
	void set_controls();
	// Allow rendering routines access to the structure
	friend class canvas;
	// Allow GL onject generation access to the structure
	friend class gl_objects;

	/*
	// Rendering - View Objects
	*/
	private:
	// List of visibilities of renderable objects
	bool render_objects[VO_NITEMS];
	// Repeat units in positive xyz directions
	vec3<int> render_repcellpos;
	// Repeat units in negative xyz directions
	vec3<int> render_repcellneg;
	// Scaling factor for 3D labels
	double render_label_scale;
	// Size in pixels of the viewport to draw the rotation globe in.
	int render_globe_size;
	// Rendering style of models
	draw_style render_style;

	public:
	// Set the visibility of an object
	void set_visible(view_object vo, bool b) { render_objects[vo] = b; }
	// Return whether the specified object is visible (i.e. should be rendered)
	bool should_render(view_object vo) { return render_objects[vo]; }
	// Return the radius of an atom calculated from the element and draw style
	double screenradius(atom*);
	// Set the drawing style of models
	void set_render_style(draw_style ds) { render_style = ds; }
	// Return the current drawing style of models
	draw_style get_render_style() { return render_style; }
	// Set the scale of labels in the model
	void set_label_scale(double v) { render_label_scale = v; }
	// Return the current label scale
	double get_label_scale() { return render_label_scale; }
	// Set positive repeat cell value
	void set_repcellpos(int i, int r) { render_repcellpos.set(i,r); }
	// Get positive repeat cell value
	int get_repcellpos(int i) { return render_repcellpos.get(i); }
	// Set negative repeat cell value
	void set_repcellneg(int i, int r) { render_repcellneg.set(i,r); }
	// Get negative repeat cell value
	int get_repcellneg(int i) { return render_repcellneg.get(i); }

	/*
	// Rendering - Style
	*/
	private:
	// Atom sizes / radii
	GLdouble render_atom_size[DS_NITEMS];
	// Tube size for DS_SPHERE / DS_TUBE / DS_SCALED
	GLdouble render_tube_size;
	// Size scaling for atom selection transparency
	GLdouble render_selection_scale;
	// Detail of atom quadric (slices/stacks)
	int render_atom_detail;
	// Detail of bond quadric (slices/stacks)
	int render_bond_detail;
	// Whether to use a perspective (TRUE) or orthographic (FALSE) projection
	bool render_perspective;
	// Viewing angle for perspective projection
	GLdouble render_fov;
	// Whether the spotlight is on
	bool spotlight_on;
	// Spotlight components
	GLfloat spotlight_components[SL_NITEMS][4];
	// Atom colouring style
	atom_colours colour_scheme;
	// Number of segments between lo/hi and mid colours in colour scale
	int scale_segments;
	// Graduated colour scale colours
	GLfloat **scale_colours;

	public:
	// Sets the specified atom size to the given value
	void set_atom_size(draw_style ds, double f) { render_atom_size[(int)ds] = f; }
	// Return the specified atom size
	GLdouble get_atom_size(draw_style ds) { return render_atom_size[(int)ds]; }
	// Sets the tube size in DS_TUBE
	void set_tube_size(double f) { render_tube_size = f; }
	// Return the tube size used in DS_TUBE
	GLdouble get_tube_size() { return render_tube_size; }
	// Sets the detail for atom quadrics
	void set_atom_detail(int n) { render_atom_detail = n; }
	// Return the current detail of atom quadrics
	int get_atom_detail() { return render_atom_detail; }
	// Sets the detail for bond quadrics
	void set_bond_detail(int n) { render_bond_detail = n; }
	// Return the current detail of bond quadrics
	int get_bond_detail() { return render_bond_detail; }
	// Sets the scale of selected atoms
	void set_selection_scale(double f) { render_selection_scale = f; }
	// Return the scale of selected atoms
	GLdouble get_selection_scale() { return render_selection_scale; }
	// Return whether perspective viewing is enabled
	bool using_perspective() { return render_perspective; }
	// Sets perspective viewing on/off
	void set_perspective(bool b) { render_perspective = b; }
	// Set status of spotlight
	void set_spotlight_on(bool status) { spotlight_on = status; }
	// Return status of spotlight
	bool get_spotlight_on() { return spotlight_on; }
	// Set spotlight ambient component
	void set_spotlight(spotlight_component sc, int i, GLfloat value) { spotlight_components[sc][i] = value; }
	void set_spotlight(spotlight_component sc, GLfloat r, GLfloat g, GLfloat b) { spotlight_components[sc][0] = r; spotlight_components[sc][1] = g; spotlight_components[sc][2] = b; }
	// Return spotlight ambient component
	GLfloat *get_spotlight(spotlight_component sc) { return spotlight_components[sc]; }
	// Set atom colour scheme
	void set_colour_scheme(atom_colours ac) { colour_scheme = ac; }
	// Return atom colour scheme
	atom_colours get_colour_scheme() { return colour_scheme; }
	// Set number of segments in colour scale
	void set_scale_segments(int nsegments);
	// Get number of segments in colour scale
	int get_scale_segments() { return scale_segments; }
	// Set colour scale colours
	void set_scale_colours();
	// Copy colour scale segment into supplied array
	void get_scale_colour(int n, GLfloat *v);

	/*
	// GL Options
	*/
	private:
	// Bitvector for GL options
	int gloptions;
	// Shininess of 3D objects
	GLint gl_shininess;
	// Fog start and finish depths
	GLint gl_fog_near, gl_fog_far;
	// Near and far clipping planes for glPerspective() and glFrustum();
	GLdouble gl_clip_near, gl_clip_far;

	public:
	// Set the bit for the specified option (if it is not set already)
	void add_gl_option(gl_option go) { if (!(gloptions&go)) gloptions += go; }
	// Unsets the bit for the specified option (if it is not unset already)
	void remove_gl_option(gl_option go) { if (gloptions&go) gloptions -= go; }
	// Return whether a given option is set
	bool get_gl_option(gl_option go) { return (gloptions&go ? TRUE : FALSE); }
	// Sets the start depth of depth cueing
	void set_fog_near(int i) { gl_fog_near = i; }
	// Return depth cue start depth
	GLint get_fog_near() { return gl_fog_near; }
	// Sets the end depth of depth cueing
	void set_fog_far(int i) { gl_fog_far = i; }
	// Return depth cue end depth
	GLint get_fog_far() { return gl_fog_far; }
	// Return the Z depth of the near clipping plane
	GLdouble get_clip_near() { return gl_clip_near; }
	// Sets the shininess of GL objects
	void set_shininess(int n) { gl_shininess = n; }
	// Return the current shininess of GL objects
	GLint get_shininess() { return gl_shininess; }

	/*
	// Rendering - Colours
	*/
	private:
	// RGB colour values
	GLfloat colours[COL_NITEMS][4];
	// Numerical low limit corresponding to COL_ACSCHEMELO
	double colour_scheme_lo[AC_NITEMS];
	// Numerical high limit corresponding to COL_ACSCHEMELO
	double colour_scheme_hi[AC_NITEMS];

	public:
	// Set the specified colour to the integer RGB values supplied
	void set_colour(colour c, GLfloat r, GLfloat g, GLfloat b, GLfloat a);
	// Set the specified colour to the RGB values supplied
	void set_colour(colour c, double r, double g, double b, double a);
	// Return the specified colour
	GLfloat *get_colour(colour c) { return colours[c]; }
	// Return the low limit for the scheme specified
	double get_colour_scheme_lo(int i) { return colour_scheme_lo[i]; }
	// Sets the low limit for the scheme specified
	void set_colour_scheme_lo(int i, double d) { colour_scheme_lo[i] = d; }
	// Return the high limit for the scheme specified
	double get_colour_scheme_hi(int i) { return colour_scheme_hi[i]; }
	// Sets the high limit for the scheme specified
	void set_colour_scheme_hi(int i, double d) { colour_scheme_hi[i] = d; }

	/*
	// File Preferences (file_)
	*/
	private:
	// Recalculate bonding when model has loaded
	file_prefswitch file_bond_on_load;
	// Centre non-periodic models on load
	file_prefswitch file_centre_on_load;
	// Fold atomic positions after model load
	file_prefswitch file_fold_on_load;
	// Whether to apply symmetry operators to get crystal packing on load
	file_prefswitch file_pack_on_load;
	// Whether to load in all coordinate sets from a file
	bool file_load_all_coords;
	// Convert coordinates from Bohr to Angstrom on load
	bool file_coords_in_bohr;
	// Size limit (kbytes) for caching trajectory frames
	int file_cache_limit;
	// Type of name->Z mapping to use
	zmap_type file_zmap_type;

	public:
	// Sets whether to calculate bonding on model load
	void set_bond_on_load(file_prefswitch s) { file_bond_on_load = s; }
	// Whether bonding should be recalculated on model load
	file_prefswitch get_bond_on_load() { return file_bond_on_load; }
	// Sets whether to centre molecule on load
	void set_centre_on_load(file_prefswitch b) { file_centre_on_load = b; }
	// Whether molecule should be centred on model load
	file_prefswitch get_centre_on_load() { return file_centre_on_load; }
	// Sets whether to fold atomic positions after model load
	void set_fold_on_load(file_prefswitch b) { file_fold_on_load = b; }
	// Whether atoms should be folded after model load
	file_prefswitch get_fold_on_load() { return file_fold_on_load; }
	// Sets whether to apply symmetry operators (pack) on load
	void set_pack_on_load(file_prefswitch s) { file_pack_on_load = s; }
	// Whether atoms should be packed (with symmetry operations) after model load
	file_prefswitch get_pack_on_load() { return file_pack_on_load; }
	// Sets whether to load all coordinate sets on model load
	void set_load_all_coords(bool b) { file_load_all_coords = b; }
	// Whether all geometries in a non-trajectory file should be loaded
	bool load_all_coords() { return file_load_all_coords; }
	// Set the cache limit (in kb) for trajectory files
	void set_cache_limit(int i) { file_cache_limit = i; }
	// Return the cache limit for trajectory files
	int get_cache_limit() { return file_cache_limit; }
	// Sets the style of element conversion to use
	void set_zmapping(zmap_type i) { file_zmap_type = i; }
	// Return the style of element conversion in use
	zmap_type get_zmapping() { return file_zmap_type; }
	// Sets whether to convert coords from Bohr to Angstrom on load
	void set_coords_in_bohr(bool b) { file_coords_in_bohr = b; }
	// Whether coordinates should be converted from Bohr to Angstrom
	bool get_coords_in_bohr() { return file_coords_in_bohr; }

	/*
	// Builder Preferences (build_)
	*/
	private:
	// Bonding tolerance for automatic calculation
	double build_bond_tolerance;
	// Depth for drawing guide
	double build_draw_depth;
	// Spacing of grid on drawing guide
	double build_guide_spacing;
	// Extent (+- guide_spacing in xy plane) of drawing guide 
	int build_guide_extent;
	// Number of ticks between gridpoints of guide
	int build_guide_ticks;
	// Whether to show the drawing guide
	bool build_show_guide;
	// Geometry of the grid in the drawing guide
	guide_geometry build_guide_shape;
	// User-definable mouse button actions
	mouse_action mb_action[MB_NITEMS];
	// User-definable key modifier actions
	key_action keymod_action[MK_NITEMS];
	// Whether automatic patterns use bonds as a fingerprint
	bool patterns_use_bonds;

	public:
	// Return the bonding tolerance for automatic calculation
	double get_bond_tolerance() { return build_bond_tolerance; }
	// Sets the bonding tolerance
	void set_bond_tolerance(double v) { build_bond_tolerance = v; }
	// Sets the position of the drawing guide
	void set_draw_depth(double v) { build_draw_depth = v; }
	// Return the current position of the drawing guide
	double get_draw_depth() { return build_draw_depth; }
	// Sets the visibility of the drawing guide
	void set_guide_visible(bool b) { build_show_guide = b; }
	// Return whether the draw guide is visible
	bool is_guide_visible() { return build_show_guide; }
	// Sets the shape of the drawing guide
	void set_guide_shape(guide_geometry g) { build_guide_shape = g; }
	// Sets the action for the specified mouse button
	void set_mb_action(mouse_button mb, mouse_action ma) { mb_action[mb] = ma; }
	// Return the action associated with the specified mouse button
	mouse_action get_mb_action(mouse_button mb) { return mb_action[mb]; }
	// Sets the modifier key for the specified action
	void set_keymod_action(modifier_key mk, key_action ka) { keymod_action[mk] = ka; }
	// Return the action associated with the specified keymod button
	key_action get_keymod_action(modifier_key mk) { return keymod_action[mk]; }

	/*
	// Method Preferences (method_)
	*/
	private:
	// Main modelview update and energy output frequencies
	int method_mupdate, method_eupdate;
	// Maximum ring size in ring search algorithm
	int method_maxringsize;

	public:
	// Set the model update frequency
	void set_mupdate(int n) { method_mupdate = n; }
	// Return the model update frequency
	int get_mupdate() { return method_mupdate; }
	// Set the energy update frequency
	void set_eupdate(int n) { method_eupdate = n; }
	// Return the energy update frequency
	int get_eupdate() { return method_eupdate; }
	// Return whether to update the energy, given the cycle number
	bool update_energy(int n) { return (n%method_eupdate == 0 ? TRUE : FALSE); }
	// Return the maximum ring size allowed
	int get_maxringsize() { return method_maxringsize; }

	/*
	// Units and Conversion
	*/
	private:
	density_unit density_internal;
	// Internal energy units to use for forcefield storage, energy calculation etc.
	energy_unit energy_internal;
	// Conversion factors for energy units
	double energy_factors[EU_NITEMS];

	public:
	// Factor to convert from atomic units to internal units
	double elec_convert;
	// Sets the current internal energy unit
	void set_internal_units(energy_unit eu);
	// Return the working energy units
	energy_unit get_internal_units() { return energy_internal; }
	// Set the density unit to use
	void set_density_units(density_unit du) { density_internal = du; }
	// Return the current density units to use
	density_unit get_density_units() { return density_internal; }
	// Convert the units of the given quantity
	double convert_energy(double,energy_unit);

	/*
	// Expression (general parameters)
	*/
	private:
	// Method of electrostatic calculation
	elec_method method_electrostatics;
	// Whether to calculate VDW interactions
	bool method_calc_vdw;
	// Whether to calculate electrostatic interactions
	bool method_calc_elec;
	// Whether to calculate intramolecular interactions
	bool method_calc_intra;
	// Ewald sum extent
	vec3<int> ewald_kvec;
	// Ewald sum gaussian width and (for auto option) precision
	double ewald_alpha;
	// Ewald sum precision for automatic parameter estimation
	double ewald_precision;
	// Cutoff distances for VDW and electrostatics
	double vdw_cut, elec_cut;
	// Scale factor for VDW radii (used in disorder build)
	double vdw_radius_scale;
	// Where to get charges from for the model
	charge_source qsource;

	public:
	// Sets the electrostatic model to use in energy/force calculation
	void set_electrostatics(elec_method em) { method_electrostatics = em; }
	// Return the type of electrostatic treatment to use
	elec_method get_electrostatics() { return method_electrostatics; }
	// Sets whether to calculate intramolecular interactions
	void set_calc_intra(bool b) { method_calc_intra = b; }
	// Return whether to calculate intramolocular interactions
	bool calc_intra() { return method_calc_intra; }
	// Sets whether to calculate VDW interactions
	void set_calc_vdw(bool b) { method_calc_vdw = b; }
	// Return whether to calculate VDW interactions
	bool calc_vdw() { return method_calc_vdw; }
	// Sets whether to calculate electrostatic interactions
	void set_calc_elec(bool b) { method_calc_elec = b; }
	// Return whether to calculate electrostatic interactions
	bool calc_elec() { return method_calc_elec; }
	// Sets the Ewald k-vector extents
	void set_ewald_kvec(int a, int b, int c) { ewald_kvec.set(a,b,c); }
	void set_ewald_kvec(vec3<int> v) { ewald_kvec = v; }
	// Return the Ewald k-vector extents
	vec3<int> get_ewald_kvec() { return ewald_kvec; }
	// Sets the Ewald precision
	void set_ewald_precision(double d) { ewald_precision = d; }
	// Return the Ewald precision
	double get_ewald_precision() { return ewald_precision; }
	// Set the Gaussian width to use in the Ewald sum
	void set_ewald_alpha(double d) { ewald_alpha = d; }
	// Return the Ewald alpha value
	double get_ewald_alpha() { return ewald_alpha; }
	// Set the short-range and electrostatic cutoffs
	void set_cutoffs(double ,double);
	void ewald_estimate_parameters(unitcell*);
	// Flag to indicate validity of automatic Ewald params (invalidated on cell change)
	bool valid_ewaldauto;
	// Sets the VDW cutoff radius to use
	void set_vdw_cutoff(double d) { vdw_cut = d; }
	// Return the VDW cutoff radius
	double get_vdw_cutoff() { return vdw_cut; }
	// Sets the electrostatic cutoff radius to use
	void set_elec_cutoff(double d) { elec_cut = d; }
	// Return the electrostatic cutoff radius
	double get_elec_cutoff() { return elec_cut; }
	// Sets the vdw radius scaling factor
	void set_vdw_radius_scale(double d) { vdw_radius_scale = d; }
	// Return the VDW radius scaling factor
	double get_vdw_radius_scale() { return vdw_radius_scale; }
	// Set the charge source for the model
	void set_chargesource(charge_source cs) { qsource = cs; }
	// Get the charge source for the model
	charge_source get_chargesource() { return qsource; }

	/*
	// Undo levels
	*/
	private:
	// Maximum number of undo levels (-1 for unlimited)
	int maxundo;

	public:
	// Set the maximum number of undo levels allowed
	void set_maxundo(int n) { maxundo = n; }
	// Return the maximum number of undo levels allowed
	int get_maxundo() { return maxundo; }
};

extern prefs_data prefs;

#endif

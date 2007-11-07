/*
	*** Preferences storage
	*** src/base/prefs.cpp
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

#include "classes/forcefield.h"
#include "file/parse.h"
#include "base/master.h"
#include "base/elements.h"
#include "base/prefs.h"
#include "base/sysfunc.h"

prefs_data prefs;

// GL Options
const char *GO_keywords[GO_NITEMS] = { "fog", "linealias", "polyalias", "backcull" };
gl_option GO_from_text(const char *s)
	{ return (gl_option) pow(2,enum_search("GL option",GO_NITEMS,GO_keywords,s)); }

// Mouse buttons
const char *MB_keywords[MB_NITEMS] = { "left", "middle", "right", "wheel" };
mouse_button MB_from_text(const char *s)
	{ return (mouse_button) enum_search("mouse button",MB_NITEMS,MB_keywords,s); }
const char *text_from_MB(mouse_button i)
	{ return MB_keywords[i]; }

// Mouse actions
const char *MA_strings[MA_NITEMS] = { "None", "Rotate", "Translate", "Interact", "Zoom", "Z-Rotate" };
mouse_action MA_from_text(const char *s)
	{ return (mouse_action) enum_search("mouse action",MA_NITEMS,MA_strings,s); }
const char *text_from_MA(mouse_action i)
	{ return MA_strings[i]; }
const char **get_MA_strings()
	{ return MA_strings; }

// Key modifiers
const char *MK_strings[MK_NITEMS] = { "Shift", "Ctrl", "Alt" };
const char **get_MK_strings()
	{ return MK_strings; }
modifier_key MK_from_text(const char *s)
	{ return (modifier_key) enum_search("modifier key",MK_NITEMS,MK_strings,s); }
const char *KA_strings[KA_NITEMS] = { "None", "Transform", "ZRotate" };
const char **get_KA_strings()
	{ return KA_strings; }
key_action KA_from_text(const char *s)
	{ return (key_action) enum_search("key action",KA_NITEMS,KA_strings,s); }

// Colours
const char *COL_strings[COL_NITEMS] = { "General pen colour", "Background", "Atom Colour Scheme - Lo",
	"Atom Colour Scheme = Mid", "Atom Colour Scheme = Hi", "Specular reflection" };
const char *COL_keywords[COL_NITEMS] = { "pen", "bg", "schemelo", "schememid", "schemehi", "specular" };
const char *text_from_COL(colour i)
	{ return COL_strings[i]; }
colour COL_from_text(const char *s)
	{ return (colour) enum_search("colour",COL_NITEMS,COL_keywords,s); }

// Density calculation units
const char *DU_strings[DU_NITEMS] = { "g / cm**3", "atoms / A**3" };
const char *DU_keywords[DU_NITEMS] = { "gpercm", "atomsperang" };
const char *text_from_DU(density_unit i)
	{ return DU_strings[i]; }
density_unit DU_from_text(const char *s)
	{ return (density_unit) enum_search("density units",DU_NITEMS,DU_keywords,s); }

// Energy Units
const char *EU_strings[EU_NITEMS] = { "J/mol", "kJ/mol", "cal/mol", "kcal/mol", "eV/mol", "Ha/mol" };
const char *EU_keywords[EU_NITEMS] = { "j", "kj", "cal", "kcal", "ev", "ha" };
const char *text_from_EU(energy_unit i)
	{ return EU_strings[i]; }
energy_unit EU_from_text(const char *s)
	{ return (energy_unit) enum_search("energy units",EU_NITEMS,EU_keywords,s); }

// ZMapping types
const char *ZM_keywords[ZM_NITEMS] = { "Alpha", "Name", "Numeric", "FF", "Auto" };
zmap_type ZM_from_text(const char *s)
	{ return (zmap_type) enum_search("element mapping style",ZM_NITEMS,ZM_keywords,s); }
const char **get_ZM_keywords()
	{ return ZM_keywords; }

// View Objects
const char *VO_keywords[VO_NITEMS] = { "atoms", "cell", "cellaxes", "cellrepeat", "forcearrows", "globe", "labels", "measurements", "regions" };
view_object VO_from_text(const char *s)
	{ return (view_object) enum_search("view object",VO_NITEMS,VO_keywords,s); }


// Guide Geometries
const char *GG_strings[GG_NITEMS] = { "Square", "Hexagonal" };
const char **get_GG_strings()
	{ return GG_strings; }

// Constructor
prefs_data::prefs_data()
{
	// Rendering - Style
	colour_scheme = AC_ELEMENT;
	scale_segments = 2;
	scale_colours = NULL;
	set_scale_colours();
	render_atom_size[DS_STICK] = 0.1;      // Only used as a selection radius
	render_atom_size[DS_TUBE] = 0.095;
	render_atom_size[DS_SPHERE] = 0.35;
	render_atom_size[DS_SCALED] = 1.0;     // Used as a general scaling factor for all atoms
	render_tube_size = 0.1;
	render_selection_scale = 1.5;
	render_selection_scale_current = 1.5;
	render_globe_size = 75;
	render_atom_detail = 20;
	render_bond_detail = 6;
	render_perspective = TRUE;
	render_fov = 20.0;
	spotlight_on = TRUE;
	spotlight_components[SL_AMBIENT][0] = (GLint) (0.0 * INT_MAX);
	spotlight_components[SL_AMBIENT][1] = (GLint) (0.0 * INT_MAX);
	spotlight_components[SL_AMBIENT][2] = (GLint) (0.0 * INT_MAX);
	spotlight_components[SL_AMBIENT][3] = (GLint) (1.0 * INT_MAX);
	spotlight_components[SL_DIFFUSE][0] = (GLint) (0.8 * INT_MAX);
	spotlight_components[SL_DIFFUSE][1] = (GLint) (0.8 * INT_MAX);
	spotlight_components[SL_DIFFUSE][2] = (GLint) (0.8 * INT_MAX);
	spotlight_components[SL_DIFFUSE][3] = (GLint) (1.0 * INT_MAX);
	spotlight_components[SL_SPECULAR][0] = (GLint) (0.7 * INT_MAX);
	spotlight_components[SL_SPECULAR][1] = (GLint) (0.7 * INT_MAX);
	spotlight_components[SL_SPECULAR][2] = (GLint) (0.7 * INT_MAX);
	spotlight_components[SL_SPECULAR][3] = (GLint) (1.0 * INT_MAX);
	spotlight_components[SL_POSITION][0] = 1;
	spotlight_components[SL_POSITION][1] = 1;
	spotlight_components[SL_POSITION][2] = 1;
	spotlight_components[SL_POSITION][3] = 0;

	// GL Options
	gloptions = 0;
	gl_shininess = 10;
	gl_clip_near = 1.0;
	gl_clip_far = 1000.0;
	gl_fog_near = 1;
	gl_fog_far = 200;

	// Rendering - Objects
	render_label_scale = 4.0;
	render_objects[VO_ATOMS] = TRUE;
	render_objects[VO_LABELS] = TRUE;
	render_objects[VO_MEASUREMENTS] = TRUE;
	render_objects[VO_GLOBE] = TRUE;
	render_objects[VO_CELL] = TRUE;
	render_objects[VO_CELLAXES] = TRUE;
	render_objects[VO_CELLREPEAT] = FALSE;
	render_objects[VO_REGIONS] = TRUE;
	render_objects[VO_FORCEARROWS] = FALSE;
	render_objects[VO_SURFACES] = TRUE;
	render_style = DS_STICK;
	render_static_style = DS_STICK;
	render_dynamic_style = DS_STICK;

	// Build
	build_show_guide = FALSE;
	build_bond_tolerance = 1.1;
	build_draw_depth = 0.0;
	build_guide_spacing = 1.0;
	build_guide_ticks = 5;
	build_guide_extent = 10;
	build_guide_shape = GG_SQUARE;

	// Input
	mb_action[MB_LEFT] = MA_INTERACT;
	mb_action[MB_MIDDLE] = MA_VIEWTRANSLATE;
	mb_action[MB_RIGHT] = MA_VIEWROTATE;
	mb_action[MB_WHEEL] = MA_VIEWZOOM;
	keymod_action[MK_SHIFT] = KA_ZROTATE;
	keymod_action[MK_CTRL] = KA_MANIPULATE;
	keymod_action[MK_ALT] = KA_NONE;

	// Colours
	set_colour(COL_SPECREFLECT, 1.0, 0.9, 0.75, 1.0);
	set_colour(COL_PEN, 0.0, 0.0, 0.0, 1.0);
	set_colour(COL_BG, 1.0, 1.0, 1.0, 1.0);
	set_colour(COL_SCHEMELO, 1.0, 0.0, 0.0, 1.0);
	set_colour(COL_SCHEMEMID, 0.7, 0.7, 0.7, 1.0);
	set_colour(COL_SCHEMEHI, 0.0, 0.0, 1.0, 1.0);
	colour_scheme_lo[AC_ELEMENT] = 0.0;
	colour_scheme_lo[AC_CHARGE] = -1.0;
	colour_scheme_lo[AC_VELOCITY] = 0.0;
	colour_scheme_lo[AC_FORCE] = 0.0;
	colour_scheme_hi[AC_ELEMENT] = 0.0;
	colour_scheme_hi[AC_CHARGE] = 1.0;
	colour_scheme_hi[AC_VELOCITY] = 200.0;
	colour_scheme_hi[AC_FORCE] = 10000.0;

	// Methods
	method_mupdate = 5;
	method_eupdate = 1;
	method_maxringsize = 6;

	// File
	file_bond_on_load = PS_ASFILTER;
	file_fold_on_load = PS_ASFILTER;
	file_centre_on_load = PS_ASFILTER;
	file_pack_on_load = PS_ASFILTER;
	file_load_all_coords = TRUE;
	file_cache_limit = 1024;
	file_zmap_type = ZM_AUTO;
	file_coords_in_bohr = FALSE;

	// Units
	energy_factors[EU_J] = 1.0;
	energy_factors[EU_KJ] = 1000.0;
	energy_factors[EU_CAL] = 4.184;
	energy_factors[EU_KCAL] = 4184.0;
	energy_factors[EU_EV] = 96485.14925;
	energy_factors[EU_HARTREE] = 2625494.616;
	set_internal_units(EU_KJ);
	density_internal = DU_GPERCM;

	// Energy
	method_electrostatics = EM_EWALDAUTO;
	method_calc_intra = TRUE;
	method_calc_vdw = TRUE;
	method_calc_elec = FALSE;
	ewald_kvec.zero();
	ewald_alpha = 0.5;
	ewald_precision = 5.0E-6;
	vdw_cut = 10.0;
	elec_cut = 10.0;
	vdw_radius_scale = 1.0;

	// GUI
	showgui = TRUE;
	gui_msg_font = "Adobe Courier 10";
}
	
void prefs_data::load(const char *filename)
{
	// Read in a user preferences file
	dbg_begin(DM_CALLS,"prefs_data::load");
	int success;
	// Open the file
	ifstream prefsfile(filename,ios::in);
	if (!prefsfile.good())
	{
		printf("Couldn't open preferences file in '%s'\n",filename);
		prefsfile.close();
		dbg_end(DM_CALLS,"prefs_data::load");
		return;
	}
	// Create script structure and initialise
	script prefscript;
	prefscript.commands.clear();
	while (!prefsfile.eof())
	{
		success = parser.get_args_delim(&prefsfile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(DM_NONE,"prefs::load - Error reading file.\n");
			break;
		}
		else if (success == -1) break;
		// Add script command
		if (!prefscript.cache_command()) break;
	}
	// Check the flowstack - it should contain just the BC_ROOTNODE branch
	if (prefscript.commands.get_topbranch_type() != BC_ROOTNODE)
	{
		int i = prefscript.commands.get_branchstack_size() - 1;
		msg(DM_NONE,"Loops in prefs? : %i block%s not been terminated.>>>\n",i ,(i == 1 ? " has" : "s have"));
		dbg_end(DM_CALLS,"prefs::load");
		return;
	}
	dbg_end(DM_CALLS,"prefs::load");
	prefscript.run();
}

double prefs_data::screenradius(atom *i)
{
	// Simple routine that returns the screen 'radius' of the supplied atom, which depends on the drawing style
	draw_style dstyle;
	double radius;
	render_static_style == DS_INDIVIDUAL ? dstyle = i->get_style() : dstyle = render_static_style;
	return (dstyle == DS_SCALED) ? (elements.radius(i) * prefs.render_atom_size[DS_SCALED]) : prefs.render_atom_size[dstyle];
}

/*
// Colours
*/

void prefs_data::set_colour(colour c, GLint r, GLint g, GLint b, GLint a)
{
	colours[c][0] = r;
	colours[c][1] = g;
	colours[c][2] = b;
	colours[c][3] = a;
}

void prefs_data::set_colour(colour c, double r, double g, double b, double a)
{
	colours[c][0] = (GLint) (r * INT_MAX);
	colours[c][1] = (GLint) (g * INT_MAX);
	colours[c][2] = (GLint) (b * INT_MAX);
	colours[c][3] = (GLint) (a * INT_MAX);
}

/*
// Energy Units
*/

// Set the internal energy units to use
void prefs_data::set_internal_units(energy_unit eu)
{
	// Reconvert any forcefields already loaded so that they are in the new energy units
	energy_unit euold = energy_internal;
	energy_internal = eu;
	for (forcefield *ff = master.get_ffs(); ff != NULL; ff = ff->next) ff->convert_parameters(euold);
	// Calculate Electrostatic conversion factor
	// COULCONVERT is stored in J/mol. Use this to calculate new elec_convert
	elec_convert = COULCONVERT / energy_factors[energy_internal];
}

// Convert energy from specified unit to current internal unit
double prefs_data::convert_energy(double energy, energy_unit from)
{
	static double result;
	// Convert supplied value to units of J/mol
	result = energy * energy_factors[from];
	// Then, convert to internal units
	result /= energy_factors[energy_internal];
	return result;
}

// Set number of segments in colour scale
void prefs_data::set_scale_segments(int nsegments)
{
	scale_segments = nsegments;
	set_scale_colours();
}

// Get colour scale segment
void prefs_data::get_scale_colour(int n, GLint *v)
{
	// Check range of requested colour
	if ((n < 0) || (n > (3+2*scale_segments))) 
	{
		printf("prefs::get_scale_colour - Requested colour is out of range.\n");
		v[0] = scale_colours[0][0];
		v[1] = scale_colours[0][1];
		v[2] = scale_colours[0][2];
		v[3] = scale_colours[0][3];
	}
	else
	{
		v[0] = scale_colours[n][0];
		v[1] = scale_colours[n][1];
		v[2] = scale_colours[n][2];
		v[3] = scale_colours[n][3];
	}
}

// Set colours in colour scale
void prefs_data::set_scale_colours()
{
	static int lastnsegments = -1, n;
	static GLint newcol[4];
	static double delta;
	// Check current value of scale_segments against last value. If different, recreate array
	if (lastnsegments != scale_segments)
	{
		if (scale_colours != NULL)
		{
			for (n=0; n<(3+scale_segments*2); n++) delete[] scale_colours[n];
			delete[] scale_colours;
		}
		// Create new array
		scale_colours = new GLint*[3+scale_segments*2];
		for (n=0; n<(3+scale_segments*2); n++) scale_colours[n] = new GLint[4];
		lastnsegments = scale_segments;
	}
	// Set values of lo, mid, and hi colours.
	scale_colours[0][0] = colours[COL_SCHEMELO][0];
	scale_colours[0][1] = colours[COL_SCHEMELO][1];
	scale_colours[0][2] = colours[COL_SCHEMELO][2];
	scale_colours[0][3] = colours[COL_SCHEMELO][3];
	scale_colours[scale_segments+1][0] = colours[COL_SCHEMEMID][0];
	scale_colours[scale_segments+1][1] = colours[COL_SCHEMEMID][1];
	scale_colours[scale_segments+1][2] = colours[COL_SCHEMEMID][2];
	scale_colours[scale_segments+1][3] = colours[COL_SCHEMEMID][3];
	scale_colours[scale_segments*2+2][0] = colours[COL_SCHEMEHI][0];
	scale_colours[scale_segments*2+2][1] = colours[COL_SCHEMEHI][1];
	scale_colours[scale_segments*2+2][2] = colours[COL_SCHEMEHI][2];
	scale_colours[scale_segments*2+2][3] = colours[COL_SCHEMEHI][3];
	// Interpolate between the lo and mid points.
	delta = 1.0 / (scale_segments + 1);	
	for (n=0; n<scale_segments; n++)
	{
		scale_colours[n+1][0] = (GLint) (scale_colours[0][0] + (colours[COL_SCHEMEMID][0]-scale_colours[0][0]) * n * delta);
		scale_colours[n+1][1] = (GLint) (scale_colours[0][1] + (colours[COL_SCHEMEMID][1]-scale_colours[0][1]) * n * delta);
		scale_colours[n+1][2] = (GLint) (scale_colours[0][2] + (colours[COL_SCHEMEMID][2]-scale_colours[0][2]) * n * delta);
		scale_colours[n+1][3] = (GLint) (scale_colours[0][3] + (colours[COL_SCHEMEMID][3]-scale_colours[0][3]) * n * delta);
	}
}

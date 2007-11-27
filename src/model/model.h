/*
	*** Model definition
	*** src/model/model.h
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

#ifndef H_MODEL_H
#define H_MODEL_H

#include "templates/vector3.h"
#include "classes/energystore.h"
#include "classes/cell.h"
#include "classes/spacegroup.h"
#include "classes/measurement.h"
#include "classes/glyph.h"
#include "classes/site.h"
#include "classes/undostate.h"
#include "base/prefs.h"
#include "methods/quantity.h"
#ifdef IS_MAC
	#include "OpenGL/gl.h"
#else
	#include "GL/gl.h"
#endif

// Forward Declarations
class canvas_master;
class forcefield;
class bond;
class constraint;
class undostate;
class pattern;
class filter;
class energystore;
class atomaddress;

// Model
class model
{
	public:
	// Constructor / Destructor
	model();
	~model();
	// List pointers
	model *prev, *next;

	/*
	// Model
	*/
	private:
	// Integer 'logs' of model changes
	// LOG_STRUCTURE : create/destroy atoms/bonds, change elements
	// LOG_COORDS    : atomic coordinates
	// LOG_VISUAL    : visual changes that require re-rendering
	// LOG_SELECTION : atom selection
	// LOG_CAMERA    : view (mainly used to flag reprojection)
	// LOG_TOTAL     : sum of all changes
	int logs[LOG_NITEMS];
	// Log point of the last save / point on load
	int save_point;
	// Log point of the last project_all() (LOG_COORDS+LOG_CAMERA)
	int projection_point;
	// Total mass of model
	double mass;
	// Density of model (if periodic)
	double density;
	// Name of model
	dnchar name;
	// Format of model when loaded / last saved
	filter *filefilter;
	// Filename of model when loaded / last saved
	dnchar filename;

	public:
	// Increment specified log point of the model
	void log_change(change_log);
	// Return the log quantity specified
	int get_log(change_log cl) { return logs[cl]; }
	// Reset all logs to zero
	void reset_logs() { for (int i=0; i<LOG_NITEMS; i++) logs[i] = 0; }
	// Set the save point log for the model
	void update_save_point() { save_point = logs[LOG_STRUCTURE] + logs[LOG_COORDS]; }
	// Return if the model has been modified since last being saved
	bool is_modified() { return (save_point == (logs[LOG_STRUCTURE] + logs[LOG_COORDS]) ? FALSE : TRUE); }
	// Sets the filename of the model
	void set_filename(const char *s) { filename = s; }
	// Return the stored filename of the model
	const char *get_filename() { return filename.get(); }
	// Sets the file filter of the model
	void set_filter(filter *f) { filefilter = f; }
	// Return the stored file filter of the model
	filter *get_filter() { return filefilter; }
	// Sets the name of the model
	void set_name(const char *s) { name = s; }
	// Return the name of the model
	const char *get_name() { return name.get(); }
	// Return the mass of the molecule
	double get_mass() { return mass; }
	// Return the density of the model
	double get_density() { return density; }
	// Clear all data in model
	void clear();
	// Calculate the total mass of the model
	void calculate_mass();
	// Calculate the density of the model
	void calculate_density();
	// Print information about the model (inc atoms)
	void print();
	// Copy all information from another model
	void copy(model*);
	// Copy all atom data from specified model
	void copy_atom_data(model*, int);
	// Copy range of atom data from specified model
	void copy_atom_data(model*, int, int, int);

	/*
	// Atoms
	*/
	private:
	// Atoms in model
	list<atom> atoms;
	// Delete the specified atom
	void remove_atom(atom*);
	// Array of static atom pointers (for energy calculation etc.)
	atom **staticatoms;
	// Log point for static atom pointer list
	int staticatoms_point;
	
	public:
	// Create a new atom
	atom *add_atom(int el, vec3<double> r);
	// Create copy of supplied atom
	atom *add_copy(atom *source);
	// Create copy of supplied atom at the specified position
	atom *add_copy(atom *after, atom *source);
	// Return the start of the atom list
	atom *get_atoms() { return atoms.first(); }
	// Return the number of atoms in the model
	int get_natoms() { return atoms.size(); }
	// Delete specified atom
	void delete_atom(atom *target);
	// Delete all atoms in the model
	void clear_atoms();
	// Perform alchemy on an atom 
	void transmute_atom(atom *target, int element);
	// Renumber atoms in the model
	void renumber_atoms(atom *from = NULL);
	// Reset tempi values of all atoms
	void reset_tempi(int);
	// Return pointer to the atom with the specified id
	atom *find_atom(int);
	// Return the (first) atom with matching tempi
	atom *find_atom_by_tempi(int);
	// Move selected atoms one place 'up' in the list
	void shift_selection_up();
	// Move selected atoms one place 'down' in the list
	void shift_selection_down();
	// Move selected atoms to start of the list
	void move_selection_to_start();
	// Move selection to end of the list
	void move_selection_to_end();
	// Return (and autocreate if necessary) the static atoms array
	atom **get_staticatoms();
	// Set visibility of specified atom
	void set_hidden(atom *i, bool hidden);
	// Prints out the coordinates of the atoms in the model
	void print_coords();

	/*
	// Unit Cell
	*/
	private:
	// Spacegroup of the model (if any)
	int spgrp;
	// Setting for spacegroup (if any)
	int spgrpsetting;
	// List of symmetry generators for crystal structure (read in from file)
	list<symmop> symmops;
	// Cell definition (also contains reciprocal cell definition)
	unitcell cell;

	public:
	// Return pointer to unit cell structure
	unitcell *get_cell() { return &cell; }
	// Return type of unit cell
	cell_type get_celltype() { return cell.get_type(); }
	// Return volume of cell
	double get_volume() { return cell.get_volume(); }
	// Return cell axes (untransposed)
	mat3<double> get_cellaxes() { return cell.get_axes(); }
	// Return cell origin
	vec3<double> get_cellorigin() { return cell.get_origin(); }
	// Set cell (vectors)
	void set_cell(vec3<double> lengths, vec3<double> angles);
	// Set cell (axes)
	void set_cell(mat3<double> axes);
	// Remove cell definition
	void remove_cell();
	// Fold all atoms into the cell
	void fold_all_atoms();
	// Sets the spacegroup of the model
	void set_spacegroup(int i) { spgrp = i; }
	// Sets the spacegroup setting
	void set_spacegroupsetting(int i) { spgrpsetting = i; }
	// Return the spacegroup of the model
	int get_spacegroup() { return spgrp; }
	// Adds a symmetry operator to the list
	symmop *add_symmop() { return symmops.add(); }
	// Return the first symmop in the list
	symmop *get_symmops() { return symmops.first(); }
	// Apply the given symmetry operator to the atoms in the model
	void apply_symmop(symmop*,atom*);
	// Apply the list of symmetry operators in the model
	void apply_model_symmops(atom*);
	// Apply the symmetry operators listed in the model's spacegroup
	void apply_spacegroup_symmops(atom*);
	// Fold all molecules into the cell
	void fold_all_molecules();
	// Replicate cell to create supercell
	void replicate_cell(const vec3<double>&, const vec3<double>&);
	// Scale cell and contents
	void scale_cell(const vec3<double>&);

	/*
	// Bonding
	*/
	public:
	// Augment specified bond
	void augment_bond(bond *b, int change);
	// Augment bond between supplied atoms
	void augment_bond(atom *i, atom *j, int change);
	// Add bond of specified type between atoms
	void bond_atoms(atom *i, atom *j, bond_type bt);
	// Add bond of specified type between atoms (by id)
	void bond_atoms(int ii, int jj, bond_type bt);
	// Delete bond between specified atoms
	void unbond_atoms(atom *i, atom *j, bond *b = NULL);
	// Change type of specified bond
	void change_bond(bond *b, bond_type bt);
	// Clear all bonding in model
	void clear_bonding();
	// Calculate bonding in the model
	void calculate_bonding();
	// Augment bonding in the model
	void augment_bonding();
	// Calculate bonding in current atom selection
	void selection_calculate_bonding();
	// Bond all atom pairs in current atom selection
	void selection_bond_all();
	// Clear bonding in current atom selection
	void selection_clear_bonding();

	/*
	// Selection
	*/
	private:
	// Number of selected atoms
	int nselected;

	public:
	// Select the specified atom
	void select_atom(atom*);
	// Deselect the specified atom
	void deselect_atom(atom*);
	// Toggle the selection state of the atom
	void selection_toggle(atom*);
	// Select all atoms
	void select_all();
	// Select no atoms
	void select_none();
	// Return the number of selected atoms
	int get_nselected() { return nselected; }
	// Invert current atom selection
	void selection_invert();
	// Delete current atom selection
	void selection_delete();
	// Expand current atom selection by one bond
	void selection_expand();
	// Return the atom at the clicked screen coordinates (if any)
	atom *atom_on_screen(double, double);
	// TODO Make private
	void select_pattern(pattern*);
	// Select all atoms within the rectangular boundary specified
	void select_box(double, double, double, double);
	// Select all atoms connected by an path from the specified atom
	void select_tree(atom*);
	// Select all atoms of the same element as the specified atom
	void select_element(atom*);
	// Select all atoms of the same element as the atom with the specified id
	void select_element(int);
	// Select all atoms within cutoff of specified atom
	void select_radial(atom*, double);
	// Return the first selected atom in the model (if any)
	atom *get_first_selected();
	// Detect and select overlapping atoms
	void select_overlaps(double tolerance);

	/*
	// Basic View
	*/
	private:
	// Camera rotation
	double camrot;
	// Camera, model, and view (cam*rot*cell) matrices associated with the model
	mat4<double> camera, rotation, view;
	// Inverse of the view matrix
	mat4<double> view_inverse;
	// Camera position
	vec3<double> camr;
	// Size of view for orthographic projection
	double ortho_size;

	public:
	// Pre-generated display list for atoms
	GLuint displaylist;
	// Project the specified world coordinates into 2D screen coords
	vec4<double> &world_to_screen(const vec3<double>&);
	// Called when, e.g. the camera position or view rotation has changed
	void calculate_viewmatrix();
	// Return the GL-compatible array from the ModelMAT structure
	void get_rotation_matrix(double *m) { rotation.get_column_major(m); }
	// Return the GL-compatible array from the ModelMAT structure
	void get_camera_matrix(double *m) { camera.get_column_major(m); }
	// Return the current camera z-rotation
	double get_camrot() { return camrot; }
	// Rotate the model about the x and y axes
	void rotate(double, double);
	// Spin the model about the z axis
	void zrotate(double);
	// Adjust the position of the camera
	void adjust_camera(double, double, double, double);
	// Adjust the position of the camera
	void adjust_camera(const vec3<double> &v, double r) { adjust_camera(v.x,v.y,v.z,r); }
	// Adjusts the orthographic size (zoom)
	void adjust_ortho_size(double);
	// Return the size of the orthographic projection
	double get_ortho_size() { return ortho_size; }
	// Reset the camera to show the entire model
	void reset_camera(const vec3<double>&);
	// Reset modelview matrix and camera position
	void reset_view();
	// Project the model coordinates of the atom into local and 2D coordinates
	void project_atom(atom*);
	// Project the model coordinates of all atoms
	void project_all();
	// Project the model coordinates of selected atoms
	void project_selection();

	/*
	// Labelling
	*/
	public:
	// Clear all atom labelling
	void clear_atom_labels();
	// Clear all atom labelling from the current selection
	void selection_clear_atom_labels();
	// Clear specified atom labelling from the current selection
	void selection_clear_atom_labels(atom_label);
	// Set the specified label for all atoms
	void set_atom_labels(atom_label);
	// Set the specified label for all atoms currently selected
	void selection_set_atom_labels(atom_label);
	// Set the visibility property for all selected atoms
	void selection_set_hidden(bool);
	// Sets the 'fixed' variable of all selected atoms to TRUE
	void selection_set_fixed();
	// Sets the 'fixed' variable of all selected atoms to FALSE
	void selection_set_free();

	/*
	// Forcefield
	*/
	private:
	// Forcefield associated with this model
	forcefield *ff;

	public:
	// Set the model to use the specified forcefield
	void set_ff(forcefield*);
	// Return the forcefield used by the model
	forcefield *get_ff() { return ff; }
	// Assign charges according to prefs QS
	void assign_charges(charge_source);
	// Reset all model charges to zero
	void clear_charges();

	/*
	// Typing
	*/
	public:
	// Determine hybridicities of atoms
	void describe_atoms();
	// Assign forcefield types to all atoms
	bool type_all();
	// Remove forcefield types from all atoms
	void remove_typing();

	/*
	// Energy / Forces
	*/
	private:
	// Atom changeid at which the expression was/is valid
	int expression_point;

	public:
	// Storage for energy
	energystore energy;
	// Create total energy function shell for the model
	bool create_expression();
	// Return whether the expression is valid
	bool expression_is_valid() { return (expression_point == logs[LOG_STRUCTURE] ? TRUE : FALSE); }
	// Manually invalidates the expression
	void invalidate_expression() { expression_point --; }
	// Manually sets the valid_expression flag
	void force_valid_expression() { expression_point = logs[LOG_STRUCTURE]; }
	// Generate parameters for total energy function
	void fill_expression(int);
	// Calculate (and return) the total energy of the specified model configuration
	double total_energy(model*);
	// Calculate forces in the specified model configuration
	void calculate_forces(model*);
	// Prints out atomic forces
	void print_forces();
	// Calculate RMS of current forces
	double calculate_rms_force();
	// Normalise forces (make largest component equal to specified value)
	void normalise_forces(double norm);
	// Zero forces on all atoms
	void zero_forces();
	// Zero forces on all atoms that have their 'fixed' property set to true
	void zero_forces_fixed();

	/*
	// Pattern / Molecule
	*/
	private:
	// Pattern nodes for the model
	list<pattern> patterns;
	// Flag to indicate a valid pattern for the model
	int patterns_point;

	public:
	// Create a new pattern node (nmols,natoms,name)
	pattern *add_pattern(int, int, const char*);
	// Cut the pattern from the list
	void cut_pattern(pattern*);
	// Own the specified pattern (bool = whether to set ownermodel)
	void own_pattern(pattern*, bool);
	// Number of nodes in pattern
	int get_npatterns() { return patterns.size(); }
	// Return the first pattern node of the model
	pattern *get_patterns() { return patterns.first(); }
	// Return the pattern with the ID specified
	pattern *get_pattern(int id);
	// Return the pattern that the specified atom is in
	pattern *get_pattern(atom *i);
	// Return the last pattern node of the model
	pattern *get_last_pattern() { return patterns.last(); }
	// Find pattern by name
	pattern *find_pattern(const char*);
	// Autocreate patterns for the model
	bool autocreate_patterns();
	// Create representative molecules for patterns
	void create_pattern_molecules();
	// Clear the current pattern definition
	void clear_patterns();
	// Return whether the patterns are valid
	bool patterns_are_valid() { return (patterns_point == logs[LOG_STRUCTURE] ? TRUE : FALSE); }
	// Sets the 'fixed' property of all current patterns
	void set_patterns_fixed(int);
	int pattern_maxmols();
	// Generate array of public pointers to pattern nodes (in **plist)
	int make_plist();
	// Temporary array of pattern pointers (convenience variables)
	pattern **plist;
	// Calculates the atom locality of the supplied atom
	atomaddress *locate_atom(atom*);
	// Creates a string of the element symbols in the selection
	void selection_get_atom_fingerprint(dnchar&);
	// Creates a characteristic string of the bonds in the selection
	void selection_get_bond_fingerprint(dnchar&);
	// Charge the pattern atom across the model
	void charge_pattern_atom(pattern*, int, double);
	// Calculate bonding restricted to patterns
	void pattern_calculate_bonding();
	// Position specified molecule within pattern
	void position_molecule(pattern*, int, const vec3<double>&);
	// Translate specified molecule within pattern
	void translate_molecule(pattern*, int, const vec3<double>&);
	// Rotate specified molecule within pattern
	void rotate_molecule(pattern*, int, double, double);
	// Set the hidden flag on atoms of the specified molecule
	void hide_molecule(pattern*, int, bool);

	/*
	// Model Building
	*/
	private:
	// Iteratively add hydrogens to the specified atom in the desired general geometry
	void add_hydrogens(atom *target, int nhydrogen, hadd_geom geometry);

	public:
	// Adds hydrogens to satisfy the bond order requirements of atoms in the model
	void hydrogen_satisfy();

	/*
	// Geometry (using staticatoms[])
	*/
	public:
	// Calculate distance
	double distance(int, int);
	double distance(atom *i, atom *j) { return cell.distance(i,j); }
	// Calculate angle
	double angle(int, int, int);
	double angle(atom *i, atom *j, atom *k) { return cell.angle(i,j,k); }
	// Calculate torsion
	double torsion(int, int, int, int);
	double torsion(atom *i, atom *j, atom *k, atom *l) { return cell.torsion(i,j,k,l); }

	/*
	// Transformations
	*/
	private:
	// Length scale to use for world translations through GUI
	double translatescale;

	public:
	// Prepare for atom manipulation
	void prepare_transform();
	// Return the translation scale
	double get_translatescale() { return translatescale; }
	// Finalize atom transform
	void finalize_transform();
	// Rotate the atom selection
	void rotate_selection_world(double, double);
	// Spin the atom selection
	void manip_rotate_zaxis(double);
	// Puts the system's centre of geometry at 0,0,0
	void centre();
	// Translate selection by the vector specified
	void translate_selection_local(const vec3<double>&);
	// Translate selection by the vector specified (in world coordinates)
	void translate_selection_world(const vec3<double>&);
	// Rotate selection about specified vector
	void rotate_selection_vector(vec3<double>, vec3<double>, double);
	// Mirror selection about specified axis
	void mirror_selection_local(int axis);

	/*
	// Trajectory Frames
	*/
	private:
	// Parent model of trajectory
	model *trajparent;
	// Name associated with trajectory file
	dnchar trajname;
	// Filename of file
	dnchar trajfilename;
	// Format of trajectory file
	filter *trajfilefilter;
	// File structure
	ifstream *trajfile;
	// File offsets for first and last frames
	streampos trajposfirst, trajposlast;
	// Size of one frame
	long int framesize;
	// Frame list
	list<model> frames;
	// Add frame to trajectory
	model *add_frame();
	// Remove frame from trajectory
	void remove_frame(model*);
	// Number of frames cached
	int ncachedframes;
	// Total number of frames available in file or cache
	int totalframes;
	// Whether this is a cached trajectory (TRUE) or just one frame (FALSE)
	bool trajcached;
	// Position marker
	int frameposition;
	// Whether the trajectory is currently being 'played'
	bool trajplaying;
	// Pointer to config to be drawn
	model *currentframe;

	public:
	// Set parent model of trajectory
	void set_trajparent(model *m) { trajparent = m; }
	// Return parent model of trajectory
	model *get_trajparent() { return trajparent; }
	// Initialise trajectory from file specified
	bool initialise_trajectory(const char*, filter*);
	// Reinitialise (clear) the associated trajectory
	void clear_trajectory();
	// Set the format of the trajectory
	void set_trajfilter(filter *f) { trajfilefilter = f; }
	// Return the trajectory file pointer
	ifstream *get_trajfile() { return trajfile; }
	// Return the current frame pointer
	model *get_currentframe() { return currentframe; }
	// Return the total number of frames in the trajectory (file or cached)
	int get_totalframes() { return totalframes; }
	// Return the current integer frame position
	int get_frameposition() { return frameposition; }
	// Seek to first frame
	void seek_first_frame();
	// Seek to last frame
	void seek_last_frame();
	// Seek to next frame
	void seek_next_frame();
	// Seek to previous frame
	void seek_previous_frame();

	/*
	// View / Rendering
	*/
	private:
	// Pointer to config if draw_source == RS_CONFIG
	model *render_source;

	public:
	// Render from self
	void render_from_self() { render_source = this; }
	// Render from trajectory
	void render_from_frames() { render_source = currentframe; }
	// Set the drawing style of the current atom selection
	void selection_set_style(draw_style);
	// Return the current rendering source for the model
	model *get_render_source() {return render_source; }

	/*
	// Coordinate Transformations
	*/
	public:
	// Convert screen coordinates into modelspace coordinates
	vec3<double> guide_to_model(const vec3<double> &v) { return guide_to_model(v.x, v.y); }
	vec3<double> guide_to_model(double x, double y);
	// Convert from Bohr to Angstrom
	void bohr_to_angstrom();
	// Convert from Angstrom to Bohr
	void angstrom_to_bohr();
	// COnvert fractional coordinates to real coordinates
	void frac_to_real();

	/*
	// Measurements
	*/
	private:
	// List of measurements
	list<measurement> measurements;

	public:
	// Return first measurement in the list
	measurement *get_measurements() { return measurements.first(); }
	// Clear all measurements
	void clear_measurements() { measurements.clear(); }
	// Find specific measurement
	measurement *find_measurement(geom_type, atom*, ...);
	// Clear specific type of measurements
	void remove_measurements(geom_type);
	// Delete specific measurement
	void remove_measurement(measurement *me);
	// Delete all measurements involving supplied atom
	void remove_measurements(atom*);
	// Add measurement (list of atoms)
	void add_measurement(geom_type, atom*, ...);
	// Add measurements of specific type in current selection
	void add_measurements_in_selection(geom_type);
	// Measure distances between atoms
	void measure_distance(atom*, atom*);
	// Measure angles between atoms
	void measure_angle(atom*, atom*, atom*);
	// Measure torsions between atoms
	void measure_torsion(atom*, atom*, atom*, atom*);
	// Update stored measurements
	void update_measurements();

	/*
	// Sites
	*/
	public:
	// List of site definitions
	list<site> sites;
	// Find site by name
	site *find_site(const char*);

	/*
	// Calculated quantities
	*/
	public:
	// List of calculate quantities
	list<calculable> quantities;
	// List of pending or calculating quantities
	list<calculable> pending_quantities;

	/*
	// Misc (to be put somewhere)
	*/
	public:
	constraint *constraints;
	constraint *constraints_tail;
	int nconstraints;

	/*
	// Selection Actions
	*/
	public:
	// Return the empirical formula of the selected atoms
	void selection_get_empirical(dnchar&);
	// Get selection's centre of geometry
	vec3<double> selection_get_cog();

	/*
	// Glyphs
	*/
	private:
	list<glyph> glyphs;

	public:
	glyph *add_glyph();

	/*
	// Undo / Redo
	*/
	private:
	// Pointer to current 'states' of the model in the list
	undostate *currentundostate, *currentredostate;
	// List of undo levels for the model
	list<undostate> undolevels;
	// Current state that we're adding changes to
	undostate *recordingstate;

	public:
	// Return the current undo level pointer
	undostate *get_currentundostate() { return currentundostate; }
	// Return the current redo level pointer
	undostate *get_currentredostate() { return currentredostate; }
	// Signal to begin recording new changes
	void begin_undostate(const char *text);
	// Signal to end recording of changes and to add recorded changes as a new undolevel in the model
	void end_undostate();
	// Perform the undo action pointed to by 'currentundostate'
	void undo();
	// Perform the redo action pointed to by 'currentredostate'
	void redo();
};

#endif

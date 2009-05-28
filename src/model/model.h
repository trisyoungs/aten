/*
	*** Model definition
	*** src/model/model.h
	Copyright T. Youngs 2007-2009

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

#ifndef ATEN_MODEL_H
#define ATEN_MODEL_H

#include "ff/energystore.h"
#include "base/cell.h"
#include "base/log.h"
#include "base/measurement.h"
#include "base/glyph.h"
#include "base/bond.h"
#include "base/atom.h"
#include "base/lineparser.h"
#define SGCOREDEF__
#include "base/sginfo.h"
#include "methods/mc.h"

// Forward Declarations
class Forcefield;
class ForcefieldBound;
class Constraint;
class Pattern;
class Tree;
class Site;
class UndoState;
class Atomaddress;
class Calculable;
class Measurement;
class Grid;

// Model
class Model
{
	public:
	// Constructor / Destructor
	Model();
	~Model();
	// List pointers
	Model *prev, *next;
	// Friend declarations
	friend class IdShiftEvent;


	/*
	// Model
	*/
	private:
	// Name of model
	Dnchar name_;
	// Format of model when loaded / last saved
	Tree *filter_;
	// Filename of model when loaded / last saved
	Dnchar filename_;

	public:
	// Sets the filename of the model
	void setFilename(const char *s);
	// Return the stored filename of the model
	const char *filename() const;
	// Sets the file filter of the model
	void setFilter(Tree *f);
	// Return the stored file filter of the model
	Tree *filter() const;
	// Sets the name of the model
	void setName(const char *s);
	// Return the name of the model
	const char *name() const;
	// Clear all data in model
	void clear();
	// Print information about the model (inc atoms)
	void print();
	// Print log information for the current model
	void printLogs();
	// Copy all information from another model
	void copy(Model*);
	// Copy all atom data from specified model
	void copyAtomData(Model*, int);
	// Copy range of atom data from specified model
	void copyAtomData(Model*, int, int, int);


	/*
	// Log
	*/
	public:
	Log changeLog;


	/*
	// Atoms
	*/
	private:
	// Atoms in model
	List<Atom> atoms_;
	// Delete the specified atom (internal function)
	void removeAtom(Atom*, bool noupdate);
	// Move specified atom one place 'up' in the list (to lower ID)
	void shiftAtomUp(Atom *i);
	// Move specified atom one place 'down' in the list (to higher ID)
	void shiftAtomDown(Atom *i);
	// Total mass of atoms in the model
	double mass_;
	// Number of atoms with unidentified element ('XX') in model
	int nUnknownAtoms_;
	// Reduce the mass (and unknown element count) of the model
	void reduceMass(int element);
	// Increase the mass (and unknown element count) of the model
	void increaseMass(int element);
	
	public:
	// Create a new atom
	Atom *addAtom(short int el, Vec3<double> r, int position = -1);
	// Create a new atom at the Model's current pen position
	Atom *addAtomAtPen(short int el, int position = -1);
	// Create copy of supplied atom
	Atom *addCopy(Atom *source);
	// Create copy of supplied atom at the specified position
	Atom *addCopy(Atom *after, Atom *source);
	// Return the start of the atom list
	Atom *atoms() const;
	// Return the n'th atom in the atom list
	Atom *atom(int n);
	// Return the number of atoms in the model
	int nAtoms() const;
	// Return the list index of the specified atom
	int atomIndex(Atom *i) const;
	// Delete specified atom
	void deleteAtom(Atom *target, bool noupdate = FALSE);
	// Translate specified atom
	void translateAtom(Atom *target, Vec3<double> delta);
	// Position specified atom
	void positionAtom(Atom *target, Vec3<double> newr);
	// Delete all atoms in the model
	void clearAtoms();
	// Perform alchemy on an atom
	void transmuteAtom(Atom *target, short int element);
	// Renumber atoms in the model
	void renumberAtoms(Atom *from = NULL);
	// Reset tempi values of all atoms
	void resetTempi(int);
	// Return pointer to the atom with the specified id
	Atom *findAtom(int);
	// Return the (first) atom with matching tempi
	Atom *findAtomByTempi(int);
	// Move selected atoms one place 'up' in the list
	void shiftSelectionUp();
	// Move selected atoms one place 'down' in the list
	void shiftSelectionDown();
	// Move selected atoms to start of the list
	void moveSelectionToStart();
	// Move selection to end of the list
	void moveSelectionToEnd();
	// Return (and autocreate if necessary) the static atoms array
	Atom **atomArray();
	// Set visibility of specified atom
	void setHidden(Atom *i, bool hidden);
	// Set charge of specified atom
	void chargeAtom(Atom *i, double q);
	// Prints out the coordinates of the atoms in the model
	void printCoords() const;
	// Return total bond order penalty of atoms in the first pattern molecule
	int totalBondOrderPenalty() const;
	// Return the number of bonds of specified type to the atom
	int countBondsToAtom(Atom *i, Bond::BondType);
	// Set the drawing style of the specified atom
	void styleAtom(Atom *i, Atom::DrawStyle);
	// Set the drawing style of the current atom selection
	void styleSelection(Atom::DrawStyle);
	// Calculate the total mass of the model
	void calculateMass();
	// Return the mass of the molecule
	double mass() const;
	// Return number of unknown atoms in the model
	int nUnknownAtoms();


	/*
	// Unit Cell
	*/
	private:
	// Cell definition (also contains reciprocal cell definition)
	Cell cell_;
	// Density of model (if periodic)
	double density_;
	// Calculate the density of the model
	void calculateDensity();
	// SGInfo structure
	T_SgInfo spacegroup_;

	public:
	// Return pointer to unit cell structure
	Cell *cell();
	// Set cell (vectors)
	void setCell(Vec3<double> lengths, Vec3<double> angles);
	// Set cell (axes)
	void setCell(Mat3<double> axes);
	// Set cell (parameter)
	void setCell(Cell::CellParameter cp, double value);
	// Set cell (other Cell pointer)
	void setCell(Cell *newcell);
	// Remove cell definition
	void removeCell();
	// Fold all atoms into the cell
	void foldAllAtoms();
	// Set up spacegroup data for model (with sginfo)
	void setSpacegroup(const char *s);
	// Apply the given symmetry generator to the current atom selection in the model
	void pack(Generator *gen);
	// Apply the symmetry operators listed in the model's spacegroup
	void pack();
	// Fold all molecules into the cell
	void foldAllMolecules();
	// Replicate cell to create supercell
	void replicateCell(const Vec3<double>&, const Vec3<double>&);
	// Scale cell and contents
	bool scaleCell(const Vec3<double> &scale, bool usecogs);
	// Rotate cell and contents
	void rotateCell(int axis, double angle);
	// Return the density of the model
	double density() const;


	/*
	// Bonds
	*/
	private:
	// Bonds in the model
	List<Bond> bonds_;
	// Atom reflists (pointers) for bond calculation
	Reflist<Atom,double> *bondingCuboids_, *bondingOverlays_;
	// Number of cuboids in atom reflists
	int nCuboids_;
	// Integer number of boxes in each direction
	Vec3<int> cuboidBoxes_;
	// Product of y and z cuboidBoxes_
	int cuboidYZ_;
	// Extent of model, and size of cuboids
	Vec3<double> extentMin_, extentMax_, extentRange_, cuboidSize_;
	// Initialise reflists based on current extent of model
	void initialiseBondingCuboids();
	// Free any created reflists
	void freeBondingCuboids();
	// Add atom to cuboid reflists
	void addAtomToCuboid(Atom *i);
	// Calculate bonding from stored cuboid lists
	void rebond();

	public:
	// Return first bond in the model
	Bond *bonds();
	// Return number of bonds in the model
	int nBonds();
	// Return the nth bond in the model
	Bond *bond(int n);
	// Add bond of specified type between atoms
	void bondAtoms(Atom *i, Atom *j, Bond::BondType bt);
	// Add bond of specified type between atoms (by id)
	void bondAtoms(int ii, int jj, Bond::BondType bt);
	// Delete bond between specified atoms
	void unbondAtoms(Atom *i, Atom *j, Bond *b = NULL);
	// Change type of specified bond
	void changeBond(Bond *b, Bond::BondType bt);
	// Clear all bonding in model
	void clearBonding();
	// Calculate bonding in the model
	void calculateBonding();
	// Augment bonding in the model
	void augmentBonding();
	// Calculate bonding in current atom selection
	void selectionCalculateBonding();
	// Bond all atom pairs in current atom selection
	void selectionBondAll();
	// Clear bonding in current atom selection
	void selectionClearBonding();


	/*
	// Selection / Marked Atoms
	*/
	private:
	// Number of selected atoms
	int nSelected_;
	// Number of marked atoms
	int nMarked_;

	public:
	// Select the specified atom
	void selectAtom(Atom *i, bool markonly = FALSE);
	// Select the specified atom ID
	void selectAtom(int id, bool markonly = FALSE);
	// Deselect the specified atom
	void deselectAtom(Atom *i, bool markonly = FALSE);
	// Deselect the specified atom
	void deselectAtom(int id, bool markonly = FALSE);
	// Toggle the selection state of the atom
	void selectionToggle(Atom *i, bool markonly = FALSE);
	// Select all atoms
	void selectAll(bool markonly = FALSE);
	// Select no atoms
	void selectNone(bool markonly = FALSE);
	// Return the number of selected atoms
	int nSelected();
	// Return the number of marked atoms
	int nMarked();
	// Mark all atoms
	void markAll();
	// Match marked atoms to current selection
	void markSelectedAtoms();
	// Invert current atom selection
	void selectionInvert(bool markonly = FALSE);
	// Delete current atom selection
	void selectionDelete(bool markonly = FALSE);
	// Expand current atom selection by one bond
	void selectionExpand(bool markonly = FALSE);
	// Return the atom at the clicked screen coordinates (if any)
	Atom *atomOnScreen(double, double);
	// Select all atoms in specified pattern
	void selectPattern(Pattern *p, bool markonly = FALSE, bool deselect = FALSE);
	// Select all atoms within the rectangular boundary specified
	void selectBox(double, double, double, double, bool deselect = FALSE);
	// Select all atoms connected by a path from the specified atom
	void selectTree(Atom *i, bool markonly = FALSE, bool deselect = FALSE);
	// Select all atoms of the same element as the specified atom
	void selectElement(Atom *i, bool markonly = FALSE, bool deselect = FALSE);
	// Select all atoms of the same element as the atom with the specified id
	void selectElement(int el, bool markonly = FALSE, bool deselect = FALSE);
	// DeSelect all atoms of the same element as the atom with the specified id
	void deselectElement(int el, bool markonly = FALSE);
	// Select all atoms which match the provided type
	int selectType(int element, const char *typedesc, bool markonly = FALSE, bool deselect = FALSE);
	// Select all atoms within cutoff of specified atom
	void selectRadial(Atom *i, double d);
	// Return the first selected atom in the model (if any)
	Atom *firstSelected(bool markonly = FALSE);
	// Detect and select overlapping atoms
	void selectOverlaps(double tolerance, bool markonly = FALSE);
	// Select atoms (or molecule COGs) inside of the current unit cell
	void selectInsideCell(bool moleculecogs, bool markonly = FALSE);
	// Select atoms (or molecule COGs) outside of the current unit cell
	void selectOutsideCell(bool moleculecogs, bool markonly = FALSE);
	// Get atoms of a bound fragment with the current selection
	void fragmentFromSelection(Atom *start, Reflist<Atom,int> &list);
	// Recursive selector for fragmentFromSelection()
	void fragmentFromSelectionSelector(Atom *start, Reflist<Atom,int> &list);


	/*
	// View
	*/
	private:
	// Camera rotation
	double cameraRotation_;
	// Camera, model, and view (cam*rot*cell) matrices associated with the model
	Mat4<double> cameraMatrix_, rotationMatrix_, viewMatrix_;
	// Inverse of the view matrix
	Mat4<double> viewMatrixInverse_;
	// Camera position
	Vec3<double> camera_;
	// Log point at the last projection (Log::Coordinate+Log::Camera)
	int projectionPoint_;

	public:
	// Pre-generated display list for atoms
	//GLuint displaylist;
	// Project the specified world coordinates into 2D screen coords
	Vec4<double> &worldToScreen(const Vec3<double>&);
	// Called when, e.g. the camera position or view rotation has changed
	void calculateViewMatrix();
	// Set the current rotation matrix
	void setRotationMatrix(Mat4<double> &rmat);
	// Return the current rotation matrix
	Mat4<double> rotationMatrix();
	// Return the GL-compatible array from the ModelMAT structure
	void copyRotationMatrix(double *m);
	// Return the GL-compatible array from the ModelMAT structure
	void copyCameraMatrix(double *m);
	// Set the camera z-rotation
	void setCameraRotation(double r);
	// Return the current camera z-rotation
	double cameraRotation();
	// Set model rotation to exact values
	void setRotation(double rotx, double roty);
	// Set view to be along the specified cartesian axis
	void viewAlong(double x, double y, double z);
	// Set view to be along the specified cell axis
	void viewAlongCell(double x, double y, double z);
	// Rotate the model about the x and y axes
	void rotate(double, double);
	// Spin the model about the z axis
	void zRotate(double angle);
	// Adjust the position of the camera
	void adjustCamera(double, double, double, double);
	void adjustCamera(const Vec3<double> &v, double r);
	// Adjusts the camera zoom
	void adjustZoom(bool zoomin);
	// (Re)set the camera position and matrix
	void resetCamera(const Vec3<double>&);
	// Reset modelview matrix and camera position
	void resetView();
	// Project the model coordinates of the atom into local and 2D coordinates
	void projectAtom(Atom*);
	// Project given model coordinates into screen coordinates
	Vec3<double> &modelToScreen(Vec3<double> &pos);
	// Project the model coordinates of all atoms
	void projectAll();
	// Project the model coordinates of selected atoms
	void projectSelection();
	// Return the camera position vector
	Vec3<double> camera();
	// Calculate and return drawing pixel width
	double drawPixelWidth();


	/*
	// Labelling
	*/
	public:
	// Add label to atom
	void addLabel(Atom *i, Atom::AtomLabel al);
	// Remove atom label
	void removeLabel(Atom *i, Atom::AtomLabel al);
	// Clear all atom labelling
	void clearAllLabels();
	// Clear labelling from specific atom
	void clearLabels(Atom *i);
	// Clear all atom labelling from the current selection
	void selectionClearLabels();
	// Clear specified atom labelling from the current selection
	void selectionRemoveLabels(Atom::AtomLabel);
	// Set the specified label for all atoms currently selected
	void selectionAddLabels(Atom::AtomLabel);
	// Set the visibility property for all selected atoms
	void selectionSetHidden(bool);
	// Sets the 'fixed' variable of all selected atoms to TRUE
	void selectionSetFixed();
	// Sets the 'fixed' variable of all selected atoms to FALSE
	void selectionSetFree();


	/*
	// Forcefield
	*/
	private:
	// Forcefield associated with this model
	Forcefield *forcefield_;
	// Forcefield containing original file type names (if requested)
	Forcefield *namesForcefield_;

	public:
	// Set the model to use the specified forcefield
	void setForcefield(Forcefield*);
	// Return the forcefield used by the model
	Forcefield *forcefield();
	// Assign forcefield charges to model atoms
	bool assignForcefieldCharges();
	// Reset all model charges to zero
	void clearCharges();
	// Set the forcefield containing original atom names for the model
	void setNamesForcefield(Forcefield *f);
	// Return the forcefield containing original atom names for the model
	Forcefield *namesForcefield();


	/*
	// Expression / Typing
	*/
	private:
	// Atom changeid at which the expression was/is valid
	int expressionPoint_;
	// List containing copies of unique atom types in model (useful in expression export)
	List<ForcefieldAtom> uniqueTypes_;
	// List containing copies of bond interactions in model (useful in expression export)
	List<ForcefieldBound> uniqueBondTerms_;
	// List containing copies of angle interactions in model (useful in expression export)
	List<ForcefieldBound> uniqueAngleTerms_;
	// List containing copies of torsion interactions in model (useful in expression export)
	List<ForcefieldBound> uniqueTorsionTerms_;

	public:
	// Set type of specified atom
	void setAtomtype(Atom *i, ForcefieldAtom *ffa, bool fixed);
	// Determine hybridicities of atoms
	void describeAtoms();
	// Assign forcefield types to all atoms
	bool typeAll();
	// Remove forcefield types from all atoms
	void removeTyping();
	// Set atomtypes of selected atoms
	void selectionSetType(ForcefieldAtom *ffa, bool fixed);
	// Create unique lists
	void createUniqueLists();
	// Return number of unique atom types in model
	int nUniqueTypes();
	// Return the first item in the list of unique types in the model
	ForcefieldAtom *uniqueTypes();
	// Return the unique type specified
	ForcefieldAtom *uniqueType(int i);
	// Return number of unique bond interactions in model
	int nUniqueBondTerms();
	// Return the first item in the list of unique bond interactions in the model
	ForcefieldBound *uniqueBondTerms();
	// Return the unique bond term specified
	ForcefieldBound *uniqueBondTerm(int i);
	// Return number of unique angle interactions in model
	int nUniqueAngleTerms();
	// Return the first item in the list of unique angle interactions in the model
	ForcefieldBound *uniqueAngleTerms();
	// Return the unique angle term specified
	ForcefieldBound *uniqueAngleTerm(int i);
	// Return number of unique torsion interactionss in model
	int nUniqueTorsionTerms();
	// Return the list of unique torsion interactions in the model
	ForcefieldBound *uniqueTorsionTerms();
	// Return the unique torsion term specified
	ForcefieldBound *uniqueTorsionTerm(int i);
	// Create total energy function shell for the model
	bool createExpression(bool vdwOnly = FALSE);
	// Return whether the expression is valid
	bool isExpressionValid();
	// Manually invalidates the expression
	void invalidateExpression();
	// Generate parameters for total energy function
	void fillExpression(int);


	/*
	// Energy / Forces
	*/
	public:
	// Storage for energy
	Energy energy;
	// Calculate (and return) the total energy of the specified model configuration
	double totalEnergy(Model *config);
	// Calculate (and return) the total interaction energy of the specified pattern molecule with the remainder
	double moleculeEnergy(Model *config, Pattern *molpattern, int molecule);
	// Calculate forces in the specified model configuration
	void calculateForces(Model*);
	// Prints out atomic forces
	void printForces();
	// Calculate RMS of current forces
	double calculateRmsForce();
	// Normalise forces (make largest component equal to specified value)
	void normaliseForces(double norm);
	// Zero forces on all atoms
	void zeroForces();
	// Zero forces on all atoms that have their 'fixed' property set to true
	void zeroForcesFixed();


	/*
	// Patterns
	*/
	private:
	// Pattern nodes for the model
	List<Pattern> patterns_;
	// Flag to indicate a valid pattern for the model
	int patternsPoint_;

	public:
	// Create a new pattern node (nmols,natoms,name)
	Pattern *addPattern(int, int, const char*);
	// Cut the pattern from the list
	void cutPattern(Pattern*);
	// Own the specified pattern (bool = whether to set ownermodel)
	void ownPattern(Pattern*, bool);
	// Number of nodes in pattern
	int nPatterns();
	// Return the first pattern node of the model
	Pattern *patterns();
	// Return the pattern with the ID specified
	Pattern *pattern(int id);
	// Return the pattern that the specified atom is in
	Pattern *pattern(Atom *i);
	// Return the last pattern node of the model
	Pattern *lastPattern();
	// Find pattern by name
	Pattern *findPattern(const char*);
	// Autocreate patterns for the model
	bool autocreatePatterns(bool acceptDefault = TRUE);
	// Validate current pattern definition
	bool validatePatterns();
	// Clear the current pattern definition
	void clearPatterns();
	// Return whether the patterns are valid
	bool arePatternsValid();
	// Sets the 'fixed' property of all current patterns
	void setPatternsFixed(int);
	// Calculates the atom locality of the supplied atom
	Atomaddress locateAtom(Atom*);
	// Creates a string of the element symbols in the selection
	void selectionAtomFingerprint(Dnchar&);
	// Creates a characteristic string of the bonds in the selection
	void selectionBondFingerprint(Dnchar&);
	// Charge the pattern atom across the model
	void chargePatternAtom(Pattern*, int, double);
	// Calculate bonding restricted to patterns
	void patternCalculateBonding();
	// Position specified molecule within pattern
	void positionMolecule(Pattern*, int, const Vec3<double>&);
	// Translate specified molecule within pattern
	void translateMolecule(Pattern*, int, const Vec3<double>&);
	// Rotate specified molecule within pattern
	void rotateMolecule(Pattern*, int, double, double);
	// Set the hidden flag on atoms of the specified molecule
	void hideMolecule(Pattern*, int, bool);
	// Print patterns
	void printPatterns();


	/*
	// Model Building
	*/
	private:
	// Iteratively add hydrogens to the specified atom in the desired general geometry
	void addHydrogens(Atom *target, int nhydrogen, Atom::HAddGeom geometry);
	// Pen orientation matrix
	Mat3<double> penOrientation_;
	// Pen position
	Vec3<double> penPosition_;

	public:
	// Adds hydrogens to satisfy the bond order requirements of atoms in the model
	void hydrogenSatisfy(Atom *target = NULL);
	// Return the pen orientation matrix
	Mat3<double> penOrientation();
	// Rotate the pen orientation matrix about the specified axis
	void rotatePenAxis(int axis, double degrees);
	// Reset the pen axis system
	void resetPenOrientation();
	// Return the current pen position
	Vec3<double> penPosition();
	// Move the pen (in its current axis system)
	void movePenPosition(Vec3<double> v);
	// Set the pen position absolutely
	void setPenPosition(Vec3<double> v);


	/*
	// Geometry (using staticatoms[])
	*/
	public:
	// Calculate distance
	double distance(int i, int j);
	double distance(Atom *i, Atom *j);
	// Calculate angle
	double angle(int i, int j, int k);
	double angle(Atom *i, Atom *j, Atom *k);
	// Calculate torsion
	double torsion(int i, int j, int k, int l);
	double torsion(Atom *i, Atom *j, Atom *k, Atom *l);


	/*
	// Transformations
	*/
	private:
	// Length scale to use for world translations through GUI
	double translateScale_;

	public:
	// Prepare for atom manipulation
	void prepareTransform();
	// Return the translation scale
	double translateScale();
	// Finalize atom transform
	void finalizeTransform(Reflist< Atom,Vec3<double> >&, const char *statetitle);
	// Rotate the atom selection
	void rotateSelectionWorld(double, double);
	// Spin the atom selection
	void rotateSelectionZaxis(double);
	// Puts the selections centre of geometry at 0,0,0
	void centre(const Vec3<double> &v);
	void centre(double x, double y, double z);
	// Translate selection by the vector specified
	void translateSelectionLocal(const Vec3<double>&);
	// Translate selection by the vector specified (in world coordinates)
	void translateSelectionWorld(const Vec3<double>&);
	// Rotate selection about specified vector
	void rotateSelectionVector(Vec3<double>, Vec3<double>, double);
	// Mirror selection about specified axis
	void mirrorSelectionLocal(int axis);
	// Matrix transform current selection
	void matrixTransformSelection(Vec3<double> origin, Mat3<double> matrix, bool markonly = FALSE);


	/*
	// Trajectory Frames
	*/
	private:
	// Parent model of trajectory
	Model *trajectoryParent_;
	// Name associated with trajectory file
	Dnchar trajectoryName_;
	// Filename of file
	Dnchar trajectoryFilename_;
	// Filter for trajectory file
	Tree *trajectoryFilter_;
	// Header and frame read functions from filter
	Tree *trajectoryHeaderFunction_, *trajectoryFrameFunction_;
	// Trajectory file parser
	LineParser trajectoryParser_;
	// File offsets for frames
	streampos *trajectoryOffsets_;
	// Number of highest frame file offset stored
	int highestFrameOffset_;
	// Size of one frame
	long int frameSize_;
	// Frame list
	List<Model> frames_;
	// Remove frame from trajectory
	void removeFrame(Model*);
	// Total number of frames available in file (if an uncached trajectory)
	int nFileFrames_;
	// Whether this is a cached trajectory (TRUE) or just one frame (FALSE)
	bool framesAreCached_;
	// Current frame position counter
	int frameIndex_;
	// Whether the trajectory is currently being 'played'
	bool trajectoryPlaying_;
	// Pointer to config to be drawn
	Model *currentFrame_;

	public:
	// Add frame to trajectory
	Model *addFrame();
	// Return whether a trajectory for this model exists
	bool hasTrajectory();
	// Return whether the trajectory is cached (if there is one)
	bool trajectoryIsCached();
	// Set parent model of trajectory
	void setTrajectoryParent(Model *m);
	// Return parent model of trajectory
	Model *trajectoryParent();
	// Initialise trajectory from file specified
	bool initialiseTrajectory(const char*, Tree*);
	// Reinitialise (clear) the associated trajectory
	void clearTrajectory();
	// Set the format of the trajectory
	void setTrajectoryFilter(Tree *f);
	// Return the trajectory file pointer
	ifstream *trajectoryFile();
	// Return the current frame pointer
	Model *currentFrame();
	// Return pointer to specified frame number
	Model *frame(int n);
	// Return the total number of frames in the trajectory (file or cached)
	int nFrames();
	// Return the current integer frame position
	int frameIndex();
	// Seek to first frame
	void seekFirstFrame();
	// Seek to last frame
	void seekLastFrame();
	// Seek to next frame
	void seekNextFrame();
	// Seek to previous frame
	void seekPreviousFrame();
	// Seek to specified frame
	void seekFrame(int frameno);


	/*
	// Rendering
	*/
	private:
	// Whether to render from self (TRUE) or trajectory frame (FALSE)
	bool renderFromSelf_;

	public:
	// Render from self
	void setRenderFromSelf();
	// Render from trajectory
	void setRenderFromFrames();
	// Return the current rendering source for the model
	Model *renderSource();


	/*
	// Coordinate Transformations
	*/
	public:
	// Convert screen coordinates into modelspace coordinates
	Vec3<double> guideToModel(const Vec3<double> &v);
	Vec3<double> guideToModel(double x, double y);
	// Convert from Bohr to Angstrom
	void bohrToAngstrom();
	// Convert from Angstrom to Bohr
	void angstromToBohr();
	// Cnvert fractional coordinates to real coordinates
	void fracToReal();


	/*
	// Measurements
	*/
	private:
	// List of measurements
	List<Measurement> measurements_;

	public:
	// Return first measurement in the list
	Measurement *measurements();
	// Clear all measurements
	void clearMeasurements();
	// Find specific measurement
	Measurement *findMeasurement(Measurement::MeasurementType, Atom*, ...);
	// Clear specific type of measurements
	void removeMeasurements(Measurement::MeasurementType);
	// Delete specific measurement
	void removeMeasurement(Measurement *me);
	// Delete all measurements involving supplied atom
	void removeMeasurements(Atom*);
	// Add measurement (list of atoms)
	Measurement *addMeasurement(Measurement::MeasurementType, Atom*, ...);
	// Add measurements of specific type in current selection
	void addMeasurementsInSelection(Measurement::MeasurementType);
	// Measure distances between atoms
	double measureDistance(Atom*, Atom*);
	// Measure distances between atom ids
	double measureDistance(int i, int j);
	// Measure angles between atoms
	double measureAngle(Atom*, Atom*, Atom*);
	// Measure angles between atom ids
	double measureAngle(int i, int j, int k);
	// Measure torsions between atoms
	double measureTorsion(Atom*, Atom*, Atom*, Atom*);
	// Measure torsions between atom ids
	double measureTorsion(int i, int j, int k, int l);
	// Update stored measurements
	void updateMeasurements();
	// List stored measurements
	void listMeasurements();


	/*
	// Sites
	*/
	public:
	// List of site definitions
	List<Site> sites;
	// Find site by name
	Site *findSite(const char*);
	// Calculate site centre from config and molecule ID supplied
	Vec3<double> siteCentre(Site *s, int molid);
	// Calculate local coordinate system for site / molecule ID supplied
	Mat3<double> siteAxes(Site *s, int molid);


	/*
	// Calculated quantities
	*/
	public:
	// List of calculate quantities
	List<Calculable> quantities;
	// List of pending or calculating quantities
	List<Calculable> pendingQuantities;


	/*
	// Selection Actions
	*/
	public:
	// Return the empirical formula of the selected atoms
	void selectionEmpirical(Dnchar&, bool markonly);
	// Get selection's centre of geometry
	Vec3<double> selectionCog();
	// Get selection's centre of mass
	Vec3<double> selectionCom();
	// Reorder bound atoms/fragments within the selection so that they are consecutive
	void reorderSelectedAtoms();


	/*
	// Glyphs
	*/
	private:
	// List of glyphs within model
	List<Glyph> glyphs_;

	public:
	// Create new glyph in this model
	Glyph *addGlyph(Glyph::GlyphType gt);
	// Return list of glyphs
	Glyph *glyphs();
	// Return vector for data point in Glyph
	Vec3<double> glyphVector(Glyph *g, int dataid);
	// Automatically add polyhedra glyphs to current atom selection
	void addPolyhedraGlyphs(bool centresonly, bool linkatoms, double rcut);
	// Automatically add ellipsoids to current atom selection
	void addEllipsoidGlyphs();


	/*
	// Undo / Redo
	*/
	private:
	// Whether undo/redo is enabled (default is false)
	bool undoRedoEnabled_;
	// Pointer to current and previous states of the model in the list
	UndoState *currentUndoState_, *currentRedoState_;
	// List of undo states for the model
	List<UndoState> undoStates_;
	// Current state that we're adding changes to
	UndoState *recordingState_;

	public:
	// Flag that undo/redo should be enabled for the model
	void enableUndoRedo();
	// Return the current undo level pointer
	UndoState *currentUndoState();
	// Return the current redo level pointer
	UndoState *currentRedoState();
	// Signal to begin recording new changes
	void beginUndoState(const char *fmt ...);
	// Signal to end recording of changes and to add recorded changes as a new undolevel in the model
	void endUndoState();
	// Perform the undo action pointed to by 'currentundostate'
	void undo();
	// Perform the redo action pointed to by 'currentredostate'
	void redo();
	// List undo states
	void listUndoStates();


	/*
	// Component Definition (for disordered builder only)
	*/
	private:
	// Pointer to the Components related pattern
	Pattern *componentPattern_;
	// Number of requested copies
	int nRequested_;
	// Lists which MC move types are allowed for this Component
	bool moveAllowed_[MonteCarlo::nMoveTypes];

	public:
	// Definition of region the Component is restricted to
	ComponentRegion area;
	// Set the Component's pattern
	void setComponentPattern(Pattern *p);
	// Return the Component's pattern
	Pattern *componentPattern();
	// Set the requested number of molecules
	void setNRequested(int i);
	// Return the requested number of molecules
	int nRequested();
	// Set a specific move type for the Component
	void setMoveAllowed(MonteCarlo::MoveType m, bool b);
	// Set whether the Component may be translated
	bool isMoveAllowed(MonteCarlo::MoveType m);


	/*
	// Grid Data
	*/
	private:
	// Grids currently associated to the model
	List<Grid> grids_;

	public:
	// Return list of surfaces
	Grid *grids() const;
	// Return number of surfaces loaded
	int nGrids() const;
	// Return specified surface
	Grid *grid(int id);
	// Add new surface
	Grid *addGrid();
	// Remove surface
	void removeGrid(Grid *s);
	// Request rerendering of all grid data
	void rerenderGrids();
};

#endif

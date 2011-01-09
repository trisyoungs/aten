/*
	*** Model definition
	*** src/model/model.h
	Copyright T. Youngs 2007-2010

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

#include "templates/pointerpair.h"
#include "ff/energystore.h"
#include "base/cell.h"
#include "base/log.h"
#include "base/measurement.h"
#include "base/glyph.h"
#include "base/bond.h"
#include "base/atom.h"
#include "base/lineparser.h"
#include "base/eigenvector.h"
#include "classes/basisshell.h"
#define SGCOREDEF__
#include "base/sginfo.h"
#include "methods/mc.h"
#include "base/vibration.h"
#include "classes/zmatrix.h"

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
	// Render source list
	enum RenderSource { ModelSource, TrajectorySource };
	// Model types
	enum ModelType { ParentModelType, TrajectoryFrameType, VibrationFrameType };
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
	// Parent model (if a trajectory or vibration frame)
	Model *parent_;
	// Type of model
	ModelType type_;

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
	void print() const;
	// Print log information for the current model
	void printLogs() const;
	// Copy all information from another model
	void copy(Model *source);
	// Copy all atom data from specified model
	void copyAtomData(Model *source, int data);
	// Copy range of atom data from specified model
	void copyAtomData(Model *source, int data, int startid, int n);
	// Set parent model of model (for frames)
	void setParent(Model *m);
	// Return parent model of model (for frames)
	Model *parent() const;
	// Set model type
	void setType(Model::ModelType mt);
	// Return model type
	Model::ModelType type();


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
	Atom *addAtom(short int el, Vec3<double> r, Vec3<double> f = Vec3<double>(), Vec3<double> v = Vec3<double>());
	// Create a new atom with specified id
	Atom *addAtomWithId(short int el, Vec3<double> r, int atomid);
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
	void atomSetHidden(Atom *i, bool hidden);
	// Set fixed status of specified atom
	void atomSetFixed(Atom *i, bool fixed);
	// Set charge of specified atom
	void atomSetCharge(Atom *i, double q);
	// Set custom colour of specified atom
	void atomSetColour(Atom *i, double r, double g, double b, double a = 1.0);
	// Reset custom colour of specified atom
	void atomResetColour(Atom *i);
	// Set the drawing style of the specified atom
	void atomSetStyle(Atom *i, Atom::DrawStyle);
	// Prints out the coordinates of the atoms in the model
	void printCoords() const;
	// Return total bond order penalty of atoms in the first pattern molecule
	int totalBondOrderPenalty() const;
	// Return the number of bonds of specified type to the atom
	int countBondsToAtom(Atom *i, Bond::BondType);
	// Calculate the total mass of the model
	void calculateMass();
	// Return the mass of the model
	double mass() const;
	// Calculate and return the forcefield mass of the model
	double forcefieldMass() const;
	// Return number of unknown atoms in the model
	int nUnknownAtoms() const;


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
	void setCell(Matrix axes);
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
	bool scaleCell(const Vec3<double> &scale, bool usecogs, bool calcenergy);
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
	int nBonds() const;
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
	void calculateBonding(bool augment);
	// Augment bonding in the model
	void augmentBonding();
	// Calculate bonding in current atom selection
	void selectionCalculateBonding(bool augment);
	// Bond all atom pairs in current atom selection
	void selectionBondAll();
	// Clear bonding in current atom selection
	void selectionClearBonding();


	/*
	// Selection / Marked Atoms
	*/
	private:
	// Reflist of selected atoms
	Reflist<Atom,int> selection_;
	// Reflist of marked atoms, always in ID order
	Reflist<Atom,int> marked_;

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
	int nSelected() const;
	// Return the number of marked atoms
	int nMarked() const;
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
	void selectTree(Atom *i, bool markonly = FALSE, bool deselect = FALSE, Bond *omitbond = NULL);
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
	Refitem<Atom,int> *selection(bool markonly = FALSE) const;
	// Return the nth selected atom in the model
	Refitem<Atom,int> *selected(int n);
	// Detect and select overlapping atoms
	void selectOverlaps(double tolerance, bool markonly = FALSE);
	// Select atoms (or molecule COGs) inside of the current unit cell
	void selectInsideCell(bool moleculecogs, bool markonly = FALSE);
	// Select atoms (or molecule COGs) outside of the current unit cell
	void selectOutsideCell(bool moleculecogs, bool markonly = FALSE);
	// Perform a Miller 'selection' on the model contents
	void selectMiller(int h, int k, int l, bool inside, bool markonly = FALSE);
	// Select atoms within distance from a line (i.e. cylinder select)
	void selectLine(Vec3<double> line, Vec3<double> point, double dr, bool markonly = FALSE);
	// Get atoms of a bound fragment with the current selection
	void fragmentFromSelection(Atom *start, Reflist<Atom,int> &list);
	// Recursive selector for fragmentFromSelection()
	void fragmentFromSelectionSelector(Atom *start, Reflist<Atom,int> &list);
	// Clear all atom labelling from the current selection
	void selectionClearLabels();
	// Clear specified atom labelling from the current selection
	void selectionRemoveLabels(Atom::AtomLabel);
	// Set the specified label for all atoms currently selected
	void selectionAddLabels(Atom::AtomLabel);
	// Set the visibility property for all selected atoms
	void selectionSetHidden(bool hidden);
	// Sets the 'fixed' variable of all selected atoms
	void selectionSetFixed(bool fixed);
	// Set the custom colour of all selected atoms
	void selectionSetColour(double r, double g, double b, double a = 1.0);
	// Reset the custom colours of all selected atoms
	void selectionResetColour();
	// Set the drawing style of the current atom selection
	void selectionSetStyle(Atom::DrawStyle);


	/*
	// View
	*/
	private:
	// Modelview matrix for this model's current view
	Matrix modelViewMatrix_;
	// Inverse of the view matrix
	Matrix modelViewMatrixInverse_;

	private:
	// Calculate and return inverse of current view matrix
	Matrix &modelViewMatrixInverse();

	public:
	// Return current view matrix
	Matrix &modelViewMatrix();
	// Set the current modelview matrix
	void setModelViewMatrix(Matrix &mvmat);
	// Set view to be along the specified cartesian axis
	void viewAlong(double x, double y, double z);
	// Set view to be along the specified cell axis
	void viewAlongCell(double x, double y, double z);
	// Rotate view about arbitrary axis
	void axisRotateView(Vec3<double> axis, double angle);
	// Set exact rotation of model (angles passed in degrees)
	void setRotation(double rotx, double roty);
	// Rotate view about the x and y axes
	void rotateView(double xang, double yang);
	// Spin view about the z axis
	void zRotateView(double dz);
	// Adjust the position of the camera
	void adjustCamera(double dx, double dy, double dz);
	// Adjusts the camera zoom
	void adjustZoom(bool zoomin);
	// Reset modelview matrix and camera position
	void resetView();


	/*
	// Other Properties
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


	/*
	// Forcefield
	*/
	private:
	// Forcefield associated with this model
	Forcefield *forcefield_;
	// Forcefield containing original file type names (if requested)
	Forcefield *namesForcefield_;
	// Create a names forcefield containing original atom names for the model
	void createNamesForcefield();

	public:
	// Set the model to use the specified forcefield
	void setForcefield(Forcefield*);
	// Return the forcefield used by the model
	Forcefield *forcefield();
	// Assign forcefield charges to model atoms
	bool assignForcefieldCharges();
	// Reset all model charges to zero
	void clearCharges();
	// Return the forcefield containing original atom names for the model
	Forcefield *namesForcefield() const;
	// Add name to names forcefield
	ForcefieldAtom *addAtomName(int el, const char *name);
	// Remove reference to names forcefield
	void removeNamesForcefield();


	/*
	// Expression / Typing
	*/
	private:
	// Log value at which the expression was valid
	int expressionPoint_;
	// Whether the last succesfully created expression was VDW-only
	bool expressionVdwOnly_;
	// List containing references tobond interactions in model (useful in expression export)
	Reflist<ForcefieldBound,int> forcefieldBonds_;
	// List containing references to angle interactions in model (useful in expression export)
	Reflist<ForcefieldBound,int> forcefieldAngles_;
	// List containing references to torsion interactions in model (useful in expression export)
	Reflist<ForcefieldBound,int> forcefieldTorsions_;
	// List containing references to unique (by name) atom types in used in model (useful in expression export)
	Reflist<ForcefieldAtom,int> uniqueForcefieldTypes_;
	// List containing references to all (i.e. unique by pointer) atom types use in model
	Reflist<ForcefieldAtom,int> allForcefieldTypes_;
	// Combination table, containing pre-combined VDW parameters
	PairTable<ForcefieldAtom,double> combinationTable_;

	public:
	// Set type of specified atom
	void setAtomType(Atom *i, ForcefieldAtom *ffa, bool fixed);
	// Determine hybridicities of atoms
	void describeAtoms();
	// Assign forcefield types to all atoms
	bool typeAll();
	// Remove forcefield types from all atoms
	void removeTyping();
	// Set atomtypes of selected atoms
	void selectionSetType(ForcefieldAtom *ffa, bool fixed);
	// Create unique lists
	void createForcefieldLists();
	// Return number of unique bond interactions in model
	int nForcefieldBonds() const;
	// Return the first item in the list of unique bond interactions in the model
	Refitem<ForcefieldBound,int> *forcefieldBonds();
	// Return the unique bond term specified
	Refitem<ForcefieldBound,int> *forcefieldBond(int i);
	// Return number of unique angle interactions in model
	int nForcefieldAngles() const;
	// Return the first item in the list of unique angle interactions in the model
	Refitem<ForcefieldBound,int> *forcefieldAngles();
	// Return the unique angle term specified
	Refitem<ForcefieldBound,int> *forcefieldAngle(int i);
	// Return number of unique torsion interactionss in model
	int nForcefieldTorsions() const;
	// Return the list of unique torsion interactions in the model
	Refitem<ForcefieldBound,int> *forcefieldTorsions();
	// Return the unique torsion term specified
	Refitem<ForcefieldBound,int> *forcefieldTorsion(int i);
	// Return number of unique (by name) atom types in model
	int nUniqueForcefieldTypes() const;
	// Return the first item in the list of unique types in the model
	Refitem<ForcefieldAtom,int> *uniqueForcefieldTypes();
	// Return the unique type specified
	Refitem<ForcefieldAtom,int> *uniqueForcefieldType(int i);
	// Create total energy function shell for the model
	bool createExpression(bool vdwOnly = FALSE, bool allowDummy = FALSE);
	// Return whether the expression is valid
	bool isExpressionValid() const;
	// Clear the current expression
	void clearExpression();
	// Manually invalidates the expression
	void invalidateExpression();
	// Generate parameters for total energy function
	void fillExpression(int);
	// Return specified pair data from combination table
	PointerPair<ForcefieldAtom,double> *combinedParameters(ForcefieldAtom *at1, ForcefieldAtom *at2);


	/*
	// Energy / Forces
	*/
	private:
	// RMS force for last calculated forces
	double rmsForce_;

	public:
	// Storage for energy
	EnergyStore energy;
	// Calculate (and return) the total energy of the specified model configuration
	double totalEnergy(Model *config, bool &success);
	// Calculate (and return) the total interaction energy of the specified pattern molecule with the remainder
	double moleculeEnergy(Model *config, Pattern *molpattern, int molecule, bool &success);
	// Calculate forces in the specified model configuration
	bool calculateForces(Model *config);
	// Prints out atomic forces
	void printForces() const;
	// Return RMS of last calculated forces
	double rmsForce() const;
	// Normalise all atomic forces to the value provided
	void normaliseForces(double norm, bool tolargest);
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
	// Log value at which current pattern was valid
	int patternsPoint_;

	public:
	// Create a new pattern node (nmols,natoms,name)
	Pattern *addPattern(int, int, const char*);
	// Cut the pattern from the list
	void cutPattern(Pattern*);
	// Own the specified pattern (bool = whether to set ownermodel)
	void ownPattern(Pattern*, bool);
	// Number of nodes in pattern
	int nPatterns() const;
	// Return the first pattern node of the model
	Pattern *patterns() const;
	// Return the pattern with the ID specified
	Pattern *pattern(int id);
	// Return the pattern that the specified atom is in
	Pattern *pattern(Atom *i);
	// Return the last pattern node of the model
	Pattern *lastPattern() const;
	// Find pattern by name
	Pattern *findPattern(const char *name) const;
	// Autocreate patterns for the model
	bool autocreatePatterns(bool acceptDefault = TRUE);
	// Validate current pattern definition
	bool validatePatterns();
	// Clear the current pattern definition
	void clearPatterns();
	// Return whether the patterns are valid
	bool arePatternsValid() const;
	// Sets the 'fixed' property of all current patterns
	void setPatternsFixed(int);
	// Calculates the atom locality of the supplied atom
	Atomaddress locateAtom(Atom*);
	// Charge the pattern atom across the model
	void chargePatternAtom(Pattern*, int, double);
	// Calculate bonding restricted to patterns
	void patternCalculateBonding(bool augment);
	// Position specified molecule within pattern
	void positionMolecule(Pattern*, int, const Vec3<double>&);
	// Translate specified molecule within pattern
	void translateMolecule(Pattern*, int, const Vec3<double>&);
	// Rotate specified molecule within pattern
	void rotateMolecule(Pattern*, int, double, double);
	// Set the hidden flag on atoms of the specified molecule
	void hideMolecule(Pattern*, int, bool);
	// Print patterns
	void printPatterns() const;


	/*
	// Model Building
	*/
	private:
	// Iteratively add hydrogens to the specified atom in the desired general geometry
	void addHydrogens(Atom *target, int nhydrogen, Atom::HAddGeom geometry);
	// Pen orientation matrix
	Matrix penOrientation_;
	// Pen position
	Vec3<double> penPosition_;

	public:
	// Adds hydrogens to satisfy the bond order requirements of atoms in the model
	void hydrogenSatisfy(Atom *target = NULL);
	// Return the pen orientation matrix
	Matrix penOrientation() const;
	// Rotate the pen orientation matrix about the specified axis
	void rotatePenAxis(int axis, double degrees);
	// Reset the pen axis system
	void resetPenOrientation();
	// Return the current pen position
	Vec3<double> penPosition() const;
	// Move the pen (in its current axis system)
	void movePenPosition(Vec3<double> v);
	// Set the pen position absolutely
	void setPenPosition(Vec3<double> v);
	// Set distance between atoms, moving atom j
	void setAtomicDistance(Atom *i, Atom *j, double newdistance);
	// Set angle between atoms, moving atom k
	void setAtomicAngle(Atom *i, Atom *j, Atom *k, double newangle);
	// Set torsionx between atoms, moving atom l
	void setAtomicTorsion(Atom *i, Atom *j, Atom *k, Atom *l, double newtorsion);


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
	double translateScale() const;
	// Finalize atom transform
	void finalizeTransform(Reflist< Atom,Vec3<double> >&, const char *statetitle, bool nofold);
	// Rotate the atom selection
	void rotateSelectionWorld(double, double);
	// Spin the atom selection
	void rotateSelectionZaxis(double);
	// Puts the selections centre of geometry at 0,0,0
	void centre(const Vec3<double> &v, bool lockx = FALSE, bool locky = FALSE, bool lockz = FALSE);
	void centre(double x, double y, double z, bool lockx = FALSE, bool locky = FALSE, bool lockz = FALSE);
	// Translate selection by the vector specified
	void translateSelectionLocal(const Vec3<double>&, bool markonly = FALSE);
	// Translate selection by the vector specified (in world coordinates)
	void translateSelectionWorld(const Vec3<double>&, bool markonly = FALSE);
	// Rotate selection about specified vector
	void rotateSelectionVector(Vec3<double> origin, Vec3<double> vector, double angle, bool markonly = FALSE);
	// Mirror selection about specified axis
	void mirrorSelectionLocal(int axis, bool markonly = FALSE);
	// Matrix transform current selection
	void matrixTransformSelection(Vec3<double> origin, Matrix transform, bool markonly = FALSE);


	/*
	// Trajectory Frames
	*/
	private:
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
	int trajectoryHighestFrameOffset_;
	// Size of one frame
	long int trajectoryFrameSize_;
	// Frame list
	List<Model> trajectoryFrames_;
	// Remove frame from trajectory
	void removeTrajectoryFrame(Model*);
	// Total number of frames available in file (if an uncached trajectory)
	int nTrajectoryFileFrames_;
	// Whether this is a cached trajectory (TRUE) or just one frame (FALSE)
	bool trajectoryFramesAreCached_;
	// Current frame position counter
	int trajectoryFrameIndex_;
	// Whether the trajectory is currently being 'played'
	bool trajectoryPlaying_;
	// Current trajectory frame (Model*) to be drawn
	Model *trajectoryCurrentFrame_;

	public:
	// Add frame to trajectory
	Model *addTrajectoryFrame();
	// Return whether a trajectory for this model exists
	bool hasTrajectory() const;
	// Return whether the trajectory is cached (if there is one)
	bool trajectoryIsCached() const;
	// Initialise trajectory from file specified
	bool initialiseTrajectory(const char*, Tree*);
	// Reinitialise (clear) the associated trajectory
	void clearTrajectory();
	// Set the format of the trajectory
	void setTrajectoryFilter(Tree *f);
	// Return the trajectory file pointer
	ifstream *trajectoryFile();
	// Return the current frame pointer
	Model *trajectoryCurrentFrame() const;
	// Return pointer to specified frame number
	Model *trajectoryFrame(int n);
	// Return the total number of frames in the trajectory (file or cached)
	int nTrajectoryFrames() const;
	// Return the current integer frame position
	int trajectoryFrameIndex() const;
	// Seek to first frame
	void seekFirstTrajectoryFrame();
	// Seek to last frame
	void seekLastTrajectoryFrame();
	// Seek to next frame
	void seekNextTrajectoryFrame();
	// Seek to previous frame
	void seekPreviousTrajectoryFrame();
	// Seek to specified frame
	void seekTrajectoryFrame(int frameno);


	/*
	// Rendering Source
	*/
	private:
	// Where to get 
	RenderSource renderSource_;
	// Flags whether to draw from associated vibration instead of model
	bool renderFromVibration_;

	public:
	// Set rendering source
	void setRenderSource(RenderSource rs);
	// Return whether rendering from self
	RenderSource renderSource() const;
	// Return the current rendering source for the model
	Model *renderSourceModel();
	// Set whether to render from vibration frames
	void setRenderFromVibration(bool b);
	// Return whether to render from vibration frames
	bool renderFromVibration();


	/*
	// Coordinate Transformations
	*/
	public:
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
	// List of distance measurements
	List<Measurement> distanceMeasurements_;
	// List of distance measurements
	List<Measurement> angleMeasurements_;
	// List of distance measurements
	List<Measurement> torsionMeasurements_;

	public:
	// Return number of measurements in the angles list
	int nAngleMeasurements() const;
	// Return number of measurements in the distances list
	int nDistanceMeasurements() const;
	// Return number of measurements in the torsions list
	int nTorsionMeasurements() const;
	// Return first measurement in the angles list
	Measurement *angleMeasurements() const;
	// Return first measurement in the distances list
	Measurement *distanceMeasurements() const;
	// Return first measurement in the torsions list
	Measurement *torsionMeasurements() const;
	// Return nth measurement in the angles list
	Measurement *angleMeasurement(int index);
	// Return nth measurement in the distances list
	Measurement *distanceMeasurement(int index);
	// Return nth measurement in the torsions list
	Measurement *torsionMeasurement(int index);
	// Clear all measurements
	void clearMeasurements();
	// Find specific distance
	Measurement *findDistanceMeasurement(Atom *i, Atom *j) const;
	// Find specific angle
	Measurement *findAngleMeasurement(Atom *i, Atom *j, Atom *k) const;
	// Find specific torsion
	Measurement *findTorsionMeasurement(Atom *i, Atom *j, Atom *k, Atom *l) const;
	// Clear specific type of measurements
	void removeMeasurements(Measurement::MeasurementType);
	// Delete specific measurement
	void removeMeasurement(Measurement *me);
	// Delete all measurements involving supplied atom
	void removeMeasurements(Atom*);
	// Add measurement (list of atoms)
	Measurement *addMeasurement(Measurement::MeasurementType ...);
	// Add measurements of specific type in current selection
	void addMeasurementsInSelection(Measurement::MeasurementType);
	// Add distance measurement between atoms
	double addDistanceMeasurement(Atom *i, Atom *j, bool quiet = FALSE);
	// Add distance measurement between atom ids
	double addDistanceMeasurement(int i, int j, bool quiet = FALSE);
	// Add angle measurement between atoms
	double addAngleMeasurement(Atom *i, Atom *j, Atom *k, bool quiet = FALSE);
	// Add angle measurement between atom ids
	double addAngleMeasurement(int i, int j, int k, bool quiet = FALSE);
	// Add torsion measurement between atoms
	double addTorsionMeasurement(Atom *i, Atom *j, Atom *k, Atom *l, bool quiet = FALSE);
	// Add torsion measurement between atom ids
	double addTorsionMeasurement(int i, int j, int k, int l, bool quiet = FALSE);
	// Update stored measurements
	void updateMeasurements();
	// List stored measurements
	void listMeasurements() const;


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
	Matrix siteAxes(Site *s, int molid);


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
	void selectionEmpirical(Dnchar&, bool markonly, bool addspaces = FALSE) const;
	// Get selection's centre of geometry
	Vec3<double> selectionCentreOfGeometry() const;
	// Get selection's centre of mass
	Vec3<double> selectionCentreOfMass() const;
	// Reorder bound atoms/fragments within the selection so that they are consecutive
	void reorderSelectedAtoms();
	// Creates a string of the element symbols in the selection
	void selectionAtomFingerprint(Dnchar&);
	// Creates a characteristic string of the bonds in the selection
	void selectionBondFingerprint(Dnchar&);


	/*
	// Glyphs
	*/
	private:
	// List of glyphs within model
	List<Glyph> glyphs_;

	public:
	// Create new glyph in this model
	Glyph *addGlyph(Glyph::GlyphType gt);
	// Remove specified glyph from model
	void removeGlyph(Glyph *g);
	// Return number of glyphs defined in model
	int nGlyphs() const;
	// Return first glyph in list (if any)
	Glyph *glyphs() const;
	// Return specific glyph
	Glyph *glyph(int n);
	// Return vector for data point in Glyph
	Vec3<double> glyphVector(Glyph *g, int dataid) const;
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
	// Flag that undo/redo should be disabled for the model
	void disableUndoRedo();
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
	// Definition of region the Component is restricted to
	ComponentRegion region_;
	// Pointer to the Components related pattern
	Pattern *componentPattern_;
	// Number of requested copies
	int nRequested_;
	// Lists which MC move types are allowed for this Component
	bool moveAllowed_[MonteCarlo::nMoveTypes];

	public:
	// Return region data for model
	ComponentRegion *region();
	// Set the Component's pattern
	void setComponentPattern(Pattern *p);
	// Return the Component's pattern
	Pattern *componentPattern() const;
	// Set the requested number of molecules
	void setNRequested(int i);
	// Return the requested number of molecules
	int nRequested() const;
	// Set a specific move type for the Component
	void setMoveAllowed(MonteCarlo::MoveType m, bool b);
	// Set whether the Component may be translated
	bool isMoveAllowed(MonteCarlo::MoveType m) const;


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


	/*
	// ZMatrix
	*/
	private:
	// Zmatrix definition of model
	ZMatrix zMatrix_;
	// Log point at which the zmatrix was valid
	int zMatrixPoint_;

	public:
	// Retrieve (creating or updating as necessary) the zmatrix for the model
	ZMatrix *zMatrix();
	// Recalculate model atom posions from ZMatrix definition
	void recalculateFromZMatrix();


	/*
	// Molecular Orbital Data
	*/
	private:
	// List of basis shells
	List<BasisShell> basisShells_;
	// Eigenvectors
	List<Eigenvector> eigenvectors_;

	public:
	// Add new basis function to the list
	BasisShell *addBasisShell();
	// Return the first basis function in the list
	BasisShell *basisShells();
	// Return total number of defined basis functions (shells)
	int nBasisShells();
	// Return total number of cartesian basis functions
	int nCartesianBasisFunctions();
	// Add new eigenvevtor to the list
	Eigenvector *addEigenvector();
	// Return the first eigenvector in the list
	Eigenvector *eigenvectors();
	// Return the n'th eigenvector in the list
	Eigenvector *eigenvector(int n);
	// Return total number of defined eigenvectors
	int nEigenvectors();
	// Return density of nth eigenvalue at given coordinates
	double eigenvectorDensityAt(int id, Vec3<double> v);


	/*
	// Vibration Data
	*/
	private:
	// List of defined vibrations
	List<Vibration> vibrations_;
	// List of vibration frames
	List<Model> vibrationFrames_;
	// Current vibration frame
	Model *vibrationCurrentFrame_;
	// Direction of current playback
	bool vibrationForward_;

	public:
	// Add a new vibration to the model
	Vibration *addVibration(int size = -1);
	// Return number of defined vibrations
	int nVibrations();
	// Return first vibration
	Vibration *vibrations();
	// Return n'th vibration
	Vibration *vibration(int n);
	// Generate trajectory for n'th vibration
	void generateVibration(int index);
	// Return current vibration frame
	Model *vibrationCurrentFrame();
	// Move on to next/prev frame (depending on current playback direction)
	void vibrationNextFrame();
};

#endif

/*
	*** Atom
	*** src/base/atom.h
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

#ifndef ATEN_ATOM_H
#define ATEN_ATOM_H

#include "templates/vector3.h"
#include "templates/vector4.h"
#include "templates/reflist.h"

// Forward declarations
class Model;
class Bond;
class ForcefieldAtom;

// Base Atom Data
class Atom
{
	public:
	// Constructor
	Atom();
	// List pointers
	Atom *prev, *next;
	// Get next selected atom in list
	Atom *nextSelected(bool markonly = FALSE);
	// Drawing style enum
	enum DrawStyle { StickStyle, TubeStyle, SphereStyle, ScaledStyle, IndividualStyle, nDrawStyles };
	static DrawStyle drawStyle(const char*);
	static const char *drawStyle(DrawStyle);	
	// Atom label enum
	enum AtomLabel { IdLabel=1, ElementLabel=2, TypeLabel=4, EquivLabel=8, ChargeLabel=16, nLabelTypes=5 };
	static AtomLabel atomLabel(const char*);
	static const char *atomLabel(Atom::AtomLabel);
	// Hydrogen-add geometries enum
	enum HAddGeom { LinearHydrogen, PlanarHydrogen, TetrahedralHydrogen };
	// Atom structure data
	enum AtomData { AllData=0, PositionData=1, ForceData=2, VelocityData=4, ChargeData=8, FixedData=16, ElementData=32 };
	// Atom environment
	enum AtomEnvironment { NoEnvironment, UnboundEnvironment, Sp3Environment, Sp2Environment, SpEnvironment, AromaticEnvironment, nEnvironments };
	static const char *atomEnvironment(AtomEnvironment);
	// Geometries about atomic centres
	enum AtomGeometry { NoGeometry, UnboundGeometry, OneBondGeometry, LinearGeometry, TShapeGeometry, TrigPlanarGeometry, TetrahedralGeometry, SquarePlanarGeometry, TrigBipyramidGeometry, OctahedralGeometry, nAtomGeometries };
	static AtomGeometry atomGeometry(const char*);
	static const char *atomGeometry(AtomGeometry);

	private:
	// Parent model
	Model *parent_;

	public:
	// Add bound neighbours to reflist specified
	void addBoundToReflist(Reflist<Atom,int>*);
	// Reset all data items in structure
	void reset();
	// Copy atom data from supplied atom
	void copy(Atom*);
	// Copy style data (no q, r, f, or v) from supplied atom
	void copyStyle(Atom*);
	// Print out all info about the atom
	void print();
	// One-line atom summary
	void printSummary();
	// Set parent model
	void setParent(Model *m);
	// Return parent model
	Model *parent();

	/*
	// Coordinates
	*/
	protected:
	Vec3<double> r_;
	public:
	Vec3<double> &r();

	/*
	// Forces
	*/
	protected:
	Vec3<double> f_;
	public:
	Vec3<double> &f();

	/*
	// Velocities
	*/
	protected:
	Vec3<double> v_;
	public:
	Vec3<double> &v();

	/*
	// Character
	*/
	protected:
	// Atomic charge
	double charge_;
	// Element number
	short int el_;
	// Oxidation state (used by typing routines)
	short int os_;
	// Forcefield atom type
	ForcefieldAtom *type_;
	// Whether the assigned forcefield type is fixed
	bool fixedType_;
	// Chemical environment of atom
	Atom::AtomEnvironment environment_;
	// Whether the atom will be moved in minimisations etc.
	bool fixedPosition_;

	public:
	// Sets the atom charge
	void setCharge(double d);
	// Return the atom charge
	double charge();
	// Set the element type of the atom
	void setElement(short int newel);
	// Return the element of the atom
	short int element();
	// Check element against the supplied value
	bool isElement(short int n);
	// Check oxidation state against supplied value
	bool isOs(short int n);
	// Return the oxidation state of the atom
	short int os();
	// Set the forcefield type of the atom
	void setType(ForcefieldAtom *ffa);
	// Return the forcefield type of the atom
	ForcefieldAtom *type();
	// Set the fixed status of the assigned atom type
	void setTypeFixed(bool b);
	// Return the fixed status of the assigned atom type
	bool hasFixedType();
	// Check the ff type of the atom against the supplied value
	bool typeIs(ForcefieldAtom *type);
	// Set the environment of the atom
	void setEnvironment(Atom::AtomEnvironment ae);
	// Return the environment of the atom
	Atom::AtomEnvironment environment();
	// Check the environment of the atom against the supplied value
	bool isEnvironment(Atom::AtomEnvironment ae);
	// Set whether the atom's position is fixed
	void setPositionFixed(bool b);
	// Return whether the atom's position is fixed
	bool isPositionFixed();

	/*
	// Bonds / Bonding
	*/
	protected:
	// Bond list for atom
	Reflist<Bond,int> bonds_;

	public:
	// Return the number of bonds to the atom
	int nBonds();
	// Return the current bond list
	Refitem<Bond,int> *bonds();
	// Check the number of bonds against the supplied value
	bool isNBonds(int n);
	// Accept the specified bond to the atom's local reference list
	void acceptBond(Bond *b);
	// Delete the specified bond from the atom's local reference list
	void detachBond(Bond*);
	// Return the total bond order of the atom
	int totalBondOrder();
	// Calculate the bond order between this atom and the specified atom
	double bondOrder(Atom*);
	// Calculates the geometry of the atom's bound environment
	Atom::AtomGeometry geometry(Model*);
	// Returns bond pointer between this and atom 'j' (if it exists)
	Bond *findBond(Atom*);
	// Determine bond plane
	Vec3<double> findBondPlane(Atom*, Bond*, const Vec3<double>&);

	/*
	// Selection
	*/
	private:
	// Selection flag
	bool selected_;
	// Hidden flag
	bool hidden_;
	// Private (marked) selection flag, for internal calculation purposes
	bool marked_;

	public:
	// Sets the selected (or marked) flag of the atom
	void setSelected(bool b, bool markonly = FALSE);
	// Returns the current selection state of the atom
	bool isSelected(bool markonly = FALSE);
	// Sets the hidden flag of the atom
	void setHidden(bool b);
	// Return whether the atom is hidden
	bool isHidden();

	/*
	// Identity
	*/
	private:
	// ID number of atom
	int id_;

	public:
	// Temporary integer variable
	int tempi;
	// Sets the atom id
	void setId(int newid);
	// Decreases the id of the atom by 1
	void decreaseId();
	// Return the id of the atom
	int id();

	/*
	// Rendering Coordinates
	*/
	private:
	// 2D coordinates (screen) and 2D depth
	Vec3<double> rScreen_;
	// Screen radius for selection
	double screenRadius_;
	// World (GL) coordinates, transformed by camera and rotation matrices
	Vec3<double> rWorld_;

	public:
	// World (GL Transformed) coordinates
	Vec3<double> &rWorld();
	// Screen (two-dimensional) coordinates
	Vec3<double> &rScreen();
	// Set the screen radius of the atom
	void setScreenRadius(double radius);
	// Return the screen radius of the atom
	double screenRadius();

	/*
	// Rendering
	*/
	protected:
	// How to draw this atom (and its associated bonds)
	Atom::DrawStyle style_;
	// Bitvector for atom labelling
	short int labels_;

	public:
	// Sets the drawing style of the atom
	void setStyle(Atom::DrawStyle style);
	// Returns the drawing style of the atom
	Atom::DrawStyle style();
	// Returns TRUE id the atom has at least one label specified
	bool hasLabels();
	// Set label bitvector to specified value
	void setLabels(short int l);
	// Returns the label bitmask of the atom
	short int labels();
	// Set the bit for the specified label (if it is not set already)
	void addLabel(Atom::AtomLabel label);
	// Unsets the bit for the specified label (if it is not unset already)
	void removeLabel(Atom::AtomLabel label);
	// Clear all labels from the atom
	void clearLabels();
};

#endif

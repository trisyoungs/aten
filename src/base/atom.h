/*
	*** Atom
	*** src/base/atom.h
	Copyright T. Youngs 2007-2016

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
#include "templates/list.h"
#include "base/prefs.h"
#include "base/namespace.h"
#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;
class Bond;
class ForcefieldAtom;

// Base Atom Data
class Atom : public ListItem<Atom>
{
	public:
	// Constructor / Destructor
	Atom();
	~Atom();
	// Atom label enum
	enum AtomLabel { IdLabel, ElementLabel, TypeLabel, EquivalentLabel, ChargeLabel, nLabelTypes };
	static AtomLabel atomLabel(QString s, bool reportError = false);
	static const char* atomLabel(Atom::AtomLabel al);
	static const char* atomLabelNice(Atom::AtomLabel al);
	// Atom structure data
	enum AtomData { AllData=0, PositionData=1, ForceData=2, VelocityData=4, ChargeData=8, FixedData=16, ElementData=32 };
	// Atom environment
	enum AtomEnvironment { NoEnvironment, UnboundEnvironment, PureEnvironment, NonPureEnvironment, AromaticEnvironment, nEnvironments };
	static const char* atomEnvironment(Atom::AtomEnvironment ae);
	// Geometries about atomic centres
	enum AtomGeometry { NoGeometry, UnboundGeometry, OneBondGeometry, LinearGeometry, TShapeGeometry, TrigPlanarGeometry, TetrahedralGeometry, SquarePlanarGeometry, TrigBipyramidGeometry, OctahedralGeometry, nAtomGeometries };
	static AtomGeometry atomGeometry(QString s, bool reportError = false);
	static const char* atomGeometry(Atom::AtomGeometry ag);
	static int atomGeometryNBonds(Atom::AtomGeometry ag);

	private:
	// Parent model
	Model* parent_;

	public:
	// Add bound neighbours to reflist specified
	void addBoundToRefList(RefList<Atom,int>*);
	// Reset all data items in structure
	void reset();
	// Copy atom data from supplied atom
	bool copy(Atom* source);
	// Copy style data (no q, r, f, or v) from supplied atom
	void copyStyle(Atom* source);
	// Print out all info about the atom
	void print() const;
	// One-line atom summary
	void printSummary() const;
	// Set parent model
	void setParent(Model* m);
	// Return parent model
	Model* parent();


	/*
	 * Coordinates
	 */
	protected:
	Vec3<double> r_;
	public:
	Vec3<double>& r();


	/*
	 * Forces
	 */
	protected:
	Vec3<double> f_;
	public:
	Vec3<double>& f();


	/*
	 * Velocities
	 */
	protected:
	Vec3<double> v_;
	public:
	Vec3<double>& v();


	/*
	 * Character
	 */
	protected:
	// Atomic charge
	double charge_;
	// Element number
	short int element_;
	// Oxidation state (used by typing routines)
	short int os_;
	// Forcefield atom type
	ForcefieldAtom* type_;
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
	double charge() const;
	// Set the element type of the atom
	void setElement(short int newel);
	// Return the element of the atom
	short int element() const;
	// Check element against the supplied value
	bool isElement(short int n) const;
	// Check oxidation state against supplied value
	bool isOs(short int n) const;
	// Return the oxidation state of the atom
	short int os() const;
	// Set the forcefield type of the atom
	void setType(ForcefieldAtom* ffa);
	// Return the forcefield type of the atom
	ForcefieldAtom* type() const;
	// Set the fixed status of the assigned atom type
	void setTypeFixed(bool b);
	// Return the fixed status of the assigned atom type
	bool hasFixedType() const;
	// Check the ff type of the atom against the supplied value
	bool typeIs(ForcefieldAtom* type) const;
	// Set the environment of the atom
	void setEnvironment(Atom::AtomEnvironment ae);
	// Return the environment of the atom
	Atom::AtomEnvironment environment() const;
	// Check the environment of the atom against the supplied value
	bool isEnvironment(Atom::AtomEnvironment ae) const;
	// Set whether the atom's position is fixed
	void setPositionFixed(bool b);
	// Return whether the atom's position is fixed
	bool isPositionFixed() const;
	// Return number of attached hydrogens
	int nHydrogens();


	/*
	 * Bonds / Bonding
	 */
	protected:
	// Bond list for atom
	RefList<Bond,int> bonds_;

	public:
	// Return the number of bonds to the atom
	int nBonds() const;
	// Return the current bond list
	RefListItem<Bond,int>* bonds();
	// Return nth bond in the list
	RefListItem<Bond,int>* bond(int index);
	// Check the number of bonds against the supplied value
	bool isNBonds(int n) const;
	// Accept the specified bond to the atom's local reference list
	void acceptBond(Bond* b);
	// Delete the specified bond from the atom's local reference list
	void detachBond(Bond* b);
	// Return the total bond order of the atom
	int totalBondOrder();
	// Calculate the bond order between this atom and the specified atom
	double bondOrder(Atom* j);
	// Returns bond pointer between this and atom 'j' (if it exists)
	Bond* findBond(Atom* j);


	/*
	 * Geometry
	 */
	public:
	// Calculates the geometry of the atom's bound environment
	Atom::AtomGeometry geometry();
	// Return if the local bound geometry of the atom is planar (within a certain tolerance)
	bool isPlanar(double tolerance);
	// Calculate bond plane (unit) vector
	Vec3<double> findBondPlane(Atom* other, Bond* excludedBond, const Vec3<double>& vij, bool vijIsNormalised);
	// Return next best vector for addition of new atom
	bool nextBondVector(Vec3<double>& vector, Atom::AtomGeometry geometry);
	// Return whether specified atoms form a (bound) angle
	static bool formAngle(Atom* atoms[3], Atom* orderedAtoms[3]);
	// Return whether specified atoms form a (bound) torsion
	static bool formTorsion(Atom* atoms[4], Atom* orderedAtoms[4]);


	/*
	 * Selection
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
	void setSelected(bool b, bool markonly = false);
	// Returns the current selection state of the atom
	bool isSelected(bool markonly = false) const;
	// Sets the hidden flag of the atom
	void setHidden(bool b);
	// Return whether the atom is hidden
	bool isHidden() const;


	/*
	 * Identity
	 */
	private:
	// ID number of atom
	int id_;
	// Data attached to atom (if any)
	char* data_;
	// Temporary integer bitmask
	int tempBit_;

	public:
	// Sets the atom id
	void setId(int newid);
	// Decreases the id of the atom by 1
	void decreaseId();
	// Return the id of the atom
	int id() const;
	// Return data set for atom
	const char* data();
	// Set data for atom
	void setData(const char* data);
	// Clear temporary integer
	void clearBit();
	// Add bit to temporary integer
	void addBit(int bit);
	// Remove bit from temporary integer
	void removeBit(int bit);
	// Check presence of bit in temporary integer
	bool hasBit(int bit);
	// Set value of tempBit
	void setBit(int value);
	// Return value of tempBit
	int bit();


	/*
	 * Rendering
	 */
	protected:
	// How to draw this atom (and its associated bonds)
	Prefs::DrawStyle style_;
	// Bitvector for atom labelling
	short int labels_;
	// Custom colour
	double colour_[4];

	public:
	// Sets the drawing style of the atom
	void setStyle(Prefs::DrawStyle style);
	// Returns the drawing style of the atom
	Prefs::DrawStyle style() const;
	// Returns true id the atom has at least one label specified
	bool hasLabels() const;
	// Set label bitvector to specified value
	void setLabels(short int l);
	// Returns the label bitmask of the atom
	short int labels() const;
	// Set the bit for the specified label (if it is not set already)
	void addLabel(Atom::AtomLabel label);
	// Unsets the bit for the specified label (if it is not unset already)
	void removeLabel(Atom::AtomLabel label);
	// Clear all labels from the atom
	void clearLabels();
	// Set custom colour of atom
	void setColour(double r, double g, double b, double a = 1.0f);
	// Set n'th component of custom colour
	void setColour(int n, double d);
	// Set custom colour from current atom element
	void setColourFromElement();
	// Return custom colour
	double* colour();
	// Copy custom colour
	void copyColour(Vec4<GLfloat>& col) const;
};

ATEN_END_NAMESPACE

#endif

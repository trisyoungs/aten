/*
	*** Energy store
	*** src/ff/energystore.h
	Copyright T. Youngs 2007-2015

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

#ifndef ATEN_ENERGYSTORE_H
#define ATEN_ENERGYSTORE_H

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Pattern;
class Model;

// Energy store
class EnergyStore
{
	public:
	// Constructor / Destructor
	EnergyStore();
	EnergyStore(int npatterns);
	~EnergyStore();
	// Energy Types
	enum EnergyType { BondEnergy, AngleEnergy, TorsionEnergy, UreyBradleyEnergy, VdwIntraEnergy, VdwInterEnergy, VdwTailEnergy, CoulombIntraEnergy, CoulombInterEnergy, EwaldRealIntraEnergy, EwaldRealInterEnergy, EwaldRecipIntraEnergy, EwaldRecipInterEnergy, EwaldSelfEnergy, EwaldMolecularEnergy, nEnergyTypes };
	private:
	// Initialise pointer variables
	void initialise();

	/*
	// Energy components
	*/
	private:
	// Dimension of arrays in created structure.
	int size_;
	// Whether the Energy has actually been used (i.e. calculated)
	bool calculated_;
	// Bond energies
	double* bond_;
	// Angle energies
	double* angle_;
	// Torsion energies
	double* torsion_;
	// Urey-Bradley energies
	double* ureyBradley_;
	// Intramolecular VDW energies
	double* vdwIntra_;
	// Intermolecular VDW energies
	double** vdwInter_;
	// Long-range tail correction to VDW
	double vdwTail_;
	// Intramolecular coulomb energies
	double* coulombIntra_;
	// Intermolecular coulomb energies
	double** coulombInter_;
	// Intramolecular real-space Ewald energies
	double* ewaldRealIntra_;
	// Intermolecular real-space Ewald energies
	double** ewaldRealInter_;
	// Intramolecular reciprocal-space Ewald energies
	double* ewaldRecipIntra_;
	// Intermolecular reciprocal-space Ewald energies
	double** ewaldRecipInter_;
	// Ewald self-interaction correction to reciprocal energy
	double* ewaldSelfCorrect_;
	// Ewald molecular-interaction correction to reciprocal energy
	double* ewaldMolCorrect_;
	// Intramolecular contributions
	double totalBond_, totalAngle_, totalTorsion_, totalUreyBradley_;
	// Short-range interactions total
	double totalVdw_;
	// Electrostatic interactions total
	double totalElectrostatic_;
	// Ewald real/reciprocal totals
	double totalEwaldReal_, totalEwaldRecip_;
	// Ewald correction totals
	double totalEwaldSelf_, totalEwaldMol_;
	// Totals
	double total_, totalInter_, totalIntra_;
	// Deallocate arrays in Energy
	void deallocate();
	// Reset totals
	void resetTotals();

	public:
	// Clear the energy store (reset to zero)
	void clear();
	// Resize the Energy
	void resize(int npatterns);
	// Calculate the sub-total and total energies.
	void totalise();
	// Add to energy
	void add(EnergyType type, double value, int p1, int p2 = -1);
	// Returns the total energy in the store
	double total();
	// Returns the total bond energy in the store
	double bond();
	// Returns the total angle energy in the store
	double angle();
	// Returns the total torsion energy in the store
	double torsion();
	// Returns the total Urey-Bradley energy in the store
	double ureyBradley();
	// Returns the VDW part of the model's energy
	double vdw();
	// Returns the electrostatic part of the model's energy
	double electrostatic();


	/*
	// Printing
	*/
	public:
	// Print out the full energy
	void print();
	// Print out the energy (1-line version)
	void printSummary();
	// Prints out components of the Ewald sum energy
	void printEwald();
	// Prints out energy decomposition matrix for VDW
	void printVdwMatrix(Model* model);
	// Prints out energy decomposition matrix for electrostatics
	void printElecMatrix(Model* model);
	// Prints out energy decomposition matrix for total interpattern interactions
	void printInterMatrix(Model* model);
	// Prints out energy decomposition matrix for intramolecular interactions
	void printIntraMatrix(Model* model);
};

ATEN_END_NAMESPACE

#endif

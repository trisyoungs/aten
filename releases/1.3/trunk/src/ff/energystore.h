/*
	*** Energy store
	*** src/ff/energystore.h
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

#ifndef ATEN_ENERGYSTORE_H
#define ATEN_ENERGYSTORE_H

// Forward Declarations
class Pattern;
class Model;

// Energy store
class Energy
{
	public:
	// Energy Types
	enum EnergyType { BondEnergy, AngleEnergy, TorsionEnergy, VdwIntraEnergy, VdwInterEnergy, VdwTailEnergy, CoulombIntraEnergy, CoulombInterEnergy, EwaldRealIntraEnergy, EwaldRealInterEnergy, EwaldRecipIntraEnergy, EwaldRecipInterEnergy, EwaldSelfEnergy, EwaldMolecularEnergy, nEnergyTypes };

	private:
	// Dimension of arrays in created structure.
	int size_;
	// Whether the Energy has actually been used (i.e. calculated)
	bool calculated_;
	// Deallocate arrays in Energy
	void deallocate();
	// Bond energies
	double *bond_;
	// Angle energies
	double *angle_;
	// Torsion energies
	double *torsion_;
	// Intramolecular VDW energies
	double *vdwIntra_;
	// Intermolecular VDW energies
	double **vdwInter_;
	// Long-range tail correction to VDW
	double vdwTail_;
	// Intramolecular coulomb energies
	double *coulombIntra_;
	// Intermolecular coulomb energies
	double **coulombInter_;
	// Intramolecular real-space Ewald energies
	double *ewaldRealIntra_;
	// Intermolecular real-space Ewald energies
	double **ewaldRealInter_;
	// Intramolecular reciprocal-space Ewald energies
	double *ewaldRecipIntra_;
	// Intermolecular reciprocal-space Ewald energies
	double **ewaldRecipInter_;
	// Ewald self-interaction correction to reciprocal energy
	double *ewaldSelfCorrect_;
	// Ewald molecular-interaction correction to reciprocal energy
	double *ewaldMolCorrect_;
	// Intramolecular contributions
	double totBond_, totAngle_, totTorsion_;
	// Short-range interactions total
	double totVdw_;
	// Electrostatic interactions total
	double totElec_;
	// Ewald real/reciprocal totals
	double totEwaldReal_, totEwaldRecip_;
	// Ewald correction totals
	double totEwaldSelf_, totEwaldMol_;
	// Totals
	double total_, totInter_, totIntra_;

	public:
	// Constructor / Destructor
	Energy();
	~Energy();
	// Clear the energy store (reset to zero)
	void clear();
	// Resize the Energy
	void resize(int);
	// Calculate the sub-total and total energies.
	void totalise();

	/*
	// Set / Get
	*/
	public:
	// Add to energy
	void add(EnergyType, double, int, int=-1);
	// Returns the total energy in the store
	double total();
	// Returns the VDW part of the model's energy
	double vdw();
	// Returns the electrostatic part of the model's energy
	double elec();

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
	void printVdwMatrix(Model*);
	// Prints out energy decomposition matrix for electrostatics
	void printElecMatrix(Model*);
	// Prints out energy decomposition matrix for total interpattern interactions
	void printInterMatrix(Model*);
	// Prints out energy decomposition matrix for intramolecular interactions
	void printIntraMatrix(Model*);
};

#endif

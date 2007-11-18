/*
	*** Energy store
	*** src/classes/energystore.h
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

#ifndef H_ESTORE_H
#define H_ESTORE_H

// Energy Types
enum energy_type { ET_BOND, ET_ANGLE, ET_TORSION, ET_VDWINTRA, ET_VDWINTER, ET_VDWTAIL,
	ET_COULOMBINTRA, ET_COULOMBINTER, ET_EWALDREALINTRA, ET_EWALDREALINTER, 
	ET_EWALDRECIPINTRA, ET_EWALDRECIPINTER, ET_EWALDSELF, ET_EWALDMOL, ET_NITEMS };

// Forward Declarations
class pattern;
class model;

// Energy store
class energystore
{
	private:
	// Dimension of arrays in created structure.
	int size;
	// Whether the energystore has actually been used (i.e. calculated)
	bool calculated;
	// Deallocate arrays in energystore
	void deallocate();
	// Bond energies
	double *bond;
	// Angle energies
	double *angle;
	// Torsion energies
	double *torsion;
	// Intramolecular VDW energies
	double *vdw_intra;
	// Intermolecular VDW energies
	double **vdw_inter;
	// Long-range tail correction to VDW
	double vdw_tail;
	// Intramolecular coulomb energies
	double *coulomb_intra;
	// Intermolecular coulomb energies
	double **coulomb_inter;
	// Intramolecular real-space Ewald energies
	double *ewald_real_intra;
	// Intermolecular real-space Ewald energies
	double **ewald_real_inter;
	// Intramolecular reciprocal-space Ewald energies
	double *ewald_recip_intra;
	// Intermolecular reciprocal-space Ewald energies
	double **ewald_recip_inter;
	// Ewald self-interaction correction to reciprocal energy
	double *ewald_self_correct;
	// Ewald molecular-interaction correction to reciprocal energy
	double *ewald_mol_correct;
	// Intramolecular contributions
	double tot_bond, tot_angle, tot_torsion;
	// Short-range interactions total
	double tot_vdw;
	// Electrostatic interactions total
	double tot_elec;
	// Ewald real/reciprocal totals
	double tot_ereal, tot_erecip;
	// Ewald correction totals
	double tot_eself, tot_emol;
	// Totals
	double total, tot_inter, tot_intra;
	public:
	// Constructor / Destructor
	energystore();
	~energystore();
	// Clear the energy store (reset to zero)
	void clear();
	// Resize the energystore
	void resize(int);
	// Calculate the sub-total and total energies.
	void totalise();

	/*
	// Set / Get
	*/
	public:
	// Add to energy
	void add(energy_type, double, int, int=-1);
	// Returns the total energy in the store
	double get_total() { return total; }
	// Returns the VDW part of the model's energy
	double get_vdw() { return tot_vdw; }
	// Returns the electrostatic part of the model's energy
	double get_elec() { return tot_elec; }

	/*
	// Printing
	*/
	public:
	// Print out the full energy
	void print();
	// Print out the energy (1-line version)
	void print_summary();
	// Prints out components of the Ewald sum energy
	void print_ewald();
	// Prints out energy decomposition matrix for VDW
	void print_vdwmatrix(model*);
	// Prints out energy decomposition matrix for electrostatics
	void print_elecmatrix(model*);
	// Prints out energy decomposition matrix for total interpattern interactions
	void print_intermatrix(model*);
	// Prints out energy decomposition matrix for intramolecular interactions
	void print_intramatrix(model*);

};

#endif

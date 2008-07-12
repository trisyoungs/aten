/*
	*** Energy store
	*** src/classes/Energy.cpp
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

#include "base/prefs.h"
#include "classes/energystore.h"
#include "classes/pattern.h"
#include "model/model.h"

// Constructor
Energy::Energy()
{
	// Private variables
	bond_ = NULL;
	angle_ = NULL;
	torsion_ = NULL;
	vdwIntra_ = NULL;
	coulombIntra_ = NULL;
	ewaldRealIntra_ = NULL;
	ewaldRecipIntra_ = NULL;
	vdwInter_ = NULL;
	vdwTail_ = 0.0;
	coulombInter_ = NULL;
	ewaldRealInter_ = NULL;
	ewaldRecipInter_ = NULL;
	ewaldSelfCorrect_ = NULL;
	ewaldMolCorrect_ = NULL;
	size_ = 0;
	calculated_ = FALSE;
}

// Destructor
Energy::~Energy()
{
	deallocate();
}

// Returns the total energy in the store
double Energy::total()
{
	return total_;
}

// Returns the VDW part of the model's energy
double Energy::vdw()
{
	return totVdw_;
}

// Returns the electrostatic part of the model's energy
double Energy::elec()
{
	return totElec_;
}

// Add
void Energy::add(EnergyType et, double energy, int id1, int id2)
{
	msg.enter("Energy::add");
	if ((id1 >= size_) || (id2 >= size_))
	{
		printf("Energy::add <<<< Array element out of range - %i %i - Ignored >>>>\n", id1, id2);
		msg.exit("Energy::add");
		return;
	}
	switch (et)
	{
		case (Energy::BondEnergy):
			bond_[id1] += energy;
			break;
		case (Energy::AngleEnergy):
			angle_[id1] += energy;
			break;
		case (Energy::TorsionEnergy):
			torsion_[id1] += energy;
			break;
		case (Energy::VdwIntraEnergy):
			vdwIntra_[id1] += energy;
			break;
		case (Energy::VdwInterEnergy):
			vdwInter_[id1][id2] += energy;
			break;
		case (Energy::VdwTailEnergy):
			vdwTail_ += energy;
			break;
		case (Energy::CoulombIntraEnergy):
			coulombIntra_[id1] += energy;
			break;
		case (Energy::CoulombInterEnergy):
			coulombInter_[id1][id2] += energy;
			break;
		case (Energy::EwaldRealIntraEnergy):
			ewaldRealIntra_[id1] += energy;
			break;
		case (Energy::EwaldRealInterEnergy):
			ewaldRealInter_[id1][id2] += energy;
			break;
		case (Energy::EwaldRecipIntraEnergy):
			ewaldRecipIntra_[id1] += energy;
			break;
		case (Energy::EwaldRecipInterEnergy):
			ewaldRecipInter_[id1][id2] += energy;
			break;
		case (Energy::EwaldSelfEnergy):
			ewaldSelfCorrect_[id1] += energy;
			break;
		case (Energy::EwaldMolecularEnergy):
			ewaldMolCorrect_[id1] += energy;
			break;
	}
	msg.exit("Energy::add");
}

// Deallocate arrays
void Energy::deallocate()
{
	msg.enter("Energy::deallocate");
	if (bond_ != NULL) delete[] bond_;
	if (angle_ != NULL) delete[] angle_;
	if (torsion_ != NULL) delete[] torsion_;
	if (vdwIntra_ != NULL) delete[] vdwIntra_;
	if (coulombIntra_ != NULL) delete[] coulombIntra_;
	if (ewaldRealIntra_ != NULL) delete[] ewaldRealIntra_;
	if (ewaldRecipIntra_ != NULL) delete[] ewaldRecipIntra_;
	for (int n=0; n<size_; n++)
	{
		delete[] vdwInter_[n];
		delete[] coulombInter_[n];
		delete[] ewaldRealInter_[n];
		delete[] ewaldRecipInter_[n];
	}
	if (vdwInter_ != NULL) delete[] vdwInter_;
	if (coulombInter_ != NULL) delete[] coulombInter_;
	if (ewaldRealInter_ != NULL) delete[] ewaldRealInter_;
	if (ewaldRecipInter_ != NULL) delete[] ewaldRecipInter_;
	if (ewaldSelfCorrect_ != NULL) delete[] ewaldSelfCorrect_;
	if (ewaldMolCorrect_ != NULL) delete[] ewaldMolCorrect_;
	size_ = 0;
	msg.exit("Energy::deallocate");
}

// Resize
void Energy::resize(int newsize)
{
	msg.enter("Energy::resize");
	// Delete old data first
	deallocate();
	// Now create new arrays...
	size_ = newsize;
	bond_ = new double[size_];
	angle_ = new double[size_];
	torsion_ = new double[size_];
	vdwIntra_ = new double[size_];
	vdwInter_ = new double*[size_];
	coulombIntra_ = new double[size_];
	ewaldRealIntra_ = new double[size_];
	ewaldRecipIntra_ = new double[size_];
	coulombInter_ = new double*[size_];
	ewaldRealInter_ = new double*[size_];
	ewaldRecipInter_ = new double*[size_];
	for (int n=0; n<size_; n++)
	{
		vdwInter_[n] = new double[size_];
		coulombInter_[n] = new double[size_];
		ewaldRealInter_[n] = new double[size_];
		ewaldRecipInter_[n] = new double[size_];
	}
	ewaldSelfCorrect_ = new double[size_];
	ewaldMolCorrect_ = new double[size_];
	clear();
	msg.print(Messenger::Verbose,"Energy store resized to %i\n",size_);
	msg.exit("Energy::resize");
}

// CLear values
void Energy::clear()
{
	// Clear all the values in the energy store
	msg.enter("Energy::clear");
	int n,m;
	for (n=0; n<size_; n++)
	{
		bond_[n] = 0.0;
		angle_[n] = 0.0;
		torsion_[n] = 0.0;
		vdwIntra_[n] = 0.0;
		coulombIntra_[n] = 0.0;
		ewaldRealIntra_[n] = 0.0;
		ewaldRecipIntra_[n] = 0.0;
		for (m=0; m<size_; m++)
		{
			vdwInter_[n][m] = 0.0;
			coulombInter_[n][m] = 0.0;
			ewaldRealInter_[n][m] = 0.0;
			ewaldRecipInter_[n][m] = 0.0;
		}
		ewaldSelfCorrect_[n] = 0.0;
		ewaldMolCorrect_[n] = 0.0;
	}
	vdwTail_ = 0.0;
	totBond_ = 0.0;
	totAngle_ = 0.0;
	totTorsion_ = 0.0;
	totVdw_ = 0.0;
	totElec_ = 0.0;
	totIntra_ = 0.0;
	totInter_ = 0.0;
	totEwaldReal_ = 0.0;
	totEwaldRecip_ = 0.0;
	totEwaldSelf_ = 0.0;
	totEwaldMol_ = 0.0;
	total_ = 0.0;
	calculated_ = FALSE;
	msg.exit("Energy::clear");
}

// Create totals
void Energy::totalise()
{
	// Sum up the energies to get totals for individual aspects and the total overall energy.
	msg.enter("Energy::totalise");
	totBond_ = 0.0;
	totAngle_ = 0.0;
	totTorsion_ = 0.0;
	totVdw_ = 0.0;
	totElec_ = 0.0;
	totIntra_ = 0.0;
	totInter_ = 0.0;
	totEwaldReal_ = 0.0;
	totEwaldRecip_ = 0.0;
	totEwaldSelf_ = 0.0;
	totEwaldMol_ = 0.0;
	total_ = 0.0;
	int n,m;
	for (n=0; n<size_; n++)
	{
		totBond_ += bond_[n];
		totAngle_ += angle_[n];
		totTorsion_ += torsion_[n];
		totVdw_ += vdwIntra_[n];
		totElec_ += coulombIntra_[n];
		totElec_ += ewaldRealIntra_[n];
		totElec_ += ewaldRecipIntra_[n];
		for (m=n; m<size_; m++)
		{
			// Symmetrise matrices here (for printed output)
			if (n != m)
			{
				vdwInter_[n][m] += vdwInter_[m][n];
				vdwInter_[m][n] = vdwInter_[n][m];
				coulombInter_[n][m] += coulombInter_[m][n];
				coulombInter_[m][n] = coulombInter_[n][m];
				ewaldRealInter_[n][m] += ewaldRealInter_[m][n];
				ewaldRealInter_[m][n] = ewaldRealInter_[n][m];
				ewaldRecipInter_[n][m] += ewaldRecipInter_[m][n];
				ewaldRecipInter_[m][n] = ewaldRecipInter_[n][m];
			}
			// Sum intermolecular contributions
			totVdw_ += vdwInter_[n][m];
			totElec_ += coulombInter_[n][m];
			totEwaldReal_ += ewaldRealInter_[n][m];
			// Ewald off-diagonal terms must be added twice
			totEwaldRecip_ += (n == m ? ewaldRecipInter_[n][m] : 2.0*ewaldRecipInter_[n][m]);
		}
		totEwaldSelf_ += ewaldSelfCorrect_[n];
		totEwaldMol_ += ewaldMolCorrect_[n];
	}
	totVdw_ += vdwTail_;
	totElec_ += totEwaldReal_ + totEwaldRecip_ - totEwaldSelf_ - totEwaldMol_;
	totIntra_ = totBond_ + totAngle_ + totTorsion_;
	totInter_ = totVdw_ + totElec_;
	total_ = totIntra_ + totInter_;
	calculated_ = TRUE;
	msg.exit("Energy::totalise");
}

// Print out all energy terms
void Energy::print()
{
	msg.enter("Energy::print");
	if (!calculated_)
	{
		msg.print("Energy::print - Total energy has not yet been calculated.\n");
		msg.exit("Energy::print");
		return;
	}
	msg.print("Energy (%s):\n",Prefs::energyUnit(prefs.energyUnit()));
	msg.print("   Bond : %13.6f\n",totBond_);
	msg.print("  Angle : %13.6f\n",totAngle_);
	msg.print("Torsion : %13.6f\n",totTorsion_);
	msg.print("    VDW : %13.6f (tail contribution : %13.6f)\n",totVdw_,vdwTail_);
	msg.print("   Elec : %13.6f\n",totElec_);
	msg.print("  TOTAL : %13.6f\n",total_);
	msg.exit("Energy::print");
}

// Print energy summary
void Energy::printSummary()
{
	msg.enter("Energy::printSummary");
	if (!calculated_)
	{
		msg.print("Energy::printSummary - Total energy has not yet been calculated.\n");
		msg.exit("Energy::printSummary");
		return;
	}
	msg.print("Etot = %13.6e %s, b a t = %13.6e %13.6e %13.6e v = %13.6e e = %13.6e\n", total_, Prefs::energyUnit(prefs.energyUnit()), totBond_, totAngle_, totTorsion_, totVdw_, totElec_);
	msg.exit("Energy::printSummary");
}

// Print out Ewald energy terms
void Energy::printEwald()
{
	msg.enter("Energy::printEwald");
	if (!calculated_)
	{
		msg.print("Energy::printEwald - Total energy has not yet been calculated.\n");
		msg.exit("Energy::printEwald");
		return;
	}
	msg.print("Ewald Energy (%s):\n",Prefs::energyUnit(prefs.energyUnit()));
	msg.print(" Real : %13.6f\n",totEwaldReal_);
	msg.print("Recip : %13.6f\n",totEwaldRecip_);
	msg.print(" Self : %13.6f\n",totEwaldSelf_);
	msg.print("  Mol : %13.6f\n",totEwaldMol_);
	msg.print("TOTAL : %13.6f\n",totElec_);
	msg.exit("Energy::printEwald");
}

// Print out VDW energy decomposition Matrix
void Energy::printVdwMatrix(Model *m)
{
	msg.enter("Energy::printVdwMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	if (!calculated_)
	{
		msg.print("Energy::printVdwMatrix - Total energy has not yet been calculated.\n");
		msg.exit("Energy::printVdwMatrix");
		return;
	}
	// Print out VDW energy decomposition
	printf("VDW Interaction Energy:\n     Pattern         Intra  ");
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next) printf("%13s  ",p1->name());
	printf("\n");
	count1 = 0;
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next)
	{
		count2 = 0;
		printf("%13s  %13.6e  ",p1->name(),vdwIntra_[count1]);
		for (p2 = m->patterns(); p2 != NULL; p2 = p2->next)
		{
			printf("%13.6e  ",vdwInter_[count1][count2]);
			count2 ++;
		}
		printf("\n");
		count1 ++;
	}
	msg.exit("Energy::printVdwMatrix");
}

// Print out electrostatic energy decomposition Matrix
void Energy::printElecMatrix(Model *m)
{
	msg.enter("Energy::printElecMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	double energy;
	if (!calculated_)
	{
		msg.print("Energy::printElecMatrix - Total energy has not yet been calculated.\n");
		msg.exit("Energy::printElecMatrix");
		return;
	}
	Electrostatics::ElecMethod et = prefs.electrostaticsMethod();
	count1 = 0;
	// Print out electrostatic energy decomposition
	printf("Electrostatic Interaction Energy:\n      Pattern          Intra  ");
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next) printf("%13s  ",p1->name());
	printf("\n");
	count1 = 0;
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next)
	{
		count2 = 0;
		switch (et)
		{
			case (Electrostatics::Coulomb):
				printf("%13s  %13.6e  ",p1->name(),coulombIntra_[count1]);
				for (p2 = m->patterns(); p2 != NULL; p2 = p2->next)
				{
					printf("%13.6e  ",coulombInter_[count1][count2]);
					count2 ++;
				}
				break;
			default:	 // Ewald
				energy = ewaldRealIntra_[count1] + ewaldRecipIntra_[count1] - ewaldMolCorrect_[count1];
				printf("%13s  %13.6e  ",p1->name(),energy);
				for (p2 = m->patterns(); p2 != NULL; p2 = p2->next)
				{
					energy = ewaldRealInter_[count1][count2] + ewaldRecipInter_[count1][count2];
					if (count1 == count2) energy -= ewaldSelfCorrect_[count1];
					else energy += ewaldRecipInter_[count1][count2];
					printf("%13.6e  ",energy);
					count2 ++;
				}
				break;
		}
		printf("\n");
		count1 ++;
	}
	msg.exit("Energy::printElecMatrix");
}

// Print out interpattern energy decomposition Matrix
void Energy::printInterMatrix(Model *m)
{
	msg.enter("Energy::printInterMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	double energyInter, energyIntra;
	if (!calculated_)
	{
		msg.print("Energy::printInterMatrix - Total energy has not yet been calculated.\n");
		msg.exit("Energy::printInterMatrix");
		return;
	}
	Electrostatics::ElecMethod et = prefs.electrostaticsMethod();
	// Print out total interpattern energy decomposition
	printf("Total Interaction Energy:\n      Pattern          Intra  ");
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next) printf("%13s  ",p1->name());
	printf("\n");
	count1 = 0;
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next)
	{
		count2 = 0;
		// Calculate total intrapattern contribution
		energyIntra = vdwIntra_[count1];
		switch (et)
		{
			case (Electrostatics::None):
				break;
			case (Electrostatics::Coulomb):
				energyIntra += coulombIntra_[count1];
				break;
			default: // Ewald
				energyIntra += ewaldRealIntra_[count1] + ewaldRecipIntra_[count1] - ewaldMolCorrect_[count1];
				break;
		}
		printf("%13s  %13.6e  ",p1->name(),energyIntra);
		// Calculate total interpattern contributions
		for (p2 = m->patterns(); p2 != NULL; p2 = p2->next)
		{
			energyInter = vdwInter_[count1][count2];
			switch (et)
			{
				case (Electrostatics::None):
					break;
				case (Electrostatics::Coulomb):
					energyInter += coulombInter_[count1][count2];
					break;
				default:	 // Ewald
					energyInter += ewaldRealInter_[count1][count2] + ewaldRecipInter_[count1][count2];
					if (count1 == count2) energyInter -= ewaldSelfCorrect_[count1];
					else energyInter += ewaldRecipInter_[count1][count2];
					break;
			}
			printf("%13.6e  ",energyInter);
			count2 ++;
		}
		printf("\n");
		count1 ++;
	}
	msg.exit("Energy::printInterMatrix");
}

// Print out intramolecular energy decomposition Matrix
void Energy::printIntraMatrix(Model *m)
{
	msg.enter("Energy::printIntraMatrix");
	int count1;
	Pattern *p1;
	double energy;
	if (!calculated_)
	{
		msg.print("Energy::printIntraMatrix - Total energy has not yet been calculated.\n");
		msg.exit("Energy::printIntraMatrix");
		return;
	}
	// Print out VDW energy decomposition
	printf("Intramolecular Energy:\n     Pattern         Total       Per Mol         Bond         Angle       Torsion \n");
	count1 = 0;
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next)
	{
		energy = bond_[count1] + angle_[count1] + torsion_[count1];
		printf("%13s  %13.6e  %13.6e  %13.6e  %13.6e  %13.6e\n", p1->name(), energy, energy/p1->nMols(), bond_[count1], angle_[count1], torsion_[count1]);
		count1 ++;
	}
	msg.exit("Energy::printIntraMatrix");
}

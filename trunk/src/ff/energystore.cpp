/*
	*** Energy store
	*** src/ff/energystore.cpp
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

#include "model/model.h"
#include "ff/energystore.h"
#include "classes/prefs.h"
#include "base/pattern.h"

// Constructor
EnergyStore::EnergyStore()
{
	// Private variables
	bond_ = NULL;
	angle_ = NULL;
	torsion_ = NULL;
	ureyBradley_ = NULL;
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
	resetTotals();
	size_ = 0;
	calculated_ = FALSE;
}

// Destructor
EnergyStore::~EnergyStore()
{
	deallocate();
}

/*
// Energy components
*/

// Deallocate arrays
void EnergyStore::deallocate()
{
	msg.enter("EnergyStore::deallocate");
	if (bond_ != NULL) delete[] bond_;
	if (angle_ != NULL) delete[] angle_;
	if (torsion_ != NULL) delete[] torsion_;
	if (ureyBradley_ != NULL) delete[] ureyBradley_;
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
	msg.exit("EnergyStore::deallocate");
}

// Reset totals
void EnergyStore::resetTotals()
{
	totalBond_ = 0.0;
	totalAngle_ = 0.0;
	totalTorsion_ = 0.0;
	totalUreyBradley_ = 0.0;
	totalVdw_ = 0.0;
	totalElectrostatic_ = 0.0;
	totalIntra_ = 0.0;
	totalInter_ = 0.0;
	totalEwaldReal_ = 0.0;
	totalEwaldRecip_ = 0.0;
	totalEwaldSelf_ = 0.0;
	totalEwaldMol_ = 0.0;
	total_ = 0.0;
}

// Clear values
void EnergyStore::clear()
{
	// Clear all the values in the energy store
	msg.enter("EnergyStore::clear");
	int n,m;
	for (n=0; n<size_; n++)
	{
		bond_[n] = 0.0;
		angle_[n] = 0.0;
		torsion_[n] = 0.0;
		ureyBradley_[n] = 0.0;
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
	resetTotals();
	calculated_ = FALSE;
	msg.exit("EnergyStore::clear");
}

// Resize
void EnergyStore::resize(int newsize)
{
	msg.enter("EnergyStore::resize");
	// Delete old data first
	deallocate();
	// Now create new arrays...
	size_ = newsize;
	bond_ = new double[size_];
	angle_ = new double[size_];
	torsion_ = new double[size_];
	ureyBradley_ = new double[size_];
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
	msg.exit("EnergyStore::resize");
}

// Create totals
void EnergyStore::totalise()
{
	// Sum up the energies to get totals for individual aspects and the total overall energy.
	msg.enter("EnergyStore::totalise");
	resetTotals();
	int n,m;
	for (n=0; n<size_; n++)
	{
		totalBond_ += bond_[n];
		totalAngle_ += angle_[n];
		totalTorsion_ += torsion_[n];
		totalUreyBradley_ += ureyBradley_[n];
		totalVdw_ += vdwIntra_[n];
		totalElectrostatic_ += coulombIntra_[n];
		totalElectrostatic_ += ewaldRealIntra_[n];
		totalElectrostatic_ += ewaldRecipIntra_[n];
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
			totalVdw_ += vdwInter_[n][m];
			totalElectrostatic_ += coulombInter_[n][m];
			totalEwaldReal_ += ewaldRealInter_[n][m];
			// Ewald off-diagonal terms must be added twice
			totalEwaldRecip_ += (n == m ? ewaldRecipInter_[n][m] : 2.0*ewaldRecipInter_[n][m]);
		}
		totalEwaldSelf_ += ewaldSelfCorrect_[n];
		totalEwaldMol_ += ewaldMolCorrect_[n];
	}
	totalVdw_ += vdwTail_;
	totalElectrostatic_ += totalEwaldReal_ + totalEwaldRecip_ - totalEwaldSelf_ - totalEwaldMol_;
	totalIntra_ = totalBond_ + totalAngle_ + totalTorsion_ + totalUreyBradley_;
	totalInter_ = totalVdw_ + totalElectrostatic_;
	total_ = totalIntra_ + totalInter_;
	calculated_ = TRUE;
	msg.exit("EnergyStore::totalise");
}

// Add
void EnergyStore::add(EnergyType et, double energy, int id1, int id2)
{
	msg.enter("EnergyStore::add");
	if ((id1 >= size_) || (id2 >= size_))
	{
		printf("EnergyStore::add <<<< Array element out of range - %i %i - Ignored >>>>\n", id1, id2);
		msg.exit("EnergyStore::add");
		return;
	}
	switch (et)
	{
		case (EnergyStore::BondEnergy):
			bond_[id1] += energy;
			break;
		case (EnergyStore::AngleEnergy):
			angle_[id1] += energy;
			break;
		case (EnergyStore::TorsionEnergy):
			torsion_[id1] += energy;
			break;
		case (EnergyStore::UreyBradleyEnergy):
			ureyBradley_[id1] += energy;
			break;
		case (EnergyStore::VdwIntraEnergy):
			vdwIntra_[id1] += energy;
			break;
		case (EnergyStore::VdwInterEnergy):
			vdwInter_[id1][id2] += energy;
			break;
		case (EnergyStore::VdwTailEnergy):
			vdwTail_ += energy;
			break;
		case (EnergyStore::CoulombIntraEnergy):
			coulombIntra_[id1] += energy;
			break;
		case (EnergyStore::CoulombInterEnergy):
			coulombInter_[id1][id2] += energy;
			break;
		case (EnergyStore::EwaldRealIntraEnergy):
			ewaldRealIntra_[id1] += energy;
			break;
		case (EnergyStore::EwaldRealInterEnergy):
			ewaldRealInter_[id1][id2] += energy;
			break;
		case (EnergyStore::EwaldRecipIntraEnergy):
			ewaldRecipIntra_[id1] += energy;
			break;
		case (EnergyStore::EwaldRecipInterEnergy):
			ewaldRecipInter_[id1][id2] += energy;
			break;
		case (EnergyStore::EwaldSelfEnergy):
			ewaldSelfCorrect_[id1] += energy;
			break;
		case (EnergyStore::EwaldMolecularEnergy):
			ewaldMolCorrect_[id1] += energy;
			break;
		default:
			printf("Internal Error: Summation of energy type %i missed.\n", et);
			break;
	}
	msg.exit("EnergyStore::add");
}

// Returns the total energy in the store
double EnergyStore::total()
{
	return total_;
}

// Returns the total bond energy in the store
double EnergyStore::bond()
{
	return totalBond_;
}

// Returns the total angle energy in the store
double EnergyStore::angle()
{
	return totalAngle_;
}

// Returns the total torsion energy in the store
double EnergyStore::torsion()
{
	return totalTorsion_;
}

// Returns the total Urey-Bradley energy in the store
double EnergyStore::ureyBradley()
{
	return totalUreyBradley_;
}

// Returns the VDW part of the model's energy
double EnergyStore::vdw()
{
	return totalVdw_;
}

// Returns the electrostatic part of the model's energy
double EnergyStore::electrostatic()
{
	return totalElectrostatic_;
}

/*
// Printing
*/

// Print out all energy terms
void EnergyStore::print()
{
	msg.enter("EnergyStore::print");
	if (!calculated_)
	{
		msg.print("EnergyStore::print - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::print");
		return;
	}
	msg.print("Energy (%s):\n",Prefs::energyUnit(prefs.energyUnit()));
	msg.print("        Bond : %13.6f\n",totalBond_);
	msg.print("       Angle : %13.6f\n",totalAngle_);
	msg.print("Urey-Bradley : %13.6f\n",totalUreyBradley_);
	msg.print("     Torsion : %13.6f\n",totalTorsion_);
	msg.print("         VDW : %13.6f (tail contribution : %13.6f)\n",totalVdw_,vdwTail_);
	msg.print("        Elec : %13.6f\n",totalElectrostatic_);
	msg.print("       TOTAL : %13.6f\n",total_);
	msg.exit("EnergyStore::print");
}

// Print energy summary
void EnergyStore::printSummary()
{
	msg.enter("EnergyStore::printSummary");
	if (!calculated_)
	{
		msg.print("EnergyStore::printSummary - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::printSummary");
		return;
	}
	msg.print("Etot = %13.6e %s, b a t u = %13.6e %13.6e %13.6e %13.6e v = %13.6e e = %13.6e\n", total_, Prefs::energyUnit(prefs.energyUnit()), totalBond_, totalAngle_, totalTorsion_, totalUreyBradley_, totalVdw_, totalElectrostatic_);
	msg.exit("EnergyStore::printSummary");
}

// Print out Ewald energy terms
void EnergyStore::printEwald()
{
	msg.enter("EnergyStore::printEwald");
	if (!calculated_)
	{
		msg.print("EnergyStore::printEwald - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::printEwald");
		return;
	}
	msg.print("Ewald Energy (%s):\n",Prefs::energyUnit(prefs.energyUnit()));
	msg.print(" Real : %13.6f\n",totalEwaldReal_);
	msg.print("Recip : %13.6f\n",totalEwaldRecip_);
	msg.print(" Self : %13.6f\n",totalEwaldSelf_);
	msg.print("  Mol : %13.6f\n",totalEwaldMol_);
	msg.print("TOTAL : %13.6f\n",totalElectrostatic_);
	msg.exit("EnergyStore::printEwald");
}

// Print out VDW energy decomposition Matrix
void EnergyStore::printVdwMatrix(Model *m)
{
	msg.enter("EnergyStore::printVdwMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	if (!calculated_)
	{
		msg.print("EnergyStore::printVdwMatrix - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::printVdwMatrix");
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
	msg.exit("EnergyStore::printVdwMatrix");
}

// Print out electrostatic energy decomposition Matrix
void EnergyStore::printElecMatrix(Model *m)
{
	msg.enter("EnergyStore::printElecMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	double energy;
	if (!calculated_)
	{
		msg.print("EnergyStore::printElecMatrix - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::printElecMatrix");
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
	msg.exit("EnergyStore::printElecMatrix");
}

// Print out interpattern energy decomposition Matrix
void EnergyStore::printInterMatrix(Model *m)
{
	msg.enter("EnergyStore::printInterMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	double energyInter, energyIntra;
	if (!calculated_)
	{
		msg.print("EnergyStore::printInterMatrix - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::printInterMatrix");
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
	msg.exit("EnergyStore::printInterMatrix");
}

// Print out intramolecular energy decomposition Matrix
void EnergyStore::printIntraMatrix(Model *m)
{
	msg.enter("EnergyStore::printIntraMatrix");
	int count1;
	Pattern *p1;
	double energy;
	if (!calculated_)
	{
		msg.print("EnergyStore::printIntraMatrix - Total energy has not yet been calculated.\n");
		msg.exit("EnergyStore::printIntraMatrix");
		return;
	}
	// Print out intramolecular energy decomposition
	printf("Intramolecular Energy:\n     Pattern         Total       Per Mol         Bond         Angle       Torsion \n");
	count1 = 0;
	for (p1 = m->patterns(); p1 != NULL; p1 = p1->next)
	{
		energy = bond_[count1] + angle_[count1] + torsion_[count1] + ureyBradley_[count1];
		printf("%13s  %13.6e  %13.6e  %13.6e  %13.6e  %13.6e\n", p1->name(), energy, energy/p1->nMolecules(), bond_[count1], angle_[count1], torsion_[count1]);
		count1 ++;
	}
	msg.exit("EnergyStore::printIntraMatrix");
}

/*
	*** Energy store
	*** src/ff/energystore.cpp
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

#include "model/model.h"
#include "ff/energystore.h"
#include "base/prefs.h"
#include "base/pattern.h"

ATEN_USING_NAMESPACE

// Constructors
EnergyStore::EnergyStore()
{
	initialise();
}
EnergyStore::EnergyStore(int nPatterns)
{
	initialise();
	resize(nPatterns);
	clear();
}

// Destructor
EnergyStore::~EnergyStore()
{
	deallocate();
}

// Initialise
void EnergyStore::initialise()
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

/*
// Energy components
*/

// Deallocate arrays
void EnergyStore::deallocate()
{
	Messenger::enter("EnergyStore::deallocate");
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
	Messenger::exit("EnergyStore::deallocate");
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
	Messenger::enter("EnergyStore::clear");
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
	Messenger::exit("EnergyStore::clear");
}

// Resize
void EnergyStore::resize(int npatterns)
{
	Messenger::enter("EnergyStore::resize");
	// Delete old data first
	deallocate();
	// Now create new arrays...
	size_ = npatterns;
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
	Messenger::print(Messenger::Verbose, "Energy store resized to %i\n",size_);
	Messenger::exit("EnergyStore::resize");
}

// Create totals
void EnergyStore::totalise()
{
	// Sum up the energies to get totals for individual aspects and the total overall energy.
	Messenger::enter("EnergyStore::totalise");
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
	Messenger::exit("EnergyStore::totalise");
}

// Add
void EnergyStore::add(EnergyStore::EnergyType type, double value, int p1, int p2)
{
	Messenger::enter("EnergyStore::add");
	if ((p1 >= size_) || (p2 >= size_))
	{
		printf("EnergyStore::add <<<< Array element out of range - %i %i - Ignored >>>>\n", p1, p2);
		Messenger::exit("EnergyStore::add");
		return;
	}
	switch (type)
	{
		case (EnergyStore::BondEnergy):
			bond_[p1] += value;
			break;
		case (EnergyStore::AngleEnergy):
			angle_[p1] += value;
			break;
		case (EnergyStore::TorsionEnergy):
			torsion_[p1] += value;
			break;
		case (EnergyStore::UreyBradleyEnergy):
			ureyBradley_[p1] += value;
			break;
		case (EnergyStore::VdwIntraEnergy):
			vdwIntra_[p1] += value;
			break;
		case (EnergyStore::VdwInterEnergy):
			vdwInter_[p1][p2] += value;
			break;
		case (EnergyStore::VdwTailEnergy):
			vdwTail_ += value;
			break;
		case (EnergyStore::CoulombIntraEnergy):
			coulombIntra_[p1] += value;
			break;
		case (EnergyStore::CoulombInterEnergy):
			coulombInter_[p1][p2] += value;
			break;
		case (EnergyStore::EwaldRealIntraEnergy):
			ewaldRealIntra_[p1] += value;
			break;
		case (EnergyStore::EwaldRealInterEnergy):
			ewaldRealInter_[p1][p2] += value;
			break;
		case (EnergyStore::EwaldRecipIntraEnergy):
			ewaldRecipIntra_[p1] += value;
			break;
		case (EnergyStore::EwaldRecipInterEnergy):
			ewaldRecipInter_[p1][p2] += value;
			break;
		case (EnergyStore::EwaldSelfEnergy):
			ewaldSelfCorrect_[p1] += value;
			break;
		case (EnergyStore::EwaldMolecularEnergy):
			ewaldMolCorrect_[p1] += value;
			break;
		default:
			printf("Internal Error: Summation of energy type %i missed.\n", type);
			break;
	}
	Messenger::exit("EnergyStore::add");
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
	Messenger::enter("EnergyStore::print");
	if (!calculated_)
	{
		Messenger::print("EnergyStore::print - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::print");
		return;
	}
	Messenger::print("Energy (%s):\n",Prefs::energyUnit(prefs.energyUnit()));
	Messenger::print("        Bond : %13.6e\n",totalBond_);
	Messenger::print("       Angle : %13.6e\n",totalAngle_);
	Messenger::print("Urey-Bradley : %13.6e\n",totalUreyBradley_);
	Messenger::print("     Torsion : %13.6e\n",totalTorsion_);
	Messenger::print("         VDW : %13.6e (tail contribution : %13.6e)\n",totalVdw_,vdwTail_);
	Messenger::print("        Elec : %13.6e\n",totalElectrostatic_);
	Messenger::print("       TOTAL : %13.6e\n",total_);
	Messenger::exit("EnergyStore::print");
}

// Print energy summary
void EnergyStore::printSummary()
{
	Messenger::enter("EnergyStore::printSummary");
	if (!calculated_)
	{
		Messenger::print("EnergyStore::printSummary - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::printSummary");
		return;
	}
	Messenger::print("Etot = %13.6e %s, b a t u = %13.6e %13.6e %13.6e %13.6e v = %13.6e e = %13.6e\n", total_, Prefs::energyUnit(prefs.energyUnit()), totalBond_, totalAngle_, totalTorsion_, totalUreyBradley_, totalVdw_, totalElectrostatic_);
	Messenger::exit("EnergyStore::printSummary");
}

// Print out Ewald energy terms
void EnergyStore::printEwald()
{
	Messenger::enter("EnergyStore::printEwald");
	if (!calculated_)
	{
		Messenger::print("EnergyStore::printEwald - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::printEwald");
		return;
	}
	Messenger::print("Ewald Energy (%s):\n",Prefs::energyUnit(prefs.energyUnit()));
	Messenger::print(" Real : %13.6e\n",totalEwaldReal_);
	Messenger::print("Recip : %13.6e\n",totalEwaldRecip_);
	Messenger::print(" Self : %13.6e\n",totalEwaldSelf_);
	Messenger::print("  Mol : %13.6e\n",totalEwaldMol_);
	Messenger::print("TOTAL : %13.6e\n",totalElectrostatic_);
	Messenger::exit("EnergyStore::printEwald");
}

// Print out VDW energy decomposition Matrix
void EnergyStore::printVdwMatrix(Model* m)
{
	Messenger::enter("EnergyStore::printVdwMatrix");
	int count1, count2;
	Pattern* p1, *p2;
	if (!calculated_)
	{
		Messenger::print("EnergyStore::printVdwMatrix - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::printVdwMatrix");
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
	Messenger::exit("EnergyStore::printVdwMatrix");
}

// Print out electrostatic energy decomposition Matrix
void EnergyStore::printElecMatrix(Model* m)
{
	Messenger::enter("EnergyStore::printElecMatrix");
	int count1, count2;
	Pattern* p1, *p2;
	double energy;
	if (!calculated_)
	{
		Messenger::print("EnergyStore::printElecMatrix - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::printElecMatrix");
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
	Messenger::exit("EnergyStore::printElecMatrix");
}

// Print out interpattern energy decomposition Matrix
void EnergyStore::printInterMatrix(Model* m)
{
	Messenger::enter("EnergyStore::printInterMatrix");
	int count1, count2;
	Pattern* p1, *p2;
	double energyInter, energyIntra;
	if (!calculated_)
	{
		Messenger::print("EnergyStore::printInterMatrix - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::printInterMatrix");
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
	Messenger::exit("EnergyStore::printInterMatrix");
}

// Print out intramolecular energy decomposition Matrix
void EnergyStore::printIntraMatrix(Model* m)
{
	Messenger::enter("EnergyStore::printIntraMatrix");
	int count1;
	Pattern* p1;
	double energy;
	if (!calculated_)
	{
		Messenger::print("EnergyStore::printIntraMatrix - Total energy has not yet been calculated.\n");
		Messenger::exit("EnergyStore::printIntraMatrix");
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
	Messenger::exit("EnergyStore::printIntraMatrix");
}

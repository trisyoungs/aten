/*
	*** Energy store
	*** src/classes/EnergyStore.cpp
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
EnergyStore::EnergyStore()
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
EnergyStore::~EnergyStore()
{
	deallocate();
}

// Returns the total energy in the store
double EnergyStore::total()
{
	return total_;
}

// Returns the VDW part of the model's energy
double EnergyStore::vdw()
{
	return totVdw_;
}

// Returns the electrostatic part of the model's energy
double EnergyStore::elec()
{
	return totElec_;
}

// Add
void EnergyStore::add(EnergyType et, double energy, int id1, int id2)
{
	dbgBegin(DM_CALLS,"EnergyStore::add");
	if ((id1 >= size_) || (id2 >= size_))
	{
		printf("EnergyStore::add <<<< Array element out of range - %i %i - Ignored >>>>\n", id1, id2);
		dbgEnd(DM_CALLS,"EnergyStore::add");
		return;
	}
	switch (et)
	{
		case (ET_BOND):
			bond_[id1] += energy;
			break;
		case (ET_ANGLE):
			angle_[id1] += energy;
			break;
		case (ET_TORSION):
			torsion_[id1] += energy;
			break;
		case (ET_VDWINTRA):
			vdwIntra_[id1] += energy;
			break;
		case (ET_VDWINTER):
			vdwInter_[id1][id2] += energy;
			break;
		case (ET_VDWTAIL):
			vdwTail_ += energy;
			break;
		case (ET_COULOMBINTRA):
			coulombIntra_[id1] += energy;
			break;
		case (ET_COULOMBINTER):
			coulombInter_[id1][id2] += energy;
			break;
		case (ET_EWALDREALINTRA):
			ewaldRealIntra_[id1] += energy;
			break;
		case (ET_EWALDREALINTER):
			ewaldRealInter_[id1][id2] += energy;
			break;
		case (ET_EWALDRECIPINTRA):
			ewaldRecipIntra_[id1] += energy;
			break;
		case (ET_EWALDRECIPINTER):
			ewaldRecipInter_[id1][id2] += energy;
			break;
		case (ET_EWALDSELF):
			ewaldSelfCorrect_[id1] += energy;
			break;
		case (ET_EWALDMOL):
			ewaldMolCorrect_[id1] += energy;
			break;
	}
	dbgEnd(DM_CALLS,"EnergyStore::add");
}

// Deallocate arrays
void EnergyStore::deallocate()
{
	dbgBegin(DM_CALLS,"EnergyStore::deallocate");
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
	dbgEnd(DM_CALLS,"EnergyStore::deallocate");
}

// Resize
void EnergyStore::resize(int newsize)
{
	dbgBegin(DM_CALLS,"EnergyStore::resize");
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
	msg(DM_VERBOSE,"Energy store resized to %i\n",size_);
	dbgEnd(DM_CALLS,"EnergyStore::resize");
}

// CLear values
void EnergyStore::clear()
{
	// Clear all the values in the energy store
	dbgBegin(DM_CALLS,"EnergyStore::clear");
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
	dbgEnd(DM_CALLS,"EnergyStore::clear");
}

// Create totals
void EnergyStore::totalise()
{
	// Sum up the energies to get totals for individual aspects and the total overall energy.
	dbgBegin(DM_CALLS,"EnergyStore::totalise");
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
			vdwInter_[m][n] = vdwInter_[n][m];
			coulombInter_[m][n] = coulombInter_[n][m];
			ewaldRealInter_[m][n] = ewaldRealInter_[n][m];
			ewaldRecipInter_[m][n] = ewaldRecipInter_[n][m];
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
	dbgEnd(DM_CALLS,"EnergyStore::totalise");
}

// Print out all energy terms
void EnergyStore::print()
{
	dbgBegin(DM_CALLS,"EnergyStore::print");
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::print - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::print");
		return;
	}
	msg(DM_NONE,"Energy (%s):\n",text_from_EU(prefs.energyUnit()));
	msg(DM_NONE,"   Bond : %13.6f\n",totBond_);
	msg(DM_NONE,"  Angle : %13.6f\n",totAngle_);
	msg(DM_NONE,"Torsion : %13.6f\n",totTorsion_);
	msg(DM_NONE,"    VDW : %13.6f (tail contribution : %13.6f)\n",totVdw_,vdwTail_);
	msg(DM_NONE,"   Elec : %13.6f\n",totElec_);
	msg(DM_NONE,"  TOTAL : %13.6f\n",total_);
	dbgEnd(DM_CALLS,"EnergyStore::print");
}

// Print energy summary
void EnergyStore::printSummary()
{
	dbgBegin(DM_CALLS,"EnergyStore::printSummary");
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::printSummary - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::printSummary");
		return;
	}
	msg(DM_NONE,"Etot = %13.6e %s, b a t = %13.6e %13.6e %13.6e v = %13.6e e = %13.6e\n", total_, text_from_EU(prefs.energyUnit()), totBond_, totAngle_, totTorsion_, totVdw_, totElec_);
	dbgEnd(DM_CALLS,"EnergyStore::printSummary");
}

// Print out Ewald energy terms
void EnergyStore::printEwald()
{
	dbgBegin(DM_CALLS,"EnergyStore::printEwald");
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::printEwald - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::printEwald");
		return;
	}
	msg(DM_NONE,"Ewald Energy (%s):\n",text_from_EU(prefs.energyUnit()));
	msg(DM_NONE," Real : %13.6f\n",totEwaldReal_);
	msg(DM_NONE,"Recip : %13.6f\n",totEwaldRecip_);
	msg(DM_NONE," Self : %13.6f\n",totEwaldSelf_);
	msg(DM_NONE,"  Mol : %13.6f\n",totEwaldMol_);
	msg(DM_NONE,"TOTAL : %13.6f\n",totElec_);
	dbgEnd(DM_CALLS,"EnergyStore::printEwald");
}

// Print out VDW energy decomposition Matrix
void EnergyStore::printVdwMatrix(Model *m)
{
	dbgBegin(DM_CALLS,"EnergyStore::printVdwMatrix");
	int i, count1, count2;
	Pattern *p1, *p2;
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::printVdwMatrix - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::printVdwMatrix");
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
	dbgEnd(DM_CALLS,"EnergyStore::printVdwMatrix");
}

// Print out electrostatic energy decomposition Matrix
void EnergyStore::printElecMatrix(Model *m)
{
	dbgBegin(DM_CALLS,"EnergyStore::printElecMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	double energy;
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::printElecMatrix - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::printElecMatrix");
		return;
	}
	ElecMethod et = prefs.electrostaticsMethod();
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
			case (EM_COULOMB):
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
	dbgEnd(DM_CALLS,"EnergyStore::printElecMatrix");
}

// Print out interpattern energy decomposition Matrix
void EnergyStore::printInterMatrix(Model *m)
{
	dbgBegin(DM_CALLS,"EnergyStore::printInterMatrix");
	int count1, count2;
	Pattern *p1, *p2;
	double energyInter, energyIntra;
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::printInterMatrix - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::printInterMatrix");
		return;
	}
	ElecMethod et = prefs.electrostaticsMethod();
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
			case (EM_OFF):
				break;
			case (EM_COULOMB):
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
				case (EM_OFF):
					break;
				case (EM_COULOMB):
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
	dbgEnd(DM_CALLS,"EnergyStore::printInterMatrix");
}

// Print out intramolecular energy decomposition Matrix
void EnergyStore::printIntraMatrix(Model *m)
{
	dbgBegin(DM_CALLS,"EnergyStore::printIntraMatrix");
	int count1;
	Pattern *p1;
	double energy;
	if (!calculated_)
	{
		msg(DM_NONE,"EnergyStore::printIntraMatrix - Total energy has not yet been calculated.\n");
		dbgEnd(DM_CALLS,"EnergyStore::printIntraMatrix");
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
	dbgEnd(DM_CALLS,"EnergyStore::printIntraMatrix");
}

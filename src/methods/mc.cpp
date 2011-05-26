/*
	*** Monte Carlo methods
	*** src/methods/mc.cpp
	Copyright T. Youngs 2007-2011

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
#include "methods/mc.h"
#include "model/clipboard.h"
#include "gui/gui.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "base/progress.h"

// Static Singleton
MonteCarlo mc;

// Monte Carlo move types
const char *MoveTypeKeywords[MonteCarlo::nMoveTypes] = { "Translate", "Rotate", "ZMatrix", "Insert", "Delete" };
const char *MonteCarlo::moveTypeKeyword(MonteCarlo::MoveType mt)
{
	return MoveTypeKeywords[mt];
}

MonteCarlo::MoveType MonteCarlo::moveType(const char *s, bool reporterror)
{
	MonteCarlo::MoveType mt = (MonteCarlo::MoveType) enumSearch("Monte Carlo move", MonteCarlo::nMoveTypes, MoveTypeKeywords, s);
	if ((mt == MonteCarlo::nMoveTypes) && reporterror) enumPrintValid(MonteCarlo::nMoveTypes,MoveTypeKeywords);
	return mt;
}

// Constructor
MonteCarlo::MonteCarlo()
{
	// Private variables
	maxStep_[MonteCarlo::Translate] = 1.0;
	maxStep_[MonteCarlo::Rotate] = 20.0;
	maxStep_[MonteCarlo::ZMatrix] = 0.0;
	nTrials_[MonteCarlo::Translate] = 10;
	nTrials_[MonteCarlo::Rotate] = 10; 
	nTrials_[MonteCarlo::ZMatrix] = 0;
	nTrials_[MonteCarlo::Insert] = 20;
	nTrials_[MonteCarlo::Delete] = 0; 
	moveAllowed_[MonteCarlo::Translate] = TRUE;
	moveAllowed_[MonteCarlo::Rotate] = TRUE;
	moveAllowed_[MonteCarlo::ZMatrix] = FALSE;
	moveAllowed_[MonteCarlo::Insert] = TRUE;
	moveAllowed_[MonteCarlo::Delete] = TRUE; 
	acceptanceEnergy_[MonteCarlo::Translate] = 0.0;
	acceptanceEnergy_[MonteCarlo::Rotate] = 0.0;
	acceptanceEnergy_[MonteCarlo::ZMatrix] = 0.0;
	acceptanceEnergy_[MonteCarlo::Insert] = 100.0;
	acceptanceEnergy_[MonteCarlo::Delete] = 0.0;
	acceptanceRatio_ = NULL;
	acceptanceRatioSize_ = 0;
	temperature_ = 300.0;
	nCycles_ = 100;
}

// Destructor
MonteCarlo::~MonteCarlo()
{
	if (acceptanceRatio_ != NULL) delete[] acceptanceRatio_;
}

// Set maximum stepsize for MC move
void MonteCarlo::setMaxStep(MonteCarlo::MoveType m, double d)
{
	maxStep_[m] = d;
}

// Get maximum stepsize for MC move
double MonteCarlo::maxStep(MonteCarlo::MoveType m) const
{
	return maxStep_[m];
}

// Set nTrials_ for MC move
void MonteCarlo::setNTrials(MonteCarlo::MoveType m, int i)
{
	nTrials_[m] = i;
}

// Get nTrials_ for MC move
int MonteCarlo::nTrials(MonteCarlo::MoveType m) const
{
	return nTrials_[m];
}

// Set moveAllowed_ flag for MC move
void MonteCarlo::setMoveAllowed(MonteCarlo::MoveType m, bool b)
{
	moveAllowed_[m] = b;
}

// Get moveAllowed_ flag for MC move
bool MonteCarlo::isMoveAllowed(MonteCarlo::MoveType m) const
{
	return moveAllowed_[m];
}

// Set acceptanceEnergy_ limit for MC move
void MonteCarlo::setAcceptanceEnergy(MonteCarlo::MoveType m, double d)
{
	acceptanceEnergy_[m] = d;
}

// Get acceptanceEnergy_ limit for MC move
double MonteCarlo::acceptanceEnergy(MonteCarlo::MoveType m) const
{
	return acceptanceEnergy_[m];
}

// Set number of MC cycles to perform
void MonteCarlo::setNCycles(int i)
{
	nCycles_ = i;
}

// Get nTrials_ for MC move
int MonteCarlo::nCycles() const
{
	return nCycles_;
}

// Create ratio acceptance array
void MonteCarlo::createRatioArray(int newsize)
{
	int n, m;
	if (acceptanceRatio_ != NULL)
	{
		for (n=0; n<acceptanceRatioSize_; n++) delete[] acceptanceRatio_[n];
		delete[] acceptanceRatio_;
	}
	acceptanceRatio_ = new double*[newsize];
	for (n=0; n<newsize; n++) acceptanceRatio_[n] = new double[MonteCarlo::nMoveTypes];
	acceptanceRatioSize_ = newsize;
	// Zero the elements
	for (n=0; n<newsize; n++) for (m=0; m<MonteCarlo::nMoveTypes; m++) acceptanceRatio_[n][m] = 0.0;
}

// MC Geometry Minimise
bool MonteCarlo::minimise(Model* srcmodel, double econ, double fcon)
{
	// Monte Carlo energy minimisation.
	// Validity of forcefield and energy setup must be performed before calling and is *not* checked here.
	msg.enter("MonteCarlo::minimise");
	int n, cycle, nmoves, move, mol, randpat, npats;
	Dnchar s;
	double enew, ecurrent, currentVdwEnergy, currentElecEnergy, elast, phi, theta;
	double deltaMoleculeEnergy, deltaVdwEnergy, deltaElecEnergy, referenceMoleculeEnergy, referenceVdwEnergy, referenceElecEnergy;
	Vec3<double> v;
	double beta = 1.0 / (prefs.gasConstant() * temperature_);
	Dnchar etatext;
	bool success;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg.print("Creating expression for target model...\n");
        if ((!srcmodel->createExpression(TRUE)) || (srcmodel->nAtoms() == 0))
	{
		msg.exit("MonteCarlo::minimise");
		return FALSE;
	}

	// Create coordinate backup model for minimisation
	Model bakmodel;
	bakmodel.copy(srcmodel);

	// Create ratio array (not per-pattern, just per move type)
	createRatioArray(1);

	msg.print("Beginning Monte Carlo minimise...\n\n");
	msg.print(" Step     Energy        Delta          VDW          Elec        T%%  R%%  Z%%  I%%  D%%\n");

	// Calculate initial reference energy
	ecurrent = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
		msg.exit("MonteCarlo::minimise");
		return FALSE;
	}
	currentVdwEnergy = srcmodel->energy.vdw();
	currentElecEnergy = srcmodel->energy.electrostatic();
	elast = ecurrent;
	msg.print("       %13.6e               %13.6e %13.6e\n", ecurrent,  currentVdwEnergy, currentElecEnergy);

	// Cycle through move types; try and perform nTrials_ for each; move on.
	// For each attempt, select a random molecule in a random pattern
	nmoves = 0;
	npats = srcmodel->nPatterns();
	Pattern *p = NULL;

	// Start progess indicator
	int pid = progress.initialise("Performing MC minimisation...", nCycles_ * MonteCarlo::Insert, !gui.exists());

	// Loop over MC cycles
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Loop over MC moves
		for (move=0; move<MonteCarlo::Insert; ++move)
		{
			// Update progress indicator
 			if (!progress.update(pid)) break;

			acceptanceRatio_[0][move] = 0;
			// If this move type isn't moveAllowed_ then continue onto the next
			if (!moveAllowed_[move]) continue;
			for (n=0; n<nTrials_[move]; ++n)
			{
				// Select random pattern and molecule
				do
				{
					npats != 1 ? randpat = AtenMath::randomi(npats) : randpat = 0;
					p = srcmodel->pattern(randpat);
				} while ((p->nMolecules() == 0) && (!p->areAtomsFixed()));
				mol = AtenMath::randomi(p->nMolecules());
	
				// Copy the coordinates of the current molecule
				if (p->nMolecules() != 0) bakmodel.copyAtomData(srcmodel, Atom::PositionData, p->offset(mol),p->nAtoms());

				// Calculate reference energy (before move)
				referenceMoleculeEnergy = srcmodel->moleculeEnergy(srcmodel, p, mol, success);
				referenceVdwEnergy = srcmodel->energy.vdw();
				referenceElecEnergy = srcmodel->energy.electrostatic();

				// Otherwise, generate the new configuration (in model's cfg space)
				switch (move)
				{
					// Translate COG of molecule
					case (MonteCarlo::Translate):
						// Create a random translation vector
						v.randomUnit();
						v *= maxStep_[MonteCarlo::Translate]*AtenMath::random();
						// Translate the coordinates of the molecule in cfg
						srcmodel->translateMolecule(p,mol,v);
						break;
					// Rotate molecule about COG
					case (MonteCarlo::Rotate):
						// To do the random rotation, do two separate random rotations about the x and y axes.
						phi = AtenMath::random() * maxStep_[MonteCarlo::Rotate];
						theta = AtenMath::random() * maxStep_[MonteCarlo::Rotate];
						srcmodel->rotateMolecule(p,mol,phi,theta);
						break;
					// Other moves....
				}

				// Get the energy of this new configuration.
				enew = srcmodel->moleculeEnergy(srcmodel, p, mol, success);

				// If the energy has gone up, undo the move.
				deltaMoleculeEnergy = enew - referenceMoleculeEnergy;
				deltaVdwEnergy = srcmodel->energy.vdw() - referenceVdwEnergy;
				deltaElecEnergy = srcmodel->energy.electrostatic() - referenceElecEnergy;

				// Do we accept the move?
				if ((deltaMoleculeEnergy < acceptanceEnergy_[move]) || ( AtenMath::random() < exp(-beta*deltaMoleculeEnergy) ))
				{
					// Update energy and move counters
					//ecurrent = enew;
					//currentVdwEnergy = srcmodel->energy.get_vdw();
					//currentElecEnergy = srcmodel->energy.get_elec();
					ecurrent += deltaMoleculeEnergy;
					currentVdwEnergy += deltaVdwEnergy;
					currentElecEnergy += deltaElecEnergy;
					acceptanceRatio_[0][move] ++;
				}
				else
				{
					// Put the molecules back to where it was before
					srcmodel->copyAtomData(&bakmodel, Atom::PositionData, p->offset(mol), p->nAtoms());
				}
			}
			if (nTrials_[move] != 0) acceptanceRatio_[0][move] /= nTrials_[move];
		} // Loop over MC moves

		if (prefs.shouldUpdateEnergy(cycle+1))
		{
			s.sprintf(" %-5i %13.6e %13.6e %13.6e %13.6e", cycle, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy);
			for (n=0; n<MonteCarlo::nMoveTypes; n++) s.strcatf(" %3i", int(acceptanceRatio_[0][n]*100.0));
			s.strcatf("  %s\n", etatext.get());
			msg.print(s.get());
			//msg.print(" %-5i %13.6e %13.6e %13.6e %13.6e", cycle, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy);
			//for (n=0; n<MonteCarlo::nMoveTypes; n++) msg.print(" %3i",int(acceptanceRatio_[0][n]*100.0));
			//msg.print("\n");
		}
		
		if (prefs.shouldUpdateModel(cycle+1)) gui.update(GuiQt::CanvasTarget);
		
		elast = ecurrent;

	} // Loop over MC cycles
	progress.terminate(pid);

	// Print final energy
	enew = srcmodel->totalEnergy(srcmodel, success);
	srcmodel->energy.print();

	// Finalise
	srcmodel->changeLog.add(Log::Coordinates);

	msg.exit("MonteCarlo::minimise");
	return TRUE;
}

/*
	*** Monte Carlo Methods
	*** src/methods/mc.cpp
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
#include "methods/mc.h"
#include "model/clipboard.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "base/progress.h"

ATEN_BEGIN_NAMESPACE

// Static Singleton
MonteCarlo mc;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Monte Carlo move types
const char* MoveTypeKeywords[MonteCarlo::nMoveTypes] = { "Translate", "Rotate", "ZMatrix", "Insert", "Delete" };
const char* MonteCarlo::moveTypeKeyword(MonteCarlo::MoveType mt)
{
	return MoveTypeKeywords[mt];
}

MonteCarlo::MoveType MonteCarlo::moveType(QString s, bool reportError)
{
	MonteCarlo::MoveType mt = (MonteCarlo::MoveType) enumSearch("Monte Carlo move", MonteCarlo::nMoveTypes, MoveTypeKeywords, s);
	if ((mt == MonteCarlo::nMoveTypes) && reportError) enumPrintValid(MonteCarlo::nMoveTypes,MoveTypeKeywords);
	return mt;
}

// Constructor
MonteCarlo::MonteCarlo()
{
	// Private variables
	// Monte Carlo
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

	// Disorder builder
	disorderAccuracy_ = 0.01;
	disorderMaxFailures_ = 5;
	disorderReductionFactor_ = 0.98;
	disorderMinimumScaleFactor_ = 0.95;
	disorderMaximumScaleFactor_ = 2.0;
	disorderNTweaks_ = 5;
	disorderDeltaDistance_ = 0.3;
	disorderDeltaAngle_ = 5.0;
	disorderMaxCycles_ = 10000;
	disorderRecoveryMaxCycles_ = 30;
	disorderRecoveryMaxTweaks_ = 10;
	disorderRecoveryThreshold_ = 0.90;
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

// Set MC temperature for Boltzmann probabilities
void MonteCarlo::setTemperature(double t)
{
	temperature_ = t;
}

// Return current MC temperature
double MonteCarlo::temperature() const
{
	return temperature_;
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

// Set maximum number of disorder cycles to perform
void MonteCarlo::setDisorderMaxCycles(int ncycles)
{
	disorderMaxCycles_ = ncycles;
}

// Return maximum number of disorder cycles to perform
int MonteCarlo::disorderMaxCycles()
{
	return disorderMaxCycles_;
}

// Set percentage error allowed in actual vs requested densities
void MonteCarlo::setDisorderAccuracy(double accuracy)
{
	disorderAccuracy_ = accuracy;
}

// Return percentage error allowed in actual vs requested densities
double MonteCarlo::disorderAccuracy()
{
	return disorderAccuracy_;
}

// Set number of successive failed insertions allowed before component scale factor is reduced
void MonteCarlo::setDisorderMaxFailures(int maxfails)
{
	disorderMaxFailures_ = maxfails;
}

// Return number of successive failed insertions allowed before component scale factor is reduced
int MonteCarlo::disorderMaxFailures()
{
	return disorderMaxFailures_;
}

// Set factor by which to reduce scale factors after reaching the limit of unsuccessful insertions
void MonteCarlo::setDisorderReductionFactor(double factor)
{
	disorderReductionFactor_ = factor;
}

// Return factor by which to reduce scale factors after reaching the limit of unsuccessful insertions
double MonteCarlo::disorderReductionFactor()
{
	return disorderReductionFactor_;
}

// Set minimum scale factor to allow
void MonteCarlo::setDisorderMinimumScaleFactor(double factor)
{
	disorderMinimumScaleFactor_ = factor;
}

// Return minimum scale factor to allow
double MonteCarlo::disorderMinimumScaleFactor()
{
	return disorderMinimumScaleFactor_;
}

// Set maximum scale factor to allow
void MonteCarlo::setDisorderMaximumScaleFactor(double factor)
{
	disorderMaximumScaleFactor_ = factor;
}

// Return maximum scale factor to allow
double MonteCarlo::disorderMaximumScaleFactor()
{
	return disorderMaximumScaleFactor_;
}

// Set number of tweaks to attempt, per component, per cycle
void MonteCarlo::setDisorderNTweaks(int ntweaks)
{
	disorderNTweaks_ = ntweaks;
}

// Retur number of tweaks to attempt, per component, per cycle
int MonteCarlo::disorderNTweaks()
{
	return disorderNTweaks_;
}

// Set maximum distance to translate molecule in tweak
void MonteCarlo::setDisorderDeltaDistance(double distance)
{
	disorderDeltaDistance_ = distance;
}

// Return maximum distance to translate molecule in tweak
double MonteCarlo::disorderDeltaDistance()
{
	return disorderDeltaDistance_;
}

// Set maximum angle (each around X and Y) to rotate molecule in tweak
void MonteCarlo::setDisorderDeltaAngle(double angle)
{
	disorderDeltaAngle_ = angle;
}

// Return maximum angle (each around X and Y) to rotate molecule in tweak
double MonteCarlo::disorderDeltaAngle()
{
	return disorderDeltaAngle_;
}

// Set maximum number of recovery cycles to perform
void MonteCarlo::setDisorderRecoveryMaxCycles(int ncycles)
{
	disorderRecoveryMaxCycles_ = ncycles;
}

// Return maximum number of recovery cycles to perform
int MonteCarlo::disorderRecoveryMaxCycles()
{
	return disorderRecoveryMaxCycles_;
}

// Set maximum number of tweaks, per molecule per component, in recovery
void MonteCarlo::setDisorderRecoveryMaxTweaks(int n)
{
	disorderRecoveryMaxTweaks_ = n;
}

// Return maximum number of tweaks, per molecule per component, in recovery
int MonteCarlo::disorderRecoveryMaxTweaks()
{
	return disorderRecoveryMaxTweaks_;
}

// Set fraction of non-overlapping component molecules required for success
void MonteCarlo::setDisorderRecoveryThreshold(double d)
{
	disorderRecoveryThreshold_ = d;
}

// Return fraction of non-overlapping component molecules required for success
double MonteCarlo::disorderRecoveryThreshold()
{
	return disorderRecoveryThreshold_;
}

// MC Geometry Minimise
bool MonteCarlo::minimise(Model* srcmodel, double econ, double fcon)
{
	// Monte Carlo energy minimisation.
	// Validity of forcefield and energy setup must be performed before calling and is *not* checked here.
	Messenger::enter("MonteCarlo::minimise");
	int n, cycle, nmoves, move, mol, randpat, npats;
	double newEnergy, currentEnergy, lastPrintedEnergy, currentVdwEnergy, currentElecEnergy, phi, theta;
	double deltaMoleculeEnergy, deltaVdwEnergy, deltaElecEnergy, referenceMoleculeEnergy, referenceVdwEnergy, referenceElecEnergy;
	Vec3<double> v;
	double beta = 1.0 / (prefs.gasConstant() * temperature_);
	bool success;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	Messenger::print("Creating expression for target model...");
        if ((!srcmodel->isExpressionValid()) || (srcmodel->nAtoms() == 0))
	{
		Messenger::exit("MonteCarlo::minimise");
		return FALSE;
	}

	// Create coordinate backup model for minimisation
	Model bakmodel;
	bakmodel.copy(srcmodel);

	// Create ratio array (not per-pattern, just per move type)
	createRatioArray(1);

	Messenger::print("Beginning Monte Carlo minimise...\n");
	Messenger::print(" Step     Energy        Delta          VDW          Elec        T%%  R%%  Z%%  I%%  D%%");

	// Calculate initial reference energy
	currentEnergy = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
		Messenger::exit("MonteCarlo::minimise");
		return FALSE;
	}
	currentVdwEnergy = srcmodel->energy.vdw();
	currentElecEnergy = srcmodel->energy.electrostatic();
	lastPrintedEnergy = currentEnergy;
	Messenger::print("--     %13.6e               %13.6e %13.6e", currentEnergy,  currentVdwEnergy, currentElecEnergy);

	// Cycle through move types; try and perform nTrials_ for each; move on.
	// For each attempt, select a random molecule in a random pattern
	nmoves = 0;
	npats = srcmodel->nPatterns();
	Pattern* p = NULL;

	// Start progess indicator
	int pid = progress.initialise("Performing MC minimisation...", nCycles_ * MonteCarlo::Insert);

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
				newEnergy = srcmodel->moleculeEnergy(srcmodel, p, mol, success);

				// If the energy has gone up, undo the move.
				deltaMoleculeEnergy = newEnergy - referenceMoleculeEnergy;
				deltaVdwEnergy = srcmodel->energy.vdw() - referenceVdwEnergy;
				deltaElecEnergy = srcmodel->energy.electrostatic() - referenceElecEnergy;

				// Do we accept the move?
				if ((deltaMoleculeEnergy < acceptanceEnergy_[move]) || ( AtenMath::random() < exp(-beta*deltaMoleculeEnergy) ))
				{
					// Update energy and move counters
					currentEnergy += deltaMoleculeEnergy;
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

		if (prefs.shouldUpdateEnergy(cycle))
		{
			QString s;
			s.sprintf(" %-5i %13.6e %13.6e %13.6e %13.6e ", cycle, currentEnergy, currentEnergy-lastPrintedEnergy, currentVdwEnergy, currentElecEnergy);
			for (n=0; n<MonteCarlo::nMoveTypes; n++) s += QString::number(int(acceptanceRatio_[0][n]*100.0), 10, 3) + " ";
			Messenger::print(s);
			lastPrintedEnergy = currentEnergy;
		}
		
// 		if (prefs.shouldUpdateModel(cycle+1)) updateWidgets(AtenWindow::CanvasTarget);  ATEN2 TODO

	} // Loop over MC cycles
	progress.terminate(pid);

	// Print final energy
	newEnergy = srcmodel->totalEnergy(srcmodel, success);
	srcmodel->energy.print();

	// Finalise
	srcmodel->changeLog.add(Log::Coordinates);

	Messenger::exit("MonteCarlo::minimise");
	return TRUE;
}

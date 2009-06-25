/*
	*** Monte Carlo methods
	*** src/methods/mc.cpp
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

#include "main/aten.h"
#include "methods/mc.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "gui/gui.h"
#include "gui/disorder.h"
#include "base/pattern.h"
#include "base/sysfunc.h"

// Static Singleton
MonteCarlo mc;

// Monte Carlo move types
const char *MoveTypeKeywords[MonteCarlo::nMoveTypes] = { "Translate", "Rotate", "ZMatrix", "Insert", "Delete" };
const char *MonteCarlo::moveTypeKeyword(MonteCarlo::MoveType mt)
{
	return MoveTypeKeywords[mt];
}

MonteCarlo::MoveType MonteCarlo::moveType(const char *s)
{
	return (MonteCarlo::MoveType) enumSearch("Monte Carlo move", MonteCarlo::nMoveTypes, MoveTypeKeywords, s);
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
	vdwScale_ = 1.0;
	temperature_ = 300.0;
	nCycles_ = 100;
}

// Set maximum stepsize for MC move
void MonteCarlo::setMaxStep(MonteCarlo::MoveType m, double d)
{
	maxStep_[m] = d;
}

// Get maximum stepsize for MC move
double MonteCarlo::maxStep(MonteCarlo::MoveType m)
{
	return maxStep_[m];
}

// Set nTrials_ for MC move
void MonteCarlo::setNTrials(MonteCarlo::MoveType m, int i)
{
	nTrials_[m] = i;
}

// Get nTrials_ for MC move
int MonteCarlo::nTrials(MonteCarlo::MoveType m)
{
	return nTrials_[m];
}

// Set moveAllowed_ flag for MC move
void MonteCarlo::setMoveAllowed(MonteCarlo::MoveType m, bool b)
{
	moveAllowed_[m] = b;
}

// Get moveAllowed_ flag for MC move
bool MonteCarlo::isMoveAllowed(MonteCarlo::MoveType m)
{
	return moveAllowed_[m];
}

// Set acceptanceEnergy_ limit for MC move
void MonteCarlo::setAcceptanceEnergy(MonteCarlo::MoveType m, double d)
{
	acceptanceEnergy_[m] = d;
}

// Get acceptanceEnergy_ limit for MC move
double MonteCarlo::acceptanceEnergy(MonteCarlo::MoveType m)
{
	return acceptanceEnergy_[m];
}

// Set number of MC cycles to perform
void MonteCarlo::setNCycles(int i)
{
	nCycles_ = i;
}

// Get nTrials_ for MC move
int MonteCarlo::nCycles()
{
	return nCycles_;
}

// Sets the vDW radius scale
void MonteCarlo::setVdwScale(double d)
{
	vdwScale_ = d;
}

// Return the VDW radius scale
double MonteCarlo::vdwScale()
{
	return vdwScale_;
}

// Create ratio acceptance array
void MonteCarlo::createRatioArray(int newsize) {
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
	char s[256], t[32];
	double enew, ecurrent, currentVdwEnergy, currentElecEnergy, elast, phi, theta;
	double deltaMoleculeEnergy, deltaVdwEnergy, deltaElecEnergy, referenceMoleculeEnergy, referenceVdwEnergy, referenceElecEnergy;
	Vec3<double> v;
	double beta = 1.0 / (prefs.gasConstant() * temperature_);
	Dnchar etatext;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg.print("Creating expression for target model...\n");
        if ((!srcmodel->createExpression()) || (srcmodel->nAtoms() == 0))
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
	ecurrent = srcmodel->totalEnergy(srcmodel);
	currentVdwEnergy = srcmodel->energy.vdw();
	currentElecEnergy = srcmodel->energy.elec();
	elast = ecurrent;
	msg.print("       %13.6e               %13.6e %13.6e\n", ecurrent,  currentVdwEnergy, currentElecEnergy);

	// Cycle through move types; try and perform nTrials_ for each; move on.
	// For each attempt, select a random molecule in a random pattern
	nmoves = 0;
	npats = srcmodel->nPatterns();
	Pattern *p = NULL;

	// Start progess indicator
	if (gui.exists()) gui.progressCreate("Performing MC minimisation...", nCycles_ * MonteCarlo::Insert);

	// Loop over MC cycles
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Loop over MC moves
		for (move=0; move<MonteCarlo::Insert; ++move)
		{
			// Update progress indicator
			if (gui.exists() && (!gui.progressUpdate(-1, &etatext))) break;

			acceptanceRatio_[0][move] = 0;
			// If this move type isn't moveAllowed_ then continue onto the next
			if (!moveAllowed_[move]) continue;
			for (n=0; n<nTrials_[move]; ++n)
			{
				// Select random pattern and molecule
				do
				{
					npats != 1 ? randpat = csRandomi(npats) : randpat = 0;
					p = srcmodel->pattern(randpat);
				} while (p->nMolecules() == 0);
				mol = csRandomi(p->nMolecules());
	
				// Copy the coordinates of the current molecule
				if (p->nMolecules() != 0) bakmodel.copyAtomData(srcmodel, Atom::PositionData, p->offset(mol),p->nAtoms());

				// Calculate reference energy (before move)
				referenceMoleculeEnergy = srcmodel->moleculeEnergy(srcmodel, p, mol);
				referenceVdwEnergy = srcmodel->energy.vdw();
				referenceElecEnergy = srcmodel->energy.elec();

				// Otherwise, generate the new configuration (in model's cfg space)
				switch (move)
				{
					// Translate COG of molecule
					case (MonteCarlo::Translate):
						// Create a random translation vector
						v.randomUnit();
						v *= maxStep_[MonteCarlo::Translate]*csRandom();
						// Translate the coordinates of the molecule in cfg
						srcmodel->translateMolecule(p,mol,v);
						break;
					// Rotate molecule about COG
					case (MonteCarlo::Rotate):
						// To do the random rotation, do two separate random rotations about the x and y axes.
						phi = csRandom() * maxStep_[MonteCarlo::Rotate];
						theta = csRandom() * maxStep_[MonteCarlo::Rotate];
						srcmodel->rotateMolecule(p,mol,phi,theta);
						break;
					// Other moves....
				}

				// Get the energy of this new configuration.
				enew = srcmodel->moleculeEnergy(srcmodel, p, mol);

				// If the energy has gone up, undo the move.
				deltaMoleculeEnergy = enew - referenceMoleculeEnergy;
				deltaVdwEnergy = srcmodel->energy.vdw() - referenceVdwEnergy;
				deltaElecEnergy = srcmodel->energy.elec() - referenceElecEnergy;

				// Do we accept the move?
				if ((deltaMoleculeEnergy < acceptanceEnergy_[move]) || ( csRandom() < exp(-beta*deltaMoleculeEnergy) ))
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

		if (prefs.shouldUpdateEnergy(cycle))
		{
			sprintf(s," %-5i %13.6e %13.6e %13.6e %13.6e", cycle+1, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy);
			for (n=0; n<MonteCarlo::nMoveTypes; n++)
			{
				sprintf(t," %3i", int(acceptanceRatio_[0][n]*100.0));
				strcat(s,t);
			}
			strcat(s, "  ");
			strcat(s, etatext.get());
			strcat(s,"\n");
			msg.print(s);
			//msg.print(" %-5i %13.6e %13.6e %13.6e %13.6e", cycle+1, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy);
			//for (n=0; n<MonteCarlo::nMoveTypes; n++) msg.print(" %3i",int(acceptanceRatio_[0][n]*100.0));
			//msg.print("\n");
		}
		elast = ecurrent;

	} // Loop over MC cycles
	if (gui.exists()) gui.progressTerminate();

	// Print final energy
	enew = srcmodel->totalEnergy(srcmodel);
	srcmodel->energy.print();

	// Finalise
	srcmodel->changeLog.add(Log::Coordinates);

	msg.exit("MonteCarlo::minimise");
	return TRUE;
}

// MC Insertion
bool MonteCarlo::disorder(Model *destmodel)
{
	// Monte Carlo Insertion
	msg.enter("MonteCarlo::disorder");
	int n, m, cycle, move, mol, nOldAtoms, nOldPatterns;
	int patternNMols, prog;
	char s[256], t[32];
	Refitem<Model,int> *ri;
	Model *c;
	double enew, ecurrent, elast, phi, theta, currentVdwEnergy, currentElecEnergy;
	double deltaMoleculeEnergy, deltaVdwEnergy, deltaElecEnergy, referenceMoleculeEnergy, referenceVdwEnergy, referenceElecEnergy;
	double penalty;
	bool done;
	Cell *cell;
	Pattern *p;
	ComponentRegion *r;
	Vec3<double> v, cog;
	Clipboard clip;
	Reflist<Model,int> components;
	double beta = 1.0 / (prefs.gasConstant() * temperature_);

	// Model must have a cell to continue
	if (destmodel->cell()->type() == Cell::NoCell)
	{
		msg.print("Model must have a cell definition to be the target of a disordered build.\n");
		msg.exit("MonteCarlo::disorder");
		return FALSE;
	}

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg.print("Creating expression for target model...\n");
        if (!destmodel->createExpression())
	{
		msg.exit("MonteCarlo::disorder");
		return FALSE;
	}
	nOldPatterns = destmodel->nPatterns();
	nOldAtoms = destmodel->nAtoms();
	cell = destmodel->cell();
	// Fix all patterns in the destmodel
	destmodel->setPatternsFixed(destmodel->nPatterns());

	// Before we add in the molecule copies, check that we can create expressions for each component (and build reflist)
	msg.print("Checking component models...\n");
	for (c = aten.models(); c != NULL; c = c->next)
	{
		// Check that this model is a required component
		if (c->nRequested() < 0) continue;
		if (c->cell()->type() != Cell::NoCell) continue;
		// Add this model to the component reflist
		components.add(c);
		// TODO Autocreation of patterns may not give a 1*N pattern. Add option to force 1*N pattern.
		if (!c->createExpression())
		{
			msg.print("Failed to create expression for component model '%s'.\n", c->name());
			msg.exit("MonteCarlo::disorder");
			return FALSE;
		}
	}

	// Check that there were actually components specified
	if (components.nItems() == 0)
	{
		msg.print("No components have been specified for insertion into the model.\n");
		msg.exit("MonteCarlo::disorder");
		return FALSE;
	}

	// Autocreate expressions for component models, paste copies in to the target model, and then add a corresponding pattern node.
	msg.print("Preparing destination model...\n");
	for (ri = components.first(); ri != NULL; ri = ri->next)
	{
		c = ri->item;
		// Copy the model and paste it 'nrequested' times into destmodel
		clip.copyAll(c);
		for (mol=0; mol<c->nRequested(); mol++) clip.pasteToModel(destmodel);
		// Create a new pattern node in the destination model to cover these molecules and set it as the component's pattern
		p = destmodel->addPattern(c->nRequested(), c->nAtoms(), c->name());
		p->setNExpectedMolecules(c->nRequested());
		c->setComponentPattern(p);
		// Set the forcefield of the new pattern fo that of the source model
		p->setForcefield(c->forcefield());
        }

	// Create master expression for the new (filled) model
	if (!destmodel->createExpression())
	{
		msg.print("Couldn't create master expression for destination model.\n");
		return FALSE;
	}

	// Set starting populations of patterns, add atom space to target model, and print out pattern list info.
	msg.print("Pattern info for insertion to model '%s':\n", destmodel->name());
	msg.print("  ID  nmols  atoms  starti  finali  name\n");
	for (p = destmodel->patterns(); p != NULL; p = p->next)
	{
		// Set nmols, starti, endi, startatom and endatom in the pattern
		msg.print("  %2i  %5i  %5i  %6i  %6i  %s\n",p->id(),p->nMolecules(),p->nAtoms(),
			p->startAtom(),p->startAtom() + p->totalAtoms(),p->name());
	}

	// Reset number of molecules in component patterns to zero (except those for the original patterns of the model)
	for (p = destmodel->pattern(nOldPatterns); p != NULL; p = p->next) p->setNMolecules(0);

	// Hide unused atoms to begin with
	Atom **modelAtoms = destmodel->atomArray();
	for (n = nOldAtoms; n < destmodel->nAtoms(); n++) modelAtoms[n]->setHidden(TRUE);

	// Create a backup model
	msg.print("Preparing workspace for maximum of %i atoms...\n", destmodel->nAtoms());
	Model bakmodel;
	bakmodel.copy(destmodel);

	// Make sure intramolecular energy calculation is off (bound terms only)
	bool intrastatus = prefs.calculateIntra();
	prefs.setCalculateIntra(FALSE);

	// Create array for move acceptance ratios
	createRatioArray(destmodel->nPatterns());

	// Set VDW scale ratio
	prefs.setVdwScale(vdwScale_);

	/*
	// Calculation
	*/
	// Cycle through move types; try and perform nTrials_ for each; move on.
	// For each attempt, select a random molecule in a random pattern
	msg.print("Beginning Monte Carlo insertion...\n\n");
	msg.print(" Step     Energy        Delta          VDW          Elec         Model    N     Nreq   T%%  R%%  Z%%  I%%  D%%\n");
	// Calculate initial reference energies
	ecurrent = destmodel->totalEnergy(destmodel);
	currentVdwEnergy = destmodel->energy.vdw();
	currentElecEnergy = destmodel->energy.elec();

	elast = ecurrent;
	msg.print(" %-5i %13.6e %13s %13.6e %13.6e \n", 0, ecurrent, "     ---     ", destmodel->energy.vdw(), destmodel->energy.elec());

	// Loop over MC cycles
	if (gui.exists()) gui.progressCreate("Building disordered system", nCycles_ * components.nItems() * MonteCarlo::nMoveTypes);
	prog = 0;
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		msg.print(Messenger::Verbose,"Begin cycle %i...\n",cycle);
		done = TRUE;

		// Loop over component models searching for ones that are to be added to the target model
		for (ri = components.first(); ri != NULL; ri = ri->next)
		{
			// Get model pointer
			c = ri->item;

			// Skip if nRequested == 0, cell.type() != Cell::NoCell, and c == destmodel (to be sure).
			if ((c->nRequested() == 0) || (c->cell()->type() != Cell::NoCell) || (c == destmodel)) continue;

			// Get pointers to variables
			p = c->componentPattern();
			r = c->region();
			msg.print(Messenger::Verbose,"Working on pattern '%s'\n",p->name());
			// If the pattern is fixed, move on
			if (p->isFixed())
			{
				prog += MonteCarlo::nMoveTypes;
				msg.print(Messenger::Verbose,"Pattern '%s' is fixed.\n",p->name());
				continue;
			}
			msg.print(Messenger::Verbose,"Pattern region is '%s'.\n", ComponentRegion::regionShape(r->shape()));

			// Loop over MC moves in reverse order so we do creation / destruction first
			for (move=MonteCarlo::Delete; move>-1; move--)
			{
				prog ++;
				if (gui.exists() && (!gui.progressUpdate(prog))) break;

				acceptanceRatio_[p->id()][move] = 0;
				// If this move type isn't allowed then continue onto the next
				if (!moveAllowed_[move]) continue;
				if (!c->isMoveAllowed((MonteCarlo::MoveType) move)) continue;
				for (n=0; n<nTrials_[move]; n++)
				{
					// Reset penalty value
					penalty = 0.0;
					// Grab number of molecules currently in this pattern
					patternNMols = p->nMolecules();
					// Perform the move
					switch (move)
					{
						// New molecule
						case (MonteCarlo::Insert):
							// Check if we've already filled as many as requested
							msg.print(Messenger::Verbose,"insert : Component %s has %i molecules.\n",c->name(),patternNMols);
							if (patternNMols == p->nExpectedMolecules()) continue;
							// Paste a new molecule into the working configuration
							msg.print(Messenger::Verbose,"insert : Pasting new molecule - component %s, mol %i\n",c->name(),patternNMols);
							//clip.paste_to_model(destmodel,p,patternNMols);
							// Increase nmols for pattern and natoms for config
							mol = patternNMols;		// Points to new molecule, since m-1
							p->setNMolecules(mol+1);
							// Set the hidden flag on the new molecule to FALSE
							destmodel->hideMolecule(p,mol,FALSE);
							// Randomise position of new molecule
							v = r->randomCoords(cell,components);
							destmodel->positionMolecule(p,mol,v); 
							// Only rotate the new molecule if the component allows it
							if (c->isMoveAllowed(MonteCarlo::Rotate))
							{
								phi = csRandom() * 360.0;
								theta = csRandom() * 360.0;
								destmodel->rotateMolecule(p,mol,phi,theta);
							}
							referenceMoleculeEnergy = 0.0;
							referenceVdwEnergy = 0.0;
							referenceElecEnergy = 0.0;
							break;
						// Translate COG of molecule
						case (MonteCarlo::Translate):
							if (patternNMols == 0) continue;
							// Select random molecule, store, and move
							mol = csRandomi(patternNMols-1);
							referenceMoleculeEnergy = destmodel->moleculeEnergy(destmodel, p, mol);
							referenceVdwEnergy = destmodel->energy.vdw();
							referenceElecEnergy = destmodel->energy.elec();
							bakmodel.copyAtomData(destmodel, Atom::PositionData, p->offset(mol), p->nAtoms());
							// Create a random translation vector
							v.randomUnit();
							v *= maxStep_[MonteCarlo::Translate]*csRandom();
							// Translate the coordinates of the molecule in cfg
							destmodel->translateMolecule(p,mol,v);
							// Check new COG is inside region
							cog = p->calculateCog(mol,destmodel);
							if ((!r->coordsInRegion(cog,cell)) || r->pointOverlaps(cog,cell,components)) penalty += 1e6;
							break;
						// Rotate molecule about COG
						case (MonteCarlo::Rotate):
							if (patternNMols == 0) continue;
							// Select random molecule, store, and rotate
							mol = csRandomi(patternNMols-1);
							referenceMoleculeEnergy = destmodel->moleculeEnergy(destmodel, p, mol);
							referenceVdwEnergy = destmodel->energy.vdw();
							referenceElecEnergy = destmodel->energy.elec();
							bakmodel.copyAtomData(destmodel, Atom::PositionData, p->offset(mol), p->nAtoms());
							// Do two separate random rotations about the x and y axes.
							phi = csRandom() * maxStep_[MonteCarlo::Rotate];
							theta = csRandom() * maxStep_[MonteCarlo::Rotate];
							destmodel->rotateMolecule(p,mol,phi,theta);
							break;
						// Other moves....
					}

					// Get the energy of this new configuration.
					enew = destmodel->moleculeEnergy(destmodel, p, mol);
					// Add on any penalty value
					enew += penalty;
					// If the energy has gone up, undo the move.
					deltaMoleculeEnergy = enew - referenceMoleculeEnergy;
					deltaVdwEnergy = destmodel->energy.vdw() - referenceVdwEnergy;
					deltaElecEnergy = destmodel->energy.elec() - referenceElecEnergy;
					msg.print(Messenger::Verbose,"eNew = %f, deltaMoleculeEnergy = %f, deltaVdwEnergy = %f\n", enew, deltaMoleculeEnergy, deltaVdwEnergy);
					if ((deltaMoleculeEnergy < acceptanceEnergy_[move]) || ( csRandom() < exp(-beta*deltaMoleculeEnergy) ))
					{
						//printf("ACCEPTING MOVE : edelta = %20.14f\n",edelta);
						// Fold the molecule's atoms and recalculate its centre of geometry 
						//cfg->fold_molecule(p,mol);
						//destmodel->set_Prefs::ColourScheme(NULL);
						// Update energy and move counters
						//ecurrent = enew;
						//currentVdwEnergy = destmodel->energy.get_vdw();
						//currentElecEnergy = destmodel->energy.get_elec();
						ecurrent += deltaMoleculeEnergy;
						currentVdwEnergy += deltaVdwEnergy;
						currentElecEnergy += deltaElecEnergy;
						acceptanceRatio_[p->id()][move] ++;
					}
					else
					{
						//printf("REJECTING MOVE : edelta = %20.14f\n",edelta);
						// Revert to the previous state.
						switch (move)
						{
							case (MonteCarlo::Insert):
								// Set the hidden flag on the new molecule to TRUE
								destmodel->hideMolecule(p,mol,TRUE);
								p->setNMolecules(mol);
								break;
							case (MonteCarlo::Delete):
							default:
								destmodel->copyAtomData(&bakmodel, Atom::PositionData, p->offset(mol), p->nAtoms());
								break;
						}
					}
				}
				// Get acceptance ratio percentages
				if (nTrials_[move] != 0) acceptanceRatio_[p->id()][move] /= nTrials_[move];
			}
			// Check to see if this component has the required number of molecules
			if (c->nRequested() != p->nMolecules()) done = FALSE;
		}
		if (prefs.shouldUpdateEnergy(cycle))
		{
			// Print start of first line (current energy and difference)
			//msg.print(" %-5i %13.6e %13.6e %13.6e %13.6e   ", cycle+1, ecurrent, ecurrent-elastcycle, currentVdwEnergy, currentElecEnergy);
			for (p = destmodel->patterns(); p != NULL; p = p->next)
			{
				n = p->id();
				s[0] = '\n';
				if (p == destmodel->patterns())
				{
					sprintf(s," %-5i %13.6e %13.6e %13.6e %13.6e   %-12.12s %-4i (%-4i)", cycle+1, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy, p->name(), p->nMolecules(), p->nExpectedMolecules());
				}
				else sprintf(s,"%65s%-12.12s %-4i (%-4i)", " ", p->name(), p->nMolecules(), p->nExpectedMolecules());
				for (m=0; m<MonteCarlo::nMoveTypes; m++)
				{
					sprintf(t," %3i", int(acceptanceRatio_[n][m]*100.0));
					strcat(s,t);
				}
				strcat(s,"\n");
				msg.print(s);
			}
		}
		elast = ecurrent;
		// Check for early termination
		if (done)
		{
			msg.print("All component populations satisfied.\n");
			break;
		}
	}
	if (gui.exists()) gui.progressTerminate();
	
	// Print out final energy and data
	enew = destmodel->totalEnergy(destmodel);
	destmodel->energy.print();
	// Print out pattern list info here
	msg.print("Final populations for model '%s':\n",destmodel->name());
	msg.print("  ID  name                 nmols \n");
	for (p = destmodel->patterns(); p != NULL; p = p->next) msg.print("  %2i  %-20s  %6i\n", p->id()+1, p->name(), p->nMolecules());

	// Reset VDW scale ratio and intramolecular status
	prefs.setVdwScale(1.0);
	prefs.setCalculateIntra(intrastatus);

	// Remove all hidden atoms in the model (unused molecule space)
	Atom *i, *j;
	i = destmodel->atoms();
	while (i != NULL)
	{
		if (i->isHidden())
		{
			j = i;
			i = i->next;
			destmodel->deleteAtom(j);
		}
		else i = i->next;
	}

	// Remove number of molecules inserted from original number requested
	for (ri = components.first(); ri != NULL; ri = ri->next)
	{
		// Get model pointer
		c = ri->item;
		c->setNRequested( c->nRequested() - c->componentPattern()->nMolecules() );
	}
	// Fix pattern startAtom and endAtom (pointers to first atom are okay)
	for (p = destmodel->patterns(); p != NULL; p = p->next)
	{
		// For the first pattern, set StartAtom to zero. Otherwise, use previous pattern's endAtom.
		p->setStartAtom( p == destmodel->patterns() ? 0 : p->prev->startAtom() + p->prev->nMolecules()*p->prev->nAtoms() );
		p->setEndAtom( p->startAtom() + p->nAtoms() - 1 );
		//printf("PATTERN %li NEW START/END = %i/%i\n",p,p->startAtom(),p->endAtom());
	}
	destmodel->foldAllAtoms();
	destmodel->calculateMass();
	//if (destmodel->arePatternsValid()) printf("Patterns are valid...\n");
	//else printf("Patterns are NOT valid.\n");
	//if (destmodel->isExpressionValid()) printf("Expression is valid...\n");
	//else printf("Expression is NOT valid.\n");
	destmodel->changeLog.add(Log::Coordinates);
	gui.disorderWindow->refresh();
	gui.modelChanged();
	msg.exit("MonteCarlo::insert");
	return TRUE;
}


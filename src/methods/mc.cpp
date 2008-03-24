/*
	*** Monte Carlo methods
	*** src/methods/mc.cpp
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

#include "classes/pattern.h"
#include "classes/clipboard.h"
#include "model/model.h"
#include "methods/mc.h"
#include "base/master.h"
#include "gui/gui.h"

// Static Singleton
MethodMc mc;

// Monte Carlo move types
const char *MT_strings[MT_NITEMS] = { "Translation", "Rotation", "Z-Matrix", "Insertion", "Deletion" };
const char *MT_keywords[MT_NITEMS] = { "translate", "rotate", "zmatrix", "insert", "delete" };
const char *text_from_MT(MonteCarloMove i)
	{ return MT_strings[i]; };
MonteCarloMove MT_from_text(const char *s)
	{ return (MonteCarloMove) enumSearch("Monte Carlo move",MT_NITEMS,MT_keywords,s); }

// Constructors
MethodMc::MethodMc()
{
	// Private variables
	maxStep_[MT_TRANSLATE] = 1.0;
	maxStep_[MT_ROTATE] = 20.0;
	maxStep_[MT_ZMATRIX] = 0.0;
	nTrials_[MT_TRANSLATE] = 10;
	nTrials_[MT_ROTATE] = 10; 
	nTrials_[MT_ZMATRIX] = 0;
	nTrials_[MT_INSERT] = 20;
	nTrials_[MT_DELETE] = 0; 
	moveAllowed_[MT_TRANSLATE] = TRUE;
	moveAllowed_[MT_ROTATE] = TRUE;
	moveAllowed_[MT_ZMATRIX] = FALSE;
	moveAllowed_[MT_INSERT] = TRUE;
	moveAllowed_[MT_DELETE] = TRUE; 
	acceptanceEnergy_[MT_TRANSLATE] = 0.0;
	acceptanceEnergy_[MT_ROTATE] = 0.0;
	acceptanceEnergy_[MT_ZMATRIX] = 0.0;
	acceptanceEnergy_[MT_INSERT] = 100.0;
	acceptanceEnergy_[MT_DELETE] = 0.0;
	acceptanceRatio_ = NULL;
	acceptanceRatioSize_ = 0;
	vdwScale_ = 1.0;
	nCycles_ = 100;
}

Component::Component()
{
	// Private variables
	model_ = NULL;
	pattern_ = NULL;
	nRequested_ = 10;
	nFilled_ = 0;
	moveAllowed_[MT_INSERT] = TRUE;
	moveAllowed_[MT_DELETE] = FALSE;
	moveAllowed_[MT_TRANSLATE] = TRUE;
	moveAllowed_[MT_ROTATE] = TRUE;
	moveAllowed_[MT_ZMATRIX] = FALSE;
	// Public variables
	prev = NULL;
	next = NULL;
}

// Set the Component's model
void Component::setModel(Model *m)
{
	model_ = m;
}

// Return the Component's model
Model *Component::model()
{
	return model_;
}

// Set the Component's pattern
void Component::setPattern(Pattern *p)
{
	pattern_ = p;
}

// Return the Component's pattern
Pattern *Component::pattern()
{
	return pattern_;
}

// Set the requested number of molecules
void Component::setNRequested(int i)
{
	nRequested_ = i;
}

// Return the requested number of molecules
int Component::nRequested()
{
	return nRequested_;
}

// Set the number of molecules filled
void Component::setNFilled(int i)
{
	nFilled_ = i;
}

// Return the number of molecules filled
int Component::nFilled()
{
	return nFilled_;
}

// Set a specific move type for the Component
void Component::setMoveAllowed(MonteCarloMove m, bool b)
{
	moveAllowed_[m] = b;
}

// Set whether the Component may be translated
bool Component::isMoveAllowed(MonteCarloMove m)
{
	return moveAllowed_[m];
}

// Set name of Component
void Component::setName(const char *s)
{
	name_ = s;
}

// Get name of Component
const char *Component::name()
{
	return name_.get();
}

// Set maximum stepsize for MC move
void MethodMc::setMaxStep(MonteCarloMove m, double d)
{
	maxStep_[m] = d;
}

// Get maximum stepsize for MC move
double MethodMc::maxStep(MonteCarloMove m)
{
	return maxStep_[m];
}

// Set nTrials_ for MC move
void MethodMc::setNTrials(MonteCarloMove m, int i)
{
	nTrials_[m] = i;
}

// Get nTrials_ for MC move
int MethodMc::nTrials(MonteCarloMove m)
{
	return nTrials_[m];
}

// Set moveAllowed_ flag for MC move
void MethodMc::setMoveAllowed(MonteCarloMove m, bool b)
{
	moveAllowed_[m] = b;
}

// Get moveAllowed_ flag for MC move
bool MethodMc::isMoveAllowed(MonteCarloMove m)
{
	return moveAllowed_[m];
}

// Set acceptanceEnergy_ limit for MC move
void MethodMc::setAcceptanceEnergy(MonteCarloMove m, double d)
{
	acceptanceEnergy_[m] = d;
}

// Get acceptanceEnergy_ limit for MC move
double MethodMc::acceptanceEnergy(MonteCarloMove m)
{
	return acceptanceEnergy_[m];
}

// Set number of MC cycles to perform
void MethodMc::setNCycles(int i)
{
	nCycles_ = i;
}

// Get nTrials_ for MC move
int MethodMc::nCycles()
{
	return nCycles_;
}

// Sets the vDW radius scale
void MethodMc::setVdwScale(double d)
{
	vdwScale_ = d;
}

// Create ratio acceptance array
void MethodMc::createRatioArray(int newsize) {
	int n, m;
	if (acceptanceRatio_ != NULL)
	{
		for (n=0; n<acceptanceRatioSize_; n++) delete[] acceptanceRatio_[n];
		delete[] acceptanceRatio_;
	}
	acceptanceRatio_ = new double*[newsize];
	for (n=0; n<newsize; n++) acceptanceRatio_[n] = new double[MT_NITEMS];
	acceptanceRatioSize_ = newsize;
	// Zero the elements
	for (n=0; n<newsize; n++) for (m=0; m<MT_NITEMS; m++) acceptanceRatio_[n][m] = 0.0;
}

/*
// Component management routines
*/

// Find by ID
Component *MethodMc::componentByName(const char *search)
{
	for (Component *c = components.first(); c != NULL; c = c->next)
		if (strcmp(c->model()->name(),search) == 0) return c;
	return NULL;
}

// MC Geometry Minimise
bool MethodMc::minimise(Model* srcmodel, double econ, double fcon)
{
	// Monte Carlo energy minimisation.
	// Validity of forcefield and energy setup must be performed before calling and is *not* checked here.
	dbgBegin(DM_CALLS,"mc::minimise");
	int n, cycle, nmoves, move, mol, randpat, npats, prog;
	char s[256], t[32];
	double enew, ecurrent, currentVdwEnergy, currentElecEnergy, elast, phi, theta;
	double deltaMoleculeEnergy, deltaVdwEnergy, deltaElecEnergy, referenceMoleculeEnergy, referenceVdwEnergy, referenceElecEnergy;
	Vec3<double> v;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg(DM_NONE,"Creating expression for target model...\n");
        if (!srcmodel->createExpression())
	{
		dbgEnd(DM_CALLS,"mc::minimise");
		return FALSE;
	}
	srcmodel->assignCharges(prefs.chargeSource());

	// Create coordinate backup model for minimisation
	Model bakmodel;
	bakmodel.copy(srcmodel);

	// Create ratio array (not per-pattern, just per move type)
	createRatioArray(1);

	msg(DM_NONE,"Beginning Monte Carlo minimise...\n\n");
	msg(DM_NONE," Step     Energy        Delta          VDW          Elec              T%%  R%%  Z%%  I%%  D%%\n");

	// Calculate initial reference energy
	ecurrent = srcmodel->totalEnergy(srcmodel);
	currentVdwEnergy = srcmodel->energy.vdw();
	currentElecEnergy = srcmodel->energy.elec();
	elast = ecurrent;
	msg(DM_NONE,"       %13.6e               %13.6e %13.6e\n", ecurrent,  currentVdwEnergy, currentElecEnergy);

	// Cycle through move types; try and perform nTrials_ for each; move on.
	// For each attempt, select a random molecule in a random pattern
	nmoves = 0;
	npats = srcmodel->nPatterns();
	Pattern *p = NULL;
	prog = 0;

	// Start progess indicator
	if (gui.exists()) gui.progressCreate("Performing MC minimisation...", nCycles_ * npats * MT_NITEMS);

	// Loop over MC cycles
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Loop over MC moves
		for (move=0; move<MT_INSERT; move++)
		{
			// Update progress indicator
			prog ++;
			if (gui.exists() && (!gui.progressUpdate(prog))) break;

			acceptanceRatio_[0][move] = 0;
			// If this move type isn't moveAllowed_ then continue onto the next
			if (!moveAllowed_[move]) continue;
			for (n=0; n<nTrials_[move]; n++)
			{
				// Select random pattern and molecule
				npats != 1 ? randpat = csRandomi(npats) : randpat = 0;
				p = srcmodel->pattern(randpat);
				mol = csRandomi(p->nMols());
	
				// Copy the coordinates of the current molecule
				if (p->nMols() != 0) bakmodel.copyAtomData(srcmodel, AD_R, p->offset(mol),p->nAtoms());

				// Calculate reference energy (before move)
				referenceMoleculeEnergy = srcmodel->moleculeEnergy(srcmodel, p, mol);
				referenceVdwEnergy = srcmodel->energy.vdw();
				referenceElecEnergy = srcmodel->energy.elec();

				// Otherwise, generate the new configuration (in model's cfg space)
				switch (move)
				{
					// Translate COG of molecule
					case (MT_TRANSLATE):
						// Create a random translation vector
						v.randomUnit();
						v *= maxStep_[MT_TRANSLATE]*csRandom();
						// Translate the coordinates of the molecule in cfg
						srcmodel->translateMolecule(p,mol,v);
						break;
					// Rotate molecule about COG
					case (MT_ROTATE):
						// To do the random rotation, do two separate random rotations about the x and y axes.
						phi = csRandom() * maxStep_[MT_ROTATE];
						theta = csRandom() * maxStep_[MT_ROTATE];
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

				// If the energy has gone up, undo the move.
				if (deltaMoleculeEnergy > acceptanceEnergy_[move])
				{
					// Put the molecules back to where it was before
					srcmodel->copyAtomData(&bakmodel, AD_R, p->offset(mol), p->nAtoms());
				}
				else
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
			}
			if (nTrials_[move] != 0) acceptanceRatio_[0][move] /= nTrials_[move];
		} // Loop over MC moves

		gui.processEvents();

		if (prefs.shouldUpdateEnergy(cycle))
		{
			msg(DM_NONE," %-5i %13.6e %13.6e %13.6e %13.6e", cycle+1, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy);
			for (n=0; n<MT_NITEMS; n++) msg(DM_NONE," %3i",int(acceptanceRatio_[0][n]*100.0));
			msg(DM_NONE,"\n");
		}
		elast = ecurrent;

	} // Loop over MC cycles
	if (gui.exists()) gui.progressTerminate();

	// Print final energy
	enew = srcmodel->totalEnergy(srcmodel);
	srcmodel->energy.print();

	// Finalise
	srcmodel->logChange(LOG_COORDS);

	dbgEnd(DM_CALLS,"mc::minimise");
	return TRUE;
}

// MC Insertion
bool MethodMc::disorder(Model *destmodel)
{
	// Monte Carlo Insertion
	dbgBegin(DM_CALLS,"mc::disorder");
	int n, m, cycle, move, mol, nOldAtoms, nOldPatterns;
	int patternNMols, prog;
	char s[256], t[32];
	Component *c;
	double enew, ecurrent, elast, phi, theta, currentVdwEnergy, currentElecEnergy;
	double deltaMoleculeEnergy, deltaVdwEnergy, deltaElecEnergy, referenceMoleculeEnergy, referenceVdwEnergy, referenceElecEnergy;
	double penalty;
	bool done;
	Cell *cell;
	Pattern *p;
	ComponentRegion *r;
	Vec3<double> v, cog;
	Clipboard clip;

	/*
	// Prepare the calculation
	*/
        // First, create expression for the current model and assign charges
	msg(DM_NONE,"Creating expression for target model...\n");
        if (!destmodel->createExpression())
	{
		dbgEnd(DM_CALLS,"mc::disorder");
		return FALSE;
	}
	nOldPatterns = destmodel->nPatterns();
	nOldAtoms = destmodel->nAtoms();
	cell = destmodel->cell();
	// Fix all patterns in the destmodel
	destmodel->setPatternsFixed(destmodel->nPatterns());

	// Check that there were actually components specified
	if (components.nItems() == 0)
	{
		msg(DM_NONE,"No components have been specified for inclusion into the model.\n");
		dbgEnd(DM_CALLS,"mc::disorder");
		return FALSE;
	}

	// Autocreate expressions for component models, paste copies in to the target model, and then add a corresponding pattern node.
	msg(DM_NONE,"Preparing destination model...\n");
	for (c = components.first(); c != NULL; c = c->next)
	{
		// Grab the model pointer for this component and set the number filled to zero
		Model *m = c->model();
		c->setNFilled(0);
		// Check that we can create a suitable expression for the component model
		if (!m->createExpression())
		{
			msg(DM_NONE,"Failed to create expression for component model '%s'.\n", m->name());
			dbgEnd(DM_CALLS,"mc::disorder");
			return FALSE;
		}
		// TODO Autocreation of patterns may not give a 1*N pattern. Add option to force 1*N pattern.
		// Copy the model and paste it 'nrequested' times into destmodel
		clip.copyAll(m);
		for (mol=0; mol<c->nRequested(); mol++) clip.pasteToModel(destmodel);
		// Create a new pattern node in the destination model to cover these molecules and set it as the component's pattern
		p = destmodel->addPattern(c->nRequested(), m->nAtoms(), m->name());
		p->setNExpectedMols(c->nRequested());
		c->setPattern(p);
		// Set the forcefield of the new pattern fo that of the source model
		p->setForcefield(m->forcefield());
        }

	// Create master expression for the new (filled) model
	if (!destmodel->createExpression())
	{
		msg(DM_NONE,"Couldn't create master expression for destination model.\n");
		return FALSE;
	}
	// Set starting populations of patterns, add atom space to target model, and print out pattern list info.
	msg(DM_NONE,"Pattern info for insertion to model '%s':\n",destmodel->name());
	msg(DM_NONE,"  ID  nmols  atoms  starti  finali  name\n");
	for (p = destmodel->patterns(); p != NULL; p = p->next)
	{
		// Set nmols, starti, endi, startatom and endatom in the pattern
		msg(DM_NONE,"  %2i  %5i  %5i  %6i  %6i  %s\n",p->id(),p->nMols(),p->nAtoms(),
			p->startAtom(),p->startAtom() + p->totalAtoms(),p->name());
	}

	// Reset number of molecules in component patterns to zero (except those for the original patterns of the model)
	for (p = destmodel->pattern(nOldPatterns); p != NULL; p = p->next) p->setNMols(0);

	// Hide unused atoms to begin with
	Atom **modelAtoms = destmodel->atomArray();
	for (n = nOldAtoms; n < destmodel->nAtoms(); n++) modelAtoms[n]->setHidden(TRUE);

	// Create a backup model
	msg(DM_NONE,"Preparing workspace for maximum of %i atoms...\n", destmodel->nAtoms());
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
	msg(DM_NONE,"Beginning Monte Carlo insertion...\n\n");
	msg(DM_NONE," Step     Energy        Delta          VDW          Elec         Model    N     Nreq   T%%  R%%  Z%%  I%%  D%%\n");
	// Calculate initial reference energies
	ecurrent = destmodel->totalEnergy(destmodel);
	currentVdwEnergy = destmodel->energy.vdw();
	currentElecEnergy = destmodel->energy.elec();

	elast = ecurrent;
	msg(DM_NONE," %-5i %13.6e %13s %13.6e %13.6e \n", 0, ecurrent, "     ---     ", destmodel->energy.vdw(), destmodel->energy.elec());

	// Loop over MC cycles
	if (gui.exists()) gui.progressCreate("Building disordered system", nCycles_ * components.nItems() * MT_NITEMS);
	prog = 0;
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		msg(DM_VERBOSE,"Begin cycle %i...\n",cycle);

		// Loop over patterns and regions together
		for (c = components.first(); c != NULL; c = c->next)
		{
			// Get pointers to variables
			p = c->pattern();
			r = &c->area;
			msg(DM_VERBOSE,"Working on pattern '%s'\n",p->name());
			// If the pattern is fixed, move on
			if (p->isFixed())
			{
				prog += MT_NITEMS;
				msg(DM_VERBOSE,"Pattern '%s' is fixed.\n",p->name());
				continue;
			}
			msg(DM_VERBOSE,"Pattern region is '%s'.\n",text_from_RS(r->shape()));

			// Loop over MC moves in reverse order so we do creation / destruction first
			for (move=MT_DELETE; move>-1; move--)
			{
				prog ++;
				if (gui.exists() && (!gui.progressUpdate(prog))) break;

				acceptanceRatio_[p->id()][move] = 0;
				// If this move type isn't moveAllowed_ then continue onto the next
				if (!moveAllowed_[move]) continue;
				//if (!comp->get_moveAllowed_((type) move)) continue;
				for (n=0; n<nTrials_[move]; n++)
				{
					// Reset penalty value
					penalty = 0.0;
					// Grab number of molecules currently in this pattern
					patternNMols = p->nMols();
					// Perform the move
					switch (move)
					{
						// New molecule
						case (MT_INSERT):
							// Check if we've already filled as many as requested
							msg(DM_VERBOSE,"insert : Pattern %s has %i molecules.\n",p->name(),patternNMols);
							if (patternNMols == p->nExpectedMols()) continue;
							// Paste a new molecule into the working configuration
							msg(DM_VERBOSE,"insert : Pasting new molecule - pattern %s, mol %i\n",p->name(),patternNMols);
							//clip.paste_to_model(destmodel,p,patternNMols);
							// Increase nmols for pattern and natoms for config
							mol = patternNMols;		// Points to new molecule, since m-1
							p->setNMols(mol+1);
							// Set the hidden flag on the new molecule to FALSE
							destmodel->hideMolecule(p,mol,FALSE);
							// Randomise position of new molecule
							v = r->randomCoords(cell,components.first());
							destmodel->positionMolecule(p,mol,v); 
							// Only rotate the new molecule if the component allows it
							// TODO if (!comp->get_moveAllowed_(MT_ROTATE)) break;
							if (moveAllowed_[MT_ROTATE])
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
						case (MT_TRANSLATE):
							if (patternNMols == 0) continue;
							// Select random molecule, store, and move
							mol = csRandomi(patternNMols-1);
							referenceMoleculeEnergy = destmodel->moleculeEnergy(destmodel, p, mol);
							referenceVdwEnergy = destmodel->energy.vdw();
							referenceElecEnergy = destmodel->energy.elec();
							bakmodel.copyAtomData(destmodel, AD_R, p->offset(mol), p->nAtoms());
							// Create a random translation vector
							v.randomUnit();
							v *= maxStep_[MT_TRANSLATE]*csRandom();
							// Translate the coordinates of the molecule in cfg
							destmodel->translateMolecule(p,mol,v);
							// Check new COG is inside region
							cog = p->calculateCog(destmodel,mol);
							if ((!r->checkCoords(cog,cell)) || r->checkOverlap(cog,cell,components.first())) penalty += 1e6;
							break;
						// Rotate molecule about COG
						case (MT_ROTATE):
							if (patternNMols == 0) continue;
							// Select random molecule, store, and rotate
							mol = csRandomi(patternNMols-1);
							referenceMoleculeEnergy = destmodel->moleculeEnergy(destmodel, p, mol);
							referenceVdwEnergy = destmodel->energy.vdw();
							referenceElecEnergy = destmodel->energy.elec();
							bakmodel.copyAtomData(destmodel, AD_R, p->offset(mol), p->nAtoms());
							// Do two separate random rotations about the x and y axes.
							phi = csRandom() * maxStep_[MT_ROTATE];
							theta = csRandom() * maxStep_[MT_ROTATE];
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
				//	printf("enew = %f, eref = %f, edelta = %f\n",enew, eref, edelta);
					if (deltaMoleculeEnergy > acceptanceEnergy_[move])
					{
						//printf("REJECTING MOVE : edelta = %20.14f\n",edelta);
						// Revert to the previous state.
						switch (move)
						{
							case (MT_INSERT):
								// Set the hidden flag on the new molecule to TRUE
								destmodel->hideMolecule(p,mol,TRUE);
								p->setNMols(mol);
								break;
							case (MT_DELETE):
							default:
								destmodel->copyAtomData(&bakmodel, AD_R, p->offset(mol), p->nAtoms());
								break;
						}
					}
					else
					{
						//printf("ACCEPTING MOVE : edelta = %20.14f\n",edelta);
						// Fold the molecule's atoms and recalculate its centre of geometry 
						//cfg->fold_molecule(p,mol);
						//destmodel->set_AtomColours(NULL);
						// Update energy and move counters
						//ecurrent = enew;
						//currentVdwEnergy = destmodel->energy.get_vdw();
						//currentElecEnergy = destmodel->energy.get_elec();
						ecurrent += deltaMoleculeEnergy;
						currentVdwEnergy += deltaVdwEnergy;
						currentElecEnergy += deltaElecEnergy;
						acceptanceRatio_[p->id()][move] ++;
					}
					gui.processEvents();
				}
				// Get acceptance ratio percentages
				if (nTrials_[move] != 0) acceptanceRatio_[p->id()][move] /= nTrials_[move];
			}
		}
		if (prefs.shouldUpdateEnergy(cycle))
		{
			// Print start of first line (current energy and difference)
			//msg(DM_NONE," %-5i %13.6e %13.6e %13.6e %13.6e   ", cycle+1, ecurrent, ecurrent-elastcycle, currentVdwEnergy, currentElecEnergy);
			for (p = destmodel->patterns(); p != NULL; p = p->next)
			{
				n = p->id();
				s[0] = '\n';
				if (p == destmodel->patterns())
				{
					sprintf(s," %-5i %13.6e %13.6e %13.6e %13.6e   %-8s %-4i (%-4i)", cycle+1, ecurrent, ecurrent-elast, currentVdwEnergy, currentElecEnergy, p->name(), p->nMols(), p->nExpectedMols());
				}
				else sprintf(s,"%65s%-8s %-4i (%-4i)", " ", p->name(), p->nMols(), p->nExpectedMols());
				for (m=0; m<MT_NITEMS; m++)
				{
					sprintf(t," %3i", int(acceptanceRatio_[n][m]*100.0));
					strcat(s,t);
				}
				strcat(s,"\n");
				msg(DM_NONE,s);
			}
		}
		elast = ecurrent;
		// Check for early termination
		done = TRUE;
		for (c = components.first(); c != NULL; c = c->next)
		{
			// Get pointers to variables
			p = c->pattern();
			if (p->nMols() != p->nExpectedMols()) done = FALSE;
		}
		if (done)
		{
			msg(DM_NONE,"All component populations satisfied.\n");
			break;
		}
	}
	if (gui.exists()) gui.progressTerminate();
	
	// Print out final energy and data
	enew = destmodel->totalEnergy(destmodel);
	destmodel->energy.print();
	// Print out pattern list info here
	msg(DM_NONE,"Final populations for model '%s':\n",destmodel->name());
	msg(DM_NONE,"  ID  name                 nmols \n");
	p = destmodel->patterns();
	while (p != NULL)
	{
		msg(DM_NONE,"  %2i  %-20s  %6i\n",p->id(),p->name(),p->nMols());
		p = p->next;
	}

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
	//bakmodel.copy(destmodel);
	//destmodel->recreate_from_patterns(&bakmodel);
	//destmodel->renderFromSelf();
	destmodel->foldAllAtoms();
	destmodel->calculateMass();
	destmodel->calculateDensity();
	destmodel->logChange(LOG_COORDS);
	gui.refresh();
	dbgEnd(DM_CALLS,"mc::insert");
	return TRUE;
}


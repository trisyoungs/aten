/*
	*** Model energy and force calculation
	*** src/model/energy.cpp
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

#include "model/model.h"
#include "classes/pattern.h"
#include "classes/energystore.h"
#include "classes/forcefield.h"
#include "energy/fourier.h"
#include "base/prefs.h"
#include "base/elements.h"

// Calculate total energy of model (from supplied coordinates)
double Model::totalEnergy(Model *srcmodel)
{
	msg.enter("Model::totalEnergy");
	// Check the expression validity
	if (!isExpressionValid())
	{
		msg.print("Model::totalEnergy - No valid energy expression defined for model.\n");
		msg.exit("Model::totalEnergy");
		return 0.0;
	}
	// Clear the energy store
	energy.clear();
	// Cycle through patterns, calculating the contributions from each
	Pattern *p, *p2;
	p = patterns_.first();
	// Calculate VDW correction
	if (prefs.calculateVdw() && (cell_.type() != Cell::NoCell)) p->vdwCorrectEnergy(&cell_, &energy);
	// Prepare Ewald (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	if (prefs.calculateElec())
	{
		if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&srcmodel->cell_);
		// Create the fourier space for use in the Ewald sum
		if (emodel != Electrostatics::Coulomb) fourier.prepare(srcmodel,prefs.ewaldKvec());
	}
	while (p != NULL)
	{
		// Intramolecular Interactions
		if (prefs.calculateIntra())
		{
			p->bondEnergy(srcmodel, &energy);
			p->angleEnergy(srcmodel, &energy);
			p->torsionEnergy(srcmodel, &energy);
		}
		// Van der Waals Interactions
		if (prefs.calculateVdw())
		{
			p->vdwIntraPatternEnergy(srcmodel, &energy);
			for (p2 = p; p2 != NULL; p2 = p2->next)
				p->vdwInterPatternEnergy(srcmodel, p2, &energy);
		}
		// Electrostatic Interactions
		if (prefs.calculateElec())
		{
			switch (emodel)
			{
				case (Electrostatics::None):
					msg.print("Electrostatics requested but no method of calculation chosen!\n");
					break;
				case (Electrostatics::Coulomb):
					p->coulombIntraPatternEnergy(srcmodel,&energy);
					for (p2 = p; p2 != NULL; p2 = p2->next) p->coulombInterPatternEnergy(srcmodel,p2,&energy);
					break;
				default: // Ewald
					p->ewaldRealIntraPatternEnergy(srcmodel,&energy);
					p->ewaldCorrectEnergy(srcmodel,&energy);
					for (p2 = p; p2 != NULL; p2 = p2->next) p->ewaldRealInterPatternEnergy(srcmodel,p2,&energy);
					// Calculate reciprocal space part (called once from first pattern only)
					if (p == patterns_.first())
						p->ewaldReciprocalEnergy(srcmodel,p,patterns_.nItems(),&energy);
					break;
			}
		}
		p = p->next;
	}
	energy.totalise();
	msg.exit("Model::totalEnergy");
	return energy.total();
}

// Calculate total interaction energy of specified molecule with remainder of model
double Model::moleculeEnergy(Model *srcmodel, Pattern *molpattern, int molecule)
{
	msg.enter("Model::moleculeEnergy");
	// Check the expression validity
	if (!isExpressionValid())
	{
		msg.print("Model::moleculeEnergy - No valid energy expression defined for model.\n");
		msg.exit("Model::moleculeEnergy");
		return 0.0;
	}
	// Clear the energy store
	energy.clear();
	Pattern *p;
	// Prepare Ewald (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	if (prefs.calculateElec())
	{
		if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&srcmodel->cell_);
		// Create the fourier space for use in the Ewald sum
		if (emodel != Electrostatics::Coulomb) fourier.prepare(srcmodel,prefs.ewaldKvec());
	}
	// Calculate VDW interactions between 'molecule' in pattern 'molpattern' and molecules in it and other's patterns
	for (p = patterns_.first(); p != NULL; p = p->next)
		molpattern->vdwInterPatternEnergy(srcmodel, p, &energy, molecule);
	// Electrostatic Interactions between 'molecule' in pattern 'molpattern' and molecules in it and other's patterns
	if (prefs.calculateElec())
	{
		switch (emodel)
		{
			case (Electrostatics::None):
				msg.print("Electrostatics requested but no method of calculation chosen!\n");
				break;
			case (Electrostatics::Coulomb):
				for (p = patterns_.first(); p != NULL; p = p->next) molpattern->coulombInterPatternEnergy(srcmodel,p,&energy);
				break;
			default: // Ewald
				for (p = patterns_.first(); p != NULL; p = p->next) p->ewaldRealInterPatternEnergy(srcmodel,p,&energy);
				// Calculate reciprocal space part (called once from first pattern only)
				if (p == patterns_.first())
					p->ewaldReciprocalEnergy(srcmodel,p,patterns_.nItems(),&energy);
				break;
		}
	}
	energy.totalise();
	msg.exit("Model::moleculeEnergy");
	return energy.total();
}

// Calculate forces from specified config
void Model::calculateForces(Model *srcmodel)
{
	// Calculate the forces for the atoms of 'srcmodel' from the expression defined in the *this model
	msg.enter("Model::calculateForces");
	// Check the expression validity
	if (!isExpressionValid())
	{
		msg.print("calculateForces : No valid energy expression defined for model.\n");
		msg.exit("Model::calculateForces");
		return;
	}
	srcmodel->zeroForces();
	// Cycle through patterns, calculate the intrapattern forces for each
	Pattern *p, *p2;
	p = patterns_.first();
	// Prepare Ewald (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	if (prefs.calculateElec())
	{
		if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&srcmodel->cell_);
		// Create the fourier space for use in the Ewald sum
		if (emodel != Electrostatics::Coulomb) fourier.prepare(srcmodel,prefs.ewaldKvec());
	}
	while (p != NULL)
	{
		// Bonded Interactions
		if (prefs.calculateIntra())
		{
			p->bondForces(srcmodel);
			p->angleForces(srcmodel);
			p->torsionForces(srcmodel);
		}
		// VDW
		if (prefs.calculateVdw())
		{
			p->vdwIntraPatternForces(srcmodel);
			for (p2 = p; p2 != NULL; p2 = p2->next)	p->vdwInterPatternForces(srcmodel,p2);
		}
		// Electrostatics
		if (prefs.calculateElec())
		{
			switch (emodel)
			{
				case (Electrostatics::None):
					msg.print("Electrostatics requested but no method of calculation chosen!\n");
					break;
				case (Electrostatics::Coulomb):
					p->coulombIntraPatternForces(srcmodel);
					for (p2 = p; p2 != NULL; p2 = p2->next) p->coulombInterPatternForces(srcmodel,p2);
					break;
				default: // Ewald
					p->ewaldRealIntraPatternForces(srcmodel);
					p->ewaldCorrectForces(srcmodel);
					for (p2 = p; p2 != NULL; p2 = p2->next) p->ewaldRealInterPatternForces(srcmodel,p2);
					// Calculate reciprocal space part (called once from first pattern only)
					if (p == patterns_.first()) p->ewaldReciprocalForces(srcmodel);
					break;
			}
		}
		p = p->next;
	}
	msg.exit("Model::calculateForces");
}


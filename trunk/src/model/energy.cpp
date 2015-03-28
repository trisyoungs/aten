/*
	*** Model energy and force calculation
	*** src/model/energy.cpp
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
#include "base/pattern.h"
#include "ff/fourier.h"

ATEN_USING_NAMESPACE

// Calculate total energy of model (from supplied coordinates)
double Model::totalEnergy(Model* srcmodel, bool& success)
{
	Messenger::enter("Model::totalEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::totalEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::totalEnergy");
		return 0.0;
	}

	// Clear the energy store
	energy.clear();

	// Cycle through patterns, calculating the contributions from each
	Pattern* p, *p2;
	p = patterns_.first();

	// Calculate VDW correction
	if (prefs.calculateVdw() && (cell_.type() != UnitCell::NoCell))
	{
		if (!p->vdwCorrectEnergy(&cell_, &energy))
		{
			success = FALSE;
			Messenger::exit("Model::totalEnergy");
			return 0.0;
		}
	}
	
	// Prepare Ewald sum (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	if ((emodel == Electrostatics::Ewald) || (emodel == Electrostatics::EwaldAuto))
	{
		// Only valid for a periodic system...
		if (srcmodel->cell_.type() == UnitCell::NoCell)
		{
			Messenger::print("Error: Ewald sum is not applicable to non-periodic models.");
			success = FALSE;
			Messenger::exit("Model::moleculeEnergy");
			return 0.0;
		}
		// Estimate parameters if automatic mode selected
		if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&srcmodel->cell_);
		fourier.prepare(srcmodel,prefs.ewaldKMax());
	}
	
	// Loop over patterns
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
			if (!p->vdwIntraPatternEnergy(srcmodel, &energy))
			{
				success = FALSE;
				Messenger::exit("Model::totalEnergy");
				return 0.0;
			}
			for (p2 = p; p2 != NULL; p2 = p2->next)
			{
				if (!p->vdwInterPatternEnergy(srcmodel, p2, &energy))
				{
					success = FALSE;
					Messenger::exit("Model::totalEnergy");
					return 0.0;
				}
			}
		}
		// Electrostatic Interactions
		switch (emodel)
		{
			case (Electrostatics::None):
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
		p = p->next;
	}
	energy.totalise();
	success = TRUE;
	Messenger::exit("Model::totalEnergy");
	return energy.total();
}

// Calculate total interaction energy of specified molecule with remainder of model
double Model::moleculeEnergy(Model* srcmodel, Pattern* molpattern, int molecule, bool& success)
{
	Messenger::enter("Model::moleculeEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::moleculeEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::moleculeEnergy");
		return 0.0;
	}
	// Clear the energy store
	energy.clear();
	Pattern* p;
	
	// Prepare Ewald (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	switch (emodel)
	{
		case (Electrostatics::None):
		case (Electrostatics::Coulomb):
			break;
		case (Electrostatics::Ewald):
		case (Electrostatics::EwaldAuto):
			// Only valid for a periodic system...
			if (srcmodel->cell_.type() == UnitCell::NoCell)
			{
				Messenger::print("Error: Ewald sum is not applicable to non-periodic models.");
				success = FALSE;
				Messenger::exit("Model::moleculeEnergy");
				return 0.0;
			}
			// Estimate parameters if automatic mode selected
			if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&srcmodel->cell_);
			 fourier.prepare(srcmodel,prefs.ewaldKMax());
			break;
	}
	
	// Calculate VDW interactions between 'molecule' in pattern 'molpattern' and molecules in it and other's patterns
	for (p = patterns_.first(); p != NULL; p = p->next)
	{
		if (!molpattern->vdwInterPatternEnergy(srcmodel, p, &energy, molecule))
		{
			success = FALSE;
			Messenger::exit("Model::moleculeEnergy");
			return 0.0;
		}
	}
	
	// Electrostatic Interactions between 'molecule' in pattern 'molpattern' and molecules in it and other's patterns
	switch (emodel)
	{
		case (Electrostatics::None):
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

	energy.totalise();
	success = TRUE;
	Messenger::exit("Model::moleculeEnergy");
	return energy.total();
}

// Calculate and return the total angle energy of the model
double Model::angleEnergy(Model* config, bool& success)
{
	Messenger::enter("Model::angleEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::angleEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::angleEnergy");
		return 0.0;
	}
	
	// Calculate total angle energy over all patterns
	EnergyStore tempenergy(patterns_.nItems());
	for (Pattern* p = patterns_.first(); p != NULL; p = p->next) p->angleEnergy(config, &tempenergy);
	
	success = TRUE;
	tempenergy.totalise();
	Messenger::exit("Model::angleEnergy");
	return tempenergy.angle();
}

// Calculate and return the total bond energy of the model
double Model::bondEnergy(Model* config, bool& success)
{
	Messenger::enter("Model::bondEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::bondEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::bondEnergy");
		return 0.0;
	}
	
	// Calculate total bond energy over all patterns
	EnergyStore tempenergy(patterns_.nItems());
	for (Pattern* p = patterns_.first(); p != NULL; p = p->next) p->bondEnergy(config, &tempenergy);
	
	success = TRUE;
	tempenergy.totalise();
	Messenger::exit("Model::bondEnergy");
	return tempenergy.bond();
}

// Calculate and return the total coulomb energy of the model
double Model::electrostaticEnergy(Model* config, bool& success)
{
	Messenger::enter("Model::coulombEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::coulombEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::coulombEnergy");
		return 0.0;
	}
	
	// Prepare Ewald (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	switch (emodel)
	{
		case (Electrostatics::None):
		case (Electrostatics::Coulomb):
			break;
		case (Electrostatics::Ewald):
		case (Electrostatics::EwaldAuto):
			// Only valid for a periodic system...
			if (config->cell_.type() == UnitCell::NoCell)
			{
				Messenger::print("Error: Ewald sum is not applicable to non-periodic models.");
				success = FALSE;
				Messenger::exit("Model::coulombEnergy");
				return 0.0;
			}
			// Estimate parameters if automatic mode selected
			if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&config->cell_);
			fourier.prepare(config,prefs.ewaldKMax());
			break;
	}
	
	// Calculate total torsion energy over all patterns
	EnergyStore tempenergy(patterns_.nItems());
	Pattern* p2;
	for (Pattern* p = patterns_.first(); p != NULL; p = p->next)
	{
		switch (emodel)
		{
			case (Electrostatics::None):
				break;
			case (Electrostatics::Coulomb):
				p->coulombIntraPatternEnergy(config, &tempenergy);
				for (p2 = p; p2 != NULL; p2 = p2->next) p->coulombInterPatternEnergy(config, p2, &tempenergy);
				break;
			default: // Ewald
				p->ewaldRealIntraPatternEnergy(config, &tempenergy);
				p->ewaldCorrectEnergy(config, &tempenergy);
				for (p2 = p; p2 != NULL; p2 = p2->next) p->ewaldRealInterPatternEnergy(config, p2, &tempenergy);
				// Calculate reciprocal space part (called once from first pattern only)
				if (p == patterns_.first()) p->ewaldReciprocalEnergy(config, p, patterns_.nItems(), &tempenergy);
				break;
		}
	}
	success = TRUE;
	tempenergy.totalise();
	Messenger::exit("Model::coulombEnergy");
	return tempenergy.electrostatic();
}

// Calculate and return the total intermolecular energy of the model
double Model::intermolecularEnergy(Model* config, bool& success)
{
	double interenergy = vdwEnergy(config, success);
	if (!success) return 0.0;
	interenergy += electrostaticEnergy(config, success);
	if (!success) return 0.0;
	return interenergy;
}

// Calculate and return the total intramolecular energy of the model
double Model::intramolecularEnergy(Model* config, bool& success)
{
	double intraenergy = bondEnergy(config, success);
	if (!success) return 0.0;
	intraenergy += angleEnergy(config, success);
	if (!success) return 0.0;
	intraenergy += torsionEnergy(config, success);
	if (!success) return 0.0;
	return intraenergy;
}

// Calculate and return the total torsion energy of the model
double Model::torsionEnergy(Model* config, bool& success)
{
	Messenger::enter("Model::torsionEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::torsionEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::torsionEnergy");
		return 0.0;
	}
	
	// Calculate total torsion energy over all patterns
	EnergyStore tempenergy(patterns_.nItems());
	for (Pattern* p = patterns_.first(); p != NULL; p = p->next) p->torsionEnergy(config, &tempenergy);
	
	success = TRUE;
	tempenergy.totalise();
	Messenger::exit("Model::torsionEnergy");
	return tempenergy.torsion();
}

// Calculate and return the total van der Waals energy of the model
double Model::vdwEnergy(Model* config, bool& success)
{
	Messenger::enter("Model::vdwEnergy");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("Model::vdwEnergy - No valid energy expression defined for model.");
		success = FALSE;
		Messenger::exit("Model::vdwEnergy");
		return 0.0;
	}
	
	// Calculate total torsion energy over all patterns
	EnergyStore tempenergy(patterns_.nItems());
	Pattern* p2;
	for (Pattern* p = patterns_.first(); p != NULL; p = p->next)
	{
		if (!p->vdwIntraPatternEnergy(config, &tempenergy))
		{
			success = FALSE;
			Messenger::exit("Model::totalEnergy");
			return 0.0;
		}
		for (p2 = p; p2 != NULL; p2 = p2->next)
		{
			if (!p->vdwInterPatternEnergy(config, p2, &tempenergy))
			{
				success = FALSE;
				Messenger::exit("Model::totalEnergy");
				return 0.0;
			}
		}
	}
	
	success = TRUE;
	tempenergy.totalise();
	Messenger::exit("Model::vdwEnergy");
	return tempenergy.vdw();
}

// Calculate forces from specified config
bool Model::calculateForces(Model* srcmodel)
{
	// Calculate the forces for the atoms of 'srcmodel' from the expression defined in the *this model
	Messenger::enter("Model::calculateForces");

	// Check the expression validity
	if (!isExpressionValid())
	{
		Messenger::print("calculateForces : No valid energy expression defined for model.");
		Messenger::exit("Model::calculateForces");
		return FALSE;
	}
	srcmodel->zeroForces();

	// Cycle through patterns, calculate the intrapattern forces for each
	Pattern* p, *p2;
	p = patterns_.first();
	
	// Prepare Ewald (if necessary)
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	switch (emodel)
	{
		case (Electrostatics::None):
		case (Electrostatics::Coulomb):
			break;
		case (Electrostatics::Ewald):
		case (Electrostatics::EwaldAuto):
			// Only valid for a periodic system...
			if (srcmodel->cell_.type() == UnitCell::NoCell)
			{
				Messenger::print("Error: Ewald sum is not applicable to non-periodic models.");
				Messenger::exit("Model::calculateForces");
				return FALSE;
			}
			// Estimate parameters if automatic mode selected
			if (emodel == Electrostatics::EwaldAuto) prefs.estimateEwaldParameters(&srcmodel->cell_);
			// Create the fourier space for use in the Ewald sum
			if (emodel != Electrostatics::Coulomb) fourier.prepare(srcmodel,prefs.ewaldKMax());
			break;
	}
	
	// Loop over patterns
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
			if (!p->vdwIntraPatternForces(srcmodel))
			{
				Messenger::exit("Model::calculateForces");
				return FALSE;
			}
			for (p2 = p; p2 != NULL; p2 = p2->next)
			{
				if (!p->vdwInterPatternForces(srcmodel,p2))
				{
				Messenger::exit("Model::calculateForces");
				return FALSE;
				}
			}
		}
		// Electrostatics
		switch (emodel)
		{
			case (Electrostatics::None):
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
		p = p->next;
	}
	
	// Calculate RMS force
	rmsForce_ = 0.0;
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) rmsForce_ += i->f().magnitudeSq();
	rmsForce_ /= atoms_.nItems();
	rmsForce_ = sqrt(rmsForce_);
	Messenger::exit("Model::calculateForces");
	return TRUE;
}

// Print Forces
void Model::printForces() const
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("%4i %3s  %14.6e  %14.6e  %14.6e\n", i->id(), Elements().symbol(i), i->f().x, i->f().y, i->f().z);
	}
}

// Return RMS of last calculated atomic forces
double Model::rmsForce() const
{
	return rmsForce_;
}

// Reset forces on all atoms
void Model::zeroForces()
{
	Messenger::enter("Model::zeroForces");
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) i->f().zero();
	Messenger::exit("Model::zeroForces");
}

// Reset forces on all fixed atoms
void Model::zeroForcesFixed()
{
	Messenger::enter("Model::zeroForcesFixed");

	// First, apply pattern-designated fixes
	for (Pattern* p = patterns_.first(); p != NULL; p = p->next) if (p->areAtomsFixed())
	{
		Atom* i = p->firstAtom();
		for (int n=0; n<p->totalAtoms(); ++n) { i->f().zero(); i = i->next; }
	}

	// Next, apply specific atom fixes
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isPositionFixed()) i->f().zero();

	Messenger::exit("Model::zeroForcesFixed");
}

// Normalise forces
void Model::normaliseForces(double norm, bool tolargest)
{
	// 'Normalise' the forces such that the largest force is equal to the value provided
	Messenger::enter("Model::normaliseForces");
	if (tolargest)
	{
		double largestsq = 0.0, f;
		for (Atom* i = atoms_.first(); i != NULL; i = i->next)
		{
			f = i->f().magnitudeSq();
			if (f > largestsq) largestsq = f;
		}
		largestsq = sqrt(largestsq);
		for (Atom* i = atoms_.first(); i != NULL; i = i->next) i->f() /= largestsq;
	}
	else
	{
		for (Atom* i = atoms_.first(); i != NULL; i = i->next)
		{
			i->f().normalise();
			i->f() *= norm;
		}
	}
	Messenger::exit("Model::normaliseForces");
}

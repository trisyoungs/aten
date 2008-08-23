/*
	*** Energy expression 
	*** src/energy/expression.cpp
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
#include "model/model.h"
#include "classes/forcefield.h"
#include "base/prefs.h"
#include "base/elements.h"

// Return the forcefield used by the model
Forcefield *Model::forcefield()
{
	return forcefield_;
}

// Set the forcefield containing original atom names for the model
void Model::setNamesForcefield(Forcefield *f)
{
	namesForcefield_ = f;
}

// Return the forcefield containing original atom names for the model
Forcefield *Model::namesForcefield()
{
	return namesForcefield_;
}

// Return whether the expression is valid
bool Model::isExpressionValid()
{
	return (expressionPoint_ == changeLog.log(Log::Structure) ? TRUE : FALSE);
}

// Manually invalidates the expression
void Model::invalidateExpression()
{
	expressionPoint_  = -1;;
}

// Assign charges from forcefield
void Model::assignForcefieldCharges()
{
	// Assign atom-type charges from the currently associated forcefield to the model
	// Perform forcefield typing if necessary
	msg.enter("Model::assignForcefieldCharges");
	Atom *i;
	Forcefield *xff, *patff;
	if (!arePatternsValid())
	{
		msg.print("Cannot assign atomic charges without a valid pattern setup.\n");
		msg.exit("Model::assignForcefieldCharges");
		return;
	}
	typeAll();
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// Grab current model (global) forcefield
		xff = forcefield_;	
		patff = p->forcefield();
		// Grab pattern forcefield in preference to model's
		if (patff != NULL) xff = patff;
		if (xff == NULL) msg.print("No forcefield is currently assigned to pattern %s. No charges assigned.\n",p->name());
		else
		{
			i = p->firstAtom();
			int ptotalatoms = p->totalAtoms();
			int count = 0;
			while (count < ptotalatoms)
			{
				chargeAtom(i, i->type()->charge());
				i = i->next;
				count ++;
			}
		}
	}
	msg.exit("Model::assignForcefieldCharges");
}

// Set model's forcefield
void Model::setForcefield(Forcefield *newff)
{
	// Change the associated forcefield of the model to 'newff'
	if (forcefield_ != newff)
	{
		invalidateExpression();
		forcefield_ = newff;
		msg.print("Forcefield '%s' now associated with model '%s'.\n",forcefield_->name(),name_.get());
	}
}

// Create full forcefield expression for model
bool Model::createExpression(bool vdwOnly)
{
	// This routine should be called before any operation (or series of operations) requiring calculation of energy / forces. Here, we check the validity / existence of an energy expression for the specified model, and create / recreate if necessary.
	msg.enter("Model::createExpression");
	// 0) If the expression is already valid, just update scaling terms in pattern matrices and return
	if (isExpressionValid())
	{
		for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->updateScaleMatrices();
		msg.exit("Model::createExpression");
		return TRUE;
	}
	// Reset some variables
	prefs.invalidateEwaldAuto();
	uniqueTypes_.clear();
	if (vdwOnly) msg.print("Creating VDW-only expression for model %s...\n",name_.get());
	else msg.print("Creating expression for model %s...\n",name_.get());
	// 1) Assign internal atom type data (hybridisations). [typeAll also performs create_pattern()]
	if (!typeAll())
	{
		msg.print("Couldn't type atoms.\n");
		msg.exit("Model::createExpression");
		return FALSE;
	}
	// 2) Remove old expression data and create new
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		p->deleteExpression();
		p->initExpression(vdwOnly);
		if (!p->fillExpression()) return FALSE;
		p->createMatrices();
	}
	// 3) Check the electrostatic setup for the model
	if (prefs.calculateElec())
	{
		Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
		switch (emodel)
		{
			case (Electrostatics::None):
				msg.print("Electrostatics are off.\n");
				break;
			case (Electrostatics::Coulomb):
				if (cell_.type() != Cell::NoCell) msg.print("!!! Coulomb sum requested for periodic model.\n");
				break;
			default: // Ewald - issue warnings, but don't return FALSE
				if (cell_.type() == Cell::NoCell)
				{
					msg.print("!!! Ewald sum cannot be used for a non-periodic model.\n");
					//msg.exit("Model::createExpression");
					//return FALSE;
				}
				else if (cell_.type() != Cell::CubicCell)
				{
					msg.print("!!! Ewald sum only implemented for cubic cells.\n");
					//msg.exit("Model::createExpression");
					//return FALSE;
				}
				break;
		}
	}
	expressionPoint_ = changeLog.log(Log::Structure);
	msg.exit("Model::createExpression");
	return TRUE;
}

// Create lists of unique FF terms in the model
void Model::createUniqueLists()
{
	msg.enter("Model::createUniqueLists");
	uniqueTypes_.clear();

	// First, create a list of unique type references
	Reflist<ForcefieldAtom,int> uniqueRef;
	Refitem<ForcefieldAtom,int> *ri, *rj;
	ForcefieldAtom *ffa;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) uniqueRef.addUnique(i->type());
	// Now, populate the uniquetypes list with copies of these atom types
	for (ri = uniqueRef.first(); ri != NULL; ri = ri->next)
	{
		// We only add types to the list that have a unique type name
		for (rj = uniqueRef.first(); rj != ri; rj = rj->next) if (strcmp(ri->item->name(),rj->item->name()) == 0) break;
		if (rj != ri) continue;
		ffa = uniqueTypes_.add();
		ffa->copy(ri->item);
	}

	// TODO Bond, angle, torsion lists...

	msg.exit("Model::createUniqueLists");
}

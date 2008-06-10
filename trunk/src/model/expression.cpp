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
	return (expressionPoint_ == logs_[Change::StructureLog] ? TRUE : FALSE);
}

// Manually invalidates the expression
void Model::invalidateExpression()
{
	expressionPoint_  = -1;;
}

// Create full forcefield expression for model
bool Model::createExpression(bool vdwOnly)
{
	// This routine should be called before any operation (or series of operations) requiring calculation of energy / forces. Here, we check the validity / existence of an energy expression for the specified model, and create / recreate if necessary.
	dbgBegin(Debug::Calls,"Model::createExpression");
	// 0) If the expression is already valid, return
	if (isExpressionValid())
	{
		dbgEnd(Debug::Calls,"Model::createExpression");
		return TRUE;
	}
	// Reset some variables
	prefs.invalidateEwaldAuto();
	uniqueTypes_.clear();
	if (vdwOnly) msg(Debug::None,"Creating VDW-only expression for model %s...\n",name_.get());
	else msg(Debug::None,"Creating expression for model %s...\n",name_.get());
	// 1) Assign internal atom type data (hybridisations). [typeAll also performs create_pattern()]
	if (!typeAll())
	{
		msg(Debug::None,"Couldn't type atoms.\n");
		dbgEnd(Debug::Calls,"Model::createExpression");
		return FALSE;
	}
	// 2) Remove old expression data and create new
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		p->deleteExpression();
		p->initExpression(vdwOnly);
		if (!p->fillExpression()) return FALSE;
		p->createConMat();
	}
	// 3) Check the electrostatic setup for the model
	if (prefs.calculateElec())
	{
		Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
		switch (emodel)
		{
			case (Electrostatics::None):
				msg(Debug::None,"Electrostatics are off.\n");
				break;
			case (Electrostatics::Coulomb):
				if (cell_.type() != Cell::NoCell) msg(Debug::None,"!!! Coulomb sum requested for periodic model.\n");
				break;
			default: // Ewald - issue warnings, but don't return FALSE
				if (cell_.type() == Cell::NoCell)
				{
					msg(Debug::None,"!!! Ewald sum cannot be used for a non-periodic model.\n");
					//dbgEnd(Debug::Calls,"Model::createExpression");
					//return FALSE;
				}
				else if (cell_.type() != Cell::CubicCell)
				{
					msg(Debug::None,"!!! Ewald sum only implemented for cubic cells.\n");
					//dbgEnd(Debug::Calls,"Model::createExpression");
					//return FALSE;
				}
				break;
		}
	}
	expressionPoint_ = logs_[Change::StructureLog];
	dbgEnd(Debug::Calls,"Model::createExpression");
	return TRUE;
}

// Create lists of unique FF terms in the model
void Model::createUniqueLists()
{
	dbgBegin(Debug::Calls,"Model::createUniqueLists");
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

	dbgEnd(Debug::Calls,"Model::createUniqueLists");
}

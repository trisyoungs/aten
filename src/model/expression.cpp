/*
	*** Energy expression 
	*** src/model/expression.cpp
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

#include "base/pattern.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "main/aten.h"
#include "gui/gui.h"
#include "gui/forcefields.h"

// Return the forcefield used by the model
Forcefield *Model::forcefield()
{
	return forcefield_;
}

// Create a forcefield containing original atom names for the model
void Model::createNamesForcefield()
{
	if (namesForcefield_ != NULL) msg.print("Warning - an atom names forcefield already exists for model '%s'.\n", name_.get());
	msg.print("Creating atom names forcefield for model '%s'.\n", name_.get());
	Dnchar s;
	s.sprintf("Names kept from Model %s",name_.get());
	namesForcefield_ = aten.addForcefield(s);
}

// Return the forcefield containing original atom names for the model
Forcefield *Model::namesForcefield() const
{
	return namesForcefield_;
}

// Add name to names forcefield
ForcefieldAtom *Model::addAtomName(int el, const char *name)
{
	if (namesForcefield_ == NULL) createNamesForcefield();
	// Search for this typename in the ff
	ForcefieldAtom *ffa = namesForcefield_->findType(name);
	if (ffa == NULL)
	{
		ffa = namesForcefield_->addType();
		ffa->setName(name);
		ffa->neta()->setCharacterElement(el);
	}
	return ffa;
}

// Remove the names forcefield reference
void Model::removeNamesForcefield()
{
	namesForcefield_ = NULL;
}

// Return whether the expression is valid
bool Model::isExpressionValid() const
{
	return (expressionPoint_ == changeLog.log(Log::Structure) ? TRUE : FALSE);
}

// Clear the current expression
void Model::clearExpression()
{
	msg.enter("Model::clearExpression");
	forcefieldAngles_.clear();
	forcefieldBonds_.clear();
	forcefieldTorsions_.clear();
	uniqueForcefieldTypes_.clear();
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->deleteExpression();
	expressionPoint_  = -1;
	msg.exit("Model::clearExpression");
}

// Manually invalidates the expression
void Model::invalidateExpression()
{
	expressionPoint_  = -1;
}

// Assign charges from forcefield
bool Model::assignForcefieldCharges()
{
	// Assign atom-type charges from the currently assigned atom types
	// Perform forcefield typing if necessary
	msg.enter("Model::assignForcefieldCharges");
	int nfailed = 0;
	double totalq = 0.0;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->type() == NULL)
		{
			nfailed ++;
			msg.print("Could not assign charge to atom %i since it has no atomtype associated to it.\n", i->id()+1);
		}
		else
		{
			i->setCharge( i->type()->charge() );
			totalq += i->type()->charge();
		}
	}
	if (nfailed == 0) msg.print("Charges assigned successfully to all atoms.\nTotal charge in model is %f e.\n", totalq);
	else msg.print("Failed to assign charges to %i atoms.\n", nfailed);
	msg.exit("Model::assignForcefieldCharges");
	return (nfailed == 0);
}

// Set model's forcefield
void Model::setForcefield(Forcefield *newff)
{
	// Change the associated forcefield of the model to 'newff'
	if (forcefield_ != newff)
	{
		invalidateExpression();
		forcefield_ = newff;
		if (forcefield_ == NULL) msg.print("Model '%s' has had its associated forcefield removed.\n", name_.get());
		else msg.print("Forcefield '%s' now associated with model '%s'.\n", forcefield_->name(), name_.get());
	}
}

// Create full forcefield expression for model
bool Model::createExpression(Choice vdwOnly, Choice allowDummy, Choice assignCharges)
{
	// This routine should be called before any operation (or series of operations) requiring calculation of energy / forces. Here, we check the validity / existence of an energy expression for the specified model, and create / recreate if necessary.
	msg.enter("Model::createExpression");
	
	// Resolve supplied choices
	vdwOnly.resolve(FALSE);
	allowDummy.resolve(FALSE);
	assignCharges.resolve( gui.exists() ? gui.forcefieldsWidget->ui.AssignFFChargesCheck->isChecked() : TRUE);
	
	// 0) If the expression is already valid, just update scaling terms in pattern matrices and return
	if (isExpressionValid() && (vdwOnly == expressionVdwOnly_))
	{
		for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->updateScaleMatrices();
		msg.exit("Model::createExpression");
		return TRUE;
	}

	// Reset some variables
	prefs.invalidateEwaldAuto();
	forcefieldAngles_.clear();
	forcefieldBonds_.clear();
	forcefieldTorsions_.clear();
	uniqueForcefieldTypes_.clear();
	allForcefieldTypes_.clear();
	expressionVdwOnly_ = vdwOnly;
	expressionPoint_ = -1;
	if (expressionVdwOnly_) msg.print("Creating VDW-only expression for model %s...\n",name_.get());
	else msg.print("Creating expression for model %s...\n",name_.get());
	
	// 1) Assign forcefield types to all atoms
	if (!typeAll())
	{
		msg.print("Couldn't type atoms.\n");
		msg.exit("Model::createExpression");
		return FALSE;
	}
	
	// 2) Remove old expression data and create new
	bool done;
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		done = FALSE;
		while (!done)
		{
			p->deleteExpression();
			if (!p->createExpression(vdwOnly, allowDummy))
			{
				// Failed to create an expression for this pattern, so....
				if (!gui.exists())
				{
					msg.print("To force expression generation with missing terms see the optional parameters for the 'createexpression()' command:\n");
					msg.print("   Syntax: createexpression(%s)\n", Command::data[Command::CreateExpression].argText);
					msg.print("           %s\n", Command::data[Command::CreateExpression].syntax);
					msg.exit("Model::createExpression");
					return FALSE;
				}
				else
				{
					// What to do?
					Tree dialog;
					Dnchar title(-1,"Expression for Pattern '%s'", p->name());
					TreeGui &ui = dialog.defaultDialog();
					ui.setValue(title);
					ui.addLabel("One or more terms are missing from a pattern expression:", 1,1);
					ui.addRadioGroup("choice");
					ui.addRadioButton("cancel", "Cancel expression generation", "choice", 1, 1,2);
					ui.addRadioButton("dummy", "Add in dummy parameters to complete expression (type=ignore)", "choice", 0, 1,3);
					
					// Run the custom dialog
					if (dialog.defaultDialog().execute())
					{
						int choice = ui.asInteger("choice");
						if (choice == 1)
						{
							msg.exit("Model::createExpression");
							return FALSE;
						}
						else if (choice == 2)
						{
							// Flag generation of dummy terms in expression, and let the loop cycle
							p->setAddDummyTerms(TRUE);
							done = FALSE;
						}
					}
					else
					{
						msg.exit("Model::createExpression");
						return FALSE;
					}
				}
			}
			else done = TRUE;
		}
		p->createMatrices();
	}
	
	// 3) Check the electrostatic setup for the model
	Electrostatics::ElecMethod emodel = prefs.electrostaticsMethod();
	switch (emodel)
	{
		case (Electrostatics::None):
			break;
		case (Electrostatics::Coulomb):
			if (cell_.type() != UnitCell::NoCell) msg.print("!!! Coulomb sum requested for periodic model.\n");
			break;
		default: // Ewald - issue warnings, but don't return FALSE
			if (cell_.type() == UnitCell::NoCell)
			{
				msg.print("!!! Ewald sum cannot be used for a non-periodic model.\n");
				//msg.exit("Model::createExpression");
				//return FALSE;
			}
			break;
	}
	
	// 4) Create master (Model) forcefield term lists
	createForcefieldLists();
	
	// 5) Assign charges to atoms (if requested)
	if (assignCharges && (!assignForcefieldCharges()))
	{
		msg.exit("Model::createExpression");
		return FALSE;
	}
	
	// 6) Create VDW lookup table of combined parameters
	combinationTable_.clear();
	PointerPair<ForcefieldAtom,double> *pp;
	ForcefieldAtom *ffa, *ffb;
	Combine::CombinationRule *crflags;
	int i;
	for (Refitem<ForcefieldAtom,int> *rfa = allForcefieldTypes_.first(); rfa != NULL; rfa = rfa->next)
	{
		ffa = rfa->item;
		for (Refitem<ForcefieldAtom,int> *rfb = rfa; rfb != NULL; rfb = rfb->next)
		{
			ffb = rfb->item;
			// Check for compatible interaction types between atoms
			if (ffa->vdwForm() != ffb->vdwForm())
			{
				msg.print("Conflicting van der Waals functional forms for atom types '%s' and '%s'.\n", VdwFunctions::VdwFunctions[ffa->vdwForm()].name, VdwFunctions::VdwFunctions[ffb->vdwForm()].name);
				msg.print("Expression for model cannot be completed.\n");
				msg.exit("Model::createExpression");
				return FALSE;
			}
			// Create item in table
			pp = combinationTable_.add(ffa, ffb, VdwFunctions::VdwFunctions[ffa->vdwForm()].nParameters);
			// Combine parameters
			crflags = VdwFunctions::VdwFunctions[ffa->vdwForm()].combinationRules;
			for (i=0; i<VdwFunctions::VdwFunctions[ffa->vdwForm()].nParameters; ++i)
			{
				pp->setData(i, Combine::combine( crflags[i], ffa->parameter(i), ffb->parameter(i) ) );
// 	printf("combined Parameter is %f, original = %f,%f\n", Combine::combine( crflags[i], ffa->parameter(i), ffb->parameter(i)), ffa->parameter(i), ffb->parameter(i));
			}

		}
	}
	
	expressionPoint_ = changeLog.log(Log::Structure);
	msg.exit("Model::createExpression");
	return TRUE;
}

// Create lists of forcefield terms in the model
void Model::createForcefieldLists()
{
	msg.enter("Model::createForcefieldLists");

	msg.print(Messenger::Verbose, "Constructing global forcefield term lists for model...\n");
	forcefieldAngles_.clear();
	forcefieldBonds_.clear();
	forcefieldTorsions_.clear();
	uniqueForcefieldTypes_.clear();

	Refitem<ForcefieldAtom,int> *ffa2;

	// Cycle over patterns, adding their unique forcefield terms to ours...
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		msg.print(Messenger::Verbose, "Pattern '%s' uses %i atom types, %i bond terms, %i angle terms, and %i torsion terms.\n", p->name(), p->nUniqueForcefieldTypes(), p->nForcefieldBonds(), p->nForcefieldAngles(), p->nForcefieldTorsions());

		// Atom types. We only add types to the list that have a unique type name.
		for (Refitem<ForcefieldAtom,int> *ffa1 = p->allForcefieldTypes(); ffa1 != NULL; ffa1 = ffa1->next)
		{
			// Add to list of unique (by pointer) types
			allForcefieldTypes_.addUnique(ffa1->item);
			// Add to list of unique (by name) types
			for (ffa2 = uniqueForcefieldTypes_.first(); ffa2 != NULL; ffa2 = ffa2->next) if (strcmp(ffa1->item->name(),ffa2->item->name()) == 0) break;
			if (ffa2 != NULL) continue;
			uniqueForcefieldTypes_.add(ffa1->item);
		}

		// Bond terms
		for (Refitem<ForcefieldBound,int> *ffb = p->forcefieldBonds(); ffb != NULL; ffb = ffb->next)
		{
			if (forcefieldBonds_.contains(ffb->item) == NULL) forcefieldBonds_.add(ffb->item);
		}

		// Angle terms
		for (Refitem<ForcefieldBound,int> *ffb = p->forcefieldAngles(); ffb != NULL; ffb = ffb->next)
		{
			if (forcefieldAngles_.contains(ffb->item) == NULL) forcefieldAngles_.add(ffb->item);
		}

		// Torsion terms
		for (Refitem<ForcefieldBound,int> *ffb = p->forcefieldTorsions(); ffb != NULL; ffb = ffb->next)
		{
			if (forcefieldTorsions_.contains(ffb->item) == NULL) forcefieldTorsions_.add(ffb->item);
		}
	}

	msg.print(Messenger::Verbose, "Model '%s' uses %i atom types, %i bond terms, %i angle terms, and %i torsion terms over all patterns.\n", name(), nUniqueForcefieldTypes(), nForcefieldBonds(), nForcefieldAngles(), nForcefieldTorsions());

	msg.exit("Model::createForcefieldLists");
}

// Return specified pair data from combination table
PointerPair<ForcefieldAtom,double> *Model::combinedParameters(ForcefieldAtom *at1, ForcefieldAtom *at2)
{
	if ((at1 == NULL) || (at2 == NULL))
	{
		printf("Warning - NULL atom type(s) passed to Model::combineParameters...\n");
		return NULL;
	}
	PointerPair<ForcefieldAtom,double> *pp = combinationTable_.find(at1, at2);
	if (pp == NULL) printf("Internal Error : Couldn't find combined parameters for atom types '%s' and '%s'.\n", at1->name(), at2->name());
	return pp;
}

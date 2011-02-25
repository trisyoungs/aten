/*
	*** Model Extras Commands
	*** src/command/modelextras.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "base/elements.h"

// Create new basis function in the current model
bool Command::function_NewBasisShell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Get shell type argument
	BasisShell::BasisShellType bft = BasisShell::basisShellType(c->argc(1), TRUE);
	if (bft == BasisShell::nBasisShellTypes) return FALSE;
	BasisShell *bf = obj.rs->addBasisShell();
	bf->setAtomId(c->argi(0)-1);
	bf->setType(bft);
	rv.set(VTypes::BasisShellData, bf);
	return TRUE;
}

// Create new eigenvector in the current model
bool Command::function_NewEigenvector(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Optional argument is size of vector. If not present, use current size of basis function array
	int size = c->hasArg(0) ? c->argi(0) : obj.rs->nCartesianBasisFunctions();
	Eigenvector *vec = obj.rs->addEigenvector();
	vec->initialise(size);
	rv.set(VTypes::EigenvectorData, vec);
	return TRUE;
}

// Create new vibration in current model
bool Command::function_NewVibration(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vibration *v = obj.rs->renderSourceModel()->addVibration();
	if (c->hasArg(0)) v->setName(c->argc(0));
	msg.print(Messenger::Verbose, "Added vibration to model '%s'\n", obj.rs->name());
	rv.set(VTypes::VibrationData, v);
	return TRUE;
}

// Print z-matrix for model
bool Command::function_PrintZMatrix(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Grab (and create) zmatrix for current model
	ZMatrix *zmat = obj.rs->zMatrix();
	if (zmat == NULL) return FALSE;
	
	Atom *i, *j, *k, *l;
	for (ZMatrixElement *zel = zmat->elements(); zel != NULL; zel = zel->next)
	{
		// First atom (the creation target)
		i = zel->atom(0);
		// Second atom (distance specifier)
		j = zel->atom(1);
		if (j != NULL)
		{
			// Third atom (angle specifier)
			k = zel->atom(2);
			if (k != NULL)
			{
				// Fourth atom (torsion specifier)
				l = zel->atom(3);
				if (l != NULL)
				{
					printf("%-4s %-4i %-6s %-4i %-6s %-4i %-6s\n", elements().symbol(i), j->id()+1, zel->distanceVariable()->name(), k->id()+1, zel->angleVariable()->name(), l->id()+1, zel->torsionVariable()->name());
				}
				else printf("%-4s %-4i %-6s %-4i %-6s\n", elements().symbol(i), j->id()+1, zel->distanceVariable()->name(), k->id()+1, zel->angleVariable()->name());
			}
			else printf("%-4s %-4i %-6s\n", elements().symbol(i), j->id()+1, zel->distanceVariable()->name());
		}
		else printf("%-4s\n", elements().symbol(i));
	}
	printf("\n");

	// Variable list
	ReturnValue varrv;
	Variable *var;
	for (TreeNode *v = zmat->distances(); v != NULL; v = v->next)
	{
		var = (Variable*) v;
		var->execute(varrv);
		printf("%-6s  %f\n", var->name(), varrv.asDouble());
	}
	for (TreeNode *v = zmat->angles(); v != NULL; v = v->next)
	{
		var = (Variable*) v;
		var->execute(varrv);
		printf("%-6s  %f\n", var->name(), varrv.asDouble());
	}
	for (TreeNode *v = zmat->torsions(); v != NULL; v = v->next)
	{
		var = (Variable*) v;
		var->execute(varrv);
		printf("%-6s  %f\n", var->name(), varrv.asDouble());
	}

	return TRUE;
}


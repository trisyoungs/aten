/*
	*** Molecular Orbital Commands
	*** src/command/mo.cpp
	Copyright T. Youngs 2007-2010

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

// Create new basis function in the current model
bool Command::function_NewBasisFunction(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Get atom and type arguments
	Atom *i = (c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0, VTypes::AtomData) : obj.rs->atom(c->argi(0)-1));
	if (i == NULL)
	{
		msg.print("Illegal atom pointer/id specified. Basis function will not be created.\n");
		rv.reset();
		return FALSE;
	}
	BasisFunction::BasisFunctionType bft = BasisFunction::basisFunctionType(c->argc(1), TRUE);
	if (bft == BasisFunction::nBasisFunctionTypes) return FALSE;
	BasisFunction *bf = obj.rs->addBasisFunction();
	bf->setAtom(i);
	bf->setType(bft);
	rv.set(VTypes::BasisFunctionData, bf);
	return TRUE;
}

// Create new eigenvector in the current model
bool Command::function_NewEigenvector(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Optional argument is size of vector. If not present, use current size of basis function array
	int size = c->hasArg(0) ? c->argi(0) : obj.rs->nBasisFunctions();
	Eigenvector *vec = obj.rs->addEigenvector();
	vec->initialise(size);
	rv.set(VTypes::EigenvectorData, vec);
	return TRUE;
}

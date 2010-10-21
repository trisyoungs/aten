/*
	*** Model Extras Commands
	*** src/command/modelextras.cpp
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

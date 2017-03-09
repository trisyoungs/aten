/*
	*** Model Extras Commands
	*** src/command/modelextras.cpp
	Copyright T. Youngs 2007-2017

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
#include "model/bundle.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Create new basis function in the current model
bool Commands::function_NewBasisShell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Get shell type argument
	BasisShell::BasisShellType bft = BasisShell::basisShellType(c->argc(1), true);
	if (bft == BasisShell::nBasisShellTypes) return false;
	BasisShell *bf = obj.rs()->addBasisShell();
	bf->setAtomId(c->argi(0)-1);
	bf->setType(bft);
	rv.set(VTypes::BasisShellData, bf);
	return true;
}

// Create new eigenvector in the current model
bool Commands::function_NewEigenvector(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Optional argument is size of vector. If not present, use current size of basis function array
	int size = c->hasArg(0) ? c->argi(0) : obj.rs()->nCartesianBasisFunctions();
	Eigenvector *vec = obj.rs()->addEigenvector();
	vec->initialise(size);
	rv.set(VTypes::EigenvectorData, vec);
	return true;
}

// Create new vibration in current model
bool Commands::function_NewVibration(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vibration* v = obj.rs()->addVibration();
	if (c->hasArg(0)) v->setName(c->argc(0));
	Messenger::print(Messenger::Verbose, "Added vibration to model '%s'", qPrintable(obj.rs()->name()));
	rv.set(VTypes::VibrationData, v);
	return true;
}

// Print z-matrix for model
bool Commands::function_PrintZMatrix(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Grab (and create) zmatrix for current model
	ZMatrix* zmat = obj.rs()->zMatrix();
	if (zmat == NULL) return false;
	
	zmat->print();

	return true;
}


/*
	*** Minimiser Commands
	*** src/command/minimise.cpp
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
#include "methods/sd.h"
#include "methods/mc.h"
#include "methods/cg.h"
#include "main/aten.h"
#include "base/sysfunc.h"
#include <QApplication>
#include <QFileInfo>

ATEN_USING_NAMESPACE

// Minimise with conjugate gradient
bool Commands::function_CGMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Ensure we have a valid expression
	if (!obj.rs()->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield()))
	{
		Messenger::print("Failed to create expression - minimisation can't be performed.");
		return false;
	}

	// Get argument values
	int nCycles = c->hasArg(0) ? c->argi(0) : 100;
	double eConverge = c->hasArg(1) ? c->argd(1) : 1.0e-3;
	double fConverge = c->hasArg(2) ? c->argd(2) : 1.0e-2;
	double lineTolerance = c->hasArg(3) ? c->argd(3) : 1.0e-4;

	// Create and setup the minimiser
	CGMinimiser cg;
	cg.setTolerance(lineTolerance);
	cg.setNCycles(nCycles);

	// Store current positions of atoms so we can undo the minimisation
	RefList< Atom, Vec3<double> > oldpos;
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	rv = cg.minimise(obj.rs(), eConverge, fConverge);

	// Finalise the 'transformation' (creates an undo state)
	obj.rs()->finalizeTransform(oldpos, "Minimise (Conjugate Gradient)", true);

	return true;
}

// Minimise current model with Monte-Carlo method ('mcminimise <maxsteps>')
bool Commands::function_MCMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Get argument values
	int nCycles = c->hasArg(0) ? c->argi(0) : 100;
	double eConverge = c->hasArg(1) ? c->argd(1) : 1.0e-3;
	double fConverge = c->hasArg(2) ? c->argd(2) : 1.0e-2;

	mc.setNCycles(nCycles);

	// Store current positions of atoms so we can undo the minimisation
	RefList< Atom, Vec3<double> > oldpos;
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	rv = mc.minimise(obj.rs(), eConverge, fConverge);

	// Finalise the 'transformation' (creates an undo state)
	obj.rs()->finalizeTransform(oldpos, "Minimise (Monte Carlo)", true);

	return true;
}

// Minimise current model with Steepest Descent method ('sdminimise <maxsteps>')
bool Commands::function_SDMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Ensure we have a valid expression
	if (!obj.rs()->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield()))
	{
		Messenger::print("Failed to create expression - minimisation can't be performed.");
		return false;
	}

	// Get converge values passed
	int nCycles = c->hasArg(0) ? c->argi(0) : 100;
	double eConverge = c->hasArg(1) ? c->argd(1) : 1.0e-3;
	double fConverge = c->hasArg(2) ? c->argd(2) : 1.0e-2;
	double lineTolerance = c->hasArg(3) ? c->argd(3) : 1.0e-4;
	bool simple = c->hasArg(4) ? c->argb(4) : false;

	// Create and setup minimiser
	SDMinimiser sd;
	sd.setTolerance(lineTolerance);
	sd.setNCycles(nCycles);

	// Store current positions of atoms so we can undo the minimisation
	RefList< Atom, Vec3<double> > oldpos;
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	rv = sd.minimise(obj.rs(), eConverge, fConverge, simple);

	// Finalise the 'transformation' (creates an undo state)
	obj.rs()->finalizeTransform(oldpos, "Minimise (Steepest Descent)", true);

	return true;
}

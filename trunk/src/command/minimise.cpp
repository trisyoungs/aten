/*
	*** Minimiser Commands
	*** src/command/minimise.cpp
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
#include "methods/sd.h"
#include "methods/mc.h"
#include "methods/cg.h"
#include "model/model.h"

// Local variables
double econverge = 0.001, fconverge = 0.01, linetolerance = 0.0001;

// Minimise with conjugate gradient
bool Command::function_CGMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	cg.setTolerance(linetolerance);
	cg.setNCycles( c->hasArg(0) ? c->argi(0) : 100);
	// Store current positions of atoms so we can undo the minimisation
	Reflist< Atom, Vec3<double> > oldpos;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	cg.minimise(obj.rs, econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Conjugate Gradient)");
	rv.reset();
	return TRUE;
}

// Set convergence criteria
bool Command::function_Converge(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	econverge = c->argd(0);
	fconverge = c->argd(1);
	rv.reset();
	return TRUE;
}

// Set line minimiser tolerance
bool Command::function_LineTolerance(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	linetolerance = c->argd(0);
	rv.reset();
	return TRUE;
}

// Minimise current model with Monte-Carlo method ('mcminimise <maxsteps>')
bool Command::function_MCMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	mc.setNCycles( c->hasArg(0) ? c->argi(0) : 100);
	// Store current positions of atoms so we can undo the minimisation
	Reflist< Atom, Vec3<double> > oldpos;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	mc.minimise(obj.rs, econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Monte Carlo)");
	rv.reset();
	return TRUE;
}

// Minimise current model with Steepest Descent method ('sdminimise <maxsteps>')
bool Command::function_SDMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	sd.setTolerance(linetolerance);
	sd.setNCycles( c->hasArg(0) ? c->argi(0) : 100);
	// Store current positions of atoms so we can undo the minimisation
	Reflist< Atom, Vec3<double> > oldpos;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	sd.minimise(obj.rs, econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Steepest Descent)");
	rv.reset();
	return TRUE;
}

bool Command::function_SimplexMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.reset();
	return FALSE;
}


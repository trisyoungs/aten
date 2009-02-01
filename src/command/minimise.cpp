/*
	*** Minimiser command functions
	*** src/command/minimise.cpp
	Copyright T. Youngs 2007-2009

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

#include "command/commandlist.h"
#include "methods/sd.h"
#include "methods/mc.h"
#include "methods/cg.h"
#include "model/model.h"

// Local variables
double econverge = 0.001, fconverge = 0.01, linetolerance = 0.0001;

// Minimise with conjugate gradient
int Command::function_CA_CGMINIMISE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	cg.setTolerance(linetolerance);
	cg.setNCycles(c->argi(0));
	// Store current positions of atoms so we can undo the minimisation
	Reflist< Atom, Vec3<double> > oldpos;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	cg.minimise(obj.rs, econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Conjugate Gradient)");
	return Command::Success;
}

// Set convergence criteria
int Command::function_CA_CONVERGE(CommandNode *&c, Bundle &obj)
{
	econverge = c->argd(0);
	fconverge = c->argd(1);
	return Command::Success;
}

// Set line minimiser tolerance
int Command::function_CA_LINETOL(CommandNode *&c, Bundle &obj)
{
	linetolerance = c->argd(0);
	return Command::Success;
}

// Minimise current model with Monte-Carlo method ('mcminimise <maxsteps>')
int Command::function_CA_MCMINIMISE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	mc.setNCycles(c->argi(0));
	// Store current positions of atoms so we can undo the minimisation
	Reflist< Atom, Vec3<double> > oldpos;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	mc.minimise(obj.rs, econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Monte Carlo)");
	return Command::Success;
}

// Minimise current model with Steepest Descent method ('sdminimise <maxsteps>')
int Command::function_CA_SDMINIMISE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	sd.setTolerance(linetolerance);
	sd.setNCycles(c->argi(0));
	// Store current positions of atoms so we can undo the minimisation
	Reflist< Atom, Vec3<double> > oldpos;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	sd.minimise(obj.rs, econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Steepest Descent)");
	return Command::Success;
}

int Command::function_CA_SIMPLEXMINIMISE(CommandNode *&c, Bundle &obj)
{
	return Command::Fail;
}

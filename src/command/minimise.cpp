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
#include "main/aten.h"
#include "base/sysfunc.h"
#include "parser/filterdata.h"

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

// Use MOPAC to minimise the current model
bool Command::function_MopacMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	rv.reset();
	// Check that we can run system commands
	int cmdresult = system(NULL);
	if (cmdresult == 0)
	{
		msg.print("Error: Unable to run system commands.\n");
		return FALSE;
	}
	// Grab pointers to MOPAC import and export filters
	Tree *mopacexport = aten.findFilter(FilterData::ModelExport, "mopac");
	if (mopacexport == NULL)
	{
		msg.print("Error: Couldn't find MOPAC export filter.\n");
		return FALSE;
	}
	Tree *mopacimport = aten.findFilter(FilterData::ModelImport, "mopacarc");
	if (mopacimport == NULL)
	{
		msg.print("Error: Couldn't find MOPAC arc import filter.\n");
		return FALSE;
	}
	// Grab/create various filenames and paths
	static int runid = 1;
	Dnchar mopacexe, mopacinput, mopacoutput, mopaccmd;
	mopacexe = "/home/tris/bin/MOPAC2009.exe";
	mopacinput.sprintf("/home/tris/tmp/aten-mopac%05i.mop", runid);
	mopacoutput.sprintf("/home/tris/tmp/aten-mopac%05i.arc", runid);
	mopaccmd.sprintf("\"%s\" \"%s\"", mopacexe.get(), mopacinput.get());
	msg.print(Messenger::Verbose, "Command to run will be '%s'\n", mopaccmd.get());
	// Create copy of current model so as not to disturb filename/filter values
	Model tempmodel;
	tempmodel.copy(aten.currentModel());
	tempmodel.setFilter(mopacexport);
	tempmodel.setFilename(mopacinput);
	// Save the input file...
	bool result = TRUE;
	// Construct temporary bundle object containing our model pointer
	Bundle bundle(&tempmodel);
	ReturnValue temprv;
	result = CommandNode::run(Command::SaveModel, bundle, "cc", "mopac", mopacinput.get());
	// Ready to run command....
	cmdresult = system(mopaccmd.get());
	if (cmdresult != 0)
	{
		msg.print("Error: Failed to run MOPAC. Is it installed correctly?\n");
		return FALSE;
	}
	// Check for existence of output file....
	if (!fileExists(mopacoutput))
	{
		msg.print("Error: Can't locate MOPAC output '%s'.\n", mopacoutput.get());
		return FALSE;
	}
	// Time to load in the results
	printf("AT THIS POINT, obj.rs = %p\n", obj.rs);
	aten.setUseWorkingList(TRUE);
	result = CommandNode::run(Command::LoadModel, "c", mopacoutput.get());
	// There should now be a model in the working model list (our results)
	Model *m = aten.workingModels();
	if (m == NULL)
	{
		msg.print("Error: No results model found.\n");
		return FALSE;
	}
	printf("LOADED MOPAC MODEL is %p\n", m);
	// Copy the atoms back into our tempmodel
	tempmodel.copy(m);
	aten.setUseWorkingList(FALSE);
	// Start a new undostate in the original model
	printf("AND NOW, obj.rs = %p\n", obj.rs);
	obj.rs->beginUndoState("MOPAC geometry optimisation");
	Atom *i, *j = obj.rs->atoms();
	for (i = tempmodel.atoms(); i != NULL; i = i->next)
	{
		obj.rs->positionAtom(j, i->r());
		j = j->next;
	}
	obj.rs->endUndoState();
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
	sd.minimise(obj.rs, econverge, fconverge, c->hasArg(1) ? c->argb(1) : FALSE);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs->finalizeTransform(oldpos, "Minimise (Steepest Descent)");
	rv.reset();
	return TRUE;
}

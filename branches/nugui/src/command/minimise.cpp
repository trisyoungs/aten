/*
	*** Minimiser Commands
	*** src/command/minimise.cpp
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
#include "methods/sd.h"
#include "methods/mc.h"
#include "methods/cg.h"
#include "model/model.h"
#include "main/aten.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"
#include "parser/filterdata.h"
#include "gui/gui.h"
#include "gui/tprocess.uih"

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
	for (Atom *i = obj.rs()->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	cg.minimise(obj.rs(), econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs()->finalizeTransform(oldpos, "Minimise (Conjugate Gradient)", TRUE);
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
	for (Atom *i = obj.rs()->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	mc.minimise(obj.rs(), econverge, fconverge);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs()->finalizeTransform(oldpos, "Minimise (Monte Carlo)", TRUE);
	rv.reset();
	return TRUE;
}

// Use MOPAC to minimise the current model
bool Command::function_MopacMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	rv.reset();

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
	// Check that defined MOPAC exe exists
	if (!fileExists(prefs.mopacExe()))
	{
		msg.print("Error: MOPAC excutable doesn't appear to be at '%s'.\n", prefs.mopacExe());
		return FALSE;
	}
	// Grab/create various filenames and paths
	TProcess mopacProcess;
	Dnchar mopacInput, mopacArc, mopacCmd, mopacOut;
	int runid;
	// Determine unique filename
	do
	{
		runid = AtenMath::randomi(RAND_MAX);
		mopacInput.sprintf("%s%caten-%i-mopac%i.mop", prefs.tempDir(), PATHSEP, gui.pid(), runid);
	} while (fileExists(mopacInput));
	mopacArc.sprintf("%s%caten-%i-mopac%i.arc", prefs.tempDir(), PATHSEP, gui.pid(), runid);
	mopacOut.sprintf("%s%caten-%i-mopac%i.out", prefs.tempDir(), '/', gui.pid(), runid);
	mopacCmd.sprintf("\"%s\" \"%s\"", prefs.mopacExe(), mopacInput.get());
	msg.print("Command to run will be '%s'\n", mopacCmd.get());
	
	// Save input file
	LineParser parser(mopacInput, TRUE);
	int opt;
	if (c->hasArg(0)) parser.writeLineF("MOZYME BFGS %s\n",c->argc(0));
	else parser.writeLine("MOZYME BFGS PM6 RHF SINGLET\n");
	parser.writeLineF("Temporary MOPAC Job Input  : %s\n", mopacInput.get());
	parser.writeLineF("Temporary MOPAC Job Output : %s\n", mopacArc.get());	
	for (Atom *i = aten.currentModelOrFrame()->atoms(); i != NULL; i = i->next)
	{
		opt = 1 - i->isPositionFixed();
		parser.writeLineF("%3s %12.6f %1i %12.6f %1i %12.6f %1i\n", elements().symbol(i), i->r().x, opt, i->r().y, opt, i->r().z, opt);
	}
	if (aten.currentModelOrFrame()->cell()->type() != UnitCell::NoCell)
	{
		Matrix mat = aten.currentModelOrFrame()->cell()->axes();
		parser.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n",mat[0], mat[1], mat[2]);
		parser.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n",mat[4], mat[5], mat[6]);
		parser.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n",mat[8], mat[9], mat[10]);
	}
	parser.closeFile();
	
	// Ready to run command....
	if (!mopacProcess.execute(mopacCmd, NULL, mopacOut))
	{
		msg.print("Error: Failed to run MOPAC. Is it installed correctly?\n");
		return FALSE;
	}

	// Follow output here...
	while (!mopacProcess.finished())
	{
		// Is output file already present?
		while (mopacProcess.outputAvailable()) mopacProcess.printLineToMessages();
		gui.processMessages();
	}
	
	// Check for existence of output file....
	if (!fileExists(mopacArc))
	{
		msg.print("Error: Can't locate MOPAC output '%s'.\n", mopacArc.get());
		return FALSE;
	}
	// Time to load in the results
	aten.setUseWorkingList(TRUE);
	int result = CommandNode::run(Command::LoadModel, "c", mopacArc.get());
	// There should now be a model in the working model list (our results)
	Model *m = aten.workingModels();
	if (m == NULL)
	{
		msg.print("Error: No results model found.\n");
		return FALSE;
	}
	// Copy the atoms into a temporary model
	Model tempmodel;
	tempmodel.copy(m);
	aten.setUseWorkingList(FALSE);
	// Start a new undostate in the original model
	//printf("Target for new coords = %p\n", obj.rs);
	obj.rs()->beginUndoState("MOPAC geometry optimisation");
	Atom *i, *j = obj.rs()->atoms();
	for (i = tempmodel.atoms(); i != NULL; i = i->next)
	{
		obj.rs()->positionAtom(j, i->r());
		j = j->next;
	}
	obj.rs()->endUndoState();
	
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
	for (Atom *i = obj.rs()->atoms(); i != NULL; i = i->next) oldpos.add(i, i->r());
	sd.minimise(obj.rs(), econverge, fconverge, c->hasArg(1) ? c->argb(1) : FALSE);
	// Finalise the 'transformation' (creates an undo state)
	obj.rs()->finalizeTransform(oldpos, "Minimise (Steepest Descent)", TRUE);
	rv.reset();
	return TRUE;
}

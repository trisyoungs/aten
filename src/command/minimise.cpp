/*
	*** Minimiser Commands
	*** src/command/minimise.cpp
	Copyright T. Youngs 2007-2016

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

// Use MOPAC to minimise the current model
bool Commands::function_MopacMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.reset();

	// ATEN2 TODO ENDOFFILTERS
// 	// Grab pointers to MOPAC import and export filters
// 	Tree* mopacexport = aten_.findFilter(FilterData::ModelExport, "mopac");
// 	if (mopacexport == NULL)
// 	{
// 		Messenger::print("Error: Couldn't find MOPAC export filter.");
// 		return false;
// 	}
// 	Tree* mopacimport = aten_.findFilter(FilterData::ModelImport, "mopacarc");
// 	if (mopacimport == NULL)
// 	{
// 		Messenger::print("Error: Couldn't find MOPAC arc import filter.");
// 		return false;
// 	}

	// Check that defined MOPAC exe exists
	QFileInfo fileInfo(prefs.mopacExe());
	if (!fileInfo.exists())
	{
		Messenger::print("Error: MOPAC excutable '%s' doesn't exists.", qPrintable(prefs.mopacExe()));
		return false;
	}

	// Grab/create various filenames and paths
// 	TProcess mopacProcess; // ATEN2 TODO
	QString mopacInput, mopacArc, mopacCmd, mopacOut;
	int runid;

	// Determine unique filename
	do
	{
		runid = AtenMath::randomimax();
		mopacInput = prefs.tempDir().filePath("aten-mopac-%1-%2.mop").arg(QApplication::applicationPid(), runid);
		fileInfo.setFile(mopacInput);
	} while (fileInfo.exists());
	mopacArc = prefs.tempDir().filePath("aten-mopac-%1-%2.arc").arg(QApplication::applicationPid(), runid);
	mopacOut = prefs.tempDir().filePath("aten-mopac-%1-%2.out").arg(QApplication::applicationPid(), runid);
	mopacCmd.sprintf("\"%s\" \"%s\"", qPrintable(prefs.mopacExe()), qPrintable(mopacInput));
	Messenger::print("Command to run will be '%s'", qPrintable(mopacCmd));
	
	// Save input file
	LineParser parser;
	parser.openOutput(mopacInput, true);
	int opt;
	if (c->hasArg(0)) parser.writeLineF("MOZYME BFGS %s", qPrintable(c->argc(0)));
	else parser.writeLine("MOZYME BFGS PM6 RHF SINGLET");
	parser.writeLineF("Temporary MOPAC Job Input  : %s", qPrintable(mopacInput));
	parser.writeLineF("Temporary MOPAC Job Output : %s", qPrintable(mopacArc));	
	for (Atom* i = aten_.currentModelOrFrame()->atoms(); i != NULL; i = i->next)
	{
		opt = 1 - i->isPositionFixed();
		parser.writeLineF("%3s %12.6f %1i %12.6f %1i %12.6f %1i", ElementMap::symbol(i), i->r().x, opt, i->r().y, opt, i->r().z, opt);
	}
	if (aten_.currentModelOrFrame()->cell().type() != UnitCell::NoCell)
	{
		Matrix mat = aten_.currentModelOrFrame()->cell().axes();
		parser.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0",mat[0], mat[1], mat[2]);
		parser.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0",mat[4], mat[5], mat[6]);
		parser.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0",mat[8], mat[9], mat[10]);
	}
	parser.closeFiles();
	
	// Ready to run command....  ATEN2 TODO
// 	if (!mopacProcess.execute(mopacCmd, NULL, mopacOut))
// 	{
// 		Messenger::print("Error: Failed to run MOPAC. Is it installed correctly?");
// 		return false;
// 	}

	// Follow output here...    ATEN2 TODO
// 	while (!mopacProcess.finished())
// 	{
// 		// Is output file already present?
// 		while (mopacProcess.outputAvailable()) mopacProcess.printLineToMessages();
// 		QApplication::processEvents();
// 	}
	
	// Check for existence of output file....
	fileInfo.setFile(mopacArc);
	if (!fileInfo.exists())
	{
		Messenger::print("Error: Can't locate MOPAC output '%s'.", qPrintable(mopacArc));
		return false;
	}

	// Time to load in the results
	// ATEN2 TODO
// 	aten_.setUseWorkingList(true);
	ReturnValue result = CommandNode::run(Commands::LoadModel, "c", qPrintable(mopacArc));

	// There should now be a model in the working model list (our results)
// 	Model* m = aten_.workingModels();
// 	if (m == NULL)
// 	{
// 		Messenger::print("Error: No results model found.");
// 		return false;
// 	}

	// Cleanup
	QFile::remove(qPrintable(mopacArc));
	QFile::remove(qPrintable(mopacOut));
	QFile::remove(qPrintable(mopacInput));

	// Copy the atoms into a temporary model
	Model tempmodel;
// 	tempmodel.copy(m);
// 	aten_.setUseWorkingList(false);   ATEN2 TODO

	// Start a new undostate in the original model
	//printf("Target for new coords = %p\n", obj.rs);
	obj.rs()->beginUndoState("MOPAC geometry optimisation");
	Atom* i, *j = obj.rs()->atoms();
	for (i = tempmodel.atoms(); i != NULL; i = i->next)
	{
		obj.rs()->positionAtom(j, i->r());
		j = j->next;
	}
	obj.rs()->endUndoState();
	
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

/*
	*** Analysis Commands
	*** src/nucommand/analyse.cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "base/messenger.h"
#include "methods/rdf.h"
#include "methods/pdens.h"
#include "methods/geometry.h"
#include "model/model.h"

// Finalise calculated quantites ('finalise')
bool NuCommand::function_Finalise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	for (Calculable *calc = obj.m->pendingQuantities.first(); calc != NULL; calc = calc->next) calc->finalise(obj.m);
	rv.reset();
	return TRUE;
}

// Accumulate data for current frame ('frameanalyse')
bool NuCommand::function_FrameAnalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Grab trajectory config for analysis
	Model *frame = obj.m->currentFrame();
	for (Calculable *calc = obj.m->pendingQuantities.first(); calc != NULL; calc = calc->next) calc->accumulate(frame);
	rv.reset();
	return TRUE;
}

// Calculate geometry ('geometry <name> <min> <binwidth> <nbins> <filename> <site1> <site2> [site3 [site4]]')
bool NuCommand::function_Geometry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Geometry::Geometry *newgeom = new Geometry::Geometry;
	obj.m->pendingQuantities.own(newgeom);
	// Set quantity name and destination filename
	newgeom->setName(c->argc(0));
	newgeom->setFilename(c->argc(4));
	// Associate sites to quantity
	newgeom->setSite(0,obj.m->findSite(c->argc(5)));
	newgeom->setSite(1,obj.m->findSite(c->argc(6)));
	if (c->hasArg(7)) newgeom->setSite(1,obj.m->findSite(c->argc(7)));
	if (c->hasArg(8)) newgeom->setSite(1,obj.m->findSite(c->argc(8)));
	newgeom->setRange(c->argd(1), c->argd(2), c->argi(3));
	rv.reset();
	return (newgeom->initialise() ? TRUE : FALSE);
}

// Accumulate data for current model ('modelanalyse')
bool NuCommand::function_ModelAnalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	for (Calculable *calc = obj.m->pendingQuantities.first(); calc != NULL; calc = calc->next) calc->accumulate(obj.m);
	rv.reset();
	return TRUE;
}

// Request calculation of a 3Ddens ('analyse pdens <name> <grid> <nsteps> <filename> <site1> <site2>')
bool NuCommand::function_PDens(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Pdens *newpdens = new Pdens;
	obj.m->pendingQuantities.own(newpdens);
	// Set pdens name and destination filename
	newpdens->setName(c->argc(0));
	newpdens->setFilename(c->argc(3));
	// Associate sites to quantity
	newpdens->setSite(0,obj.m->findSite(c->argc(4)));
	newpdens->setSite(1,obj.m->findSite(c->argc(5)));
	newpdens->setRange(c->argd(1), c->argi(2));
	rv.reset();
	return (newpdens->initialise() ? TRUE : FALSE);
}

// Print current job list ('printjobs')
bool NuCommand::function_PrintJobs(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	return FALSE;
}

// Request calculation of an RDF ('rdf <name> <rmin> <binwidth> <nbins> <filename> <site1> <site2>')
bool NuCommand::function_RDF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Rdf *newrdf = new Rdf;
	obj.m->pendingQuantities.own(newrdf);
	// Set RDF name and destination filename
	newrdf->setName(c->argc(0));
	newrdf->setFilename(c->argc(4));
	// Associate sites to quantity
	newrdf->setSite(0,obj.m->findSite(c->argc(5)));
	newrdf->setSite(1,obj.m->findSite(c->argc(6)));
	newrdf->setRange(c->argd(1), c->argd(2), c->argi(3));
	rv.reset();
	return (newrdf->initialise() ? TRUE : FALSE);
}

// Save calculated quantities to filenames provided ('savequantities')
bool NuCommand::function_SaveQuantities(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	for (Calculable *calc = obj.m->pendingQuantities.first(); calc != NULL; calc = calc->next) calc->save();
	rv.reset();
	return TRUE;
}

// Calculate quantities over entire trajectory
bool NuCommand::function_TrajAnalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	int n, startframe, totalframes, frameskip, framestodo, framesdone;
	bool calculate;
	Model *frame;
	Calculable *calc;
	// Check that the model has a trajectory associated to it
	totalframes = obj.m->nTrajectoryFrames();
	if (totalframes == 0)
	{
		msg.print("No trajectory associated to model.\n");
		rv.reset();
		return FALSE;
	}
	// Get start frame, frame skip, and frames to do (if supplied)
	startframe = c->argi(0);
	frameskip = c->argi(1);
	framestodo = (c->hasArg(2) ? c->argi(2) : -1);
	// Rewind trajectory to first frame and begin
	obj.m->seekFirstFrame();
	framesdone = 0;
	for (n=1; n <= totalframes; n++)
	{
		// Work out whether to calculate quantities from this frame
		calculate = TRUE;
		if (n < startframe) calculate = FALSE;
		else if ((n-startframe)%frameskip != 0) calculate = FALSE;
		// Calculate quantities
		if (calculate)
		{
			frame = obj.m->currentFrame();
			for (calc = obj.m->pendingQuantities.first(); calc != NULL; calc = calc->next) calc->accumulate(frame);
			framesdone ++;
		}
		// Check for required number of frames completed
		if (framesdone == framestodo) break;
		// Move to next frame
		if (n != totalframes) obj.m->seekNextFrame();
	}
	msg.print("Finished calculating properties - used %i frames from trajectory.\n", framesdone);
	rv.reset();
	return TRUE;
}

/*
	*** Analysis command functions
	*** src/command/analyse.cpp
	Copyright T. Youngs 2007

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
#include "base/debug.h"
#include "methods/rdf.h"
#include "methods/pdens.h"
#include "model/model.h"

// Finalise calculated quantites ('finalise')
int command_functions::function_CA_FINALISE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->finalise(obj.m);
	return CR_SUCCESS;
}

// Accumulate data for current frame ('frameanalyse')
int command_functions::function_CA_FRAMEANALYSE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// Grab trajectory config for analysis
	model *frame = obj.m->get_currentframe();
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(frame);
	return CR_SUCCESS;
}

// Accumulate data for current model ('modelanalyse')
int command_functions::function_CA_MODELANALYSE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(obj.m);
	return CR_SUCCESS;
}

// Request calculation of a 3Ddens ('analyse pdens <name> <site1> <site2> <grid> <nsteps> <filename>')
int command_functions::function_CA_PDENS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	pdens *newpdens = new pdens;
	obj.m->pending_quantities.own(newpdens);
	// Set pdens name and destination filename
	newpdens->set_name(c->argc(0));
	newpdens->set_filename(c->argc(5));
	// Associate sites to quantity
	newpdens->set_site(0,obj.m->find_site(c->argc(1)));
	newpdens->set_site(1,obj.m->find_site(c->argc(2)));
	newpdens->set_range(c->argd(3), c->argi(4));
	return (newpdens->initialise() ? CR_SUCCESS : CR_FAIL);
}

// Print current job list ('printjobs')
int command_functions::function_CA_PRINTJOBS(command *&c, bundle &obj)
{
	return CR_FAIL;
}

// Request calculation of an RDF ('rdf <name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>')
int command_functions::function_CA_RDF(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	rdf *newrdf = new rdf;
	obj.m->pending_quantities.own(newrdf);
	// Set RDF name and destination filename
	newrdf->set_name(c->argc(0));
	newrdf->set_filename(c->argc(6));
	// Associate sites to quantity
	newrdf->set_site(0,obj.m->find_site(c->argc(1)));
	newrdf->set_site(1,obj.m->find_site(c->argc(2)));
	newrdf->set_range(c->argd(3), c->argd(4), c->argi(5));
	return (newrdf->initialise() ? CR_SUCCESS : CR_FAIL);
}

// Save calculated quantities to filenames provided ('savequantities')
int command_functions::function_CA_SAVEQUANTITIES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->save();
	return CR_SUCCESS;
}

// Calculate quantities over entire trajectory
int command_functions::function_CA_TRAJANALYSE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	int n, startframe, totalframes, frameskip, framestodo, framesdone;
	bool calculate;
	model *frame;
	calculable *calc;
	// Check that the model has a trajectory associated to it
	totalframes = obj.m->get_totalframes();
	if (totalframes == 0)
	{
		msg(DM_NONE,"No trajectory associated to model.\n");
		return CR_FAIL;
	}
	// Get start frame, frame skip, and frames to do (if supplied)
	startframe = c->argi(0);
	frameskip = c->argi(1);
	framestodo = (c->has_arg(2) ? c->argi(2) : -1);
	// Rewind trajectory to first frame and begin
	obj.m->seek_first_frame();
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
			frame = obj.m->get_currentframe();
			for (calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(frame);
			framesdone ++;
		}
		// Check for required number of frames completed
		if (framesdone == framestodo) break;
		// Move to next frame
		if (n != totalframes) obj.m->seek_next_frame();
	}
	msg(DM_NONE,"Finished calculating properties - used %i frames from trajectory.\n", framesdone);
	return CR_SUCCESS;
}

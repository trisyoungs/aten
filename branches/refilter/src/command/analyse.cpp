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
int command_functions::function_CA_FINALISE(command *&c, objects &obj)
{
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->finalise(obj.m);
	return CR_SUCCESS;
}

// Accumulate data for current frame ('frameanalyse')
int command_functions::function_CA_FRAMEANALYSE(command *&c, objects &obj)
{
	// Grab trajectory config for analysis
	model *frame = obj.m->get_currentframe();
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(frame);
	return CR_SUCCESS;
}

// Accumulate data for current model ('modelanalyse')
int command_functions::function_CA_MODELANALYSE(command *&c, objects &obj)
{
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(obj.m);
	return CR_SUCCESS;
}

// Request calculation of a 3Ddens ('analyse pdens <name> <site1> <site2> <grid> <nsteps> <filename>')
int command_functions::function_CA_PDENS(command *&c, objects &obj)
{
	pdens *newpdens = new pdens;
	obj.m->pending_quantities.own(newpdens);
	// Set pdens name and destination filename
	newpdens->set_name(c->datavar[0]->get_as_char());
	newpdens->set_filename(c->datavar[5]->get_as_char());
	// Associate sites to quantity
	newpdens->set_site(0,obj.m->find_site(c->datavar[1]->get_as_char()));
	newpdens->set_site(1,obj.m->find_site(c->datavar[2]->get_as_char()));
	newpdens->set_range(c->datavar[3]->get_as_double(), c->datavar[4]->get_as_int());
	return (newpdens->initialise() ? CR_SUCCESS : CR_FAILED);
}

// Print current job list ('printjobs')
int command_functions::function_CA_PRINTJOBS(command *&c, objects &obj)
{
}

// Request calculation of an RDF ('rdf <name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>')
int command_functions::function_CA_RDF(command *&c, objects &obj)
{
	rdf *newrdf = new rdf;
	obj.m->pending_quantities.own(newrdf);
	// Set RDF name and destination filename
	newrdf->set_name(c->datavar[0]->get_as_char());
	newrdf->set_filename(c->datavar[6]->get_as_char());
	// Associate sites to quantity
	newrdf->set_site(0,obj.m->find_site(c->datavar[1]->get_as_char()));
	newrdf->set_site(1,obj.m->find_site(c->datavar[2]->get_as_char()));
	newrdf->set_range(c->datavar[3]->get_as_double(), c->datavar[4]->get_as_double(), c->datavar[5]->get_as_int());
	return (newrdf->initialise() ? CR_SUCCESS : CR_FAILED);
}

// Save calculated quantities to filenames provided ('savequantities')
int command_functions::function_CA_SAVEQUANTITIES(command *&c, objects &obj)
{
	for (calculable *calc = obj.m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->save();
	return CR_SUCCESS;
}


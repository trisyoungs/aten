/*
	*** Script analyse functions
	*** src/script/analyse.cpp
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

#include "script/script.h"
#include "base/debug.h"
#include "methods/rdf.h"
#include "methods/pdens.h"

// Analysis-related script commands (root=SR_ANALYSE)
bool script::command_analyse(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_analyse");
	bool result = TRUE;
	model *m, *framemodel;
	m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_analyse");
		return FALSE;
	}
	calculable *calc;
	rdf *newrdf;
	pdens *newpdens;
	site *s;
	switch (cmd->get_command())
	{
		// Accumulate data for current model ('modelanalyse')
		case (SC_MODELANALYSE):
			// Create temporary config for analysis
			for (calc = m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(m);
			break;
		// Accumulate data for current frame ('frameanalyse')
		case (SC_FRAMEANALYSE):
			// Grab trajectory config for analysis
			framemodel = m->get_currentframe();
			for (calc = m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->accumulate(framemodel);
			break;
		// Finalise calculated quantites ('finalise')
		case (SC_FINALISE):
			// Run 'finalise' method on each quantity
			for (calc = m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->finalise(m);
			break;
		// Print current job list ('printjobs')
		case (SC_PRINTJOBS):
			break;
		// Request calculation of an RDF ('rdf <name> <site1> <site2> <rmin> <binwidth> <nbins> <filename>')
		case (SC_RDF):
			newrdf = new rdf;
			m->pending_quantities.own(newrdf);
			// Set RDF name and destination filename
			newrdf->set_name(cmd->datavar[0]->get_as_char());
			newrdf->set_filename(cmd->datavar[6]->get_as_char());
			// Associate sites to quantity
			newrdf->set_site(0,m->find_site(cmd->datavar[1]->get_as_char()));
			newrdf->set_site(1,m->find_site(cmd->datavar[2]->get_as_char()));
			newrdf->set_range(cmd->datavar[3]->get_as_double(), cmd->datavar[4]->get_as_double(), cmd->datavar[5]->get_as_int());
			if (!newrdf->initialise()) result = FALSE;
			break;
		// Request calculation of a 3Ddens ('analyse pdens <name> <site1> <site2> <grid> <nsteps> <filename>')
		case (SC_PDENS):
			newpdens = new pdens;
			m->pending_quantities.own(newpdens);
			// Set pdens name and destination filename
			newpdens->set_name(cmd->datavar[0]->get_as_char());
			newpdens->set_filename(cmd->datavar[5]->get_as_char());
			// Associate sites to quantity
			newpdens->set_site(0,m->find_site(cmd->datavar[1]->get_as_char()));
			newpdens->set_site(1,m->find_site(cmd->datavar[2]->get_as_char()));
			newpdens->set_range(cmd->datavar[3]->get_as_double(), cmd->datavar[4]->get_as_int());
			if (!newpdens->initialise()) result = FALSE;
			break;
		// Save calculated quantities to filenames provided ('savequantities')
		case (SC_SAVEQUANTITIES):
			for (calc = m->pending_quantities.first(); calc != NULL; calc = calc->next) calc->save();
			break;
		default:
			printf("Error - missed analyse command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_analyse");
	return result;
}

/*
	*** File filter definition
	*** src/parse/filter.cpp
	Copyright T. Youngs 2007,2008

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

#include "parse/filter.h"
#include "parse/parser.h"
#include "base/sysfunc.h"
#include "base/master.h"
#include "model/model.h"
#include "classes/pattern.h"
#include "gui/gui.h"
#include <fstream>

// Filter types
const char *FT_strings[FT_NITEMS] = { "importmodel", "importtrajectory", "importfield", "importgrid", "exportmodel", "exporttrajectory", "exportfield", "exportgrid" };
const char *text_from_FT(FilterType ft)
	{ return FT_strings[ft]; }
FilterType FT_from_text(const char *s)
	{ return (FilterType) enumSearch("filter section",FT_NITEMS,FT_strings,s); }

// Filter commands
const char* FC_data[FC_NITEMS] =  { "name", "nickname", "extension", "glob", "exact", "zmap", "id" };
FilterCommmand FC_from_text(const char* s)
	{ return (FilterCommmand) enumSearch("", FC_NITEMS, FC_data, s); }
const char *text_from_FC(FilterCommmand fc)
	{ return FC_data[fc]; }

// Constructor
Filter::Filter()
{
	// Private variables
	type_ = FT_NITEMS;
	hasExtension_ = FALSE;
	hasZmapping_ = FALSE;
	zmapping_ = Prefs::AlphaZmap;
	name_.set("unnamed");
	glob_.set("*");
	id_ = -1;
	partner_ = NULL;
	commands_.setFilter(this);
	// Public variables
	next = NULL;
	prev = NULL;
}

// Return the ID of the filter
int Filter::id()
{
	return id_;
}

// Return the descriptive name of the filter
const char *Filter::name()
{
	return name_.get();
}

// Return the short nickname of the filter
const char *Filter::nickname()
{
	return nickname_.get();
}

// Return the file extension
const char *Filter::extension()
{
	return extension_.get();
}

// Return the aliases list
const char *Filter::exactNames()
{
	return exactNames_.get();
}

// Return whether filter has an extension
bool Filter::hasExtension()
{
	return hasExtension_;
}

// Set the partner filter
void Filter::setPartner(Filter *f)
{
	partner_ = f;
}

// Return the partner filter
Filter *Filter::partner()
{
	return partner_;
}

// Return the file filter
const char *Filter::glob()
{
	return glob_.get();
}

// Return the type of filter
FilterType Filter::type()
{
	return type_;
}

// Return the long description of the filter (including glob)
const char *Filter::description()
{
	return description_.get();
}

// Load filter (from file)
bool Filter::load(ifstream &filterFile)
{
	dbgBegin(Debug::Calls,"Filter::load");
	Command *c;
	CommandAction ca;
	FilterCommmand fc;
	char longname[256];
	Prefs::ZmapType zm;
	int success, itemsleft;
	bool done, error;
	// First, we must add a command to the flowstack so we know when to return (or raise an error)
	commands_.clear();
	// Read in commands
	while (!filterFile.eof())
	{
		success = parser.getArgsDelim(&filterFile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(Debug::None,"Filter::load - Error reading filter file.\n");
			dbgEnd(Debug::Calls,"Filter::load");
			return FALSE;
		}
		else if (success == -1) break;
		// Check branchstack - if empty then we're done (all filters have  a final 'END' command so the CA_ROOTNODE will get terminated)
		if (commands_.nBranches() == 0)
		{
			// Create long filefilter string
			sprintf(longname,"%s (%s)",name_.get(),glob_.get());
			description_ = longname;
			dbgEnd(Debug::Calls,"Filter::load");
			return TRUE;
		}
		// Check for filter specification commands
		fc = FC_from_text(parser.argc(0));
		// Some commands do not require nodes in the list, but set properties in the filter itself
		switch (fc)
		{
			// Long name of filter
			case (FC_NAME):
				name_ = parser.argc(1);
				break;
			// Nickname for filter
			case (FC_NICKNAME):
				nickname_ = parser.argc(1);
				break;
			// File extension(s)
			case (FC_EXTENSION):
				extension_ = parser.argc(1);
				break;
			// Exact filename list
			case (FC_EXACT):
				exactNames_ = parser.argc(1);
				break;
			// Set file filter glob for GUI
			case (FC_GLOB):
				glob_ = parser.argc(1);
				break;
			// Set filter ID
			case (FC_ID):
				id_ = parser.argi(1);
				break;
			// Set element zmapping to use for import
			case (FC_ZMAP):
				zm = Prefs::zmapType(parser.argc(1));
				if (zm != Prefs::nZmapTypes)
				{
					zmapping_ = zm;
					hasZmapping_ = TRUE;
				}
				break;
			// A normal command
			default:
				ca = CA_from_text(parser.argc(0));
				if (ca != CA_NITEMS)
				{
					// Add the command to the list
					if (commands_.addCommand(ca)) continue;
					else
					{
						msg(Debug::None,"Filter::load <<< Error adding command '%s' >>>>\n", parser.argc(0));
						dbgEnd(Debug::Calls,"Filter::load");
						return FALSE;
					}
				}
				else
				{
					msg(Debug::None,"Unrecognised command '%s' in filter.\n", parser.argc(0));
					dbgEnd(Debug::Calls,"Filter::load");
					return FALSE;
				}
				break;
		}
	}
	// Create long filefilter string
	sprintf(longname,"%s (%s)",name_.get(),glob_.get());
	description_ = longname;
	// Check the flowstack - it should be empty...
	itemsleft = commands_.nBranches();
	if (itemsleft != 0)
	{
		printf("Filter::load <<<< %i block%s not been terminated >>>>\n", itemsleft, (itemsleft == 1 ? " has" : "s have"));
		dbgEnd(Debug::Calls,"Filter::load");
		return FALSE;
	}
	dbgEnd(Debug::Calls,"Filter::load");
	return TRUE;
}

// Set type (and initialise any necessary variables)
void Filter::setType(FilterType ft)
{
	dbgBegin(Debug::Calls,"Filter::setType");
	type_ = ft;
	Variable *v;
	switch (type_)
	{
		case (FT_MODEL_IMPORT):
			break;
		case (FT_TRAJECTORY_IMPORT):
			v = commands_.variables.createVariable("header","",VT_CHAR);
			v = commands_.variables.createVariable("natoms","",VT_INTEGER);
			v = commands_.variables.createVariable("cell","type",VT_CHAR);
			break;
		case (FT_EXPRESSION_IMPORT):
			break;
		case (FT_GRID_IMPORT):
			break;
		case (FT_MODEL_EXPORT):
			v = commands_.variables.createVariable("cell","type",VT_CHAR);
			v = commands_.variables.createVariable("cell","a",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","b",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","c",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","alpha",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","beta",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","gamma",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","ax",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","ay",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","az",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","bx",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","by",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","bz",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","cx",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","cy",VT_DOUBLE);
			v = commands_.variables.createVariable("cell","cz",VT_DOUBLE);
			v = commands_.variables.createVariable("natoms","",VT_INTEGER);
			v = commands_.variables.createVariable("title","",VT_CHAR);
			break;
		case (FT_TRAJECTORY_EXPORT):
			v = commands_.variables.createVariable("header","",VT_CHAR);
			v = commands_.variables.createVariable("natoms","",VT_INTEGER);
			break;
		case (FT_EXPRESSION_EXPORT):
			v = commands_.variables.createVariable("title","",VT_CHAR);
			v = commands_.variables.createVariable("npatterns","",VT_INTEGER);
			v = commands_.variables.createVariable("energyunit","",VT_CHAR);
			v = commands_.variables.createVariable("ntypes","",VT_INTEGER);
			break;
		case (FT_GRID_EXPORT):
			break;
	}
	dbgEnd(Debug::Calls,"Filter::setType");
}


// Print
void Filter::print()
{
	dbgBegin(Debug::Calls,"Filter::print");
	printf("Filter Name : '%s'\n",name_.get());
	printf(" Shell glob : '%s'\n",glob_.get());
	printf(" Extensions : '%s'\n",extension_.get());
	printf("Exact Names : '%s'\n",exactNames_.get());
	printf("       Type : %s\n",text_from_FT(type_));
	dbgEnd(Debug::Calls,"Filter::print");
}

// Execute filter
bool Filter::execute(const char *filename, ifstream *trajfile, bool trajheader, Model *framemodel)
{
	dbgBegin(Debug::Calls,"Filter::execute");
	// Grab pointer Bundle from master
	Bundle &obj = master.current;
	// Set element mapping type to that specified in file
	Prefs::ZmapType temp_zmap = prefs.zmapType();
	if (hasZmapping_) prefs.setZmapType(zmapping_);
	// Setup based on filter type...
	switch (type_)
	{
		case (FT_MODEL_IMPORT):
			msg(Debug::None,"Load Model : %s (%s)\n", filename, name_.get());
			// Reset reserved variables
			commands_.variables.set("title","Unnamed");
			// Open file and set target
			if (!commands_.setInputFile(filename))
			{
				msg(Debug::None,"Error opening input file '%s'.\n",filename);
				dbgEnd(Debug::Calls,"Filter::execute");
				return FALSE;
			}
			break;
		case (FT_MODEL_EXPORT):
			msg(Debug::None,"Save Model : %s (%s)...", obj.m->filename(), name_.get());
			// Open file and set target
			if (!commands_.setOutputFile(obj.m->filename()))
			{
				msg(Debug::None,"Error opening output file '%s'.\n",obj.m->filename());
				dbgEnd(Debug::Calls,"Filter::execute");
				return FALSE;
			}
			// Set variables
			commands_.setModelVariables(obj.m);
			commands_.setCellVariables(obj.m->cell());
			break;
		case (FT_EXPRESSION_EXPORT):
			msg(Debug::None,"Save Field : %s (%s)\n", filename, name_.get());
			// Need a valid pattern and energy expression to export
			if (!obj.m->autocreatePatterns() || !obj.m->createExpression())
			{
				msg(Debug::None,"Filter::execute - Must have valid pattern and energy expression to export a field file\n.");
				dbgEnd(Debug::Calls,"Filter::execute");
				return FALSE;
			}
			// Set variables
			commands_.variables.set("title",obj.m->name());
			commands_.variables.set("npatterns",obj.m->nPatterns());
			commands_.variables.set("energyunit",Prefs::energyUnitKeyword(prefs.energyUnit()));
			commands_.variables.set("ntypes",obj.m->nUniqueTypes());
			// Open file...
			if (!commands_.setOutputFile(filename))
			{
				msg(Debug::None,"Error opening field file '%s'.\n", filename);
				dbgEnd(Debug::Calls,"Filter::execute");
				return FALSE;
			}
			break;
		case (FT_GRID_IMPORT):
			msg(Debug::None,"Load Grid  : %s (%s)\n", filename, name_.get());
			// Open file...
			if (!commands_.setInputFile(filename))
			{
				msg(Debug::None,"Error opening grid file '%s'.\n", filename);
				dbgEnd(Debug::Calls,"Filter::execute");
				return FALSE;
			}
			break;
		case (FT_GRID_EXPORT):
			msg(Debug::None,"Save Grid  : %s (%s)\n",filename,name_.get());
			// Open file...
			if (!commands_.setOutputFile(filename))
			{
				msg(Debug::None,"Error opening grid file '%s'.\n", filename);
				dbgEnd(Debug::Calls,"Filter::execute");
				return FALSE;
			}
			break;
		case (FT_TRAJECTORY_IMPORT):
			// Set variables
			commands_.variables.set("header",(trajheader ? "true" : "false"));
			commands_.variables.set("frame",(trajheader ? "false" : "true"));
			// Set model target (if reading a frame)
			if (!trajheader)
			{
				Model *parent = framemodel->trajectoryParent();
				if (parent == NULL)
				{
					msg(Debug::None,"Filter::read_trajectory <<<< Trajectory parent is not set in frame model >>>>\n");
					dbgEnd(Debug::Calls,"Filter::read_trajectory(frame)");
					return FALSE;	
				}
				commands_.variables.set("natoms",parent->nAtoms());
				commands_.variables.set("cell.type",lowerCase(text_from_CT(parent->cell()->type())));
				framemodel->clear();
			}
			else
			{
				commands_.variables.set("natoms",framemodel->nAtoms());
				commands_.variables.set("cell.type",lowerCase(text_from_CT(framemodel->cell()->type())));
			}

	}
	// Execute CommandList
	bool result = commands_.execute(framemodel,trajfile);
	// Perform post-filter operations
	switch (type_)
	{
		case (FT_MODEL_IMPORT):
			// Reset element mapping style
			prefs.setZmapType(temp_zmap);
			commands_.closeFiles();
			msg(Debug::None,"Model import %s.\n",(result ? "completed" : "failed"));
			break;
		case (FT_MODEL_EXPORT):
			obj.m->updateSavePoint();
			commands_.closeFiles();
			msg(Debug::None,"Model export %s.\n",(result ? "completed" : "failed"));
			break;
		case (FT_EXPRESSION_EXPORT):
			commands_.closeFiles();
			msg(Debug::None,"Field export %s.\n",(result ? "completed" : "failed"));
			break;
		case (FT_TRAJECTORY_IMPORT):
			//commands_.close_files();
			//if (trajheader) (result ? msg(Debug::None,"Trajectory opened successfully.\n") : msg(Debug::None,"Failed to open trajectory.\n"));
			//else if (!result) msg(Debug::None,"Failed to read frame from trajectory.\n");
			break;
		case (FT_GRID_IMPORT):
			commands_.closeFiles();
			msg(Debug::None,"Grid import %s.\n",(result ? "completed" : "failed"));
			break;
		case (FT_GRID_EXPORT):
			commands_.closeFiles();
			msg(Debug::None,"Grid export %s.\n",(result ? "completed" : "failed"));
			break;
	}
	dbgEnd(Debug::Calls,"Filter::execute");
	return result;
}


/*
	*** File filter definition
	*** src/parse/filter.cpp
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

#include "parse/filter.h"
#include "parse/parser.h"
#include "base/sysfunc.h"
#include "base/aten.h"
#include "model/model.h"
#include "classes/pattern.h"
#include "gui/gui.h"
#include <fstream>

// Filter types
const char *FilterTypeKeywords[Filter::nFilterTypes] = { "importmodel", "importtrajectory", "importfield", "importgrid", "exportmodel", "exporttrajectory", "exportfield", "exportgrid" };
const char *Filter::filterType(Filter::FilterType ft)
{
	return FilterTypeKeywords[ft];
}
Filter::FilterType Filter::filterType(const char *s)
{
	return (Filter::FilterType) enumSearch("filter type", Filter::nFilterTypes, FilterTypeKeywords, s);
}

// Filter commands
const char* FilterCommandKeywords[Filter::nFilterCommands] =  { "name", "nickname", "extension", "glob", "exact", "zmap", "id" };
Filter::FilterCommmand Filter::filterCommand(const char* s)
{
	return (Filter::FilterCommmand) enumSearch("", Filter::nFilterCommands, FilterCommandKeywords, s);
}
const char *Filter::filterCommand(Filter::FilterCommmand fc)
{
	return FilterCommandKeywords[fc];
}

// Constructor
Filter::Filter()
{
	// Private variables
	type_ = Filter::nFilterTypes;
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
Dnchar *Filter::extensions()
{
	return extensions_.first();
}

// Return the aliases list
Dnchar *Filter::exactNames()
{
	return exactNames_.first();
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
Filter::FilterType Filter::type()
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
	msg.enter("Filter::load");
	FilterCommmand fc;
	char longname[256];
	Prefs::ZmapType zm;
	int success, itemsleft, n;
	Dnchar *d;
	// First, we must add a command to the flowstack so we know when to return (or raise an error)
	commands_.clear();
	// Read in commands
	while (!filterFile.eof())
	{
		success = parser.getArgsDelim(&filterFile, Parser::UseQuotes+Parser::SkipBlanks);
		if (success == 1)
		{
			msg.print("Filter::load - Error reading filter file.\n");
			msg.exit("Filter::load");
			return FALSE;
		}
		else if (success == -1) break;
		// Check branchstack - if empty then we're done (all filters have  a final 'END' command so the CA_ROOTNODE will get terminated)
		if (commands_.nBranches() == 0) break;
		// Check for filter specification commands
		fc = Filter::filterCommand(parser.argc(0));
		// Some commands do not require nodes in the list, but set properties in the filter itself
		switch (fc)
		{
			// Long name of filter
			case (Filter::NameCommand):
				name_ = parser.argc(1);
				break;
			// Nickname for filter
			case (Filter::NicknameCommand):
				nickname_ = parser.argc(1);
				break;
			// File extension(s)
			case (Filter::ExtensionCommand):
				for (n=1; n<parser.nArgs(); n++)
				{
					d = extensions_.add();
					d->set(parser.argc(n));
				}
				break;
			// Exact filename list
			case (Filter::ExactCommand):
				for (n=1; n<parser.nArgs(); n++)
				{
					d = exactNames_.add();
					d->set(parser.argc(n));
				}
				break;
			// Set file filter glob for GUI
			case (Filter::GlobCommand):
				glob_ = parser.argc(1);
				break;
			// Set filter ID
			case (Filter::IdCommand):
				id_ = parser.argi(1);
				break;
			// Set element zmapping to use for import
			case (Filter::ZMapCommand):
				zm = Prefs::zmapType(parser.argc(1));
				if (zm != Prefs::nZmapTypes)
				{
					zmapping_ = zm;
					hasZmapping_ = TRUE;
				}
				break;
			// A normal command
			default:
				if (commands_.cacheCommand()) continue;
				else
				{
					msg.exit("Filter::load");
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
		msg.print("%i block%s not been terminated within the filter.\n", itemsleft, (itemsleft == 1 ? " has" : "s have"));
		msg.exit("Filter::load");
		return FALSE;
	}
	// Check a few other things:
	// At least one extension must have been specified
	if (extensions_.nItems() == 0)
	{
		msg.print("At least one extension must be specified for the filter.\n");
		msg.exit("Filter::load");
		return FALSE;
	}
	msg.exit("Filter::load");
	return TRUE;
}

// Set type (and initialise any necessary variables)
void Filter::setType(FilterType ft)
{
	msg.enter("Filter::setType");
	type_ = ft;
	Variable *v;
	switch (type_)
	{
		case (Filter::ModelImport):
			v = commands_.variables.createVariable("title","",Variable::CharacterVariable);
			break;
		case (Filter::TrajectoryImport):
			v = commands_.variables.createVariable("header","",Variable::CharacterVariable);
			v = commands_.variables.createVariable("natoms","",Variable::IntegerVariable);
			v = commands_.variables.createVariable("cell","type",Variable::CharacterVariable);
			break;
		case (Filter::ExpressionImport):
			break;
		case (Filter::GridImport):
			v = commands_.variables.createVariable("title","",Variable::CharacterVariable);
			break;
		case (Filter::ModelExport):
			commands_.createModelVariables("");
			break;
		case (Filter::TrajectoryExport):
			v = commands_.variables.createVariable("header","",Variable::CharacterVariable);
			v = commands_.variables.createVariable("natoms","",Variable::IntegerVariable);
			break;
		case (Filter::ExpressionExport):
			v = commands_.variables.createVariable("energyunit","",Variable::CharacterVariable);
			v = commands_.variables.createVariable("natomtypes","",Variable::IntegerVariable);
			v = commands_.variables.createVariable("nbondterms","",Variable::IntegerVariable);
			v = commands_.variables.createVariable("nangleterms","",Variable::IntegerVariable);
			v = commands_.variables.createVariable("ntorsionterms","",Variable::IntegerVariable);
			v = commands_.variables.createVariable("npatterns","",Variable::IntegerVariable);
			commands_.createModelVariables("");
			break;
		case (Filter::GridExport):
			break;
	}
	msg.exit("Filter::setType");
}


// Print
void Filter::print()
{
	msg.enter("Filter::print");
	char s[512];
	printf("Filter Name : '%s'\n", name_.get());
	printf(" Shell glob : '%s'\n", glob_.get());
	s[0] = '\0';
	for (Dnchar *d = extensions_.first(); d != NULL; d = d->next)
	{
		strcat(s,d->get());
		strcat(s," ");
	}
	printf(" Extensions : '%s'\n", s);
	s[0] = '\0';
	for (Dnchar *d = exactNames_.first(); d != NULL; d = d->next)
	{
		strcat(s,d->get());
		strcat(s," ");
	}
	printf("Exact Names : '%s'\n", s);
	printf("       Type : %s\n", Filter::FilterType(type_));
	msg.exit("Filter::print");
}

// Execute filter
bool Filter::execute(const char *filename, ifstream *trajfile, bool trajheader)
{
	msg.enter("Filter::execute");
	bool result, proceed;
	// Grab pointer Bundle from master
	Bundle &obj = aten.current;
	// Set element mapping type to that specified in file
	Prefs::ZmapType temp_zmap = prefs.zmapType();
	if (hasZmapping_) prefs.setZmapType(zmapping_);
	// Flag to indicate we should proceed to execute filter
	proceed = TRUE;
	// Setup based on filter type...
	switch (type_)
	{
		case (Filter::ModelImport):
			msg.print("Load Model : %s (%s)\n", filename, name_.get());
			// Reset reserved variables
			commands_.variables.set("title",filename);
			// Open file and set target
			if (!commands_.setInputFile(filename))
			{
				msg.print("Error opening input file '%s'.\n",filename);
				proceed = FALSE;
			}
			break;
		case (Filter::ModelExport):
			msg.print("Save Model : %s (%s)...", obj.rs->filename(), name_.get());
			// Open file and set target
			if (!commands_.setOutputFile(obj.rs->filename()))
			{
				msg.print("Error opening output file '%s'.\n",obj.rs->filename());
				proceed = FALSE;
				break;
			}
			// Set variables
			commands_.setModelVariables("",obj.rs);
			break;
		case (Filter::ExpressionExport):
			msg.print("Save Field : %s (%s)\n", filename, name_.get());
			// Need a valid pattern and energy expression to export
			if (!obj.rs->autocreatePatterns() || !obj.rs->createExpression())
			{
				msg.print("Filter::execute - Must have valid pattern and energy expression to export a field file\n.");
				proceed = FALSE;
			}
			// Generate unique term lists
			obj.rs->createUniqueLists();
			// Set variables
			commands_.setModelVariables("",obj.rs);
			commands_.variables.set("npatterns",obj.rs->nPatterns());
			commands_.variables.set("energyunit",Prefs::energyUnit(prefs.energyUnit()));
			commands_.variables.set("natomtypes",obj.rs->nUniqueTypes());
			commands_.variables.set("nbondterms",obj.rs->nUniqueBondTerms());
			commands_.variables.set("nangleterms",obj.rs->nUniqueAngleTerms());
			commands_.variables.set("ntorsionterms",obj.rs->nUniqueTorsionTerms());
			// Open file...
			if (!commands_.setOutputFile(filename))
			{
				msg.print("Error opening field file '%s'.\n", filename);
				proceed = FALSE;
			}
			break;
		case (Filter::GridImport):
			msg.print("Load Grid  : %s (%s)\n", filename, name_.get());
			// Reset reserved variables
			commands_.variables.set("title",filename);
			// Open file...
			if (!commands_.setInputFile(filename))
			{
				msg.print("Error opening grid file '%s'.\n", filename);
				proceed = FALSE;
			}
			break;
		case (Filter::GridExport):
			msg.print("Save Grid  : %s (%s)\n",filename,name_.get());
			// Open file...
			if (!commands_.setOutputFile(filename))
			{
				msg.print("Error opening grid file '%s'.\n", filename);
				proceed = FALSE;
			}
			break;
		case (Filter::TrajectoryImport):
			// Set variables
			commands_.variables.set("header",(trajheader ? "true" : "false"));
			commands_.variables.set("frame",(trajheader ? "false" : "true"));
			if (obj.rs == NULL)
			{
				msg.print("No current model set for trajectory import.\n");
				proceed = FALSE;
				break;
			}
			// Set model target (if reading a frame)
			if (!trajheader)
			{
				//Model *parent = framemodel->trajectoryParent();
				if (obj.rs->renderSource() == obj.m)
				{
					msg.print("Trajectory frame model has not been set for trajectory import.\n");
					proceed = FALSE;
					break;
				}
				//obj.rs->renderSource()->clear();
			}
			commands_.variables.set("natoms",obj.m->nAtoms());
			commands_.variables.set("cell.type",lowerCase(Cell::cellType(obj.m->cell()->type())));
			break;
	}
	// Execute CommandList
	result = (proceed ? commands_.execute(trajfile) : FALSE);
	// Perform post-filter operations
	switch (type_)
	{
		case (Filter::ModelImport):
			// Reset element mapping style
			prefs.setZmapType(temp_zmap);
			commands_.closeFiles();
			msg.print("Model import %s.\n",(result ? "completed" : "failed"));
			break;
		case (Filter::ModelExport):
			obj.rs->changeLog.updateSavePoint();
			commands_.closeFiles();
			msg.print("Model export %s.\n",(result ? "completed" : "failed"));
			break;
		case (Filter::ExpressionExport):
			commands_.closeFiles();
			msg.print("Field export %s.\n",(result ? "completed" : "failed"));
			break;
		case (Filter::TrajectoryImport):
			//commands_.close_files();
			//if (trajheader) (result ? msg.print("Trajectory opened successfully.\n") : msg.print("Failed to open trajectory.\n"));
			//else if (!result) msg.print("Failed to read frame from trajectory.\n");
			break;
		case (Filter::GridImport):
			// Reset element mapping style
			prefs.setZmapType(temp_zmap);
			commands_.closeFiles();
			msg.print("Grid import %s.\n",(result ? "completed" : "failed"));
			break;
		case (Filter::GridExport):
			commands_.closeFiles();
			msg.print("Grid export %s.\n",(result ? "completed" : "failed"));
			break;
	}
	msg.exit("Filter::execute");
	return result;
}


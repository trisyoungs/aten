/*
	*** Aten Filter-Specific Routines
	*** src/main/aten_filters.cpp
	Copyright T. Youngs 2007-2013

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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"

// Load filters
void Aten::openFilters()
{
	msg.enter("Aten::openFilters");
	Dnchar path;
	bool found = FALSE;
	int nfailed;

	nFiltersFailed_ = 0;
	failedFilters_.clear();

	// Construct a list of possible locations for the filters
	QStringList paths;
	if (!aten.dataDir_.isEmpty())
	{
		msg.print(Messenger::Verbose, "Aten::openFilters() - data directory is '%s'.\n", dataDir_.get());
		paths << dataDir_.get();
	}
	else
	{
		msg.print(Messenger::Verbose, "Data directory has not yet been set. Default locations will be searched...\n");
		// Default locations
		paths << "/usr/share/aten";
		paths << "/usr/local/share/aten";
		paths << "../share/aten";
		paths << gui.application()->applicationDirPath() + "/../share/aten";
		paths << gui.application()->applicationDirPath() + "/../SharedSupport";
	}

	for (int i=0; i < paths.size(); i++)
	{
		path.sprintf("%s/filters", qPrintable(paths.at(i)));
		path = qPrintable(QDir::toNativeSeparators(path.get()));
		msg.print(Messenger::Verbose, "Looking for filters in '%s'...\n", path.get());
		nfailed = parseFilterDir(path);
		if (nfailed == -1) continue;	// Directory not found
		found = TRUE;
		nFiltersFailed_ += nfailed;
		dataDir_ = qPrintable(QDir::toNativeSeparators(paths.at(i)));
		break;
	}

	if (!found)
	{
		msg.print(Messenger::Always, "No filters found in any known default locations.\n");
		msg.print(Messenger::Always, "Probable solutions:\n");
		msg.print(Messenger::Always, "  1) Set the environment variable ATENDATA to point to the installed location of the filters\n");
		msg.print(Messenger::Always, "         e.g. (in Linux) 'export ATENDATA=/usr/share/aten/'\n");
		msg.print(Messenger::Always, "  2) Set the environment variable ATENDATA to point to the location of the 'data' directory in the source tree\n");
		msg.print(Messenger::Always, "         e.g. (in Linux) 'export ATENDATA=/home/bob/src/aten-1.2/data)\n");
		msg.print(Messenger::Always, "  3) Use the command-line switch --atendata <dir> to specify either of the above locations\n");
		msg.print(Messenger::Always, "         e.g. (in Linux) 'aten --atendata /home/bob/src/aten-1.2/data\n");
		nFiltersFailed_ = -1;
	}

	// Try to load user filters - we don't mind if the directory doesn't exist...
	path.sprintf("%s%c%s%cfilters%c", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP, PATHSEP);
	path = qPrintable(QDir::toNativeSeparators(path.get()));
	msg.print(Messenger::Verbose, "Looking for user filters in '%s'...\n", path.get());
	nfailed = parseFilterDir(path);
	if (nfailed > 0) nFiltersFailed_ += nfailed;

	// Print out info and partner filters if all was successful
	if (found)
	{
		partnerFilters();
		msg.print(Messenger::Verbose, "Found (import/export):  Models (%i/%i) ", filters_[FilterData::ModelImport].nItems(), filters_[FilterData::ModelExport].nItems());
		msg.print(Messenger::Verbose, "Trajectory (%i/%i) ", filters_[FilterData::TrajectoryImport].nItems(), filters_[FilterData::TrajectoryExport].nItems());
		msg.print(Messenger::Verbose, "Expression (%i/%i) ", filters_[FilterData::ExpressionImport].nItems(), filters_[FilterData::ExpressionExport].nItems());
		msg.print(Messenger::Verbose, "Grid (%i/%i)\n", filters_[FilterData::GridImport].nItems(), filters_[FilterData::GridExport].nItems());
	}
	msg.exit("Aten::openFilters");
}

// Load filter from specified filename
bool Aten::openFilter(const char *filename)
{
	msg.enter("Aten::openFilter");
	// Construct filter Program...
	Program *f = filterPrograms_.add();
	if (!f->generateFromFile(filename, filename, TRUE, TRUE, TRUE))
	{
		msg.print("Failed to load filters from '%s'...\n", filename);
		failedFilters_.add()->set( filename );
		filterPrograms_.remove(f);
		msg.exit("Aten::openFilter");
		return FALSE;
	}
	msg.exit("Aten::openFilter");
	return TRUE;
}

// Register a filter of a given type at start of list
void Aten::registerFilter(Tree *filter, FilterData::FilterType ft)
{
	filters_[ft].addStart(filter);
}

// Reload filters
int Aten::reloadFilters()
{
	msg.enter("Aten::reloadFilters");
	Dnchar path;
	msg.print("Clearing current filters....\n");
	filters_[FilterData::ModelImport].clear();
	filters_[FilterData::ModelExport].clear();
	filters_[FilterData::TrajectoryImport].clear();
	filters_[FilterData::TrajectoryExport].clear();
	filters_[FilterData::ExpressionImport].clear();
	filters_[FilterData::ExpressionExport].clear();
	filters_[FilterData::GridImport].clear();
	filters_[FilterData::GridExport].clear();
	filterPrograms_.clear();
	nFiltersFailed_ = 0;
	failedFilters_.clear();

	// Load filters
	path.sprintf("%s%cfilters", dataDir_.get(), PATHSEP);
	path = qPrintable(QDir::toNativeSeparators(path.get()));

	msg.print("Reading filters from '%s'...\n", path.get());
	int result = parseFilterDir(path);

	// Print out info and partner filters 
	partnerFilters();
	msg.print("Found (import/export):  Models (%i/%i) ", filters_[FilterData::ModelImport].nItems(), filters_[FilterData::ModelExport].nItems());
	msg.print("Trajectory (%i/%i) ", filters_[FilterData::TrajectoryImport].nItems(), filters_[FilterData::TrajectoryExport].nItems());
	msg.print("Expression (%i/%i) ", filters_[FilterData::ExpressionImport].nItems(), filters_[FilterData::ExpressionExport].nItems());
	msg.print("Grid (%i/%i)\n", filters_[FilterData::GridImport].nItems(), filters_[FilterData::GridExport].nItems());

	msg.exit("Aten::reloadFilters");
	return result;
}

// Return status of filter load on startup
int Aten::nFiltersFailed() const
{
	return nFiltersFailed_;
}

// Return status of filter load on startup
Dnchar *Aten::failedFilters() const
{
	return failedFilters_.first();
}

// Parse filter index file (rooted in the path provided)
int Aten::parseFilterDir(const char *path)
{
	msg.enter("Aten::parseFilterDir");
	int i, nfailed = 0;
	Dnchar s("--> ");
	// First check - does this directory actually exist
	QDir filterdir(path);
	if (!filterdir.exists())
	{
		msg.exit("Aten::parseFilterDir");
		return -1;
	}
	// Filter the directory contents - show only files and exclude '.' and '..'
	QStringList filterlist = filterdir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<filterlist.size(); i++)
	{
		// Construct filter Program...
		Program *f = filterPrograms_.add();
		QString filename(path);
		filename += "/";
		filename += filterlist.at(i);
		if (!f->generateFromFile(qPrintable(QDir::toNativeSeparators(filename)), qPrintable(filterlist.at(i)), TRUE, TRUE, TRUE))
		{
			msg.print("Failed to load filters from '%s'...\n", qPrintable(filterlist.at(i)));
			failedFilters_.add()->set( qPrintable(QDir::toNativeSeparators(filename)) );
			nfailed ++;
			filterPrograms_.remove(f);
		}
		else
		{
			// Add on a bit of useful text to print out
			s.strcatf("%s  ", qPrintable(filterlist.at(i)));
		}
	}
	s += '\n';
	msg.print(Messenger::Verbose, s);
	msg.exit("Aten::parseFilterDir");
	return nfailed;
}

// Set filter partners
void Aten::partnerFilters()
{
	msg.enter("Aten::partnerFilters");
	// Loop through import filters and search / set export partners
	Dnchar s("Model Formats:");
	Refitem<Tree,int> *ri, *rj;
	Tree *imp, *exp;
	int importid;
	for (ri = filters_[FilterData::ModelImport].first(); ri != NULL; ri = ri->next)
	{
		imp = ri->item;
		importid = imp->filter.id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (rj = filters_[FilterData::ModelExport].first(); rj != NULL; rj = rj->next)
			{
				exp = rj->item;
				if (importid == exp->filter.id())
				{
					msg.print(Messenger::Verbose, "--- Partnering model filters for '%s', id = %i\n", imp->filter.nickname(), imp->filter.id());
					imp->filter.setPartner(exp);
					break;
				}
			}
		}
		s.strcatf(" %s[r%c]", imp->filter.nickname(), exp == NULL ? 'o' : 'w');
	}
	s+= '\n';
	msg.print(Messenger::Verbose, s);
	s = "Grid Formats :";
	for (ri = filters_[FilterData::GridImport].first(); ri != NULL; ri = ri->next)
	{
		imp = ri->item;
		importid = imp->filter.id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (rj = filters_[FilterData::GridExport].first(); rj != NULL; rj = rj->next)
			{
				exp = rj->item;
				if (importid == exp->filter.id())
				{
					msg.print(Messenger::Verbose, "--- Partnering grid filters for '%s', id = %i\n", imp->filter.nickname(), imp->filter.id());
					imp->filter.setPartner(exp);
					printf("w]");
					break;
				}
			}
		}
		s.strcatf(" %s[r%c]", imp->filter.nickname(), exp == NULL ? 'o' : 'w');
	}
	s += '\n';
	msg.print(Messenger::Verbose, s);
	msg.exit("Aten::partnerFilters");
}

// Find filter with specified type and nickname
Tree *Aten::findFilter(FilterData::FilterType ft, const char *nickname) const
{
	msg.enter("Aten::findFilter");
	Refitem<Tree,int> *result;
	for (result = filters_[ft].first(); result != NULL; result = result->next)
		if (strcmp(result->item->filter.nickname(), nickname) == 0) break;
	if (result == NULL) msg.print("No %s filter with nickname '%s' defined.\n", FilterData::filterType(ft), nickname);
	msg.exit("Aten::findFilter");
	return (result == NULL ? NULL : result->item);
}

// Find filter by description
Tree *Aten::findFilterByDescription(FilterData::FilterType ft, const char *description) const
{
	msg.enter("Aten::findFilterByDescription");
	Refitem<Tree,int> *result;
	for (result = filters_[ft].first(); result != NULL; result = result->next)
		if (strcmp(result->item->filter.description(), description) == 0) break;
// 	if (result == NULL) msg.print("Internal Error: No %s filter matches description '%s'.\n", FilterData::filterType(ft), description);
	msg.exit("Aten::findFilterByDescription");
	return (result == NULL ? NULL : result->item);
}

// Return first filter refitem in list (of a given type)
Refitem<Tree,int> *Aten::filters(FilterData::FilterType ft) const
{
	return filters_[ft].first();
}

// Return nth filter in list (of a given type)
Refitem<Tree,int> *Aten::filter(FilterData::FilterType ft, int index)
{
	if ((index < 0) || (index >= filters_[ft].nItems()))
	{
		printf("Internal Error : Index %i is out of range for '%s' filter list.\n", index, FilterData::filterType(ft));
		return NULL;
	}
	return filters_[ft][index];
}

// Return number of filters of a given type
int Aten::nFilters(FilterData::FilterType ft) const
{
	return filters_[ft].nItems();
}

// Return pointer to list of filters of given type
Reflist<Tree,int> *Aten::filterList(FilterData::FilterType ft)
{
	return &filters_[ft];
}

// Print list of valid filter nicknames
void Aten::printValidNicknames(FilterData::FilterType ft)
{
	msg.print("Valid %s nicknames are:\n", FilterData::filterType(ft));
	if (filters_[ft].nItems() == 0) msg.print("  <None Available>\n");
	for (Refitem<Tree,int> *ri = filters_[ft].first(); ri != NULL; ri = ri->next)
		msg.print("  %-15s %s\n", ri->item->filter.nickname(), ri->item->filter.name());
}

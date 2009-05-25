/*
	*** Aten Filter-Specific Routines
	*** src/aten/aten_filters.cpp
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

#include "main/aten.h"
#include "gui/gui.h"

// Load filters
void Aten::openFilters()
{
	msg.enter("Aten::openFilters");
	char path[512];
	bool found = FALSE;
	int nfailed;

	nFiltersFailed_ = 0;
	failedFilters_.clear();

	// Construct a list of possible locations for the filters
	QStringList paths;
	if (!aten.dataDir_.isEmpty())
	{
		msg.print(Messenger::Verbose, "$ATENDATA points to '%s'.\n", dataDir_.get());
		paths << dataDir_.get();
	}
	else msg.print(Messenger::Verbose, "$ATENDATA has not been set. Default locations will be searched...\n");
	// Default locations
	paths << "/usr/share/aten";
	paths << "/usr/local/share/aten";
	paths << gui.app->applicationDirPath() + "/../share/aten";
	paths << gui.app->applicationDirPath() + "/../SharedSupport";

	for (int i=0; i < paths.size(); i++)
	{
		sprintf(path,"%s/filters", qPrintable(paths.at(i)));
		QDir pathname(path);
		msg.print(Messenger::Verbose, "Looking for filters in '%s'...\n", qPrintable(pathname.path()));
		nfailed = parseFilterDir( qPrintable(pathname.path()) );
		if (nfailed == -1) continue;	// Directory not found
		found = TRUE;
		nFiltersFailed_ += nfailed;
		dataDir_ = qPrintable(pathname.path());
		break;
	}

	if (!found)
	{
		msg.print(Messenger::Error, "No filters found in any known locations.\n");
		msg.print(Messenger::Error, "Set $ATENDATA to point to the (installed) location of the 'data' directory, or to the directory that contains Aten's 'filters' directory:\n");
		msg.print(Messenger::Error, "      e.g. (in bash) 'export ATENDATA=/usr/share/aten/' on most systems.\n");
		msg.print(Messenger::Error, "Alternatively, use the command-line switch --atendata <dir> to specify a location.\n");
		nFiltersFailed_ = -1;
	}

	// Try to load user filters - we don't mind if the directory doesn't exist...
	sprintf(path,"%s%s", homeDir_.get(), "/.aten/filters/");
	msg.print(Messenger::Verbose, "Looking for user filter index in '%s'...\n", path);
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
	// Construct Forest...
	Forest *f = filterForests_.add();
	if (!f->generateFromFile(filename, filename, TRUE))
	{
		msg.print("Failed to load filters from '%s'...\n", filename);
		failedFilters_.add()->set( filename );
		filterForests_.remove(f);
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
bool Aten::reloadFilters()
{
	msg.enter("Aten::reloadFilters");
	char indexfile[512], path[512], filename[128], message[512];
	ifstream *file;
	msg.print("Clearing current filters....\n");
	filters_[FilterData::ModelImport].clear();
	filters_[FilterData::ModelExport].clear();
	filters_[FilterData::TrajectoryImport].clear();
	filters_[FilterData::TrajectoryExport].clear();
	filters_[FilterData::ExpressionImport].clear();
	filters_[FilterData::ExpressionExport].clear();
	filters_[FilterData::GridImport].clear();
	filters_[FilterData::GridExport].clear();
	filterForests_.clear();
	nFiltersFailed_ = 0;
	failedFilters_.clear();

	// Load filters
	sprintf(path,"%s%s", dataDir_.get(), "/filters");
	msg.print("Reading filters from '%s'...\n", path);
	bool result = parseFilterDir(path);

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
int Aten::nFiltersFailed()
{
	return nFiltersFailed_;
}

// Return status of filter load on startup
Dnchar *Aten::failedFilters()
{
	return failedFilters_.first();
}

// Parse filter index file (rooted in the path provided)
int Aten::parseFilterDir(const char *path)
{
	msg.enter("Aten::parseFilterDir");
	int i, nfailed = 0;
	char s[8096], bit[128];
	strcpy(s, "--> ");
	// First check - does this directory actually exist
	QDir filterdir(path);
	if (!filterdir.exists())
	{
		msg.enter("Aten::parseFilterDir");
		return -1;
	}
	// Filter the directory contents - show only files and exclude '.' and '..'
	QStringList filterlist = filterdir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<filterlist.size(); i++)
	{
		// Construct Forest...
		Forest *f = filterForests_.add();
		if (!f->generateFromFile(qPrintable(QString(path)+"/"+filterlist.at(i)), qPrintable(filterlist.at(i)), TRUE))
		{
			msg.print("Failed to load filters from '%s'...\n", qPrintable(filterlist.at(i)));
			failedFilters_.add()->set( qPrintable(QString(path)+"/"+filterlist.at(i)) );
			nfailed ++;
			filterForests_.remove(f);
		}
		else
		{
			// Add on a bit of useful text to print out
			sprintf(bit, "%s  ", qPrintable(filterlist.at(i)));
			strcat(s, bit);
		}
	}
	strcat(s, "\n");
	msg.print(Messenger::Verbose, s);
	msg.exit("Aten::parseFilterDir");
	return nfailed;
}

// Set filter partners
void Aten::partnerFilters()
{
	msg.enter("Aten::partnerFilters");
	// Loop through import filters and search / set export partners
	char s[512], bit[32];
	Refitem<Tree,int> *ri, *rj;
	Tree *imp, *exp;
	int importid;
	strcpy(s,"Model Formats:");
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
		sprintf(bit, " %s[r%c]", imp->filter.nickname(), exp == NULL ? 'o' : 'w');
		strcat(s,bit);
	}
	strcat(s, "\n");
	msg.print(Messenger::Verbose, s);
	strcpy(s,"Grid Formats :");
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
		sprintf(bit, " %s[r%c]", imp->filter.nickname(), exp == NULL ? 'o' : 'w');
		strcat(s,bit);
	}
	strcat(s, "\n");
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


/*
	*** Aten Filter-Specific Routines
	*** src/main/aten_filters.cpp
	Copyright T. Youngs 2007-2015

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
#include "gui/mainwindow.h"
#include <QDir>

// Load filters
void Aten::openFilters()
{
	Messenger::enter("Aten::openFilters");

	bool found = false;

	failedFilters_.clear();

	QDir path = dataDirectoryFile("filters");
	Messenger::print(Messenger::Verbose, "Looking for filters in '%s'...", qPrintable(path.path()));
	int nLoaded = parseFilterDir(path);

	// Try to load user filters - we don't mind if the directory doesn't exist...
	path = atenDirectoryFile("filters");
	Messenger::print(Messenger::Verbose, "Looking for user filters in '%s'...", qPrintable(path.path()));
	nLoaded += parseFilterDir(path);

	// Print out info and partner filters if all was successful
	if (found)
	{
		partnerFilters();
		Messenger::print("Found (import/export):  Models (%i/%i) ", filters_[FilterData::ModelImport].nItems(), filters_[FilterData::ModelExport].nItems());
		Messenger::print("Trajectory (%i/%i) ", filters_[FilterData::TrajectoryImport].nItems(), filters_[FilterData::TrajectoryExport].nItems());
		Messenger::print("Expression (%i/%i) ", filters_[FilterData::ExpressionImport].nItems(), filters_[FilterData::ExpressionExport].nItems());
		Messenger::print("Grid (%i/%i)", filters_[FilterData::GridImport].nItems(), filters_[FilterData::GridExport].nItems());
	}

	// Create filter lists
	createFileDialogFilters();

	Messenger::exit("Aten::openFilters");
}

// Load filter from specified filename
bool Aten::openFilter(QString filename)
{
	Messenger::enter("Aten::openFilter");

	// Construct filter Program...
	Program* filter = filterPrograms_.add();
	if (!filter->generateFromFile(filename, filename, true, true, true))
	{
		Messenger::error("Failed to load filters from '%s'...", qPrintable(filename));
		failedFilters_ << filename;
		filterPrograms_.remove(filter);
		Messenger::exit("Aten::openFilter");
		return false;
	}

	Messenger::exit("Aten::openFilter");
	return true;
}

// Create filter strings for file dialogs
void Aten::createFileDialogFilters()
{
	Messenger::enter("Aten::createFileDialogFilters");

	// Standard filter types
	for (int n=0; n<FilterData::nFilterTypes; ++n)
	{
		FilterData::FilterType ft = (FilterData::FilterType) n;
		fileDialogFilters_[ft].clear();
		fileDialogFilters_[ft] += "All files (*)";
		for (Refitem<Tree,int>* ri = filters_[ft].first(); ri != NULL; ri = ri->next)
		{
			fileDialogFilters_[ft] += ";;";
			fileDialogFilters_[ft] += ri->item->filter.description();
		}
	}

	// Bitmap formats
	bitmapFileDialogFilters_.clear();
	for (int n=0; n < AtenWindow::nBitmapFormats; ++n)
	{
		if (!bitmapFileDialogFilters_.isEmpty()) bitmapFileDialogFilters_ += ";;";
		bitmapFileDialogFilters_ += AtenWindow::bitmapFormatFilter( (AtenWindow::BitmapFormat) n);
	}

	Messenger::exit("Aten::createFileDialogFilters");
}

// Register a filter of a given type at start of list
void Aten::registerFilter(Tree* filter, FilterData::FilterType ft)
{
	filters_[ft].addStart(filter);
}

// Reload filters
int Aten::reloadFilters()
{
	Messenger::enter("Aten::reloadFilters");

	Messenger::print("Clearing current filters....");
	filters_[FilterData::ModelImport].clear();
	filters_[FilterData::ModelExport].clear();
	filters_[FilterData::TrajectoryImport].clear();
	filters_[FilterData::TrajectoryExport].clear();
	filters_[FilterData::ExpressionImport].clear();
	filters_[FilterData::ExpressionExport].clear();
	filters_[FilterData::GridImport].clear();
	filters_[FilterData::GridExport].clear();
	filterPrograms_.clear();
	failedFilters_.clear();

	// Load filters
	QDir path = dataDirectoryFile("filters");
	Messenger::print("Reading filters from '%s'...", qPrintable(path.absolutePath()));
	int nLoaded = parseFilterDir(path);

	// Print out info and partner filters 
	partnerFilters();
	Messenger::print("Found (import/export):  Models (%i/%i) ", filters_[FilterData::ModelImport].nItems(), filters_[FilterData::ModelExport].nItems());
	Messenger::print("Trajectory (%i/%i) ", filters_[FilterData::TrajectoryImport].nItems(), filters_[FilterData::TrajectoryExport].nItems());
	Messenger::print("Expression (%i/%i) ", filters_[FilterData::ExpressionImport].nItems(), filters_[FilterData::ExpressionExport].nItems());
	Messenger::print("Grid (%i/%i)", filters_[FilterData::GridImport].nItems(), filters_[FilterData::GridExport].nItems());

	// Create filter lists
	createFileDialogFilters();

	Messenger::exit("Aten::reloadFilters");
	return nLoaded;
}

// Return current number of filter programs
int Aten::nFilterPrograms() const
{
	filterPrograms_.nItems();
}

// Return status of filter load on startup
QStringList Aten::failedFilters() const
{
	return failedFilters_;
}

// Parse directory index and load filters
int Aten::parseFilterDir(QDir path)
{
	Messenger::enter("Aten::parseFilterDir");

	// First check - does this directory actually exist
	if (!path.exists())
	{
		Messenger::warn("Filter directory '%s' does not exist.", qPrintable(path.path()));
		Messenger::exit("Aten::parseFilterDir");
		return 0;
	}

	// Filter the directory contents - show only files and exclude '.' and '..'
	int i, nLoaded = 0;
	QString s = "Filters --> [" + path.path() + "] ";
	QStringList filterList = path.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<filterList.size(); i++)
	{
		// Construct filter Program...
		Program* filter = filterPrograms_.add();
		QString filename = path.filePath(filterList.at(i));
		if (!filter->generateFromFile(qPrintable(QDir::toNativeSeparators(filename)), qPrintable(filterList.at(i)), true, true, true))
		{
			Messenger::error("Failed to load filters from '%s'...", qPrintable(filterList.at(i)));
			failedFilters_ << QDir::toNativeSeparators(filename);
			filterPrograms_.remove(filter);
		}
		else
		{
			s += filterList.at(i) + "  ";
			++nLoaded;
		}
	}
	Messenger::print(s);

	Messenger::exit("Aten::parseFilterDir");
	return nLoaded;
}

// Set filter partners
void Aten::partnerFilters()
{
	Messenger::enter("Aten::partnerFilters");

	// Loop through import filters and search / set export partners
	QString s = "Model Formats:";
	Refitem<Tree,int>* ri, *rj;
	Tree* imp, *exp;
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
					Messenger::print(Messenger::Verbose, "--- Partnering model filters for '%s', id = %i", qPrintable(imp->filter.nickname()), imp->filter.id());
					imp->filter.setPartner(exp);
					break;
				}
			}
		}
		s += QString(" %1[r%2]").arg(imp->filter.nickname(), exp == NULL ? "o" : "w");
	}
	Messenger::print(s);

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
					Messenger::print(Messenger::Verbose, "--- Partnering grid filters for '%s', id = %i", qPrintable(imp->filter.nickname()), imp->filter.id());
					imp->filter.setPartner(exp);
					break;
				}
			}
		}
		s += QString(" %1[r%2]").arg(imp->filter.nickname(), exp == NULL ? "o" : "w");
	}
	Messenger::print(s);

	Messenger::exit("Aten::partnerFilters");
}

// Find filter with specified type and nickname
Tree* Aten::findFilter(FilterData::FilterType ft, QString nickname) const
{
	Messenger::enter("Aten::findFilter");

	Refitem<Tree,int>* result;
	for (result = filters_[ft].first(); result != NULL; result = result->next) if (result->item->filter.nickname() == nickname) break;
	if (result == NULL) Messenger::print("No %s filter with nickname '%s' defined.", FilterData::filterType(ft), qPrintable(nickname));

	Messenger::exit("Aten::findFilter");
	return (result == NULL ? NULL : result->item);
}

// Find filter by description
Tree* Aten::findFilterByDescription(FilterData::FilterType ft, QString description) const
{
	Messenger::enter("Aten::findFilterByDescription");

	Refitem<Tree,int>* result;
	for (result = filters_[ft].first(); result != NULL; result = result->next) if (result->item->filter.description() == description) break;
// 	if (result == NULL) Messenger::print("Internal Error: No %s filter matches description '%s'.", FilterData::filterType(ft), description);

	Messenger::exit("Aten::findFilterByDescription");
	return (result == NULL ? NULL : result->item);
}

// Return first filter refitem in list (of a given type)
Refitem<Tree,int>* Aten::filters(FilterData::FilterType ft) const
{
	return filters_[ft].first();
}

// Return nth filter in list (of a given type)
Refitem<Tree,int>* Aten::filter(FilterData::FilterType ft, int index)
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
Reflist<Tree,int>* Aten::filterList(FilterData::FilterType ft)
{
	return &filters_[ft];
}

// Print list of valid filter nicknames
void Aten::printValidNicknames(FilterData::FilterType ft)
{
	Messenger::print("Valid %s nicknames are:", FilterData::filterType(ft));
	if (filters_[ft].nItems() == 0) Messenger::print("  <None Available>");
	for (Refitem<Tree,int>* ri = filters_[ft].first(); ri != NULL; ri = ri->next)
		Messenger::print("  %-15s %s", qPrintable(ri->item->filter.nickname()), qPrintable(ri->item->filter.name()));
}

// Return filter strings for file dialogs
const QString& Aten::fileDialogFilters(FilterData::FilterType ft) const
{
	return fileDialogFilters_[ft];
}

// Return filter strings for bitmap file dialogs
const QString& Aten::bitmapFileDialogFilters() const
{
	return bitmapFileDialogFilters_;
}

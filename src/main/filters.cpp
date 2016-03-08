/*
	*** Aten Filter-Specific Routines
	*** src/main/filters.cpp
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
	partnerFilters();
	Messenger::print("Found (import/export):  Models (%i/%i) ", filters_[FilterData::ModelImport].nItems(), filters_[FilterData::ModelExport].nItems());
	Messenger::print("Trajectory (%i/%i) ", filters_[FilterData::TrajectoryImport].nItems(), filters_[FilterData::TrajectoryExport].nItems());
	Messenger::print("Expression (%i/%i) ", filters_[FilterData::ExpressionImport].nItems(), filters_[FilterData::ExpressionExport].nItems());
	Messenger::print("Grid (%i/%i)", filters_[FilterData::GridImport].nItems(), filters_[FilterData::GridExport].nItems());

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
	if (!filter->generateFromFile(filename, filename, false, true, true))
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
		for (RefListItem<Tree,int>* ri = filters_[ft].first(); ri != NULL; ri = ri->next)
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

	// Construct a reflist of all current models and frames that have filters currently set, and store the id of the filter...
	RefList<Model,int> modelFilterIDs;
	Model* m, *frame;
	for (m = models_.first(); m != NULL; m = m->next)
	{
		if (m->filter()) modelFilterIDs.add(m, m->filter()->filter.id());
		for (int n=0; n<m->nTrajectoryFrames(); ++n)
		{
			frame = m->trajectoryFrame(n);
			if (frame->filter()) modelFilterIDs.add(frame, frame->filter()->filter.id());
		}
	}

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

	// Re-set all filter pointers...
	for (RefListItem<Model,int>* ri = modelFilterIDs.first(); ri != NULL; ri = ri->next)
	{
		m = ri->item;
		Tree* filter = findFilterByID(FilterData::ModelExport, ri->data);
		m->setFilter(filter);
	}

	Messenger::exit("Aten::reloadFilters");
	return nLoaded;
}

// Return current number of filter programs
int Aten::nFilterPrograms() const
{
	return filterPrograms_.nItems();
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
		QString filename = path.filePath(filterList.at(i));
		if (openFilter(QDir::toNativeSeparators(filename)))
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
	RefListItem<Tree,int>* ri, *rj;
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
Tree* Aten::findFilter(FilterData::FilterType filterType, QString nickname) const
{
	Messenger::enter("Aten::findFilter");

	RefListItem<Tree,int>* result;
	for (result = filters_[filterType].first(); result != NULL; result = result->next) if (result->item->filter.nickname() == nickname) break;
	if (result == NULL) Messenger::print("No %s filter with nickname '%s' defined.", FilterData::filterType(filterType), qPrintable(nickname));

	Messenger::exit("Aten::findFilter");
	return (result == NULL ? NULL : result->item);
}

// Find filter by id
Tree* Aten::findFilterByID(FilterData::FilterType filterType, int id) const
{
	Messenger::enter("Aten::findFilterByDescription");

	RefListItem<Tree,int>* result;
	for (result = filters_[filterType].first(); result != NULL; result = result->next) if (result->item->filter.id() == id) break;

	Messenger::exit("Aten::findFilterByDescription");
	return (result == NULL ? NULL : result->item);
}

// Find filter by description
Tree* Aten::findFilterByDescription(FilterData::FilterType filterType, QString description) const
{
	Messenger::enter("Aten::findFilterByDescription");

	RefListItem<Tree,int>* result;
	for (result = filters_[filterType].first(); result != NULL; result = result->next) if (result->item->filter.description() == description) break;
// 	if (result == NULL) Messenger::print("Internal Error: No %s filter matches description '%s'.", FilterData::filterType(ft), description);

	Messenger::exit("Aten::findFilterByDescription");
	return (result == NULL ? NULL : result->item);
}

// Return first filter refitem in list (of a given type)
RefListItem<Tree,int>* Aten::filters(FilterData::FilterType filterType) const
{
	return filters_[filterType].first();
}

// Return nth filter in list (of a given type)
RefListItem<Tree,int>* Aten::filter(FilterData::FilterType filterType, int index)
{
	if ((index < 0) || (index >= filters_[filterType].nItems()))
	{
		printf("Internal Error : Index %i is out of range for '%s' filter list.\n", index, FilterData::filterType(filterType));
		return NULL;
	}
	return filters_[filterType][index];
}

// Return number of filters of a given type
int Aten::nFilters(FilterData::FilterType filterType) const
{
	return filters_[filterType].nItems();
}

// Return pointer to list of filters of given type
RefList<Tree,int>* Aten::filterList(FilterData::FilterType filterType)
{
	return &filters_[filterType];
}

// Print list of valid filter nicknames
void Aten::printValidNicknames(FilterData::FilterType filterType)
{
	Messenger::print("Valid %s nicknames are:", FilterData::filterType(filterType));
	if (filters_[filterType].nItems() == 0) Messenger::print("  <None Available>");
	for (RefListItem<Tree,int>* ri = filters_[filterType].first(); ri != NULL; ri = ri->next)
		Messenger::print("  %-15s %s", qPrintable(ri->item->filter.nickname()), qPrintable(ri->item->filter.name()));
}

// Return filter strings for file dialogs
const QString& Aten::fileDialogFilters(FilterData::FilterType filterType) const
{
	return fileDialogFilters_[filterType];
}

// Return filter strings for bitmap file dialogs
const QString& Aten::bitmapFileDialogFilters() const
{
	return bitmapFileDialogFilters_;
}

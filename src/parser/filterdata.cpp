/*
	*** Filter Data
	*** src/parser/filterdata.cpp
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

#include "parser/filterdata.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Filter types
const char* FilterTypeKeywords[FilterData::nFilterTypes] = { "importmodel", "importtrajectory", "importexpression", "importgrid", "exportmodel", "exporttrajectory", "exportexpression", "exportgrid" };
const char* FilterData::filterType(FilterData::FilterType ft)
{
        return FilterTypeKeywords[ft];
}
FilterData::FilterType FilterData::filterType(QString s, bool reportError)
{
        FilterData::FilterType ft = (FilterData::FilterType) enumSearch("filter type", FilterData::nFilterTypes, FilterTypeKeywords, s);
	if ((ft == FilterData::nFilterTypes) && reportError) enumPrintValid(FilterData::nFilterTypes,FilterTypeKeywords);
	return ft;
}

// Filter options
const char* FilterOptionKeywords[FilterData::nFilterOptions] =  { "exact", "extension", "glob", "id", "name", "nickname", "search", "type", "within", "zmap" };
FilterData::FilterOption FilterData::filterOption(QString s)
{
	return (FilterData::FilterOption) enumSearch("", FilterData::nFilterOptions, FilterOptionKeywords, s);
}
const char* FilterData::filterOption(FilterData::FilterOption fc)
{
	return FilterOptionKeywords[fc];
}
VTypes::DataType FilterOptionTypes[FilterData::nFilterOptions] =  { VTypes::StringData, VTypes::StringData, VTypes::StringData, VTypes::IntegerData, VTypes::StringData, VTypes::StringData, VTypes::StringData, VTypes::StringData, VTypes::IntegerData, VTypes::StringData };

// Constructor
FilterData::FilterData()
{
	// Private variables
	type_ = FilterData::nFilterTypes;
	hasExtension_ = false;
	hasZmapping_ = false;
	zMapType_ = ElementMap::AlphaZMap;
	name_ = "unnamed filter";
	glob_ = "*";
	nLinesToSearch_ = 10;
	id_ = -1;
	partner_ = NULL;
	headerFunction_ = NULL;
	frameFunction_ = NULL;
}

// Destructor
FilterData::~FilterData()
{
}

// Return the ID of the filter
int FilterData::id() const
{
	return id_;
}

// Return the descriptive name of the filter
QString FilterData::name() const
{
	return name_;
}

// Return the short nickname of the filter
QString FilterData::nickname() const
{
	return nickname_;
}

// Return the first file extension
QStringList FilterData::extensions() const
{
	return extensions_;
}

// Return a comma-separated list of file extensions
QString FilterData::extensionList() const
{
	QString extList;
	for (int n=0; n<extensions_.count(); ++n)
	{
		if (! extList.isEmpty()) extList += ", ";
		extList += extensions_.at(n);
	}
	return extList;
}

// Return the first alias
QStringList FilterData::exactNames() const
{
	return exactNames_;
}

// Return the number of lines to search for defining strings
int FilterData::nLinesToSearch() const
{
	return nLinesToSearch_;
}

// Return the first identifying text string
QStringList FilterData::searchStrings() const
{
	return searchStrings_;
}

// Return whether filter has an extension
bool FilterData::hasExtension() const
{
	return hasExtension_;
}

// Return whether the supplied text matches any of the filter's possible extensions
bool FilterData::doesExtensionMatch(QString ext) const
{
	QString extLower = ext.toLower();
	for (int n=0; n<extensions_.count(); ++n) if (extensions_.at(n).toLower() == extLower) return true;
	return false;
}

// Return whether the supplied text matches any of the filter's possible exact filenames
bool FilterData::doesNameMatch(QString name) const
{
	QString nameLower = name.toLower();
	for (int n=0; n<exactNames_.count(); ++n)  if (exactNames_.at(n).toLower() == nameLower) return true;
	return false;
}

// Set the partner filter
void FilterData::setPartner(Tree* filter)
{
	partner_ = filter;
}

// Return the partner filter
Tree* FilterData::partner() const
{
	return partner_;
}

// Return the file filter
QString FilterData::glob() const
{
	return glob_;
}

// Set the type of filter
void FilterData::setType(FilterType ft)
{
	type_ = ft;
}

// Return the type of filter
FilterData::FilterType FilterData::type() const
{
	return type_;
}

// Return if the filter is an export filter
bool FilterData::isExportFilter() const
{
	return (type_ >= FilterData::ModelExport);
}

// Set filter option
bool FilterData::setOption(QString name, TreeNode* value)
{
	Messenger::enter("FilterData::setOption");
	// Determine filter option supplied
	FilterData::FilterOption fo = FilterData::filterOption(name);
	if (fo == FilterData::nFilterOptions)
	{
		Messenger::print("Error: '%s' is not a valid filter option.", qPrintable(name));
		Messenger::exit("FilterData::setOption");
		return false;
	}
	// Check argument type
	if (FilterOptionTypes[fo] != value->returnType())
	{
		Messenger::print("Error: Filter option '%s' takes %s value.", qPrintable(name), VTypes::dataType(FilterOptionTypes[fo]));
		Messenger::exit("FilterData::setOption");
		return false;
	}
	ReturnValue rv;
	ElementMap::ZMapType zm;
	FilterType ft;
	LineParser parser;
	int n;
	switch (fo)
	{
		case (FilterData::ExactOption):
			if (!value->execute(rv)) printf("Error retrieving 'exact' filter option value.\n");
			parser.getArgsDelim(0, rv.asString());
			for (n = 0; n < parser.nArgs(); ++n) exactNames_ << parser.argc(n);
			break;
		case (FilterData::ExtensionOption):
			if (!value->execute(rv)) printf("Error retrieving 'extension' filter option value.\n");
			parser.getArgsDelim(0, rv.asString());
			for (n = 0; n < parser.nArgs(); ++n) extensions_ << parser.argc(n);
			break;
		case (FilterData::GlobOption):
			if (!value->execute(rv)) printf("Error retrieving 'glob' filter option value.\n");
			glob_ = rv.asString();
			break;
		case (FilterData::IdOption):
			if (!value->execute(rv)) printf("Error retrieving 'id' filter option value.\n");
			id_ = rv.asInteger();
			break;
		case (FilterData::NameOption):
			if (!value->execute(rv)) printf("Error retrieving 'name' filter option value.\n");
			name_ = rv.asString();
			break;
		case (FilterData::NicknameOption):
			if (!value->execute(rv)) printf("Error retrieving 'nickname' filter option value.\n");
			nickname_ = rv.asString();
			break;
		case (FilterData::SearchOption):
			if (!value->execute(rv)) printf("Error retrieving filter option value.\n");
			searchStrings_ << rv.asString();
			break;
		case (FilterData::TypeOption):
			if (!value->execute(rv)) printf("Error retrieving filter type value.\n");
			ft = FilterData::filterType(rv.asString());
			if (ft == FilterData::nFilterTypes)
			{
				Messenger::exit("FilterData::setOption");
				return false;
			}
			type_ = ft;
			break;
		case (FilterData::WithinOption):
			if (!value->execute(rv)) printf("Error retrieving 'within' filter option value.\n");
			nLinesToSearch_ = rv.asInteger();
			break;
		case (FilterData::ZMapOption):
			if (!value->execute(rv)) printf("Error retrieving 'zmap' filter option value.\n");
			zm = ElementMap::zMapType(rv.asString());
			if (zm == ElementMap::nZMapTypes)
			{
				Messenger::exit("FilterData::setOption");
				return false;
			}
			zMapType_ = zm;
			break;
		default:
			printf("Internal Error: Unrecognised filter option.\n");
	}
	Messenger::exit("FilterData::setOption");
	return true;
}

// Return the long description of the filter (including glob)
QString FilterData::description()
{
	// If the description string is empty, create a new one
	if (description_.length() < 3) description_.sprintf("%s (%s)", qPrintable(name_), qPrintable(glob_));
	return description_;
}

// Set trajectory header function
void FilterData::setTrajectoryHeaderFunction(Tree* func)
{
	headerFunction_ = func;
}

// Set trajectory frame function
void FilterData::setTrajectoryFrameFunction(Tree* func)
{
	frameFunction_ = func;
}

// Set trajectory header function
Tree* FilterData::trajectoryHeaderFunction() const
{
	return headerFunction_;
}

// Set trajectory frame function
Tree* FilterData::trajectoryFrameFunction() const
{
	return frameFunction_;
}

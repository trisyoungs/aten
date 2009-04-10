/*
	*** Filter Data
	*** src/parser/filterdata.cpp
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

#include "parser/filterdata.h"
#include "base/sysfunc.h"

// Filter types
const char *FilterTypeKeywords[FilterData::nFilterTypes] = { "importmodel", "importtrajectory", "importfield", "importgrid", "exportmodel", "exporttrajectory", "exportfield", "exportgrid" };
const char *FilterData::filterType(FilterData::FilterType ft)
{
        return FilterTypeKeywords[ft];
}
FilterData::FilterType FilterData::filterType(const char *s, bool quiet)
{
        return (FilterData::FilterType) enumSearch( quiet ? "filter type" : "", FilterData::nFilterTypes, FilterTypeKeywords, s);
}

// Filter options
const char* FilterOptionKeywords[FilterData::nFilterOptions] =  { "exact", "extension", "glob", "id", "name", "nickname", "search", "type", "within", "zmap" };
FilterData::FilterOption FilterData::filterOption(const char* s)
{
	return (FilterData::FilterOption) enumSearch("", FilterData::nFilterOptions, FilterOptionKeywords, s);
}
const char *FilterData::filterOption(FilterData::FilterOption fc)
{
	return FilterOptionKeywords[fc];
}
NuVTypes::DataType FilterOptionTypes[FilterData::nFilterOptions] =  { NuVTypes::StringData, NuVTypes::StringData, NuVTypes::StringData, NuVTypes::IntegerData, NuVTypes::StringData, NuVTypes::StringData, NuVTypes::StringData, NuVTypes::StringData, NuVTypes::IntegerData, NuVTypes::StringData };

// Constructor
FilterData::FilterData()
{
	// Private variables
	type_ = FilterData::nFilterTypes;
	hasExtension_ = FALSE;
	hasZmapping_ = FALSE;
	zMapType_ = ElementMap::AlphaZMap;
	name_.set("unnamed filter");
	glob_.set("*");
	nLinesToSearch_ = 10;
	id_ = -1;
	partner_ = NULL;
}

// Destructor
FilterData::~FilterData()
{
}

// Return the ID of the filter
int FilterData::id()
{
	return id_;
}

// Return the descriptive name of the filter
const char *FilterData::name()
{
	return name_.get();
}

// Return the short nickname of the filter
const char *FilterData::nickname()
{
	return nickname_.get();
}

// Return the first file extension
Dnchar *FilterData::extensions()
{
	return extensions_.first();
}

// Return the first alias
Dnchar *FilterData::exactNames()
{
	return exactNames_.first();
}

// Return the number of lines to search for defining strings
int FilterData::nLinesToSearch()
{
	return nLinesToSearch_;
}

// Return the first identifying text string
Dnchar *FilterData::searchStrings()
{
	return searchStrings_.first();
}

// Return whether filter has an extension
bool FilterData::hasExtension()
{
	return hasExtension_;
}

// Set the partner filter
void FilterData::setPartner(Tree *filter)
{
	partner_ = filter;
}

// Return the partner filter
Tree *FilterData::partner()
{
	return partner_;
}

// Return the file filter
const char *FilterData::glob()
{
	return glob_.get();
}

// Set the type of filter
void FilterData::setType(FilterType ft)
{
	type_ = ft;
}

// Return the type of filter
FilterData::FilterType FilterData::type()
{
	return type_;
}

// Set filter option
bool FilterData::setOption(Dnchar *name, TreeNode *value)
{
	msg.enter("FilterData::setOption");
	// Determine filter option supplied
	FilterData::FilterOption fo = FilterData::filterOption(name->get());
	if (fo == FilterData::nFilterOptions)
	{
		msg.print("Error: '%s' is not a valid filter option.\n", name->get());
		msg.exit("FilterData::setOption");
		return FALSE;
	}
	// Check argument type
	if (FilterOptionTypes[fo] != value->returnType())
	{
		msg.print("Error: Filter option '%s' takes %s value.\n", name->get(), NuVTypes::dataType(FilterOptionTypes[fo]));
		msg.exit("FilterData::setOption");
		return FALSE;
	}
	Dnchar *d;
	NuReturnValue rv;
	ElementMap::ZMapType zm;
	FilterType ft;
	LineParser parser;
	int n;
	switch (fo)
	{
		case (FilterData::ExactOption):
			if (!value->execute(rv)) printf("Error retrieving 'exact' filter option value.\n");
			parser.getArgsDelim(rv.asString());
			for (n = 0; n < parser.nArgs(); ++n) exactNames_.add()->set(parser.argc(n));
// 			d = exactNames_.add();
// 			d->set(rv.asString());
			break;
		case (FilterData::ExtensionOption):
			if (!value->execute(rv)) printf("Error retrieving 'extension' filter option value.\n");
			parser.getArgsDelim(rv.asString());
			for (n = 0; n < parser.nArgs(); ++n) extensions_.add()->set(parser.argc(n));
// 			d = extensions_.add();
// 			d->set(rv.asString());
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
			d = searchStrings_.add();
			if (!value->execute(rv)) printf("Error retrieving filter option value.\n");
			d->set(rv.asString());
			break;
		case (FilterData::TypeOption):
			if (!value->execute(rv)) printf("Error retrieving filter type value.\n");
			ft = FilterData::filterType(rv.asString());
			if (ft == FilterData::nFilterTypes)
			{
				msg.exit("FilterData::setOption");
				return TRUE;
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
				msg.exit("FilterData::setOption");
				return TRUE;
			}
			zMapType_ = zm;
			break;
		default:
			printf("Internal Error: Unrecognised filter option.\n");
	}
	msg.exit("FilterData::setOption");
	return TRUE;
}

// Return the long description of the filter (including glob)
const char *FilterData::description()
{
	// If the description string is empty, create a new one
	if (description_.length() < 3)
	{
		int size = name_.length() + glob_.length() + 10;
		char *longname = new char[size];
		// Generate the filter desciption string
		sprintf(longname,"%s (%s)",name_.get(),glob_.get());
		description_ = longname;
		delete[] longname;
	}
	return description_.get();
}

/*
	*** Filter commands
	*** src/parse/filtercommands.cpp
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

#include "parse/filtercommands.h"
#include "base/sysfunc.h"

// Filter commands
const char* FC_data[FC_NITEMS] =  { "name", "nickname", "extension", "glob", "exact", "zmap", "id" };
filter_command FC_from_text(const char* s)
	{ return (filter_command) enum_search_data("filter command", FC_NITEMS, FC_data, s); }
const char *text_from_FC(filter_command fc)
	{ return get_before_comma(FC_data[fc]); }

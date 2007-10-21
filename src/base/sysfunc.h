/*
	*** System functions
	*** src/base/sysfunc.h
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

#ifndef H_SYSFUNC_H
#define H_SYSFUNC_H

// Forward declarations
class variable_list;

// Number/string conversion
const char *itoa(int);
const char *ftoa(double);
const char *ftoa(double, const char*);

// String functions
const char *remove_path(const char*);
const char *upper_case(const char*);
const char *lower_case(const char*);
const char *get_before_comma(const char*);
const char *get_after_comma(const char*);

// Enum search
int enum_search(const char*, int, const char**, const char*);
int enum_search_data(const char*, int, const char**, const char*);
int sinlist(const char*, const char*);

// Expression evaluate
const char *evaluate(const char*, variable_list*);

#endif

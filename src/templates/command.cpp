/*
	*** Command list functions
	*** src/templates/command.cpp

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

#include "templates/command.h"
#include "base/sysfunc.h"

const char *BC_keywords[BC_NITEMS] = {
	"_ROOTNODE_",

	"repeat,G|g",
	"foratoms,A|Pg",
	"forbonds",
	"forpatterns,P",
	"formolecules",
	"forffbonds,BP",
	"forffangles,BP",
	"forfftorsions,BP",

	"if,VX?",
	"elseif,VX?",
	"else",
	"end",

	"let,V=?",
	"inc,V",
	"dec,V",

	"_GOTO_",
	"_GOTONONIF_",
	"_TERMINATE_",
	"_OTHER_"
	};
basic_command BC_from_text(const char* s)
	{ return (basic_command) enum_search_data("NULL",BC_NITEMS,BC_keywords,s); }
const char *text_from_BC(basic_command bc)
	{ return get_before_comma(BC_keywords[bc]); }
const char *vars_from_BC(basic_command bc)
	{ return get_after_comma(BC_keywords[bc]); }

// If Conditions
const char *IC_strings[6] = { "eq", "l", "le", "g", "ge", "neq" };
const char *text_from_IC(if_condition i)
	{ return IC_strings[i-1]; }

/*
	*** Filter commands
	*** src/file/filtercommands.cpp
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

#include "file/filtercommands.h"
#include "base/sysfunc.h"

// Filter commands
const char* FC_data[FC_NITEMS] =  {
	"name,G",
	"nickname,G",
	"extension,G",
	"glob,G",
	"exact,G",
	"zmap,G",
	"id,G",

	"newmodel,G",
	"newsurface,G",
	"finalisemodel",
	"finalisegrid",

	"rebond",
	"pack",
	"centre",
	"fold",
	"fractoreal",

	"readline,F",
	"readvar,VF",
	"readnext,V",
	"writeline,F",
	"writevar,VF",
	"skipline,|g",

	"readint,V",
	"readdouble,G",
	"readchars,Vg",
	"skipchars",

	"find,gV|V",

	"addatom",
	"addbond,GG|G",
	"createatoms,G",
	"modeltemplate",
	"setatom,G",
	"setcell",
	"setcellaxes",
	"setspacegroup,G",
	"setrx,GG",
	"setry,GG",
	"setrz,GG",
	"setfx,GG",
	"setfy,GG",
	"setfz,GG",
	"setvx,GG",
	"setvy,GG",
	"setvz,GG",

	"addpoint,GGGG",
	"addnextpoint,G",
	"setgrid,GGGGGGGGG",
	"setgridcubic,G",
	"setgridorigin,GGG",
	"setgridortho,GGG",
	"setgridsize,GGG",

	"rewind",
	"storepos",
	"seekpos",

	"warn,G",
	"error,G"
	};
filter_command FC_from_text(const char* s)
	{ return (filter_command) enum_search_data("filter command", FC_NITEMS, FC_data, s); }
const char *text_from_FC(filter_command fc)
	{ return get_before_comma(FC_data[fc]); }
const char *vars_from_FC(filter_command fc)
	{ return get_after_comma(FC_data[fc]); }

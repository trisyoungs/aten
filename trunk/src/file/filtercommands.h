/*
	*** Filter commands
	*** src/file/filtercommands.h
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

#ifndef H_FILTERCMDS_H
#define H_FILTERCMDS_H

// Filter commands
enum filter_command {
	FC_NAME,
	FC_NICKNAME,
	FC_EXTENSION,
	FC_GLOB,
	FC_EXACT,
	FC_ZMAP,
	FC_PARTNER,
	
	FC_NEWMODEL,
	FC_NEWSURFACE,
	FC_FINALISEMODEL,
	FC_FINALISESURFACE,

	FC_REBOND,
	FC_PACK,
	FC_CENTRE,
	FC_FOLD,
	FC_FRACTOREAL,
	
	FC_READLINE,
	FC_READVAR,
	FC_READNEXT,
	FC_WRITELINE,
	FC_WRITEVAR,
	FC_SKIPLINE,

	FC_READINTEGER,
	FC_READDOUBLE,
	FC_READCHARS,
	FC_SKIPCHARS,

	FC_ADDATOM,
	FC_ADDBOND,
	FC_CREATEATOMS,
	FC_MODELTEMPLATE,
	FC_SETATOM,
	FC_SETCELL,
	FC_SETCELLA,
	FC_SETSPACEGROUP,
	FC_SETRX,
	FC_SETRY,
	FC_SETRZ,
	FC_SETFX,
	FC_SETFY,
	FC_SETFZ,
	FC_SETVX,
	FC_SETVY,
	FC_SETVZ,

	FC_ADDGRIDPOINT,
	FC_ADDNEXTGRIDPOINT,
	FC_SETGRID,
	FC_SETGRIDCUBIC,
	FC_SETGRIDORIGIN,
	FC_SETGRIDORTHO,
	FC_SETGRIDSIZE,

	FC_REWIND,
	FC_STOREPOS,
	FC_SEEKPOS,
	
	FC_WARN,
	FC_ERROR,
	FC_NITEMS };
filter_command FC_from_text(const char*);
const char *text_from_FC(filter_command);
const char *vars_from_FC(filter_command);

#endif

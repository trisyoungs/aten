/*
	*** Debug / messaging routines
	*** src/base/debug.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_DEBUG_H
#define ATEN_DEBUG_H

#include "../config.h"

// Debug preparation / output
void prepare_debug();
void print_debuginfo();

/*
// Messaging
*/

// Debug messaging modes
enum debug_mode { DM_NONE=0, DM_CALLS=1, DM_MORECALLS=2, DM_TYPING=4, DM_PARSE=8, DM_VERBOSE=16, DM_FILTERS=32 };
void add_debuglevel(debug_mode);	// Add a debug level to the debug output bitvector
void remove_debuglevel(debug_mode);	// Remove a debug level from the debug output bitvector
bool debuglevel_active(debug_mode);	// Returns whether the specified debug level is set

// Messaging Functions
void msg(debug_mode, const char* ...);
void dbg_begin(debug_mode, const char* ...);
void dbg_end(debug_mode, const char* ...);

/*
// Dynamic Memory debugging ('MEMDEBUG')
// Maintains count of creation / destruction calls to custom types
*/

#ifdef MEMDEBUG

enum memdebug_types { MD_ATOM, MD_ATOMADDRESS, MD_ATOMLOCALE, MD_ATOMPAIR, MD_ATOMTYPE,
	MD_BOND,
	MD_CLIPATOM, MD_CLIPBOARD, MD_CLIPBOND, MD_COMMANDNODE, MD_COMPONENT, MD_CONFIG, MD_CONSTRAINT,
	MD_DNCHAR, 
	MD_ELEMENT, MD_ENERGYSTORE,
	MD_FF_ATOM, MD_FF_BOUND, MD_FF_PARAMS, MD_FILTER, MD_FORCEFIELD, MD_FORMAT, MD_FORMAT_NODE, MD_FOURIER,
	MD_GLYPH,
	MD_IFTEST,
	MD_LINKBOND, MD_LIST, MD_LISTITEM,
	MD_MEASUREMENT, MD_MODEL,
	MD_PATTERN, MD_PATTERN_ATOM, MD_PATTERN_BOUND, MD_PIXMAPCANVAS,
	MD_REFLIST, MD_REFLISTITEM, MD_REGION, MD_RESTRAINTS, MD_RESTRAINT_IJ, MD_RING, MD_RINGATOM, MD_RINGTYPE,
	MD_SCRIPT, MD_SITE, MD_SPACEGROUP, MD_SUBSELECTION, MD_SYMMOP,
	MD_TREEVIEW, 
	MD_UNITCELL,
	MD_VARIABLE, MD_VARIABLELIST,
	MD_WIDGETCANVAS,
	MD_MAT3, MD_MAT3COPY, MD_MAT4, MD_MAT4COPY,
	MD_VEC2, MD_VEC2COPY, MD_VEC3, MD_VEC3COPY, MD_VEC4, MD_VEC4COPY,
	MD_NITEMS };

class memdbg_data
{
	public:
	int create[MD_NITEMS];
	int destroy[MD_NITEMS];
};

extern memdbg_data memdbg;

#endif

/*
// Rendering speed test
*/

#ifdef SPEEDTEST
	#include <time.h>
	extern int speedtest_numrenders, speedtest_totalrenders;
	extern clock_t speedtest_start, speedtest_finish;
#endif

#endif

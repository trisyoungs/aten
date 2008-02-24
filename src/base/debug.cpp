/*
	*** Debug / messaging routines
	*** src/base/debug.cpp
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

#include "base/master.h"
#include "gui/gui.h"
#include <stdarg.h>

/*
// Messaging
*/

// Bitvector of debug levels
int debug_output = 0;
// Formatting indent for call debugging output
int funclevel = 0;
// Add a debug level to the debug output bitvector
void add_debuglevel(debug_mode dm) { if (!(debug_output&dm)) debug_output += dm; }
// Remove a debug level from the debug output bitvector
void remove_debuglevel(debug_mode dm) { if (debug_output&dm) debug_output -= dm; }
// Returns whether the specified debug level is set
bool debuglevel_active(debug_mode dm) { return ((debug_output&dm) ? TRUE : FALSE); }

// Standard message
void msg(debug_mode dm, const char *fmt ...)
{
	// Print to the text view in the main window if it has been initialised.
	// Otherwise, print to stdout. Also print to stdout if debuglevel >= msglevel.
	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	// We always print messages with mode DM_NONE to stdout *or* the GUI (if it has been initialised)
	// For other message levels, only print if it's debug level is active
	if (dm == DM_NONE)
	{
		if (gui.exists()) gui.print_message(msgs);
		else printf("%s",msgs);
	}
	else if (debuglevel_active(dm))
	{
		//if (master.GUI) mainwin_textview_append(msgs);
		//else printf("%s",msgs);
		printf("%s",msgs);
	}
	va_end(arguments);
}

// Function enter
void dbg_begin(debug_mode dm, const char *func)
{
	// Debug Messaging - Enter Function
	if (!debuglevel_active(dm)) return;
	printf("%2i ",funclevel);
	for (int n=0; n<funclevel; n++) printf("--");
	printf("Begin : %s...\n",func);
	funclevel ++;
}

// Function leave
void dbg_end(debug_mode dm, const char *func)
{
	// Debug Messaging - Leave Function
	if (!debuglevel_active(dm)) return;
	funclevel --;
	printf("%2i ",funclevel);
	for (int n=0; n<funclevel; n++) printf("--");
	printf("End   : %s.\n",func);
}

/*
// Dynamic Memory debugging ('MEMDEBUG')
// Maintains count of creation / destruction calls to custom types
*/

#ifdef MEMDEBUG

#include "base/debug.h"

const char *MD_keywords[MD_NITEMS] = { "atom", "atomaddress", "atomlocale", "atompair", "atomtype",
	"bond",
	"clipatom", "clipboard", "clipbond", "command_node", "component", "config", "constraint",
	"dnchar",
	"element", "energystore",
	"ffatom", "ffbound", "ffparams", "filter", "forcefield", "format", "format_node", "fourier_data",
	"glyph",
	"iftest",
	"linkbond", "list", "listitem",
	"measurement", "model",
	"pattern", "pattern_atom", "pattern_bound", "pixmapcanvas",
	"reflist", "reflistitem", "region", "restraints", "restraint_ij", "ring", "ringatom", "ringtype",
	"script", "site", "spacegroup", "subselection", "symmop",
	"treeview",
	"unitcell",
	"variable", "variable_list",
	"widgetcanvas",
	"mat3", "mat4copy", "mat4", "mat4copy",
	"vec2", "vec2copy", "vec3", "vec3copy", "vec4", "vec4copy" };

memdbg_data memdbg;

void print_memdebuginfo()
{
	printf("Class constructor / destructor counts are:\n");
	printf("                     Class        Con         Des         +/-\n");
	int n;
	for (n=0; n<MD_MAT3; n++)
	{
		int diff = memdbg.create[n]-memdbg.destroy[n];
		printf("(%2i) %20s  %10i  %10i  %10i",n,MD_keywords[n],
			memdbg.create[n],memdbg.destroy[n],diff);
		diff == 0 ? printf("\n") : printf("  <--\n");
	}
	printf("\nTemplate constructor / destructor counts are:\n");
	printf("                     Class        Con         Copy        Des         +/-\n");
	for (n=MD_MAT3; n<MD_NITEMS; n+=2)
	{
		int diff = memdbg.create[n] + memdbg.create[n+1] - memdbg.destroy[n];
		printf("(%2i) %20s  %10i  %10i  %10i  %10i",n,MD_keywords[n],
			memdbg.create[n],memdbg.create[n+1],memdbg.destroy[n],diff);
		diff == 0 ? printf("\n") : printf("  <--\n");
	}
}

#endif

/*
// Rendering Speedtest
*/

#ifdef SPEEDTEST
	int speedtest_numrenders = 0, speedtest_totalrenders = 100;
	clock_t speedtest_start, speedtest_finish;
#endif

/*
// Debug prep / output
*/

// Prepare
void prepare_debug()
{
	#ifdef MEMDEBUG
		for (int i=0; i<MD_NITEMS; i++) memdbg.create[i] = 0;
		for (int i=0; i<MD_NITEMS; i++) memdbg.destroy[i] = 0;
	#endif
	#ifdef SPEEDTEST
		speedtest_start = clock();
	#endif
}

// Output
void print_debuginfo()
{
	#ifdef MEMDEBUG
		print_memdebuginfo();
	#endif
	#ifdef SPEEDTEST
		speedtest_finish = clock();
		double nsec = double(speedtest_finish-speedtest_start) / CLOCKS_PER_SEC;
		printf("SPEEDTEST : Performed %i renders over %8.2f seconds (%8.2f/sec).\n",speedtest_totalrenders,nsec,speedtest_totalrenders/nsec);
	#endif
}


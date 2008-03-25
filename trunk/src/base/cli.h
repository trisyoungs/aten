/*
	*** Command-line option parsing
	*** src/base/cli.h
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

#ifndef ATEN_CLI_H
#define ATEN_CLI_H

#include <iostream>
#include <getopt.h>

// Command option data
class Cli
{
	public:
	// Command line switches
	enum CliSwitch { BohrSwitch, BondSwitch, CacheSwitch, CentreSwitch, CommandSwitch, ForcefieldSwitch, FoldSwitch, FormatSwitch, GridSwitch, HelpSwitch, InteractiveSwitch, MapSwitch, NoBondSwitch, NoCentreSwitch, NoFoldSwitch, NoPackSwitch, PackSwitch, ScriptSwitch, UndoLevelSwitch, ZmapSwitch, DebugSwitch, DebugAllSwitch, DebugFileSwitch, DebugMoreSwitch, DebugParseSwitch, DebugTypingSwitch, VerboseSwitch, nSwitchItems };
	/*
	// Description of command line option
	*/
	public:
	// Identifier
	CliSwitch option;
	// Short option character
	char shortOpt;
	// Long option keyword
	const char *longOpt;
	// Argument type
	int argument;
	// Argument text (for description)
	const char *argText;
	// Description of option
	const char *description;
};

#endif

/*
	*** Command-line option parsing
	*** src/main/cli.h
	Copyright T. Youngs 2007-2010

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

#ifndef ATENCALC_CLI_H
#define ATENCALC_CLI_H

// Command option data
class Cli
{
	public:
	// Command line switches
	enum CliSwitch { DummySwitch, HelpSwitch, nSwitchItems };

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

	/*
	// Member functions
	*/
	public:
	// Parse CLI options, after filters / prefs have been loaded
	static int parse(int argc, char *argv[]);
	// Search for short option
	static CliSwitch cliSwitch(char c);
	// Search for long option
	static CliSwitch cliSwitch(const char *s);
};

#endif

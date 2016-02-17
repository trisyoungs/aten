/*
	*** Command-line option parsing
	*** src/main/cli.h
	Copyright T. Youngs 2007-2016

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

#include <QString>
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Command option data
class Cli
{
	public:
	// Command line switches
	enum CliSwitch { AtenDataSwitch, BatchSwitch, BohrSwitch, BondSwitch, CacheSwitch, CentreSwitch, CommandSwitch, DebugSwitch, DialogsSwitch, DoubleSwitch, ExportSwitch, ExportMapSwitch, ExpressionSwitch, FilterSwitch, ForcefieldSwitch, FoldSwitch, FormatSwitch, GridSwitch, HelpSwitch, IntSwitch, InteractiveSwitch, KeepNamesSwitch, KeepTypesSwitch, KeepViewSwitch, ListsSwitch, LoadFromListSwitch, MapSwitch, NewModelSwitch, NicknamesSwitch, NoBondSwitch, NoCentreSwitch, NoFiltersSwitch, NoFoldSwitch, NoFragmentsSwitch, NoFragmentIconsSwitch, NoIncludesSwitch, NoInstancesSwitch, NoPackSwitch, NoPartitionsSwitch, NoQtSettingsSwitch, PackSwitch, PipeSwitch, ProcessSwitch, QuietSwitch, ScriptSwitch, StringSwitch, TrajectorySwitch, TrajectoryFormatSwitch, UndoLevelSwitch, VerboseSwitch, VersionSwitch, ZmapSwitch, nSwitchItems };


	/*
	 * Description of command line option
	 */
	public:
	// Identifier
	CliSwitch option;
	// Short option character
	char shortOpt;
	// Long option keyword
	const char* longOpt;
	// Argument type
	int argument;
	// Argument text (for description)
	const char* argText;
	// Description of option
	const char* description;


	/*
	 * Member functions
	 */
	public:
	// Search for short option
	static Cli::CliSwitch cliSwitch(char c);
	// Search for long option
	static Cli::CliSwitch cliSwitch(QString s);
};

ATEN_END_NAMESPACE

#endif

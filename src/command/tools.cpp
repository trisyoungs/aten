/*
	*** Tool Commands
	*** src/command/tool.cpp
	Copyright T. Youngs 2007-2018

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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

// Run specified tool, with options if provided
bool Commands::function_RunTool(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Parse the first option so we can get the filter nickname and any filter options
	LineParser parser;
	parser.getArgsDelim(Parser::UseQuotes, c->argc(0));

	// First part of argument zero is tool nickname (followed by options)
	ToolPluginInterface* plugin = aten_.pluginStore().findToolPluginByNickname(PluginTypes::GeneralToolPlugin, parser.argc(0));
	
	// Check that a suitable plugin was found
	if (plugin == NULL)
	{
		// Print list of valid plugin nicknames
		aten_.pluginStore().showToolPluginNicknames(PluginTypes::GeneralToolPlugin);
		Messenger::print("Nothing was run.");
		rv.set(false);
		return false;
	}

	// Loop over remaining arguments which are option assignments
	KVMap pluginOptions;
	for (int n = 1; n < parser.nArgs(); ++n) pluginOptions.add(parser.argc(n));

	bool result = aten_.runTool(plugin, pluginOptions, false);
	
	rv.set(result);

	return result;
}


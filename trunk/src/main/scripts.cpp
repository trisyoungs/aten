/*
	*** Aten script functions
	*** src/main/scripts.cpp
	Copyright T. Youngs 2007-2015

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

#include "main/aten.h"
// #include "main/version.h"
// #include "gui/mainwindow.h"
// #include "gui/grids.h"
// #include "gui/modellist.h"
// #include "model/model.h"
// #include "model/clipboard.h"
// #include "ff/forcefield.h"
// #include "base/grid.h"
// #include "base/pattern.h"
// #include "base/sysfunc.h"
// #include "parser/aten.h"

ATEN_USING_NAMESPACE

// Add script
Program* Aten::addScript()
{
	return scripts_.add();
}

// Remove specified script
void Aten::removeScript(Program* script)
{
	scripts_.remove(script);
}

// Return number of loaded scripts
int Aten::nScripts()
{
	return scripts_.nItems();
}

// Return first script in list
Program* Aten::scripts()
{
	return scripts_.first();
}

// Return n'th script in list
Program* Aten::script(int n)
{
	if ((n < 0) || (n >= scripts_.nItems())) Messenger::print("Script %i is out of range.\n", n);
	else return scripts_[n];
	return NULL;
}

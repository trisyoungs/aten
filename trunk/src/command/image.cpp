/*
	*** Image command functions
	*** src/command/image.cpp
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

#include "command/commandlist.h"
#include "base/debug.h"
#include "gui/gui.h"

// Save current view as bitmap image
int CommandData::function_CA_SAVEBITMAP(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	return CR_FAIL;
}

// Save current view a vector graphic
int CommandData::function_CA_SAVEVECTOR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	vector_format vf = VIF_from_text(c->argc(0));
	if (vf == VIF_NITEMS)
	{
		msg(Debug::None,"Unrecognised vector format '%s'.\n",c->argc(0));
		return CR_FAIL;
	}
	// If gui exists, use the main canvas. Otherwise, use the offscreen canvas
	if (gui.exists()) gui.mainView.saveVector(obj.m, vf, c->argc(1));
	else
	{
		gui.offscreenCanvas.saveVector(obj.m, vf, c->argc(1));
	}
	return CR_SUCCESS;
}

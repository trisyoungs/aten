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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/tcanvas.uih"
#include "model/model.h"
#include "classes/prefs.h"

// Save current view as bitmap image
int Command::function_CA_SAVEBITMAP(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;

	// Convert format to bitmap_format
	GuiQt::BitmapFormat bf = GuiQt::bitmapFormat(c->argc(0));
	if (bf == GuiQt::nBitmapFormats)
	{
		msg.print("Unrecognised bitmap format.\n");
		return Command::Fail;
	}

	int width = 0, height = 0, quality = -1;
	if (c->hasArg(3))
	{
		width = c->argi(2);
		height = c->argi(3);
	}
	if (c->hasArg(4)) quality = c->argi(4);

	return (gui.saveImage(c->argc(1), bf, width, height, quality) ? Command::Success : Command::Fail);
}

// Save current view a vector graphic
int Command::function_CA_SAVEVECTOR(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	vector_format vf = VIF_from_text(c->argc(0));
	if (vf == VIF_NITEMS)
	{
		msg.print("Unrecognised vector format '%s'.\n",c->argc(0));
		return Command::Fail;
	}
	// If gui exists, use the main canvas. Otherwise, use the offscreen canvas
	gui.mainView.saveVector(obj.rs, vf, c->argc(1));
	return Command::Success;
}

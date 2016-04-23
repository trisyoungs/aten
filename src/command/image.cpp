/*
	*** Image Commands
	*** src/command/image.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "base/prefs.h"
#include "base/sysfunc.h"
#include <QtGui>
#include <QFileInfo>
#include "main/aten.h"

ATEN_USING_NAMESPACE

// Save current view as bitmap image
bool Commands::function_SaveBitmap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Convert format to bitmap_format
	AtenWindow::BitmapFormat bf = AtenWindow::bitmapFormat(c->argc(0));
	if (bf == AtenWindow::nBitmapFormats)
	{
		Messenger::print("Unrecognised bitmap format.");
		return false;
	}

	int width = 0, height = 0, quality = -1;
	if (c->hasArg(3))
	{
		width = c->argi(2);
		height = c->argi(3);
	}
	if (c->hasArg(4)) quality = c->argi(4);

	// Set some options, remembering current values
	bool viewGlobe = prefs.viewRotationGlobe();
	prefs.setViewRotationGlobe(false);

	rv.reset();
	bool result;

	QPixmap pixmap = aten_.atenWindow()->scenePixmap(width, height, false);
	result = pixmap.save(c->argc(1), AtenWindow::bitmapFormatExtension(bf), quality);

	prefs.setViewRotationGlobe(viewGlobe);
	return result;
}


/*
	*** View Commands
	*** src/command/view.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include <QtCore/QTime>

ATEN_USING_NAMESPACE

// Rotate view about an arbitrary axis
bool Commands::function_AxisRotateView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->axisRotateView(c->arg3d(0), c->argd(3));
	rv.reset();
	return TRUE;
}

// Get current view
bool Commands::function_GetView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Matrix rmat = obj.rs()->modelViewMatrix();
	Messenger::print( "View [R c] = { %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f, %8.4f }", rmat[0], rmat[1], rmat[2], rmat[4], rmat[5], rmat[6], rmat[8], rmat[9], rmat[10], rmat[12], rmat[13], rmat[14]);
	rv.reset();
	return TRUE;
}

// Set orthographic view
bool Commands::function_Orthographic(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	prefs.setPerspective(FALSE);
	rv.reset();
	return TRUE;
}

// Set perspective view
bool Commands::function_Perspective(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	prefs.setPerspective(TRUE);
	if (c->hasArg(0)) prefs.setPerspectiveFov(c->argd(0));
	rv.reset();
	return TRUE;
}

// Reset view
bool Commands::function_ResetView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	obj.rs()->resetView(aten_.atenWindow()->ui.MainView->contextWidth(), aten_.atenWindow()->ui.MainView->contextHeight());
	rv.reset();
	return TRUE;
}

// Rotate view
bool Commands::function_RotateView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->rotateView(c->argd(1), c->argd(0));
	rv.reset();
	return TRUE;
}

// Set current view
bool Commands::function_SetView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Matrix rmat;
	// Get rotation matrix
	rmat.setColumn(0,c->arg3d(0),0.0);
	rmat.setColumn(1,c->arg3d(3),0.0);
	rmat.setColumn(2,c->arg3d(6),0.0);
	rmat.setColumn(3,c->arg3d(9),1.0);
	obj.rs()->setModelViewMatrix(rmat);
	rv.reset();
	return TRUE;
}

// Render speed test
bool Commands::function_SpeedTest(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	QTime total, split;
	int msec;
	total.start();
	split.start();
	// Loop over n renders (or 1000 if no variable given)
	int nrenders = (c->hasArg(0) ? c->argi(0) : 1000);
	for (int n=0; n < nrenders; n ++)
	{
		// (Re)start split timer
		if ((n>0) && (n%100 == 0))
		{
			msec = split.restart();
			Messenger::print("100 render average = %6.1f fps", 1000.0 / msec);
		}
		obj.rs()->rotateView(5.0,0.0);
	}
	msec = total.elapsed();
	Messenger::print("Performed %i renders over %8.2f seconds (%8.2f/sec).", nrenders, msec/1000.0, nrenders/(msec/1000.0));
	rv.reset();
	return TRUE;
}

// Translate view
bool Commands::function_TranslateView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->adjustCamera(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return TRUE;
}

// View along specified axis
bool Commands::function_ViewAlong(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Set model rotation matrix to be along the specified axis
	obj.rs()->viewAlong(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return TRUE;
}

// View along specified cell axis
bool Commands::function_ViewAlongCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Set model rotation matrix to be along the specified axis
	obj.rs()->viewAlongCell(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return TRUE;
}

// Zoom view
bool Commands::function_ZoomView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->adjustCamera(0.0,0.0,c->argd(0));
	rv.reset();
	return TRUE;
}

// ZRotate view
bool Commands::function_ZRotateView(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->zRotateView(-c->argd(0));
	rv.reset();
	return TRUE;
}

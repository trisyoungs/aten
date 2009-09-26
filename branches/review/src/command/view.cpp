/*
	*** View Commands
	*** src/command/view.cpp
	Copyright T. Youngs 2007-2009

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
#include "base/messenger.h"
#include "gui/gui.h"
#include "model/model.h"
#include <ctime>


// Rotate view about an arbitrary axis
bool Command::function_AxisRotateView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->axisRotateView(c->arg3d(0), c->argd(3));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Get current view
bool Command::function_GetView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
// 	Mat4<double> rmat = obj.rs->rotationMatrix();
// 	Vec3<double> camr = obj.rs->camera();
// 	double camrot = obj.rs->cameraRotation();   TGAY
// 	msg.print( "View [R c z] = %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", rmat.rows[0].x, rmat.rows[0].y, rmat.rows[0].z, rmat.rows[1].x, rmat.rows[1].y, rmat.rows[1].z, rmat.rows[2].x, rmat.rows[2].y, rmat.rows[2].z, camr.x, camr.y, camr.z, camrot * DEGRAD);
	rv.reset();
	return TRUE;
}

// Set orthographic view
bool Command::function_Orthographic(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setPerspective(FALSE);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set perspective view
bool Command::function_Perspective(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setPerspective(TRUE);
	if (c->hasArg(0)) prefs.setPerspectiveFov(c->argd(0));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Reset view
bool Command::function_ResetView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	obj.rs->resetView();
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Rotate view
bool Command::function_RotateView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->rotateView(c->argd(1), c->argd(0));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set current view
bool Command::function_SetView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Mat4<double> rmat;
	Vec3<double> camr;
	// Get camera position
	camr = c->arg3d(9);
// 	obj.rs->resetCamera(camr);   TGAY
	// Get rotation matrix
	rmat.rows[0].set(c->arg3d(0),0.0);
	rmat.rows[1].set(c->arg3d(3),0.0);
	rmat.rows[2].set(c->arg3d(6),0.0);
	rmat.rows[3].set(0.0,0.0,0.0,1.0);
// 	obj.rs->setRotationMatrix(rmat);   TGAY
	// Get camera z-rotation (if present)
// 	obj.rs->setCameraRotation(c->hasArg(12) ? c->argd(12) / DEGRAD : 0.0);
	rv.reset();
	return TRUE;
}

// Render speed test
bool Command::function_SpeedTest(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!gui.exists())
	{
		msg.print("Can't perform rendering speedtest without the GUI.\n");
		return FALSE;
	}
	clock_t tstart = clock();
	// Loop over n renders (or 100 of no variable given)
	int nrenders = (c->hasArg(0) ? c->argi(0) : 100);
	for (int n=0; n < nrenders; n ++)
	{
		obj.rs->rotateView(5.0,0.0);
		gui.mainView.postRedisplay();
	}
	clock_t tfinish = clock();
	printf("%f    %f    %f \n", double(tstart), double(tfinish), double(CLOCKS_PER_SEC));
	double nsec = (tfinish-tstart) / double(CLOCKS_PER_SEC);
	msg.print("SPEEDTEST : Performed %i renders over %8.2f seconds (%8.2f/sec).\n", nrenders, nsec, nrenders/nsec);
	rv.reset();
	return TRUE;
}

// Translate view
bool Command::function_TranslateView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->adjustCamera(c->arg3d(0),0.0);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// View along specified axis
bool Command::function_ViewAlong(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlong(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// View along specified cell axis
bool Command::function_ViewAlongCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlongCell(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Zoom view
bool Command::function_ZoomView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->adjustCamera(0.0,0.0,c->argd(0),0.0);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// ZRotate view
bool Command::function_ZRotateView(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->zRotateView(-c->argd(0));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}


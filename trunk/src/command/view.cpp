/*
	*** View command functions
	*** src/command/view.cpp
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
#include "base/messenger.h"
#include "gui/gui.h"
#include "model/model.h"
#include <time.h>

// Get current view
int Command::function_CA_GETVIEW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Mat4<double> rmat = obj.rs->rotationMatrix();
	Vec3<double> camr = obj.rs->camera();
	double camrot = obj.rs->cameraRotation();
	msg.print( "View [R c z] = %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", rmat.rows[0].x, rmat.rows[0].y, rmat.rows[0].z, rmat.rows[1].x, rmat.rows[1].y, rmat.rows[1].z, rmat.rows[2].x, rmat.rows[2].y, rmat.rows[2].z, camr.x, camr.y, camr.z, camrot * DEGRAD);
	return Command::Success;
}

// Set orthographic view
int Command::function_CA_ORTHOGRAPHIC(CommandNode *&c, Bundle &obj)
{
	prefs.setPerspective(FALSE);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set perspective view
int Command::function_CA_PERSPECTIVE(CommandNode *&c, Bundle &obj)
{
	prefs.setPerspective(TRUE);
	if (c->hasArg(0)) prefs.setPerspectiveFov(c->argd(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Reset view
int Command::function_CA_RESETVIEW(CommandNode *&c, Bundle &obj)
{
	obj.rs->resetView();
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Rotate view
int Command::function_CA_ROTATEVIEW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->rotate(c->argd(0), c->argd(1));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set current view
int Command::function_CA_SETVIEW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Mat4<double> rmat;
	Vec3<double> camr;
	obj.rs->rotationMatrix().print();
	// Get camera position
	camr = c->arg3d(9);
	obj.rs->resetCamera(camr);
	// Get rotation matrix
	rmat.rows[0].set(c->arg3d(0),0.0);
	rmat.rows[1].set(c->arg3d(3),0.0);
	rmat.rows[2].set(c->arg3d(6),0.0);
	rmat.rows[3].set(0.0,0.0,0.0,1.0);
	obj.rs->setRotationMatrix(rmat);
	// Get camera z-rotation (if present)
	obj.rs->setCameraRotation(c->hasArg(12) ? c->argd(12) / DEGRAD : 0.0);
	return Command::Success;
}

// Render speed test
int Command::function_CA_SPEEDTEST(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (!gui.exists())
	{
		msg.print("Can't perform rendering speedtest without the GUI.\n");
		return Command::Fail;
	}
	clock_t tstart = clock();
	// Loop over n renders (or 100 of no variable given)
	int nrenders = (c->hasArg(0) ? c->argi(0) : 100);
	for (int n=0; n < nrenders; n ++)
	{
		obj.rs->rotate(5.0,0.0);
		gui.mainView.postRedisplay();
	}
	clock_t tfinish = clock();
	double nsec = double(tfinish-tstart) / CLOCKS_PER_SEC;
	msg.print("SPEEDTEST : Performed %i renders over %8.2f seconds (%8.2f/sec).\n", nrenders, nsec, nrenders/nsec);
	return Command::Success;
}

// Translate view
int Command::function_CA_TRANSLATEVIEW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->adjustCamera(c->arg3d(0),0.0);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// View along specified axis
int Command::function_CA_VIEWALONG(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlong(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// View along specified cell axis
int Command::function_CA_VIEWALONGCELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlongCell(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Zoom view
int Command::function_CA_ZOOMVIEW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->adjustCamera(0.0,0.0,c->argd(0),0.0);
	obj.rs->adjustOrthoSize(-c->argd(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// ZRotate view
int Command::function_CA_ZROTATEVIEW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->zRotate(-c->argd(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

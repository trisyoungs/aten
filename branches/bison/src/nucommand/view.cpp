/*
	*** View functions
	*** src/parser/view.cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "base/messenger.h"
#include "gui/gui.h"
#include "model/model.h"
#include <ctime>

// Get current view
bool NuCommand::function_Getview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Mat4<double> rmat = obj.rs->rotationMatrix();
	Vec3<double> camr = obj.rs->camera();
	double camrot = obj.rs->cameraRotation();
	msg.print( "View [R c z] = %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", rmat.rows[0].x, rmat.rows[0].y, rmat.rows[0].z, rmat.rows[1].x, rmat.rows[1].y, rmat.rows[1].z, rmat.rows[2].x, rmat.rows[2].y, rmat.rows[2].z, camr.x, camr.y, camr.z, camrot * DEGRAD);
	return TRUE;
}

// Set orthographic view
bool NuCommand::function_Orthographic(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	prefs.setPerspective(FALSE);
	gui.mainView.postRedisplay();
	return TRUE;
}

// Set perspective view
bool NuCommand::function_Perspective(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	prefs.setPerspective(TRUE);
	if (c->hasArg(0)) prefs.setPerspectiveFov(c->argd(0));
	gui.mainView.postRedisplay();
	return TRUE;
}

// Reset view
bool NuCommand::function_Resetview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	obj.rs->resetView();
	gui.mainView.postRedisplay();
	return TRUE;
}

// Rotate view
bool NuCommand::function_Rotateview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->rotate(c->argd(0), c->argd(1));
	gui.mainView.postRedisplay();
	return TRUE;
}

// Set current view
bool NuCommand::function_Setview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Mat4<double> rmat;
	Vec3<double> camr;
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
	return TRUE;
}

// Render speed test
bool NuCommand::function_Speedtest(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
		obj.rs->rotate(5.0,0.0);
		gui.mainView.postRedisplay();
	}
	clock_t tfinish = clock();
	printf("%f    %f    %f \n", double(tstart), double(tfinish), double(CLOCKS_PER_SEC));
	double nsec = (tfinish-tstart) / double(CLOCKS_PER_SEC);
	msg.print("SPEEDTEST : Performed %i renders over %8.2f seconds (%8.2f/sec).\n", nrenders, nsec, nrenders/nsec);
	return TRUE;
}

// Translate view
bool NuCommand::function_Translateview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->adjustCamera(c->arg3d(0),0.0);
	gui.mainView.postRedisplay();
	return TRUE;
}

// View along specified axis
bool NuCommand::function_Viewalong(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlong(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	return TRUE;
}

// View along specified cell axis
bool NuCommand::function_Viewalongcell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlongCell(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	return TRUE;
}

// Zoom view
bool NuCommand::function_Zoomview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->adjustCamera(0.0,0.0,c->argd(0),0.0);
// 	obj.rs->adjustOrthoSize(-c->argd(0));   TGAY
	gui.mainView.postRedisplay();
	return TRUE;
}

// ZRotate view
bool NuCommand::function_Zrotateview(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->zRotate(-c->argd(0));
	gui.mainView.postRedisplay();
	return TRUE;
}

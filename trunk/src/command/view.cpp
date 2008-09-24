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
int CommandData::function_CA_GETVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Mat4<double> rmat = obj.rs->rotationMatrix();
	Vec3<double> camr = obj.rs->camera();
	double camrot = obj.rs->cameraRotation();
	msg.print( "View [R c z] = %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", rmat.rows[0].x, rmat.rows[0].y, rmat.rows[0].z, rmat.rows[1].x, rmat.rows[1].y, rmat.rows[1].z, rmat.rows[2].x, rmat.rows[2].y, rmat.rows[2].z, camr.x, camr.y, camr.z, camrot * DEGRAD);
	return CR_SUCCESS;
}

// Set orthographic view
int CommandData::function_CA_ORTHOGRAPHIC(Command *&c, Bundle &obj)
{
	prefs.setPerspective(FALSE);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Set perspective view
int CommandData::function_CA_PERSPECTIVE(Command *&c, Bundle &obj)
{
	prefs.setPerspective(TRUE);
	if (c->hasArg(0)) prefs.setPerspectiveFov(c->argd(0));
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Reset view
int CommandData::function_CA_RESETVIEW(Command *&c, Bundle &obj)
{
	obj.rs->resetView();
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Rotate view
int CommandData::function_CA_ROTATEVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->rotate(c->argd(0), c->argd(1));
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Set current view
int CommandData::function_CA_SETVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Mat4<double> rmat;
	Vec3<double> camr;
	printf("Old view matrix is:\n");
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
	printf("New view matrix is:\n");
	obj.rs->rotationMatrix().print();
	return CR_SUCCESS;
}

// Render speed test
int CommandData::function_CA_SPEEDTEST(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (!gui.exists())
	{
		msg.print("Can't perform rendering speedtest without the GUI.\n");
		return CR_FAIL;
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
	return CR_SUCCESS;
}

// Translate view
int CommandData::function_CA_TRANSLATEVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->adjustCamera(c->arg3d(0),0.0);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// View along specified axis
int CommandData::function_CA_VIEWALONG(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlong(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// View along specified cell axis
int CommandData::function_CA_VIEWALONGCELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Set model rotation matrix to be along the specified axis
	obj.rs->viewAlongCell(c->argd(0), c->argd(1), c->argd(2));
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Zoom view
int CommandData::function_CA_ZOOMVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->adjustCamera(0.0,0.0,c->argd(0),0.0);
	obj.rs->adjustOrthoSize(-c->argd(0));
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// ZRotate view
int CommandData::function_CA_ZROTATEVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->zRotate(c->argd(0) / DEGRAD);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

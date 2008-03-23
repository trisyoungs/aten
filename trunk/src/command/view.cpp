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
#include "base/debug.h"
#include "gui/gui.h"
#include "model/model.h"
#include <time.h>

// Reset view
int CommandData::function_CA_RESETVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->resetView();
	gui.refresh();
	return CR_SUCCESS;
}

// Rotate view
int CommandData::function_CA_ROTATEVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->rotate(c->argd(0), c->argd(1));
	gui.refresh();
	return CR_SUCCESS;
}

// Translate view
int CommandData::function_CA_TRANSLATEVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->adjustCamera(c->arg3d(0),0.0);
	gui.refresh();
	return CR_SUCCESS;
}

// Zoom view
int CommandData::function_CA_ZOOMVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->adjustCamera(0.0,0.0,c->argd(2),0.0);
	gui.refresh();
	return CR_SUCCESS;
}

// ZRotate view
int CommandData::function_CA_ZROTATEVIEW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->zRotate(c->argd(0));
	gui.refresh();
	return CR_SUCCESS;
}

// Render speed test
int CommandData::function_CA_SPEEDTEST(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (!gui.exists())
	{
		msg(DM_NONE,"Can't perform rendering speedtest without the GUI.\n");
		return CR_FAIL;
	}
	clock_t tstart = clock();
	// Loop over n renders (or 100 of no variable given)
	int nrenders = (c->hasArg(0) ? c->argi(0) : 100);
	for (int n=0; n < nrenders; n ++)
	{
		obj.m->rotate(5.0,0.0);
		gui.refresh();
	}
	clock_t tfinish = clock();
	double nsec = double(tfinish-tstart) / CLOCKS_PER_SEC;
	msg(DM_NONE,"SPEEDTEST : Performed %i renders over %8.2f seconds (%8.2f/sec).\n", nrenders, nsec, nrenders/nsec);
	return CR_SUCCESS;
}

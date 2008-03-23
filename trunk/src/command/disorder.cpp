/*
	*** Disorder command functions
	*** src/command/disorder.cpp
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

#include "command/commands.h"
#include "base/master.h"
#include "base/debug.h"
#include "model/model.h"
#include "methods/mc.h"

// Adds component to list ('addcomponent <name> <model> <nmols>')
int CommandData::function_CA_ADDCOMPONENT(Command *&c, Bundle &obj)
{
	Model *compm = master.findModel(c->argc(1));
	if (compm != NULL)
	{
		Component *newcomp = mc.components.add();
		newcomp->setModel(compm);
		newcomp->setNRequested(c->argi(2));
		newcomp->setName(c->argc(0));
	}
	else
	{
		msg(DM_NONE,"Couldn't find model '%s' specified in 'addcomponent'\n", c->argc(1));
		return CR_FAIL;
	}
	return CR_SUCCESS;
}

// Performs MC insertion ('disorder <ncycles>')
int CommandData::function_CA_DISORDER(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (mc.components.nItems() == 0)
	{
		msg(DM_NONE,"Disordered builder requires a list of components.\n");
		return CR_FAIL;
	}
	msg(DM_NONE,"Performing disordered build for model '%s'\n", obj.m->name());
	mc.setNCycles(c->argi(0));
	mc.disorder(obj.m);
	return CR_SUCCESS;
}

// Print current component list ('printcomponents')
int CommandData::function_CA_PRINTCOMPONENTS(Command *&c, Bundle &obj)
{
	msg(DM_NONE,"Current component list:\n");
	Vec3<double> v1, v2;
	Component *comp = mc.components.first();
	comp != NULL ? printf("Name         Nmols  I D T R Z  Model         ComponentRegion       cent.x  cent.y  cent.z  size.x  size.y  size.z  Overlap\n")
		: printf("None.\n");
	while (comp != NULL)
	{
		v1 = comp->area.centre();
		v2 = comp->area.size();
		printf("%-10s  %5i  %s %s %s %s %s  %-12s  %-12s %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %3s\n",
			comp->name(),comp->nRequested(),
			(comp->isMoveAllowed(MT_INSERT) ? "+" : " "),
			(comp->isMoveAllowed(MT_DELETE) ? "+" : " "),
			(comp->isMoveAllowed(MT_TRANSLATE) ? "+" : " "),
			(comp->isMoveAllowed(MT_ROTATE) ? "+" : " "),
			(comp->isMoveAllowed(MT_ZMATRIX) ? "+" : " "),
			comp->model()->name(),
			text_from_RS(comp->area.shape()),
			v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
			(comp->area.allowOverlap() ? "Yes" : "No"));
		comp = comp->next;
	}
	return CR_SUCCESS;
}

// Set region centre to position ('setcentre <name> <x y z>')
int CommandData::function_CA_SETCENTRE(Command *&c, Bundle &obj)
{
	Component *comp = mc.componentByName(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	comp->area.setCentre(c->arg3d(1));
	return CR_SUCCESS;
}

// Set geometry of region ('setgeometry <name> <x y z> [l]')
int CommandData::function_CA_SETGEOMETRY(Command *&c, Bundle &obj)
{
	Component *comp = mc.componentByName(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	comp->area.setSize(c->arg3d(1));
	if (!c->hasArg(4)) comp->area.setLength(c->argd(4));
	return CR_SUCCESS;
}

// Set overlap flag ('setoverlap <name> true|false')
int CommandData::function_CA_SETOVERLAP(Command *&c, Bundle &obj)
{
	Component *comp = mc.componentByName(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	comp->area.setAllowOverlap(c->argb(1));
	return CR_SUCCESS;
}

// Set shape for region ('setshape <name> <shape>')
int CommandData::function_CA_SETSHAPE(Command *&c, Bundle &obj)
{
	Component *comp = mc.componentByName(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	ComponentRegionShape rs = RS_from_text(c->argc(1));
	if (rs != RS_NITEMS) comp->area.setShape(rs);
	return CR_SUCCESS;
}

// Set vdw radius scaling for method ('vdwscale <scale>')
int CommandData::function_CA_VDWSCALE(Command *&c, Bundle &obj)
{
	mc.setVdwScale(c->argd(0));
	return CR_SUCCESS;
}

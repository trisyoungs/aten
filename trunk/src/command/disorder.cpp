/*
	*** Disorder command functions
	*** src/command/disorder.cpp
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

#include "command/commandlist.h"
#include "main/aten.h"
#include "model/model.h"
#include "methods/mc.h"

// Performs MC insertion ('disorder <ncycles>')
int Command::function_CA_DISORDER(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	msg.print("Performing disordered build for model '%s'\n", obj.m->name());
	mc.setNCycles(c->argi(0));
	if (!mc.disorder(obj.m)) return Command::Fail;
	return Command::Success;
}

// Print current component list ('listcomponents')
int Command::function_CA_LISTCOMPONENTS(CommandNode *&c, Bundle &obj)
{
	msg.print("Current component specification:\n");
	Vec3<double> v1, v2;
	char s[150];
	msg.print("                                                      Centre                   Size\n");
	msg.print("Model        nMols  I D T R Z    Region         X       Y       Z       X       Y       Z     Overlap\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		v1 = m->area.centre();
		v2 = m->area.size();
		sprintf(s,"%-10s  %5i  %s %s %s %s %s  %-12s %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %3s\n",
			m->name(),m->nRequested(),
			(m->isMoveAllowed(MonteCarlo::Insert) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::Delete) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::Translate) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::Rotate) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::ZMatrix) ? "+" : " "),
			ComponentRegion::regionShape(m->area.shape()),
			v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
			(m->area.allowOverlap() ? "Yes" : "No"));
		msg.print(s);
	}
	return Command::Success;
}

// Set region definition ('region <shape> <cx cy cz> <x y z> yes|no')
int Command::function_CA_REGION(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs == ComponentRegion::nRegionShapes) return Command::Fail;
	obj.m->area.setShape(rs);
	obj.m->area.setCentre(c->arg3d(1));
	obj.m->area.setSize(c->arg3d(4));
	obj.m->area.setAllowOverlap(c->argb(7));
	return Command::Success;
}

// Set region centre ('regioncentre <x y z>')
int Command::function_CA_REGIONCENTRE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->area.setCentre(c->arg3d(0));
	return Command::Success;
}

// Set region centre in fractional coordinates ('regioncentref <x y z>')
int Command::function_CA_REGIONCENTREF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->area.setCentreFrac(c->arg3d(0));
	return Command::Success;
}

// Set region definition in fractional coordinates ('regionf <shape> <cx cy cz> <x y z> yes|no')
int Command::function_CA_REGIONF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs == ComponentRegion::nRegionShapes) return Command::Fail;
	obj.m->area.setShape(rs);
	obj.m->area.setCentreFrac(c->arg3d(1));
	obj.m->area.setSizeFrac(c->arg3d(4));
	obj.m->area.setAllowOverlap(c->argb(7));
	return Command::Success;
}

// Set geometry of region in fractional coordinates ('regiongeometryf <x y z> [l]')
int Command::function_CA_REGIONGEOMETRY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->area.setSize(c->arg3d(0));
	if (!c->hasArg(3)) obj.m->area.setLength(c->argd(3));
	return Command::Success;
}

// Set geometry of region ('regiongeometryf <x y z> [l]')
int Command::function_CA_REGIONGEOMETRYF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->area.setSizeFrac(c->arg3d(0));
	if (!c->hasArg(3)) obj.m->area.setLength(c->argd(3));
	return Command::Success;
}

// Set overlap flag for the current model ('regionoverlaps true|false')
int Command::function_CA_REGIONOVERLAPS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->area.setAllowOverlap(c->argb(0));
	return Command::Success;
}

// Set shape for region ('regionshape <shape>')
int Command::function_CA_REGIONSHAPE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs != ComponentRegion::nRegionShapes) obj.m->area.setShape(rs);
	return Command::Success;
}

// Set number of requested molecules ('nmols <n>')
int Command::function_CA_NMOLS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->setNRequested(c->argi(0));
	return Command::Success;
}

// Set vdw radius scaling for method ('vdwscale <scale>')
int Command::function_CA_VDWSCALE(CommandNode *&c, Bundle &obj)
{
	mc.setVdwScale(c->argd(0));
	return Command::Success;
}

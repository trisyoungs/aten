/*
	*** Disorder Commands
	*** src/nucommand/disorder.cpp
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
#include "main/aten.h"
#include "model/model.h"
#include "methods/mc.h"

// Performs MC insertion ('disorder <ncycles>')
bool NuCommand::function_Disorder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	msg.print("Performing disordered build for model '%s'\n", obj.m->name());
	mc.setNCycles(c->argi(0));
	rv.reset();
	if (!mc.disorder(obj.m)) return FALSE;
	return TRUE;
}

// Print current component list ('listcomponents')
bool NuCommand::function_ListComponents(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
	rv.reset();
	return TRUE;
}

// Set region definition ('region <shape> <cx cy cz> <x y z> yes|no')
bool NuCommand::function_Region(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs == ComponentRegion::nRegionShapes) return FALSE;
	obj.m->area.setShape(rs);
	obj.m->area.setCentre(c->arg3d(1));
	obj.m->area.setSize(c->arg3d(4));
	obj.m->area.setAllowOverlap(c->argb(7));
	rv.reset();
	return TRUE;
}

// Set region centre ('regioncentre <x y z>')
bool NuCommand::function_RegionCentre(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->area.setCentre(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set region centre in fractional coordinates ('regioncentref <x y z>')
bool NuCommand::function_RegionCentreFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->area.setCentreFrac(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set region definition in fractional coordinates ('regionf <shape> <cx cy cz> <x y z> yes|no')
bool NuCommand::function_RegionFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs == ComponentRegion::nRegionShapes) return FALSE;
	obj.m->area.setShape(rs);
	obj.m->area.setCentreFrac(c->arg3d(1));
	obj.m->area.setSizeFrac(c->arg3d(4));
	obj.m->area.setAllowOverlap(c->argb(7));
	rv.reset();
	return TRUE;
}

// Set geometry of region in fractional coordinates ('regiongeometryf <x y z> [l]')
bool NuCommand::function_RegionGeometry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->area.setSize(c->arg3d(0));
	if (!c->hasArg(3)) obj.m->area.setLength(c->argd(3));
	rv.reset();
	return TRUE;
}

// Set geometry of region ('regiongeometryf <x y z> [l]')
bool NuCommand::function_RegionGeometryFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->area.setSizeFrac(c->arg3d(0));
	if (!c->hasArg(3)) obj.m->area.setLength(c->argd(3));
	rv.reset();
	return TRUE;
}

// Set overlap flag for the current model ('regionoverlaps true|false')
bool NuCommand::function_RegionOverlaps(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->area.setAllowOverlap(c->argb(0));
	rv.reset();
	return TRUE;
}

// Set shape for region ('regionshape <shape>')
bool NuCommand::function_RegionShape(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs != ComponentRegion::nRegionShapes) obj.m->area.setShape(rs);
	rv.reset();
	return TRUE;
}

// Set number of requested molecules ('nmols <n>')
bool NuCommand::function_NMols(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->setNRequested(c->argi(0));
	rv.reset();
	return TRUE;
}

// Set vdw radius scaling for method ('vdwscale <scale>')
bool NuCommand::function_VdwScale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	mc.setVdwScale(c->argd(0));
	rv.reset();
	return TRUE;
}

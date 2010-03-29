/*
	*** Disorder Commands
	*** src/command/disorder.cpp
	Copyright T. Youngs 2007-2010

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
#include "main/aten.h"
#include "model/model.h"
#include "methods/mc.h"

// Performs MC insertion ('disorder <ncycles>')
bool Command::function_Disorder(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	msg.print("Performing disordered build for model '%s'\n", obj.m->name());
	mc.setNCycles(c->argi(0));
	rv.reset();
	if (!mc.disorder(obj.m)) return FALSE;
	return TRUE;
}

// Print current component list ('listcomponents')
bool Command::function_ListComponents(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	msg.print("Current component specification:\n");
	Vec3<double> v1, v2;
	char s[150];
	msg.print("                                                      Centre                 Geometry\n");
	msg.print("Model        nMols  I D T R Z    Region         X       Y       Z       X       Y       Z     Overlap\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		ComponentRegion *r = m->region();
		v1 = r->centre();
		v2 = r->geometry();
		sprintf(s,"%-10s  %5i  %s %s %s %s %s  %-12s %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %3s\n",
			m->name(),m->nRequested(),
			(m->isMoveAllowed(MonteCarlo::Insert) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::Delete) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::Translate) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::Rotate) ? "+" : " "),
			(m->isMoveAllowed(MonteCarlo::ZMatrix) ? "+" : " "),
			ComponentRegion::regionShape(r->shape()),
			v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
			(r->allowOverlap() ? "Yes" : "No"));
		msg.print(s);
	}
	rv.reset();
	return TRUE;
}

// Set number of requested molecules ('nmols <n>')
bool Command::function_NMols(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->setNRequested(c->argi(0));
	rv.reset();
	return TRUE;
}

// Set region centre ('regioncentre <x y z>')
bool Command::function_RegionCentre(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->region()->setCentre(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set region centre in fractional coordinates ('regioncentref <x y z>')
bool Command::function_RegionCentreFrac(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->region()->setCentreFrac(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set geometry of region in fractional coordinates ('regiongeometryf <x y z>')
bool Command::function_RegionGeometry(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->region()->setGeometry(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set geometry of region ('regiongeometryf <x y z>')
bool Command::function_RegionGeometryFrac(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->region()->setGeometryFrac(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set overlap flag for the current model ('regionoverlaps true|false')
bool Command::function_RegionOverlaps(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->region()->setAllowOverlap(c->argb(0));
	rv.reset();
	return TRUE;
}

// Set rotations for the current region ('regionrotation x y')
bool Command::function_RegionRotation(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> v( c->argd(0), c->argd(1), 0.0 );
	obj.m->region()->setRotations(v);
	rv.reset();
	return TRUE;
}

// Set shape for region ('regionshape <shape>')
bool Command::function_RegionShape(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	ComponentRegion *r = obj.m->region();
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs != ComponentRegion::nRegionShapes) r->setShape(rs);
	rv.reset();
	return TRUE;
}

// Set region definition ('setregion <shape> <cx cy cz> <x y z> yes|no')
bool Command::function_SetRegion(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs == ComponentRegion::nRegionShapes) return FALSE;
	ComponentRegion *r = obj.m->region();
	r->setShape(rs);
	r->setCentre(c->arg3d(1));
	r->setGeometry(c->arg3d(4));
	if (c->hasArg(7)) r->setAllowOverlap(c->argb(7));
	rv.reset();
	return TRUE;
}

// Set region definition in fractional coordinates ('setregionfrac <shape> <cx cy cz> <x y z> yes|no')
bool Command::function_SetRegionFrac(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	ComponentRegion::RegionShape rs = ComponentRegion::regionShape(c->argc(0));
	if (rs == ComponentRegion::nRegionShapes) return FALSE;
	ComponentRegion *r = obj.m->region();
	r->setShape(rs);
	r->setCentreFrac(c->arg3d(1));
	r->setGeometryFrac(c->arg3d(4));
	r->setAllowOverlap(c->argb(7));
	rv.reset();
	return TRUE;
}

// Set vdw radius scaling for method ('vdwscale <scale>')
bool Command::function_VdwScale(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) mc.setVdwScale(c->argd(0));
	rv.set(mc.vdwScale());
	return TRUE;
}


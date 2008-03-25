/*
	*** Prefs command functions
	*** src/command/prefs.cpp
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
#include "base/elements.h"
#include "gui/gui.h"
#include "model/model.h"

// Atom quadric detail
int CommandData::function_CA_ATOMDETAIL(Command *&c, Bundle &obj)
{
	prefs.setAtomDetail(c->argi(0));
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Bond quadric detail
int CommandData::function_CA_BONDDETAIL(Command *&c, Bundle &obj)
{
	prefs.setBondDetail(c->argi(0));
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Colours
int CommandData::function_CA_COLOUR(Command *&c, Bundle &obj)
{
	Colour col = COL_from_text(c->argc(0));
	if (col == COL_NITEMS) return CR_FAIL;
	Vec3<GLfloat> colvec = c->arg3f(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.setColour(col, colvec.x, colvec.y, colvec.z, alpha);
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Set density unit to use in output ('densityunits <unit>')
int CommandData::function_CA_DENSITYUNITS(Command *&c, Bundle &obj)
{
	DensityUnit du = DU_from_text(c->argc(0));
	if (du == DU_NITEMS) return CR_FAIL;
	else prefs.setDensityUnits(du);
	return CR_SUCCESS;
}

// Set electrostatics cutoff ('ecut <cut>')
int CommandData::function_CA_ECUT(Command *&c, Bundle &obj)
{
	prefs.setElecCutoff(c->argd(0));
	return CR_SUCCESS;
}

// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
int CommandData::function_CA_ELEC(Command *&c, Bundle &obj)
{
	ElecMethod em = EM_from_text(c->argc(0));
	if (em == EM_NITEMS) return CR_FAIL;
	prefs.setElectrostaticsMethod(em);
	prefs.setCalculateElec(em == EM_OFF ? FALSE : TRUE);
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (EM_EWALD):
			prefs.setEwaldAlpha(c->argd(1));
			prefs.setEwaldKvec(c->arg3i(2));
			break;
		// Set ewald precision
		case (EM_EWALDAUTO):
			prefs.setEwaldPrecision(c->argd(1));
			break;
	}
	return CR_SUCCESS;
}

// Set element's ambient colour
int CommandData::function_CA_ELEMENTAMBIENT(Command *&c, Bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.setAmbientColour(el,0,c->argi(1));
	elements.setAmbientColour(el,1,c->argi(2));
	elements.setAmbientColour(el,2,c->argi(3));
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Set element's diffuse colour
int CommandData::function_CA_ELEMENTDIFFUSE(Command *&c, Bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.setDiffuseColour(el,0,c->argi(1));
	elements.setDiffuseColour(el,1,c->argi(2));
	elements.setDiffuseColour(el,2,c->argi(3));
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Set element's radius
int CommandData::function_CA_ELEMENTRADIUS(Command *&c, Bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.setAtomicRadius(el, c->argd(1));
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Set energy unit to use in output ('energyunits <unit>')
int CommandData::function_CA_ENERGYUNITS(Command *&c, Bundle &obj)
{
	EnergyUnit eu = EU_from_text(c->argc(0));
	if (eu == EU_NITEMS) return CR_FAIL;
	else prefs.setEnergyUnit(eu);
	return CR_SUCCESS;
}

// GL Options
int CommandData::function_CA_GL(Command *&c, Bundle &obj)
{
	GlOption go = GO_from_text(c->argc(0));
	if (go == GO_NITEMS) return CR_FAIL;
	if (c->argb(1)) prefs.addGlOption(go);
	else prefs.removeGlOption(go);
	if (gui.exists()) gui.mainView.initGl();
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Turn on/off calculation of intra ('intra on|off')
int CommandData::function_CA_INTRA(Command *&c, Bundle &obj)
{
	prefs.setCalculateIntra(c->argb(0));
	return CR_SUCCESS;
}

// Key bindings
int CommandData::function_CA_KEY(Command *&c, Bundle &obj)
{
	ModifierKey mk = MK_from_text(c->argc(0));
	KeyAction ka = KA_from_text(c->argc(1));
	if ((mk != MK_NITEMS) && (ka != KA_NITEMS)) prefs.setKeyAction(mk,ka);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Mouse bindings
int CommandData::function_CA_MOUSE(Command *&c, Bundle &obj)
{
	MouseButton mb = MB_from_text(c->argc(0));
	MouseAction ma = MA_from_text(c->argc(1));
	if ((ma != MA_NITEMS) && (mb != MB_NITEMS)) prefs.setMouseAction(mb,ma);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Atom screen radii
int CommandData::function_CA_RADIUS(Command *&c, Bundle &obj)
{
	Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
	if (ds != Atom::nDrawStyles) prefs.setAtomSize(ds, c->argd(1));
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Atom shininess
int CommandData::function_CA_SHININESS(Command *&c, Bundle &obj)
{
	prefs.setShininess(c->argi(0));
	if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
	if (gui.exists()) gui.refresh();
	return CR_SUCCESS;
}

// Render Objects
int CommandData::function_CA_SHOW(Command *&c, Bundle &obj)
{
	ViewObject vo = VO_from_text(c->argc(0));
	if (vo != VO_NITEMS)
	{
		prefs.setVisible(vo, c->argb(1));
		if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
		if (gui.exists()) gui.refresh();
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}

// View Styles
int CommandData::function_CA_STYLE(Command *&c, Bundle &obj)
{
	Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
	if (ds != Atom::nDrawStyles)
	{
		prefs.setRenderStyle(ds);
		if (obj.m != NULL) obj.m->logChange(LOG_VISUAL);
		if (gui.exists()) gui.refresh();
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Set VDW cutoff ('vcut <cut>')
int CommandData::function_CA_VCUT(Command *&c, Bundle &obj)
{
	prefs.setVdwCutoff(c->argd(0));
	return CR_SUCCESS;

}

// Turn on/off calculation of vdw ('vdw on|off')
int CommandData::function_CA_VDW(Command *&c, Bundle &obj)
{
	prefs.setCalculateVdw(c->argb(0));
	return CR_SUCCESS;
}

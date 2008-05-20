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
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Bond quadric detail
int CommandData::function_CA_BONDDETAIL(Command *&c, Bundle &obj)
{
	prefs.setBondDetail(c->argi(0));
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Colours
int CommandData::function_CA_COLOUR(Command *&c, Bundle &obj)
{
	Prefs::Colour col = Prefs::colour(c->argc(0));
	if (col == Prefs::nColours) return CR_FAIL;
	Vec3<GLfloat> colvec = c->arg3f(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.setPenColour(col, colvec.x, colvec.y, colvec.z, alpha);
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Set density unit to use in output ('densityunits <unit>')
int CommandData::function_CA_DENSITYUNITS(Command *&c, Bundle &obj)
{
	Prefs::DensityUnit du = Prefs::densityUnit(c->argc(0));
	if (du == Prefs::nDensityUnits) return CR_FAIL;
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
	Electrostatics::ElecMethod em = Electrostatics::elecMethod(c->argc(0));
	if (em == Electrostatics::nElectrostatics) return CR_FAIL;
	prefs.setElectrostaticsMethod(em);
	prefs.setCalculateElec(em == Electrostatics::None ? FALSE : TRUE);
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (Electrostatics::Ewald):
			prefs.setEwaldAlpha(c->argd(1));
			prefs.setEwaldKvec(c->arg3i(2));
			break;
		// Set ewald precision
		case (Electrostatics::EwaldAuto):
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
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
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
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Set element's radius
int CommandData::function_CA_ELEMENTRADIUS(Command *&c, Bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.setAtomicRadius(el, c->argd(1));
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Set energy unit to use in output ('energyunits <unit>')
int CommandData::function_CA_ENERGYUNITS(Command *&c, Bundle &obj)
{
	Prefs::EnergyUnit eu = Prefs::energyUnit(c->argc(0));
	if (eu == Prefs::nEnergyUnits) return CR_FAIL;
	else prefs.setEnergyUnit(eu);
	return CR_SUCCESS;
}

// GL Options
int CommandData::function_CA_GL(Command *&c, Bundle &obj)
{
	Prefs::GlOption go = Prefs::glOption(c->argc(0));
	if (go == Prefs::nGlOptions) return CR_FAIL;
	if (c->argb(1)) prefs.addGlOption(go);
	else prefs.removeGlOption(go);
	gui.mainView.initGl();
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
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
	Prefs::ModifierKey mk = Prefs::modifierKey(c->argc(0));
	Prefs::KeyAction ka = Prefs::keyAction(c->argc(1));
	if ((mk != Prefs::nModifierKeys) && (ka != Prefs::nKeyActions)) prefs.setKeyAction(mk,ka);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Mouse bindings
int CommandData::function_CA_MOUSE(Command *&c, Bundle &obj)
{
	Prefs::MouseButton mb = Prefs::mouseButton(c->argc(0));
	Prefs::MouseAction ma = Prefs::mouseAction(c->argc(1));
	if ((ma != Prefs::nMouseActions) && (mb != Prefs::nMouseButtons)) prefs.setMouseAction(mb,ma);
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

// Set whether replicate folds atoms beforehand
int CommandData::function_CA_REPLICATEFOLD(Command *&c, Bundle &obj)
{
	prefs.setReplicateFold(c->argb(0));
	return CR_SUCCESS;
}

// Set whether replicate trims atoms afterwards
int CommandData::function_CA_REPLICATETRIM(Command *&c, Bundle &obj)
{
	prefs.setReplicateTrim(c->argb(0));
	return CR_SUCCESS;
}

// Colouring scheme
int CommandData::function_CA_SCHEME(Command *&c, Bundle &obj)
{
	Prefs::ColouringScheme cs = Prefs::colouringScheme(c->argc(0));
	if (cs != Prefs::nColouringSchemes)
	{
		prefs.setColourScheme(cs);
		if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
		gui.mainView.postRedisplay();
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Atom shininess
int CommandData::function_CA_SHININESS(Command *&c, Bundle &obj)
{
	prefs.setShininess(c->argi(0));
	if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
	gui.mainView.postRedisplay();
	return CR_SUCCESS;
}

// Render Objects
int CommandData::function_CA_SHOW(Command *&c, Bundle &obj)
{
	Prefs::ViewObject vo = Prefs::viewObject(c->argc(0));
	if (vo != Prefs::nViewObjects)
	{
		prefs.setVisible(vo, c->argb(1));
		if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
		gui.mainView.postRedisplay();
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
		if (obj.m != NULL) obj.m->logChange(Change::VisualLog);
		gui.mainView.postRedisplay();
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Set whether to use nice text rendering ('usenicetext on|off')
int CommandData::function_CA_USENICETEXT(Command *&c, Bundle &obj)
{
	prefs.setUseNiceText(c->argb(0));
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

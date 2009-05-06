/*
	*** Prefs Commands
	*** src/command/prefs.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "gui/gui.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/prefs.h"

// Angle label postfix
bool Command::function_AngleLabel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		prefs.setAngleLabel(c->argc(0));
		gui.mainView.postRedisplay();
	}
	rv.set(prefs.angleLabel());
	return TRUE;
}

// Atom quadric detail
bool Command::function_AtomDetail(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		prefs.setAtomDetail(c->argi(0));
		if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
		gui.mainView.postRedisplay();
	}
	rv.set(prefs.atomDetail());
	return TRUE;
}

// Bond quadric detail
bool Command::function_BondDetail(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		prefs.setBondDetail(c->argi(0));
		if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
		gui.mainView.postRedisplay();
	}
	rv.set(prefs.bondDetail());
	return TRUE;
}

// Cache Limit
bool Command::function_CacheLimit(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setCacheLimit(c->argi(0));
	rv.set(prefs.cacheLimit());
	return TRUE;
}

// Colours
bool Command::function_Colour(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Prefs::PenColour col = Prefs::penColour(c->argc(0));
	if (col == Prefs::nPenColours) return FALSE;
	Vec3<GLfloat> colvec = c->arg3GLf(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.setColour(col, colvec.x, colvec.y, colvec.z, alpha);
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Common elements list
bool Command::function_CommonElements(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setCommonElements(c->argc(0));
	rv.reset();
	return TRUE;
}

// Set density unit to use in output ('densityunits <unit>')
bool Command::function_DensityUnits(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		Prefs::DensityUnit du = Prefs::densityUnit(c->argc(0));
		if (du == Prefs::nDensityUnits) return FALSE;
		else prefs.setDensityUnits(du);
	}
	rv.set(Prefs::densityUnit(prefs.densityUnit()));
	return TRUE;
}

// Distance label postfix
bool Command::function_DistanceLabel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setDistanceLabel(c->argc(0));
	gui.mainView.postRedisplay();
	rv.set(prefs.distanceLabel());
	return TRUE;
}

// Set electrostatics cutoff ('ecut <cut>')
bool Command::function_ECut(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setElecCutoff(c->argd(0));
	rv.set(prefs.elecCutoff());
	return TRUE;
}

// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
bool Command::function_Elec(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Electrostatics::ElecMethod em = Electrostatics::elecMethod(c->argc(0));
	if (em == Electrostatics::nElectrostatics) return FALSE;
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (Electrostatics::Ewald):
			if (!c->hasArg(4))
			{
				msg.print("Must supply the alpha parameter and kmax vectors to used this electrostatics option.\n");
				return FALSE;
			}
			prefs.setEwaldAlpha(c->argd(1));
			prefs.setEwaldKvec(c->arg3i(2));
			break;
		// Set ewald precision
		case (Electrostatics::EwaldAuto):
			if (!c->hasArg(1))
			{
				msg.print("Must supply the Ewald precision parameter to used this electrostatics option.\n");
				return FALSE;
			}
			prefs.setEwaldPrecision(c->argd(1));
			break;
	}
	// Set method
	prefs.setElectrostaticsMethod(em);
	prefs.setCalculateElec(em == Electrostatics::None ? FALSE : TRUE);
	rv.reset();
	return TRUE;
}

// Set element's ambient colour
bool Command::function_ElementAmbient(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	int el = elements().findAlpha(c->argc(0));
	if (el == 0) return FALSE;
	elements().setAmbientColour(el,0,c->argd(1));
	elements().setAmbientColour(el,1,c->argd(2));
	elements().setAmbientColour(el,2,c->argd(3));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set element's diffuse colour
bool Command::function_ElementDiffuse(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	int el = elements().findAlpha(c->argc(0));
	if (el == 0) return FALSE;
	elements().setDiffuseColour(el,0,c->argd(1));
	elements().setDiffuseColour(el,1,c->argd(2));
	elements().setDiffuseColour(el,2,c->argd(3));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set element's radius
bool Command::function_ElementRadius(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	int el = elements().findAlpha(c->argc(0));
	if (el == 0) return FALSE;
	if (c->hasArg(1))
	{
		elements().setAtomicRadius(el, c->argd(1));
		if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
		gui.mainView.postRedisplay();
	}
	rv.set(elements().atomicRadius(el));
	return TRUE;
}

// Set energy unit to use in output ('energyunits <unit>')
bool Command::function_EnergyUnits(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) 
	{
		Prefs::EnergyUnit eu = Prefs::energyUnit(c->argc(0));
		if (eu == Prefs::nEnergyUnits) return FALSE;
		else
		{
			prefs.setEnergyUnit(eu);
			// Convert loaded forcefields
			for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next) ff->convertParameters();
		}
	}
	rv.set(Prefs::energyUnit(prefs.energyUnit()));
	return TRUE;
}

// GL Options
bool Command::function_GL(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Prefs::GlOption go = Prefs::glOption(c->argc(0));
	if (go == Prefs::nGlOptions) return FALSE;
	if (c->argb(1)) prefs.addGlOption(go);
	else prefs.removeGlOption(go);
	gui.mainView.initGl();
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set distance to use when adding hydrogens ('hdistance <d>')
bool Command::function_HDistance(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setHydrogenDistance(c->argd(0));
	rv.set(prefs.hydrogenDistance());
	return TRUE;
}

// Turn on/off calculation of intra ('intra on|off')
bool Command::function_Intra(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setCalculateIntra(c->argb(0));
	rv.set(prefs.calculateIntra());
	return TRUE;
}

// Key bindings
bool Command::function_Key(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Prefs::ModifierKey mk = Prefs::modifierKey(c->argc(0));
	Prefs::KeyAction ka = Prefs::keyAction(c->argc(1));
	if ((mk != Prefs::nModifierKeys) && (ka != Prefs::nKeyActions)) prefs.setKeyAction(mk,ka);
	else return FALSE;
	rv.set(Prefs::keyAction(prefs.keyAction(mk)));
	return TRUE;
}

// Text label pointsize
bool Command::function_LabelSize(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setLabelSize(c->argi(0));
	gui.mainView.postRedisplay();
	rv.set(prefs.labelSize());
	return TRUE;
}

// Turn on/off spotlight
bool Command::function_Light(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setSpotlightActive(c->argb(0));
	gui.mainView.postRedisplay();
	rv.set( prefs.spotlightActive() );
	return TRUE;
}

// Set ambient component of spotlight
bool Command::function_LightAmbient(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setSpotlightColour(Prefs::AmbientComponent, c->argGLf(0), c->argGLf(1), c->argGLf(2));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set diffuse component of spotlight
bool Command::function_LightDiffuse(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setSpotlightColour(Prefs::DiffuseComponent, c->argGLf(0), c->argGLf(1), c->argGLf(2));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

bool Command::function_LightPosition(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setSpotlightPosition(c->argGLf(0), c->argGLf(1), c->argGLf(2));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Set specular component of spotlight
bool Command::function_LightSpecular(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setSpotlightColour(Prefs::SpecularComponent, c->argGLf(0), c->argGLf(1), c->argGLf(2));
	gui.mainView.postRedisplay();
	rv.reset();
	return TRUE;
}

// Mouse bindings
bool Command::function_Mouse(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Prefs::MouseButton mb = Prefs::mouseButton(c->argc(0));
	Prefs::MouseAction ma = Prefs::mouseAction(c->argc(1));
	if ((ma != Prefs::nMouseActions) && (mb != Prefs::nMouseButtons)) prefs.setMouseAction(mb,ma);
	else return FALSE;
	rv.set(Prefs::mouseAction(prefs.mouseAction(mb)));
	return TRUE;
}

// Atom screen radii
bool Command::function_Radius(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
	if (ds != Atom::nDrawStyles) prefs.setAtomStyleRadius(ds, c->argd(1));
	else return FALSE;
	rv.set(prefs.atomStyleRadius(ds));
	return TRUE;
}

// Set whether replicate folds atoms beforehand
bool Command::function_ReplicateFold(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setReplicateFold(c->argb(0));
	msg.print("Messenger::Verbose, Folding of atoms into unit cell before replicate is %s.\n", prefs.replicateFold() ? "on" : "off");
	rv.set(prefs.replicateFold());
	return TRUE;
}

// Set whether replicate trims atoms afterwards
bool Command::function_ReplicateTrim(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	prefs.setReplicateTrim(c->argb(0));
	msg.print(Messenger::Verbose, "Trimming of atoms outside of unit cell after replicate is %s.\n", prefs.replicateTrim() ? "on" : "off");
	rv.set(prefs.replicateTrim());
	return TRUE;
}

// Colouring scheme
bool Command::function_Scheme(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		Prefs::ColouringScheme cs = Prefs::colouringScheme(c->argc(0));
		if (cs != Prefs::nColouringSchemes)
		{
			prefs.setColourScheme(cs);
			if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
			gui.mainView.postRedisplay();
		}
		else return FALSE;
	}
	rv.set(Prefs::colouringScheme( prefs.colourScheme() ));
	return TRUE;
}

// Atom shininess
bool Command::function_Shininess(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		prefs.setShininess(c->argi(0));
		if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
		gui.mainView.postRedisplay();
	}
	rv.set(prefs.shininess());
	return TRUE;
}

// Render Objects on screen
bool Command::function_ShowOnScreen(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		Prefs::ViewObject vo = Prefs::viewObject(c->argc(0));
		if (vo != Prefs::nViewObjects)
		{
			prefs.setVisibleOnScreen(vo, c->argb(1));
			if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
			gui.mainView.postRedisplay();
		}
		else return FALSE;
	}
	else
	{
		char shown[512], notshown[512];
		shown[0] = '\0';
		notshown[0] = '\0';
		strcat(shown,"Visible: ");
		strcat(notshown,"Hidden : ");
		msg.print( "Current on-screen object status:\n");
		for (int i=0; i<Prefs::nViewObjects; i++)
		{
			if (prefs.isVisibleOnScreen( (Prefs::ViewObject) i))
			{
				strcat(shown, Prefs::viewObject( (Prefs::ViewObject) i));
				strcat(shown, " ");
			}
			else
			{
				strcat(notshown, Prefs::viewObject( (Prefs::ViewObject) i));
				strcat(notshown, " ");
			}
		}
		msg.print( "%s\n", shown);
		msg.print( "%s\n", notshown);
	}
	rv.reset();
	return TRUE;
}

// Render Objects on saved images
bool Command::function_ShowOnImage(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		Prefs::ViewObject vo = Prefs::viewObject(c->argc(0));
		if (vo != Prefs::nViewObjects) prefs.setVisibleOffScreen(vo, c->argb(1));
		else return FALSE;
	}
	else
	{
		char shown[512], notshown[512];
		shown[0] = '\0';
		notshown[0] = '\0';
		strcat(shown,"Visible: ");
		strcat(notshown,"Hidden : ");
		msg.print( "Current on-image object status:\n");
		for (int i=0; i<Prefs::nViewObjects; i++)
		{
			if (prefs.isVisibleOffScreen( (Prefs::ViewObject) i))
			{
				strcat(shown, Prefs::viewObject( (Prefs::ViewObject) i));
				strcat(shown, " ");
			}
			else
			{
				strcat(notshown, Prefs::viewObject( (Prefs::ViewObject) i));
				strcat(notshown, " ");
			}
		}
		msg.print( "%s\n", shown);
		msg.print( "%s\n", notshown);
	}
	rv.reset();
	return TRUE;
}

// View Styles
bool Command::function_Style(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0))
	{
		Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
		if (ds != Atom::nDrawStyles)
		{
			prefs.setRenderStyle(ds);
			if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
			gui.mainView.postRedisplay();
		rv.reset();
		}
		else return FALSE;
	}
	rv.set(Atom::drawStyle(prefs.renderStyle()));
	return TRUE;
}

// Set whether to perform manual buffer swapping ('swapbuffers [on|off]')
bool Command::function_SwapBuffers(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setManualSwapBuffers(c->argb(0));
	rv.set(prefs.manualSwapBuffers());
	return TRUE;
}

// Set whether to use nice text rendering ('usenicetext on|off')
bool Command::function_UseNiceText(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setUseNiceText(c->argb(0));
	rv.set( prefs.useNiceText() );
	return TRUE;
}

// Set VDW cutoff ('vcut <cut>')
bool Command::function_VCut(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setVdwCutoff(c->argd(0));
	rv.set(prefs.vdwCutoff());
	return TRUE;
}

// Turn on/off calculation of vdw ('vdw on|off')
bool Command::function_Vdw(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setCalculateVdw(c->argb(0));
	rv.set(prefs.calculateVdw());
	return TRUE;
}

// Display or set zoom throttle ('zoomthrottle [ratio]')
bool Command::function_ZoomThrottle(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setZoomThrottle(c->argd(0));
	msg.print(Messenger::Verbose, "Zooming throttle is %f.\n", prefs.zoomThrottle());
	rv.set(prefs.zoomThrottle());
	return TRUE;
}

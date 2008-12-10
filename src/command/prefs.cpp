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
#include "main/aten.h"
#include "gui/gui.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/prefs.h"

// Angle label postfix
int Command::function_CA_ANGLELABEL(CommandNode *&c, Bundle &obj)
{
	prefs.setAngleLabel(c->argc(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Atom quadric detail
int Command::function_CA_ATOMDETAIL(CommandNode *&c, Bundle &obj)
{
	prefs.setAtomDetail(c->argi(0));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Bond quadric detail
int Command::function_CA_BONDDETAIL(CommandNode *&c, Bundle &obj)
{
	prefs.setBondDetail(c->argi(0));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Colours
int Command::function_CA_COLOUR(CommandNode *&c, Bundle &obj)
{
	Prefs::PenColour col = Prefs::penColour(c->argc(0));
	if (col == Prefs::nPenColours) return Command::Fail;
	Vec3<GLfloat> colvec = c->arg3f(1);
	GLfloat alpha = (c->hasArg(4) ? (GLfloat) c->argd(4) : 1.0f);
	prefs.setColour(col, colvec.x, colvec.y, colvec.z, alpha);
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Common elements list
int Command::function_CA_COMMONELEMENTS(CommandNode *&c, Bundle &obj)
{
	prefs.setCommonElements(c->argc(0));
	return Command::Success;
}

// Set density unit to use in output ('densityunits <unit>')
int Command::function_CA_DENSITYUNITS(CommandNode *&c, Bundle &obj)
{
	Prefs::DensityUnit du = Prefs::densityUnit(c->argc(0));
	if (du == Prefs::nDensityUnits) return Command::Fail;
	else prefs.setDensityUnits(du);
	return Command::Success;
}

// Distance label postfix
int Command::function_CA_DISTANCELABEL(CommandNode *&c, Bundle &obj)
{
	prefs.setDistanceLabel(c->argc(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set electrostatics cutoff ('ecut <cut>')
int Command::function_CA_ECUT(CommandNode *&c, Bundle &obj)
{
	prefs.setElecCutoff(c->argd(0));
	return Command::Success;
}

// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
int Command::function_CA_ELEC(CommandNode *&c, Bundle &obj)
{
	Electrostatics::ElecMethod em = Electrostatics::elecMethod(c->argc(0));
	if (em == Electrostatics::nElectrostatics) return Command::Fail;
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (Electrostatics::Ewald):
			if (!c->hasArg(4))
			{
				msg.print("Must supply the alpha parameter and kmax vectors to used this electrostatics option.\n");
				return Command::Fail;
			}
			prefs.setEwaldAlpha(c->argd(1));
			prefs.setEwaldKvec(c->arg3i(2));
			break;
		// Set ewald precision
		case (Electrostatics::EwaldAuto):
			if (!c->hasArg(1))
			{
				msg.print("Must supply the Ewald precision parameter to used this electrostatics option.\n");
				return Command::Fail;
			}
			prefs.setEwaldPrecision(c->argd(1));
			break;
	}
	// Set method
	prefs.setElectrostaticsMethod(em);
	prefs.setCalculateElec(em == Electrostatics::None ? FALSE : TRUE);
	return Command::Success;
}

// Set element's ambient colour
int Command::function_CA_ELEMENTAMBIENT(CommandNode *&c, Bundle &obj)
{
	int el = elements().find(c->argc(0));
	if (el == 0) return Command::Fail;
	elements().setAmbientColour(el,0,c->argi(1));
	elements().setAmbientColour(el,1,c->argi(2));
	elements().setAmbientColour(el,2,c->argi(3));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set element's diffuse colour
int Command::function_CA_ELEMENTDIFFUSE(CommandNode *&c, Bundle &obj)
{
	int el = elements().find(c->argc(0));
	if (el == 0) return Command::Fail;
	elements().setDiffuseColour(el,0,c->argi(1));
	elements().setDiffuseColour(el,1,c->argi(2));
	elements().setDiffuseColour(el,2,c->argi(3));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set element's radius
int Command::function_CA_ELEMENTRADIUS(CommandNode *&c, Bundle &obj)
{
	int el = elements().find(c->argc(0));
	if (el == 0) return Command::Fail;
	elements().setAtomicRadius(el, c->argd(1));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set energy unit to use in output ('energyunits <unit>')
int Command::function_CA_ENERGYUNITS(CommandNode *&c, Bundle &obj)
{
	Prefs::EnergyUnit eu = Prefs::energyUnit(c->argc(0));
	if (eu == Prefs::nEnergyUnits) return Command::Fail;
	else
	{
		prefs.setEnergyUnit(eu);
		// Convert loaded forcefields
		for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next) ff->convertParameters();
	}
	return Command::Success;
}

// GL Options
int Command::function_CA_GL(CommandNode *&c, Bundle &obj)
{
	Prefs::GlOption go = Prefs::glOption(c->argc(0));
	if (go == Prefs::nGlOptions) return Command::Fail;
	if (c->argb(1)) prefs.addGlOption(go);
	else prefs.removeGlOption(go);
	gui.mainView.initGl();
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set distance to use when adding hydrogens ('hdistance <d>')
int Command::function_CA_HDISTANCE(CommandNode *&c, Bundle &obj)
{
	prefs.setHydrogenDistance(c->argd(0));
	return Command::Success;
}

// Turn on/off calculation of intra ('intra on|off')
int Command::function_CA_INTRA(CommandNode *&c, Bundle &obj)
{
	prefs.setCalculateIntra(c->argb(0));
	return Command::Success;
}

// Key bindings
int Command::function_CA_KEY(CommandNode *&c, Bundle &obj)
{
	Prefs::ModifierKey mk = Prefs::modifierKey(c->argc(0));
	Prefs::KeyAction ka = Prefs::keyAction(c->argc(1));
	if ((mk != Prefs::nModifierKeys) && (ka != Prefs::nKeyActions)) prefs.setKeyAction(mk,ka);
	else return Command::Fail;
	return Command::Success;
}

// Text label pointsize
int Command::function_CA_LABELSIZE(CommandNode *&c, Bundle &obj)
{
	prefs.setLabelSize(c->argi(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Turn on/off spotlight
int Command::function_CA_LIGHT(CommandNode *&c, Bundle &obj)
{
	prefs.setSpotlightActive(c->argb(0));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set ambient component of spotlight
int Command::function_CA_LIGHTAMBIENT(CommandNode *&c, Bundle &obj)
{
	prefs.setSpotlightColour(Prefs::AmbientComponent, c->argf(0), c->argf(1), c->argf(2));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set diffuse component of spotlight
int Command::function_CA_LIGHTDIFFUSE(CommandNode *&c, Bundle &obj)
{
	prefs.setSpotlightColour(Prefs::DiffuseComponent, c->argf(0), c->argf(1), c->argf(2));
	gui.mainView.postRedisplay();
	return Command::Success;
}

int Command::function_CA_LIGHTPOSITION(CommandNode *&c, Bundle &obj)
{
	prefs.setSpotlightPosition(c->argf(0), c->argf(1), c->argf(2));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Set specular component of spotlight
int Command::function_CA_LIGHTSPECULAR(CommandNode *&c, Bundle &obj)
{
	prefs.setSpotlightColour(Prefs::SpecularComponent, c->argf(0), c->argf(1), c->argf(2));
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Mouse bindings
int Command::function_CA_MOUSE(CommandNode *&c, Bundle &obj)
{
	Prefs::MouseButton mb = Prefs::mouseButton(c->argc(0));
	Prefs::MouseAction ma = Prefs::mouseAction(c->argc(1));
	if ((ma != Prefs::nMouseActions) && (mb != Prefs::nMouseButtons)) prefs.setMouseAction(mb,ma);
	else return Command::Fail;
	return Command::Success;
}

// Atom screen radii
int Command::function_CA_RADIUS(CommandNode *&c, Bundle &obj)
{
	Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
	if (ds != Atom::nDrawStyles) prefs.setAtomStyleRadius(ds, c->argd(1));
	else return Command::Fail;
	return Command::Success;
}

// Set whether replicate folds atoms beforehand
int Command::function_CA_REPLICATEFOLD(CommandNode *&c, Bundle &obj)
{
	prefs.setReplicateFold(c->argb(0));
	return Command::Success;
}

// Set whether replicate trims atoms afterwards
int Command::function_CA_REPLICATETRIM(CommandNode *&c, Bundle &obj)
{
	prefs.setReplicateTrim(c->argb(0));
	return Command::Success;
}

// Colouring scheme
int Command::function_CA_SCHEME(CommandNode *&c, Bundle &obj)
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
		else return Command::Fail;
	}
	else msg.print( "Current atom colouring scheme is '%s'\n", Prefs::colouringScheme( prefs.colourScheme() ));
	return Command::Success;
}

// Atom shininess
int Command::function_CA_SHININESS(CommandNode *&c, Bundle &obj)
{
	prefs.setShininess(c->argi(0));
	if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
	return Command::Success;
}

// Render Objects on screen
int Command::function_CA_SHOWONSCREEN(CommandNode *&c, Bundle &obj)
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
		else return Command::Fail;
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
	return Command::Success;
}

// Render Objects on saved images
int Command::function_CA_SHOWONIMAGE(CommandNode *&c, Bundle &obj)
{
	if (c->hasArg(0))
	{
		Prefs::ViewObject vo = Prefs::viewObject(c->argc(0));
		if (vo != Prefs::nViewObjects) prefs.setVisibleOnImage(vo, c->argb(1));
		else return Command::Fail;
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
			if (prefs.isVisibleOnImage( (Prefs::ViewObject) i))
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
	return Command::Success;
}

// View Styles
int Command::function_CA_STYLE(CommandNode *&c, Bundle &obj)
{
	if (c->hasArg(0))
	{
		Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
		if (ds != Atom::nDrawStyles)
		{
			prefs.setRenderStyle(ds);
			if (obj.rs != NULL) obj.rs->changeLog.add(Log::Visual);
			gui.mainView.postRedisplay();
		}
		else return Command::Fail;
	}
	else msg.print( "Current model drawing style is '%s'\n", Atom::drawStyle(prefs.renderStyle()));
	return Command::Success;
}

// Set whether to perform manual buffer swapping ('swapbuffers [on|off]')
int Command::function_CA_SWAPBUFFERS(CommandNode *&c, Bundle &obj)
{
	if (c->hasArg(0)) prefs.setManualSwapBuffers(c->argb(0));
	else msg.print("Manual swapping of buffers is %s.\n", prefs.manualSwapBuffers() ? "on" : "off");
	return Command::Success;
}

// Set whether to use nice text rendering ('usenicetext on|off')
int Command::function_CA_USENICETEXT(CommandNode *&c, Bundle &obj)
{
	prefs.setUseNiceText(c->argb(0));
	return Command::Success;
}

// Set VDW cutoff ('vcut <cut>')
int Command::function_CA_VCUT(CommandNode *&c, Bundle &obj)
{
	prefs.setVdwCutoff(c->argd(0));
	return Command::Success;
}

// Turn on/off calculation of vdw ('vdw on|off')
int Command::function_CA_VDW(CommandNode *&c, Bundle &obj)
{
	prefs.setCalculateVdw(c->argb(0));
	return Command::Success;
}

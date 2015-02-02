/*
	*** User Actions Rendering
	*** src/render/engine_useractions.cpp
	Copyright T. Youngs 2007-2015

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

#include "main/aten.h"
#include "render/engine.h"
#include "model/model.h"
#include "model/fragment.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "gui/fragments.h"
#include "base/sysfunc.h"

// Render active mode embellishments
void RenderEngine::renderActiveModes(QPainter& painter, int width, int height)
{
	// Variables
	Dnchar text;
	QColor color;
	QBrush nobrush(Qt::NoBrush);
	GLfloat colour[4];
	Vec3<double> r, mouseLast, mouseDown;
	int i, skip, n;
	double dx, halfw;

	// Active mode embellishments
	prefs.copyColour(Prefs::BackgroundColour, colour);
	color.setRgbF(1.0-colour[0], 1.0-colour[1], 1.0-colour[2], 1.0);
	painter.setPen(color);
	painter.setPen(Qt::DashLine);
	painter.setBrush(nobrush);
	switch (gui.mainCanvas()->activeMode())
	{
		case (UserAction::NoAction):
			break;
		// Only selection mode where we draw a selection box
		case (UserAction::SelectAction):
			mouseLast = gui.mainCanvas()->rMouseLast();
			mouseDown = gui.mainCanvas()->rMouseDown();
			painter.drawRect(mouseDown.x, mouseDown.y, mouseLast.x-mouseDown.x, mouseLast.y-mouseDown.y);
			break;
		default:
			break;
	}

	// Passive mode embellishments
	Model *modelTarget = aten.currentModel();
	if (modelTarget != NULL) switch (gui.mainCanvas()->selectedMode())
	{
		// Draw on distance ruler for drawing modes
		case (UserAction::DrawAtomAction):
		case (UserAction::DrawChainAction):
			// Get pixel 'length' in Angstrom terms at current draw depth
			r = modelTarget->screenToModel(width/2+10, contextHeight_/2, gui.mainCanvas()->currentDrawDepth());
			r -= modelTarget->screenToModel(width/2, contextHeight_/2, gui.mainCanvas()->currentDrawDepth());
			dx = 10.0 / r.magnitude();
			
			halfw = width / 2.0;
			i = int( halfw / dx);
			skip = 1;
			while ( (i/skip) > 5)
			{
				skip += (skip == 1 ? 4 : 5);
			}
			for (n = -i; n <= i; n ++)
			{
				if ((n%skip) != 0) continue;
				painter.drawLine(halfw + n*dx, 20, halfw + n*dx, 10);
				painter.drawLine(halfw + n*dx, contextHeight_-20, halfw + n*dx, contextHeight_-10);
				if (n != i)
				{
					painter.drawLine(halfw + (n+0.5*skip)*dx, contextHeight_-15, halfw + (n+0.5*skip)*dx, contextHeight_-10);
					painter.drawLine(halfw + (n+0.5*skip)*dx, contextHeight_-15, halfw + (n+0.5*skip)*dx, contextHeight_-10);
				}
			}
			painter.drawLine(halfw - i*dx, 10, halfw + i*dx, 10);
			painter.drawLine(halfw - i*dx, contextHeight_-10, halfw + i*dx, contextHeight_-10);
			for (n = -i; n <= i; n++)
			{
				if ((n%skip) != 0) continue;
				painter.drawText(halfw + n*dx - (n < 0 ? 8 : 3), contextHeight_, itoa(n));
			}
			break;
		default:
			break;
	}
}

// Render addition elements related to selected/active UserActions
void RenderEngine::renderUserActions(Model *source)
{
	Matrix A;
	Atom *i, *j, tempj;
	GLfloat colour[4], colour_j[4];
	Atom::DrawStyle style_i, style_j;
	Vec3<double> pos, rmouse, v;
	Bond::BondType bt = Bond::Single;
	double radius_i, radius_j;
	Dnchar text;
	Fragment *frag;

	if (!gui.exists()) return;
	TCanvas *canvas = gui.mainCanvas();

	// Draw on the selection highlights (for atoms in the canvas' pickedAtoms list)
	for (Refitem<Atom,int> *ri = canvas->pickedAtoms(); ri != NULL; ri = ri->next)
	{
		i = ri->item;
		// Move to local atom position
		A.setIdentity();
		A.applyTranslation(i->r());
		prefs.copyColour(Prefs::TextColour, colour);
		// Draw a wireframe sphere at the atoms position
		style_i = (prefs.renderStyle() == Atom::IndividualStyle ? i->style() : prefs.renderStyle());
		radius_i = prefs.atomStyleRadius(style_i);
		if (style_i == Atom::ScaledStyle) radius_i *= Elements().el[i->element()].atomicRadius;
		A.applyScaling(radius_i, radius_i, radius_i);
		renderPrimitive(RenderEngine::GuiObject, primitives_[set_].selectedAtom_, colour, A, GL_LINE);
	}

	// Active user actions
	i = canvas->atomClicked();
	switch (canvas->activeMode())
	{
		// Draw on bond and new atom for chain drawing (if mode is active)
		case (UserAction::DrawBondSingleAction):
		case (UserAction::DrawBondDoubleAction):
		case (UserAction::DrawBondTripleAction):
			if (i == NULL) i = (gui.mainCanvas()->pickedAtoms() != NULL ? gui.mainCanvas()->pickedAtoms()->item : NULL);
			bt = (Bond::BondType) (1+canvas->activeMode()-UserAction::DrawBondSingleAction);
		case (UserAction::DrawChainAction):
			if (i == NULL) break;
			pos = i->r();
			rmouse = canvas->rMouseLast();
			style_i = (prefs.renderStyle() == Atom::IndividualStyle ? i->style() : prefs.renderStyle());
			if (style_i == Atom::TubeStyle) radius_i = 0.0;
			else if (style_i == Atom::ScaledStyle) radius_i = prefs.styleRadius(i) - scaledAtomAdjustments_[i->element()];
			else radius_i = prefs.styleRadius(i) - sphereAtomAdjustment_;

			// We need to project a point from the mouse position onto the canvas plane, unless the mouse is over an existing atom in which case we snap to its position instead
			j = source->atomOnScreen(rmouse.x, rmouse.y);
			if (j == NULL)
			{
				j = &tempj;
				v = source->screenToModel(rmouse.x, rmouse.y, canvas->currentDrawDepth());
				j->r() = v;
				style_j = (prefs.renderStyle() == Atom::IndividualStyle ? Atom::StickStyle : prefs.renderStyle());
				j->setStyle(style_j);
			}
			else
			{
				v = j->r();
				style_j = (prefs.renderStyle() == Atom::IndividualStyle ? j->style() : prefs.renderStyle());
			}
			if (style_j == Atom::TubeStyle) radius_j = 0.0;
			else if (style_j == Atom::ScaledStyle) radius_j = prefs.styleRadius(j) - scaledAtomAdjustments_[canvas->sketchElement()];
			else radius_j = prefs.styleRadius(j) - sphereAtomAdjustment_;
			v -= pos;

			// Select colour
			if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour);
			else switch (prefs.colourScheme())
			{
				case (Prefs::ElementScheme):
					Elements().copyColour(i->element(), colour);
					break;
				case (Prefs::ChargeScheme):
					prefs.colourScale[0].colour(i->charge(), colour);
					break;
				case (Prefs::VelocityScheme):
					prefs.colourScale[1].colour(i->v().magnitude(), colour);
					break;
				case (Prefs::ForceScheme):
					prefs.colourScale[2].colour(i->f().magnitude(), colour);
					break;
				case (Prefs::CustomScheme):
					i->copyColour(colour);
					break;
				default:
					break;
			}
			Elements().copyColour(canvas->sketchElement(), colour_j);
			
			// Construct transformation matrix to centre on original (first) atom
			A.setIdentity();
			A.applyTranslation(pos);
			
			// Render new (temporary) bond
			renderBond(GuiObject, GuiObject, A, v, i, style_i, colour, radius_i, j, style_j, colour_j, radius_j, bt, prefs.selectionScale(), NULL, false, colour);
			
			// Draw text showing distance
			text.sprintf("r = %f ", v.magnitude());
			renderTextPrimitive(rmouse.x, canvas->contextHeight()-rmouse.y, text.get(), 0x212b);
			break;
	}
	
	// Selected user mode actions
	i = canvas->atomClicked();
	switch (canvas->selectedMode())
	{
		// Draw on fragment (as long as mode is selected)
		case (UserAction::DrawFragmentAction):
			if (gui.fragmentsWidget->currentFragment() == NULL) break;
			frag = gui.fragmentsWidget->currentFragment();
			j = source->atomOnScreen(canvas->rMouseLast().x, canvas->rMouseLast().y);
			if ((i != NULL) || (j != NULL))
			{
				// Atom is now fragment anchor point - make sure we select a non-null atom i or j
				if (i != NULL) j = i;
				pos = j->r();
				Model *m = frag->anchoredModel(j, canvas->keyModifier(Prefs::ShiftKey), gui.fragmentsWidget->bondId());

				A.setIdentity();
				A.applyTranslation(pos);
				// Did we find a valid anchor point?
				if (m != NULL) renderAtomsAndBonds(m, A, TRUE);
				else
				{
					prefs.copyColour(Prefs::TextColour, colour);
					renderPrimitive(RenderEngine::GuiObject, &primitives_[set_].crossedCube_, FALSE, colour, A, GL_LINE, 2.0);
				}
			}
			else
			{
				// No atom under the mouse pointer, so draw on at the prefs drawing depth in its current orientation
				// Get drawing point origin, translate to it, and render the stored model
				if (canvas->activeMode() == UserAction::DrawFragmentAction) pos = source->screenToModel(canvas->rMouseDown().x, canvas->rMouseDown().y, prefs.drawDepth());
				else pos = source->screenToModel(canvas->rMouseLast().x, canvas->rMouseLast().y, prefs.drawDepth());
				A.setIdentity();
				A.applyTranslation(pos);
				renderAtomsAndBonds(frag->orientedModel(), A, TRUE);
			}
			break;
	}
}

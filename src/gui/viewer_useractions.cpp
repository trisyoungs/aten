/*
	*** User Actions Rendering
	*** src/gui/viewer_useractions.cpp
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
#include "gui/viewer.hui"
#include "model/model.h"
#include "model/fragment.h"
#include "base/sysfunc.h"
#include "gui/mainwindow.h"
#include <QPainter>

// Render active mode embellishments
void Viewer::renderActiveModes(Model* currentModel)
{
	// Variables
	QColor color;
	QBrush nobrush(Qt::NoBrush);
	GLfloat colour[4];
	Vec3<double> r, mouseLast, mouseDown;
	int i, skip, n;
	double dx, halfw;

	QPainter painter(this);
	
	// Active mode embellishments
	prefs.copyColour(Prefs::BackgroundColour, colour);
	color.setRgbF(1.0-colour[0], 1.0-colour[1], 1.0-colour[2], 1.0);
	painter.setPen(color);
	painter.setPen(Qt::DashLine);
	painter.setBrush(nobrush);
	switch (activeMode_)
	{
		case (UserAction::NoAction):
			break;
		// Only selection mode where we draw a selection box
		case (UserAction::SelectAction):
			painter.drawRect(rMouseDown_.x, rMouseDown_.y, rMouseLast_.x-rMouseDown_.x, rMouseLast_.y-rMouseDown_.y);
			break;
		default:
			break;
	}

	// Passive mode embellishments
	if (currentModel != NULL) switch (selectedMode_)
	{
		// Draw on distance ruler for drawing modes //ATEN2 TODO
		case (UserAction::DrawAtomsAction):
			// Get pixel 'length' in Angstrom terms at current draw depth
			r = currentModel->screenToModel(contextWidth_/2+10, contextHeight_/2, currentDrawDepth_);
			r -= currentModel->screenToModel(contextWidth_/2, contextHeight_/2, currentDrawDepth_);
			dx = 10.0 / r.magnitude();
			
			halfw = contextWidth_ / 2.0;
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
				painter.drawText(halfw + n*dx - (n < 0 ? 8 : 3), contextHeight_, QString::number(n));
			}
			break;
		default:
			break;
	}
}

// Render addition elements related to selected/active UserActions
void Viewer::renderUserActions(Model* source)
{
	Matrix A;
	Atom* j, tempj;
	Vec4<GLfloat> colour_i, colour_j;
	Prefs::DrawStyle style_i, style_j;
	Vec3<double> pos, v;
	Bond::BondType bt = Bond::Single;
	double radius_i, radius_j;
	QString text;

	// Draw on the selection highlights (for atoms in the canvas' pickedAtoms list)
	prefs.copyColour(Prefs::TextColour, colour_i);
	glColor4f(colour_i.x, colour_i.y, colour_i.z, colour_i.w);
	for (RefListItem<Atom,int>* ri = pickedAtoms_.first(); ri != NULL; ri = ri->next)
	{
		// Get Atom pointer
		Atom* i = ri->item;

		// Set a matrix to move to the local atom position
		A.createTranslation(i->r());

		// Draw a wireframe sphere at the atoms position
		style_i = (prefs.renderStyle() == Prefs::OwnStyle ? i->style() : prefs.renderStyle());
		radius_i = prefs.atomStyleRadius(style_i);
		if (style_i == Prefs::ScaledStyle) radius_i *= Elements().atomicRadius(i->element());
		A.applyScaling(radius_i, radius_i, radius_i);
		A *= modelTransformationMatrix_;
		glLoadMatrixd(A.matrix());
// 		renderGroup_.addLines(); primitives_[primitiveSet_].selectedAtom(); ATEN2 TODO Do this as a set of arrows pointing at the atom?
	}

	// Active user actions
	switch (activeMode_)
	{
		// Draw on bond and new atom for chain drawing (if mode is active)
		case (UserAction::DrawAtomsAction):
			if (atomClicked_ == NULL) break;
			pos = atomClicked_->r();
			style_i = (prefs.renderStyle() == Prefs::OwnStyle ? atomClicked_->style() : prefs.renderStyle());
			if (style_i == Prefs::TubeStyle) radius_i = 0.0;
			else if (style_i == Prefs::ScaledStyle) radius_i = prefs.styleRadius(Prefs::ScaledStyle, atomClicked_->element()) - primitives_[primitiveSet_].scaledAtomAdjustment(atomClicked_->element());
			else radius_i = prefs.styleRadius(style_i, atomClicked_->element()) - primitives_[primitiveSet_].sphereAtomAdjustment();

			// We need to project a point from the mouse position onto the canvas plane, unless the mouse is over an existing atom in which case we snap to its position instead
			j = source->atomOnScreen(rMouseLast_.x, rMouseLast_.y);
			if (j == NULL)
			{
				j = &tempj;
				v = source->screenToModel(rMouseLast_.x, rMouseLast_.y, currentDrawDepth_);
				j->r() = v;
				style_j = (prefs.renderStyle() == Prefs::OwnStyle ? Prefs::LineStyle : prefs.renderStyle());
				j->setStyle(style_j);
			}
			else
			{
				v = j->r();
				style_j = (prefs.renderStyle() == Prefs::OwnStyle ? j->style() : prefs.renderStyle());
			}
			if (style_j == Prefs::TubeStyle) radius_j = 0.0;
			else if (style_j == Prefs::ScaledStyle) radius_j = prefs.styleRadius(Prefs::ScaledStyle, j->element()) - primitives_[primitiveSet_].scaledAtomAdjustment(atenWindow_->currentBuildElement());
			else radius_j = prefs.styleRadius(style_j, j->element()) - primitives_[primitiveSet_].sphereAtomAdjustment();
			v -= pos;

			// Select colour
			if (atomClicked_->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_i);
			else switch (prefs.colourScheme())
			{
				case (Prefs::ElementScheme):
					Elements().copyColour(atomClicked_->element(), colour_i);
					break;
				case (Prefs::ChargeScheme):
					prefs.colourScale[0].colour(atomClicked_->charge(), colour_i);
					break;
				case (Prefs::VelocityScheme):
					prefs.colourScale[1].colour(atomClicked_->v().magnitude(), colour_i);
					break;
				case (Prefs::ForceScheme):
					prefs.colourScale[2].colour(atomClicked_->f().magnitude(), colour_i);
					break;
				case (Prefs::OwnScheme):
					atomClicked_->copyColour(colour_i);
					break;
				default:
					break;
			}
			Elements().copyColour(atenWindow_->currentBuildElement(), colour_j);
			
			// Construct transformation matrix to centre on original (first) atom
			A.createTranslation(pos);
			
			// Render new (temporary) bond
			// ATEN2 TODO Add a RenderGroup to Viewer, in order to store the bond primitive?
// 			renderBond(GuiObject, GuiObject, A, v, atomClicked_, style_i, colour_i, radius_i, j, style_j, colour_j, radius_j, bt, prefs.selectionScale(), NULL, false);
			
			// Draw text showing distance
			text.sprintf("r = %f ", v.magnitude());
// 			renderTextPrimitive(rMouseLast_.x, contextHeight_-rMouseLast_.y, text.get(), 0x212b);
			break;
	}
	
	// Selected user mode actions
	switch (selectedMode_)
	{
		// Draw on fragment (as long as mode is selected)
		case (UserAction::DrawFragmentsAction):
			if (!aten_->currentFragment()) break;
			j = source->atomOnScreen(rMouseLast_.x, rMouseLast_.y);
			if ((atomClicked_ != NULL) || (j != NULL))
			{
				// Atom is now fragment anchor point - make sure we select a non-null atom i or j
				if (atomClicked_ != NULL) j = atomClicked_;
				pos = j->r();
				Model* m = aten_->currentFragment()->anchoredModel(j, keyModifier(Prefs::ShiftKey), aten_->fragmentBondId());
				A.createTranslation(pos);

				// Did we find a valid anchor point?
				if (m != NULL) renderGroup_.createAtomsAndBonds(primitives_[primitiveSet_], m, A);
				else
				{
					prefs.copyColour(Prefs::TextColour, colour_i);
					renderGroup_.addLines(primitives_[primitiveSet_].crossedCube(), A, colour_i, true);
				}
			}
			else
			{
				// No atom under the mouse pointer, so draw on at the prefs drawing depth in its current orientation
				// Get drawing point origin, translate to it, and render the stored model
				if (activeMode_ == UserAction::DrawFragmentsAction) pos = source->screenToModel(rMouseDown_.x, rMouseDown_.y, prefs.drawDepth());
				else pos = source->screenToModel(rMouseLast_.x, rMouseLast_.y, prefs.drawDepth());
				A.createTranslation(pos);
				renderGroup_.createAtomsAndBonds(primitives_[primitiveSet_], aten_->currentFragment()->orientedModel(), A);
			}
			break;
	}
}

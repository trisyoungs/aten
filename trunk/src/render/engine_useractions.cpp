/*
	*** User Actions Rendering
	*** src/render/engine_useractions.cpp
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

#include "render/engine.h"
#include "model/model.h"
#include "model/fragment.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "gui/fragment.h"

// Render addition elements related to selected/active UserActions
void RenderEngine::renderUserActions(Model *source, Matrix baseTransform, TCanvas *canvas)
{
	Matrix A;
	Atom *i, *j, tempj;
	GLfloat colour[4], colour_j[4];
	Atom::DrawStyle style_i, style_j;
	Vec3<double> pos, rmouse, v;
	double radius_i, radius_j;
	Dnchar text;
	Fragment *frag;

	// Draw on the selection highlights (for atoms in canvas.subsel)
	for (Refitem<Atom,int> *ri = canvas->pickedAtoms(); ri != NULL; ri = ri->next)
	{
		i = ri->item;
		// Move to local atom position
		A = modelTransformationMatrix_;
		A.applyTranslation(i->r());
		prefs.copyColour(Prefs::TextColour, colour);
		// Draw a wireframe sphere at the atoms position
		style_i = (prefs.renderStyle() == Atom::IndividualStyle ? i->style() : prefs.renderStyle());
		switch (style_i)
		{
			case (Atom::StickStyle):
			case (Atom::TubeStyle):
				renderPrimitive(selectedAtoms_[Atom::TubeStyle], 0, colour, A, GL_LINE);
				break;
			case (Atom::SphereStyle):
				renderPrimitive(selectedAtoms_[Atom::SphereStyle], 0, colour, A, GL_LINE);
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(selectedScaledAtoms_[i->element()], 0, colour, A, GL_LINE);
				break;
		}
	}

	
	// Active user actions
	i = canvas->atomClicked();
	switch (canvas->activeMode())
	{
		// Draw on bond and new atom for chain drawing (if mode is active)
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
				v = screenToModel(rmouse.x, rmouse.y, canvas->currentDrawDepth());
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
					elements().copyColour(i->element(), colour);
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
			elements().copyColour(canvas->sketchElement(), colour_j);
			
			// Construct transformation matrix to centre on original (first) atom
			A = modelTransformationMatrix_;
			A.applyTranslation(pos);
			
			// Render new (temporary) bond
			renderBond(A, v, i, style_i, colour, radius_i, j, style_j, colour_j, radius_j, Bond::Single, 0, prefs.selectionScale());
			
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
			if (gui.fragmentWindow->currentFragment() == NULL) break;
			frag = gui.fragmentWindow->currentFragment();
			j = source->atomOnScreen(canvas->rMouseLast().x, canvas->rMouseLast().y);
			if ((i != NULL) || (j != NULL))
			{
				// Atom is now fragment anchor point - make sure we select a non-null atom i or j
				if (i != NULL) j = i;
				pos = j->r();
				Model *m = frag->anchoredModel(j, canvas->keyModifier(Prefs::ShiftKey), gui.fragmentWindow->bondId());

				A = modelTransformationMatrix_;
				A.applyTranslation(pos);	
				// Did we find a valid anchor point?
				if (m != NULL) renderModel(m, A, canvas);
				else
				{
					prefs.copyColour(Prefs::TextColour, colour);
					renderPrimitive(&crossedCube_, FALSE, colour, A, GL_LINE, 2.0);
				}
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				// Get drawing point origin, translate to it, and render the stored model
				if (canvas->activeMode() == UserAction::DrawFragmentAction) pos = screenToModel(canvas->rMouseDown().x, canvas->rMouseDown().y, prefs.drawDepth());
				else pos = screenToModel(canvas->rMouseLast().x, canvas->rMouseLast().y, prefs.drawDepth());
				A = modelTransformationMatrix_;
				A.applyTranslation(pos);
				renderModel(frag->orientedModel(), A, canvas);
			}
			break;
	}

	// All 3D primitive objects have now been filtered, so add triangles, then sort and send to GL
	renderPrimitive(&glyphTriangles_[RenderEngine::SolidTriangle], FALSE, NULL, modelTransformationMatrix_);
	renderPrimitive(&glyphTriangles_[RenderEngine::WireTriangle], FALSE, NULL, modelTransformationMatrix_, GL_LINE);
	renderPrimitive(&glyphTriangles_[RenderEngine::TransparentTriangle], TRUE, NULL, modelTransformationMatrix_);
	sortAndSendGL();
	
	// Render overlays
	renderModelOverlays(source, modelTransformationMatrix_, canvas);
}

/*
	*** Overlay Rendering
	*** src/render/rendergroup_overlay.cpp
	Copyright T. Youngs 2007-2017

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

#include "render/rendergroup.h"
#include "render/primitiveset.h"
#include "model/model.h"
#include "base/forcefieldatom.h"

ATEN_USING_NAMESPACE

// Render measurements
void RenderGroup::createOverlays(Model* source, Matrix baseTransform)
{
	Messenger::enter("RenderGroup::createOverlays");

	Vec3<double> r1, r2, r3, r4, rji, rjk, pos;
	Vec4<double> screenr;
	Vec4<GLfloat> colour;
	double gamma, t, atomSize;
	int labels;
	Atom** atoms, *i;
	ForcefieldAtom* ffa;
	QStringList labelStrings;
	Prefs::DrawStyle style, drawStyle = source->drawStyle();
	prefs.copyColour(prefs.currentForegroundColour(), colour);

	// Atom labels
	atoms = source->atomArray();
	for (int n = 0; n<source->nAtoms(); ++n)
	{
		// Get atom pointer
		i = atoms[n];
		
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Labels
		labels = i->labels();
		if (labels == 0) continue;
		
		// Grab forcefield atom pointer
		ffa = i->type();

		// Now add on all parts of the label that are required
		labelStrings.clear();
		if (labels&(1 << Atom::IdLabel)) labelStrings << QString::number(i->id()+1);
		if (labels&(1 << Atom::ElementLabel)) labelStrings << QString(ElementMap::symbol(i));
		if (labels&(1 << Atom::TypeLabel))
		{
			if (ffa == NULL) labelStrings << "[None]";
			else labelStrings << QString("[%1 %2] ").arg(ffa->typeId()).arg(ffa->name());
		}
		if (labels&(1 << Atom::EquivalentLabel)) labelStrings << QString("[=%1]").arg(ffa == NULL ? "None" : ffa->equivalent());
		if (labels&(1 << Atom::ChargeLabel)) labelStrings << QString().sprintf(qPrintable(prefs.chargeLabelFormat()), i->charge());

		// Add text object
		style = (drawStyle == Prefs::OwnStyle ? i->style() : drawStyle);
		atomSize = (style == Prefs::ScaledStyle ? prefs.atomStyleRadius(style) * ElementMap::atomicRadius(i->element()) : prefs.atomStyleRadius(style)) * 1.05;
		addText(labelStrings.join(" "), i->r(), 0.25, TextPrimitive::CentralAnchor, Vec3<double>(0.0, 0.0, atomSize), true);
	}

	// Distance Measurements
	for (Measurement* m = source->distanceMeasurements(); m != NULL; m = m->next)
	{
		atoms = m->atoms();

		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden()) continue;
		r1 = atoms[0]->r();
		r2 = atoms[1]->r();

		// Add line and text
		overlayLines_.line(r1, r2, colour);
		addOverlayText(QString().sprintf(qPrintable(prefs.distanceLabelFormat()), m->value()) + QChar(0x212b), (r1+r2)*0.5, 0.1, TextPrimitive::MiddleLeftAnchor);
	}
	
	// Angle Measurements
	for (Measurement* m = source->angleMeasurements(); m != NULL; m = m->next)
	{
		atoms = m->atoms();

		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden()) continue;
		r1 = atoms[0]->r();
		r2 = atoms[1]->r();
		r3 = atoms[2]->r();

		// Add lines and text
		overlayLines_.line(r1, r2, colour);
		overlayLines_.line(r2, r3, colour);

		// Curved angle marker
		rji = (r1 - r2);
		rjk = (r3 - r2);
		rji.normalise();
		rjk.normalise();
		gamma = acos(rji.dp(rjk));

		// -- Draw segments
		t = 0.0;
		for (int n=0; n<11; ++n)
		{
			pos = (rji * sin((1.0-t)*gamma) + rjk * sin(t*gamma)) / sin(gamma);
			pos *= 0.2;
			pos += r2;
			overlayLines_.defineVertex(pos.x, pos.y, pos.z, 0.0, 0.0, 1.0, colour);
			t += 0.1;

			// Add text
			if (n == 5)
			{
				addOverlayText(QString().sprintf(qPrintable(prefs.angleLabelFormat()), m->value()) + QChar(176), pos, 0.1, TextPrimitive::MiddleLeftAnchor);
			}
		}
	}
	
	// Torsion Measurements
	for (Measurement* m = source->torsionMeasurements(); m != NULL; m = m->next)
	{
		atoms = m->atoms();
		// Check that all atoms involved in the measurement are visible (i.e. not hidden)
		if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden() || atoms[3]->isHidden()) continue;
		r1 = atoms[0]->r();
		r2 = atoms[1]->r();
		r3 = atoms[2]->r();
		r4 = atoms[3]->r();

		overlayLines_.line(r1, r2);
		overlayLines_.line(r2, r3);
		overlayLines_.line(r3, r4);

		addOverlayText(QString().sprintf(qPrintable(prefs.angleLabelFormat()), m->value()) + QChar(176), (r2+r3)*0.5, 0.1, TextPrimitive::MiddleLeftAnchor);
	}

	Messenger::exit("RenderGroup::createOverlays");
}

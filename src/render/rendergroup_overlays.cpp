/*
	*** Overlay Rendering
	*** src/render/rendergroup_overlay.cpp
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

#include "render/rendergroup.h"
#include "render/primitiveset.h"
#include "model/model.h"
#include "base/forcefieldatom.h"

ATEN_USING_NAMESPACE

// Render measurements
void RenderGroup::createOverlays(Model* source, Matrix baseTransform)
{
	Messenger::enter("Viewer::createOverlays");

	Vec3<double> r1, r2, r3, r4, rji, rjk, pos;
	Vec4<double> screenr;
	GLfloat colour[4];
	double gamma, t;
	int labels;
	QString text;
	Atom** atoms, *i;
	ForcefieldAtom* ffa;

	// Atoms and Bonds
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
		
		// Blank label string
		text.clear();

		// Now add on all parts of the label that are required
		if (labels&(1 << Atom::IdLabel)) text += QString::number(i->id()+1) + " ";
		if (labels&(1 << Atom::ElementLabel)) text += QString(Elements().symbol(i)) + " ";
		if (labels&(1 << Atom::TypeLabel))
		{
			if (ffa == NULL) text += "[None] ";
			else text += QString("[%1 %2] ").arg(ffa->typeId()).arg(ffa->name());
		}
		if (labels&(1 << Atom::EquivalentLabel)) text += QString("[=%1] ").arg(ffa == NULL ? "None" : ffa->equivalent());
		if (labels&(1 << Atom::ChargeLabel)) text += QString().sprintf(qPrintable(prefs.chargeLabelFormat()), i->charge());
		
		// Add text object
		addOverlayText(i->r(), text);
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
		overlayLines_.line(r1, r2);
		text.sprintf(qPrintable(prefs.distanceLabelFormat()), m->value()) += QChar(0x212b);
		addOverlayText((r1+r2)*0.5, text);
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
		overlayLines_.line(r1, r2);
		overlayLines_.line(r2, r3);

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
			overlayLines_.defineVertex(pos.x, pos.y, pos.z, 0.0, 0.0, 1.0);
			t += 0.1;

			// Add text
			if (n == 5)
			{
				text.sprintf(qPrintable(prefs.angleLabelFormat()), m->value()) += QChar(176);
				addOverlayText(pos, text);
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
		
		text.sprintf(qPrintable(prefs.angleLabelFormat()), m->value()) += QChar(176);
		addOverlayText((r2+r3)*0.5, text);
	}

	Messenger::exit("Viewer::createOverlays");
}

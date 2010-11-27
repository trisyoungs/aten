/*
	*** Qt canvas 2D render functions
	*** src/gui/tcanvas_render.cpp
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

#include "gui/tcanvas.uih"
#include "model/model.h"
#include "base/sysfunc.h"

// Draw 2D objects with QPainted
void TCanvas::render2D(QPainter &painter)
{
	// Variables
	static Dnchar text;
	GLfloat colour[4];
	Vec4<double> screenr;
	int i, labels, skip, n;
	ForcefieldAtom *ffa;
	double dx, halfw;

	// Setup brush and pen colour
	prefs.copyColour(Prefs::ForegroundColour, colour);
	QColor color( colour[0]*255, colour[1]*255, colour[2]*255, colour[3]*255);
	QBrush solidbrush(color, Qt::SolidPattern), nobrush(color, Qt::NoBrush);
	QPen solidpen(Qt::SolidLine), dashedpen(Qt::DashLine);

	// Text Primitives
	painter.setBrush(solidbrush);
	painter.setPen(solidpen);
	engine_.renderText(painter, this);

	// Active mode embellishments
	painter.setPen(dashedpen);
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
	painter.setPen(solidpen);
	switch (selectedMode_)
	{
		// Draw on distance ruler for drawing modes
		case (UserAction::DrawAtomAction):
		case (UserAction::DrawChainAction):
			// Get angstrom length
			dx = 1.0 / displayModel_->drawPixelWidth(currentDrawDepth_);	// BROKEN in periodic systems
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
				renderText(halfw + n*dx - (n < 0 ? 8 : 3), contextHeight_, itoa(n));
			}
			break;
		default:
			break;
	}
}


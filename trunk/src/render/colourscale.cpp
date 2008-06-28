/*
	*** Colourscale rendering
	*** src/render/colourscale.cpp
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

#include "classes/colourscale.h"
#include "gui/canvas.h"

// Render colourscales
void Canvas::renderColourscales()
{
	msg.enter("Canvas::renderColourscales");
	GLfloat col[4];

	// For now, draw in a stack at the bottom of the canvas.
	double leftx, x, y, width, height = 20.0, start, scalestart;
	double range, deltaw, cstep;
	char label[128];
	int halfslices = 5;
	width = width_ * 0.6;
	leftx = (width_ - width) * 0.50;
	y = 5.0;

	// Loop over colourscales....
	for (int n=0; n<10; n++)
	{
		if (!prefs.colourScale[n].visible()) continue;
		scalestart = 0.0;
		range = 0.0;
		// Special cases: 0 or 1 points in scale...
		if (prefs.colourScale[n].nPoints() == 0)
		{
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			glColor3f(0.0f, 0.0f, 0.0f);
			glText(leftx+10.0, height_-y-8.0, "< No points defined >");
		}
		else if (prefs.colourScale[n].nPoints() == 1)
		{
			prefs.colourScale[n].firstPoint()->copyColour(col);
			scalestart = prefs.colourScale[n].firstPoint()->value();
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glBegin(GL_QUADS);
			  glColor3f(col[0], col[1], col[2]);
			  glVertex2d(leftx,y);
			  glVertex2d(leftx,y+height);
			  glVertex2d(leftx+width,y+height);
			  glVertex2d(leftx+width,y);
			glEnd();
		}
		else
		{
			// Get the total range of the colourscale
			scalestart = prefs.colourScale[n].firstPoint()->value();
			range = prefs.colourScale[n].lastPoint()->value() - scalestart;
			// Use the delta list to draw the quads
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glBegin(GL_QUAD_STRIP);
			  for (ColourScaleDelta *delta = prefs.colourScale[n].firstDelta(); delta != NULL; delta = delta->next)
			  {
				// Grab width (delta) and start value of
				start = delta->start();
				deltaw = delta->delta();
				// Get start and end colours
				delta->colour(start, col);
				// Get starting x coordinate for 
				x = leftx + width * ( (start - scalestart) / range );
				// Draw quad
				glColor3f(col[0], col[1], col[2]);
				glVertex2d(x,y);
				glVertex2d(x,y+height);
				// If last delta, finish final quad
				if (delta->next == NULL)
				{
					delta->colour(start + deltaw, col);
					x = leftx + width;
					glColor3f(col[0], col[1], col[2]);
					glVertex2d(x,y);
					glVertex2d(x,y+height);
				}
			  }
			glEnd();
		}
		// Draw a black box surrounding the scalebar
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
		glColor3f(0.0f, 0.0f, 0.0f);
		glBegin(GL_LINE_LOOP);
		  glVertex2d(leftx,y);
		  glVertex2d(leftx, y+height);
		  glVertex2d(leftx+width, y+height);
		  glVertex2d(leftx+width,y);
		glEnd();
		// Add on text labels
		sprintf(label,"%s [%f : %f]", prefs.colourScale[n].name(), scalestart, scalestart + range);
		glText(leftx+width+2.0, height_-y-8.0, label);
		y += height + 10.0;
	}
	msg.exit("Canvas::renderColourscales");
}


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
	dbgBegin(Debug::Calls,"Canvas::renderColourscales");
	GLfloat col[4] = { 0.0f, 0.0f, 0.9f, 0.5f };

	// For now, draw in a stack at the bottom of the canvas.
	double x, y, width, height = 20.0, qwidth, x2;
	double cmin, cmax, cmid, cstep;
	char label[128];
	int halfslices = 5;
	width = width_ * 0.6;
	x = (width_ - width) * 0.25;
	y = 5.0;

	// Loop over colourscales....
	for (int n=0; n<10; n++)
	{
		if (!prefs.colourScale[n].visible()) continue;
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
// 		cmin = prefs.colourScale[n].minimum();
// 		cmax = prefs.colourScale[n].maximum();
// 		cmid = prefs.colourScale[n].middle();
		// Draw QUADS to get the gradient we want.
// 		glBegin(GL_QUAD_STRIP);
// 		  if (prefs.colourScale[n].type() == ColourScale::TwoPoint)
// 		  {
// 			cstep = (cmax - cmin) / (halfslices*2);
// 			qwidth = width / (halfslices*2);
// 			for (int i=0; i<=halfslices*2; i++)
// 			{
// 				prefs.colourScale[n].colour(cmin + i*cstep, col);
// 				glColor3f(col[0], col[1], col[2]);
// 				glVertex2d(x+i*qwidth,y);
// 				glVertex2d(x+i*qwidth,y+height);
// 			}
// 		  }
// 		  else
// 		  {
// 			cstep = (cmid - cmin) / halfslices;
// 			qwidth = (width * (cmid - cmin) / (cmax - cmin)) / halfslices;
// 			for (int i=0; i<=halfslices; i++)
// 			{
// 				prefs.colourScale[n].colour(cmin + i*cstep, col);
// 				glColor3f(col[0], col[1], col[2]);
// 				glVertex2d(x+i*qwidth,y);
// 				glVertex2d(x+i*qwidth,y+height);
// 			}
// 			x2 = x + halfslices * qwidth;
// 			cstep = (cmax - cmid) / halfslices;
// 			qwidth = (width * (cmax - cmid) / (cmax - cmin)) / halfslices;
// 			for (int i=1; i<=halfslices; i++)
// 			{
// 				prefs.colourScale[n].colour(cmid + i*cstep, col);
// 				glColor3f(col[0], col[1], col[2]);
// 				glVertex2d(x2+i*qwidth,y);
// 				glVertex2d(x2+i*qwidth,y+height);
// 			}
// 		  }
// 		glEnd();
		// Draw a black box surrounding the scalebar
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
		glColor3f(0.0f, 0.0f, 0.0f);
		glBegin(GL_LINE_LOOP);
		  glVertex2d(x,y);
		  glVertex2d(x, y+height);
		  glVertex2d(x+width, y+height);
		  glVertex2d(x+width,y);
		glEnd();
		// Add on text labels
/*		if (prefs.colourScale[n].type() == ColourScale::TwoPoint) sprintf(label,"%s [%f : %f]",prefs.colourScale[n].name(),cmin,cmax);
		else sprintf(label,"%s [%f : %f : %f]",prefs.colourScale[n].name(),cmin,cmid,cmax);*/
		glText(x+width+2.0, height_-y-8.0, label);
		y += height + 10.0;
	}
	dbgEnd(Debug::Calls,"Canvas::renderColourscales");
}


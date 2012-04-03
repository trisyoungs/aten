/*
	*** TCanvas 2D Rendering Functions
	*** src/gui/tcanvas_render.cpp
	Copyright T. Youngs 2007-2012

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

// Draw 2D objects with QPainter
void TCanvas::render2D(QPainter &painter, Model *source)
{
	msg.enter("TCanvas::render2D");
	// Variables
	static Dnchar text;
	QColor color;
	QBrush solidbrush(Qt::SolidPattern), nobrush(Qt::NoBrush);
	GLfloat colour[4];
	Vec4<double> screenr;
	Vec3<double> r;
	int i, skip, n;
	double dx, halfw;

	// Text Primitives
	prefs.copyColour(Prefs::TextColour, colour);
	color.setRgbF(colour[0], colour[1], colour[2], colour[3]);
	solidbrush.setColor(color);
	painter.setBrush(solidbrush);
	painter.setPen(Qt::SolidLine);
	painter.setPen(color);
	engine_.renderText(painter, this);

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
	switch (selectedMode_)
	{
		// Draw on distance ruler for drawing modes
		case (UserAction::DrawAtomAction):
		case (UserAction::DrawChainAction):
			// Get pixel 'length' in Angstrom terms at current draw depth
			r = source->screenToModel(contextWidth_/2+10, contextHeight_/2, currentDrawDepth_);
			r -= source->screenToModel(contextWidth_/2, contextHeight_/2, currentDrawDepth_);
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
				renderText(halfw + n*dx - (n < 0 ? 8 : 3), contextHeight_, itoa(n));
			}
			break;
		default:
			break;
	}
	msg.exit("TCanvas::render2D");
}

// Render 3D objects for current displayModel_
void TCanvas::render3D(Model *source, bool currentModel)
{	
	// Valid pointer set?
	if (source == NULL) return;
	msg.enter("TCanvas::render3D");
	
	// Render model
	msg.print(Messenger::GL, " --> RENDERING BEGIN : source model pointer = %p, renderpoint = %d\n", source, source->changeLog.log(Log::Total));
	
	// If this is a trajectory frame, check its ID against the last one rendered
	if (source->parent() != NULL)
	{
		displayFrameId_ = source->parent()->trajectoryFrameIndex();
		msg.print(Messenger::GL, " --> Source model is a trajectory frame - index = %i\n", displayFrameId_);
	}
		
	// Render 3D elements (with OpenGL)
	checkGlError();
	if (selectedMode_ == UserAction::DrawFragmentAction) engine_.flagClearLists();
	engine_.render3D(highQuality_, source, this, currentModel);
	//glFlush();
	checkGlError();

	msg.print(Messenger::GL, " --> RENDERING END\n");
	msg.exit("TCanvas::render3D");
}

// Attempt to detect/check for corrupt rendering
void TCanvas::isRenderingOk()
{
	msg.enter("TCanvas::isRenderingOk");
	if (!beginGl())
	{
		msg.print("Failed to test rendering...\n");
		msg.exit("TCanvas::isRenderingOk");
		return;
	}
	
	// Setup standard viewport
	glViewport(0,0,contextWidth_,contextHeight_);
	
	// Set clear colour to pure white
	glClearColor(1.0f,1.0f,1.0f,1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Create a temporary array and grab pixels from the current buffer
	GLubyte *pixelData_ = new GLubyte[4*contextWidth_*contextHeight_];
	glReadPixels(0,0,contextWidth_,contextHeight_, GL_RGBA, GL_UNSIGNED_BYTE, pixelData_);

	// Check first 10% of values in array - clear color was pure white, so all GLubyte values should be 255
	bool result = TRUE;
	for (int n = 0; n < 4*contextWidth_*contextHeight_*0.1; ++n)
	{
		msg.print(Messenger::GL,"isRenderingOkay - Pixel data value %i is %i\n", n, pixelData_[n]);
		if (pixelData_[n] != 255) result = FALSE;
	}

	// If we failed, set aten.prefs.manualswapbuffers and raise a message
	if (!result)
	{
		prefs.setManualSwapBuffers(TRUE);
		QMessageBox::information(NULL, "Rendering Check", "Aten detected that it's rendering was producing corrupt images. To attempt to remedy this, manual buffer swapping has been activated. If the main rendering canvas now displays everything correctly, you can add the line 'aten.prefs.manualswapbuffers = TRUE;' to your personal '.aten/user.dat' file in your home directory.");
	}

	// Set the clear colour back to the user-defined value
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);
	glClearColor(col[0],col[1],col[2],col[3]);
	
	endGl();
	
	msg.exit("TCanvas::isRenderingOk");
}

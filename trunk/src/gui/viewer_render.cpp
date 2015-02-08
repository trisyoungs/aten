/*
	*** TCanvas 2D Rendering Functions
	*** src/gui/tcanvas_render.cpp
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

#include "gui/tcanvas.uih"
#include "main/aten.h"
#include "base/sysfunc.h"

// Perform main rendering
void TCanvas::renderScene(int width, int height)
{
	msg.enter("TCanvas::renderScene");

	// Begin the GL commands
	if (!beginGl())
	{
		msg.print(Messenger::GL, " --> RENDERING END (BAD BEGIN)\n");
		msg.exit("TCanvas::renderScene");
		return;
	}

	// If we're rendering offscreen, store the current context size so we can restore it afterwards
	int oldWidth, oldHeight;
	if (renderType_ != RenderEngine::OnscreenScene)
	{
		oldWidth = contextWidth_;
		oldHeight = contextHeight_;
	}

	// Render model data - pass this TCanvas' QGLContext pointer so we can check it against the one owned by RenderEngine.
	engine().renderScene(primitiveSet_, width, height, context(), renderType_, renderIconSource_);

	// Swap buffers if necessary
	if (prefs.manualSwapBuffers()) swapBuffers();
	
	// Pop primitive instance if we were using the TCanvas to render a high-quality offscreen pixmap
	if ((primitiveSet_ == RenderEngine::HighQuality) && (!prefs.usePixelBuffers()))
	{
		msg.print(Messenger::Verbose, "GUI rendering offscreen, so popping instance created earlier.\n");
		engine().popInstance(RenderEngine::HighQuality, context());
	}
	
	// Always revert to lower quality for next pass
	primitiveSet_ = RenderEngine::LowQuality;
	
	// If we're rendering offscreen, restore the old context size
	if (renderType_ != RenderEngine::OnscreenScene)
	{
		contextWidth_ = oldWidth;
		contextHeight_ = oldHeight;
	}
	
	endGl();

	msg.exit("TCanvas::renderScene");
}


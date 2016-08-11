/*
	*** Model rendering
	*** src/model/render.cpp
	Copyright T. Youngs 2007-2016

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

#include "model/model.h"
#include "undo/undostate.h"
#include "undo/model_colourscheme.h"
#include "undo/model_drawstyle.h"

ATEN_USING_NAMESPACE

// Set default rendering style for models
void Model::setDrawStyle(Prefs::DrawStyle ds)
{
	// Nothing to do if the new style is the same as the old one
	if (ds == drawStyle_) return;

	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		ModelDrawStyleEvent* newchange = new ModelDrawStyleEvent;
		newchange->set(drawStyle_, ds);
		recordingState_->addEvent(newchange);
	}

	drawStyle_ = ds;

	logChange(Log::Style);
}

// Return rendering style for model
Prefs::DrawStyle Model::drawStyle()
{
	return drawStyle_;
}

// Return styled radius of specified atom
double Model::styleRadius(Prefs::DrawStyle ds, int el) const
{
	Prefs::DrawStyle dstyle;
	drawStyle_ == Prefs::OwnStyle ? dstyle = ds : dstyle = drawStyle_;
	return (dstyle == Prefs::ScaledStyle) ? (ElementMap::atomicRadius(el) * prefs.atomStyleRadius(Prefs::ScaledStyle)) : prefs.atomStyleRadius(dstyle);
}

// Set atom colouring style
void Model::setColourScheme(Prefs::ColouringScheme cs)
{
	// Nothing to do if the new scheme is the same as the old one
	if (cs == colourScheme_) return;

	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		ModelColourSchemeEvent* newchange = new ModelColourSchemeEvent;
		newchange->set(colourScheme_, cs);
		recordingState_->addEvent(newchange);
	}
	
	colourScheme_ = cs;

	logChange(Log::Style);
}

// Return atom colouring style
Prefs::ColouringScheme Model::colourScheme()
{
	return colourScheme_;
}

// Set rendering source
void Model::setRenderSource(Model::RenderSource rs)
{
	renderSource_ = rs;
}

// Return rendering source
Model::RenderSource Model::renderSource() const
{
	return renderSource_;
}

// Return the current rendering source for the model
Model* Model::renderSourceModel()
{
	switch (renderSource_)
	{
		case (Model::ModelSource):
			return this;
		case (Model::TrajectorySource):
			return trajectoryCurrentFrame_;
	}
	return NULL;
}

// Set whether to render from vibration frames
void Model::setRenderFromVibration(bool b)
{
	renderFromVibration_ = b;
}

// Return whether to render from vibration frames
bool Model::renderFromVibration()
{
	return renderFromVibration_;
}

// Return renderGroup, regenerating if necessary
RenderGroup& Model::renderGroup(PrimitiveSet& primitiveSet)
{
	// Check the logs, and decide if we need to regenerate the primitive list for the model
	if (renderGroupPoint_ == log(Log::Total)) return renderGroup_;

	// Calculate chunkSize, based on number of atoms in the model
	int chunkSize = pow(8, int(log10(atoms_.nItems() / 10)));
	renderGroup_.clear(chunkSize);

	renderGroup_.createAtomsAndBonds(primitiveSet, this, Matrix());

	renderGroup_.createGlyphs(primitiveSet, this);

	renderGroup_.createOverlays(this, Matrix());

	renderGroupPoint_ = log(Log::Total);

	return renderGroup_;
}

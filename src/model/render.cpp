/*
	*** Model rendering
	*** src/model/render.cpp
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

#include "model/model.h"

ATEN_USING_NAMESPACE

// Set rendering source
void Model::setRenderSource(Model::RenderSource rs)
{
	renderSource_ = rs;
	// Log a visual change here so we make sure that the GUI is updated properly
// 	logChange(Log::Visual);
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

	renderGroup_.clear();

	renderGroup_.createAtomsAndBonds(primitiveSet, this, Matrix());

	renderGroup_.createOverlays(this, Matrix());

	renderGroupPoint_ = log(Log::Total);

	return renderGroup_;
}

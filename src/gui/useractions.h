/*
	*** User actions
	*** src/gui/useractions.h
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

#ifndef ATEN_USERACTIONS_H
#define ATEN_USERACTIONS_H

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// User action text data
class UserAction
{
	public:

	// Actions
	enum Action { NoAction, SelectAction, SelectBoundAction, SelectElementAction, SelectRadialAction, MeasureDistanceAction, MeasureAngleAction, MeasureTorsionAction, DrawAtomsAction, DrawFragmentsAction, DrawTransmuteAction, DrawDeleteAction, DrawProbeAction, DrawAddHydrogenAction, DrawGrowAtomsAction, RotateXYAction, RotateZAction, TranslateAction, ZoomAction, TransformRotateXYAction, TransformRotateZAction, TransformTranslateAction, ShiftPickVectorAction, RotatePickAxisAction, TransformPickAAction, TransformPickBAction, TransformPickCAction, ConvertSourcePickAAction, ConvertSourcePickBAction, ConvertSourcePickCAction, ConvertTargetPickAAction, ConvertTargetPickBAction, ConvertTargetPickCAction, nUserActions };

	// Action texts
	const char* name;
	const char* unModified;
	const char* shiftModified;
	const char* ctrlModified;
	const char* altModified;
	int nAtomsToPick;
};

extern UserAction UserActions[];

ATEN_END_NAMESPACE

#endif

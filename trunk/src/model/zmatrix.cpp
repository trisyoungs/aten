/*
	*** Model zmatrix functions
	*** src/model/zmatrix.cpp
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

// Retrieve (creating or updating as necessary) the zmatrix for the model
ZMatrix* Model::zMatrix()
{
	// Update if necessary
	if (zMatrixPoint_ != (log(Log::Coordinates) + log(Log::Structure))) zMatrix_.create(this, FALSE);
	zMatrixPoint_ = log(Log::Coordinates) + log(Log::Structure);
	return &zMatrix_;
}

// Recalculate model atom posions from ZMatrix definition
void Model::recalculateFromZMatrix()
{
	Messenger::enter("Model::recalculateFromZMatrix");
	Atom* i, *j, *k, *l;
	// Cycle over each element in the zmatrix, repositioning atoms as we go
	for (ZMatrixElement* el = zMatrix_.elements(); el != NULL; el = el->next)
	{
		// Grab target atom
		i = el->atom(0);
		// If there are no other atoms defined, set coordinates of this atom to be the origin.
		// For other defined atoms, set distance, angle, and torsion accordingly
		j = el->atom(1);
		if (j == NULL) positionAtom(i, zMatrix_.origin());
		else setAtomicDistance(j, i, el->distance());
		k = el->atom(2);
		if (k != NULL) setAtomicAngle(k, j, i, el->angle());
		l = el->atom(3);
		if (l != NULL) setAtomicTorsion(l, k, j, i, el->torsion());
	}
	// Update model measurements
	updateMeasurements();
	// Update logpoint for ZMatrix
	zMatrixPoint_ = log(Log::Coordinates) + log(Log::Structure);
	Messenger::exit("Model::recalculateFromZMatrix");
}

/*
	*** Symmetry generator
	*** src/sg/generator.h
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

#ifndef ATEN_GENERATOR_H
#define ATEN_GENERATOR_H

#include "math/matrix.h"
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Symmetry generator
class Generator : public ListItem<Generator>
{
	public:
	// Constructor
	Generator();

	/*
	 * Rotation Matrix and Translation Vector
	 */
	private:
	// Generator text (if any)
	QString name_;
	// Generator matrix
	Matrix matrix_;
	// Set partial element of matrix in specified row
	void setMatrixPart(int row, QString part);

	public:
	// Set from XYZ-style name
	bool set(QString xyzName);
	// Set rotation matrix row (not including translation vector)
	void setRotationRow(int row, double x, double y, double z);
	// Set translation column
	void setTranslation(double tx, double ty, double tz, double divisor);
	// Return text 'name' of generator
	QString name() const;
	// Return operator matrix of generator
	Matrix& matrix();
};

ATEN_END_NAMESPACE

#endif

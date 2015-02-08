/*
	*** Line Stipple
	*** src/base/linestipple.h
	Copyright T. Youngs 2013-2014

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

#ifndef ATEN_LINESTIPPLE_H
#define ATEN_LINESTIPPLE_H

#ifdef _WIN32
#include <windows.h>
#endif
#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include <QtCore/QVector>
#include <QtCore/QString>

// Forward Declarations
class QComboBox;

// Line Stipple
class LineStipple
{
	public:
	// Line Stipple Types
	enum StippleType { NoStipple, DotStipple, FineDashStipple, EighthDashStipple, QuarterDashStipple, HalfDashStipple, DotDash1Stipple, nStippleTypes };
	// Convert text string to StippleType
	static LineStipple::StippleType stippleType(QString s);
	// Convert InputBlock to text string
	static const char* stippleType(LineStipple::StippleType stipple);


	/*
	 * Stipple
	 */
	public:
	// Line stipple factor
	GLint stippleFactor;
	// Line stipple pattern
	GLushort stipplePattern;
	// Name of stipple
	const char* name;

	public:
	// Add stipple pattern to specified QComboBox
	void addStippleItem(QComboBox* combo, int lineHeight);
	// Return stipple pattern as a Qt-compatible dash pattern
	QVector<qreal>& dashPattern();


	/*
	 * GL
	 */
	public:
	// Apply stipple pattern
	void apply();


	/*
	 * Singleton
	 */
	public:
	// Static list of fit line stipples
	static LineStipple stipple[];
};

#endif

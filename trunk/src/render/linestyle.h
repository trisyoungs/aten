/*
	*** Line Style
	*** src/base/linestyle.h
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

#ifndef ATEN_LINESTYLE_H
#define ATEN_LINESTYLE_H

#include "render/linestipple.h"
#include <QtGui/QColor>

// Forward Declarations (Aten)
/* none */

// Line Style
class LineStyle
{
	public:
	// Constructor / Destructor
	LineStyle();
	~LineStyle();
	// Copy constructor
	LineStyle(const LineStyle& source);
	// Assignment operator
	void operator=(const LineStyle& source);


	/*
	 * Style
	 */
	private:
	// Line width
	double width_;
	// Line stipple
	LineStipple::StippleType stipple_;
	// Line colour
	QColor colour_;

	public:
	// Set line style
	void set(double width, LineStipple::StippleType stipple, QColor colour);
	// Set line style
	void set(double width, LineStipple::StippleType stipple, double r, double g, double b, double a = 1.0);
	// Set line width
	void setWidth(double width);
	// Return line width
	double width();
	// Set line stipple
	void setStipple(LineStipple::StippleType stipple);
	// Return line stipple
	LineStipple::StippleType stipple();
	// Set line colour
	void setColour(QColor colour);
	// Set line colour
	void setColour(double r, double g, double b, double a = 1.0);
	// Return line colour
	QColor colour();


	/*
	 * GL
	 */
	private:
	// Scaling to use for line width
	static double lineWidthScale_;

	public:
	// Set line width scaling to use
	static void setLineWidthScale(double lineWidthScale);
	// Apply line style
	void apply();
	// Revert to normal line style (black, solid, 1.0px)
	static void revert();
};

#endif

/*
	*** Text Format
	*** src/render/textformat.h
	Copyright T. Youngs 2013-2016

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

#ifndef ATEN_TEXTFORMAT_H
#define ATEN_TEXTFORMAT_H

#include "templates/list.h"

// Forward Declarations (Aten)
/* none */

ATEN_USING_NAMESPACE

ATEN_BEGIN_NAMESPACE

// Text Formatter
class TextFormat : public ListItem<TextFormat>
{
	public:
	// Constructor / Desctructor
	TextFormat();
	~TextFormat();
	// Copy constructor
	TextFormat(const TextFormat& source);
	// Assignment operator
	void operator=(const TextFormat& source);


	/*
	 * Definition
	 */
	private:
	// Vertical (bottom-edge) position
	double y_;
	// Scale
	double scale_;
	// Whether text is italic
	bool italic_;
	// Whether text is bold
	bool bold_;

	public:
	// Set vertical (bottom-edge) position
	void setY(double y);
	// Adjust vertical (bottom-edge) position
	void adjustY(double delta);
	// Return vertical (bottom-edge) position
	double y();
	// Set scale
	void setScale(double scale);
	// Return scale
	double scale();
	// Set whether text is italic
	void setItalic(bool italic);
	// Return whether text is italic
	bool italic();
	// Set whether text is bold
	void setBold(bool bold);
	// Return whether text is bold
	bool bold();
};

ATEN_END_NAMESPACE

#endif

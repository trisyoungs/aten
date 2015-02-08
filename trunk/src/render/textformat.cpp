/*
	*** Text Format
	*** src/render/textformat.cpp
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

#include "render/textformat.h"

// Constructor
TextFormat::TextFormat() :  ListItem<TextFormat>()
{
	y_ = 0.0;
	scale_ = 1.0;
	italic_ = false;
	bold_ = false;
}

// Desctructor
TextFormat::~TextFormat()
{
}

// Copy constructor
TextFormat::TextFormat(const TextFormat& source)
{
	(*this) = source;
}

// Assignment operator
void TextFormat::operator=(const TextFormat& source)
{
	y_ = source.y_;
	scale_ = source.scale_;
	italic_ = source.italic_;
	bold_ = source.bold_;
}

// Set vertical (bottom-edge) position
void TextFormat::setY(double y)
{
	y_ = y;
}

// Adjust vertical (bottom-edge) position
void TextFormat::adjustY(double delta)
{
	y_ += delta;
}

// Return vertical (bottom-edge) position
double TextFormat::y()
{
	return y_;
}

// Set scale
void TextFormat::setScale(double scale)
{
	scale_ = scale;
}

// Return scale
double TextFormat::scale()
{
	return scale_;
}
// Set whether text is italic
void TextFormat::setItalic(bool italic)
{
	italic_ = italic;
}

// Return whether text is italic
bool TextFormat::italic()
{
	return italic_;
}

// Set whether text is bold
void TextFormat::setBold(bool bold)
{
	bold_ = bold;
}

// Return whether text is bold
bool TextFormat::bold()
{
	return bold_;
}

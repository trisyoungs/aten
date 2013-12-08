/*
	*** Text Primitive
	*** src/render/textprimitive.cpp
	Copyright T. Youngs 2007-2013

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

#include "render/textprimitive.h"
#include "gui/tcanvas.uih"

/*
// Text Primitive
*/

// Set data
void TextPrimitive::set(int x, int y, bool rightalign, const char *text)
{
	x_ = x;
	y_ = y;
	rightAlign_ = rightalign;
	text_ = text;
}

// Return x coordinate
int TextPrimitive::x()
{
	return x_;
}

// Return y coordinate
int TextPrimitive::y()
{
	return y_;
}

// Return text to render
QString &TextPrimitive::text()
{
	return text_;
}

// Return whether to right-align text
bool TextPrimitive::rightAlign()
{
	return rightAlign_;
}

/*
// Text Primitive Chunk
*/

// Constructor
TextPrimitiveChunk::TextPrimitiveChunk()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	nTextPrimitives_ = 0;
}

// Forget all text primitives in list
void TextPrimitiveChunk::forgetAll()
{
	nTextPrimitives_ = 0;
}

// Return whether array is full
bool TextPrimitiveChunk::full()
{
	return (nTextPrimitives_ == TEXTCHUNKSIZE);
}

// Add primitive to chunk
void TextPrimitiveChunk::add(int x, int y, const char *text, QChar addChar, bool rightAlign)
{
	textPrimitives_[nTextPrimitives_].set(x, y, rightAlign, text);
	if (addChar != 0) textPrimitives_[nTextPrimitives_].text() += addChar;
	++nTextPrimitives_;
}

// Render all primitives in chunk
void TextPrimitiveChunk::renderAll(QPainter &painter, int verticalOffset)
{
	QRect rect;
	for (int n=0; n<nTextPrimitives_; ++n)
	{
		rect = painter.boundingRect(textPrimitives_[n].x(), textPrimitives_[n].y()-verticalOffset, 1, -1, textPrimitives_[n].rightAlign() ? Qt::AlignRight : Qt::AlignLeft, textPrimitives_[n].text());
		painter.drawText(rect, textPrimitives_[n].rightAlign() ? Qt::AlignRight : Qt::AlignLeft, textPrimitives_[n].text());
	}
}

/*
// Text Primitive List
*/

// Constructor
TextPrimitiveList::TextPrimitiveList()
{
	currentChunk_ = NULL;
}

// Forget all text primitives, but keeping lists intact
void TextPrimitiveList::forgetAll()
{
	for (TextPrimitiveChunk *chunk = textPrimitives_.first(); chunk != NULL; chunk = chunk->next) chunk->forgetAll();
	currentChunk_ = textPrimitives_.first();
}

// Set data from literal coordinates and text
void TextPrimitiveList::add(int x, int y, const char *text, QChar addChar, bool rightAlign)
{
	if (currentChunk_ == NULL) currentChunk_ = textPrimitives_.add();
	else if (currentChunk_->full()) currentChunk_ = textPrimitives_.add();
	// Add primitive and set data
	currentChunk_->add(x, y, text, addChar, rightAlign);
}

// Render all primitives in list
void TextPrimitiveList::renderAll(QPainter &painter, int verticalOffset)
{
	for (TextPrimitiveChunk *chunk = textPrimitives_.first(); chunk != NULL; chunk = chunk->next) chunk->renderAll(painter, verticalOffset);
}

/*
// Text Primitive 3D
*/

// Constructor
TextPrimitive3D::TextPrimitive3D()
{
	// Public variables
	prev = NULL;
	next = NULL;
}

// Set data
void TextPrimitive3D::set(Vec3<double> r, bool rightalign, const char *text)
{
	r_ = r;
	rightAlign_ = rightalign;
	text_ = text;
}

// Return text coordinate
Vec3<double> TextPrimitive3D::r()
{
	return r_;
}

// Return text to render
const char *TextPrimitive3D::text()
{
	return text_.get();
}

// Return whether to right-align text
bool TextPrimitive3D::rightAlign()
{
	return rightAlign_;
}

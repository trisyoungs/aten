/*
	*** Text Primitive
	*** src/render/textprimitive.cpp
	Copyright T. Youngs 2007-2011

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

// Add primitive to list
void TextPrimitiveChunk::add(int x, int y, const char *text, QChar addChar, bool rightAlign)
{
	textPrimitives_[nTextPrimitives_].x = x;
	textPrimitives_[nTextPrimitives_].y = y;
	textPrimitives_[nTextPrimitives_].text = text;
	if (addChar != 0) textPrimitives_[nTextPrimitives_].text += addChar;
	textPrimitives_[nTextPrimitives_].rightAlign = rightAlign;
	++nTextPrimitives_;
}

// Render all primitives in list
void TextPrimitiveChunk::renderAll(QPainter &painter, TCanvas *canvas)
{
	// Grab contextHeight
	QRect rect;
	int height = canvas->contextHeight();
	int pointsize = prefs.labelSize();
	if (prefs.useNiceText())
	{
		for (int n=0; n<nTextPrimitives_; ++n)
		{
			rect = painter.boundingRect(textPrimitives_[n].x, height-textPrimitives_[n].y-pointsize, 1, -1, textPrimitives_[n].rightAlign ? Qt::AlignRight : Qt::AlignLeft, textPrimitives_[n].text);
			painter.drawText(rect, textPrimitives_[n].rightAlign ? Qt::AlignRight : Qt::AlignLeft, textPrimitives_[n].text);
		}
	}
	else for (int n=0; n<nTextPrimitives_; ++n) canvas->renderText(textPrimitives_[n].x, height-textPrimitives_[n].y-pointsize, textPrimitives_[n].text);
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
	// If we are rendering with nice text (i.e. QPainter) store info for later
	if (currentChunk_ == NULL) currentChunk_ = textPrimitives_.add();
	else if (currentChunk_->full()) currentChunk_ = textPrimitives_.add();
	// Add primitive and set data
	currentChunk_->add(x, y, text, addChar, rightAlign);
}

// Render all primitives in list
void TextPrimitiveList::renderAll(QPainter &painter, TCanvas *canvas)
{
	for (TextPrimitiveChunk *chunk = textPrimitives_.first(); chunk != NULL; chunk = chunk->next) chunk->renderAll(painter, canvas);
}


/*
	*** Text Primitive
	*** src/render/textprimitive.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_TEXTPRIMITIVE_H
#define ATEN_TEXTPRIMITIVE_H

#include <QtGui/QtGui>
#include "templates/vector3.h"
#include "base/matrix.h"
#include "templates/list.h"

#define TEXTCHUNKSIZE 100

// Forward Declarations
class TCanvas;

// Text Primitive
class TextPrimitive
{
	private:
	// Screen coordinates
	int x_, y_;
	// Whether to right-align text
	bool rightAlign_;
	// Text to render
	QString text_;
	
	public:
	// Set data
	void set(int x, int y, bool rightalign, const char *text);
	// Return x coordinate
	int x();
	// Return y coordinate
	int y();
	// Return text to render
	QString &text();
	// Return whether to right-align text
	bool rightAlign();
};

// Text Primitive Chunk
class TextPrimitiveChunk
{
	public:
	// Constructor
	TextPrimitiveChunk();
	// List pointers
	TextPrimitiveChunk *prev, *next;

	private:
	// Array of TextPrimitive
	TextPrimitive textPrimitives_[TEXTCHUNKSIZE];
	// Number of text primitives currently in the array
	int nTextPrimitives_;

	public:
	// Forget all text primitives in list
	void forgetAll();
	// Return whether array is full
	bool full();
	// Add primitive to list
	void add(int x, int y, const char *text, QChar addChar = 0, bool rightAlign = FALSE);
	// Render all primitives in chunk
	void renderAll(QPainter& painter, TCanvas* canvas);
};

// Text Primitive List
class TextPrimitiveList
{
	public:
	// Constructor
	TextPrimitiveList();

	private:
	// List of text primitive chunks
	List<TextPrimitiveChunk> textPrimitives_;
	// Current TextPrimitiveChunk
	TextPrimitiveChunk *currentChunk_;

	public:
	// Forget all text primitives, but keeping lists intact
	void forgetAll();
	// Add primitive to list
	void add(int x, int y, const char *text, QChar addChar = 0, bool rightAlign = FALSE);
	// Render all primitives in list
	void renderAll(QPainter& painter, TCanvas* canvas);
};

#endif

/*
	*** Undo Event - Glyph Creation
	*** src/undo/glyph_creation.cpp
	Copyright T. Youngs 2007-2018

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

#include "undo/glyph_creation.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
GlyphCreationEvent::GlyphCreationEvent() : UndoEvent()
{
}

// Destructor
GlyphCreationEvent::~GlyphCreationEvent()
{
}

// Set change 
void GlyphCreationEvent::set(bool creation, Glyph* g)	// TODO Need a separate event for changes to glyph data - keep all changes in one event somehow?
{
	Messenger::enter("GlyphCreationEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	// Copy Glyph data
	if (g != NULL)
	{
		glyphData_ = *g;
		for (int i=0; i<glyphData_.nGlyphData(g->type()); ++i) atomIDs_[i] = (glyphData_.data(i)->atom() == NULL ? -1 : glyphData_.data(i)->atom()->id());
	}
	else printf("Null pointer passed to GlyphCreationEvent::set()!\n");
	Messenger::exit("GlyphCreationEvent::set");
}

// Undo stored change
void GlyphCreationEvent::undo(Model* m)
{
	Messenger::enter("GlyphCreationEvent::undo");
	// Glyph creation (UndoEvent::Redo) or deletion (UndoEvent::Undo)
	Messenger::exit("GlyphCreationEvent::undo");
}

// Print event info
void GlyphCreationEvent::print()
{
}


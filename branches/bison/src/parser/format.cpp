/*
	*** String formatter
	*** src/parser/format.cpp
	Copyright T. Youngs 2007-2009

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

#include "parser/format.h"

/*
// Format
*/

// Constructor
NuFormat::NuFormat(const char *s)
{
}

// Destructor
NuFormat::~NuFormat()
{
}

/*
// Format node
*/

// Constructors
FormatChunk::FormatChunk(const char *plaintext)
{
	// Private variables
	type_ = PlainTextChunk;
	cFormat_ = plaintext;

	// Public variables
	next = NULL;
	prev = NULL;
}

FormatChunk::FormatChunk(FormatChunk::ChunkType type, const char *format, int argid) : type_(type), argId_(argid)
{
	// Private variables
	cFormat_ = format;

	// Public variables
	next = NULL;
	prev = NULL;
}

// Return chunktype
FormatChunk::ChunkType FormatChunk::type()
{
	return type_;
}

// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
const char *FormatChunk::cFormat()
{
	return cFormat_.get();
}

// Return argument id
int FormatChunk::argId()
{
	return argId_;
}

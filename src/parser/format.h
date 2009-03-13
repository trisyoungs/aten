/*
	*** String formatter
	*** src/parser/format.h
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

#ifndef ATEN_NUFORMAT_H
#define ATEN_NUFORMAT_H

#include "base/dnchar.h"
#include "templates/list.h"

// Format node
class FormatChunk
{
	public:
	// Node types
	enum ChunkType { PlainTextChunk, nChunkTypes };
	// Constructors
	FormatChunk(const char *plaintext);
	FormatChunk(ChunkType type, const char *format, int argid);
	// List pointers
	FormatChunk *next, *prev;


	/*
	// Chunk Data
	*/
	private:
	// Type of chunk
	ChunkType type_;
	// C-style format relevant to chunk *or* plain text data if PlainTextChunk
	Dnchar cFormat_;
	// Argument ID specifying source (in the case of read) or destination (in the case of write) command arguments
	int argId_;

	public:
	// Return chunktype
	ChunkType type();
	// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
	const char *cFormat();
	// Return argument id
	int argId();
};

// Format
class NuFormat
{
	public:
	// Constructor / Destructor
	NuFormat(const char *s);
	~NuFormat();


	/*
	// Data
	*/
	private:
	// Chunk list
	List<FormatChunk> chunks_;
	// Created string
	static char createdString_[8096];

	/*
	// Read/Write
	*/
	public:
	// Return last written string
	const char *string();
	// Write format to internal string
	bool writeToString();
};

#endif


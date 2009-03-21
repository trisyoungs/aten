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
#include "base/lineparser.h"
#include "templates/list.h"
#include "templates/reflist.h"

// Forward Declarations
class TreeNode;

// Format node
class FormatChunk
{
	public:
	// Node types
	enum ChunkType { PlainTextChunk, FormattedChunk, nChunkTypes };
	// Constructors
	FormatChunk(const char *plaintext);
	FormatChunk(ChunkType type, const char *format, TreeNode *arg);
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
	// Argument pointing to source (in the case of read) or destination (in the case of write) command arguments
	TreeNode *arg_;

	public:
	// Return chunktype
	ChunkType type();
	// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
	const char *cFormat();
	// Return associated argument
	TreeNode *arg();
};

// Format
class NuFormat
{
	public:
	// Constructor / Destructor
	NuFormat(const char *s, Refitem<TreeNode,int> *firstarg);
	~NuFormat();


	/*
	// Data
	*/
	private:
	// Whether the format was created succesfully
	bool isValid_;
	// Chunk list
	List<FormatChunk> chunks_;
	// Created string
	static char createdString_[8096];

	public:
	// Return whether the format was created successfully
	bool isValid();

	/*
	// Read/Write
	*/
	private:
	// Use specified parser to perform formatted read
	int read(LineParser *parser, int flags);

	public:
	// Return last written string
	const char *string();
	// Write format to internal string
	bool writeToString();
	// Read line and parse according to format
	int readFormatted(const char *line, int flags);
	// Read line from file and parse according to format
	int readFormatted(LineParser *parser, int flags);
};

#endif


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
#include "parser/vtypes.h"
#include "templates/list.h"
#include "templates/reflist.h"

// Forward Declarations
class TreeNode;

// Format node
class FormatChunk
{
	public:
	// Node types
	enum ChunkType { PlainTextChunk, FormattedChunk, DelimitedChunk, nChunkTypes };
	// Constructors
	FormatChunk(const char *plaintext);
	FormatChunk(TreeNode *arg);
	FormatChunk(const char *format, TreeNode *arg, NuVTypes::DataType retrievetype);
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
	// Length of formatted argument (if one was supplied)
	int formatLength_;
	// Argument pointing to source (in the case of read) or destination (in the case of write) command arguments
	TreeNode *arg_;
	// Variable type to retrieve variable data as (related to contents of cFormat_)
	NuVTypes::DataType retrieveType_;

	public:
	// Return chunktype
	ChunkType type();
	// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
	const char *cFormat();
	// Return length of formatted chunk
	int formatLength();
	// Return associated argument
	TreeNode *arg();
	// Return variable type to retrieve variable data as
	NuVTypes::DataType retrieveType();
};

// Format
class NuFormat
{
	public:
	// Constructors / Destructor
	NuFormat(Refitem<TreeNode,int> *firstarg);
	NuFormat(const char *format, Refitem<TreeNode,int> *firstarg);
	~NuFormat();


	/*
	// Data
	*/
	private:
	// Whether the format was created succesfully
	bool isValid_;
	// Whether the format was created from a C-style format, or just a list of data items
	bool delimited_;
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
	int executeRead(LineParser *parser, int flags);

	public:
	// Return last written string
	const char *string();
	// Write format to internal string
	bool writeToString();
	// Read line and parse according to format
	int read(const char *line, int flags);
	// Read line from file and parse according to format
	int read(LineParser *parser, int flags);
};

#endif


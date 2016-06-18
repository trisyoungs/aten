/*
	*** String formatter
	*** src/parser/format.h
	Copyright T. Youngs 2007-2016

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

#ifndef ATEN_FORMAT_H
#define ATEN_FORMAT_H

#include "base/lineparser.h"
#include "parser/vtypes.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class TreeNode;

// Format node
class FormatChunk : public ListItem<FormatChunk>
{
	public:
	// Node types
	enum ChunkType { PlainTextChunk, FormattedChunk, DelimitedChunk, GreedyDelimitedChunk, nChunkTypes };
	// Constructors
	FormatChunk(ChunkType type, QString cFormat, TreeNode* arg = NULL, VTypes::DataType retrieveType = VTypes::NoData);


	/*
	 * Chunk Data
	 */
	private:
	// Type of chunk
	ChunkType type_;
	// C-style format relevant to chunk *or* plain text data if PlainTextChunk
	QString cFormat_;
	// Length of formatted argument (if one was supplied)
	int formatLength_;
	// Argument pointing to source (in the case of read) or destination (in the case of write) command arguments
	TreeNode* arg_;
	// Variable type to retrieve variable data as (related to contents of cFormat_)
	VTypes::DataType retrieveType_;

	public:
	// Return chunktype
	ChunkType type();
	// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
	QString cFormat();
	// Return length of formatted chunk
	int formatLength();
	// Return length of plaintext (cFormat)
	int textLength();
	// Return associated argument
	TreeNode* arg();
	// Return variable type to retrieve variable data as
	VTypes::DataType retrieveType();
};

// Format
class Format
{
	public:
	// Constructors / Destructor
	Format(RefListItem<TreeNode,int>* firstarg);
	Format(QString cFormat, RefListItem<TreeNode,int>* firstarg);
	~Format();


	/*
	 * Data
	 */
	private:
	// Whether the format was created succesfully
	bool isValid_;
	// Whether the format was created from a C-style format, or just a list of data items
	bool delimited_;
	// Chunk list
	List<FormatChunk> chunks_;
	// Created string
	QString createdString_;
	// Add new plaintext chunk to format
	void addPlainTextChunk(QString plainText);
	// Add new formatted chunk to format
	void addFormattedChunk(QString cFormat, TreeNode* arg, VTypes::DataType retrievetype);
	// Add new delimited chunk to format
	void addDelimitedChunk(TreeNode* arg);
	// Add new greedy delimited chunk to format
	void addGreedyDelimitedChunk(TreeNode* arg, VTypes::DataType retrievetype);

	public:
	// Return whether the format was created successfully
	bool isValid();

	
	/*
	 * Read/Write
	 */
	private:
	// Use specified parser to perform formatted read
	int executeRead(LineParser* parser, int optionMask);

	public:
	// Return last written string
	QString string();
	// Write format to internal string
	bool writeToString();
	// Read line and parse according to format
	int read(QString line, int optionMask);
	// Read line from file and parse according to format
	int read(LineParser* parser, int optionMask);
};

// Parse Chunk (Simplified Format Chunk)
class ParseChunk : public ListItem<ParseChunk>
{
	public:
	// Node types
	enum ChunkType { PlainTextChunk, FormattedChunk, DelimitedChunk, GreedyDelimitedChunk, DiscardChunk, nChunkTypes };
	// Constructors
	ParseChunk(ChunkType type, QString cFormat);


	/*
	 * Chunk Data
	 */
	private:
	// Type of chunk
	ChunkType type_;
	// C-style format relevant to chunk *or* plain text data if PlainTextChunk
	QString cFormat_;
	// Length of formatted argument (if one was supplied)
	int formatLength_;

	public:
	// Return chunktype
	ChunkType type();
	// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
	QString cFormat();
	// Return length of formatted chunk
	int formatLength();
};

// Parse Format (Simplified Format)
class ParseFormat
{
	public:
	// Constructor
	ParseFormat(QString cFormat);


	/*
	 * Chunk Data
	 */
	private:
	// Chunk list
	List<ParseChunk> chunks_;

	private:
	// Add new chunk
	void addChunk(ParseChunk::ChunkType type, QString data);

	public:
	// Return first chunk
	ParseChunk* chunks();
};

ATEN_END_NAMESPACE

#endif

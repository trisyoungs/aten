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
#include "parser/vtypes.h"
#include "parser/treenode.h"
#include <ctype.h>

/*
// Format Chunks
*/

// Constructor
FormatChunk::FormatChunk(ChunkType type, const char *fmt, TreeNode *arg, VTypes::DataType retrieveType)
{
	// Private variables
	type_ = type;
	cFormat_ = fmt;
	arg_ = arg;
	retrieveType_ = retrieveType;
	formatLength_ = 0;
	msg.print(Messenger::Parse, "...created FormatChunk for string '%s'\n", fmt);

	// Determine length of format if one was provided
	if (fmt != NULL)
	{
		char text[32];
		int n = 0;
		for (const char *c = &fmt[1]; isdigit(*c); c++) text[n++] = *c;
		text[n] = '\0';
		formatLength_ = atoi(text);
	}

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

// Return length of formatted chunk
int FormatChunk::formatLength()
{
	return formatLength_;
}

// Return length of plaintext (cFormat)
int FormatChunk::textLength()
{
	return cFormat_.length();
}

// Return argument id
TreeNode *FormatChunk::arg()
{
	return arg_;
}

// Return variable type to retrieve variable data as
VTypes::DataType FormatChunk::retrieveType()
{
	return retrieveType_;
}

/*
// Format
*/

// Singleton
char Format::createdString_[8096];

// Constructor
Format::Format(Refitem<TreeNode,int> *firstarg)
{
	// Construct a delimited list of chunks with no specific format
	for (Refitem<TreeNode,int> *ri = firstarg; ri != NULL; ri = ri->next) addDelimitedChunk(ri->item);
	delimited_ = TRUE;
	isValid_ = TRUE;
}

// Constructor
Format::Format(const char *s, Refitem<TreeNode,int> *firstarg)
{
	// Private variables
	isValid_ = TRUE;
	delimited_ = FALSE;

	// Step through formatting string, looking for '%' symbols (terminated by a non-alpha)
	const char *c = s;
	char prevchar;
	static char plaintext[8096];
	VTypes::DataType type;
	bool isformatter = FALSE, isdiscarder, restofline;
	Refitem<TreeNode,int> *arg = firstarg;
	msg.print(Messenger::Parse, "Creating Format object from string '%s' (and any supplied arguments)...\n", s);
	int length = 0, n;
	do
	{
		// If we find a '%' store any previous characters as a plain-text chunk and begin a formatted chunk
		if (*c == '%')
		{
			// Check for a previous format, in which case this one is mangled
			if (isformatter)
			{
				msg.print("Found an unterminated format specifier (%) in format string '%s'.\n", s);
				isValid_ = FALSE;
				return;
			}
			
			if (length > 0)
			{
				plaintext[length] = '\0';
				addPlainTextChunk(plaintext);
				length = 0;
			}
			isformatter = TRUE;
			plaintext[0] = '%';
			c++;
			length = 1;
		}

		// Increment character position...
		plaintext[length] = *c;
		prevchar = *c;
		c++;
		length++;

		// If we're currently in the middle of a formatter, it's terminated by an alpha character or '*'
		if (isformatter && (isalpha(prevchar) || (prevchar == '*')))
		{
			// If the current character is 'l', 'h', or 'L' don't terminate yet
			if ((prevchar == 'l') || (prevchar == 'h') || (prevchar == 'L')) continue;

			plaintext[length] = '\0';
			msg.print(Messenger::Parse, "Detected format bit [%s]\n", plaintext);
			// Check the terminating character to make sure that its one we recognise *and* is compatible with the type of argument given
			if ((arg == NULL) && (prevchar != '*'))
			{
				msg.print("Formatter '%s' in string has no corresponding argument.\n", plaintext);
				isValid_ = FALSE;
				break;
			}
			else
			{
				isdiscarder = FALSE;
				restofline = FALSE;
				type = arg == NULL ? VTypes::NoData : arg->item->returnType();
				prevchar = plaintext[length-2];
				if (!isalpha(prevchar)) prevchar = '\0';
				switch (tolower(plaintext[length-1]))
				{
					// Integer types
					case ('i'):
					case ('d'):
					case ('x'):
					case ('u'):
						// If a preceeding 'l' was specified, then we must have a pointer
						if (prevchar == 'l')
						{
							if (type >= VTypes::AtenData) break;
							msg.print("Format '%s' expects a pointer, but has been given %s.\n", plaintext, VTypes::aDataType(type));
							isValid_ = FALSE;
						}
						else if ((prevchar == '\0') || (prevchar == 'h'))
						{
							if (type == VTypes::IntegerData) break;
							msg.print("Format '%s' expects an integer, but has been given %s.\n", plaintext, VTypes::aDataType(type));
							isValid_ = FALSE;
						}
						else
						{
							msg.print("Integer format '%c' cannot be preceeded by the identifier '%c'.\n", *c, prevchar);
							isValid_ = FALSE;
						}
						break;
					// Floating-point types
					case ('e'):
					case ('f'):
					case ('g'):
						// If a preceeding 'L' was specified, we complain!
						if (prevchar == 'L')
						{
							msg.print("Output of long doubles (prefixing a floating-point formatter with 'L') is not supported.\n");
							isValid_ = FALSE;
						}
						else if (prevchar == '\0')
						{
							if (type == VTypes::DoubleData) break;
							msg.print("Format '%s' expects a real, but has been given %s.\n", plaintext, VTypes::aDataType(type));
							isValid_ = FALSE;
						}
						else
						{
							msg.print("Floating-point format '%c' cannot be preceeded by the identifier '%c'.\n", *c, prevchar);
							isValid_ = FALSE;
						}
						break;
					// Character types
					case ('r'):
						restofline = TRUE;
					case ('s'):
						if (prevchar != '\0')
						{
							msg.print("String format '%c' cannot be preceeded by the identifier '%c'.\n", tolower(plaintext[length-1]), prevchar);
							isValid_ = FALSE;
						}
						if (type == VTypes::StringData) break;
						msg.print("Format '%s' expects a string, but has been given %s.\n", plaintext, VTypes::aDataType(type));
						isValid_ = FALSE;
						break;
					case ('c'):
						msg.print("Character format 'c'is not supported.\n");
						isValid_ = FALSE;
						break;
					// Discard identifier
					case ('*'):
						isdiscarder = TRUE;
						type = VTypes::NoData;
						break;
					default:
						msg.print("Unsupported format '%s'.\n", plaintext);
						isValid_ = FALSE;
						break;
				}
			}
			// Don't use up a variable argument if the specifier was '*'
			TreeNode *node = (arg == NULL ? NULL : arg->item);
			if (restofline) addGreedyDelimitedChunk(node);
			else addFormattedChunk(plaintext, node, type);
			if (!isdiscarder) arg = arg->next;
			length = 0;
			isformatter = FALSE;
// 			c++;
		}
	} while (*c != '\0');
	// Do we have some text left over?
	if (length > 0)
	{
		plaintext[length] = '\0'; 
		addPlainTextChunk(plaintext);
	}
	// Are there any supplied arguments remaining?
	if (arg != NULL) msg.print("Warning: Extra data arguments given to format '%s'...\n", s);
}

// Destructor
Format::~Format()
{
}

// Return whether the format was created successfully
bool Format::isValid()
{
	return isValid_;
}

// Add new plaintext chunk to format
void Format::addPlainTextChunk(const char *s)
{
	FormatChunk *chunk = new FormatChunk(FormatChunk::PlainTextChunk, s);
	chunks_.own(chunk);
}

// Add new formatted chunk to format
void Format::addFormattedChunk(const char *format, TreeNode *arg, VTypes::DataType retrievetype)
{
	FormatChunk *chunk = new FormatChunk(FormatChunk::FormattedChunk, format, arg, retrievetype);
	chunks_.own(chunk);
}

// Add new delimited chunk to format
void Format::addDelimitedChunk(TreeNode *arg)
{
	if (arg == NULL)
	{
		printf("Internal Error: Tried to create a DelimitedChunk from a NULL argument.\n");
		return;
	}
	FormatChunk *chunk = new FormatChunk(FormatChunk::DelimitedChunk, NULL, arg, arg->returnType());
	chunks_.own(chunk);
}

// Add new greedy delimited chunk to format
void Format::addGreedyDelimitedChunk(TreeNode *arg)
{
	FormatChunk *chunk = new FormatChunk(FormatChunk::GreedyDelimitedChunk, NULL, arg);
	chunks_.own(chunk);
}

/*
// Format - Read/Write
*/

// Use specified parser to perform formatted read
int Format::executeRead(LineParser *parser, int flags)
{
	msg.enter("Format::executeRead");
	int nparsed = 0, length;
	ReturnValue rv;
	Dnchar bit;
	// Cycle through the list of FormatChunks
	for (FormatChunk *chunk = chunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		// Retrieve the required characters from the input stream
		switch (chunk->type())
		{
			case (FormatChunk::PlainTextChunk):
				// Skip as many characters as there are in the string
				length = chunk->textLength();
				parser->getNextN(length, &bit);
				break;
			case (FormatChunk::DelimitedChunk):
				// Get next delimited argument from LineParser
				parser->getNextArg(&bit, flags);
				if (!bit.isEmpty()) nparsed ++;
				break;
			case (FormatChunk::GreedyDelimitedChunk):
				// Get rest of line, starting from next delimited argument
				parser->getRestDelim(&bit);
				if (!bit.isEmpty()) nparsed ++;
				break;
			case (FormatChunk::FormattedChunk):
				// Get argument from LineParser
				length = chunk->formatLength();
				if (length > 0) parser->getNextN(length, &bit);
				else parser->getNextArg(&bit, flags);
				if (!bit.isEmpty()) nparsed ++;
				break;
			default:
				printf("Internal Error: Action for this type of format chunk (%i) has not been defined.\n", chunk->type());
				msg.exit("Format::executeRead");
				return 1;
		}
		// Set the corresponding argument accordingly
		if (chunk->type() != FormatChunk::PlainTextChunk)
		{
			switch (chunk->retrieveType())
			{
				case (VTypes::NoData):
					break;
				case (VTypes::IntegerData):
					rv.set( atoi(bit.get()) );
					break;
				case (VTypes::DoubleData):
					rv.set( atof(bit.get()) );
					break;
				case (VTypes::StringData):
					rv.set( bit.get() );
					break;
				default:
					printf("Internal Error: Formatted conversion to %s is not possible.\n", VTypes::aDataType(chunk->arg()->returnType()));
					nparsed = -1;
					break;
			}
			if (nparsed == -1) break;
			if (chunk->retrieveType() != VTypes::NoData) chunk->arg()->set( rv );
		}
	}
	msg.exit("Format::executeRead");
	return nparsed;
}

// Return last written string
const char *Format::string()
{
	return createdString_;
}

// Write format to internal string
bool Format::writeToString()
{
	msg.enter("Format::writeToString");
	static char bit[4096];
	createdString_[0] = '\0';
	ReturnValue rv;
	// Cycle through the list of FormatChunks
	for (FormatChunk *chunk = chunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		// Retrieve the required characters from the input stream
		switch (chunk->type())
		{
			case (FormatChunk::FormattedChunk):
				chunk->arg()->execute(rv);
				bit[0] = '\0';
				switch (chunk->retrieveType())
				{
					case (VTypes::IntegerData):
						sprintf(bit, chunk->cFormat(), rv.asInteger());
						break;
					case (VTypes::DoubleData):
						sprintf(bit, chunk->cFormat(), rv.asDouble());
						break;
					case (VTypes::StringData):
						sprintf(bit, chunk->cFormat(), rv.asString());
						break;
					default:
						// Pointer types
						sprintf(bit, chunk->cFormat(), rv.asPointer(chunk->retrieveType()));
						break;
				}
				strcat(createdString_, bit);
				break;
			case (FormatChunk::PlainTextChunk):
				strcat(createdString_, chunk->cFormat());
				break;
			case (FormatChunk::DelimitedChunk):
				chunk->arg()->execute(rv);
				strcat(createdString_, rv.asString());
				if (chunk->next != NULL) strcat(createdString_, " ");
				break;
			default:
				printf("Internal Error: Action for this type of format chunk has not been defined.\n");
				msg.exit("Format::writeToString");
				return FALSE;
		}
	}
	// If this was originally a delimited chunk, append a newline
	if (delimited_) strcat(createdString_, "\n");
	msg.exit("Format::writeToString");
	return TRUE;
}

// Parse supplied line according to format
int Format::read(const char *line, int flags)
{
	msg.enter("Format::read[string]");
	static LineParser parser;
	parser.setLine(line);
	int result = executeRead(&parser, flags);
	msg.exit("Format::read[string]");
	return result;
}

// Read line from file and parse according to format
int Format::read(LineParser *parser, int flags)
{
	msg.enter("Format::read[file]");
	// Read a new line using the supplied parser
	if (parser == NULL)
	{
		printf("Internal Error: No LineParser given to Format::read.\n");
		msg.exit("Format::read[file]");
		return 1;
	}
	// Get next line from file
	int result = parser->readLine();
	if (result == 0) result = executeRead(parser, flags);
	msg.exit("Format::read[file]");
	return result;
}

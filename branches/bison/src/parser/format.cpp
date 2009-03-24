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

// Constructors
FormatChunk::FormatChunk(const char *plaintext)
{
	// Private variables
	type_ = PlainTextChunk;
	cFormat_ = plaintext;
	arg_ = NULL;
	msg.print(Messenger::Parse, "...created PlainTextChunk for string '%s'\n", plaintext);

	// Public variables
	next = NULL;
	prev = NULL;
}

FormatChunk::FormatChunk(TreeNode *node) : arg_(node)
{
	// Private variables
	type_ = DelimitedChunk;
	msg.print(Messenger::Parse, "...created DelimitedChunk for argument %li\n", node);

	// Public variables
	next = NULL;
	prev = NULL;
}

FormatChunk::FormatChunk(const char *format, TreeNode *node) : arg_(node)
{
	// Private variables
	cFormat_ = format;
	type_ = FormattedChunk;
	msg.print(Messenger::Parse, "...created FormattedChunk ('%s') for argument %li\n", format, node);

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
TreeNode *FormatChunk::arg()
{
	return arg_;
}

/*
// Format
*/

// Singleton
char NuFormat::createdString_[8096];

// Constructor
NuFormat::NuFormat(Refitem<TreeNode,int> *firstarg)
{
	// Construct a delimited list of chunks with no specific format
	for (Refitem<TreeNode,int> *ri = firstarg; ri != NULL; ri = ri->next) chunks_.own( new FormatChunk(ri->item) );
	isValid_ = TRUE;
}

// Constructor
NuFormat::NuFormat(const char *s, Refitem<TreeNode,int> *firstarg)
{
	// Private variables
	isValid_ = TRUE;

	// Step through formatting string, looking for '%' symbols (terminated by a non-alpha)
	const char *c = s;
	bool isformatter = FALSE;
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
			length++;
			if (length > 0)
			{
				char *plaintext = new char[length+1];
				for (n=0; n<length; n++) plaintext[n] = c[n];
				plaintext[length] = '\0'; 
				chunks_.own( new FormatChunk(plaintext) );
				delete[] plaintext;
			}
			isformatter = TRUE;
			c++;
			continue;
		}
		// If we're currently in the middle of a formatter, it's terminated by an alpha character
		if (isformatter && (isalpha(*c)))
		{
			length ++;
			// If the terminating character is 'l', 'h', or 'L', don't break yet..
			if ((*c == 'l') || (*c == 'h') || (*c == 'L')) { c++; continue; }
			isformatter = FALSE;
			char *format = new char[length+1];
			for (n=0; n<length; n++) format[n] = c[n];
			format[length] = '\0';
			// Check the terminating character to make sure that its one we recognise *and* is compatible with the type of argument given
			if (arg == NULL) msg.print("Formatter '%s' in string has no corresponding argument.\n", format);
			else
			{
				NuVTypes::DataType type = arg->item->returnType();
				switch (tolower(*c))
				{
					// Integer types
					case ('i'):
					case ('d'):
					case ('x'):
					case ('u'):
						if (type == NuVTypes::IntegerData) break;
						msg.print("Format '%s' expects an integer, but has been given %s.\n", format, NuVTypes::aDataType(type));
						isValid_ = FALSE;
						break;
					// Floating-point types
					case ('e'):
					case ('f'):
					case ('g'):
						if (type == NuVTypes::RealData) break;
						msg.print("Format '%s' expects a real, but has been given %s.\n", format, NuVTypes::aDataType(type));
						isValid_ = FALSE;
						break;
					// Character types
					case ('s'):
						if (type == NuVTypes::StringData) break;
						msg.print("Format '%s' expects a string, but has been given %s.\n", format, NuVTypes::aDataType(type));
						isValid_ = FALSE;
						break;
					default:
						msg.print("Unsupported format '%s'.\n", format);
						isValid_ = FALSE;
						break;
				}
			}
			TreeNode *node = (arg == NULL ? NULL : arg->item);
			chunks_.own( new FormatChunk(format, node) );
			delete[] format;
			c++;
			continue;
		}
		// Increment length
		c++;
		length++;
	} while (*c != '\0');
}

// Destructor
NuFormat::~NuFormat()
{
}

// Return whether the format was created successfully
bool NuFormat::isValid()
{
	return isValid_;
}

/*
// Format - Read/Write
*/

// Use specified parser to perform formatted read
int NuFormat::read(LineParser *parser, int flags)
{
	msg.enter("NuFormat::read");
	int nparsed = 0;
	NuReturnValue rv;
	Dnchar bit;
	// Cycle through the list of FormatChunks
	for (FormatChunk *chunk = chunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		// Retrieve the required characters from the input stream
		switch (chunk->type())
		{
			case (FormatChunk::DelimitedChunk):
				// Get next delimited argument from LineParser
				parser->getNextArg(&bit);
				
				break;
			default:
				printf("Internal Error: Action for this type of format chunk has not been defined.\n");
				msg.exit("NuFormat::read");
				return 1;
		}
		// Set the corresponding argument accordingly
		if (chunk->type() != FormatChunk::PlainTextChunk)
		{
			switch (chunk->arg()->returnType())
			{
				case (NuVTypes::IntegerData):
					rv.set( atoi(bit.get()) );
					break;
				case (NuVTypes::RealData):
					rv.set( atof(bit.get()) );
					break;
				case (NuVTypes::StringData):
					rv.set( bit.get() );
					break;
				default:
					printf("Internal Error: Formatted conversion to %s is not possible.\n", NuVTypes::aDataType(chunk->arg()->returnType()));
					nparsed = -1;
					break;
			}
			if (nparsed == -1) break;
			chunk->arg()->set( rv );
		}
	}
	msg.exit("NuFormat::read");
	return nparsed;
}

// Return last written string
const char *NuFormat::string()
{
	return createdString_;
}

// Write format to internal string
bool NuFormat::writeToString()
{
	msg.enter("NuFormat::writeToString");
	printf("NuFormat::writeToString not implemented yet.\n");
	createdString_[0] = '\0';
	msg.exit("NuFormat::writeToString");
	return FALSE;
}

// Read line and parse according to format
int NuFormat::readFormatted(const char *line, int flags)
{
	msg.enter("NuFormat::readFormatted[string]");
	static LineParser parser;
	parser.setLine(line);
	int result = read(&parser, flags);
	msg.exit("NuFormat::readFormatted[string]");
	return result;
}

// Read line from file and parse according to format
int NuFormat::readFormatted(LineParser *parser, int flags)
{
	msg.enter("NuFormat::readFormatted[file]");
	// Read a new line using the supplied parser
	if (parser == NULL)
	{
		printf("Internal Error: No LineParser given to NuFormat::readFormatted.\n");
		msg.exit("NuFormat::readFormatted[file]");
		return 1;
	}
	// Get next line from file
	int result = parser->readLine();
	if (result == 0) result = read(parser, flags);
	msg.exit("NuFormat::readFormatted[file]");
	return result;
}

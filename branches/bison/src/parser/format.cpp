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
// Format
*/

// Constructor
NuFormat::NuFormat(const char *s, Refitem<TreeNode,int> *firstarg)
{
	// Private variables
	isValid_ = TRUE;

	// Step through formatting string, looking for '%' symbols (terminated by a non-alpha)
	const char *c = s;
	bool isformatter = FALSE;
	Refitem<TreeNode,int> *arg = firstarg;
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
			continue;
		}
		// If we're currently in the middle of a formatter, it's terminated by an alpha character
		if (isformatter && (isalpha(*c)))
		{
			length ++;
			// If the terminating character is 'l', 'h', or 'L', don't break yet..
			if ((*c == 'l') || (*c == 'h') || (*c == 'L')) continue;
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
			chunks_.own( new FormatChunk(FormatChunk::FormattedChunk, format, node) );
			delete[] format;
			continue;
		}
		// Increment length
		length++;
	} while (c != NULL);
}

// Destructor
NuFormat::~NuFormat()
{
}

/*
// Format - Read/Write
*/

// Use specified parser to perform formatted read
int NuFormat::read(LineParser *parser, int flags)
{
	msg.enter("NuFormat::read");
	for (FormatChunk *chunk = chunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		printf("Formatting for read...\n");
	}
	msg.exit("NuFormat::read");
}

// Return last written string
const char *NuFormat::string()
{
	return createdString_;
}

// Write format to internal string
bool NuFormat::writeToString()
{

}

// Read line and parse according to format
int NuFormat::readFormatted(const char *line, int flags)
{
	msg.enter("NuFormat::readFormatted[string]");
	LineParser parser;
	// Set line in specified parser
	msg.exit("NuFormat::readFormatted[string]");
}

// Read line from file and parse according to format
int NuFormat::readFormatted(LineParser *parser, int flags)
{
	msg.enter("NuFormat::readFormatted[file]");
	// Set line in specified parser
	msg.exit("NuFormat::readFormatted[file]");
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
	arg_ = NULL;

	// Public variables
	next = NULL;
	prev = NULL;
}

FormatChunk::FormatChunk(FormatChunk::ChunkType type, const char *format, TreeNode *node) : type_(type), arg_(node)
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
TreeNode *FormatChunk::arg()
{
	return arg_;
}


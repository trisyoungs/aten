/*
	*** String formatter
	*** src/parser/format.cpp
	Copyright T. Youngs 2007-2015

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
#include <QRegularExpression>

ATEN_USING_NAMESPACE

/*
 * Format Chunks
 */

// Constructor
FormatChunk::FormatChunk(ChunkType type, QString cFormat, TreeNode* arg, VTypes::DataType retrieveType) : ListItem<FormatChunk>()
{
	// Private variables
	type_ = type;
	cFormat_ = cFormat;
	arg_ = arg;
	retrieveType_ = retrieveType;
	formatLength_ = 0;
	Messenger::print(Messenger::Parse, "...created FormatChunk for string '%s'", qPrintable(cFormat_));

	// Find length specifier in format (if there is one)
	if (cFormat_.length() > 1)
	{
		QRegularExpression re("[-\\d]+");
		QRegularExpressionMatch match = re.match(cFormat);
		if (match.hasMatch()) formatLength_ = match.captured(0).toInt();
	}
}

// Return chunktype
FormatChunk::ChunkType FormatChunk::type()
{
	return type_;
}

// Return C-style format string *or* plain text data if chunktype is PlainTextChunk
QString FormatChunk::cFormat()
{
	return cFormat_;
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
TreeNode* FormatChunk::arg()
{
	return arg_;
}

// Return variable type to retrieve variable data as
VTypes::DataType FormatChunk::retrieveType()
{
	return retrieveType_;
}

/*
 * Format
 */

// Constructor
Format::Format(RefListItem<TreeNode,int>* firstarg)
{
	// Construct a delimited list of chunks with no specific format
	for (RefListItem<TreeNode,int>* ri = firstarg; ri != NULL; ri = ri->next) addDelimitedChunk(ri->item);
	delimited_ = true;
	isValid_ = true;
}

// Constructor
Format::Format(QString cFormat, RefListItem<TreeNode,int>* firstarg)
{
	// Private variables
	isValid_ = true;
	delimited_ = false;

	// Step through formatting string, looking for '%' symbols (terminated by a non-alpha)
	int pos = 0;
	QChar prevChar, prevPrevChar;
	QString plainText;
	VTypes::DataType type;
	bool isFormatter = false, isDiscarder, restOfLine;
	RefListItem<TreeNode,int>* arg = firstarg;
	Messenger::print(Messenger::Parse, "Creating Format object from string '%s' (and any supplied arguments)...", qPrintable(cFormat));
	do
	{
		// If we find a '%' store any previous characters as a plain-text chunk and begin a formatted chunk
		if ((cFormat[pos] == '%') && (cFormat[pos+1] == '%'))
		{
			// Consecutive '%' indicates just a plain '%' - skip on one char and continue
			pos += 2;
			plainText += '%';
			prevChar = '%';
			continue;
		}
		else if (cFormat[pos] == '%')
		{
			// Check for a previous format, in which case this one is mangled
			if (isFormatter)
			{
				Messenger::print("Found an unterminated format specifier (%) in format string '%s'.", qPrintable(cFormat));
				isValid_ = false;
				return;
			}
			
			if (!plainText.isEmpty())
			{
				// Store previous chunk, and start a new one
				addPlainTextChunk(plainText);
				plainText.clear();
			}
			isFormatter = true;
			plainText = '%';
			++pos;
		}

		// Increment character position...
		plainText += cFormat.at(pos);
		prevChar = cFormat.at(pos);
		++pos;

		// If we're currently in the middle of a formatter, it's terminated by an alpha character or '*'
		if (isFormatter && (prevChar.isLetter() || (prevChar == '*')))
		{
			// If the current character is 'l', 'h', or 'L' don't terminate yet
			if ((prevChar == 'l') || (prevChar == 'h') || (prevChar == 'L')) continue;

			Messenger::print(Messenger::Parse, "Detected format bit [%s]", qPrintable(plainText));

			// Check the terminating character to make sure that its one we recognise *and* is compatible with the type of argument given
			if ((arg == NULL) && (prevChar != '*'))
			{
				Messenger::print("Formatter '%s' in string has no corresponding argument.", qPrintable(plainText));
				isValid_ = false;
				break;
			}
			else
			{
				isDiscarder = false;
				restOfLine = false;
				type = arg == NULL ? VTypes::NoData : arg->item->returnType();
				prevPrevChar = plainText.at(plainText.length()-2);
				if (!prevPrevChar.isLetter()) prevPrevChar = '\0';
				switch (prevChar.toLatin1())
				{
					// Integer types
					case ('i'):
					case ('d'):
					case ('x'):
					case ('u'):
						// If a preceeding 'l' was specified, then we must have a pointer
						if (prevPrevChar == 'l')
						{
							if (type >= VTypes::AtenData) break;
							Messenger::print("Format '%s' expects a pointer, but has been given %s.", qPrintable(plainText), VTypes::aDataType(type));
							isValid_ = false;
						}
						else if ((prevPrevChar == '\0') || (prevPrevChar == 'h'))
						{
							if (type == VTypes::IntegerData) break;
							Messenger::print("Format '%s' expects an integer, but has been given %s.", qPrintable(plainText), VTypes::aDataType(type));
							isValid_ = false;
						}
						else
						{
							Messenger::print("Integer format '%c' cannot be preceeded by the identifier '%c'.", prevChar.toLatin1(), prevPrevChar.toLatin1());
							isValid_ = false;
						}
						break;
					// Floating-point types
					case ('e'):
					case ('f'):
					case ('g'):
						// If a preceeding 'L' was specified, we complain!
						if (prevPrevChar == 'L')
						{
							Messenger::print("Output of long doubles (prefixing a floating-point formatter with 'L') is not supported.");
							isValid_ = false;
						}
						else if (prevPrevChar == '\0')
						{
							if (type == VTypes::DoubleData) break;
							Messenger::print("Format '%s' expects a real, but has been given %s.", qPrintable(plainText), VTypes::aDataType(type));
							isValid_ = false;
						}
						else
						{
							Messenger::print("Floating-point format '%c' cannot be preceeded by the identifier '%c'.", prevChar.toLatin1(), prevPrevChar.toLatin1());
							isValid_ = false;
						}
						break;
					// Character types
					case ('r'):
						restOfLine = true;
					case ('s'):
						if (prevPrevChar != '\0')
						{
							Messenger::print("String format '%c' cannot be preceeded by the identifier '%c'.", prevChar.toLatin1(), prevPrevChar.toLatin1());
							isValid_ = false;
						}
						if (type == VTypes::StringData) break;
						Messenger::print("Format '%s' expects a string, but has been given %s.", qPrintable(plainText), VTypes::aDataType(type));
						isValid_ = false;
						break;
					case ('c'):
						Messenger::print("Character format 'c'is not supported.");
						isValid_ = false;
						break;
					// Discard identifier
					case ('*'):
						isDiscarder = true;
						type = VTypes::NoData;
						break;
					default:
						Messenger::print("Unsupported format '%s'.", qPrintable(plainText));
						isValid_ = false;
						break;
				}
			}
			// Don't use up a variable argument if the specifier was '*'
			TreeNode* node = (arg == NULL ? NULL : arg->item);
			if (restOfLine) addGreedyDelimitedChunk(node, type);
			else addFormattedChunk(plainText, node, type);
			if (!isDiscarder) arg = arg->next;
			plainText.clear();
			isFormatter = false;
		}
	} while (pos < cFormat.count());

	// Do we have some text left over?
	if (!plainText.isEmpty()) addPlainTextChunk(plainText);

	// Are there any supplied arguments remaining?
	if (arg != NULL) Messenger::print("Warning: Extra data arguments given to format '%s'...", qPrintable(cFormat));
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
void Format::addPlainTextChunk(QString plainText)
{
	FormatChunk* chunk = new FormatChunk(FormatChunk::PlainTextChunk, plainText);
	chunks_.own(chunk);
}

// Add new formatted chunk to format
void Format::addFormattedChunk(QString cFormat, TreeNode* arg, VTypes::DataType retrievetype)
{
	FormatChunk* chunk = new FormatChunk(FormatChunk::FormattedChunk, cFormat, arg, retrievetype);
	chunks_.own(chunk);
}

// Add new delimited chunk to format
void Format::addDelimitedChunk(TreeNode* arg)
{
	if (arg == NULL)
	{
		printf("Internal Error: Tried to create a DelimitedChunk from a NULL argument.\n");
		return;
	}
	FormatChunk* chunk = new FormatChunk(FormatChunk::DelimitedChunk, NULL, arg, arg->returnType());
	chunks_.own(chunk);
}

// Add new greedy delimited chunk to format
void Format::addGreedyDelimitedChunk(TreeNode* arg, VTypes::DataType retrievetype)
{
	FormatChunk* chunk = new FormatChunk(FormatChunk::GreedyDelimitedChunk, NULL, arg, retrievetype);
	chunks_.own(chunk);
}

/*
// Format - Read/Write
*/

// Use specified parser to perform formatted read
int Format::executeRead(LineParser* parser, int optionMask)
{
	Messenger::enter("Format::executeRead");
	int nparsed = 0, length;
	ReturnValue rv;
	QString bit;

	// Cycle through the list of FormatChunks
	for (FormatChunk* chunk = chunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		// Retrieve the required characters from the input stream
		switch (chunk->type())
		{
			case (FormatChunk::PlainTextChunk):
				// Skip as many characters as there are in the string
				length = chunk->textLength();
				parser->getNextN(optionMask, length, bit);
				break;
			case (FormatChunk::DelimitedChunk):
				// Get next delimited argument from LineParser
				parser->getNextArg(optionMask, bit);
				if (!bit.isEmpty()) ++nparsed;
				break;
			case (FormatChunk::GreedyDelimitedChunk):
				// Get rest of line, starting from next delimited argument
				parser->getRestDelim(bit);
				if (!bit.isEmpty()) ++nparsed;
				break;
			case (FormatChunk::FormattedChunk):
				// Get argument from LineParser
				length = chunk->formatLength();
				if (length != 0) parser->getNextN(optionMask, length, bit);
				else parser->getNextArg(optionMask, bit);
				if (!bit.isEmpty()) ++nparsed;
				break;
			default:
				printf("Internal Error: Action for this type of format chunk (%i) has not been defined.\n", chunk->type());
				Messenger::exit("Format::executeRead");
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
					rv.set(bit.toInt());
					break;
				case (VTypes::DoubleData):
					rv.set(bit.toDouble());
					break;
				case (VTypes::StringData):
					rv.set(bit);
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
	Messenger::exit("Format::executeRead");
	return nparsed;
}

// Return last written string
QString Format::string()
{
	return createdString_;
}

// Write format to internal string
bool Format::writeToString()
{
	Messenger::enter("Format::writeToString");
	QString bit;
	createdString_.clear();
	ReturnValue rv;
	bool result = true;

	// Cycle through the list of FormatChunks
	for (FormatChunk* chunk = chunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		// Retrieve the required characters from the input stream
		switch (chunk->type())
		{
			case (FormatChunk::FormattedChunk):
				result = chunk->arg()->execute(rv);
				if (!result) break;
				// Check for arrays - bad!
				if (rv.arraySize() != -1)
				{
					Messenger::print("Error: Array passed to format.");
					result = false;
					break;
				}
				bit[0] = '\0';
				switch (chunk->retrieveType())
				{
					case (VTypes::IntegerData):
						bit.sprintf(qPrintable(chunk->cFormat()), rv.asInteger());
						break;
					case (VTypes::DoubleData):
						bit.sprintf(qPrintable(chunk->cFormat()), rv.asDouble());
						break;
					case (VTypes::StringData):
						bit.sprintf(qPrintable(chunk->cFormat()), qPrintable(rv.asString()));
						break;
					default:
						// Pointer types
						bit.sprintf(qPrintable(chunk->cFormat()), rv.asPointer(chunk->retrieveType()));
						break;
				}
				createdString_ += bit;
				break;
			case (FormatChunk::PlainTextChunk):
				createdString_ += chunk->cFormat();
				result = true;
				break;
			case (FormatChunk::DelimitedChunk):
				result = chunk->arg()->execute(rv);
				if (!result) break;
				// Check for arrays - bad!
				if (rv.arraySize() != -1)
				{
					Messenger::print("Error: Array passed to format.");
					result = false;
					break;
				}
				createdString_ += rv.asString();
				if (chunk->next != NULL) createdString_ += " ";
				break;
			default:
				printf("Internal Error: Action for this type of format chunk has not been defined.\n");
				Messenger::exit("Format::writeToString");
				return false;
		}
		if (!result) break;
	}

	// If this was originally a delimited chunk, append a newline
	if (delimited_) createdString_ += "\n";

	Messenger::exit("Format::writeToString");
	return result;
}

// Parse supplied line according to format
int Format::read(QString line, int optionMask)
{
	Messenger::enter("Format::read[string]");
	static LineParser parser;
	parser.setLine(line);
	int result = executeRead(&parser, optionMask);
	Messenger::exit("Format::read[string]");
	return result;
}

// Read line from file and parse according to format
int Format::read(LineParser* parser, int optionMask)
{
	Messenger::enter("Format::read[file]");

	// Read a new line using the supplied parser
	if (parser == NULL)
	{
		printf("Internal Error: No LineParser given to Format::read.\n");
		Messenger::exit("Format::read[file]");
		return 1;
	}

	// Get next line from file and parse line
	int result = parser->readNextLine(optionMask);
	if (result == 0) result = executeRead(parser, optionMask);

	Messenger::exit("Format::read[file]");
	return result;
}

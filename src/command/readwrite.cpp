/*
	*** Read/write Commands
	*** src/command/readwrite.cpp
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

#include "parser/commandnode.h"
#include "parser/tree.h"
#include "parser/variablenode.h"
#include "parser/double.h"
#include "parser/integer.h"

ATEN_USING_NAMESPACE

// Add file read option
bool Commands::function_AddReadOption(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Get parse option from variable
	Parser::ParseOption po = Parser::parseOption(c->argc(0));
	if (po != Parser::nParseOptions) c->parent()->addReadOption(po);
	return (po != Parser::nParseOptions);
}

// Check for end of file (or nothing remaining but whitespace)
bool Commands::function_Eof(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (c->parent()->parser() == NULL)
	{
		Messenger::print("Error: Tried to call 'eof' without a valid filesource.");
		return false;
	}
	else if (!c->parent()->parser()->isFileGoodForReading()) rv.set(true);
	else rv.set(c->parent()->parser()->eofOrBlank());
	return true;
}

// Return source/destination filename for filter
bool Commands::function_FilterFileName(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (c->parent()->parser() == NULL)
	{
		Messenger::print("Error: Tried to call 'filterFilename' without a valid filesource.");
		return false;
	}
	else if (!c->parent()->parser()->isFileGoodForReading())
	{
		Messenger::print("The 'filterFilename' command can only be used from within an import filter.");
		return false;
	}
	rv.set(c->parent()->parser()->inputFilename());
	return true;
}

// Search for line containing specified string
bool Commands::function_Find(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'find' command.");
		return false;
	}

	// Store current stream position in case the string is not found
	std::streampos currentpos = c->parent()->parser()->tellg();
	bool found = false;
	do
	{
		// Get line from file
		int result = c->parent()->parser()->readNextLine(c->parent()->readOptions());
		if (result != 0) break;

		// Check for string
		if (c->parent()->parser()->line().contains(c->argc(0)))
		{
			found = true;

			// Store the line if a second argument was given
			if (c->hasArg(1))
			{
				ReturnValue val(c->parent()->parser()->line());
				c->setArg(1, val);
			}
			break;
		}
	} while (1);

	// Rewind file to previous position if not found
	if (!found) c->parent()->parser()->seekg(currentpos);
	rv.set( found ? 1 : 0 );
	return true;
}

// Read line from file and return it as result
bool Commands::function_GetLine(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'getLine' command.");
		return false;
	}
	int result = c->parent()->parser()->readNextLine(c->parent()->readOptions());
	ReturnValue val;
	if (result == 0) val.set(c->parent()->parser()->line());
	else val.set("");
	c->setArg(0, val);
	rv.set(result);
	return true;
}

// Get next whitespace-delimited argument from current file
bool Commands::function_NextArg(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readNext' command.");
		return false;
	}
	QString arg;
	rv.set( c->parent()->parser()->getCharsDelim(arg) );
	ReturnValue argrv;
	argrv.set(arg);
	c->setArg(0, argrv);
	return true;
}

// Get next whitespace-delimited argument from specified variable file
bool Commands::function_NextVariableArg(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	QString source = c->argc(0);
	QString arg;
	rv.set( c->parent()->parser()->getCharsDelim(c->parent()->readOptions(), source, arg) );
	ReturnValue argrv;
	argrv.set(arg);
	c->setArg(1, argrv);
	argrv.set(source);
	c->setArg(0, argrv);
	return true;
}

// Peek next character from file
bool Commands::function_PeekChar(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'peekChar' command.");
		return false;
	}
	char s[2];
	s[0] = c->parent()->parser()->peek();
	s[1] = '\0';
	rv.set(s);
	Messenger::print(Messenger::Commands,"Peek got character '%i'", s[0]);
	return true;
}

// Peek next character from file, returning it as an integer
bool Commands::function_PeekCharI(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'peekCharI' command.");
		return false;
	}
	char s[2];
	s[0] = c->parent()->parser()->peek();
	s[1] = '\0';
	rv.set(s);
	Messenger::print(Messenger::Commands,"Peek got character '%i'", s[0]);
	return true;
}

// Read N characters from unformatted file
bool Commands::function_ReadChars(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readChars' command.");
		return false;
	}
	if (c->hasArg(1)) rv.set( c->parent()->parser()->getChars(c->argi(0), c->argb(1)) );
	else rv.set( c->parent()->parser()->getChars(c->argi(0)) );
	Messenger::print(Messenger::Commands,"Unformatted char read got '%s'", qPrintable(rv.asString()));
	return true;
}

// Read real value from unformatted file
bool Commands::function_ReadDouble(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readDouble' command.");
		return false;
	}
	rv.set( c->parent()->parser()->getDouble(c->hasArg(0) ? c->argi(0) : 0) );
	Messenger::print(Messenger::Commands,"Unformatted double read got '%f'", rv.asDouble());
	return true;
}

// Read real array from unformatted file
bool Commands::function_ReadDoubleArray(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readDoubleArray' command.");
		return false;
	}
	// Get the array pointer from the supplied argument, and check size against number of items requested
	int count = c->argi(1);
	if (c->argNode(0)->nodeType() != TreeNode::VarWrapperNode)
	{
		Messenger::print("Error: First argument to 'readDoubleArray' is not a variable.");
		return false;
	}
	Variable* v = ((VariableNode*) c->argNode(0))->variable();
	if ((v->nodeType() != TreeNode::ArrayVarNode) || (v->returnType() != VTypes::DoubleData))
	{
		Messenger::print("Error: Variable argument to 'readDoubleArray' is not an array of doubles.");
		return false;
	}
	DoubleArrayVariable* av = (DoubleArrayVariable*) ((VariableNode*) c->argNode(0))->variable();
	if (count > av->arraySize())
	{
		Messenger::print("Error: Requested number of data for 'readDoubleArray' (%i) exceeds size of supplied array (%i).", count, av->arraySize());
		return false;
	}
	rv.set( c->parent()->parser()->getDoubleArray( av->arrayData(), count) );
	Messenger::print(Messenger::Commands,"Unformatted double read returned code %i.", rv.asInteger());
	return true;
}

// Read integer from unformatted file
bool Commands::function_ReadInteger(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readInteger' command.");
		return false;
	}
	rv.set( c->parent()->parser()->getInteger( c->hasArg(0) ? c->argi(0) : 0 ) );
	Messenger::print(Messenger::Commands,"Unformatted integer read got '%i'", rv.asInteger());
	return true;
}

// Read integer array from unformatted file
bool Commands::function_ReadIntegerArray(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readIntegerArray' command.");
		return false;
	}
	// Get the array pointer from the supplied argument, and check size against number of items requested
	int count = c->argi(1);
	if (c->argNode(0)->nodeType() != TreeNode::VarWrapperNode)
	{
		Messenger::print("Error: First argument to 'readIntegerArray' is not a variable.");
		return false;
	}
	Variable* v = ((VariableNode*) c->argNode(0))->variable();
	if ((v->nodeType() != TreeNode::ArrayVarNode) || (v->returnType() != VTypes::IntegerData))
	{
		Messenger::print("Error: Variable argument to 'readIntegerArray' is not an array of integers.");
		return false;
	}
	IntegerArrayVariable* av = (IntegerArrayVariable*) ((VariableNode*) c->argNode(0))->variable();
	if (count > av->arraySize())
	{
		Messenger::print("Error: Requested number of data for 'readIntegerArray' (%i) exceeds size of supplied array (%i).", count, av->arraySize());
		return false;
	}
	rv.set( c->parent()->parser()->getIntegerArray( av->arrayData(), count) );
	Messenger::print(Messenger::Commands,"Unformatted integer read returned code %i.", rv.asInteger());
	return true;
}

// Read line and parse with format
bool Commands::function_ReadLine(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readLine' command.");
		return false;
	}
	Format* format = c->createFormat(-1,0);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readLine'.\n");
		return false;
	}
	rv.set( format->read( c->parent()->parser(), c->parent()->readOptions() ) );
	return true;
}

// Read line and parse with format
bool Commands::function_ReadLineFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readLineF' command.");
		return false;
	}
	Format* format = c->createFormat(0,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readLineF'.\n");
		return false;
	}
	rv.set( format->read( c->parent()->parser(), c->parent()->readOptions() ) );
	return true;
}

// Get next whitespace-delimited argument from current line
bool Commands::function_ReadNext(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'readNext' command.");
		return false;
	}
	QString arg;
	rv.set( c->parent()->parser()->getArgDelim(c->parent()->readOptions(), arg));
	ReturnValue argrv;
	argrv.set(arg);
	c->setArg(0, argrv);
	return true;
}

// Parse given variable using delimiters
bool Commands::function_ReadVariable(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(-1,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readVar'.\n");
		return false;
	}
	rv.set( format->read( c->argc(0), c->parent()->readOptions() ) );
	return true;
}

// Parse given variable with format
bool Commands::function_ReadVariableFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(1,2);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readVarF'.\n");
		return false;
	}
	rv.set( format->read( c->argc(0), c->parent()->readOptions() ) );
	return true;
}

// Remove file read option
bool Commands::function_RemoveReadOption(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Get parse option from variable
	Parser::ParseOption po = Parser::parseOption(c->argc(0));
	if (po != Parser::nParseOptions) c->parent()->removeReadOption(po);
	return (po != Parser::nParseOptions);
}

// Go to start of current file
bool Commands::function_Rewind(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->parser()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'rewind' command.");
		return false;
	}
	c->parent()->parser()->rewind();
	return true;
}

// Discard N characters from unformatted file
bool Commands::function_SkipChars(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'skipChars' command.");
		return false;
	}
	c->parent()->parser()->skipChars(c->argi(0));
	return true;
}

// Skip line(s) of file
bool Commands::function_SkipLine(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		Messenger::print("No valid filesource available for the 'skipLine' command.");
		return false;
	}
	c->parent()->parser()->skipLines( c->hasArg(0) ? c->argi(0) : 1 );
	return true;
}

// Write line without format, delimiting arguments with spaces
bool Commands::function_WriteLine(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForWriting())
	{
		Messenger::print("No valid filesource available for the 'writeLine' command.");
		return false;
	}
	Format* format = c->createFormat(-1,0);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writeLine'.\n");
		return false;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	c->parent()->parser()->writeLine(format->string());
	return true;
}

// Write line with C-style format
bool Commands::function_WriteLineFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForWriting())
	{
		Messenger::print("No valid filesource available for the 'writeLineF' command.");
		return false;
	}
	Format* format = c->createFormat(0,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writeLineF'.\n");
		return false;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	c->parent()->parser()->writeLine(format->string());
	return true;
}

// Write delimited line to variable
bool Commands::function_WriteVariable(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(-1,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writevar'.\n");
		return false;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	ReturnValue string;
	string.set(format->string());
	c->setArg(0, string);
	return true;
}

// Write formatted line to variable
bool Commands::function_WriteVariableFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(1,2);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writevarf'.\n");
		return false;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	ReturnValue string;
	string.set(format->string());
	c->setArg(0, string);
	return true;
}


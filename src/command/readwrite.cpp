/*
	*** Read/write Commands
	*** src/command/readwrite.cpp
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

#include "parser/commandnode.h"
#include "parser/tree.h"
#include "parser/variablenode.h"
#include "parser/double.h"
#include "parser/integer.h"
#include <cstring>

// Add file read option
bool Command::function_AddReadOption(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get parse option from variable
	LineParser::ParseOption po = LineParser::parseOption(c->argc(0));
	if (po != LineParser::nParseOptions) c->parent()->addReadOption(po);
	return TRUE;
}

// Check for end of file (or nothing remaining but whitespace)
bool Command::function_Eof(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->parser()->isFileGood())
	{
		msg.print("No valid filesource available for the 'eof' command.\n");
		return FALSE;
	}
	rv.set(c->parent()->parser()->eofOrBlank());
	return TRUE;
}

// Return source/destination filename for filter
bool Command::function_FilterFileName(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->parser()->isFileGood())
	{
		msg.print("The filterfilename' command can only be used from within a Filter.\n");
		return FALSE;
	}
	rv.set(c->parent()->parser()->filename());
	return TRUE;
}

// Search for line containing specified string
bool Command::function_Find(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'find' command.\n");
		return FALSE;
	}
	int result;
	rv.set(0);
	do
	{
		// Get line from file
		int result = c->parent()->parser()->readLine();
		if (result != 0) break;
		// Check for string
		if (strstr(c->parent()->parser()->line(), c->argc(0)) != '\0')
		{
			rv.set(1);
			// Store the line if a second argument was given
			if (c->hasArg(1))
			{
				ReturnValue val(c->parent()->parser()->line());
				c->setArg(1, val);
			}
			break;
		}
	} while (1);
	return TRUE;
}

// Read line from file and return it as result
bool Command::function_GetLine(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'getline' command.\n");
		return FALSE;
	}
	int result = c->parent()->parser()->readLine();
	ReturnValue val;
	if (result == 0) val.set(c->parent()->parser()->line());
	else val.set("");
	c->setArg(0, val);
	rv.set(result);
	return TRUE;
}

// Peek next character from file
bool Command::function_PeekChar(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'peekchar' command.\n");
		return FALSE;
	}
	char s[2];
	s[0] = c->parent()->parser()->peek();
	s[1] = '\0';
	rv.set(s);
	msg.print(Messenger::Commands,"Peek got character '%c'\n", s[0]);
	return TRUE;
}

// Peek next character from file, returning it as an integer
bool Command::function_PeekCharI(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'peekchari' command.\n");
		return FALSE;
	}
	char s[2];
	s[0] = c->parent()->parser()->peek();
	s[1] = '\0';
	rv.set(s);
	msg.print(Messenger::Commands,"Peek got character '%c'\n", s[0]);
	return TRUE;
}

// Read N characters from unformatted file
bool Command::function_ReadChars(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readchars' command.\n");
		return FALSE;
	}
	if (c->hasArg(1)) rv.set( c->parent()->parser()->getChars(c->argi(0), c->argb(1)) );
	else rv.set( c->parent()->parser()->getChars(c->argi(0)) );
	msg.print(Messenger::Commands,"Unformatted char read got '%s'\n", rv.asString());
	return TRUE;
}

// Read real value from unformatted file
bool Command::function_ReadDouble(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readdouble' command.\n");
		return FALSE;
	}
	rv.set( c->parent()->parser()->getDouble(c->hasArg(0) ? c->argi(0) : 0) );
	msg.print(Messenger::Commands,"Unformatted double read got '%f'\n", rv.asDouble());
	return TRUE;
}

// Read real array from unformatted file
bool Command::function_ReadDoubleArray(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readdoublearray' command.\n");
		return FALSE;
	}
	// Get the array pointer from the supplied argument, and check size against number of items requested
	int count = c->argi(1);
	if (c->argNode(0)->nodeType() != TreeNode::VarWrapperNode)
	{
		msg.print("Error: First argument to 'readdoublearray' is not a variable.\n");
		return FALSE;
	}
	Variable *v = ((VariableNode*) c->argNode(0))->variable();
	if ((v->nodeType() != TreeNode::ArrayVarNode) || (v->returnType() != VTypes::DoubleData))
	{
		msg.print("Error: Variable argument to 'readdoublearray' is not an array of doubles.\n");
		return FALSE;
	}
	DoubleArrayVariable *av = (DoubleArrayVariable*) ((VariableNode*) c->argNode(0))->variable();
	if (count > av->arraySize())
	{
		msg.print("Error: Requested number of data for 'readdoublearray' (%i) exceeds size of supplied array (%i).\n", count, av->arraySize());
		return FALSE;
	}
	rv.set( c->parent()->parser()->getDoubleArray( av->arrayData(), count) );
	msg.print(Messenger::Commands,"Unformatted double read %s.\n", rv.asInteger() ? "succeeded" : "failed");
	return TRUE;
}

// Read integer from unformatted file
bool Command::function_ReadInteger(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readinteger' command.\n");
		return FALSE;
	}
	rv.set( c->parent()->parser()->getInteger( c->hasArg(0) ? c->argi(0) : 0 ) );
	msg.print(Messenger::Commands,"Unformatted integer read got '%i'\n", rv.asInteger());
	return TRUE;
}

// Read integer array from unformatted file
bool Command::function_ReadIntegerArray(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readintegerarray' command.\n");
		return FALSE;
	}
	// Get the array pointer from the supplied argument, and check size against number of items requested
	int count = c->argi(1);
	if (c->argNode(0)->nodeType() != TreeNode::VarWrapperNode)
	{
		msg.print("Error: First argument to 'readintegerarray' is not a variable.\n");
		return FALSE;
	}
	Variable *v = ((VariableNode*) c->argNode(0))->variable();
	if ((v->nodeType() != TreeNode::ArrayVarNode) || (v->returnType() != VTypes::IntegerData))
	{
		msg.print("Error: Variable argument to 'readintegerarray' is not an array of integers.\n");
		return FALSE;
	}
	IntegerArrayVariable *av = (IntegerArrayVariable*) ((VariableNode*) c->argNode(0))->variable();
	if (count > av->arraySize())
	{
		msg.print("Error: Requested number of data for 'readintegerarray' (%i) exceeds size of supplied array (%i).\n", count, av->arraySize());
		return FALSE;
	}
	rv.set( c->parent()->parser()->getIntegerArray( av->arrayData(), count) );
	msg.print(Messenger::Commands,"Unformatted integer read %s.\n", rv.asInteger() ? "succeeded" : "failed");
	return TRUE;
}

// Read line and parse with format
bool Command::function_ReadLine(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readline' command.\n");
		return FALSE;
	}
	Format *format = c->createFormat(-1,0);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readline'.\n");
		return FALSE;
	}
	rv.set( format->read( c->parent()->parser(), c->parent()->readOptions() ) );
	return TRUE;
}

// Read line and parse with format
bool Command::function_ReadLineFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readlinef' command.\n");
		return FALSE;
	}
	Format *format = c->createFormat(0,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readlinef'.\n");
		return FALSE;
	}
	rv.set( format->read( c->parent()->parser(), c->parent()->readOptions() ) );
	return TRUE;
}

// Get next whitespace-delimited argument from file
bool Command::function_ReadNext(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readnext' command.\n");
		return FALSE;
	}
	Dnchar arg;
	rv.set( c->parent()->parser()->getArgDelim(&arg, c->parent()->readOptions()));
	ReturnValue argrv;
	argrv.set(arg.get());
	c->setArg(0, argrv);
	return TRUE;
}

// Parse given variable using delimiters
bool Command::function_ReadVariable(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *format = c->createFormat(-1,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readvar'.\n");
		return FALSE;
	}
	rv.set( format->read( c->argc(0), c->parent()->readOptions() ) );
	return TRUE;
}

// Parse given variable with format
bool Command::function_ReadVariableFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *format = c->createFormat(1,2);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readvarf'.\n");
		return FALSE;
	}
	rv.set( format->read( c->argc(0), c->parent()->readOptions() ) );
	return TRUE;
}

// Remove file read option
bool Command::function_RemoveReadOption(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get parse option from variable
	LineParser::ParseOption po = LineParser::parseOption(c->argc(0));
	if (po != LineParser::nParseOptions) c->parent()->removeReadOption(po);
	return TRUE;
}

// Go to start of current file
bool Command::function_Rewind(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->parser()->isFileGood())
	{
		msg.print("No valid filesource available for the 'readchars' command.\n");
		return FALSE;
	}
	c->parent()->parser()->rewind();
	return TRUE;
}

// Discard N characters from unformatted file
bool Command::function_SkipChars(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'readchars' command.\n");
		return FALSE;
	}
	c->parent()->parser()->skipChars(c->argi(0));
	return TRUE;
}

// Skip line(s) of file
bool Command::function_SkipLine(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForReading())
	{
		msg.print("No valid filesource available for the 'skipline' command.\n");
		return FALSE;
	}
	c->parent()->parser()->skipLines( c->hasArg(0) ? c->argi(0) : 1 );
	return TRUE;
}

// Write line without format, delimiting arguments with spaces
bool Command::function_WriteLine(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForWriting())
	{
		msg.print("No valid filesource available for the 'writeline' command.\n");
		return FALSE;
	}
	Format *format = c->createFormat(-1,0);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writeline'.\n");
		return FALSE;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		msg.print("Failed to format string for output.\n");
		return FALSE;
	}
	c->parent()->parser()->writeLine(format->string());
	return TRUE;
}

// Write line with C-style format
bool Command::function_WriteLineFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFileGoodForWriting())
	{
		msg.print("No valid filesource available for the 'writelinef' command.\n");
		return FALSE;
	}
	Format *format = c->createFormat(0,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writelinef'.\n");
		return FALSE;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		msg.print("Failed to format string for output.\n");
		return FALSE;
	}
	c->parent()->parser()->writeLine(format->string());
	return TRUE;
}

// Write delimited line to variable
bool Command::function_WriteVariable(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *format = c->createFormat(-1,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writevar'.\n");
		return FALSE;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		msg.print("Failed to format string for output.\n");
		return FALSE;
	}
	ReturnValue string;
	string.set(format->string());
	c->setArg(0, string);
	return TRUE;
}

// Write formatted line to variable
bool Command::function_WriteVariableFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *format = c->createFormat(1,2);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writevarf'.\n");
		return FALSE;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		msg.print("Failed to format string for output.\n");
		return FALSE;
	}
	ReturnValue string;
	string.set(format->string());
	c->setArg(0, string);
	return TRUE;
}

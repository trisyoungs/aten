/*
	*** Command Parser
	*** src/parser/parser.h
	Copyright T. Youngs 2007-2017

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

#ifndef ATEN_COMMANDPARSER_H
#define ATEN_COMMANDPARSER_H

#include "base/lineparser.h"
#include "parser/tree.h"
#include "templates/reflist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Program;
class TreeNode;
class Aten;

// Parser
class CommandParser
{
	public:
	// Constructor / Destructor
	CommandParser();
	~CommandParser();
	// Symbolic tokens - array of corresponding values refers to Bison's tokens
	enum SymbolToken { AssignSymbol, GEQSymbol, LEQSymbol, CNEQSymbol, FNEQSymbol, PlusEqSymbol, MinusEqSymbol, TimesEqSymbol, DivideEqSymbol, PlusPlusSymbol, MinusMinusSymbol, AndSymbol, OrSymbol, nSymbolTokens };
	// Source of parser input
	enum ParserSource { StringSource, StringListSource, FileSource, nParserSources };
	// Friend declarations
	friend class Program;


	/*
	 * Link to Aten
	 */
	private:
	// Pointer to Aten
	static Aten* aten_;

	public:
	// Return pointer to Aten
	static Aten* aten();

	// Set pointer to Aten
	static void setAten(Aten* aten);


	/*
	 * Create / Execute
	 */
	private:
	// Character string source
	static QString stringSource_;
	// Character string list source
	static QStringList stringListSource_;
	// Character string list source and index
	static int stringListSourceIndex_;
	// Integer position in stringSource, total length of string, and starting position of current token/function
	static int stringPos_, stringLength_, tokenStart_, functionStart_;
	// Line parser
	static LineParser parser_;
	// Local string used to construct source info
	static QString sourceInfo_;
	// Current input type to parser
	static ParserSource source_;
	// Whether the next token to expect is a path step
	static bool expectPathStep_;
	// Current lexed name (if any)
	static QString lexedName_;

	public:
	// Reset structure ready for next source
	static void reset();
	// Parser lexer, called by yylex()
	static int lex();
	// Return current input source
	static ParserSource source();
	// Get next character from current input stream
	static char getChar();
	// Peek next character from current input stream
	static char peekChar();
	// 'Replace' last character read from current input stream
	static void unGetChar();
	// Clear all node data
	static void clear();
	// Print layout of current tree
	static void print();
	// Print error information and location
	static void printErrorInfo();
	// Set source information string (if not from a file)
	static void setSourceInfo(QString sourceInfo);
	// Return short info on the current parsing source (filename, line number etc.)
	static QString sourceInfo();
	// Return current lexed name (if any)
	static QString lexedName();


	/*
	 * Tree Data
	 */
	private:
	// Current program target
	static Program* program_;
	// Current tree (target of node creation)
	static Tree* tree_;
	// Stack of created trees
	static RefList<Tree,bool> stack_;
	// Flag to indicate failure in tree generation (set by tree() function)
	static bool failed_;
	// Whether to generate program quietly (i.e. don't print any error messages)
	static bool quiet_;
	// Perform tree generation (base function, called by generateFrom*)
	static bool generate();
	// Populate target Program from specified character string
	static bool generateFromString(Program* prog, QString string, QString sourceInfo, bool pushTree = true, bool clearExisting = true, bool quiet = false);
	// Populate target Program from specified string list
	static bool generateFromStringList(Program* prog, QStringList stringList, QString sourceInfo, bool pushTree = true, bool clearExisting = true, bool quiet = false);
	// Populate target Program from specified file(name)
	static bool generateFromFile(Program* prog, QString filename, bool pushTree = true, bool clearExisting = true, bool quiet = false);

	public:
	// Return whether to generate program quietly (i.e. don't print any error messages)
	static bool quiet();
	// Return current Tree target, raising warning and setting fail flag if no tree is defined...
	static Tree* tree();
	// Push function
	static Tree* pushFunction(QString name, VTypes::DataType returntype);
	// Pop tree (or function) from stack
	static void popTree();
	// Discard current tree and its contents
	static void deleteCurrentTree();
};

ATEN_END_NAMESPACE

#endif

/*
	*** Base Parser
	*** src/parser/parser.h
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

#ifndef ATEN_NUPARSER_H
#define ATEN_NUPARSER_H

#include "templates/reflist.h"
#include "base/dnchar.h"
#include <fstream>

// Forward declarations
class Forest;
class Tree;

// Parser
class NuParser
{
	public:
	// Constructor / Destructor
	NuParser();
	~NuParser();
	// Symbolic tokens - array of corresponding values refers to Bison's tokens
	enum SymbolToken { AssignSymbol, GEQSymbol, LEQSymbol, CNEQSymbol, FNEQSymbol, PlusEqSymbol, MinusEqSymbol, TimesEqSymbol, DivideEqSymbol, nSymbolTokens };

	/*
	// Create / Execute
	*/
	private:
	// Character string source
	Dnchar stringSource_;
	// Integer position in stringSource, and total length of string
	int stringPos_, stringLength_;
	// File source
	std::ifstream *fileSource_;
	// Line number in source file that we've just read
	int lineNumber_;
	// Whether the current input source is a file or not
	bool isFileSource_;
	// Whether the next token to expect is a path step
	bool expectPathStep_;

	public:
	// Parser lexer, called by yylex()
	int lex();
	// Return whether the current input stream is a file or not
	bool isFileSource();
	// Get next character from current input stream
	char getChar();
	// Peek next character from current input stream
	char peekChar();
	// 'Replace' last character read from current input stream
	void unGetChar();
	// Clear all node data
	void clear();
	// Print layout of current tree
	void print();
	// Print error information and location
	void printErrorInfo();


	/*
	// Tree Data
	*/
	private:
	// Current forest target
	Forest *forest_;

	public:
	// Current tree (target of node creation)
	Tree *tree;
	// Create a new tree in the forest
	void createTree();
	// Create a new function tree in the forest
	void createFunction();


	/*
	// Tree Generation
	*/
	public:
	// Fill target forest from specified character string
	bool generate(Forest *f, const char *s);
	// Fill target forest from specified ifstream
	bool generate(Forest *f, ifstream *file);
};

// External declaration
extern NuParser nuparser;

#endif

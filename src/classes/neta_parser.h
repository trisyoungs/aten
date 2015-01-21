/*
	*** NETA Description Parser
	*** src/classes/neta_parser.h
	Copyright T. Youngs 2007-2013

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

#ifndef ATEN_NETAPARSER_H
#define ATEN_NETAPARSER_H

#include "base/lineparser.h"
#include "classes/neta.h"
#include "templates/list.h"
#include "templates/reflist.h"

// Forward declarations
class Neta;

// NETA parser/creator
class NetaParser
{
	public:
	// Constructor
	NetaParser();
	// Symbolic tokens - array of corresponding values refers to Bison's tokens
	enum NetaSymbolToken { GEQSymbol, LEQSymbol, CNEQSymbol, FNEQSymbol, nNetaSymbolTokens };

	/*
	// Character Stream
	*/
	private:
	// Character string source
	Dnchar stringSource_;
	// Integer position in stringSource, total length of string, and starting position of current token/function
	int stringPos_, stringLength_, tokenStart_, functionStart_;
	// Line parser
	LineParser parser_;
	// Line number in source file that we've just read
	int lineNumber_;
	// Whether the current input source is a file or not
	bool isFileSource_;

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
	// Node Functions
	*/
	private:
	// Node context stack
	Reflist<NetaContextNode,int> contextStack_;
	// Current NETA structure target
	Neta *neta_;
	// Current NETA target structure parent
	Forcefield *targetParentForcefield_;
	// Name of last unrecognised token
	Dnchar lastUnknownToken_;

	public:
	// Pop topmost node of contextStack_
	void popContext();
	// Return name of last unrecognised token
	const char *lastUnknownToken();
	// Create guts of within supplied NETA structure from supplied character element and string
	bool createNeta(Neta *target, const char *netastring, Forcefield *ffparent);
	// Set description (called by lexer)
	void setDescription(NetaNode *desc);
	// Join two nodes together
	NetaNode *join(Neta::NetaLogicType logic, NetaNode *node1, NetaNode *node2);
	// Link two nodes together (with direct pointers)
	NetaNode *link(NetaNode *node1, NetaNode *node2);
	// Create element/type list item
	Refitem<ForcefieldAtom,int> *createElementType(int eltype);
	// Join element/type list items
	Refitem<ForcefieldAtom,int> *joinElementTypes(Refitem<ForcefieldAtom,int> *type1, Refitem<ForcefieldAtom,int> *type2);
	// Create keyword node in current NETA structure
	NetaNode *createKeywordNode(Neta::NetaKeyword nk);
	// Create geometry node in current NETA structure
	NetaNode *createGeometryNode(Atom::AtomGeometry ag);
	// Create value node in current NETA structure
	NetaNode *createValueNode(Neta::NetaValue nv, Neta::NetaValueComparison nvc, int value);
	// Create bound node in current NETA structure
	NetaBoundNode *createBoundNode();
	// Create ring node in current NETA structure
	NetaRingNode *createRingNode();
	// Create chain node in current NETA structure
	NetaChainNode *createChainNode();
	// Create measurement node in current NETA structure
	NetaMeasurementNode *createMeasurementNode(bool removeNeighbours);
	// Find named define in forcefield
	NetaNode *findDefine(const char *name);
};

// External declaration
extern NetaParser netaparser;

#endif

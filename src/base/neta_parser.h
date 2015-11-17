/*
	*** NETA Description Parser
	*** src/base/neta_parser.h
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

#ifndef ATEN_NETAPARSER_H
#define ATEN_NETAPARSER_H

#include "base/lineparser.h"
#include "base/neta.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
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
	 * Character Stream
	 */
	private:
	// Character string source
	static QString stringSource_;
	// Integer position in stringSource, total length of string, and starting position of current token/function
	static int stringPos_, stringLength_, tokenStart_, functionStart_;
	// Line parser
	static LineParser parser_;
	// Line number in source file that we've just read
	static int lineNumber_;
	// Whether the current input source is a file or not
	static bool isFileSource_;

	public:
	// Parser lexer, called by yylex()
	static int lex();
	// Return whether the current input stream is a file or not
	static bool isFileSource();
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


	/*
	 * Node Functions
	 */
	private:
	// Node context stack
	static RefList<NetaContextNode,int> contextStack_;
	// Current NETA structure target
	static Neta* neta_;
	// Current NETA target structure parent
	static Forcefield* targetParentForcefield_;
	// Name of last unrecognised token
	static QString lastUnknownToken_;
	// Whether to suppress messaging output
	static bool quiet_;

	public:
	// Pop topmost node of contextStack_
	static void popContext();
	// Return name of last unrecognised token
	static QString lastUnknownToken();
	// Return whether the parser is in quiet mode
	static bool quiet();
	// Create guts of within supplied NETA structure from supplied character element and string
	static bool createNeta(Neta* target, QString neta, Forcefield* parentff, bool quiet = false);
	// Set description (called by lexer)
	static void setDescription(NetaNode* desc);
	// Join two nodes together
	static NetaNode* join(Neta::NetaLogicType logic, NetaNode* node1, NetaNode* node2);
	// Link two nodes together (with direct pointers)
	static NetaNode* link(NetaNode* node1, NetaNode* node2);
	// Create element/type list item
	static RefListItem<ForcefieldAtom,int>* createElementType(int eltype);
	// Join element/type list items
	static RefListItem<ForcefieldAtom,int>* joinElementTypes(RefListItem<ForcefieldAtom,int>* type1, RefListItem<ForcefieldAtom,int>* type2);
	// Create keyword node in current NETA structure
	static NetaNode* createKeywordNode(Neta::NetaKeyword nk);
	// Create geometry node in current NETA structure
	static NetaNode* createGeometryNode(Atom::AtomGeometry ag);
	// Create value node in current NETA structure
	static NetaNode* createValueNode(Neta::NetaValue nv, Neta::NetaValueComparison nvc, int value);
	// Create bound node in current NETA structure
	static NetaBoundNode* createBoundNode();
	// Create ring node in current NETA structure
	static NetaRingNode* createRingNode();
	// Create chain node in current NETA structure
	static NetaChainNode* createChainNode();
	// Create measurement node in current NETA structure
	static NetaMeasurementNode* createMeasurementNode(bool removeNeighbours);
	// Find named define in forcefield
	static NetaNode* findDefine(QString name);
};

// External declaration
// extern NetaParser netaparser;

ATEN_END_NAMESPACE

#endif

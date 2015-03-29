/*
	*** NETA Parser Interface
	*** src/base/neta_parser.cpp
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

#include "base/neta.h"
#include "base/neta_parser.h"
#include "ff/forcefield.h"

// External Declarations (outside of namespace)
int NetaParser_parse();

ATEN_BEGIN_NAMESPACE

// External Declarations
NetaParser netaparser;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Constructors
NetaParser::NetaParser()
{
	// Private variables
	neta_ = NULL;
	targetParentForcefield_ = NULL;
}

// Print error information and location
void NetaParser::printErrorInfo()
{
	if (isFileSource_) Messenger::print("Error occurred here (line %i in file '%s'):", parser_.lastLineNo(), qPrintable(parser_.inputFilename()));
	// QUICK'n'DIRTY!
	int i;
	char *temp = new char[stringLength_+32];
	for (i=0; i<stringLength_+32; ++i) temp[i] = '\0';
	for (i=0; i<tokenStart_; i++) temp[i] = (stringSource_.at(i) == '\t' ? '\t' : ' ');
	if (functionStart_ > -1) for (i=functionStart_; i<stringPos_; i++) if (temp[i] != '\t') temp[i] = '-';
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	Messenger::print(" %s", qPrintable(stringSource_));
	Messenger::print(" %s^", temp);
	delete[] temp;
}

/*
// Character Stream Retrieval
*/

// Return whether the current input stream is a file
bool NetaParser::isFileSource()
{
	return isFileSource_;
}

// Get next character from current input stream
char NetaParser::getChar()
{
	char c = 0;
	if (isFileSource_)
	{
		// If the stringPos_ is equal to the string length, read in another line
		if (stringPos_ == stringLength_)
		{
			if (parser_.readNextLine(0) != 0) return 0;
			stringSource_ = parser_.line();
			stringLength_ = stringSource_.length();
			stringPos_ = 0;
		}
	}

	// Return current character
	if (stringPos_ == stringLength_) return '\0';
	c = stringSource_.at(stringPos_).toLatin1();

	// Increment string position
	stringPos_++;
	return c;
}

// Peek next character from current input stream
char NetaParser::peekChar()
{
	char c = 0;
	if (isFileSource_)
	{
		if (stringPos_ == stringLength_) return parser_.peek();
		c = stringSource_.at(stringPos_).toLatin1();
	}
	else
	{
		// Return current character
		if (stringPos_ == stringLength_) return '\0';
		c = stringSource_.at(stringPos_).toLatin1();
	}
	return c;
}

// 'Replace' last character read from current input stream
void NetaParser::unGetChar()
{
	if (isFileSource_)
	{
		// If we are at position 0, then we need the last character from the previous line!
		if (stringPos_ == 0) printf("Fix Required: last character from previous line...\n");
		else --stringPos_;
	}
	else --stringPos_;
}

/*
// Node Functions
*/

// Pop topmost node of contextStack_
void NetaParser::popContext()
{
	if (contextStack_.nItems() == 0) printf("Internal Error: No context node to pop.\n");
	else contextStack_.removeLast();	
}

// Return name of last unrecognised token
QString NetaParser::lastUnknownToken()
{
	return lastUnknownToken_;
}

// Create and return NETA structure from supplied character element and string
bool NetaParser::createNeta(Neta* target, QString neta, Forcefield* parentff)
{
	Messenger::enter("NetaParser::createNeta");

	// Check for existing current target...
	if (neta_ != NULL)
	{
		Messenger::print("Internal Error: NetaParser was already busy creating a NETA description when another was started.");
		Messenger::exit("NetaParser::createNeta");
		return FALSE;
	}

	// Was a valid pointer passed?
	if (target == NULL)
	{
		Messenger::print("Error: NULL pointer passed to NetaParser::createNeta.");
		Messenger::exit("NetaParser::createNeta");
		return FALSE;
	}

	// Store new pointer targets
	neta_ = target;
	neta_->clear();
	targetParentForcefield_ = parentff;

	// Store the source string
	stringSource_ = neta;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	Messenger::print(Messenger::Typing, "Parser source string is '%s', length is %i", qPrintable(stringSource_), stringLength_);
	isFileSource_ = FALSE;

	// Perform the parsing
	contextStack_.clear();
	int result = NetaParser_parse();
	if (result != 0)
	{
		printErrorInfo();
		neta_->clear();
	}
// 	neta_->print();
	neta_ = NULL;

	Messenger::exit("NetaParser::createNeta");
	return (result != 0 ? FALSE : TRUE);
}

// Set description (called by lexer)
void NetaParser::setDescription(NetaNode* desc)
{
	Messenger::enter("NetaParser::setDescription");

	NetaRootNode* rootnode = new NetaRootNode();
	rootnode->setInnerNeta(desc);
	neta_->ownedNodes_.own(rootnode);
	neta_->description_ = rootnode;

	Messenger::exit("NetaParser::setDescription");
}

// Join two nodes together
NetaNode* NetaParser::join(Neta::NetaLogicType logic, NetaNode* node1, NetaNode* node2)
{
	Messenger::enter("NetaParser::join");

	NetaLogicNode* newnode = new NetaLogicNode(logic, node1, node2);
	newnode->setParent(neta_);
	neta_->ownedNodes_.own(newnode);

	Messenger::exit("NetaParser::join");
	return newnode;	
}

// Link two nodes together
NetaNode* NetaParser::link(NetaNode* node1, NetaNode* node2)
{
	Messenger::enter("NetaParser::link");

	NetaNode* tail = node1;
	while (tail->nextNode != NULL) tail = tail->nextNode;
	tail->nextNode = node2;
	node2->prevNode = tail;

	Messenger::exit("NetaParser::link");
	return node1;
}

// Create element/type list item
Refitem<ForcefieldAtom,int>* NetaParser::createElementType(int eltype)
{
	Messenger::enter("NetaParser::createElementType");

	Refitem<ForcefieldAtom,int>* newItem = new Refitem<ForcefieldAtom,int>;
	newItem->item = NULL;
	newItem->data = eltype;

	Messenger::exit("NetaParser::createElementType");
	return newItem;
}

// Join element/type list items
Refitem<ForcefieldAtom,int>* NetaParser::joinElementTypes(Refitem<ForcefieldAtom,int>* type1, Refitem<ForcefieldAtom,int>* type2)
{
	Messenger::enter("NetaParser::joinElementTypes");

	// Find tail of list begun by type1
	Refitem<ForcefieldAtom,int>* tail = type1;
	while (tail->next != NULL) tail = tail->next;

	// Append on type2
	type2->prev = tail;
	tail->next = type2;

	Messenger::exit("NetaParser::joinElementTypes");
	return type1;
}

// Create keyword node in current NETA structure
NetaNode* NetaParser::createKeywordNode(Neta::NetaKeyword nk)
{
	Messenger::enter("NetaParser::createKeywordNode");

	NetaKeywordNode* newNode = new NetaKeywordNode(nk);
	newNode->setParent(neta_);
	neta_->ownedNodes_.own(newNode);

	Messenger::exit("NetaParser::createKeywordNode");
	return newNode;
}

// Create geometry node in current NETA structure
NetaNode* NetaParser::createGeometryNode(Atom::AtomGeometry ag)
{
	Messenger::enter("NetaParser::createGeometryNode");

	NetaGeometryNode* newNode = new NetaGeometryNode(ag);
	newNode->setParent(neta_);
	neta_->ownedNodes_.own(newNode);

	Messenger::exit("NetaParser::createGeometryNode");
	return newNode;
}

// Create value node in current NETA structure
NetaNode* NetaParser::createValueNode(Neta::NetaValue nv, Neta::NetaValueComparison nvc, int value)
{
	Messenger::enter("NetaParser::createValueNode");

	NetaValueNode* newNode = new NetaValueNode(nv, nvc, value);
	newNode->setParent(neta_);
	neta_->ownedNodes_.own(newNode);
	// If the NetaValue is 'n' (the repeat number) then add this info to the topmost ContextNode
	if (nv == Neta::RepeatValue)
	{
		if (contextStack_.nItems() == 0)
		{
			Messenger::print("Error: Repeat value ('n') given in an invalid context.");
			Messenger::exit("NetaParser::createValueNode");
			return NULL;
		}
		contextStack_.last()->item->setRepeat(value);
		contextStack_.last()->item->setRepeatComparison(nvc);
	}

	Messenger::exit("NetaParser::createValueNode");
	return newNode;
}

// Create expander node in current NETA structure
NetaBoundNode* NetaParser::createBoundNode()
{
	Messenger::enter("NetaParser::createBoundNode");

	NetaBoundNode* newNode = new NetaBoundNode();
	newNode->setParent(neta_);
	contextStack_.add(newNode);
// 	printf("Added stacknode %p - %i in list\n", newnode, contextStack_.nItems());
	neta_->ownedNodes_.own(newNode);

	Messenger::exit("NetaParser::createBoundNode");
	return newNode;
}

// Create ring node in current NETA structure
NetaRingNode* NetaParser::createRingNode()
{
	Messenger::enter("NetaParser::createRingNode");

	NetaRingNode* newNode = new NetaRingNode();
	newNode->setParent(neta_);
	contextStack_.add(newNode);
	neta_->ownedNodes_.own(newNode);

	Messenger::exit("NetaParser::createRingNode");
	return newNode;
}

// Create chain node in current NETA structure
NetaChainNode* NetaParser::createChainNode()
{
	Messenger::enter("NetaParser::createChainNode");

	NetaChainNode* newNode = new NetaChainNode();
	newNode->setParent(neta_);
	contextStack_.add(newNode);
	neta_->ownedNodes_.own(newNode);

	Messenger::exit("NetaParser::createChainNode");
	return newNode;
}

// Create measurement node in current NETA structure
NetaMeasurementNode* NetaParser::createMeasurementNode(bool removeNeighbours)
{
	Messenger::enter("NetaParser::createMeasurementNode");

	NetaMeasurementNode* newNode = new NetaMeasurementNode();
	newNode->setParent(neta_);
	newNode->setRemoveNeighbours(removeNeighbours);
	contextStack_.add(newNode);
	neta_->ownedNodes_.own(newNode);

	Messenger::exit("NetaParser::createMeasurementNode");
	return newNode;
}

// Find named define in forcefield
NetaNode* NetaParser::findDefine(QString name)
{
	Messenger::enter("NetaParser::findDefine");

	if (targetParentForcefield_ == NULL)
	{
		Messenger::exit("NetaParser::findDefine");
		return NULL;
	}
	Neta* neta = targetParentForcefield_->typeDefine(name);
	if (neta == NULL)
	{
		Messenger::print("Error: Type define '%s' has not been defined.", qPrintable(name));
		Messenger::exit("NetaParser::findDefine");
		return NULL;
	}
	NetaNode* node = neta_->clone(neta->description());

	Messenger::exit("NetaParser::findDefine");
	return node;
}

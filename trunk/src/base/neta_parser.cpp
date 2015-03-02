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
	if (isFileSource_) Messenger::print("Error occurred here (line %i in file '%s'):\n", parser_.lastLineNo(), parser_.inputFilename());
	// QUICK'n'DIRTY!
	int i;
	char *temp = new char[stringLength_+32];
	for (i=0; i<stringLength_+32; ++i) temp[i] = '\0';
	for (i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
	if (functionStart_ > -1) for (i=functionStart_; i<stringPos_; i++) if (temp[i] != '\t') temp[i] = '-';
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	Messenger::print(" %s\n", stringSource_.get());
	Messenger::print(" %s^\n", temp);
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
	c = stringSource_[stringPos_];
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
		c = stringSource_[stringPos_];
	}
	else
	{
		// Return current character
		if (stringPos_ == stringLength_) return '\0';
		c = stringSource_[stringPos_];
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
		else stringPos_ --;
	}
	else stringPos_--;
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
const char* NetaParser::lastUnknownToken()
{
	return lastUnknownToken_.get();
}

// Create and return NETA structure from supplied character element and string
bool NetaParser::createNeta(Neta *target, const char* s, Forcefield* parentff)
{
	Messenger::enter("NetaParser::createNeta");
	// Check for existing current target...
	if (neta_ != NULL)
	{
		Messenger::print("Internal Error: NetaParser was already busy creating a NETA description when another was started.\n");
		Messenger::exit("NetaParser::createNeta");
		return FALSE;
	}
	// Was a valid pointer passed?
	if (target == NULL)
	{
		Messenger::print("Error: NULL pointer passed to NetaParser::createNeta.\n");
		Messenger::exit("NetaParser::createNeta");
		return FALSE;
	}
	// Store new pointer targets
	neta_ = target;
	neta_->clear();
	targetParentForcefield_ = parentff;
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	Messenger::print(Messenger::Typing, "Parser source string is '%s', length is %i\n", stringSource_.get(), stringLength_);
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
void NetaParser::setDescription(NetaNode *desc)
{
	Messenger::enter("NetaParser::setDescription");
	NetaRootNode *rootnode = new NetaRootNode();
	rootnode->setInnerNeta(desc);
	neta_->ownedNodes_.own(rootnode);
	neta_->description_ = rootnode;
	Messenger::exit("NetaParser::setDescription");
}

// Join two nodes together
NetaNode *NetaParser::join(Neta::NetaLogicType logic, NetaNode *node1, NetaNode *node2)
{
	Messenger::enter("NetaParser::join");
	NetaLogicNode *newnode = new NetaLogicNode(logic, node1, node2);
	newnode->setParent(neta_);
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::join");
	return newnode;	
}

// Link two nodes together
NetaNode *NetaParser::link(NetaNode *node1, NetaNode *node2)
{
	Messenger::enter("NetaParser::link");
	NetaNode *tail = node1;
	while (tail->nextNode != NULL) tail = tail->nextNode;
	tail->nextNode = node2;
	node2->prevNode = tail;
	Messenger::exit("NetaParser::link");
	return node1;
}

// Create element/type list item
Refitem<ForcefieldAtom,int> *NetaParser::createElementType(int eltype)
{
	Messenger::enter("NetaParser::createElementType");
	Refitem<ForcefieldAtom,int> *newitem = new Refitem<ForcefieldAtom,int>;
	newitem->item = NULL;
	newitem->data = eltype;
	Messenger::exit("NetaParser::createElementType");
	return newitem;
}

// Join element/type list items
Refitem<ForcefieldAtom,int> *NetaParser::joinElementTypes(Refitem<ForcefieldAtom,int> *type1, Refitem<ForcefieldAtom,int> *type2)
{
	Messenger::enter("NetaParser::joinElementTypes");
	// Find tail of list begun by type1
	Refitem<ForcefieldAtom,int> *tail = type1;
	while (tail->next != NULL) tail = tail->next;
	// Append on type2
	type2->prev = tail;
	tail->next = type2;
	Messenger::exit("NetaParser::joinElementTypes");
	return type1;
}

// Create keyword node in current NETA structure
NetaNode *NetaParser::createKeywordNode(Neta::NetaKeyword nk)
{
	Messenger::enter("NetaParser::createKeywordNode");
	NetaKeywordNode *newnode = new NetaKeywordNode(nk);
	newnode->setParent(neta_);
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::createKeywordNode");
	return newnode;
}

// Create geometry node in current NETA structure
NetaNode *NetaParser::createGeometryNode(Atom::AtomGeometry ag)
{
	Messenger::enter("NetaParser::createGeometryNode");
	NetaGeometryNode *newnode = new NetaGeometryNode(ag);
	newnode->setParent(neta_);
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::createGeometryNode");
	return newnode;
}

// Create value node in current NETA structure
NetaNode *NetaParser::createValueNode(Neta::NetaValue nv, Neta::NetaValueComparison nvc, int value)
{
	Messenger::enter("NetaParser::createValueNode");
	NetaValueNode *newnode = new NetaValueNode(nv, nvc, value);
	newnode->setParent(neta_);
	neta_->ownedNodes_.own(newnode);
	// If the NetaValue is 'n' (the repeat number) then add this info to the topmost ContextNode
	if (nv == Neta::RepeatValue)
	{
		if (contextStack_.nItems() == 0)
		{
			Messenger::print("Error: Repeat value ('n') given in an invalid context.\n");
			Messenger::exit("NetaParser::createValueNode");
			return NULL;
		}
		contextStack_.last()->item->setRepeat(value);
		contextStack_.last()->item->setRepeatComparison(nvc);
	}
	Messenger::exit("NetaParser::createValueNode");
	return newnode;
}

// Create expander node in current NETA structure
NetaBoundNode *NetaParser::createBoundNode()
{
	Messenger::enter("NetaParser::createBoundNode");
	NetaBoundNode *newnode = new NetaBoundNode();
	newnode->setParent(neta_);
	contextStack_.add(newnode);
// 	printf("Added stacknode %p - %i in list\n", newnode, contextStack_.nItems());
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::createBoundNode");
	return newnode;
}

// Create ring node in current NETA structure
NetaRingNode *NetaParser::createRingNode()
{
	Messenger::enter("NetaParser::createRingNode");
	NetaRingNode *newnode = new NetaRingNode();
	newnode->setParent(neta_);
	contextStack_.add(newnode);
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::createRingNode");
	return newnode;
}

// Create chain node in current NETA structure
NetaChainNode *NetaParser::createChainNode()
{
	Messenger::enter("NetaParser::createChainNode");
	NetaChainNode *newnode = new NetaChainNode();
	newnode->setParent(neta_);
	contextStack_.add(newnode);
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::createChainNode");
	return newnode;
}

// Create measurement node in current NETA structure
NetaMeasurementNode *NetaParser::createMeasurementNode(bool removeNeighbours)
{
	Messenger::enter("NetaParser::createMeasurementNode");
	NetaMeasurementNode *newnode = new NetaMeasurementNode();
	newnode->setParent(neta_);
	newnode->setRemoveNeighbours(removeNeighbours);
	contextStack_.add(newnode);
	neta_->ownedNodes_.own(newnode);
	Messenger::exit("NetaParser::createMeasurementNode");
	return newnode;
}

// Find named define in forcefield
NetaNode *NetaParser::findDefine(const char* name)
{
	Messenger::enter("NetaParser::findDefine");
	if (targetParentForcefield_ == NULL) return NULL;
	Neta *neta = targetParentForcefield_->typeDefine(name);
	if (neta == NULL)
	{
		Messenger::print("Error: Type define '%s' has not been defined.\n", name);
		return NULL;
	}
	NetaNode *node = neta_->clone(neta->description());
	Messenger::exit("NetaParser::findDefine");
	return node;
}

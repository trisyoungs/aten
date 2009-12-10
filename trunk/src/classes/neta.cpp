/*
	*** NETA type description
	*** src/classes/neta.cpp
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

#include "classes/neta.h"
#include "base/sysfunc.h"
#include "base/atom.h"
#include "base/dnchar.h"
#include "classes/ring.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/lineparser.h"

// NETA Keywords
const char *NetaKeywordKeywords[Neta::nNetaKeywords] = { "alphatic", "aromatic", "noring", "nonaromatic", "notself", "planar" };
enum NetaKeyword { AliphaticKeyword, AromaticKeyword, NoRingKeyword, NonAromaticKeyword, NotSelfKeyword, nNetaKeywords };
Neta::NetaKeyword Neta::netaKeyword(const char *s, bool reporterror)
{
	Neta::NetaKeyword n = (Neta::NetaKeyword) enumSearch("NETA keyword",Neta::nNetaKeywords,NetaKeywordKeywords,s, reporterror);
	if ((n == Neta::nNetaKeywords) && reporterror) enumPrintValid(Neta::nNetaKeywords,NetaKeywordKeywords);
	return n;
}
const char *Neta::netaKeyword(Neta::NetaKeyword nk)
{
	return NetaKeywordKeywords[nk];
}

// NETA values
const char *NetaValueKeywords[Neta::nNetaValues] = { "bond", "nbonds", "nh", "os", "n", "size" };
Neta::NetaValue Neta::netaValue(const char *s, bool reporterror)
{
	Neta::NetaValue n = (Neta::NetaValue) enumSearch("NETA value",Neta::nNetaValues,NetaValueKeywords,s, reporterror);
	if ((n == Neta::nNetaValues) && reporterror) enumPrintValid(Neta::nNetaValues,NetaValueKeywords);
	return n;
}
const char *Neta::netaValue(Neta::NetaValue nv)
{
	return NetaValueKeywords[nv];
}

// NETA expanders
const char *NetaExpanderKeywords[Neta::nNetaExpanders] = { "-", "chain", "=", "ring" };
Neta::NetaExpander Neta::netaExpander(const char *s, bool reporterror)
{
	Neta::NetaExpander n = (Neta::NetaExpander) enumSearch("NETA expander",Neta::nNetaExpanders,NetaExpanderKeywords,s, reporterror);
	if ((n == Neta::nNetaExpanders) && reporterror) enumPrintValid(Neta::nNetaExpanders,NetaExpanderKeywords);
	return n;
}

// NETA Value comparison operators
const char *NetaValueComparisonKeywords[Neta::nNetaValueComparisons] = { "=", "!=", ">", "<", ">=", "<=" };
const char *Neta::netaValueComparison(Neta::NetaValueComparison nvc)
{
	return NetaValueComparisonKeywords[nvc];
}
bool Neta::netaValueCompare(int lhsvalue, NetaValueComparison nvc, int rhsvalue)
{
	bool result;
	switch (nvc)
	{
		case (EqualTo):
			result = (lhsvalue == rhsvalue);
			break;
		case (NotEqualTo):
			result = (lhsvalue != rhsvalue);
			break;
		case (GreaterThan):
			result = (lhsvalue > rhsvalue);
			break;
		case (LessThan):
			result = (lhsvalue < rhsvalue);
			break;
		case (GreaterThanEqualTo):
			result = (lhsvalue >= rhsvalue);
			break;
		case (LessThanEqualTo):
			result = (lhsvalue <= rhsvalue);
			break;
	}
	return result;
}

// Node logic types
const char *NetaLogicTypeKeywords[Neta::nNetaLogicTypes] = { "and", "or", "and-not" };
const char *Neta::netaLogic(NetaLogicType lt)
{
	return NetaLogicTypeKeywords[lt];
}

/*
// Neta Parent Structure
*/

Neta::Neta()
{
	// Private variables
	characterElement_ = -1;
	parentForcefield_ = NULL;
	parentForcefieldAtom_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructors
Neta::~Neta()
{
}

// Set parent forcefield
void Neta::setParentForcefield(Forcefield *ff)
{
	parentForcefield_ = ff;
}

// Return parent forcefield
Forcefield *Neta::parentForcefield()
{
	return parentForcefield_;
}

// Set parent forcefield atom
void Neta::setParentForcefieldAtom(ForcefieldAtom *ffa)
{
	parentForcefieldAtom_ = ffa;
}

// Return parent forcefield atom
ForcefieldAtom *Neta::parentForcefieldAtom()
{
	return parentForcefieldAtom_;
}

// Return character element
void Neta::setCharacterElement(int el)
{
	characterElement_ = el;
}

// Return character element
int Neta::characterElement()
{
	return characterElement_;
}

// Take ownership of selected node
void Neta::ownNode(NetaNode *node)
{
	ownedNodes_.own(node);
}

// Return reference name (if a define)
const char *Neta::name()
{
	return name_.get();
}

// Set reference name (if a define)
void Neta::setName(const char *s)
{
	name_ = s;
}

// Return top of description nodelist
NetaRootNode *Neta::description()
{
	return description_;
}

// Print Atom Type data
void Neta::print()
{
	printf("Character element is %i\n", characterElement_);
	Dnchar neta(1024);
	if (description_ != NULL) description_->netaPrint(neta);
	printf("NETA string is '%s'\n", neta.get());
	printf("Node description is:\n");
	if (description_ != NULL) description_->nodePrint(0,"");
	else printf("   None defined.\n");
}

// Clone nodes (and own them) beginning from the node supplied
NetaNode *Neta::clone(NetaNode *topnode)
{
	msg.enter("Neta::clone");
	NetaNode *result;
	// Just call top node's routine, and get the result
	if (topnode == NULL) result = NULL;
	else result = topnode->clone(this);
	msg.exit("Neta::clone");
	return result;
}

// Clear all associated node data (but leave character element as-is)
void Neta::clear()
{
	description_ = NULL;
	ownedNodes_.clear();
}

// Return ringList of supplied atom
List<Ring> *Neta::targetRingList()
{
	return targetRingList_;
}

// Return target atom's parent model
Model *Neta::targetParent()
{
	return targetParent_;
}

// Check supplied atom to see if it matches this NETA description
int Neta::matchAtom(Atom *i, List<Ring> *rings, Model *parent)
{
	msg.enter("Neta::matchAtom");
	// Check element type first
	if (i->element() != characterElement_)
	{
		msg.exit("Neta::matchAtom");
		return -1;
	}
	// Is a description provided?
	if (description_ == NULL)
	{
		msg.exit("Neta::matchAtom");
		return 1;
	}
	// Store ring list and parent model of atom
	targetRingList_ = rings;
	targetParent_ = parent;
	// Create a bound list of atoms and a list of rings to pass to the head of the description
	Reflist<Atom,int> boundList;
	i->addBoundToReflist(&boundList);
	Reflist<Ring,int> ringList;
	for (Ring *r = targetRingList_->first(); r != NULL; r = r->next) if (r->containsAtom(i)) ringList.add(r);
	int score = description_->score(i, &boundList, &ringList, description_, NULL, 0);
// 	printf("Score is %i\n", score);
	msg.exit("Neta::matchAtom");
	return (score == -1 ? -1 : score+1);
}

// Link forcefield type references in elementtype lists
void Neta::linkReferenceTypes()
{
	msg.enter("Neta::linkReferenceTypes");
	NetaBoundNode *bnode;
	for (NetaNode *node = ownedNodes_.first(); node != NULL; node = node->next)
	{
		if (node->nodeType() != NetaNode::BoundNode) continue;
		bnode = (NetaBoundNode*) node;
		bnode->linkReferenceTypes();
	}
	msg.exit("Neta::linkReferenceTypes");
}

/*
// NetaNode
*/

NetaNode::NetaNode()
{
	// Private variables
	nodeType_ = NetaNode::nNetaNodeTypes;
	parent_ = NULL;
	reverseLogic_ = FALSE;

	// Public variables
	prev = NULL;
	next = NULL;
	prevNode = NULL;
	nextNode = NULL;
}

// Return node type
NetaNode::NetaNodeType NetaNode::nodeType()
{
	return nodeType_;
}

// Set node to use reverse logic
void NetaNode::setReverseLogic()
{
	reverseLogic_ = TRUE;
}

// Return whether to use reverse logic when returning the final value
bool NetaNode::reverseLogic()
{
	return reverseLogic_;
}

// Return parent NETA structure
Neta *NetaNode::parent()
{
	return parent_;
}

// Set parent NETA structure
void NetaNode::setParent(Neta *neta)
{
	parent_ = neta;
}

// Print contextual score
void NetaNode::printScore(int level, const char *fmt ...)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[level+32];
	tab[0] = '\0';
	for (int n=0; n<level-1; n++) strcat(tab,"\t");
	if (level > 1) strcat(tab,"   |--> ");
	if (level == 1) strcat(tab,"\t");

	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	va_end(arguments);
	// Output node data
	msg.print(Messenger::Typing, "NETA%03i:%s%s\n", level, tab, msgs);
	delete[] tab;
}

/*
// NetaContextNode
*/

// Constructor
NetaContextNode::NetaContextNode()
{
	// Private variables
	repeat_ = -1;
	innerNeta_ = NULL;
	linearNeta_ = NULL;
}

// Set repeat specifier
void NetaContextNode::setRepeat(int n)
{
	repeat_ = n;
}

// Return repetition specified
int NetaContextNode::repeat()
{
	return repeat_;
}

// Set value comparison
void NetaContextNode::setRepeatComparison(Neta::NetaValueComparison nvc)
{
	repeatComparison_ = nvc;
}

// Set inner neta
void NetaContextNode::setInnerNeta(NetaNode *innerneta, NetaNode *linearneta)
{
	innerNeta_ = innerneta;
	linearNeta_ = linearneta;
}

// Return inner neta description
NetaNode *NetaContextNode::innerNeta()
{
	return innerNeta_;
}

// Clone node structure
NetaNode *NetaContextNode::clone(Neta *newparent)
{
	printf("NetaContextNode::clone() should never be called.\n");
	return NULL;
}

/*
// NetaLogicNode
*/

// Constructor
NetaLogicNode::NetaLogicNode(Neta::NetaLogicType nt, NetaNode *arg1, NetaNode *arg2)
{
	// Private variables
	nodeType_ = NetaNode::LogicNode;
	netaLogic_ = nt;
	argument1_ = arg1;
	argument2_ = arg2;
};

// Validation function (virtual)
int NetaLogicNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaLogicNode::score");
	int score1 = -1, score2 = -1, totalscore = -1;
	switch (netaLogic_)
	{
		case (Neta::NetaAndLogic):
			score1 = argument1_->score(target, nbrs, rings, context, prevTarget, level);
			if (score1 != -1)
			{
				score2 = argument2_->score(target, nbrs, rings, context, prevTarget, level);
				if (score2 != -1) totalscore = score1 + score2;
			}
			break;
		case (Neta::NetaOrLogic):
			score1 = argument1_->score(target, nbrs, rings, context, prevTarget, level);
			if (score1 != -1) totalscore = score1;
			else
			{
				score2 = argument2_->score(target, nbrs, rings, context, prevTarget, level);
				if (score2 != -1) totalscore = score2;
			}
			break;
		case (Neta::NetaAndNotLogic):
			score1 = argument1_->score(target, nbrs, rings, context, prevTarget, level);
			if (score1 != -1)
			{
				score2 = argument2_->score(target, nbrs, rings, context, prevTarget, level);
				if (score2 == -1) totalscore = score1;
			}
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	msg.exit("NetaLogicNode::score");
	return totalscore;
}

// Print node contents
void NetaLogicNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Logic Node: %s)\n", tab, Neta::netaLogic(netaLogic_));
	argument1_->nodePrint(offset+1, "");
	argument2_->nodePrint(offset+1, "");
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaLogicNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaLogicNode::netaPrint");
	argument1_->netaPrint(neta);
	switch (netaLogic_)
	{
		case (Neta::NetaAndLogic):
			neta += ',';
			break;
		case (Neta::NetaOrLogic):
			neta += '|';
			break;
		case (Neta::NetaAndNotLogic):
			neta.cat("&!");
			break;
	}
	argument2_->netaPrint(neta);
	msg.exit("NetaLogicNode::netaPrint");
}

// Clone node structure
NetaNode *NetaLogicNode::clone(Neta *newparent)
{
	msg.enter("NetaLogicNode::clone");
	NetaLogicNode *node = new NetaLogicNode(netaLogic_, NULL, NULL);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->argument1_ = argument1_ == NULL ? NULL : argument1_->clone(newparent);
	node->argument2_ = argument2_ == NULL ? NULL : argument2_->clone(newparent);
	newparent->ownNode(node);
	msg.exit("NetaLogicNode::clone");
	return node;
}

/*
// NetaBoundNode
*/

// Constructor
NetaBoundNode::NetaBoundNode()
{
	// Private variables
	nodeType_ = NetaNode::BoundNode;
}

// Print node contents
void NetaBoundNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Bound Node: %s)\n", tab, Bond::bondType(bondType_));
	// Print element/type info
	printf("%s    Number of allowed elements/types defined = %i\n", tab, allowedElementsAndTypes_.nItems());
	printf("%s      ", tab);
	for (Refitem<ForcefieldAtom,int> *ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		if (ri->data == 0) printf("Any ");
		else if (ri->data > 0) printf("%s ", elements().symbol(ri->data));
		else printf("&%i ", -ri->data);
	}
	printf("\n");
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", tab);
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaBoundNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaBoundNode::netaPrint");
	neta.cat(elementsAndTypesString());
	if (innerNeta_ != NULL)
	{
		neta += '(';
		if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
		neta += ')';
	}
	// Is there an attached (linear) node? If so, add it on to this description
	if (nextNode != NULL) nextNode->netaPrint(neta);
	msg.exit("NetaBoundNode::netaPrint");
}

// Clone node structure
NetaNode *NetaBoundNode::clone(Neta *newparent)
{
	msg.enter("NetaBoundNode::clone");
	NetaBoundNode *node = new NetaBoundNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->allowedElementsAndTypes_ = allowedElementsAndTypes_;
	node->bondType_ = bondType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	newparent->ownNode(node);
	msg.exit("NetaBoundNode::clone");
	return node;
}

// Set bound data
void NetaBoundNode::set(Refitem<ForcefieldAtom,int> *elemtypes, NetaNode *innerneta, Bond::BondType bt)
{
	msg.enter("NetaBoundNode::set");
	bondType_ = bt;
	innerNeta_ = innerneta;
	// Take items from list
	Refitem<ForcefieldAtom,int> *item = elemtypes, *nextitem = NULL;
	while (item != NULL)
	{
		// Store next item pointer
		nextitem = item->next;
		// Remove current item from list
		item->prev = NULL;
		item->next = NULL;
		// Own item
		allowedElementsAndTypes_.own(item);
		// Next item...
		item = nextitem;
	}
	msg.exit("NetaBoundNode::set");
}

// Link forcefield type references in elementtype lists
void NetaBoundNode::linkReferenceTypes()
{
	ForcefieldAtom *ffa;
	for (Refitem<ForcefieldAtom,int> *ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		// Skip if its just an element
		if (ri->data >= 0) continue;
		// Find referenced type in forcefield
		if (parent()->parentForcefield() == NULL)
		{
			msg.print("Warning: Type '%s' (id %i) contains references to other types, but no parent Forcefield is defined.\n");
			msg.print("         --> Reset to '0' (Any element)\n");
			ri->data = 0;
		}
		else
		{
			ffa = parent()->parentForcefield()->findType(abs(ri->data));
			ri->item = ffa;
			if (ffa == NULL) msg.print("Warning: Type '%s' (id %i) references type id %i in it's NETA description, but type id %i has not been defined in the forcefield.\n", parent()->parentForcefieldAtom()->name(), parent()->parentForcefieldAtom()->typeId(), abs(ri->data), abs(ri->data));
		}
	}
}

// Validation function to check supplied atom against allowed elements and types
int NetaBoundNode::atomScore(Atom *target)
{
	msg.enter("NetaBoundNode::atomScore");
	int totalscore = -1;
	for (Refitem<ForcefieldAtom,int> *ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		// Simple element, or type reference?
		if (ri->item == NULL)
		{
			if ((ri->data == 0) || (ri->data == target->element()))
			{
				totalscore = 1;
				break;
			}
		}
		else
		{
			totalscore = ri->item->neta()->matchAtom(target, parent()->targetRingList(), parent()->targetParent());
			if (totalscore >= 0) break;
		}
	}
	msg.exit("NetaBoundNode::atomScore");
	return totalscore;
}

// Create formatted element/type list
const char *NetaBoundNode::elementsAndTypesString()
{
	msg.enter("NetaBoundNode::elementsAndTypesString");
	static char s[1024];
	switch (bondType_)
	{
		case (Bond::Any):
			s[0] = '~';
			break;
		case (Bond::Single):
			s[0] = '-';
			break;
		case (Bond::Double):
			s[0] = '=';
			break;
		default:
			msg.print("NETA Internal Error: Can't convert this bond type to a single character.\n");
			s[0] = '_';
			break;
	}
	s[1] = '\0';
	if (allowedElementsAndTypes_.nItems() != 1) strcat(s, "[");
	for (Refitem<ForcefieldAtom,int> *ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		if (ri != allowedElementsAndTypes_.first()) strcat(s,",");
		if ((ri->item != NULL) || (ri->data < 0))
		{
			strcat(s, "&");
			strcat(s, itoa(abs(ri->data)));
		}
		else if (ri->data == 0) strcat(s, "Any");
		else strcat(s, elements().symbol(ri->data));
	}
	if (allowedElementsAndTypes_.nItems() != 1) strcat(s, "]");
	msg.exit("NetaBoundNode::elementsAndTypesString");
	return s;
}

// Validation function (virtual)
int NetaBoundNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaBoundNode::score");
	int totalscore = -1, n, boundscore;
	Refitem<Atom,int> *ri;
	Refitem<ForcefieldAtom,int> *eltype;
	Reflist< Refitem<Atom,int>, int > scores;
	Refitem< Refitem<Atom,int>, int > *si;
	// Pointer check
	if (nbrs == NULL)
	{
		msg.print("NETA Internal Error: Called NetaBoundNode::score() without a valid neighbour list.\n");
		msg.exit("NetaBoundNode::score");
		return -1;
	}
	// Exactly how we proceed here depends on the current context (i.e. whether in Ring, Chain, etc.)
	if (nbrs->nItems() == 0) totalscore = -1;
	else
	{
		for (ri = nbrs->first(); ri != NULL; ri = ri->next)
		{
			// Does this bound neighbour match the element/type of the BoundNode?
			si = scores.add(ri, atomScore(ri->item));
			if (si->data == -1) continue;
			// Connection type?
			if ((context->nodeType() != NetaNode::RingNode) && (bondType_ != Bond::Any))
			{
				Bond *b = target->findBond(ri->item);
				if (b == NULL)
				{
					msg.print("NETA Internal Error: Couldn't find bond between atom ids %i and %i to check type.\n", target->id(), ri->item->id());
					msg.exit("NetaBoundNode::score");
					return -1;
				}
				if (b->type() == bondType_) si->data ++;
				else si->data = -1;
			}
			if (si->data == -1) continue;
			// Is there an inner NETA description to test?
			if (innerNeta_ == NULL) continue;
			else
			{
				// For each bound neighbour here, construct its own list of bound atoms and rings and check the inner NETA score
				Reflist<Atom,int> boundList;
				ri->item->addBoundToReflist(&boundList);
				// Construct new ringlist
				Reflist<Ring,int> ringList;
				for (Ring *r = parent()->targetRingList()->first(); r != NULL; r = r->next) if (r->containsAtom(ri->item)) ringList.add(r);
				boundscore = innerNeta_->score(ri->item, &boundList, &ringList, this, target, level+1);
				if (boundscore != -1) si->data += boundscore;
				else si->data = -1;
			}
		}
		n = 0;
		for (si = scores.first(); si != NULL; si = si->next) if (si->data != -1) ++n;
		if (n == 0) totalscore = -1;
		else if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
		{
			n = repeat_ == -1 ? 1 : repeat_;
			ri = nbrs->first();
			totalscore = 0;
			for (si = scores.first(); si != NULL; si = si->next)
			{
				if (si->data != -1)
				{
					totalscore += si->data;
					nbrs->remove(si->item);
				}
				--n;
				if (n == 0) break;
			}
		}
		else totalscore = -1;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Bound Check (%i of %s) = %i", repeat_ == -1 ? 1 : repeat_, elementsAndTypesString(), totalscore);
	msg.exit("NetaBoundNode::score");
	return totalscore;
}

/*
// NetaKeywordNode
*/

// Constructor
NetaKeywordNode::NetaKeywordNode(Neta::NetaKeyword nk)
{
	// Private variables
	netaKeyword_ = nk;
	nodeType_ = NetaNode::KeywordNode;
}

// Validation function (virtual)
int NetaKeywordNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaKeywordNode::score");
	int totalscore = -1;
	switch (netaKeyword_)
	{
		case (Neta::AliphaticKeyword):
			if (context->nodeType() == NetaNode::RingNode) totalscore = ((NetaRingNode*) context)->currentRing()->type() == Ring::AliphaticRing ? 1 : -1;
			else if (target->environment() != Atom::AromaticEnvironment) totalscore = 1;
			break;
		case (Neta::AromaticKeyword):
			if (context->nodeType() == NetaNode::RingNode) totalscore = ((NetaRingNode*) context)->currentRing()->type() == Ring::AromaticRing ? 1 : -1;
			else if (target->environment() == Atom::AromaticEnvironment) totalscore = 1;
			break;
		case (Neta::NoRingKeyword):
			if (context->nodeType() == NetaNode::RingNode) msg.print("NETA: Invalid context (Ring) for 'noring' keyword.\n");
			else
			{
				totalscore = 1;
				for (Ring *r = parent()->targetRingList()->first(); r != NULL; r = r->next) if (r->containsAtom(target)) totalscore = -1;
			}
			break;
		case (Neta::NonAromaticKeyword):
			if (context->nodeType() == NetaNode::RingNode) totalscore = ((NetaRingNode*) context)->currentRing()->type() != Ring::AromaticRing ? 1 : -1;
			else if (target->environment() != Atom::AromaticEnvironment) totalscore = 1;
			break;
		case (Neta::NotSelfKeyword):
			if (context->nodeType() == NetaNode::RootNode) msg.print("NETA: Invalid context for 'notself' keyword.\n");
			else if (target != prevTarget) totalscore = 1;
			break;
		case (Neta::PlanarKeyword):
			if (target->isPlanar(15.0)) totalscore = 1;
			break;
		default:
			msg.print("Internal NETA Error: Unrecognised keyword in NetaKeywordNode::score.\n");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Keyword (%s) = %i", Neta::netaKeyword(netaKeyword_), totalscore);
	msg.exit("NetaKeywordNode::score");
	return totalscore;
}

// Print node contents
void NetaKeywordNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Keyword Node: %s)\n", tab, Neta::netaKeyword(netaKeyword_));
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaKeywordNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaKeywordNode::netaPrint");
	neta.cat(Neta::netaKeyword(netaKeyword_));
	msg.exit("NetaKeywordNode::netaPrint");
}

// Clone node structure
NetaNode *NetaKeywordNode::clone(Neta *newparent)
{
	msg.enter("NetaKeywordNode::clone");
	NetaKeywordNode *node = new NetaKeywordNode(netaKeyword_);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	msg.exit("NetaKeywordNode::clone");
	return node;
}

/*
// NetaGeometryNode
*/

// Constructor
NetaGeometryNode::NetaGeometryNode(Atom::AtomGeometry ag)
{
	// Private variables
	geometry_ = ag;
	nodeType_ = NetaNode::GeometryNode;
}

// Validation function (virtual)
int NetaGeometryNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaGeometryNode::score");
	int totalscore = -1;
	Atom::AtomGeometry ag = target->geometry();
	if (ag == geometry_) totalscore = 1;
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Geometry (%s == %s) = %i", Atom::atomGeometry(ag), Atom::atomGeometry(geometry_), totalscore);
	msg.exit("NetaGeometryNode::score");
	return totalscore;
}

// Print node contents
void NetaGeometryNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Geometry Node: %s)\n", tab, Atom::atomGeometry(geometry_));
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaGeometryNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaGeometryNode::netaPrint");
	neta.cat(Atom::atomGeometry(geometry_));
	msg.exit("NetaGeometryNode::netaPrint");
}

// Clone node structure
NetaNode *NetaGeometryNode::clone(Neta *newparent)
{
	msg.enter("NetaGeometryNode::clone");
	NetaGeometryNode *node = new NetaGeometryNode(geometry_);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	msg.exit("NetaGeometryNode::clone");
	return node;
}

/*
// NetaValueNode
*/

// Constructor
NetaValueNode::NetaValueNode(Neta::NetaValue nv, Neta::NetaValueComparison nvc, int value)
{
	// Private variables
	netaValue_ = nv;
	netaComparison_ = nvc;
	value_ = value;
	nodeType_ = NetaNode::ValueNode;
}

// Validation function (virtual)
int NetaValueNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaValueNode::score");
	int totalscore = -1, n;
	Bond *b;
	Refitem<Bond,int> *rb;
	switch (netaValue_)
	{
		case (Neta::BondValue):
			if (prevTarget == NULL) msg.print("NETA: Invalid context for 'bond=xxx' in NETA for type %s/%i.\n", parent()->parentForcefieldAtom()->name(), parent()->parentForcefieldAtom()->typeId());
			else
			{
				b = target->findBond(prevTarget);
				if (Neta::netaValueCompare(b->type(), netaComparison_, value_)) totalscore = 1;
			}
			break;
		case (Neta::NBondsValue):
			if (Neta::netaValueCompare(target->nBonds(), netaComparison_, value_)) totalscore = 1;
			break;
		case (Neta::NHydrogensValue):
			n = 0;
			for (rb = target->bonds(); rb != NULL; rb = rb->next) if (rb->item->partner(target)->element() == 1) ++n;
			if (Neta::netaValueCompare(n, netaComparison_, value_)) totalscore = 1;
			break;
		case (Neta::OxidationStateValue):
			if (Neta::netaValueCompare(target->os(), netaComparison_, value_)) totalscore = 1;
			break;
		case (Neta::RepeatValue):
			// Accounted for in Bound, Ring or Chain node
			totalscore = 0;
			break;
		case (Neta::SizeValue):
			// Only valid for Chains and Ring
			switch (context->nodeType())
			{
				case (NetaNode::RingNode):
					n = ((NetaRingNode*) context)->currentRing()->nAtoms();
					if (Neta::netaValueCompare(n, netaComparison_, value_)) totalscore = 1;
					else totalscore = -1;
					break;
			}
			break;
		default:
			msg.print("Internal NETA Error: Unrecognised value in NetaValueNode::score.\n");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Value (%s %s %i) = %i", Neta::netaValue(netaValue_), Neta::netaValueComparison(netaComparison_), value_, totalscore);
	msg.exit("NetaValueNode::score");
	return totalscore;
}

// Print node contents
void NetaValueNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Value Node: %s %s %i)\n", tab, Neta::netaValue(netaValue_), Neta::netaValueComparison(netaComparison_), value_);
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaValueNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaValueNode::netaPrint");
	neta.cat(Neta::netaValue(netaValue_));
	neta.cat(Neta::netaValueComparison(netaComparison_));
	neta.cat(itoa(value_));
	msg.exit("NetaValueNode::netaPrint");
}

// Clone node structure
NetaNode *NetaValueNode::clone(Neta *newparent)
{
	msg.enter("NetaValueNode::clone");
	NetaValueNode *node = new NetaValueNode(netaValue_, netaComparison_, value_);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	msg.exit("NetaValueNode::clone");
	return node;
}

/*
// NetaRootNode
*/

// Constructor
NetaRootNode::NetaRootNode()
{
	// Private variables
	nodeType_ = NetaNode::RootNode;
}

// Validation function (virtual)
int NetaRootNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	return (innerNeta_ != NULL ? innerNeta_->score(target, nbrs, rings, this, prevTarget, level) : 0);
}

// Print node contents
void NetaRootNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Root Node:)\n", tab);
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, prefix);
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaRootNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaRootNode::netaPrint");
	if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
	msg.exit("NetaRootNode::netaPrint");
}

// Clone node structure
NetaNode *NetaRootNode::clone(Neta *newparent)
{
	msg.enter("NetaRootNode::clone");
	NetaRootNode *node = new NetaRootNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	if (innerNeta_ != NULL) node->innerNeta_ = innerNeta_->clone(newparent);
	msg.exit("NetaRootNode::clone");
	return node;
}

/*
// NetaRingNode
*/

// Constructor
NetaRingNode::NetaRingNode()
{
	// Private variables
	nodeType_ = NetaNode::RingNode;
}

// Retrieve current ring under consideration
Ring *NetaRingNode::currentRing()
{
	return currentRing_;
}

// Validation function (virtual)
int NetaRingNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaRingNode::score");
	int totalscore = 0, n;
	Refitem<Ring,int> *ri;
	Reflist<Atom,int> atomCheckList;
	Reflist< Refitem<Ring,int>, int > scores;
	Refitem< Refitem<Ring,int>, int > *si;
	Ring *r;
	// Pointer check
	if (rings == NULL)
	{
		msg.print("NETA Internal Error: Called NetaRingNode::score() without a valid ring list.\n");
		msg.exit("NetaRingNode::score");
		return -1;
	}
	// Exactly how we proceed here depends on the current context (i.e. whether in Bound, Ring, Chain, etc.)
	switch (context->nodeType())
	{
		case (NetaNode::RootNode):
		case (NetaNode::BoundNode):
			// If we have no items in the list then we've failed (ring specified, but no rings found)
			if (rings->nItems() == 0)
			{
				totalscore = -1;
				break;
			}
			// Now, find number of these rings that match our ring description
			for (ri = rings->first(); ri != NULL; ri = ri->next)
			{
				si = scores.add(ri, 0);
				currentRing_ = ri->item;
				if (innerNeta_ == NULL) continue;
				// Add atoms in this ring to an atomCheckList
				ri->item->addAtomsToReflist(&atomCheckList,NULL);
				// Get a match score for the innerNeta
				si->data = innerNeta_->score(target, &atomCheckList, NULL, this, prevTarget, level+1);
			}
			// Calculate how many rings we matched, and if this satisfies any repeat condition
			n = 0;
			for (si = scores.first(); si != NULL; si = si->next) if (si->data != -1) ++n;
			if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
			{
				n = repeat_ == -1 ? 1 : repeat_;
				ri = rings->first();
				totalscore = 0;
				for (si = scores.first(); si != NULL; si = si->next)
				{
					if (si->data != -1)
					{
						totalscore += si->data;
						rings->remove(si->item);
					}
					--n;
					if (n == 0) break;
				}
			}
			else totalscore = -1;
			break;
		case (NetaNode::RingNode):
			msg.print("NETA Error: Specifying a 'ring' directly inside another 'ring' is meaningless.\n");
			totalscore = -1;
			break;
		default:
			printf("NetaRingNode::score : Unrecognised context.\n");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Ring Check (%i required) = %i", repeat_ == -1 ? 1 : repeat_, totalscore);
	msg.exit("NetaRingNode::score");
	return totalscore;
}

// Print node contents
void NetaRingNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Ring Node:)\n", tab);
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", tab);
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaRingNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaRingNode::netaPrint");
	neta.cat("ring");
	if (innerNeta_ != NULL)
	{
		neta += '(';
		innerNeta_->netaPrint(neta);
		neta += ')';
	}
	msg.exit("NetaRingNode::netaPrint");
}

// Clone node structure
NetaNode *NetaRingNode::clone(Neta *newparent)
{
	msg.enter("NetaRingNode::clone");
	NetaRingNode *node = new NetaRingNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	newparent->ownNode(node);
	msg.exit("NetaRingNode::clone");
	return node;
}

/*
// NetaChainNode
*/

// Constructor
NetaChainNode::NetaChainNode()
{
	// Private variables
	nodeType_ = NetaNode::ChainNode;
}

// Private (recursive) scoring function
int NetaChainNode::score(NetaNode *currentNode, int nRepeat, Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, Atom *prevTarget, int level)
{
	msg.enter("NetaChainNode::score(private)");
	int totalscore = -1, atomscore = -1;
	Atom *i, *j;
	Refitem<Atom,int> *ri, *rj, *rk;
	// The target atom we're passed is the last atom in the current chain (or the originating atomic centre).
	// The neighbours list should contain a single bound atom reference that we are interested in checking at this point in the chain.
	// So, determine if the current chain node matches the target/nbrs combination.
	// Sanity check first...
	if (nbrs->nItems() != 1) printf("Internal NETA Error: Atom target in ChainNode not passed with exactly 1 neighbour (nNbrs = %i)\n", nbrs->nItems());
	i = nbrs->first()->item;
	totalscore = currentNode->score(target, nbrs, rings, this, prevTarget, level);
	if (totalscore != -1)
	{
		// This node matched, so proceed to branch into all neighbours on the atom contained in 'nbrs'
		// Unless, of course, this is the last node in the list, in which case exit - we're happy.
		if ((currentNode->nextNode == NULL) && (nRepeat == 1))
		{
// 			printf("Finished constructing chain succesfully.\n");
			msg.exit("NetaChainNode::score(private)");
			return totalscore;
		}
		currentChain_.add(i);
// 		printf("Added atom id %i to chain\n", currentChain_.last() == NULL ? -999 : currentChain_.last()->item->id());
// 		printf("CHAIN = "); for (rj = currentChain_.first(); rj != NULL; rj = rj->next) printf(" %i", rj->item->id()); printf("\n");
		// Cycle over new bound list for matches to the next node, exiting immediately if we get a positive match
		for (Refitem<Bond,int> *b = i->bonds(); b != NULL; b = b->next)
		{
			// Get bound partner
			j = b->item->partner(i);
			// Skip if this atom already exists in the chain
			if (currentChain_.search(j) != NULL) continue;
			// Construct new, single-item bound reflist
			Reflist<Atom,int> boundList;
			boundList.add(j);
			// Score it...
			if (nRepeat > 1) atomscore = score(currentNode, nRepeat-1, i, &boundList, rings, target, level);
			else
			{
				// Cast the next node pointer up to a BoundNode and get its repeat value
				NetaContextNode *ncn = (NetaContextNode*) (currentNode->nextNode);
// 				printf("ncn = %p, repeat is %i, last atom id is %i\n", ncn, ncn->repeat(), currentChain_.last() == NULL ? -999 : currentChain_.last()->item->id());
				atomscore = score(ncn, ncn->repeat() == -1 ? 1 : ncn->repeat(), i, &boundList, rings, target, level);
			}
			// Check atomscore - if failed, continue loop.
			if (atomscore == -1) continue;
			else break;
		}
		// Did we match a bound atom?
		if (atomscore == -1) totalscore = -1;
		else totalscore += atomscore;
		currentChain_.removeLast();
	}
	// No check for reverse logic needs to be done here since it is accounted for by NetaBoundNode::score().
	msg.exit("NetaChainNode::score(private)");
	return totalscore;
}

// Validation function (virtual)
int NetaChainNode::score(Atom *target, Reflist<Atom,int> *nbrs, Reflist<Ring,int> *rings, NetaContextNode *context, Atom *prevTarget, int level)
{
	msg.enter("NetaChainNode::score");
	int totalscore = -1, n;
	Refitem<Atom,int> *ri;
	Reflist< Refitem<Atom,int>, int > scores;
	Refitem< Refitem<Atom,int>, int > *si;
	Reflist<Atom,int> boundList;
	// Start off the private recursive method with a call centred on the first of the bound neigbours
	currentChain_.clear();
	currentChain_.add(target);
	for (ri = nbrs->first(); ri != NULL; ri = ri->next)
	{
		si = scores.add(ri, 0);
		boundList.clear();
		boundList.add(ri->item);
		if (linearNeta_ == NULL) { si->data = 0; msg.print("Warning: No chain description specified in 'chain' command.\n"); }
		else si->data = score(linearNeta_, 1, target, &boundList, rings, NULL, level);
	}
	// Execute innerNeta?
	// innerNeta_->score();
	// How many matches?
	n = 0;
	for (si = scores.first(); si != NULL; si = si->next) if (si->data != -1) ++n;
	if (n == 0) totalscore = -1;
	else if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
	{
		n = repeat_ == -1 ? 1 : repeat_;
		ri = nbrs->first();
		totalscore = 0;
		for (si = scores.first(); si != NULL; si = si->next)
		{
			if (si->data != -1)
			{
				totalscore += si->data;
				nbrs->remove(si->item);
			}
			--n;
			if (n == 0) break;
		}
	}
	else totalscore = -1;
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Chain Check (%i required) = %i", repeat_ == -1 ? 1 : repeat_, totalscore);
	msg.exit("NetaChainNode::score");
	return totalscore;
}

// Print node contents
void NetaChainNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("%s (Chain Node:)\n", tab);
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", tab);
	delete[] tab;
}

// Print (append) NETA representation of node contents
void NetaChainNode::netaPrint(Dnchar &neta)
{
	msg.enter("NetaChainNode::netaPrint");
	neta.cat("chain");
	if (innerNeta_ != NULL)
	{
		neta += '(';
		if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
		neta += ')';
	}
	msg.exit("NetaChainNode::netaPrint");
}

// Clone node structure
NetaNode *NetaChainNode::clone(Neta *newparent)
{
	msg.enter("NetaChainNode::clone");
	NetaChainNode *node = new NetaChainNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	newparent->ownNode(node);
	msg.exit("NetaChainNode::clone");
	return node;
}

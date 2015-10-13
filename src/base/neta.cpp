/*
	*** NETA type description
	*** src/base/neta.cpp
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
#include "base/sysfunc.h"
#include "ff/forcefield.h"
#include "base/forcefieldatom.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// NETA Keywords
const char* NetaKeywordKeywords[Neta::nNetaKeywords] = { "alphatic", "aromatic", "noring", "nonaromatic", "notprev", "notself", "planar" };
Neta::NetaKeyword Neta::netaKeyword(QString s, bool reportError)
{
	Neta::NetaKeyword n = (Neta::NetaKeyword) enumSearch("NETA keyword",Neta::nNetaKeywords,NetaKeywordKeywords,s, reportError);
	if ((n == Neta::nNetaKeywords) && reportError) enumPrintValid(Neta::nNetaKeywords,NetaKeywordKeywords);
	return n;
}
const char* Neta::netaKeyword(Neta::NetaKeyword nk)
{
	return NetaKeywordKeywords[nk];
}

// NETA values
const char* NetaValueKeywords[Neta::nNetaValues] = { "bond", "nbonds", "nh", "os", "n", "size" };
Neta::NetaValue Neta::netaValue(QString s, bool reportError)
{
	Neta::NetaValue n = (Neta::NetaValue) enumSearch("NETA value",Neta::nNetaValues,NetaValueKeywords,s, reportError);
	if ((n == Neta::nNetaValues) && reportError) enumPrintValid(Neta::nNetaValues,NetaValueKeywords);
	return n;
}
const char* Neta::netaValue(Neta::NetaValue nv)
{
	return NetaValueKeywords[nv];
}

// NETA expanders
const char* NetaExpanderKeywords[Neta::nNetaExpanders] = { "-", "chain", "=", "geometry", "path", "ring" };
Neta::NetaExpander Neta::netaExpander(QString s, bool reportError)
{
	Neta::NetaExpander n = (Neta::NetaExpander) enumSearch("NETA expander",Neta::nNetaExpanders,NetaExpanderKeywords,s, reportError);
	if ((n == Neta::nNetaExpanders) && reportError) enumPrintValid(Neta::nNetaExpanders,NetaExpanderKeywords);
	return n;
}

// NETA Value comparison operators
const char* NetaValueComparisonKeywords[Neta::nNetaValueComparisons] = { "=", "!=", ">", "<", ">=", "<=" };
const char* Neta::netaValueComparison(Neta::NetaValueComparison nvc)
{
	return NetaValueComparisonKeywords[nvc];
}
bool Neta::netaValueCompare(int lhsvalue, NetaValueComparison nvc, int rhsvalue)
{
	bool result = false;
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
		default:
			printf("Internal Error: Unrecognised operator (%i) in Neta::netaValueCompare.\n", nvc);
			break;
	}
	return result;
}

// Node logic types
const char* NetaLogicTypeKeywords[Neta::nNetaLogicTypes] = { "and", "or", "and-not" };
const char* Neta::netaLogic(NetaLogicType lt)
{
	return NetaLogicTypeKeywords[lt];
}

/*
 * Neta Parent Structure
 */

Neta::Neta() : ListItem<Neta>()
{
	// Private variables
	characterElement_ = -1;
	parentForcefield_ = NULL;
	parentForcefieldAtom_ = NULL;
	targetAtom_ = NULL;
	targetRingList_ = NULL;
	targetParent_ = NULL;
}

// Destructors
Neta::~Neta()
{
}

// Set parent forcefield
void Neta::setParentForcefield(Forcefield* ff)
{
	parentForcefield_ = ff;
}

// Return parent forcefield
Forcefield* Neta::parentForcefield()
{
	return parentForcefield_;
}

// Set parent forcefield atom
void Neta::setParentForcefieldAtom(ForcefieldAtom* ffa)
{
	parentForcefieldAtom_ = ffa;
}

// Return parent forcefield atom
ForcefieldAtom* Neta::parentForcefieldAtom()
{
	return parentForcefieldAtom_;
}

// Return character element
void Neta::setCharacterElement(int el)
{
	characterElement_ = el;
}

// Return character element
int Neta::characterElement() const
{
	return characterElement_;
}

// Take ownership of selected node
void Neta::ownNode(NetaNode* node)
{
	ownedNodes_.own(node);
}

// Return reference name (if a define)
QString Neta::name() const
{
	return name_;
}

// Set reference name (if a define)
void Neta::setName(QString name)
{
	name_ = name;
}

// Return top of description nodelist
NetaRootNode* Neta::description()
{
	return description_;
}

// Print Atom Type data
void Neta::print() const
{
	printf("Character element is %i\n", characterElement_);
	QString neta;
	if (description_ != NULL) description_->netaPrint(neta);
	printf("NETA string is '%s'\n", qPrintable(neta));
	printf("Node description is:\n");
	if (description_ != NULL) description_->nodePrint(0,"");
	else printf("   None defined.\n");
}

// Print Atom Type data to string supplied
void Neta::netaPrint(QString& neta) const
{
	neta = "";
	if (description_ != NULL) description_->netaPrint(neta);
}

// Clone nodes (and own them) beginning from the node supplied
NetaNode* Neta::clone(NetaNode* topnode)
{
	Messenger::enter("Neta::clone");
	NetaNode* result;
	// Just call top node's routine, and get the result
	if (topnode == NULL) result = NULL;
	else result = topnode->clone(this);
	Messenger::exit("Neta::clone");
	return result;
}

// Clear all associated node data (but leave character element as-is)
void Neta::clear()
{
	description_ = NULL;
	ownedNodes_.clear();
}

// Return current atom target
Atom* Neta::targetAtom()
{
	return targetAtom_;
}

// Return ringList of supplied atom
List<Ring> *Neta::targetRingList()
{
	return targetRingList_;
}

// Return target atom's parent model
Model* Neta::targetParent()
{
	return targetParent_;
}

// Check supplied atom to see if it matches this NETA description
int Neta::matchAtom(Atom* i, List<Ring>* rings, Model* parent)
{
	Messenger::enter("Neta::matchAtom");
	// Check element type first
	if (i->element() != characterElement_)
	{
		Messenger::exit("Neta::matchAtom");
		return -1;
	}
	// Is a description provided?
	if (description_ == NULL)
	{
		Messenger::exit("Neta::matchAtom");
		return 1;
	}
	// Store ring list and parent model of atom
	targetRingList_ = rings;
	targetParent_ = parent;
	targetAtom_ = i;
	// Create a bound list of atoms and a list of rings to pass to the head of the description
	RefList<Atom,int> boundList;
	i->addBoundToRefList(&boundList);
	RefList<Ring,int> ringList;
	if (targetRingList_) for (Ring *r = targetRingList_->first(); r != NULL; r = r->next) if (r->containsAtom(i)) ringList.add(r);
	RefList<Atom,int> path;
	int score = description_->score(i, &boundList, &ringList, description_, path, 0);
// 	printf("Score is %i\n", score);
	targetAtom_ = NULL;
	targetRingList_ = NULL;
	targetParent_ = NULL;
	Messenger::exit("Neta::matchAtom");
	return (score == -1 ? -1 : score+1);
}

// Link forcefield type references in elementtype lists
void Neta::linkReferenceTypes()
{
	Messenger::enter("Neta::linkReferenceTypes");
	NetaBoundNode* bnode;
	for (NetaNode* node = ownedNodes_.first(); node != NULL; node = node->next)
	{
		if (node->nodeType() != NetaNode::BoundNode) continue;
		bnode = (NetaBoundNode*) node;
		bnode->linkReferenceTypes();
	}
	Messenger::exit("Neta::linkReferenceTypes");
}

// Create a basic description for the specified Atom
bool Neta::createBasic(Atom* i, bool explicitBondType, double torsionTolerance)
{
	// Clear any existing data
	clear();

	// Check atom pointer
	if (i == NULL) return false;

	// Set character element and create NETA
	setCharacterElement(i->element());
	Atom* j, *k, *l;
	RefListItem<Bond,int>* rb, *rb2, *rb3;
	Bond* b, *b2, *b3;
	char bondType;
	QString typeDesc = "nbonds=" + QString::number(i->nBonds()), torsionDesc, bit;
	for (rb = i->bonds(); rb != NULL; rb = rb->next)
	{
		b = rb->item;
		j = b->partner(i);

		if (!explicitBondType) bondType = '~';
		else if (b->type() == Bond::Single) bondType = '-';
		else if (b->type() == Bond::Double) bondType = '=';
		else bondType = '~';
		bit.sprintf(",%c%s(nbonds=%i", bondType, Elements().symbol(j->element()), j->nBonds());
		typeDesc += bit;

		// Loop over secondary atoms
		for (rb2 = j->bonds(); rb2 != NULL; rb2 = rb2->next)
		{
			b2 = rb2->item;
			k = b2->partner(j);
			if (k == i) continue;

			if (!explicitBondType) bondType = '~';
			else if (b2->type() == Bond::Single) bondType = '-';
			else if (b2->type() == Bond::Double) bondType = '=';
			else bondType = '~';
			bit.sprintf(",%c%s(nbonds=%i)", bondType, Elements().symbol(k->element()), k->nBonds());
			typeDesc += bit;

			if ((torsionTolerance > 0.0) && i->parent())
			{
				for (rb3 = k->bonds(); rb3 != NULL; rb3 = rb3->next)
				{
					b3 = rb3->item;
					l = b3->partner(k);
					if (l == j) continue;
// 					if (l->element() == 1) continue;
					bit.sprintf(",geometry(%f,%f,~%s,~%s,~%s)", i->parent()->torsion(i, j, k, l), torsionTolerance, Elements().symbol(j), Elements().symbol(k), Elements().symbol(l));
					torsionDesc += bit;
				}
			}
		}
		typeDesc += ')';
	}
	if (!torsionDesc.isEmpty()) typeDesc += torsionDesc;

	if (!netaparser.createNeta(this, typeDesc, NULL))
	{
		Messenger::print("Failed to create type description in Neta::createBasic().");
		return false;
	}

	Messenger::print(Messenger::Verbose, "Create basic NETA for atom %p : %s", i, qPrintable(typeDesc));

	return true;
}

/*
 * NetaNode
 */

NetaNode::NetaNode() : ListItem<NetaNode>()
{
	// Private variables
	nodeType_ = NetaNode::nNetaNodeTypes;
	parent_ = NULL;
	reverseLogic_ = false;

	// Public variables
	prevNode = NULL;
	nextNode = NULL;
}

//Destructor
NetaNode::~NetaNode()
{
}

// Return node type
NetaNode::NetaNodeType NetaNode::nodeType()
{
	return nodeType_;
}

// Set node to use reverse logic
void NetaNode::setReverseLogic()
{
	reverseLogic_ = true;
}

// Return whether to use reverse logic when returning the final value
bool NetaNode::reverseLogic() const
{
	return reverseLogic_;
}

// Return parent NETA structure
Neta* NetaNode::parent()
{
	return parent_;
}

// Set parent NETA structure
void NetaNode::setParent(Neta* neta)
{
	parent_ = neta;
}

// Print contextual score
void NetaNode::printScore(int level, const char* fmt, ...)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<level-1; n++) tab += '\t';
	if (level > 1) tab += "   |--> ";
	if (level == 1) tab += '\t';

	va_list arguments;
	static char msgs[8096];
	msgs[0] = '\0';
	// Parse the argument list (...) and internally write the output string into msgs[]
	va_start(arguments,fmt);
	vsprintf(msgs,fmt,arguments);
	va_end(arguments);
	// Output node data
	Messenger::print(Messenger::Typing, "NETA%03i:%s%s", level, qPrintable(tab), msgs);
}

/*
 * NetaContextNode
 */

// Constructor
NetaContextNode::NetaContextNode()
{
	// Private variables
	repeat_ = -1;
	repeatComparison_ = Neta::nNetaValueComparisons;
	innerNeta_ = NULL;
	linearNeta_ = NULL;
}

//Destructor
NetaContextNode::~NetaContextNode()
{
}

// Set repeat specifier
void NetaContextNode::setRepeat(int n)
{
	repeat_ = n;
}

// Return repetition specified
int NetaContextNode::repeat() const
{
	return repeat_;
}

// Set value comparison
void NetaContextNode::setRepeatComparison(Neta::NetaValueComparison nvc)
{
	repeatComparison_ = nvc;
}

// Set inner neta
void NetaContextNode::setInnerNeta(NetaNode* innerneta, NetaNode* linearneta)
{
	innerNeta_ = innerneta;
	linearNeta_ = linearneta;
}

// Return inner neta description
NetaNode* NetaContextNode::innerNeta()
{
	return innerNeta_;
}

// Clone node structure
NetaNode* NetaContextNode::clone(Neta* newparent)
{
	printf("NetaContextNode::clone() should never be called (supposed new parent was %p).\n", newparent);
	return NULL;
}

/*
 * NetaLogicNode
 */

// Constructor
NetaLogicNode::NetaLogicNode(Neta::NetaLogicType nt, NetaNode* arg1, NetaNode* arg2)
{
	// Private variables
	nodeType_ = NetaNode::LogicNode;
	netaLogic_ = nt;
	argument1_ = arg1;
	argument2_ = arg2;
};

//Destructor
NetaLogicNode::~NetaLogicNode()
{
}


// Validation function (virtual)
int NetaLogicNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaLogicNode::score");
	int score1 = -1, score2 = -1, totalscore = -1;
	switch (netaLogic_)
	{
		case (Neta::NetaAndLogic):
			score1 = argument1_->score(target, nbrs, rings, context, path, level);
			if (score1 != -1)
			{
				score2 = argument2_->score(target, nbrs, rings, context, path, level);
				if (score2 != -1) totalscore = score1 + score2;
			}
			break;
		case (Neta::NetaOrLogic):
			score1 = argument1_->score(target, nbrs, rings, context, path, level);
			if (score1 != -1) totalscore = score1;
			else
			{
				score2 = argument2_->score(target, nbrs, rings, context, path, level);
				if (score2 != -1) totalscore = score2;
			}
			break;
		case (Neta::NetaAndNotLogic):
			score1 = argument1_->score(target, nbrs, rings, context, path, level);
			if (score1 != -1)
			{
				score2 = argument2_->score(target, nbrs, rings, context, path, level);
				if (score2 == -1) totalscore = score1;
			}
			break;
		default:
			printf("Internal Error: Unrecognised logic in Neta::score.\n");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	Messenger::exit("NetaLogicNode::score");
	return totalscore;
}

// Print node contents
void NetaLogicNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;

	// Output node data
	printf("%s (Logic Node: %s)\n", qPrintable(tab), Neta::netaLogic(netaLogic_));
	argument1_->nodePrint(offset+1, "");
	argument2_->nodePrint(offset+1, "");
}

// Print (append) NETA representation of node contents
void NetaLogicNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaLogicNode::netaPrint");
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
			neta += "&!";
			break;
		default:
			printf("Internal Error: Unrecognised logic in Neta::netaPrint.\n");
			break;
	}
	argument2_->netaPrint(neta);
	Messenger::exit("NetaLogicNode::netaPrint");
}

// Clone node structure
NetaNode* NetaLogicNode::clone(Neta* newparent)
{
	Messenger::enter("NetaLogicNode::clone");
	NetaLogicNode* node = new NetaLogicNode(netaLogic_, NULL, NULL);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->argument1_ = argument1_ == NULL ? NULL : argument1_->clone(newparent);
	node->argument2_ = argument2_ == NULL ? NULL : argument2_->clone(newparent);
	newparent->ownNode(node);
	Messenger::exit("NetaLogicNode::clone");
	return node;
}

/*
 * NetaBoundNode
 */

// Constructor
NetaBoundNode::NetaBoundNode()
{
	// Private variables
	nodeType_ = NetaNode::BoundNode;
}

//Destructor
NetaBoundNode::~NetaBoundNode()
{
}

// Print node contents
void NetaBoundNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Bound Node: %s)\n", qPrintable(tab), Bond::bondType(bondType_));
	// Print element/type info
	printf("%s    Number of allowed elements/types defined = %i\n", qPrintable(tab), allowedElementsAndTypes_.nItems());
	printf("%s      ", qPrintable(tab));
	for (RefListItem<ForcefieldAtom,int>* ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		if (ri->data == 0) printf("Any ");
		else if (ri->data > 0) printf("%s ", Elements().symbol(ri->data));
		else printf("&%i ", -ri->data);
	}
	printf("\n");
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", qPrintable(tab));
}

// Print (append) NETA representation of node contents
void NetaBoundNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaBoundNode::netaPrint");
	neta += elementsAndTypesString();
	if (innerNeta_ != NULL)
	{
		neta += '(';
		if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
		neta += ')';
	}
	// Is there an attached (linear) node? If so, add it on to this description
	if (nextNode != NULL) nextNode->netaPrint(neta);
	Messenger::exit("NetaBoundNode::netaPrint");
}

// Clone node structure
NetaNode* NetaBoundNode::clone(Neta* newparent)
{
	Messenger::enter("NetaBoundNode::clone");
	NetaBoundNode* node = new NetaBoundNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->allowedElementsAndTypes_ = allowedElementsAndTypes_;
	node->bondType_ = bondType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	newparent->ownNode(node);
	Messenger::exit("NetaBoundNode::clone");
	return node;
}

// Set bound data
void NetaBoundNode::set(RefListItem<ForcefieldAtom,int>* elemtypes, NetaNode* innerneta, Bond::BondType bt)
{
	Messenger::enter("NetaBoundNode::set");
	bondType_ = bt;
	innerNeta_ = innerneta;
	// Take items from list
	RefListItem<ForcefieldAtom,int>* item = elemtypes, *nextitem = NULL;
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
	Messenger::exit("NetaBoundNode::set");
}

// Link forcefield type references in elementtype lists
void NetaBoundNode::linkReferenceTypes()
{
	ForcefieldAtom* ffa;
	for (RefListItem<ForcefieldAtom,int>* ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		// Skip if its just an element
		if (ri->data >= 0) continue;
		// Find referenced type in forcefield
		if (parent()->parentForcefield() == NULL)
		{
			Messenger::print("Warning: Type '%s' (id %i) contains references to other types, but no parent Forcefield is defined.");
			Messenger::print("         --> Reset to '0' (Any element)");
			ri->data = 0;
		}
		else
		{
			ffa = parent()->parentForcefield()->findType(abs(ri->data));
			ri->item = ffa;
			if (ffa == NULL) Messenger::print("Warning: Type '%s' (id %i) references type id %i in it's NETA description, but type id %i has not been defined in the forcefield.", qPrintable(parent()->parentForcefieldAtom()->name()), parent()->parentForcefieldAtom()->typeId(), abs(ri->data), abs(ri->data));
		}
	}
}

// Validation function to check supplied atom against allowed elements and types
int NetaBoundNode::atomScore(Atom* target)
{
	Messenger::enter("NetaBoundNode::atomScore");
	int totalscore = -1;
	for (RefListItem<ForcefieldAtom,int>* ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
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
	Messenger::exit("NetaBoundNode::atomScore");
	return totalscore;
}

// Create formatted element/type list
QString NetaBoundNode::elementsAndTypesString()
{
	Messenger::enter("NetaBoundNode::elementsAndTypesString");
	QString s;
	switch (bondType_)
	{
		case (Bond::Any):
			s = "~";
			break;
		case (Bond::Single):
			s = "-";
			break;
		case (Bond::Double):
			s = "=";
			break;
		default:
			Messenger::print("NETA Internal Error: Can't convert this bond type to a single character.");
			s = "_";
			break;
	}
	if (allowedElementsAndTypes_.nItems() != 1) s += '[';
	for (RefListItem<ForcefieldAtom,int>* ri = allowedElementsAndTypes_.first(); ri != NULL; ri = ri->next)
	{
		if (ri != allowedElementsAndTypes_.first()) s += ',';
		if ((ri->item != NULL) || (ri->data < 0))
		{
			s += '&';
			s += QString::number(abs(ri->data));
		}
		else if (ri->data == 0) s += "Any";
		else s += Elements().symbol(ri->data);
	}
	if (allowedElementsAndTypes_.nItems() != 1) s += ']';
	Messenger::exit("NetaBoundNode::elementsAndTypesString");
	return s;
}

// Validation function (virtual)
int NetaBoundNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaBoundNode::score");
	int totalscore = -1, n, boundscore;
	RefListItem<Atom,int>* ri;
	RefList< RefListItem<Atom,int>, int > scores;
	RefListItem< RefListItem<Atom,int>, int > *si;
	// Pointer check
	if (nbrs == NULL)
	{
		Messenger::print("NETA Internal Error: Called NetaBoundNode::score() without a valid neighbour list.");
		Messenger::exit("NetaBoundNode::score");
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
				Bond* b = target->findBond(ri->item);
				if (b == NULL)
				{
					Messenger::print("NETA Internal Error: Couldn't find bond between atom ids %i and %i to check type.", target->id(), ri->item->id());
					Messenger::exit("NetaBoundNode::score");
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
				RefList<Atom,int> boundList;
				ri->item->addBoundToRefList(&boundList);
				// Construct new ringlist
				RefList<Ring,int> ringList;
				for (Ring *r = parent()->targetRingList()->first(); r != NULL; r = r->next) if (r->containsAtom(ri->item)) ringList.add(r);
				path.add(target);
				boundscore = innerNeta_->score(ri->item, &boundList, &ringList, this, path, level+1);
				path.removeLast();
				if (boundscore != -1) si->data += boundscore;
				else si->data = -1;
			}
		}
		n = 0;
		for (si = scores.first(); si != NULL; si = si->next) if (si->data > 0) ++n;
		if (n == 0) totalscore = -1;
		else if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
		{
			n = repeat_ == -1 ? 1 : repeat_;
			ri = nbrs->first();
			totalscore = 0;
			for (si = scores.first(); si != NULL; si = si->next)
			{
				if (si->data > 0)
				{
					totalscore += si->data;
					nbrs->remove(si->item);
					--n;
				}
				if (n == 0) break;
			}
		}
		else totalscore = -1;
		if (totalscore == 0) totalscore = -1;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Bound Check (%i of %s) = %i", repeat_ == -1 ? 1 : repeat_, qPrintable(elementsAndTypesString()), totalscore);
	Messenger::exit("NetaBoundNode::score");
	return totalscore;
}

/*
 * NetaKeywordNode
 */

// Constructor
NetaKeywordNode::NetaKeywordNode(Neta::NetaKeyword nk)
{
	// Private variables
	netaKeyword_ = nk;
	nodeType_ = NetaNode::KeywordNode;
}

//Destructor
NetaKeywordNode::~NetaKeywordNode()
{
}

// Validation function (virtual)
int NetaKeywordNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaKeywordNode::score");
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
			if (context->nodeType() == NetaNode::RingNode) Messenger::print("NETA: Invalid context (Ring) for 'noring' keyword.");
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
		case (Neta::NotPrevKeyword):
			if (context->nodeType() == NetaNode::RootNode) Messenger::print("NETA: Invalid context for 'notprev' keyword.");
			else if (context->nodeType() == NetaNode::RingNode)
			{
				totalscore = (((NetaRingNode*) context)->currentRing()->containsAtom( parent()->targetAtom() ) ? -1 : 1);
			}
			else
			{
				// Need to step back twice in the path, since the last atom in the path is the atom we're actually 'sat' on
				if (path.last() == NULL) totalscore = 1;
				else if (path.last()->prev == NULL) totalscore = 1;
				else if (path.last()->prev->item != target) totalscore = 1;
			}
			break;
		case (Neta::NotSelfKeyword):
			if (context->nodeType() == NetaNode::RootNode) Messenger::print("NETA: Invalid context for 'notself' keyword.");
			else if (context->nodeType() == NetaNode::RingNode)
			{
				totalscore = (((NetaRingNode*) context)->currentRing()->containsAtom( parent()->targetAtom() ) ? -1 : 1);
			}
			else
			{
				if (path.first() == NULL) totalscore = 1;
				else if (path.first()->item != target) totalscore = 1;
			}
			break;
		case (Neta::PlanarKeyword):
			if (target->isPlanar(15.0)) totalscore = 1;
			break;
		default:
			Messenger::print("Internal NETA Error: Unrecognised keyword in NetaKeywordNode::score.");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Keyword (%s) = %i", Neta::netaKeyword(netaKeyword_), totalscore);
	Messenger::exit("NetaKeywordNode::score");
	return totalscore;
}

// Print node contents
void NetaKeywordNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;

	// Output node data
	printf("%s (Keyword Node: %s)\n", qPrintable(tab), Neta::netaKeyword(netaKeyword_));
}

// Print (append) NETA representation of node contents
void NetaKeywordNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaKeywordNode::netaPrint");
	neta += Neta::netaKeyword(netaKeyword_);
	Messenger::exit("NetaKeywordNode::netaPrint");
}

// Clone node structure
NetaNode* NetaKeywordNode::clone(Neta* newparent)
{
	Messenger::enter("NetaKeywordNode::clone");
	NetaKeywordNode* node = new NetaKeywordNode(netaKeyword_);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	Messenger::exit("NetaKeywordNode::clone");
	return node;
}

/*
 * NetaGeometryNode
 */

// Constructor
NetaGeometryNode::NetaGeometryNode(Atom::AtomGeometry ag)
{
	// Private variables
	geometry_ = ag;
	nodeType_ = NetaNode::GeometryNode;
}

//Destructor
NetaGeometryNode::~NetaGeometryNode()
{
}

// Validation function (virtual)
int NetaGeometryNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaGeometryNode::score");
	int totalscore = -1;
	Atom::AtomGeometry ag = target->geometry();
	if (ag == geometry_) totalscore = 1;
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Geometry (%s == %s) = %i", Atom::atomGeometry(ag), Atom::atomGeometry(geometry_), totalscore);
	Messenger::exit("NetaGeometryNode::score");
	return totalscore;
}

// Print node contents
void NetaGeometryNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Geometry Node: %s)\n", qPrintable(tab), Atom::atomGeometry(geometry_));
}

// Print (append) NETA representation of node contents
void NetaGeometryNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaGeometryNode::netaPrint");
	neta += Atom::atomGeometry(geometry_);
	Messenger::exit("NetaGeometryNode::netaPrint");
}

// Clone node structure
NetaNode* NetaGeometryNode::clone(Neta* newparent)
{
	Messenger::enter("NetaGeometryNode::clone");
	NetaGeometryNode* node = new NetaGeometryNode(geometry_);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	Messenger::exit("NetaGeometryNode::clone");
	return node;
}

/*
 * NetaValueNode
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

//Destructor
NetaValueNode::~NetaValueNode()
{
}

// Validation function (virtual)
int NetaValueNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaValueNode::score");
	int totalscore = -1, n;
	Bond* b;
	RefListItem<Bond,int>* rb;
	switch (netaValue_)
	{
		case (Neta::BondValue):
			if (path.last() == NULL) Messenger::print("NETA: Invalid context for 'bond=xxx' in NETA for type %s/%i.", qPrintable(parent()->parentForcefieldAtom()->name()), parent()->parentForcefieldAtom()->typeId());
			else
			{
				b = target->findBond(path.last()->item);
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
				default:
					break;
			}
			break;
		default:
			Messenger::print("Internal NETA Error: Unrecognised value in NetaValueNode::score.");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Value (%s %s %i) = %i", Neta::netaValue(netaValue_), Neta::netaValueComparison(netaComparison_), value_, totalscore);
	Messenger::exit("NetaValueNode::score");
	return totalscore;
}

// Print node contents
void NetaValueNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Value Node: %s %s %i)\n", qPrintable(tab), Neta::netaValue(netaValue_), Neta::netaValueComparison(netaComparison_), value_);
}

// Print (append) NETA representation of node contents
void NetaValueNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaValueNode::netaPrint");
	neta += Neta::netaValue(netaValue_);
	neta += Neta::netaValueComparison(netaComparison_);
	neta += QString::number(value_);
	Messenger::exit("NetaValueNode::netaPrint");
}

// Clone node structure
NetaNode* NetaValueNode::clone(Neta* newparent)
{
	Messenger::enter("NetaValueNode::clone");
	NetaValueNode* node = new NetaValueNode(netaValue_, netaComparison_, value_);
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	Messenger::exit("NetaValueNode::clone");
	return node;
}

/*
 * NetaRootNode
 */

// Constructor
NetaRootNode::NetaRootNode()
{
	// Private variables
	nodeType_ = NetaNode::RootNode;
}

//Destructor
NetaRootNode::~NetaRootNode()
{
}

// Validation function (virtual)
int NetaRootNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	return (innerNeta_ != NULL ? innerNeta_->score(target, nbrs, rings, this, path, level) : 0);
}

// Print node contents
void NetaRootNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Root Node:)\n", qPrintable(tab));
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, prefix);
}

// Print (append) NETA representation of node contents
void NetaRootNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaRootNode::netaPrint");
	if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
	Messenger::exit("NetaRootNode::netaPrint");
}

// Clone node structure
NetaNode* NetaRootNode::clone(Neta* newparent)
{
	Messenger::enter("NetaRootNode::clone");
	NetaRootNode* node = new NetaRootNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	newparent->ownNode(node);
	if (innerNeta_ != NULL) node->innerNeta_ = innerNeta_->clone(newparent);
	Messenger::exit("NetaRootNode::clone");
	return node;
}

/*
 * NetaRingNode
 */

// Constructor
NetaRingNode::NetaRingNode()
{
	// Private variables
	nodeType_ = NetaNode::RingNode;
}

//Destructor
NetaRingNode::~NetaRingNode()
{
}

// Retrieve current ring under consideration
Ring *NetaRingNode::currentRing()
{
	return currentRing_;
}

// Validation function (virtual)
int NetaRingNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaRingNode::score");
	int totalscore = -1, n;
	RefListItem<Ring,int>* ri;
	RefList<Atom,int> atomCheckList;
	RefList< RefListItem<Ring,int>, int > scores;
	RefListItem< RefListItem<Ring,int>, int > *si;
	// Pointer check
	if (rings == NULL)
	{
		Messenger::print("NETA Internal Error: Called NetaRingNode::score() without a valid ring list.");
		Messenger::exit("NetaRingNode::score");
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
				if (innerNeta_ == NULL)
				{
					// By virtue of having no inner NETA, *any* ring will match...
					si->data = 1;
					continue;
				}
				// Add atoms in this ring to an atomCheckList
				ri->item->addAtomsToRefList(&atomCheckList,NULL);
				// Get a match score for the innerNeta
				si->data = innerNeta_->score(target, &atomCheckList, NULL, this, path, level+1);
			}
			// Calculate how many rings we matched, and if this satisfies any repeat condition
			n = 0;
			for (si = scores.first(); si != NULL; si = si->next) if (si->data > 0) ++n;
			if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
			{
				n = repeat_ == -1 ? 1 : repeat_;
				ri = rings->first();
				totalscore = 0;
				for (si = scores.first(); si != NULL; si = si->next)
				{
					if (si->data > 0)
					{
						totalscore += si->data;
						rings->remove(si->item);
						--n;
					}
					if (n == 0) break;
				}
			}
			else totalscore = -1;
			if (totalscore == 0) totalscore = -1;
			break;
		case (NetaNode::RingNode):
			Messenger::print("NETA Error: Specifying a 'ring' directly inside another 'ring' is meaningless.");
			totalscore = -1;
			break;
		default:
			printf("NetaRingNode::score : Unrecognised context.\n");
			break;
	}
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Ring Check (%i required) = %i", repeat_ == -1 ? 1 : repeat_, totalscore);
	Messenger::exit("NetaRingNode::score");
	return totalscore;
}

// Print node contents
void NetaRingNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Ring Node:)\n", qPrintable(tab));
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", qPrintable(tab));
}

// Print (append) NETA representation of node contents
void NetaRingNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaRingNode::netaPrint");
	neta += "ring";
	if (innerNeta_ != NULL)
	{
		neta += '(';
		innerNeta_->netaPrint(neta);
		neta += ')';
	}
	Messenger::exit("NetaRingNode::netaPrint");
}

// Clone node structure
NetaNode* NetaRingNode::clone(Neta* newparent)
{
	Messenger::enter("NetaRingNode::clone");
	NetaRingNode* node = new NetaRingNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	newparent->ownNode(node);
	Messenger::exit("NetaRingNode::clone");
	return node;
}

/*
 * NetaChainNode
 */

// Constructor
NetaChainNode::NetaChainNode()
{
	// Private variables
	nodeType_ = NetaNode::ChainNode;
}

//Destructor
NetaChainNode::~NetaChainNode()
{
}

// Private (recursive) scoring function
int NetaChainNode::score(NetaNode* currentNode, int nRepeat, Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaChainNode::score(private)");
	int totalscore = -1, atomscore = -1;
	Atom* i, *j;
	// The target atom we're passed is the last atom in the current chain (or the originating atomic centre).
	// The neighbours list should contain a single bound atom reference that we are interested in checking at this point in the chain.
	// So, determine if the current chain node matches the target/nbrs combination.
	// Sanity check first...
	if (nbrs->nItems() != 1) printf("Internal NETA Error: Atom target in ChainNode not passed with exactly 1 neighbour (nNbrs = %i)\n", nbrs->nItems());
	i = nbrs->first()->item;
	totalscore = currentNode->score(target, nbrs, rings, this, path, level);
	if (totalscore != -1)
	{
		// This node matched, so proceed to branch into all neighbours on the atom contained in 'nbrs'
		// Unless, of course, this is the last node in the list, in which case exit - we're happy.
		if ((currentNode->nextNode == NULL) && (nRepeat == 1))
		{
// 			printf("Finished constructing chain succesfully.\n");
			Messenger::exit("NetaChainNode::score(private)");
			return totalscore;
		}
		currentChain_.add(i);
// 		printf("Added atom id %i to chain\n", currentChain_.last() == NULL ? -999 : currentChain_.last()->item->id());
// 		printf("CHAIN testing atom %p %i\n", target, target->id());
// 		int count = 0; for (RefListItem<Atom,int>* ri = path.first(); ri != NULL; ri = ri->next) printf(" -- path %i : %p %i\n", ++count, ri->item, ri->item->id());
		// Cycle over new bound list for matches to the next node, exiting immediately if we get a positive match
		for (RefListItem<Bond,int>* b = i->bonds(); b != NULL; b = b->next)
		{
			// Get bound partner
			j = b->item->partner(i);
			// Skip if this atom already exists in the chain
			if (currentChain_.contains(j) != NULL) continue;
			// Construct new, single-item bound reflist
			RefList<Atom,int> boundList;
			boundList.add(j);

			// Score it...
			if (nRepeat > 1)
			{
				path.add(target);
				atomscore = score(currentNode, nRepeat-1, i, &boundList, rings, path, level);
				path.removeLast();
			}
			else
			{
				// Cast the next node pointer up to a BoundNode and get its repeat value
				NetaContextNode* ncn = (NetaContextNode*) (currentNode->nextNode);
// 				printf("ncn = %p, repeat is %i, last atom id is %i\n", ncn, ncn->repeat(), currentChain_.last() == NULL ? -999 : currentChain_.last()->item->id());
				path.add(target);
				atomscore = score(ncn, ncn->repeat() == -1 ? 1 : ncn->repeat(), i, &boundList, rings, path, level);
				path.removeLast();
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
	Messenger::exit("NetaChainNode::score(private)");
	return totalscore;
}

// Validation function (virtual)
int NetaChainNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaChainNode::score");
	int totalscore = -1, n;
	RefListItem<Atom,int>* ri;
	RefList< RefListItem<Atom,int>, int > scores;
	RefListItem< RefListItem<Atom,int>, int > *si;
	RefList<Atom,int> boundList;
	// Start off the private recursive method with a call centred on the first of the bound neigbours
	currentChain_.clear();
	currentChain_.add(target);
	for (ri = nbrs->first(); ri != NULL; ri = ri->next)
	{
		si = scores.add(ri, 0);
		boundList.clear();
		boundList.add(ri->item);
		if (linearNeta_ == NULL) { si->data = 0; Messenger::print("Warning: No chain description specified in 'chain' command."); }
		else si->data = score(linearNeta_, 1, target, &boundList, rings, path, level);
	}

	// How many matches?
	n = 0;
	for (si = scores.first(); si != NULL; si = si->next) if (si->data > 0) ++n;
	if (n == 0) totalscore = -1;
	else if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
	{
		n = repeat_ == -1 ? 1 : repeat_;
		ri = nbrs->first();
		totalscore = 0;
		for (si = scores.first(); si != NULL; si = si->next)
		{
			if (si->data > 0)
			{
				totalscore += si->data;
				nbrs->remove(si->item);
				--n;
			}
			if (n == 0) break;
		}
	}
	else totalscore = -1;
	// Check for reverse logic
	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Chain Check (%i required) = %i", repeat_ == -1 ? 1 : repeat_, totalscore);
	Messenger::exit("NetaChainNode::score");
	return totalscore;
}

// Print node contents
void NetaChainNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Chain Node:)\n", qPrintable(tab));
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", qPrintable(tab));
}

// Print (append) NETA representation of node contents
void NetaChainNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaChainNode::netaPrint");
	neta += "chain";
	if (innerNeta_ != NULL)
	{
		neta += '(';
		if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
		neta += ')';
	}
	Messenger::exit("NetaChainNode::netaPrint");
}

// Clone node structure
NetaNode* NetaChainNode::clone(Neta* newparent)
{
	Messenger::enter("NetaChainNode::clone");
	NetaChainNode* node = new NetaChainNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	newparent->ownNode(node);
	Messenger::exit("NetaChainNode::clone");
	return node;
}

/*
 * NetaMeasurementNode
 */

// Constructor
NetaMeasurementNode::NetaMeasurementNode()
{
	// Private variables
	nodeType_ = NetaNode::MeasurementNode;
	requiredValue_ = 0.0;
	tolerance_ = 1.0;
	removeNeighbours_ = false;
}

//Destructor
NetaMeasurementNode::~NetaMeasurementNode()
{
}

// Set required value
void NetaMeasurementNode::setRequiredValue(double value, double tolerance)
{
	requiredValue_ = value;
	tolerance_ = tolerance;
}

// Set whether a match should remove atoms from allowable paths for other nodes
void NetaMeasurementNode::setRemoveNeighbours(bool b)
{
	removeNeighbours_ = b;
}

// Private (recursive) scoring function
int NetaMeasurementNode::score(NetaNode* currentNode, int nRepeat, Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaMeasurementNode::score(private)");
	int totalscore = -1, atomscore = -1;
	double measurement;
	Atom* i, *j;
	// The target atom we're passed is the last atom in the current chain (or the originating atomic centre).
	// The neighbours list should contain a single bound atom reference that we are interested in checking at this point in the chain.
	// So, determine if the current chain node matches the target/nbrs combination.
	// Sanity check first...
	if (nbrs->nItems() != 1) printf("Internal NETA Error: Atom target in ChainNode not passed with exactly 1 neighbour (nNbrs = %i)\n", nbrs->nItems());
	i = nbrs->first()->item;
	totalscore = currentNode->score(target, nbrs, rings, this, path, level);
	if (totalscore != -1)
	{
		currentChain_.add(i);
// 		printf("Added atom id %i to chain\n", currentChain_.last() == NULL ? -999 : currentChain_.last()->item->id());
		// This node matched, so proceed to branch into all neighbours on the atom contained in 'nbrs'
		// Unless, of course, this is the last node in the list, in which case have found the required chain.
		// Then, we must check the geometry...
		if ((currentNode->nextNode == NULL) && (nRepeat == 1))
		{
// 			printf("Finished constructing measurement chain successfully.\n");
// 			int count = 0; for (RefListItem<Atom,int>* ri = currentChain_.first(); ri != NULL; ri = ri->next) printf(" -- measchain %i : %p %i\n", ++count, ri->item, ri->item->id());

			// The number of atoms in the chain will determine what we actually calculate here....
			if (currentChain_.nItems() > 4) return -1;
			if (!target->parent()) return -1;
			Atom* atoms[4];
			double delta;
			for (int n=0; n<currentChain_.nItems(); ++n) atoms[n] = currentChain_[n]->item;
			if (currentChain_.nItems() == 2)
			{
				measurement = target->parent()->distance(atoms[0], atoms[1]);
				delta = fabs(measurement-requiredValue_);
			}
			else if (currentChain_.nItems() == 3)
			{
				measurement = target->parent()->angle(atoms[0], atoms[1], atoms[2]);
				delta = fabs(measurement-requiredValue_);
			}
			else
			{
				// For torsions, need to account for circular nature
				measurement = target->parent()->torsion(atoms[0], atoms[1], atoms[2], atoms[3]);
				delta = fabs(measurement-requiredValue_);
				if (delta > 180.0) delta = 360.0 - delta;
			}
// 			printf("Measurement is %f, required is %f\n", measurement, requiredValue_);
			bool result = (delta < tolerance_);
			if (reverseLogic_) result = !result;

			// If it is not a match based on the required value, remove this atom from the chain and return -1
			if (!result) currentChain_.removeLast();
// 			printf("Result = %i\n", result);
			
			Messenger::exit("NetaMeasurementNode::score(private)");
			return (result ? totalscore : -1);
		}
// 		printf("CHAIN testing atom %p %i\n", target, target->id());
// 		int count = 0; for (RefListItem<Atom,int>* ri = path.first(); ri != NULL; ri = ri->next) printf(" -- path %i : %p %i\n", ++count, ri->item, ri->item->id());
		// Cycle over new bound list for matches to the next node, exiting immediately if we get a positive match
		for (RefListItem<Bond,int>* b = i->bonds(); b != NULL; b = b->next)
		{
			// Get bound partner
			j = b->item->partner(i);
			// Skip if this atom already exists in the chain
			if (currentChain_.contains(j) != NULL) continue;
			// Construct new, single-item bound reflist
			RefList<Atom,int> boundList;
			boundList.add(j);

			// Score it...
			if (nRepeat > 1)
			{
				path.add(target);
				atomscore = score(currentNode, nRepeat-1, i, &boundList, rings, path, level);
				path.removeLast();
			}
			else
			{
				// Cast the next node pointer up to a BoundNode and get its repeat value
				NetaContextNode* ncn = (NetaContextNode*) (currentNode->nextNode);
// 				printf("ncn = %p, repeat is %i, last atom id is %i\n", ncn, ncn->repeat(), currentChain_.last() == NULL ? -999 : currentChain_.last()->item->id());
				path.add(target);
				atomscore = score(ncn, ncn->repeat() == -1 ? 1 : ncn->repeat(), i, &boundList, rings, path, level);
				path.removeLast();
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
	Messenger::exit("NetaMeasurementNode::score(private)");
	return totalscore;
}

// Validation function (virtual)
int NetaMeasurementNode::score(Atom* target, RefList<Atom,int>* nbrs, RefList<Ring,int>* rings, NetaContextNode* context, RefList<Atom,int>& path, int level)
{
	Messenger::enter("NetaMeasurementNode::score");
	int totalscore = -1, n;
	RefList<Atom,int> scores;
	RefListItem<Atom,int>* si;
	RefList<Atom,int> boundList;
	Atom* j;

	// Check for a linearNeta_ definition
	if (linearNeta_ == NULL)
	{
		Messenger::print("Internal Error: No chain description specified in NetaMeasurementNode.");
		Messenger::exit("NetaMeasurementNode::score");
		return -1;
	}

	// Clear the local chain list, and add our target atom as the first item
	currentChain_.clear();
	currentChain_.add(target);

	// Depending on whether we are modifying the neighbour list (if removeNeighbours_ == true) or not, we either use the nbrs list here, or the list of bound atoms
	if (removeNeighbours_) for (RefListItem<Atom,int>* ri = nbrs->first(); ri != NULL; ri = ri->next)
	{
		boundList.clear();
		boundList.add(ri->item);
		scores.add(ri->item, score(linearNeta_, 1, target, &boundList, rings, path, level));
	}
	else for (RefListItem<Bond,int>* rb = target->bonds(); rb != NULL; rb = rb->next)
	{
		j = rb->item->partner(target);
		boundList.clear();
		boundList.add(j);
		scores.add(j, score(linearNeta_, 1, target, &boundList, rings, path, level));
	}

	// How many matches? Note that we do not modify the nbrs list here if removeNeighbours_ == false
	n = 0;
	for (si = scores.first(); si != NULL; si = si->next) if (si->data > 0) ++n;
	if (n == 0) totalscore = -1;
	else if ((repeat_ == -1) || (Neta::netaValueCompare(n, repeatComparison_, repeat_)))
	{
		n = repeat_ == -1 ? 1 : repeat_;
		totalscore = 0;
		for (si = scores.first(); si != NULL; si = si->next)
		{
			if (si->data > 0)
			{
				totalscore += si->data;
				if (removeNeighbours_) nbrs->remove(si->item);
				--n;
			}
			if (n == 0) break;
		}
	}
	else totalscore = -1;
	// Check for reverse logic
// 	if (reverseLogic_) totalscore = (totalscore == -1 ? 1 : -1);
	NetaNode::printScore(level, "Measurement Check (%i required) = %i", repeat_ == -1 ? 1 : repeat_, totalscore);
	Messenger::exit("NetaMeasurementNode::score");
	return totalscore;
}

// Print node contents
void NetaMeasurementNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	if (offset == 1) tab += '\t';
	tab += prefix;
	// Output node data
	printf("%s (Measurement Node:)\n", qPrintable(tab));
	if (innerNeta_ != NULL) innerNeta_->nodePrint(offset+1, "");
	else printf("%s   -> No Inner Description\n", qPrintable(tab));
}

// Print (append) NETA representation of node contents
void NetaMeasurementNode::netaPrint(QString& neta)
{
	Messenger::enter("NetaMeasurementNode::netaPrint");
	neta += "m";
	if (innerNeta_ != NULL)
	{
		neta += "(" + QString::number(requiredValue_) + "," + QString::number(tolerance_) + ",";
		if (innerNeta_ != NULL) innerNeta_->netaPrint(neta);
		neta += ')';
	}
	Messenger::exit("NetaMeasurementNode::netaPrint");
}

// Clone node structure
NetaNode* NetaMeasurementNode::clone(Neta* newparent)
{
	Messenger::enter("NetaMeasurementNode::clone");
	NetaMeasurementNode* node = new NetaMeasurementNode();
	node->setParent(newparent);
	node->reverseLogic_ = reverseLogic_;
	node->nodeType_ = nodeType_;
	node->repeat_ = repeat_;
	node->innerNeta_ = innerNeta_ == NULL ? NULL : innerNeta_->clone(newparent);
	// TODO Clone linearNeta_
// 	node->linearNeta_ = linearNeta_ == NULL ? NULL : linearNeta_->clone(newparent);
	node->requiredValue_ = requiredValue_;	
	node->tolerance_ = tolerance_;
	newparent->ownNode(node);
	Messenger::exit("NetaMeasurementNode::clone");
	return node;
}

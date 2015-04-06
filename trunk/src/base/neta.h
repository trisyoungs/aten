/*
	*** NETA Description
	*** src/base/neta.h
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

#ifndef ATEN_NETA_H
#define ATEN_NETA_H

#include "base/atom.h"
#include "base/bond.h"
#include "base/ring.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Neta;
class ForcefieldAtom;
class Atom;
class Model;
class Ring;
class Forcefield;

// NEW
class Element;
class NetaNode;
class NetaContextNode;
class NetaRootNode;

// NETA description
class Neta : public ListItem<Neta>
{
	public:
	// Constructor / Destructor
	Neta();
	~Neta();
	// NETA Keywords
	enum NetaKeyword { AliphaticKeyword, AromaticKeyword, NoRingKeyword, NonAromaticKeyword, NotPrevKeyword, NotSelfKeyword, PlanarKeyword, nNetaKeywords };
	static NetaKeyword netaKeyword(QString s, bool reportError = false);
	static const char* netaKeyword(NetaKeyword nk);
	// NETA expanders
	enum NetaExpander { BoundExpanded, ChainExpander, DoublyBoundExpander, GeometryExpander, PathExpander, RingExpander, nNetaExpanders };
	static NetaExpander netaExpander(QString s, bool reportError = false);
	// NETA values
	enum NetaValue { BondValue, NBondsValue, NHydrogensValue, OxidationStateValue, RepeatValue, SizeValue, nNetaValues };
	static NetaValue netaValue(QString s, bool reportError = false);
	static const char* netaValue(NetaValue nv);
	// NETA Value comparison operators
	enum NetaValueComparison { EqualTo, NotEqualTo, GreaterThan, LessThan, GreaterThanEqualTo, LessThanEqualTo, nNetaValueComparisons };
	static const char* netaValueComparison(NetaValueComparison nvc);
	static bool netaValueCompare(int lhsvalue, NetaValueComparison nvc, int rhsvalue);
	// Node logic types
	enum NetaLogicType { NetaAndLogic, NetaOrLogic, NetaAndNotLogic, nNetaLogicTypes };
	static const char* netaLogic(NetaLogicType lt);
	// Friend Class
	friend class NetaParser;


	/*
	 * Definition
	 */
	private:
	// Parent forcefield
	Forcefield* parentForcefield_;
	// Parent forcefield atom
	ForcefieldAtom* parentForcefieldAtom_;
	// Character element (i.e. the element that the matching atom must be)
	int characterElement_;
	// Owned node list
	List<NetaNode> ownedNodes_;
	// Top of NETA nodelist describing the type
	NetaRootNode* description_;
	// Reference name (if a define)
	QString name_;

	public:
	// Set parent forcefield
	void setParentForcefield(Forcefield* ff);
	// Return parent forcefield
	Forcefield* parentForcefield();
	// Set parent forcefield atom
	void setParentForcefieldAtom(ForcefieldAtom* ff);
	// Return parent forcefield atom
	ForcefieldAtom* parentForcefieldAtom();
	// Set character element
	void setCharacterElement(int el);
	// Return character element
	int characterElement() const;
	// Take ownership of selected node
	void ownNode(NetaNode* node);
	// Return reference name (if a define)
	QString name() const;
	// Set reference name (if a define)
	void setName(QString name);
	// Return top of description nodelist
	NetaRootNode* description();
	// Print
	void print() const;
	// Print (append) NETA string to target string
	void netaPrint(QString& neta) const;


	/*
	 * Methods
	 */
	private:
	// Current atom being checked...
	Atom* targetAtom_;
	// Supplied list of rings for the atom matchAtom() is targetting
	List<Ring> *targetRingList_;
	// Parent model of target atom
	Model* targetParent_;

	public:
	// Clone nodes (and own them) beginning from the node supplied
	NetaNode* clone(NetaNode* topnode);
	// Clear all associated node data (but leave character element as-is)
	void clear();
	// Return current atom target
	Atom* targetAtom();
	// Return ringList of supplied atom
	List<Ring> *targetRingList();
	// Return target atom's parent model
	Model* targetParent();
	// Check supplied atom to see if it matches this NETA description
	int matchAtom(Atom* i, List<Ring>* rings, Model* parent);
	// Link forcefield type references in elementtype lists
	void linkReferenceTypes();
	// Create a basic description for the specified Atom
	bool createBasic(Atom* i, bool explicitBondType = false, double torsionTolerance = -1.0);
};

// NETA Specification Node
class NetaNode : public ListItem<NetaNode>
{
	public:
	// Constructor / Destructor
	NetaNode();
	virtual ~NetaNode();
	// Linear node pointers
	NetaNode* prevNode, *nextNode;
	// Neta node types
	enum NetaNodeType { BoundNode, ChainNode, ElementNode, GeometryNode, KeywordNode, LogicNode, MeasurementNode, RingNode, RootNode, ValueNode, nNetaNodeTypes };

	protected:
	// Node type
	NetaNodeType nodeType_;
	// Whether to use reverse logic when returning the final value
	bool reverseLogic_;

	private:
	// Parent NETA structure
	Neta* parent_;

	public:
	// Return node type
	NetaNodeType nodeType();
	// Return whether to use reverse logic when returning the final value
	bool reverseLogic() const;
	// Set node to use reverse logic
	void setReverseLogic();
	// Return parent NETA structure
	Neta* parent();
	// Set parent NETA structure
	void setParent(Neta* neta);
	// Validation function
	virtual int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level)=0;
	// Print node contents
	virtual void nodePrint(int offset, const char* prefix)=0;
	// Print (append) NETA representation of node contents
	virtual void netaPrint(QString& neta)=0;
	// Clone node structure
	virtual NetaNode* clone(Neta* newparent)=0;
	// Print contextual score
	static void printScore(int level, const char* fmt, ...);
};

// NETA context node
class NetaContextNode : public NetaNode
{
	public:
	// Constructor / Destructor
	NetaContextNode();
	~NetaContextNode();

	protected:
	// Repetition specifier
	int repeat_;
	// Repetition logic
	Neta::NetaValueComparison repeatComparison_;
	// Inner NETA description
	NetaNode* innerNeta_;
	// Inner linear NETA description (used by ChainNode)
	NetaNode* linearNeta_;

	public:
	// Set repetition specifier
	void setRepeat(int n);
	// Return repetition specified
	int repeat() const;
	// Set value comparison
	void setRepeatComparison(Neta::NetaValueComparison nvc);
	// Set inner neta description
	void setInnerNeta(NetaNode* innerneta, NetaNode* linearneta = NULL);
	// Return inner neta description
	NetaNode* innerNeta();
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// NETA logic node
class NetaLogicNode : public NetaNode
{
	public:
	// Constructor / Destructor
	NetaLogicNode(Neta::NetaLogicType nt, NetaNode* arg1, NetaNode* arg2);
	~NetaLogicNode();
	
	private:
	// Logic type
	Neta::NetaLogicType netaLogic_;
	// Specification(s)
	NetaNode* argument1_, *argument2_;

	public:
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// NETA Bound atom node
class NetaBoundNode : public NetaContextNode
{
	public:
	// Constructor / Destructor
	NetaBoundNode();
	~NetaBoundNode();

	private:
	// List of elements/types that the current context atom may be
	Reflist<ForcefieldAtom,int> allowedElementsAndTypes_;
	// Type of required connection
	Bond::BondType bondType_;
	// Validation function to check supplied atom against allowed elements and types
	int atomScore(Atom* target);
	// Create formatted element/type list
	QString elementsAndTypesString();

	public:
	// Set node data
	void set(Refitem<ForcefieldAtom,int>* elemtypes, NetaNode* innerneta, Bond::BondType bondtype);
	// Link forcefield type references in elementtype lists
	void linkReferenceTypes();
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// NETA keyword node
class NetaKeywordNode : public NetaNode
{
	public:
	// Constructor / Destructor
	NetaKeywordNode(Neta::NetaKeyword nv);
	~NetaKeywordNode();
	
	private:
	// Keyword
	Neta::NetaKeyword netaKeyword_;

	public:
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// NETA geometry node
class NetaGeometryNode : public NetaNode
{
	public:
	// Constructor / Destructor
	NetaGeometryNode(Atom::AtomGeometry ag);
	~NetaGeometryNode();
	
	private:
	// Keyword
	Atom::AtomGeometry geometry_;

	public:
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// NETA Value comparison node
class NetaValueNode : public NetaNode
{
	public:
	// Constructor / Destructor
	NetaValueNode(Neta::NetaValue nv, Neta::NetaValueComparison nvc, int value);
	~NetaValueNode();
	
	private:
	// Value to check
	Neta::NetaValue netaValue_;
	// Comparison operator to use
	Neta::NetaValueComparison netaComparison_;
	// Integer value to compare against
	int value_;

	public:
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// Root type
class NetaRootNode : public NetaContextNode
{
	public:
	// Constructor / Destructor
	NetaRootNode();
	~NetaRootNode();
	
	public:
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// Ring type
class NetaRingNode : public NetaContextNode
{
	public:
	// Constructor / Destructor
	NetaRingNode();
	~NetaRingNode();

	private:
	// Current ring under consideration
	Ring *currentRing_;

	public:
	// Retrieve current ring under consideration
	Ring *currentRing();
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// Chain type
class NetaChainNode : public NetaContextNode
{
	public:
	// Constructor / Destructor
	NetaChainNode();
	~NetaChainNode();

	private:
	// Current chain of matched atoms
	Reflist<Atom,int> currentChain_;
	// Private (recursive) scoring function
	int score(NetaNode* currentNode, int nrepeat, Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, Reflist<Atom,int>& path, int level);

	public:
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

// Measurement type
class NetaMeasurementNode : public NetaContextNode
{
	public:
	// Constructor / Destructor
	NetaMeasurementNode();
	~NetaMeasurementNode();

	private:
	// Current chain of matched atoms
	Reflist<Atom,int> currentChain_;
	// Required measurement value
	double requiredValue_;
	// Whether a match should remove atoms from allowable paths for other nodes
	bool removeNeighbours_;
	// Allowed tolerance between measured / required value
	double tolerance_;
	// Private (recursive) scoring function
	int score(NetaNode* currentNode, int nrepeat, Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, Reflist<Atom,int>& path, int level);

	public:
	// Set required value
	void setRequiredValue(double value, double tolerance);
	// Set whether a match should remove atoms from allowable paths for other nodes
	void setRemoveNeighbours(bool b);
	// Validation function (virtual)
	int score(Atom* target, Reflist<Atom,int>* nbrs, Reflist<Ring,int>* rings, NetaContextNode* context, Reflist<Atom,int>& path, int level);
	// Print node contents
	void nodePrint(int offset, const char* prefix);
	// Print (append) NETA representation of node contents
	void netaPrint(QString& neta);
	// Clone node structure
	NetaNode* clone(Neta* newparent);
};

ATEN_END_NAMESPACE

#endif

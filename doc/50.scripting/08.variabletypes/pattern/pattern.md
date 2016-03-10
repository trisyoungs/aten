---
title: Pattern
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Pattern**](/aten/docs/scripting/variabletypes/pattern) type describes a single continuous collection of similar molecules/fragments within a model.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| angles | [**Bound**](/aten/docs/scripting/variabletypes/bound)[]  | | Array of angle interactions in one molecule of the pattern |
| atoms | [**Atom**](/aten/docs/scripting/variabletypes/atom)[] | | Array of atoms spanned by the pattern |
| bonds | [**Bound**](/aten/docs/scripting/variabletypes/bound)[] | | Array of bond interactions in one molecule of the pattern |
| ff | [**Forcefield**](/aten/docs/scripting/variabletypes/forcefield) | • | Reference to the forcefield associated to the pattern (if any) |
| ffAngles | [**FFBound**](/aten/docs/scripting/variabletypes/ffbound)[] | | List of unique forcefield angle terms in the pattern |
| ffBonds | [**FFBound**](/aten/docs/scripting/variabletypes/ffbound)[] | | List of unique forcefield bond terms in the pattern |
| ffTorsions | [**FFBound**](/aten/docs/scripting/variabletypes/ffbound)[] | | List of unique forcefield torsion terms in the pattern |
| ffTypes | [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) | | Array of unique atom types used in the pattern |
| firstAtom | [**Atom**](/aten/docs/scripting/variabletypes/atom)[] | | Reference to the first atom spanned by the pattern |
| firstAtomId | **int** | | Atom ID of the first atom spanned by the pattern |
| fixed | **int** | • | Whether the coordinates of all atoms in the pattern are fixed in minimisation routines |
| lastAtom | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | Reference to the last atom spanned by the pattern |
| lastAtomId | **int** | | Atom ID of the last atom spanned by the pattern |
| name | **string** | • | Name of the pattern |
| nAngles | **int** | | Number of angles in one molecule of the pattern |
| nAtoms | **int** | | Total number of atoms spanned by the pattern |
| nBonds | **int** | | Number of bonds in one molecule of the pattern |
| nFFAngles | **int** | | Number of unique angle terms used in the pattern |
| nFFBonds | **int** | | Number of unique bond terms used in the pattern |
| nFFTorsions | **int** | | Number of unique torsion terms used in the pattern |
| nFFTypes | **int** | | Number of unique atom types used in the pattern |
| nMolAtoms | **int** | | Number of atoms in one molecule of the pattern |
| nMols | **int** | | Number of molecules (repeat units) in the pattern |
| nTorsions | **int** | | Number of torsion interactions in one molecule of the pattern |
| torsions | [**Bound**](/aten/docs/scripting/variabletypes/bound)[] | | Array of torsion interactions in one molecule of the pattern |

## Pattern Type Functions

### atomsInRing <a id="atomsinring"></a>

_Syntax:_

**int** **atomsInRing** ( **int** _id_i_, **int** _id_j_ = -1 )

Return whether the supplied atom index (indices), given in local pattern atom numbering, is in a ring (the same ring)

---

### cog <a id="cog"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **cog** ( **int** _index_ )

Return calculated centre of geometry for the molecule index provided

---

### com <a id="com"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **com** ( **int** _index_ )

Return calculated centre of mass for the molecule index provided



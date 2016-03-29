---
title: The NETA Algorithm
brief: Description of the NETA algorithm
visible: true
template: manpage
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

## Overview

NETA stands for the Nested English Typing Algorithm – a dull yet descriptive acronym, all said and done, but with the advantage that it is "**Aten**" backwards. NETA is an attempt to provide a descriptive atom typing language that is:

+ Easily readable
+ Easily written from a small subset of keywords
+ Recursive and able to describe complex molecules

It's closest relative that I'm aware of in the literature is the ATDL as implemented in Vega-ZZ,[1] but was (genuinely) conceived without prior knowledge of that system. NETA tries to keep the language simple enough that it can almost be read aloud an make sense, given one or two special syntactic tokens, rather than needlessly use numerical codes and spurious symbols to signify certain quantities or create an ultra-compact language. The former destroys readability and the latter promotes convolution, neither of which help when trying to interpret old rules or write new ones. So, for the most part NETA is keyword-based, with a limited number of fairly 'natural' symbols employed to denote common terms.

Typing begins from a provided set of atoms and bonds (i.e. the chemical graph). The connectivity between atoms must be 'set' prior to typing, either by automatic calculation of bonds based on distance criteria, manually adding them by hand, or reading them from the input model file. The typing algorithm itself makes no additions or changes to the connectivity of the input structure.

NETA requires a knowledge of species/molecule types in the model is required. For single molecule systems there is 1 distinct molecule (species) and 1 occurrence of it. For condensed phases, e.g. liquids, there are 1 or more species each with many copies of the molecule. In the interests of efficiency for the following routines, **Aten** attempts to generate a valid pattern description of the system if one is not present already. This essentially picks out the individual species and the number of molecules of each, and permits the typing routines to consider only one molecule of each species when determining atom types etc. The assumption here is that, since all molecules in a given species will have the same chemical graph, atom types can be worked out for a single molecule and then duplicated on all of the others.

Following detection of a suitable pattern description, several tasks are then performed:

### Cycle Detection

Firstly, any cyclic structures within a single molecule are detected up to a maximum (adjustable) ring size. This is achieved from a series of simple recursive searches beginning from each atom with more than one bond.  A sequence of 'walks' along bonds are made in order to form a path of some specified length (i.e. ring size). If the final atom in this path shares a bond with the starting atom, a cycle has been found. If not, the final atom is removed and replaced with another. If there are no more atoms to try in this final position, the preceeding atom in the path is removed and replaced with another, and so on. Each unique ring (its size and pointers to the sequence of constituent atoms) is stored in the pattern.

### Assignment of Atom Environment

From the list of bound neighbours, each atom is assigned a simple hybridicity based on the character of the bonds it is involved in, and is mainly used for the determination of aromatic cycles in the next step.

### Ring Types

Once atom hybridicities have been assigned, ring types can be determined. Rings are classed as either aliphatic, aromatic, or non-aromatic (i.e. a mix of resonant and aliphatic bonds that is not itself aromatic.

Now, working only with the representative molecule of each pattern, associated (or current) forcefield(s) are searched for types that match the contained atoms. Each NETA description whose character element is the same as a given atom is tested, and a score is obtained. If this score is non-zero and positive then the atomtype is a match and is assigned to the atom if it has no previous type, or if the score is higher than the previous one.  See atom type scoring in Section 12.5.3 for more information.

## References

[1] Pedretti, A.; Villa, L.; Vistoli, G. Theor. Chem. Acc. _109_, 229-232 (2003).


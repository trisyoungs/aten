---
title: Rule-based FFs
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Forcefields exist where individual intramolecular parameter definitions (i.e. those provided by `inter`  blocks) are not necessary. Instead, such parameters are constructed as and when necessary using a set of parameters that depend only on the atomtypes involved. These forcefields are so-called ‘rule-based’, and are often able to describe enormously varied systems from a small set of defining parameters.

Rule-based forcefields are defined in exactly the same way as normal forcefields, save for the lack of blocks that define intramolecular terms. Instead, the per-atomtype parameters must be provided instead, and for all atomtypes defined in the [type](/aten/docs/ff/keywords#type) section(s). This generator data is then used by the equations defined within the code to construct the necessary intramolecular terms when required. One or more  blocks should be used to define this data for each atomtype.

# Functions

In a rule-based forcefield all the useful function declarations which calculate the correct parameters (usually from values supplied in a  block) must be made within a single [function](/aten/docs/ff/keywords#function)] block in the forcefield file.  When calling the functions, **Aten** provides the necessary structure in which the generated parameters should be stores. In the case of the VDW-generating function, the actual atomtype structure which is missing the data is passed (see the [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) variable type). In the case of intramolecular interactions, **Aten** creates and passes a new, empty [**FFBound**](/aten/docs/scripting/variabletypes/ffbound) container in which the functional form of the interaction and the relevant data values should be set. A number of [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) references are also provided, corresponding to the atom types involved in the bound interaction, and from which the necessary data values may be retrieved using the relevant data accessors. For bound interactions it is not necessary to set the equivalent names of the involved atom types since this is done automatically.

The recognised function names and their arguments are as follows:

## angleGenerator

_Syntax:_

**int**  ( [**FFBound**](/aten/docs/scripting/variabletypes/ffbound) _newdata_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _j_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _k_ )

Called whenever function data for an unrecognised angle (between the atom types currently assigned to atoms _i_, _j_, and _k_) is needed. Generated parameters are placed in the passed _newdata_ structure

---

## bondgenerator

_Syntax:_

**int**  ( [**FFBound**](/aten/docs/scripting/variabletypes/ffbound) _newdata_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _j_ )

Called whenever function data for an unrecognised bond (between the atom types currently assigned to atoms _i_ and _j_) is needed. Generated parameters are placed in the passed _newdata_ structure

---

## torsiongenerator

_Syntax:_

**int**  ( [**FFBound**](/aten/docs/scripting/variabletypes/ffbound) _newdata_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _j_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _k_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _k_ )

Called whenever function data for an unrecognised torsion (between the atom types currently assigned to atoms _i_, _j_, _k_, and _l_) is needed. Generated parameters are placed in the passed _newdata_ structure

---

## vdwgenerator

_Syntax:_

**int**  ( [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) _data_ )

Called whenever descriptive VDW data is missing from an atom type (which is passed into the function and has the resulting calculated data placed in it).



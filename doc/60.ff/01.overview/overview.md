---
title: Overview
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

If you're doing anything interesting with a model or a molecule, a suitable forcefield description of the system is a must. A forcefield contains lists of parameters that describe the interactions between atoms, for example bonds, angles, and van der Waals interactions. More specifically, a forcefield contains parameters to describe many such interactions in many different types of molecule or chemical environment. An ‘expression’, referred to throughout the manual, should be thought of as the subset of terms from a given forcefield necessary to describe all the interactions within a model.

**Aten** has its own free format for forcefield files, described in the following sections. Once loaded in, the energy and forces in Models can then be calculated, and allows energy minimisation etc. More so, once a set of forcefield parameters has been read in and used to describe a model, this expression can be written out using a custom format ready for input into something else.


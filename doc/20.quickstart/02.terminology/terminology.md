---
title: Terminology
brief: Some terms frequently used in the manual
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

It is useful to know exactly what a few terms mean in **Aten**’s world:

## Model

A model is a single molecule, a snapshot of an ensemble of molecules in a liquid, a crystal’s unit cell – basically, any collection of atoms and bonds, optionally including a unit cell definition and/or a number of glyphs (annotations or shapes).

## Pattern

A system containing many molecules can be described (and manipulated) efficiently through the recognition that there are sets of molecules of the same type. A pattern describes one such set of similar molecules, and a model’s pattern definition may contain many individual patterns.  Many operations in **Aten** require that a pattern definition be present, and one is automatically created as and when necessary. See the topic on [Patterns](/aten/docs/topics/patterns) for more information.

## Filter

A filter is a set of commands (i.e. a program) which loads data in to or saves data from **Aten**. **Aten** depends on filters to be able to load and save models, forcefields, expressions, trajectories, and grid data. All filters are written in **Aten**’s own scripting language (which is based syntactically on C) and are loaded in on startup. New filters can be added at will (by the user) to cater for specific formats. See Section 11 for more information.

## Expression

While a forcefield is a collection of terms (bonds, angles, van der Waals terms, etc.) which describe (usually) a large number of molecular types and systems, an expression is a subset of terms specific to one model.

## NETA

**Aten** is able to automatically assign forcefield atom types to atoms in a model through the use of type descriptions in the [Nested English Typing of Atoms](/aten/docs/ff/neta) (NETA) language. This is a simple, readable system for describing the connectivity and environment of individual atoms.

## Fragment

A fragment is a molecule or structure which can be used to quickly build up a new model, or modify an existing one. For example, cyclohexane, or a tertiary butyl group.


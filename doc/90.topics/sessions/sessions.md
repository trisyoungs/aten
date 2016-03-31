---
title: Sessions
brief: Saving and restoring working sessions
visible: true
template: manpage
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

A 'session' in **Aten** is a state that includes all the current data loaded into the code - models, forcefields, gridded data etc. - but also the current view position and styling of those objects if relevant. Saving or loading a session from the [Session popup](/aten/docs/gui/home#file) allows these states to be stored and retrieved at will.

It is important to understand exactly what data is saved in **Aten**'s session files (files nominally with an `asf` suffix), since it is not trivial to save references to all loaded datafiles given the flexibility of the [Filters](/aten/docs/filters) system. Currently, an `asf` file contains the following information:

+ Atom and bond data from loaded models - the original model files are **not** reloaded explicitly. Instead, a copy of the atomic data is stored in the `asf` file. If the original data from the model file is desired, then it can simply be loaded in.
+ Grid file information - links to grid files **are** saved in the `asf` file, along with all visual details set in the GUI (colour, shift offset, cutoffs etc.)
+ View information - Model view rotations and zoom information
+ Preferences - Any non-default preferences setting, including element colouring

Note that session file handling is a new feature, and will improve over time.

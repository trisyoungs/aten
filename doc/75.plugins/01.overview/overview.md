---
title: Overview
brief: Overview of plugins - what they are, and what they do
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

As of v2.0. **Aten** utilises a plugin system to provide all of it's import / export capabilities, replacing the [filters](/aten/doc/filters) system used in previous versions. The intention behing both systems is very similar - while writing a plugin requires marginally more work than writing a filter, it is more efficient, better integrated with the rest of **Aten**, and benefits from the ability to use the full C++ language set (whereas filters were always restricted to the extent of the [command language](/aten/doc/scripting).

# File Plugins

File plugins implement the FilePluginInterface class, and provide import and/or export functionality for a particular type of file of a specified format. These file type correspond loosely to a Model, Grid, or Trajectory object in Aten, or a forcefield Expression, although there is no restriction imposed on what a particular file plugin can import (despite its designated type).

# Standard Options

There are a handful of options that are treated as 'common' to the import and export of all object types, and which are always passed to the plugin doing the reading / writing. Not all the standard options make sense across all of the object types, and which are accessible at the point of import / export depends on which are incorporated into Aten's file dialogs. Some of these options are utilised by the underlying FilePluginInterface (e.g. "keepNames") while others are the responsibility of the plugin to check and act upon (e.g. "preventRebonding").

| Option | Type | Relevance | Meaning |
|--------|------|-----------|---------|
| cacheAll | bool | Trajectory Import | If **true**, then all frames from the Trajectory file should be loaded immediately into memory, rather than read in as needed from disk. |
| coordinatesInBohr | bool | Model/Grid/Trajectory Import | Whether imported coordinates of Models or Grids are in Bohr rather than Angstrom. The conversion is made automatically by Aten once the plugin has finished loading data, and there is no need for the plugin to check / act on the value. |
| forceRhombohedral | bool | Model/Grid/Trajectory Import | Whether hexagonal spacegroups should be forced into rhombohedral settings (this setting is usually passed as an argument to the pack() command). |
| keepNames | bool | Model/Trajectory Import | If **true**, the original atom names read from files are stored in a new skeleton forcefield which is immediately associated to the Model, after which the standard atom name to element conversion is applied to the atoms. In this way, the original names of the atoms can be exported afterwards. This mapping is handled by the createAtom() function of the FilePluginInterface. |
| keepTypes | bool | Model/Grid/Trajectory Import | If **true**, imported atom names are looked up in all available forcefields, with types (and elements) being assigned if positive matches are found. This mapping is handled by the createAtom() function of the FilePluginInterface. |
| keepView | bool | Model Import | Normally, the view for each individual model is reset after import is complete. If this option is set to **true**, then this reset is prevented. This is handled by Aten, and there is no need for the plugin to check / act on the value. |
| preventFolding | bool | Model/Grid/Trajectory Import | If a plugin uses the fold() or foldMolecules() Model functions, it should check this value to see whether to proceed with the fold. |
| preventPacking | bool | Model/Grid/Trajectory Import | If a plugin uses the pack() Model functions, it should check this value to see whether to proceed with the pack. |
| preventRebonding | bool | Model/Grid/Trajectory Import | If a plugin uses the calculateBonding() Model functions, it should check this value to see whether to proceed with the rebonding. |
| zMappingType  | bool | Model/Grid/Trajectory Import | Affects the type of z-mapping used to convert atom names to element types. This value is used within the createAtom() function of FilePluginInterface. |



---
title: ZMapType
brief: "Mapping types for name / element conversion"
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---
 
| Value | Description |
|-------|-------------|
| **alpha** | Convert based on the alpha part of the name only. Leading or trailing numbers and symbols are discarded. The alpha part is assumed to be an element symbol |
| **auto** | Attempts several of the other conversions of increasing complexity until a match is found |
| **ff** | Search through the names of atomtypes in currently-loaded forcefields, assigning elements based on matches found |
| **firstalpha** | Convert based on the first alpha part of the name only (i.e. until a non-alpha character is found). The alpha part is assumed to be an element symbol |
| **name** | The name is assumed to be an actual element name, and is converted to the relevant element number |
| **numeric** | Use the numeric part of the name as the element number |
| **singlealpha** | Convert based on the first alphabetic character encountered - useful only when single-character element symbols are likely to be found (e.g. for pure organic CHON systems) |


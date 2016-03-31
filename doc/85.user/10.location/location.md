---
title: File Locations
brief: User data file locations searched by <strong>Aten</strong>
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

**Aten** will search for additional filters, fragments, and forcefields in a specific location in the user’s home directory. On Linux and Mac OS X systems this directory is called `.aten`, while on Windows the directory should be called simply `aten` (i.e. without the preceding dot). Within this directory exists (optionally) further directories named in exactly the same way as those in the [main `data` directory](/aten/docs/installation/data), in which user files of the relevant type should be located.

## Preferences

The two main preferences files are located in the user’s `.aten` (or `aten`) directory. Both are optional. The first, `prefs.dat` (or, alternatively, `prefs.txt`) is written by **Aten** from the Prefs window. While this file may be modified by hand, changes will be lost if overwritten by **Aten**. The second file, `user.dat` (or, alternatively, `user.txt`) is maintained entirely by the user, and should be used to change or set up exotic situations and preferences. For instance, a specific forcefield could be loaded automatically on startup.

## History

TODO

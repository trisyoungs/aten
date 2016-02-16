---
title: File Locations
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

## Installed / Provided Files

**Aten** depends on several sets of files in order to function properly and, generally-speaking, knows where to look for them. Sometimes, however, you may need to tell **Aten** where these files are (e.g. if you have not installed **Aten** after compiling the source yourself). There are several ways of achieving this. When running **Aten** from the command-line, the [--atendata](/aten/docs/cli/switches#switch_atendata) switch can be used to specify the location of **Aten**’s data files. For instance:

```
aten --atendata=/usr/software/aten/data
```

Alternatively, the environment variable [var]$ATENDATA[/var] can be set. For example, in a bash-style shell:

```
export ATENDATA=/usr/software/aten/data
```

In both cases, **Aten** should be directed to the [value]data[/value] directory; either it’s installed location or the directory in the top level of the source distribution.

The structure of the [value]data[/value] directory is as follows:

| Directory | Description |
|-----------|-------------|
| data/external | Settings and configuration for external programs used by **Aten** |
| data/ff | Forcefield files |
| data/fftesting | Forcefields that are incomplete or have not been tested |
| data/filters | Contains stock filters for import and exporting data |
| data/fragments | Fragment models for drawing / modifying models |
| data/includes | Function includes, accessible when scripting |
| data/partitions | Partition data for the disorder builder |
| data/scripts | Example scripts |
| data/test | Various test files (models, grids etc.) (not installed) |

## User Files

**Aten** will search for additional filters, fragments, and forcefields in a specific location in the user’s home directory. On Linux and Mac OS X systems this directory is called [value].aten[/value], while on Windows the directory should be called simply [value]aten[/value] (i.e. without the preceding dot). Within this directory exists (optionally) further directories named similarly to those in the [value]data[/value] directory, in which user files of the relevant type should be located.

Finally, the two main preferences files are located in the user’s [value].aten[/value] (or [value]aten[/value]) directory. Both are optional. The first, [value]prefs.dat[/value] (or, alternatively, [value]prefs.txt[/value]) is written by **Aten** from the Prefs window. While this file may be modified by hand, changes will be lost if overwritten by **Aten**. The second file, [value]user.dat[/value] (or, alternatively, [value]user.txt[/value]) is maintained entirely by the user, and should be used to change or set up exotic situations and preferences. For instance, a specific forcefield could be loaded automatically on startup.



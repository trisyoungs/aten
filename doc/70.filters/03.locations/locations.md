---
title: Search Locations
brief: Filter file locations searched by <strong>Aten</strong>
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

A basic stock of filters is provided with **Aten** and installed with the program - several default locations are searched for these filters on startup. Alternatively, if **Aten** fails to find these filters, or you wish to point it to a suitable directory by hand, either the `$ATENDATA` environment variable may be set to the relevant path (on Windows, this variable is set by the installer) or the [`--atendata`](/aten/docs/cli/switches#atendata) command-line option may be used to provide the path.

Additional filters may be placed in a user's `.aten` directory, e.g. `~bob/.aten/filters/`.

A specific individual filter may be loaded from the command-line by using the [--filter](/aten/docs/cli/switches#filter) switch.

## Overriding

Filters that possess the same ID or nickname as other filters of the same type may be loaded simultaneously, with the last to be loaded taking preference over the other. Thus, an importmodel filter nicknamed from **Aten**'s installed filter stock will be overridden by one of the same nickname present in a user's [name].aten/filters[/name] (or [name]aten/filters[/name] on Windows) directory. Similarly, both these filters will be overridden by one of the same nickname loaded by hand from the command line (with the [`--filter`(/aten/docs/cli/switches#filter) switch).  Note that this only holds true for filters referenced by nickname or determined automatically by **Aten** when loading data - from the GUI all filters are available in the file dialogs.

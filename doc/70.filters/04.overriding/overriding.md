---
title: Overriding
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Filters that possess the same ID or nickname as other filters of the same type may be loaded simultaneously, with the last to be loaded taking preference over the other. Thus, an importmodel filter nicknamed from **Aten**'s installed filter stock will be overridden by one of the same nickname present in a user's [name].aten/filters[/name] (or [name]aten/filters[/name] on Windows) directory. Similarly, both these filters will be overridden by one of the same nickname loaded by hand from the command line (with the [`--filter`(/aten/docs/cli/switches#filter) switch).  Note that this only holds true for filters referenced by nickname or determined automatically by **Aten** when loading data - from the GUI all filters are available in the file dialogs.



---
title: Locations
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

A basic stock of filters is provided with **Aten** and installed with the program - several default locations are searched for these filters on startup. Alternatively, if **Aten** fails to find these filters, or you wish to point it to a suitable directory by hand, either the `$ATENDATA` environment variable may be set to the relevant path (on Windows, this variable is set by the installer) or the [`--atendata`](/aten/docs/cli/switches#atendata) command-line option may be used to provide the path.

Additional filters may be placed in a user's `.aten` directory, e.g. `~bob/.aten/filters/`.

A specific individual filter may be loaded from the command-line by using the [--filter](/aten/docs/cli/switches#filter) switch.


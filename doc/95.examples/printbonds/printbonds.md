---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Example - Print All Bond Distances (CLI)

Quickly getting at geometric data in a model file is often useful without having to go through the GUI, in order to quickly verify a geometry or get certain data into a file. The following command prints out all of the bond distances, including the indices of the atoms involved, from the command line:

```
bob@pc:~> aten data/test/methanol.inp -c 'for (Bond b = aten.model.bonds; b; ++b) printf("Distance %i-%i = %f\n", b.i.id, b.j.id, geometry(b.i,b.j)); quit();'
```

This will give you output along the following lines:

```
Autodetecting patterns for model 'Methanol'..
New pattern 'OCH4' added - startatom 1, 1 mols, 6 atoms per mol.
Pattern description completed (spans 6 atoms).
Done.
Augmenting bonds in pattern OCH4...
Distance 1-2 = 1.080000
Distance 1-3 = 1.080000
Distance 1-4 = 1.080000
Distance 1-5 = 1.080000
Distance 4-6 = 1.079999
```

All very good, but what about the extra information printed by **Aten** (such as pattern detection, etc.)? This can be inhibited by adding the [a]`-q`,cli-switches#switch_q[/a] switch to the command.



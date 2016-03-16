---
title: Switch Order
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Two important things to consider here. Firstly, short options (e.g. [`-b`](/aten/docs/cli/switches#b), [`-d`](/aten/docs/cli/switches#d) etc.) may not be concatenated into one long specification of a short option (i.e. `-bd`) - they must be given separately as `-b -d` or they will not be recognised. Secondly, the order of the given switches is important since their meaning is applied or acted out immediately. For example:

```
bob@pc:~> aten --nobond test1.xyz test2.xyz
```

will load the models `test1.xyz` and `test2.xyz`, preventing recalculation of bonds between atoms in both. However:

```
bob@pc:~> aten test1.xyz --nobond test2.xyz
```

will only prevent recalculation of bonds for the second model. The reason for acting on switches and arguments in the order they are encountered on the command line is to allow for flexibility, especially when using **Aten** as a non-graphical processor.

_Note:_ The position of debug switches or those affecting the verbosity of the program has no bearing on the timeliness of their effect – they are dealt with first by **Aten** regardless of where they appear in the program’s arguments.



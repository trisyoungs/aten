---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---


# Select Window

The select window provides a means to quickly access the capabilities of the [a]select,command-selection#select[/a] and [a]deSelect,command-selection#deselect[/a] commands from within the GUI. The total number of atoms selected along with their empirical formula is also displayed.

Basic atom selection operations can be made using the **All**, **None**, **Invert**, and **Expand** buttons at the top of the window. The first three are self-explanatory, selecting all atoms, no atoms, and reversing the current selection respectively. The final button, **Expand**, looks at any bonds attached to any currently-selected atoms, and selects those atoms that are bound as well. Only the immediate bound neighbours of each atom are considered each time the expansion is made.

Three more advanced selection methods are also available. In each case, selection or deselection of atoms based on the defined range(s) is made by the associated **Select** and **Deselect** buttons.

**Selection by ID/Element Ranges**

<figure>
  <image>img/window_select_id.png</image>
  <caption>Select Window with ID / Element tab open</caption>
</figure>

Ranges of atom IDs and/or elements are specified as ‘_a_-_b_’ where _a_ and _b_ are either both atom IDs or both element symbols. In addition, the ‘+’ symbol can be used before (as in ‘+_a_’) or after (as in ‘_a_+’) an atom ID or element symbol to mean, respectively, ‘everything up to and including _a_’ or ‘_a_ and everything after’. Multiple selection ranges may be given, separated by commas. 

**Select by NETA Description**

<figure>
  <image>img/window_select_neta.png</image>
  <caption>Select Window with NETA tab open</caption>
</figure>

The NETA language implemented in **Aten** also makes for a powerful tool when selection specific atoms in a model based on their chemical environment. The relevant NETA description for the target atoms should be inserted into the **Code** editbox, and the corresponding **Element** set.

**Select by For Loop**

<figure>
  <image>img/window_select_for.png</image>
  <caption>Select Window with For Loop tab open</caption>
</figure>

More advanced still, here a short snippet of command-language code can be written. The code is automatically inserted into a function which is subsequently used in a for loop of the following construction:

```
int shouldSelect(Atom i)
{
      // … code is inserted here
}

for (Atom i = aten.model.atoms; i; ++i)
{
      if (shouldSelect(i)) i.selected = TRUE;
}
```

Thus, the code should use the variable _i_ to test for whatever criteria are necessary to cause the atom to be selected, and return  (or 1) if it is to be selected, and  (or 0) (0) otherwise. In this mode, the **Deselect** button simply uses the reverse code:

```
int shouldDeselect(Atom i)
{
      // … code is inserted here
}

for (Atom i = aten.model.atoms; i; ++i)
{
      if (shouldDeselect(i)) i.selected = FALSE;
}
```



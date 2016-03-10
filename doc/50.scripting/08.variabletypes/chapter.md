---
title: Variable Types
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

As well as the standard int, double, and string-style types in **Aten**'s scripting language, many other class-types are available that allow access to all the other objects that **Aten** utilises, such as [**Atom**s](/aten/docs/scripting/variabletypes/atom) and [**Bond**s](/aten/docs/scripting/variabletypes/bond), all [**Measurement**s](/aten/docs/scripting/variabletypes/measurement) in the current [**Model**s](/aten/docs/scripting/variabletypes/model), etc.

For variables in **Aten** that are of reference type (i.e. the non-standard types) various sub-variables and functions may be accessed in the same way class members are utilised in C++. Each non-standard variable type is listed here along with the types and descriptions of their available subvariables / members.

In the same way that class members are accessed in C/C++, subvariables of reference types are accessed by use of the full stops between member names. For instance, the [**Atom**](/aten/docs/scripting/variabletypes/atom) type allows all the information of a given atom to be accessed. The following example illustrates the process of retrieving the third atom in the current model, finding its coordinates, and subsequently adjusting them:

```
Atom a = aten.model.atoms[3];
Vector v = a.r;
printf("Old coordinates are: %f %f %f\n", v.x, v.y, v.z);
v.x += 4.0;
a.r = v;
printf("New coordinates are: %f %f %f\n", a.rx, a.ry, a.rz);
```

Lots of paths are used here. Firstly, the global [**Aten**](/aten/docs/scripting/variabletypes/aten) variable is used to get access to the current model and grab a reference to its third atom (`aten.model.atoms[3]`). Then, the coordinates of this atom are placed in a **Vector** _v_ by requesting the _r_ subvariable from the stored [**Atom**](/aten/docs/scripting/variabletypes/atom) reference. We then adjust the vector and store the new coordinates in the atom.

All members can be read, but not all can be written back to – these are read-only members for which setting a value makes no sense. Members which can be modified are indicated with mark in the 'RW' column in the following tables.

Function members act in the same as subvariable members except that they take one or more arguments enclosed in parentheses immediately following the member name, just as command functions do.



---
title: Constants
brief: Built-in constants available in the scripting language
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Several predefined constants exist, and may not be overridden by variables of the same name. All predefined in constants are defined using uppercase letters, so the lower case equivalents of the names _may_ be used as variables, functions etc.

| Name | Type | Value |
|------|------|-------| 
| ANGBOHR | **double** | 0.529177249 |
| AVOGADRO | **double** | 6.0221415E23 |
| DEGRAD | **double** | 57.295779578552 |
| FALSE | **int** | 0 |
| NULL | **int** | 0 |
| PI | **double** | 3.14159265358979323846 |
| TRUE | **int** | 1 |

In addition, all element symbols found in the input will be seen as their equivalent integer atomic number. So, instead of having to provide short strings containing the element name to, for example, the [transmute](/aten/docs/scripting/commands/build#transmute) command, the capitalised element name itself may be used. Thus...

```
transmute("Xe");
transmute(Xe);
transmute(54);
```

...are all entirely equivalent.


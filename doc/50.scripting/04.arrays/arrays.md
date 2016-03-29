---
title: Arrays
brief: Declaring and using arrays
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Arrays of most variable types are allowed (some, for instance the [**Aten**](/aten/docs/scripting/variabletypes/aten) type, don't really make sense as an array). Arrays are requested by providing a constant size (or an expression) in square brackets after the name:

```
int values[100], i = 4;
double q_[i];
```

Here, two arrays are created - an array of 100 integer numbers called _values_, and an array of four floating point numbers called _q_. Array indices always run from 1 to the size of the array, unlike C in which arrays run from 0 to N-1. Note that it is **not** possible to use a custom range of array indices, as is the case in Fortran.

Arrays can be initialised to a simple value on creation, setting all elements to the same value...

```
string s[4] = "Nothing";
```

...or each element to a different value using a list enclosed in curly brackets:

```
string s[4] = { "Nothing", "Nicht", "Nada", "Not a sausage" };
```

Also, all array elements can be set to the same value with a simple assignment:

```
int t[100];
t = 40;
```


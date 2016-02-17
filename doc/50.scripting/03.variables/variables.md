---
title: Variables
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

There are several standard rules for variables within **Aten**:

+ They must be declared before use
+ They are strongly-typed, i.e. they hold a specific type of value (e.g. an integer, a character string etc.)
+ They must begin with an alpha character (‘a’ to ‘z’), but may otherwise consist of letters, numbers, and the underscore (‘_’)
+ They may not be named the same as any existing function or internal keyword
+ Since variables are strongly-typed, trying to set an integer variable from a string will not work since these are incompatible types. However, standard C-style string conversion functions are available - see the section on [string commands](/aten/docs/scripting/commands/string).

So, to initialise some variables, assign values to them, and print them out, we would do the following:

```
int i;
double result;

i = 10;
result = i*5.0;

printf("i multiplied by 5.0 = %f\n", result);
```

In addition to the standard **int** and **double** types, a **string** variable exists for the storage of arbitrary-length character strings, and do not need to have a length specified on their creation (they will adjust dynamically to suit the assigned contents). Literal character strings should be surrounded by double-quotes. A set of variable types exist that are able to contain references (not copies) of various objects within **Aten**, e.g. atoms, models, unit cells, etc. Variables of these types are declared in exactly the same way as normal variables (see Section 8.2 for a list of available types). A [**vector**](/aten/docs/scripting/variabletypes/vector) type is provided for convenience and stores a triplet of **double** values. There is no boolean type – use an **int** instead – but the built-in [constants](/aten/docs/scripting/constants) TRUE and FALSE may be used in assignment, etc., and correspond to the integer values 1 and 0 respectively.

All variables will default to sensible values (e.g. empty string, numbers equal to zero) on initialisation. To create a variable with a specific value simply do the following:

```
int i=1,j=2,k=1001;
double myvar=0.0, angle = 90.0;
```


---
title: Functions
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

For functions that take arguments, the argument list should be provided as a comma-separated list in parentheses after the function name. For instance:

```
newAtom("C", 1.1, 0, 4.2);
```

Arguments may be constant values (as in this example), variables, arithmetic expressions, or any other function that returns the correct type. For functions that (optionally) do not take arguments, the empty parentheses may be omitted. A list of all functions, their arguments, and their return types is given in the command reference in Section 9.

All functions return a value, even if it is 'no data' (i.e. **void** in C/C++). For instance, in the example above the [a]newAtom,cmd-build#newatom[/a] command actually returns a reference to the [a]Atom,vtypes-atom[/a] it has just created, and this may be stored for later use:

```
Atom a;
a = newAtom("C", 1.0, 1.0, 1.0);
```

However, if the return value of any function is not required then it may simply be forgotten about, as in the example prior to the one above.

## User Defined Functions

Custom functions may be defined and used in Aten, taking a list of variable arguments with (optional) default values. The syntax for defining a function is as follows:

```
type **name** ( arguments ) { commands }
```

 is one of the standard variable types and indicates the expected return value type for the function. If no return value is required (i.e. it is a subroutine rather than a function) then  should be replaced by the  keyword:

```
void **name** ( arguments ) { commands }
```

Once defined, functions are called in the same way as are built-in functions. The _name_ of the function obeys the same conventions that apply to variable names, e.g.  must begin with a letter, cannot be the name of an existing keyword, function, or variable. The _arguments_ are specified in a similar manner to variable declarations. A comma-separated declaration list consisting of pairs of variable types and names should be provided, e.g.:

```
void **testroutine** ( int i, int j, double factor ) { ... }
```

Our new subroutine **testroutine** is defined to take three arguments; two integers, _i_ and _j_, and a double _factor_. All three must be supplied when calling the function, e.g.

```
printf("Calling testroutine...\n");
int num = 2;
double d = 10.0;
testroutine(num, 4, d);
printf("Done.\n");
```

When defining the function/subroutine arguments, default values may be indicated, and permit the function to be called in the absence of one or more arguments. For instance, lets say that for our **testroutine**, the final argument _facto_ is likely to be 10.0 on most occasions. We may then define a default value for this argument, such that if the function is called without it, this default value will be assumed:

```
void testroutine ( int i, int j, double factor = 10.0 ) { ... }

printf("Calling testroutine...\n");
int num = 2;
testroutine(num, 4);
testroutine(num, 4, 20.0);
printf("Done.\n");
```

Both methods of calling _testroutine_ in this example are valid.

## Return Values

For functions defined to return a specific type, at some point in the body of the function a suitable value should be returned. This is achieved with the _return_ keyword. Consider this simple example which checks the sign of a numeric argument, returning 1 for a positive number and -1 for a negative number:

```
int checksign ( double num )
{
      if (num < 0) return -1;
      else return 1;
}
```

If an attempt is made to return a value whose type does not match the type of the function, an error will be raised. Note that, once a _return_ statement is reached, the function is exited immediately. For functions that do not return values (i.e. those declared with _void_) then _return_ simply exits from the function – no return value need, or should, be supplied.

## Arithmetic Expressions and Operators

Arithmetic operates in the same way as in C, and utilises the same operator precedence etc. Similarly, comparison operators (less than, equal to etc.) are the same as those found in C.


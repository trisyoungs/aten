---
title: Flow Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Loop control and conditional statements.

---

## do <a id="do"></a>

_Syntax:_

**do** { _commands_ } _while_ { _condition_ }

The do-while loop is cousin of the ‘for’ loop, except that there is no control variable. The termination of the loop depends on the _condition_ which is tested at the end of every execution of the _commands_. If the condition evaluates to **TRUE**, the _commands_ are executed again, and _condition_ re-tested afterwards. If **FALSE** the loop ends.

For example:
```aten
int i = 1;
do { i = i * 2; printf("i = %d\n", i); } while (i < 100);
```

will print out the following:
```aten
i = 2
i = 4
i = 8
i = 16
i = 32
i = 64
i = 128
```

Note that the final value of _i_ inside the loop is 128 (greater than 100) since the _condition_ is only tested at the end of the execution of the _commands_. The while loop works in the same way, save that the _condition_ is tested at the beginning of the loop, before _commands_ are executed, rather than at the end.

---

## for <a id="for"></a>

_Syntax:_

**for** ( _startvalue_ ; _condition_ ; _increment_ ) { _commands_ }

Three separate components make up a 'for' loop. _startvalue_ defines both the control variable (i.e. the variable that changes on each loop iteration) and optionally its starting value, the _condition_ is tested on each loop iteration to see whether or not to continue with the loop, and finally the _increment_ is an expression to modify the control variable after each iteration, setting it to a new value. If multiple _commands_ are to make up the body of the loop (executed on each iteration) then they should be enclosed in curly brackets (as written in the syntax above). If only a single command is executed on each iteration, the curly brackets may be omitted.

Some examples:

```aten
for (int i=1; i<=10; i = i + 1) printf("%i\n", i);
```

Loop over and print all integers between 1 and 10. A local variable _i_ is declared and initialised all in one go in the _startvalue_ part of the loop. The 'long' way of incrementing the integer variable (i = i + 1) is typically not used in C/C++, most people preferring to take advantage of the C's useful postfix and prefix operators, as in the next example).

```aten
for (n = 100; n>0; --n) printf("Counting down... %i\n", n);
```

Here, an existing variable _n_ is decreased from 100 to 1, printing out all numbers along the way. Note the usage of the double-minus '--' operator (the prefix decrease operator) which decreases its associated variable, in this case _n_. For integers, to decrease means to reduce the value by 1. For other types the meaning may vary – for instance, with reference types the '--' operator means 'previous item in the list', since all such objects in Aten (e.g. atoms) are stored in lists containing many objects of the same type. This makes iterating over, say, all atoms in a given model pretty easy...
```aten
for (atom a = aten.model.atoms; a; ++a)
{
    printf("Atom id %i is element %s.\n", a.id, a.symbol);
}
```

In this example the variable _a_ is declared and initialised to be a reference to the first atom in the current model. The _condition_ part simply consists of the expression ‘_a_’, which effectively tests the reference address currently stored in _a_. Since any positive number equates to **TRUE** (see below for the _if_ test) the loop will continue until _a_ contains no reference. Since most all reference objects in Aten are stored internally in linked lists, the prefix increment operator (++) changes the value of the variable to be the reference of the next item in the list, or 0 if there are no more items. In this way, the whole list of atoms can be traversed and neatly ended once the final item in the list has passed.

A variant of the **for** loop described above is the [name]for/in[/name] loop – here, only a control variable and initial value are supplied, both of which must be of pointer types. The loop itself will increase the value of the variable (i.e. skip to the next item in the linked list) until a **NULL** pointer is found. For example:
```aten
select(H);
for (atom i = aten.model.selection; i; ++i)
{
    printf("Atom %i is selected\n", i.id);
}

for (atom ii in aten.model.selection)
{
    printf("Atom %i is selected\n", ii.id);
}
```

This will select all hydrogen atoms in the current model then loop over the atom selection twice, once with a for loop and once with a for/in loop, both of which are equivalent.

---

## if <a id="if"></a>

_Syntax:_

**if** ( _condition_ ) { _commands_ } [ [name]else if[/name]  { _commands_ } ] [ **else** { _commands_ }

The _if_ statement permits sections of code to be executed based on the assessment of logical comparison of values. If the supplied _condition_ evaluates to be **TRUE** then the following _commands_ are executed, otherwise nothing happens. In the second form of the command, if the _condition_ evaluates to be **FALSE** then the second set of _commands_ are executed instead. If multiple _commands_ are to be executed then they should be enclosed in curly brackets (as written in the syntax above). If only a single command is to be executed the curly brackets may be omitted.

Typically, comparisons are made between two variables, for example:

```aten
if ( var1 > var2 ) ...
```

checks for _var1_ being greater in value than _var2_, executing the following commands if this turns out to be true. The comparison operator may be any one of the following symbols:

<table>
  <title>Comparison Operators</title>
 <header>
  <column>Operator</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>==</column>
  <column>Equal to</column>
 </row>
 <row>
  <column>!=</column>
  <column>Not equal to</column>
 </row>
 <row>
  <column><></column>
  <column>Not equal to</column>
 </row>
 <row>
  <column>></column>
  <column>Greater than</column>
 </row>
 <row>
  <column><</column>
  <column>Less than</column>
 </row>
 <row>
  <column>>=</column>
  <column>Greater than or equal to</column>
 </row>
 <row>
  <column><=</column>
  <column>Less than or equal to</column>
 </row>
</table>

In truth, the _condition_ part may be any expression, command, or amalgam of both, provided the end result of executing it is a single value. The type of the final result doesn't even matter, since conversion to a boolean is guaranteed. Deep down in the logic of Aten, integers are at the heart of it all, with zero or any negative number being **FALSE**, and any positive number meaning **TRUE**.

For example:
```aten
int i = 10;
if (i > 5) printf("Hooray!\n");
```

in this case 'Hooray!' will be printed, because _i_ is greater than 5.
```aten
int i = 10, j = 20;
if (i > j) printf("Hooray!\n");
```

but in this case 'Hooray!' will **not** be printed, because _i_ is not greater than _j_.
```aten
int i = 10, j = 20;
if (i > j) printf("Hooray!\n");
else { printf("Too small.\n"); i = j; }
```

Here, the test fails for the same reason, but since an _else_ part was provided we still execute some commands (printing 'Too small.' and setting the variable _i_ equal to _j_).

Since any positive number is **TRUE**, we can simply test the value of a variable.
```aten
int i = -55;
if (i) printf("Snoopy.\n");

Atom a = newAtom("H");
if (a) printf("New atom.\n");
```

In a similar way, a reference variable has a positive integer value at its heart, and so can also be tested in this way.
```aten
Atom a = newAtom("H");
double alpha = 100.0;
if ( (a) &amp;&amp; (alpha < 50.0) ) printf("Alpha and atom are OK.\n");
else printf("No good!\n");
```

Two or more consecutive _conditions_ can be tested in order to determine ‘truth’, in this case using the ‘and’ operator '&amp;&amp;'. Here, the value of the reference variable _a_ and the value of _alpha_ are both checked, and the text ‘Alpha and atom are OK.’ is only printed if both turn out to be **TRUE**.
```aten
if (time == 0) printf("There is no time.");
else if (time > 5) printf("There is more than enough time.");
else printf("There is only a little time.");
```

Multiple if tests can also be nested to create a sequence of tests. As soon as a _condition_ is encountered that equates to 'true' the accompanying _commands_ are executed and any subsequent 'else'd tests or commands are ignored.

---

## return <a id="return"></a>

_Syntax:_

**return**

**return** _value_

Used in (user-defined) functions, and returns control immediately back to the calling function. In the case of a _void_ function, no return value must be specified. Similarly, for functions returning a value a valid value of that type must be given.

---

## while <a id="while"></a>

_Syntax:_

**while** ( _condition_ ) { _commands_ }

The while loop is another cousin of the _for_ loop, and as with the [name]do-while[/name] loop there is no control variable. The termination of the loop depends on the _condition_ which is tested at the beginning of the loop, before execution of the _commands_. If **TRUE**, the _commands_ are executed, but if **FALSE** the loop ends without executing the _commands_ (and meaning that it is possible that the _commands_ are never executed).

For example:
```aten
int i = 1024;
while (i > 100) { i = i / 2; printf("i = %d\n", i); }
```

will print out the following:
```aten
i = 512
i = 256
i = 128
i = 64
```


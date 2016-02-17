---
title: Filter RW
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Formatted output in **Aten** is based largely on string formatting in C, so if you're familiar with C then this should be a breeze. If you're a Soldier of Fortran, then the principles are very similar. If you're familiar with neither, then now's the time to learn.

## Formatted Output

Formatted output corresponds to output to either the screen or to files, and is used in the following commands:

| Command | Function |
|---------|----------|
| [error](/aten/docs/scripting/commands/messaging#error) | Write a message to the screen and immediately terminate execution of the current script / filter / command structure |
| [printf](/aten/docs/scripting/commands/messaging#printf) | Write a message to the screen |
| [verbose](/aten/docs/scripting/commands/messaging#verbose) | Write a message to the screen, provided verbose output mode is on |
| [writeLineF](/aten/docs/scripting/commands/rw#writelinef) | Write a formatted line to the current output file |
| [writeVarF](/aten/docs/scripting/commands/rw#writevarf) | Write a formatted string to a variable (equivalent to the C 'sprintf' command) |

### Basic Strings

Formatting a string for output, as mentioned elsewhere on numerous occasions, works the same as for C. The C [printf](/aten/docs/scripting/commands/messaging#printf) command (equivalent to the command of the same name in **Aten**) takes a character string as its first argument, and at its simplest, this is all that is required:

```
printf("Hello");
```

This prints 'Hello' to the screen (minus the quotes). Importantly, however, a newline character is _not_ printed, meaning that the next thing you try and [printf](/aten/docs/scripting/commands/messaging#printf) will appear on the same line. For instance:

```
printf("Hello");
printf("There.");
```

would output:

```
HelloThere.
```

The end of a character constant in the printf command does not implicitly mean 'and this is the end of the line' - you must indicate the end of the line yourself by placing '\n' at the point where you wish the line to end. So:

```
printf("Hello\n");
printf("There.");
```

would output:

```
Hello
There.
```

Newlines (\n) are an example of escaped characters - the backslash '\' indicates that the following character, in this case 'n', is not to be treated as a normal 'n', but instead will take on its alternative meaning, in this case a newline character. There are one or two other escaped characters recognised - see Section 10.1 for a list. Note that the newline token can appear anywhere in the string, and any number of times. So:

```
printf("Hello\nThere\n.");
```

would output:

```
Hello
There
.
```

## Printing Data

Being able to print simple text strings is good, but not nearly enough. The first argument to the 'printf' command must always be a character string, but any number of additional arguments may be provided. Now, these additional arguments may be number constants, other character strings, variables, etc., and may be output in the resulting string by referencing them with 'specifiers' placed within the first example. One example of a specifier is '%i' which is shorthand for saying 'an integer value' – if used within the character string provided to printf, the command will expect an integer constant or variable to be provided as an additional argument. For example:

```
printf("This number is %i.\n", 10);
```

will print

```
This number is 10.
```

Similarly,

```
int value = 1234;
printf("Constant is %i, variable is %i.\n", 10, value);
```

will print

```
Constant is 10, variable is 1234.
```

There are other specifiers suitable for different types of data – see Section 11.3.4. The way data is presented by the specifier in the final output can also be controlled (e.g. for numerical arguments the number of decimal places, presence of exponentiation, etc., can be defined).

## Formatted Input

Formatted input corresponds to input from either files or string variables, and is used in the following commands:

| Command | Function |
|---------|----------|
| [readLineF](/aten/docs/scripting/commands/rw#readLineF) | Read a formatted line from the current input file |
| [readVarF](/aten/docs/scripting/commands/rw#readVarF) | Read a formatted string from a variable |

Note that the meaning of the formatting string changes slightly here - in essence, the type and formats of the specifiers are used to break up the supplied string into separate arguments, which are then placed in the provided corresponding variable arguments. When reading in string data, note that blank characters are significant and will be retained. To strip trailing blank characters (spaces and tabs) when reading a fixed-length string in a format, supply the length as a negative number.

## Specifiers

The list of allowable variable specified corresponds more or less exactly to that found in C, with some small omissions and minor inclusions. For a full list see the reference page at [www.cpluscplus.com](http://www.cplusplus.com/reference/clibrary/cstdio/printf/) or [cppreference.com](http://www.cppreference.com/wiki/c/io/printf,cppreference.com). The list of printf features that are not (currently) supported in **Aten** are as follows:

+ The pointer specifier **%p** is not supported. To print out reference addresses, use **%li**
+ The single-character specifier **%c** is not supported
+ Output of long doubles by prefixing a specifier with  (e.g. **%Le**) is not supported

## Extra Specifiers Within **Aten**

As well as the mostly complete standard set of specifiers provided by C, **Aten** also includes some other useful specifiers that may be used in formatted input and output.

| Specifier | Meaning |
|-----------|---------|
| %* | Relevant to formatted input only. Discard the next item, regardless of its type. A corresponding variable argument need not be provided |
| %r | Read characters (starting from the next delimited argument) until the end of the input line is encountered (i.e. 'rest-of-line' specifier). A corresponding string variable should be provided |

## Escaped Characters

| Escape Sequence | Meaning |
|-----------------|---------|
| \n | Print newline (next character will appear on the next line) |
| \r | Carriage return |
| \t | Tab character |

## Delimited Reading and Writing

Formatting strings (or 'format specifiers') can be used to specify the layout of data items on a line when reading or writing data, but if the data are separated by whitespace characters such as spaces or tabs (or, alternatively, commas), such delimited data can be read in more easily. In such cases, it is not necessary to know beforehand the number of characters taken up by each item on the line, since the delimiters separate adjacent data items. A simplified method for reading and writing can be employed in these cases.

Commands providing delimited reading and writing are:

| Command | Function |
|---------|----------|
| [readLine](/aten/docs/scripting/commands/rw#readLine) | Read delimited items from a source file, placing into the variables provided |
| [readNext](/aten/docs/scripting/commands/rw#readNext) | Read the next delimited item from a source file, placing into the variable provided |
| [readVar](/aten/docs/scripting/commands/rw#readVar) | Read delimited items from a source variable, placing into the variables provided |
| [writeLine](/aten/docs/scripting/commands/rw#writeLine) | Write the supplied items to a single line in the output file, separating them with whitespace |
| [writeVar](/aten/docs/scripting/commands/rw#writeVar) | Write the supplied items to a supplied string variable, separating them with whitespace |

Note that all are called the same as their formatted counterparts, but minus the 'f' at the end of the name.

### Delimited Data Example

Consider this example datafile:

```
Na      0.0     1.0     0.0
Cl      1.0     0.0     0.0
Na      0.0    -1.0     0.0
```

Since the data items (element type and coordinates) are separated by whitespace, we need only provide the target variables to the relevant command - a formatting string, as is demanded by the [printf](/aten/docs/scripting/commands/string#printf) command, is not required. Using the [readLine](/aten/docs/scripting/commands/rw#readLine) command, the following code will parse this data correctly:

```
double x,y,z;
string el;
while (!eof()) { readline(el,x,y,z); newatom(el,x,y,z); }
```

The variables _el_, _x_, _y_, and _z_ will, at any one time, contain the element type and coordinates from one line of the file. In an analogous manner, the data may be written out again with the corresponding [writeLine](/aten/docs/scripting/commands/rw#writeLine) command:

```
for (Atom i = aten.model.atoms; i; ++i) writeline(i.symbol,i,rx,i.ry,i.rz);
```

Each line will have the individual data items separated by a single space.

The [readNext](/aten/docs/scripting/commands/rw#readNext) command reads in a single delimited item from a source file, preserving the remainder of the input line for subsequent operations. If there is no data left on the current line, a new line is read and the first delimited item is returned. The example above might be written in a slightly clunkier form as:

```
double x,y,z;
string el;
while (!eof())
{
      readnext(el);
      readnext(x);
      readnext(y);
      readnext(z);
      newatom(el,x,y,z);
}
```

For all delimited reading operations, items of data read from the line are converted automatically into the type of the destination variable. So, the atom coordinates read in above, which are put into **double**-type variables, could equally well be put into string variables. Standard C routines are used to convert data items in this way, and only some conversions make sense. For instance, attempting to read an item which is a proper character string (such as element symbol/name data) into a double or integer variable does not make sense. No error message will be raised, and the variables will likely be set to a value of zero (or whatever passes for 'zero' in the context of the type).

For all delimited writing operations, a suitable standard format specifier is chosen with which to write out the data. 



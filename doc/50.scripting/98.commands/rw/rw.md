---
title: Read/Write Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Methods of reading and writing data from / to files in import and export filters. Many commands here use formatting strings to provide formatted input and output. All reading and writing commands here work on input or output files as defined internally by the program.

---

## addReadOption <a id="addReadOption"></a>

_Syntax:_

**void** **addReadOption** ( **string** _option_ )

Controls aspects of file reading. A list of possible _option_s that affect reading is given in the [**ReadOption**](/aten/docs/enums/readoption) enum.

For example:

```aten
addReadOption("stripbrackets");
```

---

## eof <a id="eof"></a>

_Syntax:_

**int** **eof** ( )

Returns 1 if at the end of the current input file, 0 otherwise.

For example:
```aten
string s;
while (!eof()) { getLine(s): printf("%s\n", s); }
```

Reads in and prints out all lines in the current source file.

---

## filterFilename <a id="filterfilename"></a>

_Syntax:_

**string** **filterFilename** ( )

Returns the name of the current input or output file (typically useful from within an import or export filter).

For example:

```aten
string filename = filterFilename();
```

Puts the current source/destination filename in the variable filename.

---

## find <a id="find"></a>

_Syntax:_

**int** **find** ( **string** _searchString_ )

**int** **find** ( **string** _searchString_, **string** _lineVar_ )

Searches for the specified _searchString_ in the input file, returning 0 if _searchString_ is not found before the end of the file, and 1 if it is. The optional argument _lineVar_ is a character variable in which the matching line (if any) is put. If the search string is not found the file position is returned to the place it was before the command was run.

For example:

```aten
int iresult = find("Final Energy:");
```

searches for the string 'Final Energy' in the input file, placing the result of the search in the variable _iresult_.
```aten
string line;
int n = find("Optimised Geometry:", line);
```

searches for the string "Optimised Geometry:" in the input file, placing the whole of the matching line from the input file in the variable _line_.

---

## getLine <a id="getline"></a>

_Syntax:_

**int** **getLine** ( **string** _destvar_ )

Read an entire line from the input file, and put it in the character variable provided. The line also becomes the current target for readnext. The command returns a [Read Success](/aten/docs/enums/readsuccess) integer.

For example:
```aten
string nextline;
int n = getLine(nextline);
```

gets the next line from the file and places it in the variable _nextline_.

---

## nextArg <a id="nextarg"></a>

_Syntax:_

**int** **nextArg** ( **int** _i_ )

**int** **nextArg** ( **double** _d_ )

**int** **nextArg** ( **string** _s_ )

Read the next whitespace-delimited chunk of text from the current file and place it in the variable supplied. Note that this command reads directly from the file and not the last line read with getline or readline (see the readnext command to read the next delimited argument from the last read line). The command returns TRUE (1) if an argument was successfully read, or **FALSE** (0) otherwise (e.g. if the end of the file was found).

The command will accept a variable of any ordinary type – one of int, double, or string – as its argument. Conversion between the text in the file and the target variable type is performed automatically.

For example:
```aten
int i;
int success = nextArg(i);
```

---

## peekChar <a id="peekchar"></a>

_Syntax:_

**string** **peekChar** ( )

Peeks the next character that will be read from the source file, and returns it as a string. The actual file position for reading is unaffected.

For example:

```aten
string char = peekChar();
```

---

## peekCharI <a id="peekchari"></a>

_Syntax:_

**int** **peekCharI** ( )

Peeks the next character that will be read from the source file, and returns it as an integer value representing the ASCII character code (see http://www.asciitable.com, for example). The actual file position for reading is unaffected.

For example:

```aten
int char = peekCharI();
```

---

## readChars <a id="readchars"></a>

_Syntax:_

**string** **readChars** ( **int** _nChars_, **bool** _skipEol_ = **TRUE** )

Reads and returns (as a string) a number of characters from the input file. If _skipEol_ is true (the default) then the end-of-line markers ‘\n’ and ‘\r’ will be ignored and will not count towards _nChars_ – this is of most use when reading formatted text files and you want to ignore the fact that data is presented on many lines rather than one. If _skipEol_ is false then ‘\n’ and ‘\r’ will count towards the total number of characters. Used on formatted text files, this might give you unexpected results.

For example:

```aten
string text = readChars(80);
```

reads the next 80 characters from the input file and puts it into the variable _text_.

---

## readDouble <a id="readdouble"></a>

_Syntax:_

**double** **readDouble** ( )

**double** **readDouble** ( **int** _nBytes_ )

Read a floating point value (the size determined from the machines ‘double’ size) from an unformatted (binary) input file. Alternatively, if a valid number of bytes is specified and corresponds to the size of another 'class' of double (e.g. long double) on the machine this size is used instead.

For example:

```aten
double x = readDouble();
```

reads a floating point value into the variable _x_.

---

## readDoubleArray <a id="readdoublearray"></a>

_Syntax:_

**int** **readDoubleArray** ( **double** _d_\[\], **int** _n_ )

Read _n_ consecutive integer values (whose individual size is determined from the result of calling ‘sizeof(double)’) from an unformatted (binary) input file, placing in the array _d_ provided. The size of the array provided must be at least _n_. The command returns a [Read Success](/aten/docs/enums/readsuccess) integer..

For example:
```aten
double data[45];
int success = readDoubleArray(data, 45);
```

reads 45 double numbers into the array _data_.

---

## readInt <a id="readint"></a>

_Syntax:_

**int** **readInt** ( )

**int** **readInt** ( **int** _nBytes_ )

Read an integer value (the size determined from the result of calling ‘sizeof(int)’) from an unformatted (binary) input file. Alternatively, if a valid number of bytes is specified and corresponds to the size of another class of int (e.g. long int) on the machine this size is used instead.

For example:

```aten
int i = readInt();
```

reads an integer number into the variable _i_.

---

## readIntArray <a id="readintarray"></a>

_Syntax:_

**int** **readIntArray** ( **int** _i_\[\], **int** _n_ )

Read _n_ consecutive integer values (whose individual size is determined from the result of calling ‘sizeof(int)’) from an unformatted (binary) input file, placing in the array _i_ provided. The size of the array provided must be at least _n_. The command returns a [Read Success](/aten/docs/enums/readsuccess) integer..

For example:

```aten
int data[100]; int success = readintarray(data, 100);
```

reads 100 integer numbers into the array _data_.

---

## readLine <a id="readline"></a>

_Syntax:_

**int** **readLine** ( **int**|**double**|**string** _var_, ... )

Read a line of delimited items from the input file, placing them into the list of variable(s) provided. Conversion of data from the file into the types of the destination variables is performed automatically. The number of items parsed successfully is returned.

For example:
```aten
double x,y,z;
int n = readLine(x,y,z);
```

reads a line from the file and places the first three delimited items on the line into the variables _x_, _y_, and _z_.

---

## readLineF <a id="readlinef"></a>

_Syntax:_

**int** **readLineF** ( **string** _format_, ... )

Read a line of data from the input file and separate them into the list of variable(s) provided, and according to the format provided. The number of items parsed successfully is returned.

For example:
```aten
double x,y,z;
int n = readLineF("%8.6f %8.6f %8.6f",x,y,z);
```

reads a line from the file, assuming that the line contains three floating point values of 8 characters length, and separated by a space, into the three variables _x_, _y_, and _z_.

---

## readNext <a id="readnext"></a>

_Syntax:_

**int** **readNext** ( **int** _i_ )

**int** **readNext** ( **double** _d_ )

**int** **readNext** ( **string** _s_ )

Read the next delimited argument from the last line read with either getline or readline into the variable supplied. The command returns either TRUE for success or **FALSE** (e.g. if the end of file was reached without reading any non-whitespace characters, or an error was encountered).

For example:
```aten
double d;
int n = readNext(d);
```

read the next delimited argument into the double variable _d_.

---

## readVar <a id="readvar"></a>

_Syntax:_

**int** **readVar** ( **string** _source_, **int**|**double**|**string** _var_, ... )

Parse the contents of the supplied string _source_ into the supplied variables, assuming delimited data items. Delimited items read from _source_ are converted automatically to the type inferred by the target variable. The number of data items parsed successfully is returned.

For example:
```aten
string data = "rubbish ignore Carbon green 1.0 2.5 5.3";
string element, discard; vector v;
int n = readVar(data,discard,discard,element,discard,v.x,v.y,v.z,discard);
printf("Element = %s, n = %i\n", element, n);
```

outputs

```aten
Element = Carbon, n = 7 
```

The character string in the variable _data_ is parsed, with delimited chunks placed into the supplied variables. Note the repeated use of the variable _discard_, used to get rid of unwanted data. Also, note that there are not enough items in _data_ to satisfy the final occurrence of _discard_, and so the function returns a value of 7 (as opposed to the actual number of target variables supplied, 8).

---

## readVarF <a id="readvarf"></a>

_Syntax:_

**int** **readVarF** ( **string** _source_, **string** _format_, **int**|**double**|**string** _var_, ... )

Parse the contents of the supplied string _source_ according to the supplied _format_ string, placing in the supplied variables. The number of format specifiers successfully parsed (or, to look at it another way, the number of the supplied variables that were assigned values) is returned.

For example:
```aten
string a, b, data = "abc def123456.0";
double d; int i, n;
n = readVarF(data,"%3s %3s%4i%4f%8*",a,b,i,d);
printf("a = %s, b = %s, d = %f, i = %i, n = %i\n", a, b, d, i, n);
```

outputs

```aten
a = abc, b = def, d = 56.000000, i = 1234, n = 4 
```

The supplied format string contains a single space in between the two ‘%3s’ specifiers, and is significant since it corresponds to actual (discarded) space when processing the format. Furthermore, the last specifier ‘%8*’ (discard 8 characters) is not fulfilled by the _data_ string, and so the number of arguments successfully parsed is 4, not 5.

---

## removeReadOption <a id="removeReadOption"></a>

_Syntax:_

**void** **removeReadOption** ( **string** _option_ )

Removes a previously-set read option. A list of possible _option_s that affect reading is given in the [**ReadOption**](/aten/docs/enums/readoption) enum.

For example:

```aten
removeReadOption("skipblanks");
```

---

## rewind <a id="rewind"></a>

_Syntax:_

**void** **rewind** ( )

Rewind the input file to the beginning of the file.

For example:

```aten
rewind();
```

---

## skipChars <a id="skipchars"></a>

_Syntax:_

**void** **skipChars** ( **int** _n_ )

Skips the next _n_ characters in the input file.

For example:

```aten
skipChars(15);
```

discards the next 15 characters from the input file.

---

## skipLine <a id="skipline"></a>

_Syntax:_

**void** **skipLine** ( **int** _n_ = 1 )

Skips the next line in the file, or the next _n_ lines if a number supplied.

For example:

```aten
skipLine();
```

skips the next line in the file.

```aten
skipLine(5);
```

discards 5 lines from the file.

---

## writeLine <a id="writeline"></a>

_Syntax:_

**void** **writeLine** ( **int**|**double**|**string** _var_, ... )

Write a line to the current output file that consists of the whitespace delimited contents of the supplied arguments. The contents of the arguments are formatted according to their type and suitable internal defaults. A newline character is appended automatically to the end of the written line.

For example:

```aten
writeLine("Number of atoms =", aten.model.nAtoms);
```

writes a line indicating the number of atoms in the model to the current output file.

---

## writeLineF <a id="writelinef"></a>

_Syntax:_

**void** **writeLineF** ( **string** _format_, ... )

Write a formatted line to the current output file, according to the supplied _format_ and any supplied arguments. Usage is the same as for the printf command. Note that a newline character is not automatically appended to the end of the written line, and one should be written explicitly using the escape sequence '\n'.

For example:

```aten
writeLineF("%s = %8i\n", "Number of atoms", aten.model.nAtoms);
```

writes a line indicating the number of atoms in the model to the current output file, e.g.:

```aten
Number of atoms = 3
```

---

## writeVar <a id="writevar"></a>

_Syntax:_

**void** **writeVar** ( **string** _dest_, ... )

Write to the supplied string variable _dest_ the whitespace delimited contents of the supplied arguments. The contents of the arguments are formatted according to their type and suitable internal defaults. A newline character is appended automatically to the end of the written line.

For example:
```aten
string s;
writeVar(s,"Number of atoms =", aten.model.nAtoms);
writeLine(s);
```

same result as the example for the **writeLine** command, except that the final string is written to a variable first, and then the file.

---

## writeVarF <a id="writevarf"></a>

_Syntax:_

**void** **writeVarF** ( **string** _dest_, **string** _format_, ... )

Write to the supplied string variable _dest_ the string resulting from the supplied _format_ and any other supplied arguments. Apart from the mandatory first argument being the destination string variable, usage is otherwise the same as the printf command. Note that a newline character is _not_ automatically appended to the end of the written line, and one should be written explicitly using the escape sequence ‘\n’.

For example:
```aten
string s;
writeVarF(s,"%s = %8i\n", "Number of atoms", aten.model.nAtoms);
writeLine(s);
```

same result as the example for the **writeLineF** command, except that the final string is written to a variable first, and then the file.


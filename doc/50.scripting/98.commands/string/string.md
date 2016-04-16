---
title: String Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Manipulation and conversion of string variables.

---

## afterStr <a id="afterstr"></a>

_Syntax:_

**string** **afterStr** ( **string** _source_, **string** _search_, **bool** _sourceOnFail_ = **FALSE** )

Return the part of the _source_ string that comes after the first occurrence of the _search_ string. If _source_ doesn’t contain any occurrences of _search_ an empty string is returned, unless the flag _sourceOnFail_ is set to **TRUE** in which case the original source string is returned instead.

For example:
```aten
string fullname = "BobBobbinson";
string firstname = afterStr(name, "Bob");
```

sets the variable _firstname_ to the value ‘Bobbinson’.
```aten
string text1, text2;
text1 = "No taxes on axes";
text2 = afterStr(text1, "x");
```

results in _text2_ having a value of ‘es on axes’.

---

## atof <a id="atof"></a>

_Syntax:_

**double** **atof** ( **string** _text_ )

Converts the supplied _text_ into its floating point (double) representation. Equivalent to the standard C routine ‘atof’.

For example:

```aten
double x = atof("1.099d");
```

would set _x_ to the value '1.099'.

---

## atoi <a id="atoi"></a>

_Syntax:_

**int** **atoi** ( **string** _text_ )

Converts the supplied _text_ into its integer representation. Equivalent to the standard C routine 'atoi'.

For example:

```aten
int i = atoi("000023");
```

would set _i_ to the value '23'.

---

## beforeStr <a id="beforestr"></a>

_Syntax:_

**string** **beforeStr** ( **string** _source_, **string** _search_, **bool** _sourceOnFail_ = **FALSE** )

Return the part of the _source_ string that comes before the first occurrence of the _search_ string. If _source_ doesn't contain any occurrences of _search_ an empty string is returned, unless the flag _sourceOnFail_ is set to **TRUE** in which case the original source string is returned instead.

For example:
```aten
string source, target;
source = "Engelbert";
target = beforeStr(source, "e");
```

places the text "Eng" in the variable _target_.
```aten
string text1 = "No taxes on axes";
string text2 = beforeStr(text1, " ax");
```

places the text "No taxes on" in the variable _text2_.

---

## contains <a id="contains"></a>

_Syntax:_

**int** **contains** ( **string** _source_, **string** _search_ )

Returns the number of times the _search_ string is found in the _source_ string. The function counts only non-overlapping occurrences of _search_.

For example:
```aten
string poem = "six sixes are thirty-six";
int count = contains(poem, "six");
```

sets _count_ to ‘3’.

---

## ftoa <a id="ftoa"></a>

_Syntax:_

**string** **ftoa** ( **double** _d_ )

Converts the supplied double _d_ into a string representation.

For example:

```aten
string num = ftoa(100.001);
```

would set _num_ to the value’100.00’.

---

## itoa <a id="itoa"></a>

_Syntax:_

**string** **itoa** ( **int** _i_ )

Converts the supplied integer _i_ into a string representation.

For example:

```aten
string num = itoa(54);
```

would set _num_ to the value "54".

---

## lowerCase <a id="lowercase"></a>

_Syntax:_

**string** **lowerCase** ( **string** _source_ )

Returns the source string with all uppercase letters converted to lowercase.

---

## replaceChars <a id="replacechars"></a>

_Syntax:_

**string** **replaceChars** ( **string** _source_, **string** _searchChars_, **string** _replaceChar_ )

Searches through the supplied _source_ string, and replaces all occurrences of the individual characters given in the string _searchChars_ with the character supplied in _replaceChar_, returning the new string.

For example:

```aten
string newstring = replaceChars("Zero 2599 these numbers", "123456789", "0");
```

replaces any numeric character with a zero.

---

## replaceStr <a id="replacestr"></a>

_Syntax:_

**string** **replaceStr** ( **string** _source_, **string** _searchStr_, **string** _replaceStr_ )

Replaces all occurrences of _searchStr_ with _replaceStr_ in the supplied _source_ string, returning the result.

For example:

```aten
string fruity = replaceStr("I don't like apples.", "apples", "oranges");
```

would change your fondness towards apples.

---

## removeStr <a id="removestr"></a>

_Syntax:_

**string** **removeStr** ( **string** _source_, **string** _searchStr_ )

Removes all occurrences of _searchStr_ from the _source_ string, returning the result.

For example:

```aten
string debt = removeStr("I owe you 2 million dollars.", "million ");
```

would reduce your outgoings considerably.

---

## sprintf <a id="sprintf"></a>

_Syntax:_

**string** **sprintf** ( **string** _dest_, **string** _format_, ... )

Prints a formatted string to the supplied variable _dest_, and is an alias for the **writeVarF** command.

---

## stripChars <a id="stripchars"></a>

_Syntax:_

**string** **stripChars** ( **string** _source_, **string** _charList_ )

Strip the supplied character(s) from the _source_ string, returning the result.

For example:

```aten
string abc = stripChars("Oodles of noodles", "o");
```

places the text "Odles f ndles" in the variable _abc_.
```aten
string abc = "Oodles of noodles";
abc = stripChars(abc, "aeiou");
```

strips all vowels from the input string, placing the text’Odls f ndl’ in the variable _abc_.

---

## toa <a id="toa"></a>

_Syntax:_

**string** **toa** ( **string** _format_, ... )

Returns a string formatted according to the supplied _format_.

---

## upperCase <a id="uppercase"></a>

_Syntax:_

**string** **upperCase** ( **string** _source_ )

Returns the source string with all lowercase letters converted to uppercase.


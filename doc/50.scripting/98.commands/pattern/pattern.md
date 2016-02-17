---
title: Pattern Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Automatically or manually create pattern descriptions for models.

---

## clearPatterns <a id="clearpatterns"></a>

_Syntax:_

**void** **clearPatterns** ( )

Delete the pattern description of the current model. It’s a good idea to run this command before adding a pattern definition by hand with calls to newpattern.

For example:


```
clearPatterns();
```


---

## createPatterns <a id="createpatterns"></a>

_Syntax:_

**void** **createPatterns** ( )

Automatically detect and create the pattern description for the current model.

For example:


```
createPatterns();
```


---

## currentPattern <a id="currentpattern"></a>

_Syntax:_

[Pattern](/aten/docs/scripting/variabletypes/pattern) **currentPattern** ( )

[Pattern](/aten/docs/scripting/variabletypes/pattern) **currentPattern** ( **string** _name_ )

[Pattern](/aten/docs/scripting/variabletypes/pattern) **currentPattern** ( **int** _id_ )

Get the named pattern or pattern with given id (if either was specified), returning its reference and setting it to be the current pattern.

For example:


```
Pattern p = currentPattern("liquid");
```


sets the pattern named 'liquid' in the current model to be the current pattern, setting its reference in the variable _p_.


```
Pattern p = currentPattern();
```


returns a reference to the current pattern.

---

## getPattern <a id="getpattern"></a>

_Syntax:_

[Pattern](/aten/docs/scripting/variabletypes/pattern) **getPattern** ( **string** _name_ )

[Pattern](/aten/docs/scripting/variabletypes/pattern) **getPattern** ( **int** _id_ )

Get the named pattern, or pattern with id specified, returning its reference.

For example:


```
Pattern p = getPattern("solid");
```


gets the pattern named "solid" in the current model, setting its reference in the variable _p_.


```
getPattern(3);
```


gets the third pattern in the current model.

---

## listPatterns <a id="listpatterns"></a>

_Syntax:_

**void** **listPatterns** ( )

List the patterns in the current model.

For example:


```
listPatterns();
```


---

## newPattern <a id="newpattern"></a>

_Syntax:_

[Pattern](/aten/docs/scripting/variabletypes/pattern) **newPattern** ( **string** _name_, **int** _nMols_, **int** _atomsPerMol_ )

Add a new pattern node to the current model, spanning _nMols_ molecules of _atomsPerMol_ atoms each, and called _name_. A reference to the new pattern is returned.

For example:


```
Pattern p = newPattern("water", 100, 3);
```


creates a new pattern description of 100 molecules of 3 atoms each named "water" (i.e. 100 water molecules) in the current model, and returns its reference



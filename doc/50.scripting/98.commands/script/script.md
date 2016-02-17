---
title: Script Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Commands to load and run scripts.

---

## listScripts <a id="listscripts"></a>

_Syntax:_

**void** **listScripts** ( )

Lists all loaded scripts.

For example:


```
listScripts();
```


---

## loadScript <a id="loadscript"></a>

_Syntax:_

**void** **loadScript** ( **string** _filename_ )

**void** **loadScript** ( **string** _filename_, **string** _nickname_ )

Loads a script from the _filename_ specified, giving it the optional _nickname_.

For example:


```
loadScript("scripts/liquid-water.txt", "water");
```


loads the script from "scripts/liquid-water.txt"  and gives it the nickname "water".

---

## runScript <a id="runscript"></a>

_Syntax:_

**void** **runScript** ( **string** _name_ )

Executes the specified script.

For example:


```
runScript("water");
```


executes the water script loaded in the previous example.



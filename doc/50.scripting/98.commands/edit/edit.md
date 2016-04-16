---
title: Edit Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Standard editing commands.

---

## copy <a id="copy"></a>

_Syntax:_

**void** **copy** ( )

Copy the current atom selection to the clipboard, ready for pasting.

For example:

```aten
copy();
```

---

## cut <a id="cut"></a>

_Syntax:_

**void** **cut** ( )

Cut the current atom selection to the clipboard.

For example:

```aten
cut();
```

---

## delete <a id="delete"></a>

_Syntax:_

**void** **delete** ( )

Delete the current atom selection.

For example:

```aten
delete();
```

---

## paste <a id="paste"></a>

_Syntax:_

**void** **paste** ( )

Paste the copied atom selection.

For example:

```aten
paste();
```

---

## redo <a id="redo"></a>

_Syntax:_

**void** **redo** ( )

Redo the last ‘undone’ operation.

For example:

```aten
redo();
```

---

## undo <a id="undo"></a>

_Syntax:_

**void** **undo** ( )

Undo the last operation.

For example:

```aten
undo();
```



---
title: Label Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Add and remove atom labels.

---

## clearLabels <a id="clearLabels"></a>

_Syntax:_

**void** **clearLabels** ( )

Remove all atom labels in the current model.

For example:

```aten
clearLabels();
```

---

## label <a id="label"></a>

_Syntax:_

**void** **label** ( **string** _type_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Adds the specified label to each atom in the current selection, or alternatively just to the atom specified. Valid _type_s are as listed for the [**LabelType**](/aten/docs/enums/labeltype) enum.

For example:

```aten
label("element");
```

adds element labels to the current atom selection.

---

## removeLabel <a id="removeLabel"></a>

_Syntax:_

**void** **removeLabel** ( **string** _type_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Remove the specified label (if it exists) from each atom in the current selection, or alternatively just from the atom specified.

For example:

```aten
removeLabel("equiv");
```

removes the forcefield equivalent type label from each atom in the current selection.

---

## removeLabels <a id="removeLabels"></a>

_Syntax:_

**void** **removeLabels** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Remove all labels from all atoms in the current selection, or alternatively just the atom specified.


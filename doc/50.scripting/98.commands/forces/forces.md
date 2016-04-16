---
title: Force Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Calculate forces for models and trajectory frames.

---

## frameForces <a id="frameforces"></a>

_Syntax:_

**void** **frameForces** ( )

Calculate the atomic forces of the current frame of the trajectory associated with the current model.

For example:

```aten
frameForces();
```

---

## modelForces <a id="modelforces"></a>

_Syntax:_

**void** **modelForces** ( )

Calculate the atomic forces of the current model.

For example:

```aten
modelForces();
```

---

## printForces <a id="printforces"></a>

_Syntax:_

**void** **printForces** ( )

Print out the forces of the current model.

For example:

```aten
printForces();
```



---
title: Colourscale Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Define colourscales to colour objects. Each colourscale has a number of points defining colours at specific values - between adjacent points the colour is linearly interpolated. Points in a colourscale are numbered from 1 onwards. There are ten available colourscales, with IDs 1-10. Some of these have specific uses within **Aten**.

The [**Topics**](/aten/docs/topics) section has an entry on [**Colourscales**](/aten/docs/topics/colourscales) explaining them in more detail.

---

## addPoint <a id="addpoint"></a>

_Syntax:_

**void** **addPoint** ( **int** _scaleid_, **double** _value_, **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Adds a new point to the end of the specified colourscale, at the point _value_ and with the RGB\[A\] colour provided (each component of which should be in the range 0.0 to 1.0 inclusive).

For example:

```aten
addPoint(1, 15.0, 0.0, 1.0, 0.0);
```

adds a point to colourscale 1 at a value of 15.0 in a nasty green colour.

---

## clearPoints <a id="clearpoints"></a>

_Syntax:_

**void** **clearPoints** ( **int** _scaleid_ )

Clears all points from the specified colourscale.

For example:

```aten
clearPoints(3);
```

clears all points from the third colourscale.

---

## listScales <a id="listscales"></a>

_Syntax:_

**void** **listScales** ( )

Lists the current types, colours, and ranges of the colourscales

For example:

```aten
listScales();
```

---

## removePoint <a id="removepoint"></a>

_Syntax:_

**void** **removePoint** ( **int** _scaleid_, **int** _pointid_ )

Remove a single point from the selected colourscale.

For example:

```aten
removePoint(1,4);
```

deletes the fourth point from colourscale 1.

---

## scaleName <a id="scalename"></a>

_Syntax:_

**string** **scaleName** ( **int** _scaleid_ )

**string** **scaleName** ( **int** _scaleid_, **string** _newname_ )

Retrieves the name of the colourscale id provided, or sets the name if a new name is provided). The name is displayed next to the gradient bar (if drawn).

For example:

```aten
scaleName(1, "Orientation");
```

renames the first colourscale to ‘Orientation’.

---

## scaleVisible <a id="scalevisible"></a>

_Syntax:_

**void** **scaleVisible** ( **int** _scaleid_, **bool** _visible_ )

Sets whether the gradient bar for the specified colourscale should be drawn in the main view. Default is ‘off’ for all colourscales.

For example:

```aten
scaleVisible(9, "yes");
```

draws the gradient bar for the 9th colourscale in the main view.

---

## setPoint <a id="setpoint"></a>

_Syntax:_

**void** **setPoint** ( **int** _scaleid_, **int** _pointid_, **double** _value_, **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Sets the value and colour of an existing point in the specified colourscale.

For example:

```aten
setPoint(1, 2, -3.3, 1.0, 1.0, 1.0);
```

sets the second point on colourscale 1 to a value of -3.3 and white colour.

---

## setPointColour <a id="setpointcolour"></a>

_Syntax:_

**void** **setPointColour** ( **int** _scaleid_, **int** _pointid_, **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Sets the colour of an existing point in the specified colourscale.

For example:

```aten
setPointColour(5, 1, 0.0, 0.0, 1.0);
```

sets the first point on colourscale 5 to be coloured blue.

---

## setPointValue <a id="setpointvalue"></a>

_Syntax:_

**void** **setPointValue** ( **int** _scaleid_, **int** _pointid_, **double** _value_ )

Sets the value of an existing point in the specified colourscale.

For example:

```aten
setPointValue(4, 3, 0.1);
```

sets the third point of colourscale 4 to a value of 0.1.


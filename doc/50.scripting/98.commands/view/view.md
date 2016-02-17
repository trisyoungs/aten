---
title: View Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Commands to change the current model’s view.

---

## axisRotateView <a id="axisrotateview"></a>

_Syntax:_

**void** **axisRotateView** ( **double** _ax_, **double** _ay_, **double** _az_, **double** _angle_ )

Rotate the current view _angle_ degrees about an axis defined between the supplied point {ax,ay,az} and the origin.

---

## getView <a id="getview"></a>

_Syntax:_

**void** **getView** ( )

Outputs the rotation matrix elements and position vector of the camera for the current model. The list of numbers may be passed directly to the **setView** command to re-create the view exactly.

For example:


```
getView();
```


---

## orthographic <a id="orthographic"></a>

_Syntax:_

**void** **orthographic** ( )

Set the view for all models to be an orthographic projection.

For example:


```
orthographic();
```


---

## perspective <a id="perspective"></a>

_Syntax:_

**void** **perspective** ( )

Set the view for all models to be a perspective projection.

For example:


```
perspective();
```


---

## resetView <a id="resetview"></a>

_Syntax:_

**void** **resetView** ( )

Resets the view rotation and zoom for the current model.

For example:


```
resetView();
```


---

## rotateView <a id="rotateview"></a>

_Syntax:_

**void** **rotateView** ( **double** _rotx_, **double** _roty_ )

Rotates the current view by _rotx_ degrees around the x axis and _roty_ degrees around the y axis.

For example:


```
rotateView(10.0, 0.0);
```


---

## setView <a id="setview"></a>

_Syntax:_

**void** **setView** ( **double** _ax_, **double** _ay_, **double** _az_, **double** _bx_, **double** _by_, **double** _bz_, **double** _cx_, **double** _cy_, **double** _cz_, **double** _x_, **double** _y_, **double** _z_ )

Sets the rotation matrix and position vector of the camera for the current model. The output of **getView** can be passed to **setView** to recreate an existing camera rotation and position.

For example:


```
setView(1, 0, 0 ,0, 0, 1, 0, -1, 0, 0.0, 0.0, -10.0);
```


sets a view with the z axis pointing up, and the y axis normal to the screen (i.e. rotated 90 degrees around the x axis)

---

## speedTest <a id="speedtest"></a>

_Syntax:_

**void** **speedTest** ( **int** _nRenders_ = 100 )

Performs a quick speed test based on rendering of the current model and general view.

For example:


```
speedTest();
```


spins the current model for the default of 100 rendering passes.


```
speedTest(2000);
```


spins the current model for 2000 rendering passes.

---

## translateView <a id="translateview"></a>

_Syntax:_

**void** **translateView** ( **double** _x_, **double** _y_, **double** _z_ )

Translates the camera viewing the current model.

For example:


```
translateView(0.0, 0.0, 1.0);
```


---

## viewAlong <a id="viewalong"></a>

_Syntax:_

**void** **viewAlong** ( **double** _x_, **double** _y_, **double** _z_ )

Sets the current view so that it is along the specified vector.

For example:


```
viewAlong(0, 0, -1);
```


view the current model along the negative z-axis.

---

## viewAlongCell <a id="viewalongcell"></a>

_Syntax:_

**void** **viewAlongCell** ( **double** _x_, **double** _y_, **double** _z_ )

Sets the current view so that is along the specified cell vector.

For example:


```
viewAlongCell(1, 0, 0);
```


view the current model along the cell’s x axis.

---

## zoomView <a id="zoomview"></a>

_Syntax:_

**void** **zoomView** ( **double** _dz_ )

Zooms the view by the specified amount.

For example:


```
zoomView(10.0);
```


moves the camera 10 Å forwards along the z-direction.


```
zoomView(-5);
```


moves the camera 5 Å backwards along the z-direction.



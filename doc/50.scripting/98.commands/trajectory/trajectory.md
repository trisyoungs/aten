---
title: Trajectory Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Open and associate trajectory files to models, and select frames to display/edit from the current trajectory.

---

## addFrame <a id="addframe"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **addFrame** ( )

[**Model**](/aten/docs/scripting/variabletypes/model) **addFrame** ( **string** _title_ )

Append a new trajectory frame to the current model’s trajectory. The reference to the new frame is returned.

For example:


```
Model m = addFrame("new config");
```


---

## clearTrajectory <a id="cleartrajectory"></a>

_Syntax:_

**void** **clearTrajectory** ( )

Clear any associated trajectory and frame data in the current model.

For example:


```
clearTrajectory();
```


---

## firstFrame <a id="firstframe"></a>

_Syntax:_

**void** **firstFrame** ( )

Select the first frame from trajectory of current model.

For example:


```
firstFrame();
```


---

## lastFrame <a id="lastframe"></a>

_Syntax:_

**void** **lastFrame** ( )

Select last frame in trajectory of current model.

For example:


```
lastFrame();
```


---

## loadTrajectory <a id="loadtrajectory"></a>

_Syntax:_

**int** **loadTrajectory** ( **string** _filename_ )

Associate trajectory in _filename_ with the current model. An integer value of '1' is returned if the association was successful, or '0' otherwise.

For example:


```
int success = loadTrajectory("/home/foo/md/water.HISf");
```


opens and associated the formatted DL_POLY trajectory file "water.HISf" with the current model.

---

## nextFrame <a id="nextframe"></a>

_Syntax:_

**void** **nextFrame** ( )

Select next frame from trajectory of current model.

For example:


```
nextFrame();
```


---

## prevFrame <a id="prevframe"></a>

_Syntax:_

**void** **prevFrame** ( )

Select the previous frame from the trajectory of the current model.

For example:


```
prevFrame();
```


---

## seekFrame <a id="seekframe"></a>

_Syntax:_

**void** **seekFrame** ( **int** _frameno_ )

Seeks to the frame number specified.

For example:


```
seekFrame(10);
```


seeks to the 10th frame of the current trajectory.



---
title: Model Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Model creation and management.

---

## createAtoms <a id="createatoms"></a>

_Syntax:_

**void** **createAtoms** ( )

Can be run when importing trajectory frames. Creates enough atoms in the current trajectory frame to match the parent model.

For example:


```
createAtoms();
```


---

## currentModel <a id="currentmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **currentModel** ( )

[**Model**](/aten/docs/scripting/variabletypes/model) **currentModel** ( **int** _id_ )

[**Model**](/aten/docs/scripting/variabletypes/model) **currentModel** ( **string** _name_ )

[**Model**](/aten/docs/scripting/variabletypes/model) **currentmodel** ( [**Model**](/aten/docs/scripting/variabletypes/model) _m_ )

Returns a reference to the current model (if no argument is given) or selects the supplied model and makes it the current model. The model may be selected either by name, by its integer position in the list of loaded models (i.e. 1 to N), or a model-type variable containing a valid model reference may be passed.

For example:


```
currentModel(4);
```


selects the fourth loaded model.


```
currentModel("Protein coordinates");
```


selects the model named "Protein coordinates" (provided it exists).

```
Model m1, m2;
m1 = newModel("Test model 1");
m2 = newModel("Test model 2");
currentModel(m1);
```

creates two models, storing references to each, and then re-selects the first one and makes it the current target again.

---

## deleteModel <a id="deletemodel"></a>

_Syntax:_

**void** **deleteModel** ( **int** _id_ )

**void** **deleteModel** ( **string** _name_ )

**void** **deleteModel** ( [**Model**](/aten/docs/scripting/variabletypes/model) _m_ )

Deletes the current model (if no argument is given) or the supplied model.

---

## finaliseModel <a id="finalisemodel"></a>

_Syntax:_

**void** **finaliseModel** ( )

Performs various internal tasks after a model has been fully created within a filter. Should be called after all operations on each created model have finished.

For example:


```
finaliseModel();
```


---

## firstModel <a id="firstmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **firstModel** ( )

Makes the first loaded / created model the current model, and returns a reference to it.

For example:


```
firstModel();
```


---

## getModel <a id="getmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **getModel** ( **int** _id_ )

[**Model**](/aten/docs/scripting/variabletypes/model) **getModel** ( **string** _name_ )

[**Model**](/aten/docs/scripting/variabletypes/model) **getModel** ( [**Model**](/aten/docs/scripting/variabletypes/model) _m_ )

Returns a reference to the requested model, but unlike [**currentModel**](/aten/docs/scripting/variabletypes/model#currentmodel) does not make it the current model.

For example:


```
Model alpha = getModel("alpha2");
```


grabs a reference to the model named "alpha2".


```
Model m = getModel(5);
```


gets a reference to the fifth loaded model.

---

## info <a id="info"></a>

_Syntax:_

**void** **info** ( )

Print out information on the current model and its atoms.

For example:


```
info();
```


---

## lastModel <a id="lastmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **lastModel** ( )

Makes the last loaded / created model the current model, and returns a reference to it.

For example:


```
Model final = lastModel();
```


---

## listModels <a id="listmodels"></a>

_Syntax:_

**void** **listModels** ( )

Lists all models currently available.

For example:


```
listModels();
```


---

## loadModel <a id="loadmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **loadModel** ( **string** _filename_ )

[**Model**](/aten/docs/scripting/variabletypes/model) **loadModel** ( **string** _filename_, **string** _filter_ = (auto) )

Load model(s) from the _filename_ provided, autodetecting the format of the file. If the optional _filter_ argument is present, then the file is forcibly loaded using the filter with the corresponding nickname. The last loaded model becomes the current model, to which a reference is returned.

For example:


```
Model m = loadModel("/home/foo/coords/test.xyz");
```


loads a model called 'test.xyz', returning a reference to it.


```
Model m = loadModel("/home/foo/coords/testfile", "xyz");
```


forces loading of the model _testfile_ as an **xyz** file.

---

## logInfo <a id="loginfo"></a>

_Syntax:_

**void** **logInfo** ( )

Prints out log information for the current model.

---

## modelTemplate <a id="modeltemplate"></a>

_Syntax:_

**void** **modelTemplate** ( )

Can only be run when importing trajectory frames. Templates the atoms in the trajectory’s parent model by creating an equal number of atoms in the target trajectory frame, and copying the element and style data. Positions, forces, and velocities are not copied from the parent model atoms.

For example:


```
modelTemplate();
```


---

## newModel <a id="newmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **newModel** ( **string** _name_ )

Create a new model called _name_ which becomes the current model, and return a reference to it.

For example:


```
newModel("emptymodel");
```


creates a new, empty model called 'emptymodel' and makes it current.


```
Model c12 = newModel("dodecane");
```


creates a new, empty model called 'dodecane', makes it current, and stores a reference to it in the variable _c12_.

---

## nextModel <a id="nextmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **nextModel** ( )

Skips to the next loaded model, makes it current, and returns a reference to it.

For example:


```
Model next = nextModel();
```


---

## parentModel <a id="parentmodel"></a>

_Syntax:_

**void** **parentModel** ( )

Makes the parent model of the current trajectory frame the current model.

For example:


```
parentModel();
```


---

## prevModel <a id="prevmodel"></a>

_Syntax:_

[**Model**](/aten/docs/scripting/variabletypes/model) **prevModel** ( )

Skips to the previous loaded model, makes it current, and returns a reference to it.

For example:


```
model prev = prevmodel();
```


---

## saveModel <a id="savemodel"></a>

_Syntax:_

**int** **saveModel** ( **string** _format_, **string** _filename_ )

Save the current model in the _format_ given (which should correspond to a model export Filter nickname) to the _filename_ specified. If the save was successful, an integer value of '1' is returned, otherwise '0'.

For example:


```
int success = saveModel("xyz", "/home/foo/newcoords/test.config");
```


saves the current model in xyz format to the filename given.

---

## saveSelection <a id="saveselection"></a>

_Syntax:_

**int** **saveSelection** ( **string** _format_, **string** _filename_ )

Save the atom selection in the current model in the _format_ given (which should correspond to a model export Filter nickname) to the _filename_ specified. If the save was successful, an integer value of 1 is returned, otherwise 0. Unit cell information is also saved, if the model has any.

---

## setName <a id="setname"></a>

_Syntax:_

**void** **setName** ( **string** _name_ )

Sets the name of the current model.

For example:


```
setName("panther");
```


gives the current model the cool-sounding name of 'panther'! Ahem.

---

## showAll <a id="showall"></a>

_Syntax:_

**void** **showAll** ( )

Makes any previously-hidden atoms in the model visible again.



---
title: Aten
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The ‘master’ class type, `Aten`, is there to provide access to various other structures such as the list of loaded models, preferences, element data etc. It is available at all times from any command, script, or filter, through a variable called _aten_. Note that it is not possible to declare new variables of type `Aten`.

| Member | Type | RW | Description |
|--------|------|----|-------------|
| elements | [**Element**]\[\](/aten/docs/scripting/variabletypes/element)  | | Array of element data (masses, symbols, names, etc.) |
 </row>
| frame | [**Model**](/aten/docs/scripting/variabletypes/model) | | The current model being displayed and the focus of editing, i.e. the current trajectory frame or, if no trajectory is associated to the current model then the current selected model is returned |
 </row>
| model | [**Model**](/aten/docs/scripting/variabletypes/model) | | The current model selected (this is the parent model of a trajectory if a frame is currently being displayed) |
 </row>
| models | [**Model**]\[\](/aten/docs/scripting/variabletypes/model) | | Array of all loaded models currently available |
 </row>
| nElements | **int** | | Number of chemical elements defined in the elements array |
 </row>
| nModels | **int** | | Number of models (parent models, i.e. not including trajectory frames) currently loaded |
 </row>
| prefs | [**Prefs**](/aten/docs/scripting/variabletypes/prefs) | | Program preferences |
 </row>

# Aten Type Member Functions

## convertEnergy <a id="convertenergy"></a>

_Syntax:_

**double** **convertEnergy** ( **double** _value_, **string** _oldunits_ )

Convert the supplied energy _value_ from _oldunits_ to the current, internal unit of energy in use by Aten. See also Energy Units (Section 16.8) and the relevant [**Prefs**](/aten/docs/scripting/variabletypes/prefs) accessor.

---

## findElement <a id="findelement"></a>

_Syntax:_

[**Element**](/aten/docs/scripting/variabletypes/element) **findElement** ( **string** _name_ )

Convert the _name_ specified into an [**Element**](/aten/docs/scripting/variabletypes/element), according to the current ZMapping Type (Section 16.17).


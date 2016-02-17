---
title: Dialog
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Dialog**](/aten/docs/scripting/variabletypes/dialog) is a parent window control used to contain one or more Widgets designed to allow setting of options etc.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| title | **string** | • | Text displayed in the Dialog’s titlebar |
| verticalFill | **int** | • | If TRUE then child widgets without specific coordinates will be created in a vertical stack. Otherwise child widgets will be added in a horizontal line |

# Dialog Type Functions

The [**Dialog**](/aten/docs/scripting/variabletypes/dialog) is itself a [**Widget**](/aten/docs/scripting/variabletypes/widget) – for widget creation functions refer to Section 8.2.45 (the [**Widget**](/aten/docs/scripting/variabletypes/widget) variable).

---

## asDouble <a id="asdouble"></a>

_Syntax:_

**double** **asDouble** ( **string** _name_ )

Return the value of the named widget as a double.

---

## asInteger <a id="asinteger"></a>

_Syntax:_

**int** **asInteger** ( **string** _name_ )

Return the value of the named widget as an integer.

---

## asString <a id="asstring"></a>

_Syntax:_

**string** **asString** ( **string** _name_ )

Return the value of the named widget as a string.

---

## asVector <a id="asvector"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **asVector** ( **string** _name1_, **string** _name2_, **string** _name3_ )

Return the values of the three named widgets as a Vector.

---

## isInteger <a id="isinteger"></a>

_Syntax:_

**int** **isInteger** ( **string** _name_, **int** _value_ )

Returns TRUE (1) if the named widget’s current value matches that provided, or FALSE (0) otherwise.

---

## isRange <a id="isrange"></a>

_Syntax:_

**int** **isRange** ( **string** _name_, **int** _minValue_, **int** _maxValue_ )

Returns TRUE (1) if the named widget’s current value is within the range provided, or FALSE (0) otherwise.

---

## isString <a id="isstring"></a>

_Syntax:_

**int** **isString** ( **string** _name_, **string** _value_ )

Returns TRUE (1) if the named widget’s current string value matches that provided, or FALSE (0) otherwise.

---

## show <a id="show"></a>

_Syntax:_

**int** **show** ( )

Shows (executes) the Dialog, blocking all input to Aten until it is accepted (OK) or rejected (Cancel).

---

## widget <a id="widget"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **widget** ( **string** _name_ )

Search for and return the named Widget.



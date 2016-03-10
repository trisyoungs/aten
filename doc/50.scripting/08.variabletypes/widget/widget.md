---
title: Widget
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Widget**](/aten/docs/scripting/variabletypes/widget) is a control present in a GUI dialog created by the user, and provides functions to create further widgets (if the control supports doing so) and create dependency paths between itself and other widgets.

| Member | Type | RW | Description |
|--------|------|----|-------------|
| enabled | **int** | • | Whether the widget is currently enabled (i.e. whether the user can interact with it) |
| verticalFill | **int** | • | If TRUE then child widgets without specific coordinates will be created in a vertical stack. Otherwise child widgets will be added in a horizontal line |
| visible | **int** | • | Whether the widget is visible in the GUI |

## Widget Type Functions

Most functions to create child widgets accept four optional integer arguments determining the position and size of the widget in the parent. These are:

            _l_           The coordinates of the left edge of the control
            _t_           The coordinates of the top edge of the control
            _xw_        The additional width to add to the control (default = 0)
            _xh_        The additional height to add to the control (default = 0)

If neither of the first two values are given (or both are set to -1) then the child widget’s position is determined automatically according to the current fill policy (see the _fillVertical_ property). Otherwise, the widget will be positioned absolutely according to the l and t values (note that overlapping widgets is possible, and no checks are made to this end).

---

### addButton <a id="addbutton"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addButton** ( **string** name, **string** label, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new button widget to this widget.

---

### addCheck <a id="addcheck"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addCheck** ( **string** name, **string** label, **int** state, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new checkbox widget to this widget.

---

### addCombo <a id="addcombo"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addCombo** ( **string** name, **string** label, **string** items, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new combo widget (drop-down list) to this widget.

---

### addDoubleSpin <a id="adddoublespin"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addDoubleSpin** ( **string** name, **string** label, **double** min, **double** max, **double** step, **double** value, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new doublespin widget to this widget.

---

### addEdit <a id="addedit"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addEdit** ( **string** name, **string** label, **string** text, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new single-line edit box to this widget.

---

### addFrame <a id="addframe"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addFrame** ( **string** name, **int** l = (auto), **int** _t_ = (auto), **int** xw = 0, **int** xh = 0 )

Add a simple frame to this widget.

---

### addGroup <a id="addgroup"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addGroup** ( **string** name, **string** label, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new groupbox to this widget.

---

### addIntegerSpin <a id="addintegerspin"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addIntegerSpin** ( **string** name, **string** label, **int** min, **int** max, **int** step, **int** value, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new integer spin widget to this widget.

---

### addLabel <a id="addlabel"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addLabel** ( **string** name, **string** text, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a simple label to this widget.

---

### addPage <a id="addpage"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addPage** ( **string** _name_, **string** _label_ )

Add a new page to this widget, which must be either a tab widget or a stack widget.

---

### addRadioButton <a id="addradiobutton"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addRadioButton** ( **string** name, **string** label, **string** group, **int** state, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new radiobutton to this widget.

---

### addRadioGroup <a id="addradiogroup"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addRadioGroup** ( **string** _name_ )

Add a new (invisible) radiobutton group to the widget (but whichi is owned by the parent Dialog).

---

### addSpacer <a id="addspacer"></a>

_Syntax:_

**void** **addSpacer** ( [var]int [/var] expandH, **int** expandV, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )



---

### addStack <a id="addstack"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addStack** ( **string** name, **int** l = (auto), **int** t = (auto), **int** xw = 0, **int** xh = 0 )

Add a new widget stack to this widget. Individual tabs should be added with the addPage function, called from the new stack widget.

---

### addTabs <a id="addtabs"></a>

_Syntax:_

[**Widget**](/aten/docs/scripting/variabletypes/widget) **addTabs** ( **string** name, **int** l = (auto), **int** _t_ = (auto), **int** xw = 0, **int** xh = 0 )

Add a new tabbed widget to this widget. Individual tabs should be added with the addPage function, called from the new tab widget.

---

### onDouble <a id="ondouble"></a>

_Syntax:_

**void** **onDouble** ( **double** minval, **double** maxval, **string** event, **string** [**Widget**](/aten/docs/scripting/variabletypes/widget), **string** property, **double|int|string** value = (auto) ... )

Define an action to be performed when this widget’s double value is within a certain range.

---

### onInteger <a id="oninteger"></a>

_Syntax:_

**void** **onInteger** ( **int** _minval_, **int** _maxval_, **string** _event_, **string** [**Widget**](/aten/docs/scripting/variabletypes/widget), **string** _property_, **double|int|string** _value_ = (auto) )

Define an action to be performed when this widget’s integer value is within a certain range.

---

### onString <a id="onstring"></a>

_Syntax:_

**void** **onString** ( **string** _text_, **string** _event_, **string** _widget_, **string** _property_, _double|int|string_ _value_ = (auto) }

Define an action to be performed when this widget takes on a specific string value.

---

### setProperty <a id="setproperty"></a>

_Syntax:_

**void** **setProperty** ( **string** _property_, **string|int|double** _value_ )

Set the named widget _property_ to the given _value_. 



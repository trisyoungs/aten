---
title: Messaging Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Output messages from command lists / filters. All commands work like the C printf() command, and accept the same fundamental format specifiers. All output from these messaging commands is directed to either the GUI message box or stdout on the command line.

---

## createDialog <a id="createdialog"></a>

_Syntax:_

[Dialog](/aten/docs/scripting/variabletypes/dialog) **createDialog** ( **string** _title_ = (none) )

Create a new, temporary _Dialog_ with titlebar text _title_ (if provided).

For example:

```
Dialog ui = createDialog("Choose a Number");
ui.addIntegerSpin("chooser", "Choice", 1, 100, 1, 25);

if (!ui.show()) error("Dialog Canceled. ");
else
{
      int n = ui.asInteger("chooser");
      printf("You chose %i\n", n);
}
```

---

## defaultDialog <a id="defaultdialog"></a>

_Syntax:_

[Dialog](/aten/docs/scripting/variabletypes/dialog) **defaultDialog** ( **string** _title_ = (none) )

Returns the default [Dialog](/aten/docs/scripting/variabletypes/dialog) structure for this filter / script / function, setting the window titlebar text to _title_, if it is provided.

For example:

```
Dialog ui = defaultDialog();
if (!ui.show()) error("Dialog Canceled. ");
```

---

## error <a id="error"></a>

_Syntax:_

**void** **error** ( **string** _format_, ... )

Print a message to screen and immediately exit the current command structure / filter.

For example:

```
int err=23;
error("Filter failed badly - error = %i.\n", err);
```

notifies the user that something bad probably happened  and promptly exits.

---

## printf <a id="printf"></a>

_Syntax:_

**void** **printf** ( **string** _format_, ... )

Standard printing command.

For example:


```
printf("Loading data...\n");
```


prints the string "Loading data..." to the screen.


```
printf("Number of atoms = %i\n", natoms);
```


prints the contents of the variable _natoms_ to the screen.

---

## showDefaultDialog <a id="showdefaultdialog"></a>

_Syntax:_

**int** **showDefaultDialog** ( **string** _title_ = (none) )

Shows (executes) the default [Dialog](/aten/docs/scripting/variabletypes/dialog) structure for this filter / script / function, setting the window titlebar text to _title_, if it is provided.

For example:


```
if (!showDefaultDialog()) error("Dialog Canceled. ");
```


---

## verbose <a id="verbose"></a>

_Syntax:_

**void** **verbose** ( **string** _format_, ... )

Prints a message, but only when verbose output is enabled (with the -v command-line switch).

For example:


```
verbose("Extra information for you.\n");
```




---
title: Option Dialogs
brief: Adding option dialogs to filters
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---

## Overview

Occasionally it is useful to be able to provide more control over the actions of a script or filter. For instance, in the latter case it is often desirable to provide a set of options which control the exact data that is written to the file on export or, conversely, to define which data is read in on import. User interaction can be achieved by the use of custom dialogs which display any number of standard GUI controls in a dialog window, allowing such options to be set. These dialogs can be called at any point in the execution of a script or filter.

## Default and Temporary Dialogs

Each filter, script, or user-defined function within has a _default dialog_ associated with it, and can be retrieved through the defaultDialog XXX command. This dialog always exists, and when repeatedly shown will retain the state of the GUI controls from the last execution. Alternatively a new, temporary dialog can be created at any point in the code by calling the createDialog function. This dialog is then populated with widgets, executed, the necessary data taken from it, and then destroyed.

**Default Dialog** – Allows for a persistent options dialog in which the states of child widgets are retained between shows.

**Temporary Dialogs** – Usually offer a more simplistic set of widgets whose states are not remembered between shows (obviously, because the Dialog is destroyed after use)

Default dialogs are commonly used in export Filters in order to allow a complete set of options to be set which define many control variables, and which it is desirable to remember for the next time the user saves a file in the same format.

For example, we we can set a string variable with one of several possible values using a combo box control in the GUI. When saving a file in a new format (or by selecting **Export Options** from the **File** menu) a dialog is shown in the GUI which contains all of the defined controls in the relevant filter, from which options can be set before the file is written. This would be written as follows in the filter file:
 TODO Horribly out of date!

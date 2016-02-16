---
title: Overview
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Filters determine the model/trajectory, grid/surface, and forcefield expression formats that **Aten** can read and write. They are essentially small programs written in **Aten**'s internal command language (based syntactically on C/C++) and are stored as plain text files. These files are parsed and 'compiled' when **Aten** starts up. This has several advantages:

+ Users may add support for their own particular formats at will.
+ No recompilation of **Aten** is necessary when adding new filters or adjusting old ones
+ Potentially any file format can be supported, even binary formats

With this flexibility, of course, come some modest disadvantages:

+ Speed - the C-style code contained within filters is, strictly speaking, interpreted, it is by no means as fast as properly compiled code
+ File formats that need particularly awkward operations requiring a more 'complete' C language may be difficult to implement

These two points aside, though, filters make **Aten** a powerful and flexible tool, adaptable to conform to many different program/code input and output formats.

As mentioned, the programming language used by filters is essentially a subset of C, implemented with the same syntax and style (see the command language overview in Chapter 9 for a description), but includes several hundred custom commands to control **Aten** in order to build up atoms in models, access data etc. So, if you already know C or C++, writing a filter should be a breeze. If you don't, it's not too difficult to pick up, and there are plenty of filters already written to use as worked examples.

When a filter is called in order to write out data, no references to any of the current (i.e. displayed or selected) data are sent directly to the filter itself. Instead, this must be probed by using the [**Aten**](/aten/docs/scripting/variabletypes/aten) master reference available to all scripts, commands and filters. Within [**Aten**](/aten/docs/scripting/variabletypes/aten) the currently displayed model may be deduced, as well as the current frame (if a trajectory is associated). In most cases for model export filters, the path 'aten.frame' should be used to determine the model data that should be written.

## What's in a Filter File?

A filter is a plain text file containing one or more C-style programs that permit the input or output of data in a specific format. For example, a purely model-oriented filter file may contain two filters, one to read in files of the specified format, and one to write the data out again. Each individual filter is given a short nickname, a shell-style glob, and possibly several other bits of data that allow files to be recognised (if the file extensions defined for it are not enough).

Different filters that recognise the same file type may be provided if necessary, each performing a slightly different set of import or export commands (if it is not convenient to do so within a single filter), and all will appear in the drop-down list of filters in file dialogs within the program. Note that in batch, command-line, or scripting mode, filters are either selected automatically based on the filename, extension, or contents, or picked by matching only the associated nickname. In the former case, the first filter that matches the extension is used.



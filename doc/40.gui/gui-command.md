---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---


# Command Window

The command window allows commands or sequences of commands to be run and stored for later use, allows management and execution of scripts, and provides access to searchable command help.

<figure>
  <image>img/window_command_prompt.png</image>
  <caption>Command Window – Prompt page</caption>
</figure>

Any command or compound command can be entered in the bottom editbox and will be executed immediately. This command will then be added to the list above, and can be single-clicked to return it to the editbox for tweaking or re-editing, or double-clicked to execute it again. The list of stored commands is saved when **Aten** exits, and loaded back in when restarted.

<figure>
  <image>img/window_command_interactive.png</image>
  <caption>Command Window – Interactive page</caption>
</figure>

For when single-line commands are too inflexible, the interactive page mimics a more ‘proper’ console-like environment. Here, variables can be defined in one instance and referred back to and manipulated in others, much like a normal shell. A list of variables currently defined in the local scope is shown in the uppermost part of the window.

<figure>
  <image>img/window_command_scripts.png</image>
  <caption>Command Window – Scripts page</caption>
</figure>

Scripts can be loaded in from here and executed at will by double-clicking individual scripts in the list or selecting multiple scripts and clicking the **Run Selected** button. Any script files loaded in this way are remembered when **Aten** exits and are loaded back in when restarted. All scripts can be reloaded from disk (if, for example, changes have been made to one or more files after they were loaded in) by clicking the **Reload All** button.

<figure>
  <image>img/window_command_help.png</image>
  <caption>Command Window – Command Help page</caption>
</figure>

All of **Aten**’s commands are listed in the panel in the upper half of the page, while the syntax and description of any command selected in the list is displayed in the lower half.  The list can be searched by typing in a partial name in the **Search** box at the top.



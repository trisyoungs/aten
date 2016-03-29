---
title: Filter Options
brief: Providing options to filters to allow user control
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

When writing data, in many cases all the information that the filter wants to write is contained within the current model, for example when outputting simple file formats such as  or **Aten**'s own  format. In other cases ther may be additional data for which would be nice to have some control over, and which lays beyond atoms and bondes. The best example is probably the input formats for nearly all _ab initio_ codes which contain (as well as the atomic coordinates) statements and additional data necessary to control the running of the code itself. It is not a problem to write out static lines of control commands from the output filter, but it would of course also be nice to be able to tailor this output from within the GUI (or from the command-line). This can be achieved by assigning values to variables in the filter through the use of Custom Dialogs (see Section 8.3).

TODO



---
title: OutputType
brief: "Program output types (debugging / messaging)"
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Valid output types (or debug modes) are as follows:
 
| Value | Description |
|-------|-------------|
| **all** | Enable output of all types listed in this table |
| **calls** | Print out entrances and exits to most subroutines to enable quick tracing of crash locations |
| **commands** | Trace execution of commands in command lists (e.g. filters) and print information on variable access paths |
| **gl** | Debug OpenGL calls and graphics capabilities as best as is possible |
| **parse** | Debug file-reading and argument parsing routines |
| **typing** | Print (lots of) information regarding setting and matching of atom type descriptions |
| **verbose** | Enable a little extra output (but not much) |


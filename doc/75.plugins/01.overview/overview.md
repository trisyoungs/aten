---
title: Overview
brief: Overview of plugins - what they are, and what they do
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

As of v2.0. **Aten** utilises a plugin system to provide all of it's import / export capabilities, replacing the [filters](/aten/doc/filters) system used in previous versions. The intention behing both systems is very similar - while writing a plugin requires marginally more work than writing a filter, it is more efficient, better integrated with the rest of **Aten**, and benefits from the ability to use the full C++ language set (whereas filters were always restricted to the extent of the [command language](/aten/doc/scripting).


---
title: ReadOption
brief: "Text reading control options"
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

These options determine how general parsing of plain text files proceeds, as well as controlling delimited argument parsing.
 
| Value | Description |
|-------|-------------|
| **noescapes** | Treat backslash as a normal character |
| **normalcommas** | Treate commas as normal characters rather than delimiters |
| **skipblanks** | Blank lines (or those containing comments) are automatically skipped |
| **stripbrackets** | Normal parentheses are automatically stripped from the input file |
| **stripcomments** | Remove comments from file (text starting with ‘//’ or ‘#’) |
| **usecurlies** | Data within curly brackets will be parsed as a single argument  |
| **usequotes** | Phrases enclosed in quotes will be parsed as a single argument |

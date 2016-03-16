---
title: General Style
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Keywords, variables, and function names are case sensitive, so ‘for’ is different from ‘For’. Individual commands must be separated by a semicolon ‘;’, and newlines are optional. For example:

```
int i; printf("Hello there.\n"); i = 5;
```

...and...

```
int i;
printf("Hello there.\n");
i = 5;
```

...are equivalent. Whitespace characters (spaces and tabs) are ignored and may be present in any number and in any position in the code.

Individual lines or parts of them may be commented out. The presence of either the hash symbol (`#`) or a double forward slash (`//`) in a line means ‘ignore rest of line’. Note that the '`/*...*/`' commenting style is not supported.


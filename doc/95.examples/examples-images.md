---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---


# Example - Generating Images Without the GUI (CLI)

Sometimes it's useful to quickly generate images for a system from the command line, either because the system is prepared from the command line, or because generating hundreds of images through the GUI is insanely repetetive.  To quickly save a picture of a system in it's ‘standard’ view orientation (i.s.  as you would see it if you loaded it into the GUI) you can do the following:

```
aten data/test/cellulose.cif -c 'saveBitmap("png", "cellulose.png"); quit();'
```

Doing this will load the cellulose model, save an image of it, and then quit without ever starting the GUI. By default, the size of a saved image is 800x600, but optional arguments to the [a],command-image#saveBitmap[/a] command allow this to be set explicitly.



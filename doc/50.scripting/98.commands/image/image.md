---
title: Image Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Save bitmap and vector images of the current view. The GUI is not required in order to save images – using these commands from the command-line, for example, works just as well (models still have a current view, even without the GUI, and can be rotated, translated etc. just as if they were in the GUI).

---

## saveBitmap <a id="savebitmap"></a>

_Syntax:_

**void** **saveBitmap** ( **string** _format_, **string** _filename_ )

**void** **saveBitmap** ( **string** _format_, **string** _filename_, **int** _width_, **int** _height_ )

**void** **saveBitmap** ( **string** _format_, **string** _filename_, **int** _width_, **int** _height_, **int** _quality_ )

Saves the current view as a bitmap image. Allowable values for _format_ are:

<table>
  <title>Bitmap formats</title>
 <header>
  <column>Format</column>
  <column>Description</column>
 </header>
 <row>
  <column>_bmp_</column>
  <column>Windows Bitmap</column>
 </row>
 <row>
  <column>_jpg_</column>
  <column>Joint Photographic Experts Group </column>
 </row>
 <row>
  <column>_png_</column>
  <column>Portable Network Graphics</column>
 </row>
 <row>
  <column>_ppm_</column>
  <column>Portable Pixmap</column>
 </row>
 <row>
  <column>_xbm_</column>
  <column>X11 Bitmap</column>
 </row>
 <row>
  <column>_xpm_</column>
  <column>X11 Pixmap</column>
 </row>
</table>

If _width_ and _height_ are not specified the current dimensions of the view are used (800x600 if no GUI is present). The _quality_ option determines the compression used on saved images, affecting, for example, the quality and size of jpegs and pngs, and should be an integer between 1 and 100 (with 100 being the best compression).

For example:

```aten
saveBitmap("bmp", "test.bmp");
```

saves the current view to a file 'test.bmp'.

```aten
saveBitmap("png", "big.png", 5000, 5000, 10);
```

saves an enormous, highly uncompressed png.

---

## saveMovie <a id="savemovie"></a>

_Syntax:_

**void** **saveMovie** ( **string** _filename_, **string** _format_, **int** _width_ = [value]-1[/value] **int** _height_ = [value]-1[/value] **int** _quality_ = [value]-1[/value], **int** _firstFrame_ = 1, **int** _lastFrame_ = (auto), **int** _interval_ = 1 )

Saves a movie of the trajectory associated to the current model. Note that a valid movie encoder must be installed (such as _mencoder_) and arguments must be set in the program preferences (see the _Prefs_ variable). Aten will first save a series of png images of the width, height and quality specified to the temporary directory (also specified in the preferences) before passing them all to the provided encoder command. The encoder arguments should contain both the text ‘FILES’ and ‘OUTPUT’ – when it comes to running the commane, Aten will substitute ‘FILES’ for a wildcard list of image files, and ‘OUTPUT’ for the target movie filename. Note that the format of the output movie is entirely guided by the options passed to the encoder command.

As with the **saveBitmap** command, if _width_ and _height_ are not specified the current dimensions of the view are used (800x600 if no GUI is present), and the _quality_ option determines the compression used on saved images. The other arguments are self-explanatory - _firstframe_ and _lastframe_ give the range of trajectory frames which will be saved, while the _interval_ value determines the 'stride' between frames (i.e. 1 for every frame, 2 for every other frame, 10 for every tenth frame etc.).

For example:

```aten
saveMovie("traj.mpg", 1024, 1024, -1, 50, 1000, 50);
```

saves a movie called "traj.mpg" with size 1024x1024, beginning at frame 50 and writing every 50th frame until frame 1000.


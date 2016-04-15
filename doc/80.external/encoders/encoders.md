---
title: Encoders
brief: Using external movie-generation tools with <strong>Aten</strong>
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

**Aten** can make use of external encoders in order to generate films of vibrations, view rotations, and trajectories. Essentially, **Aten** handles the control of writing the frame images that will comprise the final film, and then calls a set of predefined external commands to perform the actual encoding.

**Aten** looks for an encoder definition file (which lives in the `external` directory of the [main `data` directory](/aten/docs/installation/locations) which contains one or more definitions of external programs to be run in an encoding process. An individual encoder definition may contain multiple steps, each calling a different encoder executable if necessary. The `mencoder/MP4` encoder definition looks like this:

```
# mencoder two-pass outputting ISO-standard mp4
Name 'mencoder / ISO-mp4'
Nickname 'menc-mp4'
# -- First pass
CommandName 'First Pass'
Command mencoder
CommandSearchPaths /usr/bin,/usr/local/bin
CommandArguments '-ovc lavc -lavcopts vcodec=mpeg4:vpass=1:vbitrate=2343750:mbd=2:keyint=132:vqblur=1.0:cmp=2:subcmp=2:dia=2:o=mpv_flags=+mv0:last_pred=3 -mf type=FRAMEFORMAT:fps=FRAMESPERSECOND -nosound -o /dev/null mf://@FRAMESFILE'
# -- Second pass
CommandName 'Second Pass'
Command mencoder
CommandSearchPaths /usr/bin,/usr/local/bin
CommandArguments '-ovc lavc -lavcopts vcodec=mpeg4:vpass=2:vbitrate=2343750:mbd=2:keyint=132:vqblur=1.0:cmp=2:subcmp=2:dia=2:o=mpv_flags=+mv0:last_pred=3 -mf type=FRAMEFORMAT:fps=FRAMESPERSECOND -nosound -o OUTPUTFILE mf://@FRAMESFILE'
```

The example shown above contains all of the keywords allowable in an encoder definition. They are as follows:

## Basic Keywords

### Name

Defines a unique, long name for the encoder, which will be displayed in the relevant part of the [**Export Film**](/aten/docs/gui/exportfilm) dialog.

### Nickname

Defines a short, easily-typed (no spaces) nickname for the encoder.

## Command Keywords

After the basic naming of the encoder, a sequence of commands (a 'step' in the encoding process) can be specified by the `Command*` keywords. As many command steps as are necessary may be defined, and will be run in the order they appear in the encoder definition.

### CommandName

Essentially begins the definition of a command step in the encoder, with the provided name.

### Command

Specifies the executable to run in the step. This can be a fully qualified path to the executable, but it is possible to provide search locations with the `CommandSearchPaths` keyword.

### CommandSearchPaths

Specifies one or more (separated by commas) paths to search in for the defined executable, if the executable is not found in the system paths.

### CommandArguments

This specifies the rest of the command that will be run in the form of the command-line arguments to be passed to the `Command` already specified. There a a handful of recognised 'placeholder' strings (see the next section) which **Aten** will substitute for more meaningful values before the command is run.

## Placeholders

Placeholder strings allow pertinent bits of information to be passed from **Aten** to the encoder executable(s) in a definition. If they are present in the `CommandArguments` definition for a step they will be removed and replaced with the information they represent before the encoder step is executed.

### @FPS@

Integer frames-per-second value.

### @FRAMESFILE@

Name of the file (including full path) of a text file listing each individual image file to be used in construction of the final movie.

### @FRAMEBASENAME@

The basename (i.e. first, common part) of each image file to be used in construction of the final movie.

### @OUTPUTFILE@

Filename of the movie file to be created.

### @FRAMEFORMAT@

File extension of the image files, denoting their format.

### @FRAMEWIDTH@

Width, in pixels, of the image files / movie.

### @FRAMEHEIGHT@

Height, in pixels, of the image files / movie.

### @FRAMESIZE@

Size of the image files / movie, in the format `wwwxhhh`, e.g. `800x600`, `1024x400` etc.


---
title: Common Problems
brief: Common problems encountered when compiling
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---

What follows is a sort of mini FAQ detailing some commonly-encountered problems and issues when compiling **Aten**.

## Configuration Errors <a id="configuration"></a>

### [ALL] ./autogen.sh: line 35: libtoolize: command not found

You need to install the package containing `libtool` - it should be on the original CD of your linux distro or on one of the billions of repository mirrors around the world.

### [OSX] Error: Possibly undefined macro: AC_DEFINE

When running ./autogen.sh, autoconf sometimes fails with ‘configure.ac:16: error: possibly undefined macro: AC DEFINE’. This is related to the version of `pkg-config` you have installed (e.g. version 0.15.1 gives this error, but version 0.21 does not) with Fink / MacPorts. Upgrade to the latest version. Incidentally, the line-number reported (16) is not the actual location of the error - autoconf reports this wrongly (the actual error occurs later on with the ‘PKG CHECK MODULES(GTK28, ..., [AC DEFINE...’ command).

### [OSX] Warning: Underquoted definition of PKG_CHECK_MODULES

Running [name]./autogen.sh[/name], aclocal complains "/sw/share/aclocal/pkg.m4:5: warning: underquoted definition of PKG_CHECK_MODULES". If `pkgconfig` is not installed this is likely to give rise to the said spurious error. Install pkgconfig from Fink / MacPorts to proceed.

### [ALL] `autogen.sh` appears to run successfully, but `aclocal` complains about "underquoted definition of AM PATH..."

These warnings should not have affected the generation of a working ./configure script. So you may as well move on to the next step in the build.

## Compilation Errors <a id="compilation"></a>

### [ALL] rcc-qt5: command not found

This error can occur when the Qt5 binaries are not named as the `configure` script thinks they are, or are installed in a location that is not in the `$PATH`, and so they are not found.

Each of the three Qt5 utilities needed to compile **Aten** – `moc`, `uic`, and `rcc` – can be specified explicitly with three configure options; `--with-qtmoc`, `--with-qtuic`, and `--with-qtrcc`. Typically, both the Qt4 and Qt5 binaries are located in `/usr/bin`, but the Qt5 binaries are appended with a `-qt5` suffix, although this may not be the case on your particular system. For the sake of argument, let’s say they are, then the `configure` command should be invoked as follows:

```
bob@pc:~/src/aten:~> ./configure --with-qtmoc=/usr/bin/moc-qt5 --with-qtuic=/usr/bin/uic-qt5 --with-qtrcc=/usr/bin/rcc-qt5
```

### [ALL] Error: 'This file was generated using the moc from X.X.X. It cannot be used with the include files from this version...'

This is usually related to a non-Qt5 version of `moc` being found / used - see the above question.

### [OSX] Undefined reference to ___stdoutp expected to be defined in /usr/lib/libSystem.B.dylib

Chances are you have an older operating system - Panther (10.3.9) is confirmed to show this error. Since the Universal Mac binaries are built on a machine with Snow Leopard (10.6) this error can only be avoided by upgrading your operating system or compiling **Aten** by hand.


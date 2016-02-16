---
title: Compilation
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---

## Obtaining the Source

### From the Website (as an Archive)

Go to the [downloads](/aten/downloads) section and grab the `tar.gz` of the version you want, and unpack it with:

```
tar -zxvf aten-1.7.tar.gz
```

A directory called [value]aten-nn.mm[/value] will be created containing all the guts of the code, ready to build, where `nn.mm` is the current version number.

### From GoogleCode (with subversion)

If you want to build **Aten** from source and maintain a local copy so you can update it quickly to make the most of bugfixes and new features, this is the best way of doing it.  You’ll need to have subversion ([a]http://subversion.tigris.org/,http://subversion.tigris.org/[/a]) installed, since the GoogleCode repository where **Aten** lives is a subversion-style repository. To get yourself a copy of the latest source, run the following command:

```
bob@pc:~> svn co http://aten.googlecode.com/svn/trunk ./aten-latest
```

Afterwards, you’ll have a complete copy of the source in a directory called [value]aten-latest[/value].

## Installing Necessary Pre-requisites

**Aten** has a fairly modest set of external dependencies, namely Qt5 and readline. Since you’re building from source, all the development files related to these packages must also be installed. The C++ (g++) compiler and the automake/libtool packages are also a necessity.

### Debian and Variants

On deb-based systems (e.g. Debian and Ubuntu) a command a bit like this should install all that you need:

```
bob@pc:~> sudo apt-get install autotools-dev libtool autoconf \
             automake g++ libreadline5-dev libqt4-gui libqt4-opengl \
             libqt4-core libqt4-dev
```

#### Specific Notes: Ubuntu 14.04 LTS

The highest available version from the package universe on the 14.04 LTS version of Ubuntu (trusty) is 5.2, but **Aten** requires version >= 5.4, since it makes use of QOpenGLWidget which was only introduced in version 5.4. The relevant packages can be downloaded and installed from the user PPA repository at [https://launchpad.net/~beineri/+archive/ubuntu/opt-qt541-trusty], where the package is called `qt54base`. In order for button icons to work correctly you will also need the `qt54svg` package from the same repo.

### RPM-Based Systems

For most other Linux flavours the method of installing the necessary software really depends a lot on your personal preferences (or your system administrators).  For instance, using **zypper** on an OpenSuSE distribution the command is (run as root) as follows:

```
bob@pc:~> zypper install readline-devel libqt5-qtbase-devel libtool automake
```

### Mac OS X

On Mac OS X you will need to download the Qt5 'anywhere' package from their website. XXX

## Configure

### Make / Autotools

Configure the source with the following commands, run from the top level of the `aten-latest` or `aten-nn.mm` directory:

```
bob@pc:~> ./autogen.sh
```

This creates the necessary files needed to properly configure the build. If you unpacked the source from a tar.gz, it is not necessary to run autogen.sh.

Next, run the configure script to check for all programs and files that **Aten** depends on, and set up the build. If you plan on installing **Aten** ‘properly’ (i.e. install it on your machine so it is available to all users) the configure script will place all binaries and necessary files in `/usr/local` by default. This default location can be overridden by using the [var]--prefix=&lt;path&gt;[/var] option. So, the plain command is:

```
bob@pc:~> ./configure
```

To set the installation location use, for example:

```
bob@pc:~> ./configure –-prefix=/home/software
```

On Mac OS X it is necessary to specify which Qt4 installation you have installed (i.e. Framework or Fink):

XXX TODO

where [value]&lt;path&gt;[/value] points to the location of the Qt4 development binaries. All being well, no errors should be encountered by either of these two scripts. Check out the list of [a]frequently encountered problems,fep[/a]. If you get find yourself up against unresolvable issues, please email me and I’ll try to help.

## CMake

An out-of-tree build is best. Make a directory somewhere, enter in to it, and run:

```
bob@pc:~> cmake /path/to/source/for/aten-nn.mm
```

For example:

```
bob@pc:~> cd ~
bob@pc:~> mkdir aten-build
bob@pc:~> cd aten-build
bob@pc:~> cmake ~/src/aten-1.7
```

## Compile

Once successfully configured, build the source with:

```
bob@pc:~> make
```

This is probably a good time to make tea or brew coffee.



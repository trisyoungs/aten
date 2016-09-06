---
title: Compilation on OS X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

## Compiling Aten on OS X

(Thanks to N. Basma for contributing this section)

Assuming you have Xcode and the command line tools, install macports. If you don’t, go to [https://www.macports.org/install.php] and then install **Macports** for your version of OS X.

Once you have **Macports**, you will need to install a few packages. Before doing that, it is advised to have your port up to date, and upgrade outdated ports. Do that if you haven’t recently - simply run the following commands:

```
blabla:~user$		sudo port self update
blabla:~user$		sudo port upgrade outdated
```

Use Macports to install expat, libxslt, as well as some autotools. You may already have these installed, and can check that by running

```
blabla:~user$		locate name_of_package 
```

If not found, use the following commands to install:

```
blabla:~user$		sudo port install expat
blabla:~user$		sudo port install libxslt
blabla:~user$		sudo port install automake autoconf 
blabla:~user$		sudo port install libtool
```

Hopefully the above went smoothly. You can now obtain **Aten**. The best way to do that is through **git**. Run the following:

```
blabla:~user$		sudo port install git
```

and then run:

```
blabla:~user$		 git clone https://github.com/trisyoungs/aten.git ./aten-latest
```

to obtain the latest copy of the Aten source code.

Now you’ll have a complete copy of the source in wherever directory it is. It should be called **aten-latest**. Find out where that is, and move to that folder.

Next you will need to run the **autogen** script at the top of the source tree. Run the following:

```
blabla:~user$		./autogen
```

This may or may not work! It may fail and you might get an error saying: 

```
line 35: libtoolize: command not found, 
```

which is frustrating, because you have clearly installed libtoolize. This may or may not been a problem with port’s libtool package. What I did to get around this was to install libtool through homebrew using the command. Of course, for that, you will need to get homebrew. A cool guide can be found here: [https://coolestguidesontheplanet.com/installing-homebrew-on-os-x-el-capitan-10-11-package-manager-for-unix-apps/].

Once you have homebrew, you can run:

```
blabla:~user$		brew install libtool
```

**libtool** already exists on most OS X as a binary tool, and so when brew installs it, they will name it glibtool to avoid confusion. Open and edit the **autogen** file, and change **libtoolize** to **glibtoolize**.

Re-run ./autogen and hopefully this will work (it seemed to solve the problem for me). 

Before configuring, you will need to install **Qt**. Go to [http://www.qt.io/download/] and get **Qt Open Source**. It should recognise that you’re on a Mac and recommend you the **Qt Online Installer for OS X**. Download the dmg and follow the on-screen instructions, select the 5.6 package to install and put it in your home directory, i.e. /Users/username/Qt5.6. 

When complete, you can run **configure**. It is highly recommended that you provide the paths to Qt’s binaries at this stage. So run the following command:
	
```
blabla:~user$		./configure --with-qtmoc=~/Qt/5.6/clang_64/bin/moc --with-qtuic=~/Qt/5.6/clang_64/bin/uic --with-qtrcc=~/Qt/5.6/clang_64/bin/rcc
```

If you intend **not** to install **Aten**, and instead run it from the source directory, you also need to give the option --with-localdeploy to **configure**, so the plugins are put in the correct place.

Next, run **make**.

```
blabla:~user$		make
```

Once make is done, you have two options - either to not install it and have the benefit of keeping everything contained and local (and not having to sudo make install after updating and rebuilding the source), or to install it by running:

```
blabla:~user$		sudo make install
```

If you choose not to install, you can setup an alias in your bash profile to tell Aten everything it needs to run. Assuming you have chosen to build **Aten** inside the ‘aten-latest’ folder, you can setup an alias such as:

```
alias aten='/Users/username/aten-latest/src/aten --atendata=/Users/username/aten-latest/data
```

You should now be able to:

```
blabla:~user$		aten
```


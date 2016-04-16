---
title: System Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

System commands for controlling debug output, instantiation of the GUI, and exiting from the program.

---

## debug <a id="debug"></a>

_Syntax:_

**void** **debug** ( **string** _type_ )

Toggles debug output from various parts of the code. A full list of valid types is given in output types.

For example:

```aten
debug("parse");
```

---

## getEnv <a id="getenv"></a>

_Syntax:_

**string** **getEnv** ( **string** _variable_ )

Retrieves the contents of the named environment variable so that transfer of useful quantities can be made to Aten through a shell.

For example:

```aten
string s = getEnv("HOSTNAME");
```

gets the name of the host Aten is running on (although what you would then usefully do with it I don’t know).

Better examples can be found in the resources section of the website.

---

## getEnvF <a id="getenvf"></a>

_Syntax:_

**double** **getEnvF** ( **string** _variable_ )

Retrieves the contents of the named environment variable, converting it to a floating-point (double) value in the process.

For example:

```aten
double d = getEnvF("num");
```

gets the shell variable _num_ as a real number.

---

## getEnvI <a id="getenvi"></a>

_Syntax:_

**int** **getEnvI** ( **string** _variable_ )

Retrieves the contents of the named environment variable, converting it to an integer value in the process.

For example:

```aten
int i = getEnvI("count");
```

gets the shell variable _count_ as an integer number.

---

## help <a id="help"></a>

_Syntax:_

```aten
help command
```

Provide short help on the supplied _command_.

For example:

```aten
help cellaxes;
```

---

## null <a id="null"></a>

_Syntax:_

**void** **null** ( **variable** _var_, ... )

The null command accepts one or more pointer variables whose values are to be set to NULL (0).

---

## searchCommands <a id="searchcommands"></a>

_Syntax:_

**void** **searchCommands** ( **string** _search_ )

Search all available commands for the (partial) command name specified.

---

## seed <a id="seed"></a>

_Syntax:_

**void** **seed** ( **int** _i_ )

Sets the random seed.

For example:

```aten
seed(3242638);
```

---

## quit <a id="quit"></a>

_Syntax:_

**void** **quit** ( )

Quits out of the program.

For example:

```aten
quit();
```



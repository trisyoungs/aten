---
title: Math Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Standard mathematical functions.

---

## abs <a id="abs"></a>

_Syntax:_

**double** **abs** ( **int**|**double** _num_ )

Returns the absolute (positively-signed) value of _num_.

---

## cos <a id="cos"></a>

_Syntax:_

**double** **cos** ( **double** _angle_ )

Returns the cosine of _angle_ (which should be given in degrees).

---

## dotProduct <a id="dotproduct"></a>

_Syntax:_

**double** **dotproduct** ( [**Vector**](/aten/docs/scripting/variabletypes/vector) _u_, [**Vector**](/aten/docs/scripting/variabletypes/vector) _v_ )

Calculate and return the dot product of the two vectors _u_ and _v_.

---

## exp <a id="exp"></a>

_Syntax:_

**double** **exp** ( **double** _x_ )

Returns the exponential of _x_.

---

## ln <a id="ln"></a>

_Syntax:_

**double** **ln** ( **double** _x_ )

Returns the natural base-e logarithm of _x_.

---

## log <a id="log"></a>

_Syntax:_

**double** **log** ( **double** _x_ )

Returns the base-10 logarithm of _x_.

---

## nint <a id="nint"></a>

_Syntax:_

**int** **nint** ( **double** _x_ )

Returns the nearest integer value to _x_.

---

## normalise <a id="normalise"></a>

_Syntax:_

**double** **normalise** ( [**Vector**](/aten/docs/scripting/variabletypes/vector) _v_ )

Normalises the vector _v_, returning the magnitude of the vector before the normalisation was performed.

For example:
```aten
vector v = { 1, 2, 3 };
double mag = normalise(v);
printf("Normalised vector is { %f, %f, %f }, mag was %f\n", v.x, v.y, v.z, mag);
```

prints the following:

```aten
Normalised vector is { 0.267261, 0.534522, 0.801784 }, mag was 3.741657 
```

---

## random <a id="random"></a>

_Syntax:_

 **double** **random** ( )

Return a random real number between 0.0 and 1.0 inclusive.

---

## randomI <a id="randomi"></a>

_Syntax:_

**int** **randomI** ( **int** _max_ = RANDMAX )

Return a random integer between 0 and RANDMAX inclusive, or 0 and _max_ if it is supplied.

---

## sin <a id="sin"></a>

_Syntax:_

**double** **sin** ( **double** _angle_ )

Returns the sine of _angle_ (which should be given in degrees).

---

## sqrt <a id="sqrt"></a>

_Syntax:_

**double** **sqrt** ( **double** _x_ )

Returns the square root of _x_.

---

## tan <a id="tan"></a>

_Syntax:_

**double** **tan** ( **double** _angle_ )

Returns the tangent of _angle_ (which should be given in degrees).


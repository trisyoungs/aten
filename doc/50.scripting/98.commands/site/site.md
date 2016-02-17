---
title: Site Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---





XXX

Describe sites within molecules for use in analysis.

Note: These commands are outdated and will be removed completely in a future version.

---

## getSite <a id="getsite"></a>

_Syntax:_

**void** **getSite** ( **string** _name_ )

Selects (makes current) the site referenced by _name_. If the site cannot be found an error is returned.

For example:


```
getSite("carb1");
```


makes "carb1" the current site.

---

## listSites <a id="listsites"></a>

_Syntax:_

**void** **listSites** ( )

Prints the list of sites defined for the current model.

For example:


```
listSites();
```


---

## newSite <a id="newsite"></a>

_Syntax:_

**void** **newSite** ( **string** _name_, **string** _atomlist_ = "" )

Creates a new site _name_ for the current model, based on the molecule of _pattern_, and placed at the geometric centre of the atom IDs given in _atomlist_. If no atoms are given, the centre of geometry of all atoms is used. The new site becomes the current site.

For example:


```
newSite("watercentre", "h2o");
```


adds a site called 'watercentre' based on the pattern called 'h2o' and located at the centre of geometry of all atoms.


```
newSite("oxy", "methanol", "5");
```


adds a site called ‘oxy’ based on the pattern called ‘methanol’ and located at the fifth atom in each molecule.

---

## siteAxes <a id="siteaxes"></a>

_Syntax:_

**void** **siteAxes** ( **string** _x_atomlist_, **string** _y_atomlist_ )

Sets the local x (first set of atom IDs) and y (second set of atom IDs) axes for the current site. Each of the two axes is constructed by taking the vector from the site centre and the geometric centre of the list of atoms provided here. The y axis is orthogonalised with respect to the x axis and the z axis constructed from the cross product of the orthogonal x and y vectors.

For example:


```
siteAxes("1,2", "6");
```


sets the x axis definition of the current site to be the vector between the site centre and the average position of the first two atoms, and the y axis definition to be the vector between the site centre and the position of the sixth atom.



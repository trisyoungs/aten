--
title: Glyphs
brief: An overview of additional drawing objects (glyphs)
visible: true
template: manpage
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

A ‘glyph’ in **Aten** is any of a series of primitives (or shapes, or objects) that can be rendered in addition to the components that make up a standard model (i.e. atoms, bonds, and unit cell). These can be used to illustrate points of interest in a system, illustrate some vector quantity, or draw whole new objects from scratch. Glyphs can be drawn at fixed positions within a model, or can have their vertices linked to existing atoms, enabling them to be moved in conjunction with the model’s atoms. Some glyphs can also be rotated about their current position.

A glyph requires from one to four vertices to be set, depending on the type. A different colour may be assigned to each vertex enabling, for example, each corner of a triangle to possess a different colour. The available glyph types and the roles of the four possible coordinates are:


| Glyph | Rotatable?  | r1 | r2 | r3 | r4 |
| arrow | No | Tail coords | Head coords | | |
| cube | Yes | Centroid | Scaling factors | | |
| ellipsoid | | | | | |
| ellipsoidxyz | | | | | |
| line | No | Start coords | End coords | | |
| quad | No | Vertex 1 | Vertex 2 | Vertex 3 | Vertex 4 |
| sphere | Yes | Centroid | Scaling factors | | |
| svector | No | | | | |
| tetrahedron | Yes | Vertex 1 | Vertex 2 | Vertex 3 | Vertex 4 |
| text | No | Mid-left anchor of text | | | |
| text3d | No | Mid-left anchor of text | | | |
| triangle | No | Vertex 1 | Vertex 2 | Vertex 3 | |
| tubearrow | No | Tail coords | Head coords | | |
| vector | No | Centroid | Pointing vector | | |


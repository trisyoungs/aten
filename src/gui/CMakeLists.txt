# Don't forget to include output directory, otherwise
# the UI file won't be wrapped!
INCLUDE_DIRECTORIES(
${CMAKE_CURRENT_BINARY_DIR} 
${CMAKE_CURRENT_BINARY_DIR}/../
${QT_INCLUDE_DIR}
./
../
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}
)

set(qtproject_SRCS
  actiontext.h
  actiontext.cpp
  gui.h 
  ttreewidgetitem.h
  ttablewidgetitem.h
  tlistwidgetitem.h 
  tcombobox.h
  about_funcs.cpp
  atomlist_funcs.cpp
  bondactions.cpp
  build_funcs.cpp
  celldefine_funcs.cpp
  celltransform_funcs.cpp
  contextmenu_funcs.cpp
  disorder_funcs.cpp
  drawactions.cpp
  editactions.cpp
  ffeditor_funcs.cpp
  fileactions.cpp
  forcefieldactions.cpp
  forcefields_funcs.cpp
  glyphs_funcs.cpp
  grids_funcs.cpp
  gui.cpp
  input.cpp
  loadmodel_funcs.cpp
  mainwindow_funcs.cpp
  mainwindow_init.cpp
  measureactions.cpp
  minimiser_funcs.cpp
  modelactions.cpp
  mouseactions.cpp
  position_funcs.cpp
  prefs_funcs.cpp
  selectactions.cpp
  selectelement_funcs.cpp
  selectpattern_funcs.cpp
  settings.cpp
  styleactions.cpp
  systemactions.cpp
  tcanvas_funcs.cpp
  tcolourframe_funcs.cpp
  tcombobox_funcs.cpp
  tlistwidgetitem_funcs.cpp
  trajectoryactions.cpp
  transform_funcs.cpp
  ttablewidgetitem_funcs.cpp
  ttreewidgetitem_funcs.cpp
  viewactions.cpp
)

SET(pro_MOC_HDRS
  about.h
  atomlist.h
  build.h
  celldefine.h
  celltransform.h
  disorder.h
  ffeditor.h
  forcefields.h
  glyphs.h
  grids.h
  loadmodel.h
  mainwindow.h
  minimiser.h
  position.h
  prefs.h
  selectelement.h
  selectpattern.h
  transform.h
  tcanvas.uih
  tcolourframe.uih
  ttabbar.uih
  ttreewidget.uih
)
QT4_WRAP_CPP(pro_MOC_SRCS ${pro_MOC_HDRS} OPTIONS -f)

SET(pro_UIS
  about.ui
  atomlist.ui
  build.ui
  celldefine.ui
  celltransform.ui
  disorder.ui
  ffeditor.ui
  forcefields.ui
  glyphs.ui
  grids.ui
  loadmodel.ui
  mainwindow.ui
  minimiser.ui
  position.ui
  prefs.ui
  selectelement.ui
  selectpattern.ui
  transform.ui
)
QT4_WRAP_UI(pro_UIS_H ${pro_UIS})

set(pro_RES_QRC
  icons.qrc 
)
QT4_ADD_RESOURCES(pro_RES ${pro_RES_QRC})

add_library(gui ${pro_RES} ${pro_UIS_H} ${qtproject_SRCS} ${pro_MOC_SRCS})

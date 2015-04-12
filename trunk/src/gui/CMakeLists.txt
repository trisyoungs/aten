# Don't forget to include output directory, otherwise
# the UI file won't be wrapped!
INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_BINARY_DIR} 
  ${CMAKE_CURRENT_BINARY_DIR}/../
  ${Qt5Widgets_INCLUDE_DIRS}
  ./
  ../
  ${CMAKE_SOURCE_DIR}
  ${CMAKE_BINARY_DIR}
)

# Meta-Objects
SET(gui_MOC_HDRS
  about.h
  atomlist.h
  celldefinition.h
  celltransform.h
  command.h
  disorderoptions.h
  disorderwizard.h
  ffeditor.h
  forcefields.h
  fragments.h
  geometry.h
  glyphs.h
  grids.h
  layoutlist.h
  loadmodel.h
  mainwindow.h
  popupaddh.h
  popupcellangles.h
  popupcelllengths.h
  popupcellmatrix.h
  popupgrow.h
  popuprebond.h
  popupresetview.h
  popuptransmute.h
  pores.h
  position.h
  prefs.h
  progress.h
  saveimage.h
  scriptmovie.h
  select.h
  selectelement.h
  selectfilter.h
  selectpattern.h
  selectvariable.h
  trajectory.h
  transform.h
  vibrations.h
  viewbasis.h
  vieweigenvector.h
  zmatrix.h
  tcolourframe.hui
  tdoublespindelegate.hui
  texponentialspin.hui
  textratreewidgetitem.h
  tintegerspindelegate.hui
  tmenubutton.hui
  tprocess.hui
  tprompt.hui
  ttablewidget.hui
  ttreewidget.hui
  viewer.hui
)
QT5_WRAP_CPP(gui_MOC_SRCS ${gui_MOC_HDRS})

set(treegui_MOC_HDRS
  treegui.h
)
QT5_WRAP_CPP(treegui_MOC_SRCS ${treegui_MOC_HDRS})

# User Interface Files
SET(gui_UIS
  about.ui
  atomlist.ui
  celldefinition.ui
  celltransform.ui
  command.ui
  disorderoptions.ui
  disorderwizard.ui
  ffeditor.ui
  forcefields.ui
  fragments.ui
  geometry.ui
  glyphs.ui
  grids.ui
  loadmodel.ui
  mainwindow.ui
  popupaddh.ui
  popupcellangles.ui
  popupcelllengths.ui
  popupcellmatrix.ui
  popupgrow.ui
  popuprebond.ui
  popupresetview.ui
  popuptransmute.ui
  pores.ui
  position.ui
  prefs.ui
  progress.ui
  saveimage.ui
  scriptmovie.ui
  select.ui
  selectelement.ui
  selectfilter.ui
  selectpattern.ui
  selectvariable.ui
  trajectory.ui
  transform.ui
  vibrations.ui
  viewbasis.ui
  vieweigenvector.ui
  zmatrix.ui
)
QT5_WRAP_UI(gui_UIS_H ${gui_UIS})

SET(treegui_UIS
  treegui.ui
)
QT5_WRAP_UI(treegui_UIS_H ${treegui_UIS})

# Source files
set(gui_SRCS
  useractions.h
  useractions.cpp
  about_funcs.cpp
  atomlist_funcs.cpp
  celldefinition_funcs.cpp
  celltransform_funcs.cpp
  command_funcs.cpp
  disorderoptions_funcs.cpp
  disorderwizard_funcs.cpp
  ffeditor_funcs.cpp
  forcefields_funcs.cpp
  fragments_funcs.cpp
  geometry_funcs.cpp
  glyphs_funcs.cpp
  grids_funcs.cpp
  layoutlist.cpp
  loadmodel_funcs.cpp
  mainmenuactions.cpp
  mainwindow_context.cpp
  mainwindow_funcs.cpp
  mainwindow_image.cpp
  mainwindow_messages.cpp
  mainwindow_modellist.cpp
  mainwindow_panel_build.cpp
  mainwindow_panel_cell.cpp
  mainwindow_panel_view.cpp
  mainwindow_update.cpp
  popupaddh_funcs.cpp
  popupcellangles_funcs.cpp
  popupcelllengths_funcs.cpp
  popupcellmatrix_funcs.cpp
  popupgrow_funcs.cpp
  popuprebond_funcs.cpp
  popupresetview_funcs.cpp
  popuptransmute_funcs.cpp
  pores_funcs.cpp
  position_funcs.cpp
  prefs_funcs.cpp
  progress_funcs.cpp
  saveimage_funcs.cpp
  scriptmovie_funcs.cpp
  select_funcs.cpp
  selectelement_funcs.cpp
  selectfilter_funcs.cpp
  selectpattern_funcs.cpp
  selectvariable_funcs.cpp
  settings.cpp
  viewer_funcs.cpp
  viewer_key.cpp
  viewer_model.cpp
  viewer_mouse.cpp
  viewer_offscreen.cpp
  viewer_scene.cpp
  viewer_user.cpp
  viewer_useractions.cpp
  tcolourframe_funcs.cpp
  tmenubutton_funcs.cpp
  tdoublespindelegate_funcs.cpp
  texponentialspin_funcs.cpp
  tintegerspindelegate_funcs.cpp
  trajectory_funcs.cpp
  transform_funcs.cpp
  tprocess_funcs.cpp
  tprompt_funcs.cpp
  textratreewidgetitem_funcs.cpp
  vibrations_funcs.cpp
  viewbasis_funcs.cpp
  vieweigenvector_funcs.cpp
  zmatrix_funcs.cpp
)

# Resources
set(gui_RES_QRC
  icons.qrc 
)
QT5_ADD_RESOURCES(gui_RES ${gui_RES_QRC})

# Target 'gui'
add_library(gui
  ${gui_RES}
  ${gui_UIS_H}
  ${gui_SRCS}
  ${gui_MOC_SRCS}
)

# Target 'treegui'
add_library(treegui
  ${treegui_UIS_H}
  ${treegui_MOC_SRCS}
  treegui_funcs.cpp
)


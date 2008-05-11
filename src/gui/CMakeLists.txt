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
canvas.h 
gui.h 
ttreewidgetitem.h
ttablewidgetitem.h
tlistwidgetitem.h 
tcombobox.h
mainwindow_funcs.cpp 
mainwindow_init.cpp 
loadmodel_funcs.cpp
selectpattern_funcs.cpp
input.cpp 
analyse_funcs.cpp 
atomlist_funcs.cpp 
disorder_funcs.cpp 
forcefield_funcs.cpp 
transform_funcs.cpp 
edit_funcs.cpp 
cell_funcs.cpp 
position_funcs.cpp 
grid_funcs.cpp 
minimise_funcs.cpp 
contextmenu_funcs.cpp 
actions.cpp 
modelactions.cpp 
editactions.cpp 
viewactions.cpp 
trajectoryactions.cpp 
fileactions.cpp 
prefs_funcs.cpp 
gui.cpp 
tlistwidgetitem_funcs.cpp 
ttablewidgetitem_funcs.cpp 
tcanvas_funcs.cpp 
tcolourframe_funcs.cpp 
ttreewidgetitem_funcs.cpp 
canvas.cpp 
ffeditor_funcs.cpp
)

SET(pro_MOC_HDRS
 ffeditor.h
 loadmodel.h
 selectpattern.h
 mainwindow.h 
 prefs.h 
 ttabbar.uih 
 tcanvas.uih 
 tcolourframe.uih 
 ttreewidget.uih
)
QT4_WRAP_CPP(pro_MOC_SRCS ${pro_MOC_HDRS} OPTIONS -f)

SET(pro_UIS
  ffeditor.ui
  loadmodel.ui
  mainwindow.ui
  prefs.ui
  selectpattern.ui
)
QT4_WRAP_UI(pro_UIS_H ${pro_UIS})

set(pro_RES_QRC
icons.qrc 
)
QT4_ADD_RESOURCES(pro_RES ${pro_RES_QRC})

add_library(gui ${pro_RES} ${pro_UIS_H} ${qtproject_SRCS} ${pro_MOC_SRCS})

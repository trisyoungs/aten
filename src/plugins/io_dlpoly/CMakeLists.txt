# ----------------
# DL_POLY 2 CONFIG
# ----------------
set(dlp2_MOC_HDRS
  dlp2.hui
  dlp2importoptions.h
  dlp2exportoptions.h
)
QT5_WRAP_CPP(dlp2_MOC_SRCS ${dlp2_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(dlp2_UIS
  dlp2importoptions.ui 
  dlp2exportoptions.ui 
)
QT5_WRAP_UI(dlp2_UIS_H ${dlp2_UIS})

add_library(dlp2 MODULE
  dlp2_funcs.cpp
  common.cpp
  dlp2importoptions_funcs.cpp
  dlp2exportoptions_funcs.cpp
  ${dlp2_MOC_SRCS}
  ${dlp2_UIS_H}
)
target_link_libraries(dlp2
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(dlp2 PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlp2
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(dlp2 PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

# ----------------
# DL_POLY 4 CONFIG
# ----------------
set(dlp4_MOC_HDRS
  dlp4.hui
  dlp4importoptions.h
  dlp4exportoptions.h
)
QT5_WRAP_CPP(dlp4_MOC_SRCS ${dlp4_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(dlp4_UIS
  dlp4importoptions.ui 
  dlp4exportoptions.ui 
)
QT5_WRAP_UI(dlp4_UIS_H ${dlp4_UIS})

add_library(dlp4 MODULE
  dlp4_funcs.cpp
  common.cpp
  dlp4importoptions_funcs.cpp
  dlp4exportoptions_funcs.cpp
  ${dlp4_MOC_SRCS}
  ${dlp4_UIS_H}
)
target_link_libraries(dlp4
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(dlp4 PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlp4
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(dlp4 PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

# -------------
# DL_POLY FIELD
# -------------
set(dlpfield_MOC_HDRS
  field.hui
)
QT5_WRAP_CPP(dlpfield_MOC_SRCS ${dlpfield_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

#SET(dlpfield_UIS
#)
#QT5_WRAP_UI(dlpfield_UIS_H ${dlpfield_UIS})

add_library(dlpfield MODULE
  field_funcs.cpp
  common.cpp
  ${dlpfield_MOC_SRCS}
#  ${dlpfield_UIS_H}
)
target_link_libraries(dlpfield
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(dlpfield PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlpfield
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(dlpfield PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

# -------------
# DL_POLY_2 HISTORY
# -------------
set(history2_MOC_HDRS
  history2.hui
  dlp2importoptions.h
)
QT5_WRAP_CPP(history2_MOC_SRCS ${history2_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(history2_UIS
  dlp2importoptions.ui 
)
QT5_WRAP_UI(history2_UIS_H ${history2_UIS})

add_library(dlp2history MODULE
  history2_funcs.cpp
  dlp2importoptions_funcs.cpp
  common.cpp
  ${history2_MOC_SRCS}
  ${history2_UIS}
)
target_link_libraries(dlp2history
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(dlp2history PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlp2history
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)
# Includes
target_include_directories(dlp2history PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

# -------------
# DL_POLY_4 HISTORY
# -------------
set(history4_MOC_HDRS
  history4.hui
  dlp4importoptions.h
)
QT5_WRAP_CPP(history4_MOC_SRCS ${history4_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(history4_UIS
  dlp4importoptions.ui 
)
QT5_WRAP_UI(history4_UIS_H ${history4_UIS})

add_library(dlp4history MODULE
  history4_funcs.cpp
  dlp4importoptions_funcs.cpp
  common.cpp
  ${history4_MOC_SRCS}
  ${history4_UIS}
)
target_link_libraries(dlp4history
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(dlp4history PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlp4history
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)
# Includes
target_include_directories(dlp4history PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

# -------------
# DL_POLY_4 DEFECTS
# -------------
set(defects4_MOC_HDRS
  defects4.hui
  defects4importoptions.h
)
QT5_WRAP_CPP(defects4_MOC_SRCS ${defects4_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(defects4_UIS
  defects4importoptions.ui 
)
QT5_WRAP_UI(defects4_UIS_H ${defects4_UIS})

add_library(dlp4defects MODULE
  defects4_funcs.cpp
  defects4importoptions_funcs.cpp
  common.cpp
  ${defects4_MOC_SRCS}
  ${defects4_UIS}
)
target_link_libraries(dlp4defects
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(dlp4defects PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlp4defects
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)
# Includes
target_include_directories(dlp4defects PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

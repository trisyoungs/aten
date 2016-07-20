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
set(field_MOC_HDRS
  field.hui
)
QT5_WRAP_CPP(field_MOC_SRCS ${field_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

#SET(field_UIS
#)
#QT5_WRAP_UI(field_UIS_H ${field_UIS})

add_library(field MODULE
  field_funcs.cpp
  common.cpp
  ${field_MOC_SRCS}
#  ${field_UIS_H}
)
target_link_libraries(field
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(field PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS field
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(field PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)


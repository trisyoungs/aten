# Quantum Espresso Input file Export
set(espressoin_MOC_HDRS
  espressoin.hui
)
QT5_WRAP_CPP(espressoin_MOC_SRCS ${espresso_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(espressoin MODULE
  espressoin_funcs.cpp
  ${espressoin_MOC_SRCS}
)
set_target_properties(espressoin PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS espressoin
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(espressoin PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)


# Quantum Espresso Output file Import
set(espressoout_MOC_HDRS
  espressoout.hui
)
QT5_WRAP_CPP(espressoout_MOC_SRCS ${espresso_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(espressoout MODULE
  espressoout_funcs.cpp
  ${espressoout_MOC_SRCS}
)
set_target_properties(espressoout PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS espressoout
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(espressoout PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)


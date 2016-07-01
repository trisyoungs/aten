add_library(plugins
  pluginstore.cpp
  plugintypes.cpp
  pluginstore.h
  plugintypes.h
)
target_include_directories(plugins PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

target_link_libraries(plugins)

add_subdirectory(io_akf)
add_subdirectory(io_cif)
add_subdirectory(io_csd)
add_subdirectory(io_cube)
add_subdirectory(io_dlputils)
add_subdirectory(io_epsr)
add_subdirectory(io_espresso)
add_subdirectory(io_ff)
add_subdirectory(io_gamess)
add_subdirectory(io_gaussian)
add_subdirectory(io_gromacs)
add_subdirectory(io_mdlmol)
add_subdirectory(io_mopac)
add_subdirectory(io_msi)
add_subdirectory(io_pdb)
add_subdirectory(io_rmcprofile)
add_subdirectory(io_siesta)
add_subdirectory(io_sybylmol2)
add_subdirectory(io_vfield)
add_subdirectory(io_xyz)
add_subdirectory(method_mopac7)

project(Aten)

set(DESCRIPTION "Aten - Atomic configuration builder and editor")
set(AUTHOR "Tristan Youngs")
set(VERSION_MAJOR "2")
set(VERSION_MINOR "1")
set(VERSION_PATCH "2")

set(CMAKE_BUILD_TYPE "Release")

cmake_minimum_required(VERSION 2.8.12)

if(COMMAND cmake_policy)
  cmake_policy(VERSION 2.6)
endif(COMMAND cmake_policy)

# Add definitions
LIST(APPEND CMAKE_MODULE_PATH "${Aten_SOURCE_DIR}/cmake/Modules") 
include(DefineOptions)
include(DefineFunctions)
include(GNUInstallDirs)
include(FindREADLINE)
include(FindBISON)
include(FindFTGL)
include(FindGLEXT)

# Find required packages for this project
find_package(Qt5Widgets REQUIRED)
find_package(Qt5Core REQUIRED)
find_package(Qt5Gui REQUIRED)
find_package(READLINE REQUIRED)
# -- Force first look for Bison to be /usr/local/bin, since version provided by XCode on OSX is too old (v2.3) 
find_package(BISON PATHS /usr/local/bin NO_DEFAULT_PATH QUIET)
find_package(BISON REQUIRED)
find_package(OpenGL REQUIRED)
find_package(FTGL REQUIRED)
find_package(Freetype REQUIRED)
find_package(GLEXT REQUIRED)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Make sure CMake won't try to generate rules for moc (we will do it ourselves)
set(CMAKE_AUTOMOC OFF)

# Set policy for automatic linkage of Qt libs to project
CMAKE_POLICY(SET CMP0020 NEW)

# Perform system-specific setup
# -- Windows
if(WIN32)
  set (target_name Aten)

  # Adjust external include directories
  include_directories(
    ${FTGL_INCLUDE_DIRS}
    ${GLEXT_INCLUDE_DIRS}
  )

  # Set plugin link libraries
  SET(PLUGIN_LINK_LIBS
    aten
    Qt5::Widgets
    Qt5::Core
  )

  # Add global NOMINMAX define for Windows systems, and inhibit console creation
  ADD_DEFINITIONS("-DNOMINMAX")
  SET(GUI_TYPE WIN32)
  #if(MINGW)
  #  SET(CMAKE_EXE_LINKER_FLAGS "-mwindows")
  #endif(MINGW)
endif(WIN32)

# -- Unix
if(UNIX)
  set (target_name aten)
endif(UNIX)

# -- OSX
if(APPLE)
  set (target_name aten)
  set(CMAKE_OSX_ARCHITECTURES "x86_64")
  ADD_DEFINITIONS(-D_MAC)
  # Find OpenGL and GLUT
  find_package(GLUT REQUIRED)
  # Request dynamic lookup of symbols in shared library objects
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -undefined dynamic_lookup")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -undefined dynamic_lookup")
endif(APPLE)

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})
set(SRCS src/)

# Set bundle info
If(APPLE)
  set(MACOSX_BUNDLE_ICON_FILE  "Aten.icns")
  set(MACOSX_BUNDLE_GUI_IDENTIFIER  "Aten")
  set(MACOSX_BUNDLE_LONG_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
  set(MACOSX_BUNDLE_BUNDLE_NAME  "Aten" )
  set(MACOSX_BUNDLE_SHORT_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}")
  set(MACOSX_BUNDLE_BUNDLE_VERSION  "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
  set(MACOSX_BUNDLE_COPYRIGHT "${AUTHOR}")
endif(APPLE)

# Process CMakeLists in subdirs
add_subdirectory(${SRCS})

add_executable(${target_name}
  MACOSX_BUNDLE
  ${SRCS}/main.cpp 
)

# Set project-local include directories for target
target_include_directories(${target_name} PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${PROJECT_SOURCE_DIR}/src/gui
  ${FREETYPE_INCLUDE_DIRS}
)

# Perform final link
if(WIN32)
  set(ATEN_LINK_LIBS
    aten
    Qt5::Widgets Qt5::Core ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FREETYPE_LIBRARIES} ${READLINE_LIBRARIES})
else(WIN32)
  set(ATEN_LINK_LIBS
    messenger gui parser treegui command methods render model math main fourierdata ff base sg
    Qt5::Widgets Qt5::Core ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FREETYPE_LIBRARIES} ${READLINE_LIBRARIES})
endif(WIN32)
target_link_libraries( ${target_name} ${ATEN_LINK_LIBS})

# Install target for unix
if(UNIX AND NOT APPLE)
  INSTALL_TARGET(${target_name} "")
  configure_file(${PROJECT_SOURCE_DIR}/desktop.cmake Aten.desktop)
  install(FILES ${PROJECT_SOURCE_DIR}/src/gui/icons/aten.png
        DESTINATION share/aten)
  install(FILES ${PROJECT_BINARY_DIR}/Aten.desktop
        DESTINATION share/applications)
  foreach(dir ff filters fftesting fragments partitions scripts test external)  # 'includes' removed 02/08/16
    install(DIRECTORY ${PROJECT_SOURCE_DIR}/data/${dir}
          DESTINATION share/aten)
  endforeach(dir)
endif(UNIX AND NOT APPLE)

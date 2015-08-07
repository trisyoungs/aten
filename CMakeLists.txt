project(Aten)

set(DESCRIPTION "Aten - Atomic configuration builder and editor")
set(AUTHOR "Tristan Youngs")
set(VERSION_MAJOR "1")
set(VERSION_MINOR "99")
set(VERSION_PATCH "0")

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

# Find required packages for this project
find_package(Qt5Widgets REQUIRED)
find_package(Qt5Core REQUIRED)
find_package(Qt5Gui REQUIRED)
find_package(READLINE REQUIRED)
find_package(BISON REQUIRED)
find_package(OpenGL REQUIRED)
find_package(FTGL REQUIRED)
find_package(Freetype REQUIRED)
message(STATUS  "readline ${READLINE_LIBRARY}")

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
  # Setup FTGL and GLExt bits and pieces (for custom build machine)
  set(FTGL_INCLUDES "C:/Users/axr61803/src/ftgl-2.1.3~rc5/src")
  set(FT2_INCLUDES "C:/GnuWin32/include" "C:/GnuWin32/include/freetype2")
  set(FTGL_LIBRARIES "C:/Users/axr61803/src/ftgl-2.1.3~rc5/src/libftgl.a")
  set(FT2_LIBRARIES "C:/GnuWin32/lib/libfreetype.dll.a")
  set(GLEXT_INCLUDES "C:/Users/axr61803/src/include")
  # Add global NOMINMAX define for Windows systems, and inhibit console creation
  ADD_DEFINITIONS("-DNOMINMAX")
  SET(GUI_TYPE WIN32)
  if(MINGW)
    SET(CMAKE_EXE_LINKER_FLAGS "-mwindows")
  endif(MINGW)
endif(WIN32)

# -- Unix
if(UNIX)
  set (target_name aten)
  # Set include location for FreeType2
  #set(FT2_INCLUDES "/usr/include/freetype2")
endif(UNIX)

# -- OSX
if(APPLE)
  set (target_name aten)
  set(CMAKE_OSX_ARCHITECTURES "x86_64")
  ADD_DEFINITIONS(-D_MAC)
  # Find OpenGL and GLUT
  find_package(GLUT REQUIRED)
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

### htese shall go once is confirmed all is kosher
#include_directories(
#  ${CMAKE_BINARY_DIR}/src
#  ${Qt5Widgets_INCLUDE_DIRS}
#  ${Qt5OpenGL_INCLUDE_DIRS}
#  ${Qt5Core_INCLUDE_DIRS}
#  ${Qt5Gui_INCLUDE_DIRS}
#  ${FTGL_INCLUDE_DIRS}
#  ${FREETYPE_INCLUDE_DIRS}
#  ${GLEXT_INCLUDES}
#)

# Process CMakeLists in subdirs
add_subdirectory(${SRCS})

add_executable(${target_name}
  MACOSX_BUNDLE
  ${SRCS}/main.cpp 
)
target_include_directories(${target_name} PRIVATE
	${PROJECT_SOURCE_DIR}/src
	${PROJECT_BINARY_DIR}/src
	${PROJECT_SOURCE_DIR}/src/gui
)
# Perform final link
#link_directories (${LIBRARY_OUTPUT_PATH})
IF(WIN32)
  set(ATEN_LINK_LIBS Qt5::Widgets Qt5::Core ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FREETYPE_LIBRARIES} C:/GnuWin32/lib/libreadline.dll.a)
ELSEIF(APPLE)
  set(ATEN_LINK_LIBS Qt5::Widgets Qt5::Core ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FREETYPE_LIBRARIES} ${READLINE_LIBRARY})
ELSE(APPLE)
  set(ATEN_LINK_LIBS Qt5::Widgets Qt5::Core ${OPENGL_LIBRARIES} ${FTGL_LIBRARIES} ${FREETYPE_LIBRARIES} ${READLINE_LIBRARY})
ENDIF(WIN32)

target_link_libraries( ${target_name}
    gui parser treegui command methods render model math main fourierdata ff base sg
    ${ATEN_LINK_LIBS}
    )

if(UNIX AND NOT APPLE)
  INSTALL_TARGET(${target_name} "")
  configure_file(${PROJECT_SOURCE_DIR}/desktop.cmake Aten.desktop)
  install(FILES ${PROJECT_SOURCE_DIR}/src/gui/icons/aten.png
        DESTINATION share/aten)
  install(FILES ${PROJECT_BINARY_DIR}/Aten.desktop
        DESTINATION share/applications)
  foreach(i ff filters fftesting fragments partitions scripts test includes)
    install(DIRECTORY ${PROJECT_SOURCE_DIR}/data/${i}
          DESTINATION share/aten)
  endforeach(i)
endif(UNIX AND NOT APPLE)

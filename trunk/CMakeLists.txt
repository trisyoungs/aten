project(Aten)

set(DESCRIPTION "Aten - Atomic configuration builder and editor")
set(AUTHOR "Tristan Youngs")
set(VERSION_MAJOR "1")
set(VERSION_MINOR "941")
set(VERSION_PATCH "1")

set(CMAKE_BUILD_TYPE "Release")

cmake_minimum_required(VERSION 2.8.11)

if(COMMAND cmake_policy)
  cmake_policy(VERSION 2.6)
endif(COMMAND cmake_policy)

# Find required packages for this project
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}")
find_package(Qt5Widgets REQUIRED)
find_package(Qt5Core REQUIRED)
find_package(Qt5Gui REQUIRED)
find_package(READLINE REQUIRED)
include(FindBISON)
find_package(BISON REQUIRED)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Make sure CMake won't try to generate rules for moc (we will do it ourselves)
set(CMAKE_AUTOMOC OFF)

# Set policy for automatic linkage of Qt libs to project
CMAKE_POLICY(SET CMP0020 OLD)

# Perform system-specific setup
# -- Windows
if(WIN32)
  set (target_name Aten)
  # Setup FTGL and GLExt bits and pieces (for custom build machine)
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "C:/src/szip-2.1/")
  set(FTGL_INCLUDES "C:/src/ftgl-2.1.3~rc5/src")
  set(FT2_INCLUDES "C:/GnuWin32/include" "C:/GnuWin32/include/freetype2")
  set(FTGL_LIBRARIES "C:/src/ftgl-2.1.3~rc5/src/libftgl.a")
  set(FT2_LIBRARIES "C:/GnuWin32/lib/libfreetype.dll.a")
  set(GLEXT_INCLUDES "C:/src/include")
  # Find OpenGL
  find_package(OPENGL REQUIRED)
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
  set(FT2_INCLUDES "/usr/include/freetype2")
endif(UNIX)

# -- OSX
if(APPLE)
  set (target_name aten)
  #set(CMAKE_OSX_ARCHITECTURES "x86_64")
  ADD_DEFINITIONS(-D_MAC)
  # Set include location for FreeType2
  set(FT2_INCLUDES "/usr/local/include/freetype2")
  # Find OpenGL and GLUT
  find_package(OPENGL REQUIRED)
  find_package(GLUT REQUIRED)
  # Set libraries for link time
  set(FTGL_LIBRARIES "-L/usr/local/lib -lftgl")
  # Add define to suppress gl.h/gl3.h include warnings
endif(APPLE)

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
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

# Set include directories
include_directories(
  ${CMAKE_BINARY_DIR}/src
  ${CMAKE_SOURCE_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/gui
  ${Qt5Widgets_INCLUDE_DIRS}
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${FTGL_INCLUDES}
  ${FT2_INCLUDES}
  ${GLEXT_INCLUDES}
)

# Process CMakeLists in subdirs
add_subdirectory(${SRCS})

add_executable(${target_name}  MACOSX_BUNDLE
  ${SRCS}/main.cpp 
)

# Perform final link
link_directories (${LIBRARY_OUTPUT_PATH})
IF(WIN32)
  set(ATEN_LINK_LIBS Qt5::Widgets Qt5::Core ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FT2_LIBRARIES} C:/GnuWin32/lib/libreadline.dll.a)
ELSEIF(APPLE)
  set(ATEN_LINK_LIBS Qt5::Widgets Qt5::Core ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FT2_LIBRARIES} ${READLINE_LIBRARY})
ELSE(APPLE)
  set(ATEN_LINK_LIBS Qt5::Widgets Qt5::Core -lGL -lftgl)
ENDIF(WIN32)

target_link_libraries( ${target_name}
    gui parser treegui command methods render model math sg main fourier ff base
    ${ATEN_LINK_LIBS}
    )


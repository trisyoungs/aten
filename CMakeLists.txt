project(aten)


set(DESCRIPTION "Aten - Atomic configuration builder and editor")
set(AUTHOR "Tristan Youngs")
set(VERSION_MAJOR "0")
set(VERSION_MINOR "95.3")
set(VERSION_PATCH "1")

set(CMAKE_BUILD_TYPE "Release")

cmake_minimum_required(VERSION 2.4.8)
if(COMMAND cmake_policy)
  cmake_policy(VERSION 2.6)
endif(COMMAND cmake_policy)

set (target_name aten)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}")
find_package(Qt4 REQUIRED) # find and setup Qt4 for this project
find_package(READLINE REQUIRED)
IF(WIN32 OR APPLE)
find_package(OPENGL REQUIRED)
endif(WIN32 OR APPLE)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(SRCS src/)
#ADD_DEFINITIONS(-DQT_SHARED)

# tell cmake to process CMakeLists.txt in that subdirectory
add_subdirectory(${SRCS})

include_directories(
${CMAKE_SOURCE_DIR} 
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src
${SRCS}/templates/
${SRCS}/
${QT_INCLUDE_DIR}
${READLINE_INCLUDE_DIR}
)
add_executable(${target_name}
${SRCS}/main.cpp
)


link_directories (${LIBRARY_OUTPUT_PATH})
IF(WIN32 OR APPLE)
target_link_libraries(${target_name} 
gui base command render methods model
sg parse classes energy fourier math
${QT_QTOPENGL_LIBRARY} ${OPENGL_LIBRARIES} ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY}
${READLINE_LIBRARY}
)
ELSE(WIN32 OR APPLE)
target_link_libraries(${target_name} 
gui base command render methods model 
sg parse classes energy fourier math
${QT_QTOPENGL_LIBRARY} ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY}
${READLINE_LIBRARY}
)
ENDIF(WIN32 OR APPLE)

####################
####install target
####################

if(NOT ${PREFIX})
MESSAGE("Install folder ${PREFIX}")
set(CMAKE_INSTALL_PREFIX "${PREFIX}")
endif(NOT ${PREFIX})

install(TARGETS ${target_name}
      RUNTIME DESTINATION bin
      PERMISSIONS WORLD_EXECUTE)

install(DIRECTORY  "${CMAKE_HOME_DIRECTORY}/data" 
     FILE_PERMISSIONS WORLD_READ
     DIRECTORY_PERMISSIONS WORLD_EXECUTE
     DESTINATION share/aten
     REGEX ".svn" EXCLUDE
)

if(WIN32)
FIND_FILE(Rline readline5.dll ENV PATH)
Find_File(qtgui QtGui4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
Find_File(qtgl QtOpenGL4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
Find_File(qtcore QtCore4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)

install(FILES ${qtgui} ${qtgl} ${qtcore} ${Rline}
DESTINATION bin
PERMISSIONS WORLD_READ
)
ENDIF(WIN32)


#######################
## uninstall target
#######################

CONFIGURE_FILE(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)
ADD_CUSTOM_TARGET(uninstall
  "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")
###########################

INCLUDE(InstallRequiredSystemLibraries)
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${DESCRIPTION})
SET(CPACK_PACKAGE_VENDOR ${AUTHOR})
SET(CPACK_PACKAGE_CONTACT ${CPACK_PACKAGE_VENDOR})
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
SET(CPACK_PACKAGE_VERSION_MAJOR ${VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${VERSION_PATCH})
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "aten${VERSION_MAJOR}.${VERSION_MINOR}")
IF(WIN32 AND NOT UNIX)
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons\\\\aten.ico")
  set(CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons\\\\uninstall.ico")
  SET(CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons\\\\aten.bmp")
  SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\aten.exe")  
  SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} Aten")
  SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.projectaten.org")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.projectaten.org")
  SET(CPACK_NSIS_CONTACT "t.youngs@qub.ac.uk")
  SET(CPACK_NSIS_MODIFY_PATH ON)
  SET(CPACK_PACKAGE_EXECUTABLES "aten" "aten")
ELSE(WIN32 AND NOT UNIX)
SET(CPACK_GENERATOR "TGZ;TBZ2;RPM;DEB")
Endif(WIN32 AND NOT UNIX)
SET(CPACK_CMAKE_GENERATOR ${CMAKE_GENERATOR})

INCLUDE(CPack)



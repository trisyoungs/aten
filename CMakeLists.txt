project(aten)

set(VERSION 0.95)
set(PACKAGE_VERSION ${VERSION})

cmake_minimum_required(VERSION 2.4.8)
if(COMMAND cmake_policy)
  cmake_policy(VERSION 2.6)
endif(COMMAND cmake_policy)

set (target_name aten)

find_package(Qt4 REQUIRED) # find and setup Qt4 for this project
#set(QT_USE_OPENGL TRUE)
#INCLUDE(${QT_USE_FILE})
#find_package(OpenGL REQUIRED)
#message("${OPENGL_INCLUDE_DIR}")

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(SRCS src/)
#SET(QT_DEFINITIONS "")
ADD_DEFINITIONS(-DQT_SHARED)
ADD_DEFINITIONS(-DHAVE_CONFIG_H)

# tell cmake to process CMakeLists.txt in that subdirectory
add_subdirectory(${SRCS})

include_directories(
.
${SRCS}/templates/
${SRCS}/
)
add_executable(${target_name}
${SRCS}/main.cpp
)

link_directories (${LIBRARY_OUTPUT_PATH})
target_link_libraries(${target_name} 
base command gui render methods model
sg parse classes energy fourier math
${QT_QTOPENGL_LIBRARY} ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY}
)


####
#set the configure file
#####
CONFIGURE_FILE(config.h.cmake config.h)



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

install( DIRECTORY  "${PROJECT_BINARY_DIR}/data/filters" 
     FILE_PERMISSIONS WORLD_READ
     DIRECTORY_PERMISSIONS WORLD_EXECUTE
     DESTINATION share/aten
     REGEX ".svn" EXCLUDE
)
install(FILES "${PROJECT_BINARY_DIR}/data/prefs.dat"
PERMISSIONS WORLD_READ
DESTINATION $ENV{HOME}/.aten
)
install(CODE
"message(\"do not forget to set ATENDATA to ${CMAKE_INSTALL_PREFIX}/share/aten
in your profile\")"
)

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


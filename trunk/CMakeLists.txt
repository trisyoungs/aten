project(aten)


set(DESCRIPTION "Aten - Atomic configuration builder and editor")
set(AUTHOR "Tristan Youngs")
set(VERSION_MAJOR "0")
set(VERSION_MINOR "95")
set(VERSION_PATCH "3")



cmake_minimum_required(VERSION 2.4.8)
if(COMMAND cmake_policy)
  cmake_policy(VERSION 2.6)
endif(COMMAND cmake_policy)

set (target_name aten)

find_package(Qt4 REQUIRED) # find and setup Qt4 for this project

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

install( DIRECTORY  "${CMAKE_HOME_DIRECTORY}/data/filters" 
     FILE_PERMISSIONS WORLD_READ
     DIRECTORY_PERMISSIONS WORLD_EXECUTE
     DESTINATION share/aten
     REGEX ".svn" EXCLUDE
)
install(CODE
"message(\"Do not forget to set ATENDATA to ${CMAKE_INSTALL_PREFIX}/share/aten
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

INCLUDE(InstallRequiredSystemLibraries)
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${DESCRIPTION})
SET(CPACK_PACKAGE_VENDOR ${AUTHOR})
SET(CPACK_PACKAGE_CONTACT ${CPACK_PACKAGE_VENDOR})
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
SET(CPACK_PACKAGE_VERSION_MAJOR ${VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${VERSION_PATCH})
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "aten
${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
SET(CPACK_PACKAGE_EXECUTABLES "aten")
SET(CPACK_GENERATOR "TGZ;TBZ2;RPM;DEB")
SET(CPACK_CMAKE_GENERATOR ${CMAKE_GENERATOR})
INCLUDE(CPack)



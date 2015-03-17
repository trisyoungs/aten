project(Aten)

set(DESCRIPTION "Aten - Atomic configuration builder and editor")
set(AUTHOR "Tristan Youngs")
set(VERSION_MAJOR "1")
set(VERSION_MINOR "920")
set(VERSION_PATCH "1")

set(CMAKE_BUILD_TYPE "Release")

if(APPLE)
  cmake_minimum_required(VERSION 2.6.0)
  set(CMAKE_OSX_ARCHITECTURES "i386;x86_64")
  ADD_DEFINITIONS(-D_MAC)
else(APPLE)
  cmake_minimum_required(VERSION 2.4.8)
endif(APPLE)

if(COMMAND cmake_policy)
  cmake_policy(VERSION 2.6)
endif(COMMAND cmake_policy)

# Find required packages for this project
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}")
find_package(Qt4 REQUIRED)
find_package(READLINE REQUIRED)
include(FindBISON)
find_package(BISON REQUIRED)

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
  # Set include location for FreeType2
  set(FT2_INCLUDES "/usr/local/include/freetype2")
  # Find OpenGL and GLUT
  find_package(OPENGL REQUIRED)
  find_package(GLUT REQUIRED)
  # Set libraries for link time
  set(FTGL_LIBRARIES "-L/usr/local/lib -lftgl")
  # Add define to suppress gl.h/gl3.h include warnings
  ADD_DEFINITIONS("-DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED")
endif(APPLE)

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(SRCS src/)
#ADD_DEFINITIONS(-DQT_SHARED)

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
  ${QT_INCLUDE_DIR}
  ${FTGL_INCLUDES}
  ${FT2_INCLUDES}
  ${GLEXT_INCLUDES}
)

# Process CMakeLists in subdirs
add_subdirectory(${SRCS})

add_executable(${target_name}  MACOSX_BUNDLE
${SRCS}/main.cpp 
)

link_directories (${LIBRARY_OUTPUT_PATH})
IF(WIN32 OR APPLE)
  target_link_libraries( ${target_name}
    main command parser gui render methods model ff fourier base math sg
    ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY} ${QT_QTOPENGL_LIBRARY}
    ${FTGL_LIBRARIES} ${OPENGL_LIBRARIES} ${FT2_LIBRARIES} )
ELSE(WIN32 OR APPLE)
  target_link_libraries( ${target_name}
    main command parser gui render methods model ff fourier base math sg
    ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY} ${QT_QTOPENGL_LIBRARY}
    -lGL -lftgl)
ENDIF(WIN32 OR APPLE)


IF(WIN32 OR APPLE)
target_link_libraries(${target_name} 
main command parser gui render methods model ff fourier base math sg
${QT_QTOPENGL_LIBRARY} ${OPENGL_LIBRARIES} ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY}
${READLINE_LIBRARY}
)
ELSE(WIN32 OR APPLE)
target_link_libraries(${target_name} 
main command parser gui render methods model ff fourier base math sg
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

if(UNIX AND NOT APPLE)
  configure_file(${CMAKE_SOURCE_DIR}/desktop.cmake Aten.desktop)
  install(FILES ${CMAKE_BINARY_DIR}/Aten.desktop 
       DESTINATION "/usr/share/applications/")
  install(FILES ${CMAKE_SOURCE_DIR}/src/gui/icons/aten.png
        DESTINATION share/aten)
endif(UNIX AND NOT APPLE)

if (APPLE)
  install(TARGETS ${target_name}
  BUNDLE DESTINATION .
  )
else(APPLE)
  install(TARGETS ${target_name}
      RUNTIME DESTINATION bin
   )
endif(APPLE)

if (NOT APPLE)
   install(DIRECTORY  "${CMAKE_HOME_DIRECTORY}/data/" 
     DESTINATION share/aten
     REGEX ".svn" EXCLUDE
   )
endif(NOT APPLE)
if(WIN32)
  FIND_FILE(Rline readline5.dll ENV PATH)
  FIND_FILE(HRline history5.dll ENV PATH)
  Find_File(qtgui QtGui4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
  Find_File(qtgl QtOpenGL4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
  Find_File(qtcore QtCore4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
  Find_File(qtsvg QtSvg4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
  Find_File(qtxml QtXml4.dll PATHS ${QT_LIBRARY_DIR} NO_DEFAULT_PATH)
  Find_File(qsvg qsvg4.dll PATHS ${QT_PLUGINS_DIR}/iconengines NO_DEFAULT_PATH)
  Find_File(mingwgcc libgcc_s_dw2-1.dll PATHS "C:/MinGW/bin" NO_DEFAULT_PATH)
  Find_File(mingwg++ libstdc++-6.dll PATHS "C:/MinGW/bin" NO_DEFAULT_PATH)
  if(NOT qsvg) 
    Find_File(qsvg qsvgicon4.dll PATHS ${QT_PLUGINS_DIR}/iconengines NO_DEFAULT_PATH)
  endif(NOT qsvg)
  Find_File(qsvgie qsvg4.dll PATHS ${QT_PLUGINS_DIR}/imageformats NO_DEFAULT_PATH)
  install(FILES ${qtgui} ${qtgl} ${qtcore} ${Rline} ${HRline} ${qtsvg} ${qtxml} ${mingwgcc} ${mingwg++} ${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons/aten.ico
   DESTINATION bin
  )
  install(FILES ${qsvg} 
    DESTINATION bin/iconengines
  )
  install(FILES ${qsvgie} 
    DESTINATION bin/imageformats
  )
ENDIF(WIN32)


if(APPLE)
## install the icon for the bundle
  install(FILES "${CMAKE_SOURCE_DIR}/extra/aten.icns" 
        DESTINATION "${target_name}.app/Contents/Resources/")
  install(DIRECTORY  "${CMAKE_HOME_DIRECTORY}/data/"
     DESTINATION ${target_name}.app/Contents/SharedSupport/
     REGEX ".svn" EXCLUDE)
  install(FILES "${CMAKE_SOURCE_DIR}/extra/aten.icns"
        DESTINATION . RENAME ".VolumeIcon.icns")
  install(FILES "${CMAKE_SOURCE_DIR}/extra/background.png"
        DESTINATION .background/) 
  install(FILES "${CMAKE_SOURCE_DIR}/COPYING" 
         DESTINATION . RENAME .COPYING)
  install(FILES "${CMAKE_SOURCE_DIR}/extra/Aten.DS_Store"
         DESTINATION . RENAME .DS_Store)
#  INSTALL(CODE "set(input_file
#        \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${target_name}.app/Contents/MacOS/${target_name}\")")
#  INSTALL(SCRIPT "${CMAKE_SOURCE_DIR}/CMakeIngestOSXBundleLibraries.cmake")
endif(APPLE)









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

IF (APPLE)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/README ${CMAKE_BINARY_DIR}/README.txt COPYONLY)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/COPYING ${CMAKE_BINARY_DIR}/COPYING.txt COPYONLY) 
  SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_BINARY_DIR}/README.txt")
  SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_BINARY_DIR}/COPYING.txt")
ELSE(APPLE)
  SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
  SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING") 
ENDIF(APPLE)

SET(CPACK_PACKAGE_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION_MAJOR ${VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${VERSION_PATCH})
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "Aten${VERSION_MAJOR}.${VERSION_MINOR}")
IF(WIN32)
  SET(CPACK_PACKAGE_INSTALL_DIRECTORY "Aten-${VERSION_MAJOR}.${VERSION_MINOR}")
  SET(CPACK_GENERATOR "NSIS;ZIP")
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons\\\\aten.ico")
  set(CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons\\\\uninstall.ico")
  SET(CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/src/gui/icons\\\\aten.bmp")
  SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\aten.ico")  
  SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} Aten")
  SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.projectaten.net")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.projectaten.net")
  SET(CPACK_NSIS_CONTACT "tris@projectaten.net")
  SET(CPACK_NSIS_MODIFY_PATH ON)  
  SET(CPACK_PACKAGE_EXECUTABLES "aten" "aten")
  set(CPACK_NSIS_CREATE_ICONS_EXTRA "CreateShortCut \\\"$SMPROGRAMS\\\\$STARTMENU_FOLDER\\\\aten.lnk\\\" \\\"$INSTDIR\\\\bin\\\\aten.exe\\\" \\\"\\\" \\\"$INSTDIR\\\\bin\\\\aten.ico\\\"
      StrCmp $INSTALL_DESKTOP \\\"1\\\" 0 +2
      CreateShortCut \\\"$DESKTOP\\\\aten.lnk\\\" \\\"$INSTDIR\\\\bin\\\\aten.exe\\\" \\\"\\\" \\\"$INSTDIR\\\\bin\\\\aten.ico\\\"
"
  )
set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS "
    Push \\\"ATENDATA\\\"
    Push \\\"$INSTDIR\\\\share\\\\aten\\\"
    Call WriteEnvStr      
"
  )
  set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "    
    Push \\\"ATENDATA\\\"
    Call un.DeleteEnvStr 
    StrCmp $INSTALL_DESKTOP \\\"1\\\" 0 +2
    Delete \\\"$DESKTOP\\\\aten.lnk\\\"    
"
)
Endif(WIN32)

IF(UNIX)
  SET(CPACK_GENERATOR "TGZ;TBZ2;RPM;DEB")
Endif(UNIX)

IF(APPLE)
  SET(CPACK_GENERATOR "TGZ")
Endif(APPLE)

SET(CPACK_CMAKE_GENERATOR ${CMAKE_GENERATOR})

INCLUDE(CPack)



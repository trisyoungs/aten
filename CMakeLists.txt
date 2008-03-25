project(aten)

cmake_minimum_required(VERSION 2.4.8)
#cmake_policy(VERSION 2.6)

set (target_name aten)

find_package(Qt4 REQUIRED) # find and setup Qt4 for this project
#set(QT_USE_OPENGL TRUE)
#INCLUDE(${QT_USE_FILE})
#find_package(OpenGL REQUIRED)
#message("${OPENGL_INCLUDE_DIR}")

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(QT_DEFINITIONS "")
ADD_DEFINITIONS(-DQT_SHARED)
ADD_DEFINITIONS(-DHAVE_CONFIG_H)

# tell cmake to process CMakeLists.txt in that subdirectory
add_subdirectory(src)

include_directories(
.
src/templates/
src/
)
add_executable(${target_name}
src/main.cpp
)

message("lib:   ${QT_LIBRARIES}")
message("lib:   ${QT_QTOPENGL_LIBRARY} ${QT_QTCORE_LIBRARY} ${QT_QTGUI_LIBRARY}")
link_directories (${LIBRARY_OUTPUT_PATH})
target_link_libraries(${target_name} 
base command gui render methods model
sg parse classes energy fourier math
${QT_QTOPENGL_LIBRARY} ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY}
)

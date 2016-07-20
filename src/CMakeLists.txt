add_subdirectory(gui)
add_subdirectory(base)
add_subdirectory(sg)
add_subdirectory(math)
add_subdirectory(command)
add_subdirectory(parser)
add_subdirectory(ff)
add_subdirectory(methods)
add_subdirectory(model)
add_subdirectory(main)
add_subdirectory(render)
add_subdirectory(plugins)

if(WIN32)
file(WRITE empty.cpp "")
add_library(mraten SHARED empty.cpp)
target_link_libraries(mraten PRIVATE
	"-Wl,--whole-archive"
	messenger
	fourierdata
	treegui
	gui
	base
	sg
	math
	command
	parser
	ff
	methods
	model
	main
	render
	plugins
	"-Wl,--no-whole-archive"
	Qt5::Widgets
	Qt5::Core
	${FTGL_LIBRARIES}
	${OPENGL_LIBRARIES}
	${FREETYPE_LIBRARIES}
	${READLINE_LIBRARIES}
)
endif(WIN32)

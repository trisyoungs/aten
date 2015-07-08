function(INSTALL_TARGET _TAR _TAR_PUBLIC_HEADERS)
	install(TARGETS
		${_TAR}
		RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
		LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
		ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	)

	foreach(header ${${_TAR_PUBLIC_HEADERS}})
		file(RELATIVE_PATH relative_path ${CMAKE_SOURCE_DIR}/src ${CMAKE_CURRENT_SOURCE_DIR}/${header})
		get_filename_component(directories ${relative_path} DIRECTORY)
		install(FILES
			${header}
			DESTINATION
			${CMAKE_INSTALL_INCLUDEDIR}/aten/${directories}
		)
	endforeach()
endfunction(INSTALL_TARGET)

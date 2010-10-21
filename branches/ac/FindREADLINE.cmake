IF(APPLE)
FIND_PATH(READLINE_INCLUDE_DIR readline/readline.h /usr/include/readline /usr/local/include/readline ENV INCLUDE)
FIND_LIBRARY(READLINE_LIBRARY NAMES readline PATHS /usr/lib /usr/local/lib /usr/lib64 ENV DYLD_LIBRARY_PATH) 
ELSE(APPLE)
FIND_PATH(READLINE_INCLUDE_DIR readline/readline.h /usr/include/readline /usr/local/include/readline ENV INCLUDE)
FIND_LIBRARY(READLINE_LIBRARY NAMES readline PATHS /usr/lib /usr/local/lib /usr/lib64 ENV LD_LIBRARY_PATH) 
ENDIF(APPLE)
IF(WIN32)
FIND_PATH(READLINE_INCLUDE_DIR readline/readline.h /usr/include/readline /usr/local/include/readline ENV INCLUDE )
FIND_LIBRARY(READLINE_LIBRARY NAMES readline PATHS /usr/lib /usr/local/lib /usr/lib64 ENV LIB) 
ENDIF(WIN32)

IF (READLINE_INCLUDE_DIR AND READLINE_LIBRARY)
   SET(READLINE_FOUND TRUE)
ENDIF (READLINE_INCLUDE_DIR AND READLINE_LIBRARY)


IF (READLINE_FOUND)
      MESSAGE(STATUS "Found READLINE: ${READLINE_LIBRARY}")
ELSE (READLINE_FOUND)
      MESSAGE(FATAL_ERROR "Could not find READLINE")
ENDIF (READLINE_FOUND)

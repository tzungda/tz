cmake_minimum_required(VERSION 3.13)
cmake_policy(SET CMP0074 NEW) # recognize <Package>_Root variables

project(tz)

file( GLOB SRCS ${CMAKE_CURRENT_SOURCE_DIR}/*c )
file( GLOB INCS ${CMAKE_CURRENT_SOURCE_DIR}/*h )

# add files
add_executable( ${PROJECT_NAME} ${SRCS} ${INCS} )

set( tz_VERSION 1 )
set( tz_VERSION_MAJOR 0 )

set_target_properties(tz
    PROPERTIES
        CMAKE_POSITION_INDEPENDENT_CODE ON
        VERSION ${tz_VERSION}
        SOVERSION ${tz_VERSION_MAJOR}
)


install( TARGETS ${PROJECT_NAME} LIBRARY DESTINATION lib ARCHIVE DESTINATION exe )
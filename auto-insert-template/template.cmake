cmake_minimum_required(VERSION 3.10.2)
project($1)

set(CMAKE_CXX_STANDARD 11)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING
      "Choose the type of build, options are: Debug Release."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)


if(CMAKE_COMPILER_IS_GNUCXX OR "\${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        set(CMAKE_CXX_FLAGS_DEBUG "\${CMAKE_CXX_FLAGS_DEBUG} -Wall -Wextra -g")
        set(CMAKE_CXX_FLAGS_RELEASE "\${CMAKE_CXX_FLAGS_RELEASE} -O2")
endif()

# Add headers
include_directories("\${CMAKE_CURRENT_SOURCE_DIR}/include")

# Add sources
file(GLOB SOURCES CONFIGURE_DEPENDS
    "\${PROJECT_SOURCE_DIR}/main.cpp"
    "\${PROJECT_SOURCE_DIR}/src/*.cpp"
)

add_executable($1 \${SOURCES})

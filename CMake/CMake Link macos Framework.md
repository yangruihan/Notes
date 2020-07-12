# CMake Link macos Framework

You can't link to a framework this way, you have to use find_library as it includes some special handling for frameworks on OSX.

Also, don't use link_directories, CMake use full paths to libraries and it's not needed.

Here's some simple example with AudioUnit:

```CMakeLists
find_library(AUDIO_UNIT AudioUnit)
if (NOT AUDIO_UNIT)
    message(FATAL_ERROR "AudioUnit not found")
endif()

add_executable(program ${program_SOURCES})
target_link_libraries(program ${AUDIO_UNIT})
```

#----------------------------------------------------------------------
# CMake project file for handy
# Author: Benjamin Menetrier
# Licensing: this code is distributed under the CeCILL-C license
# Copyright © 2017 METEO-FRANCE
#----------------------------------------------------------------------
#
# Set your Fortran/C/C++ compiler (GNU or Intel):
set (COMPILER GNU)
#
# Set your build type (DEBUG or RELEASE):
set (BUILD_TYPE RELEASE)
#
#----------------------------------------------------------------------
# Check
#----------------------------------------------------------------------
#
# Check compiler
if (NOT ${COMPILER} MATCHES GNU AND NOT ${COMPILER} MATCHES Intel)
   message (FATAL_ERROR  "Wrong compiler, abort!")
endif (NOT ${COMPILER} MATCHES GNU AND NOT ${COMPILER} MATCHES Intel)
#
# Check build type
if (NOT ${BUILD_TYPE} MATCHES DEBUG AND NOT ${BUILD_TYPE} MATCHES RELEASE)
   message (FATAL_ERROR  "Wrong build type, abort!")
endif (NOT ${BUILD_TYPE} MATCHES DEBUG AND NOT ${BUILD_TYPE} MATCHES RELEASE)
#
#----------------------------------------------------------------------
# Setup compiler
#----------------------------------------------------------------------
#
# GNU compiler
if (${COMPILER} MATCHES "GNU")
   set (CMAKE_Fortran_COMPILER gfortran)
   set (CMAKE_C_COMPILER mpicc)
   set (CMAKE_CXX_COMPILER mpicxx)
endif (${COMPILER} MATCHES "GNU")
#
# Intel compiler
if (${COMPILER} MATCHES "Intel")
   set (CMAKE_Fortran_COMPILER ifort)
   set (CMAKE_C_COMPILER mpicc)
   set (CMAKE_C++_COMPILER mpicxx)
endif (${COMPILER} MATCHES "Intel")
#
#----------------------------------------------------------------------
# Setup project
#----------------------------------------------------------------------
#
cmake_minimum_required (VERSION 2.6)
project (handy Fortran)
file (GLOB HANDY_SRC
    "src/*.f90"
)
add_executable (handy ${HANDY_SRC})
target_link_libraries(handy)
#
#----------------------------------------------------------------------
# Define compiler flags
#----------------------------------------------------------------------
#
# GNU compiler
if (${COMPILER} MATCHES "GNU")
   set (CMAKE_Fortran_FLAGS_RELEASE "-g -funroll-all-loops -O3 -fopenmp")
   set (CMAKE_Fortran_FLAGS_DEBUG   "-g -Og -Wextra -Wall -pedantic -fbacktrace -ftrapv -fall-intrinsics -fcheck=all -fimplicit-none -ffpe-trap=invalid,zero,overflow -fopenmp")
endif (${COMPILER} MATCHES "GNU")
#
# Intel compiler
if (${COMPILER} MATCHES "Intel")
   set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -nowarn -qopenmp")
   set (CMAKE_Fortran_FLAGS_DEBUG   "-g -O0 -nowarn -g -traceback -fpe0 -check all,noarg_temp_created -qopenmp")
endif (${COMPILER} MATCHES "Intel")
#
# Set linker flags
set (LINKER_FLAGS "-fopenmp")
set_target_properties (handy PROPERTIES LINKER_LANGUAGE Fortran)
set (CMAKE_EXE_LINKER_FLAGS ${LINKER_FLAGS})
#
# Set build type
set (CMAKE_BUILD_TYPE ${BUILD_TYPE} CACHE STRING "Choose the type of build, options are: Debug/Release" FORCE)
#
#----------------------------------------------------------------------
# Define building directories
#----------------------------------------------------------------------
#
set (CMAKE_FILES_DIRECTORY ${PROJECT_BINARY_DIR})
set (EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/../run)
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../run)

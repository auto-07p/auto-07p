# plaut04 -- 3D bifurcation/solution visualizer (Qt5 + Coin3D + SoQt).
#
# CMake port of the qmake/autotools build (plaut04/src/Makefile.in). Enabled by
# the top-level -DBUILD_3D_GUI=ON. Builds the SoQt (Qt) toolkit variant, matching
# what ./configure selects when SoQt is available (USE_SOQT); the SoXt/Motif
# variant (gmainxt.c++) is not built here.
#
# Requires Qt5, Coin3D, and SoQt (all ship CMake package configs). SoQt::SoQt
# transitively pulls in Coin and the needed Qt5 modules.

find_package(Qt5 COMPONENTS Core Gui Widgets OpenGL PrintSupport REQUIRED)
find_package(Coin REQUIRED)
find_package(SoQt REQUIRED)

# gmainqt.h has Q_OBJECT; let CMake run moc on it. The generated moc is compiled
# as its own translation unit (gmainqt.c++ does not #include it), which is
# exactly what AUTOMOC produces here.
set(CMAKE_AUTOMOC ON)

set(PLAUT04_SOURCES
  # core
  src/axis.c++ src/bifurcation.c++ src/createBoundingBox.c++
  src/createCoords.c++ src/createDisk.c++ src/createLegend.c++
  src/createSphere.c++ src/gmain.c++ src/polygon3d.c++ src/printToPS.c++
  src/readFM.c++ src/rounding.c++ src/solution.c++ src/stringtrim.c++
  src/tube.c++
  # SoQt toolkit front-end (moc'd)
  src/gmainqt.c++
  # restricted three-body (r3b) support, linked into the same executable
  src/r3b/createLibPoint.c++ src/r3b/createPrimary.c++ src/r3b/gmainr3b.c++
  src/r3b/toInertialFrame.c++)

add_executable(plaut04 ${PLAUT04_SOURCES})

target_include_directories(plaut04 PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/includes"   # gplaut04.h, gVarNames.h
  "${CMAKE_CURRENT_SOURCE_DIR}/src")       # so AUTOMOC finds gmainqt.h

target_compile_definitions(plaut04 PRIVATE USE_SOQT)

target_link_libraries(plaut04 PRIVATE
  SoQt::SoQt Coin::Coin
  Qt5::Core Qt5::Gui Qt5::Widgets Qt5::OpenGL Qt5::PrintSupport)

# Deposit alongside the autotools build's output (AUTO_DIR/bin).
set_target_properties(plaut04 PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")

# Package: install the executable plus its runtime resources (config .rc files
# and the widgets/ assets it loads), matching the autotools install.
include(GNUInstallDirs)
install(TARGETS plaut04 RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
install(FILES plaut04.rc r3bplaut04.rc
        DESTINATION "${CMAKE_INSTALL_DATADIR}/auto/plaut04")
install(DIRECTORY widgets
        DESTINATION "${CMAKE_INSTALL_DATADIR}/auto/plaut04")

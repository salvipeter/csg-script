# -*- mode: Makefile -*-

TARGET = csg-script
CONFIG += c++14 qt opengl release link_pkgconfig
QT += gui widgets opengl openglwidgets xml
PKGCONFIG += guile-3.0
DC = ../dual-contouring
MC = ../marching
BL = ../polygonizer
GEOM = ../libgeom
QGL = ../../share/libQGLViewer

HEADERS = MyWindow.h MyViewer.h MyViewer.hpp
SOURCES = MyWindow.cpp MyViewer.cpp main.cpp

QMAKE_CXXFLAGS += -O3

INCLUDEPATH += /usr/include/eigen3 $${QGL} $${DC} $${MC} $${BL} $${GEOM}
LIBS += -L$${QGL}/QGLViewer -lQGLViewer -L/usr/lib/OpenMesh -lOpenMeshCore -lGL -lGLU -L$${DC}/build -ldualcontour -L$${MC}/build -lmarching -L$${BL}/build -lpolygonizer -L$${GEOM}/release -lgeom -lomp

RESOURCES = csg-script.qrc

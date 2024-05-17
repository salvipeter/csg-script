# -*- mode: Makefile -*-

TARGET = csg-script
CONFIG += c++14 qt opengl release link_pkgconfig
QT += gui widgets opengl openglwidgets xml
PKGCONFIG += guile-3.0
DC = ../dual-contouring
QGL = ../../share/libQGLViewer

HEADERS = MyWindow.h MyViewer.h MyViewer.hpp
SOURCES = MyWindow.cpp MyViewer.cpp main.cpp

QMAKE_CXXFLAGS += -O3

INCLUDEPATH += /usr/include/eigen3 $${QGL} $${DC}
LIBS += -L$${QGL}/QGLViewer -lQGLViewer-qt6 -L/usr/lib/OpenMesh -lOpenMeshCore -lGL -lGLU -L$${DC}/build -ldualcontour -lomp

RESOURCES = csg-script.qrc

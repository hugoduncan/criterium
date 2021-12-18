JAVA_HOME ?= /Library/Java/JavaVirtualMachines/jdk-11.0.12.jdk/Contents/Home
JDK ?= $(JAVA_HOME)

OSNAME ?= darwin

LIBNAME = criterium

# Linux GNU C Compiler
ifeq ($(OSNAME), linux)
    CC=gcc
    PLATFORM_CFLAGS=-std=c++17 -O2 -fPIC -shared
    CFLAGS += -I$(JDK)/include -I$(JDK)/include/$(OSNAME)
    LIBRARY=lib$(LIBNAME).so
    LDFLAGS=
endif

ifeq ($(OSNAME), darwin)
    CC=clang
    PLATFORM_CFLAGS=-std=c++17 -g -shared
    CFLAGS += -I$(JDK)/include -I$(JDK)/include/$(OSNAME)
    LIBRARY=lib$(LIBNAME).dylib
    LDFLAGS=-lstdc++
endif

all:
	$(CC) $(PLATFORM_CFLAGS) $(CFLAGS) $(LDFLAGS) -o$(LIBRARY) agent.cpp

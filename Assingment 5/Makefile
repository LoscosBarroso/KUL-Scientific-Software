.PHONY: clean all info

HOME_EXERCISE = 
TARGETS := srda srda_d time_xtx time_xtx_d
SOURCES := srda.cpp
OBJS    := $(SOURCES:.cpp=.o) $(SOURCES:.cpp=_d.o) $(SOURCES:.cpp=_p.o)


DEBUGFLAGS:= -g 
RELEASEFLAGS:= -O3 -DNDEBUG
PARALLELFLAGS:= -D_GLIBCXX_PARALLEL -fopenmp -pthread
CXXFLAGS := -Wall -std=c++17
LDFLAGS  := 
LIBS =   -lstdc++ 
OPENMPLIB =-L/usr/lib -fopenmp -pthread

EXAMPLE_DEPS = Makefile


all: $(TARGETS)

clean:
	/bin/rm -f $(OBJS) $(TARGETS)

info:
	@echo Compiler:	 CXX	  = $(CXX)
	@echo Compile command: COMPILE.cc  = $(COMPILE.cc)
	@echo Link command:    LINK.cc     = $(LINK.cc)

srda.o: srda.cpp $(EXAMPLE_DEPS)
	 @$(CXX) -c $(CXXFLAGS) $(RELEASEFLAGS)  -o srda.o srda.cpp
srda: srda.o 
	 @$(CXX) -o srda $(LDFLAGS) srda.o $(LIBS)
srda_d.o: srda.cpp $(EXAMPLE_DEPS)
	 @$(CXX) -c $(CXXFLAGS) $(DEBUGFLAGS)  -o srda_d.o srda.cpp
srda_d: srda_d.o
	 @$(CXX) -o srda_d $(LDFLAGS) srda_d.o $(LIBS)
time_xtx.o: time_xtx.cpp $(EXAMPLE_DEPS)
	 @$(CXX) -c $(CXXFLAGS) $(RELEASEFLAGS)  -o time_xtx.o time_xtx.cpp
time_xtx: time_xtx.o 
	 @$(CXX) -o time_xtx $(LDFLAGS) time_xtx.o $(LIBS)
time_xtx_d.o: time_xtx.cpp $(EXAMPLE_DEPS)
	 @$(CXX) -c $(CXXFLAGS) $(DEBUGFLAGS)  -o time_xtx_d.o time_xtx.cpp
time_xtx_d: time_xtx_d.o
	 @$(CXX) -o time_xtx_d $(LDFLAGS) time_xtx_d.o $(LIBS)

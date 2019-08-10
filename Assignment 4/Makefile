.PHONY: clean all info

HOME_EXERCISE = 
TARGETS := srda srda_d functors functors_d
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

functors.o: functors.cpp $(EXAMPLE_DEPS)
	 @$(CXX) -c $(CXXFLAGS) $(RELEASEFLAGS)  -o functors.o functors.cpp
functors: functors.o 
	 @$(CXX) -o srda $(LDFLAGS) srda.o $(LIBS)
functors_d.o: functors.cpp $(EXAMPLE_DEPS)
	 @$(CXX) -c $(CXXFLAGS) $(DEBUGFLAGS)  -o functors_d.o functors.cpp
functors_d: functors_d.o
	 @$(CXX) -o functors_d $(LDFLAGS) functors_d.o $(LIBS)


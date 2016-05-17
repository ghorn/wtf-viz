ifdef VERBOSE
Q =
endif
Q ?= @


all: libwtfviz2.so

CXX = g++

OPTFLAGS = -O2

CXXFLAGS = $(OPTFLAGS) -g `pkg-config --cflags OGRE-Overlay OIS`
CXXFLAGS += -Wall -Wextra -Werror -fPIC

LDFLAGS = `pkg-config --libs OGRE OGRE-Overlay OIS`

CPP_SRC = \
	cbits/OgreFramework.cpp \
	cbits/WtfViz2.cpp \
	cbits/Interface.cpp

CPP_HEADERS = \
	cbits/OgreFramework.hpp \
	cbits/WtfViz2.hpp

CPP_OBJ = $(CPP_SRC:%.cpp=%.o)

libwtfviz2.so: $(CPP_OBJ)
	@echo $(CXX) $@
	$(Q)$(CXX) -shared -o $@ $(CPP_OBJ) $(LDFLAGS)

src/%.o: src/%.cpp src/%.hpp
	@echo $(CXX) $@
	$(Q)$(CXX) $(CXXFLAGS) -c $< -o $@

.PHONY: clean

clean:
	rm -f libwtfviz2.so cbits/*.o

STYLE_FILTER = \
	-legal/copyright, \
	-build/include_order, \

.PHONY: lint
lint:
	./cpplint.py --filter="$(STYLE_FILTER)" --counting=detailed --extensions=c,cpp,h,hpp --linelength=100 $(CPP_SRC) $(CPP_HEADERS)

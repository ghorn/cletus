CXX = g++ -O3 -Wall -L/usr/local/lib
MAIN_BINARIES = $(basename $(wildcard *Client.cpp)) $(basename $(wildcard *Server.cpp))
TEST_BINARIES = $(basename $(wildcard *Test.cpp))
HEADER = $(wildcard *.h)
OBJECTS = $(addsuffix .o, $(basename $(filter-out %Server.cpp %Client.cpp, $(wildcard *.cpp))))
CPPLINT_PATH = ../lib/cpplint.py

.PRECIOUS: %.o

all: compile test

compile: $(MAIN_BINARIES) $(TEST_BINARIES)

test: $(TEST_BINARIES)
	for T in $(TEST_BINARIES); do ./$$T; done

checkstyle:
	python $(CPPLINT_PATH) *.h *.cpp

clean:
	rm -f *.o
	rm -f $(MAIN_BINARIES)
	rm -f $(TEST_BINARIES)
	rm -f *.class
	rm -f *Test.TMP.*
	rm -f core

%Client: %Client.o $(OBJECTS)
	$(CXX) -o $@ $^

%Server: %Server.o $(OBJECTS)
	$(CXX) -o $@ $^

%Test: %Test.o $(OBJECTS)
	$(CXX) -o $@ $^ -lgtest -lgtest_main -lpthread

%.o: %.cpp $(HEADER)
	$(CXX) -c $<

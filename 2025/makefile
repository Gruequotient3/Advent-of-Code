PROG = main

DEBUG = -fsanitize=address

CC = g++
CXXFLAGS = -Wall -Wextra
LDFLAGS = -lm

CXXSRC += $(wildcard ./*.cpp) $(wildcard ./**/*.cpp)

OBJ = $(CXXSRC:.cpp=.o)

RM = rm -rf

all: $(PROG)

debug: $(OBJ)
	$(CC) $(DEBUG) $(OBJ) $(LDFLAGS) -o $(PROG)

$(PROG): $(OBJ)
	$(CC) $(OBJ) $(LDFLAGS) -o $(PROG)

%.o: %.cpp
	$(CC) $(CXXFLAGS) -c $< -o $@ 

clean:
	$(RM) $(OBJ) $(PROG)

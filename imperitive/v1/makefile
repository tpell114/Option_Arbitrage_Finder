

program: clean main.o OptionChain.o Problem.o
	clang++ -g main.o OptionChain.o Problem.o -o program
	rm -f *.o

main.o: main.cpp
	clang++ -c main.cpp

OptionChain.o: OptionChain.cpp
	clang++ -c OptionChain.cpp

Problem.o: Problem.cpp
	clang++ -c Problem.cpp

clean:
	rm -f program *.o
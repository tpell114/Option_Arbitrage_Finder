

program: clean main.o option_chain.o
	clang++ -g main.o option_chain.o -o program
	rm -f *.o

main.o: main.cpp
	clang++ -c main.cpp

option_chain.o: option_chain.cpp
	clang++ -c option_chain.cpp

clean:
	rm -f program *.o
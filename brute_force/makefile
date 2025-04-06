bruteforce: clean BruteForceDriver.o OptionChain.o BruteForce.o
	clang++ -g BruteForceDriver.o OptionChain.o BruteForce.o -o bruteforce
	rm -f *.o

BruteForceDriver.o: BruteForceDriver.cpp
	clang++ -c BruteForceDriver.cpp

BruteForce.o: BruteForce.cpp
	clang++ -c BruteForce.cpp

OptionChain.o: OptionChain.cpp
	clang++ -c OptionChain.cpp

clean:
	rm -f bruteforce *.o
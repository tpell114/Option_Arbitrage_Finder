#include "OptionChain.h"

int main() {


    OptionChain chain;


    chain.load_from_file("data.txt");


    chain.print_chain();





    return 0;
}
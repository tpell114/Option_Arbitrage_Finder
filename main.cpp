#include "option_chain.h"

int main() {


    option_chain chain;



    chain.load_from_file("data.txt");



    chain.print_chain();










    return 0;
}
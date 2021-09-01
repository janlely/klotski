#include <iostream>
#include <sstream>
#include "klotski.hpp"
#include <set>
using namespace std;

int main() {

    string line;
    std::getline(std::cin, line);
    std::stringstream ss(line);
    int nr, nc;
    ss >> nr >> nc;
    Klotski klo(nr, nc);
    for (int i = 0; i < nr; i++) {
        std::getline(std::cin, line);
        std::stringstream sss(line);
        for (int j = 0; j < nc; j++) {
            string name;
            sss >> name;
            klo.updateSituation(name);
        }
    }
    std::getline(std::cin, line);
    klo.setTarget(line);
    std::getline(std::cin, line);
    std::stringstream sss(line);
    int x,y;
    sss >> x >> y;
    klo.setDestPost(x, y);
    std::cout << "finish input" << std::endl;
    klo.klotski(); 
    return 0;
}

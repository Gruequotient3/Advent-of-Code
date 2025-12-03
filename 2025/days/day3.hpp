#ifndef DAY3_HPP
#define DAY3_HPP

#include <vector>

namespace Day3{
    class Banks{
        public:
            std::vector<unsigned char> voltage;

            Banks(std::vector<unsigned char> voltage);
            int findLargest2Digits();
    };
    
    int part1();
    std::vector<Banks> getData();
}


#endif

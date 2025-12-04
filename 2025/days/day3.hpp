#ifndef DAY3_HPP
#define DAY3_HPP

#include <vector>



namespace Day3{
    typedef unsigned long long ullint;
    class Banks{
        public:
            std::vector<unsigned char> voltage;

            Banks(std::vector<unsigned char> voltage);
            int findLargest2Digits();
            int findNextDigit(int prevIndex, int pos, int& index);
            ullint findLargest12Digits();
    };
    
    int part1();
    ullint part2();
    std::vector<Banks> getData();
}


#endif

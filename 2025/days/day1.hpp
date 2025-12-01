#ifndef DAY1_HPP
#define DAY1_HPP

#include <vector>

namespace Day1{
    struct Rotation{
        char rot;
        int val;

        Rotation(char rot, int val) : rot{rot}, val{val} { }
    };

    int part1();
    int part2();
    int applyRotation(int value, Rotation& rot);
    int getZeroClicks(int value, Rotation& rot);
    std::vector<Rotation> getData();
}
#endif

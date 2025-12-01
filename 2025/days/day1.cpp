#include <iostream>
#include <string>
#include <fstream>

#include "day1.hpp"


using namespace Day1;

int Day1::part1(){
    std::vector<Rotation> rotations = getData();
        
    int zeroCounter = 0;
    int dvalue = 50;

    for (unsigned long i = 0; i < rotations.size(); ++i){
        dvalue = applyRotation(dvalue, rotations[i]);
        if (!dvalue) ++zeroCounter;
    }
    return zeroCounter;
}

int Day1::part2(){
    std::vector<Rotation> rotations = getData();
    int zeroCounter = 0;
    int dvalue = 50;

    for (unsigned long i = 0; i < rotations.size(); ++i){
        zeroCounter += getZeroClicks(dvalue, rotations[i]);
        dvalue = applyRotation(dvalue, rotations[i]);
    }
    return zeroCounter;
}

int Day1::applyRotation(int value, Rotation& rot){
    int newValue = (rot.rot == 'R') ? value + rot.val : value - rot.val;
    return (newValue % 100) < 0 ? newValue % 100 + 100 : newValue % 100;
}

int Day1::getZeroClicks(int value, Rotation& rot){
    int zeroCounter = rot.val / 100;
    int remainder = (rot.val % 100 < 0) ? rot.val % 100 + 100 : rot.val % 100;
    int newValue = rot.rot == 'R' ? value + remainder : value - remainder;

    if (value == 0) return zeroCounter;
    else if (newValue <= 0 || newValue >= 100) return zeroCounter + 1; 
    return zeroCounter;
}


std::vector<Rotation> Day1::getData(){
    std::string line;
    std::vector<Rotation> rotations;
    
    std::ifstream file("./data/day1.txt");
    while (std::getline(file, line)){
        rotations.push_back({line[0], std::stoi(&line[1])});
    }
    file.close();
    return rotations;
}


#include "day3.hpp"

#include <iostream>
#include <cstdio>
#include <fstream>
#include <string>
#include <array>


using namespace Day3;

Day3::Banks::Banks(std::vector<unsigned char> voltage) : voltage{voltage} { }
int Day3::Banks::findLargest2Digits(){
    int temp, largest = 0;
    for (unsigned long i = 0; i < voltage.size(); ++i){
        for (unsigned long j = i+1; j < voltage.size(); ++j){
            temp = (int)voltage[i] * 10 + (int)voltage[j];
            if (largest < temp) largest = temp;
        }
    }
    return largest;
}

int Day3::Banks::findNextDigit(int prevIndex, int pos, int& index){
    int i = voltage.size() - 12 + pos;
    unsigned char max = 0;
    index = i;
    while (i > prevIndex){
        if (voltage[i] >= max){
            index = i;
            max = voltage[i];
        }
        i--;
    }
    return max;
}

ullint Day3::Banks::findLargest12Digits(){
    std::array<unsigned int, 12> digits{};
    ullint largest = 0;
    int index = -1;
    for (int i = 0; i < 12; ++i){
        digits[i] = findNextDigit(index, i, index);
    }
    
    for (int i = 0; i < 12; ++i){
        largest += (ullint)digits[i];
        if (i < 11) largest *= 10;
    }
    return largest;
}


int Day3::part1(){
    int sumVoltage = 0;
    std::vector<Banks> banks = getData();
    
    for (Banks bank : banks){
        sumVoltage += bank.findLargest2Digits();
    }
    return sumVoltage;
}


ullint Day3::part2(){
    ullint sumVoltage = 0;
    std::vector<Banks> banks = getData();
    for (Banks bank: banks){
        ullint num = bank.findLargest12Digits();
        sumVoltage += num;
    }
    return sumVoltage;
}

std::vector<Banks> Day3::getData(){
    std::string line;
    std::vector<Banks> banks;
    std::vector<unsigned char> voltage;

    std::ifstream file("./data/day3.txt");
    while (std::getline(file, line)){
        voltage.clear();
        for (unsigned char i : line){
            if (i < '0' && i > '9') continue;
            i -= (char)'0';
            voltage.push_back(i);
        }
        banks.push_back({voltage});
    }
    file.close();
    return banks;
}





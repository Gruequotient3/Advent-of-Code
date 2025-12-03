#include "day3.hpp"

#include <iostream>
#include <fstream>
#include <string>



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

int Day3::part1(){
    int sumVoltage = 0;
    std::vector<Banks> banks = getData();
    for (Banks bank : banks){
        sumVoltage += bank.findLargest2Digits();
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





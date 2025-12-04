#include <cstdio>

#include "./days/day1.hpp"
#include "./days/day3.hpp"
#include "./days/day4.hpp"

int main(){

    printf("--------- Day 1 --------\n");
    printf("Part 1 : %d\n", Day1::part1());
    printf("Part 2 : %d\n", Day1::part2());
    
    printf("\n--------- Day 3 --------\n");
    printf("Part 1 : %d\n", Day3::part1());
    printf("Part 2 : %lld\n", Day3::part2());
    
    printf("\n--------- Day 4 --------\n");
    printf("Part 1 : %d\n", Day4::part1());
    printf("Part 2 : %d\n", Day4::part2());
    return 0;
}

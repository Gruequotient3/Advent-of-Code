#include "day4.hpp"

#include <cstdio>
#include <fstream>


using namespace Day4;

Day4::Grid::Grid() : width{0}, height{0}, grid{nullptr} { }
Day4::Grid::~Grid() {
    if (grid){
        for (int y = 0; y < height; ++y) delete grid[y];
        delete grid;
    }
}

void Day4::Grid::showGrid(){
    for (int y = 0; y < height; ++y){
        for (int x = 0; x < width; ++x){
            char value = grid[y][x];
            printf("%c ",  value == 0 ? '.' : value == 1 ? '@' : 'x');
        }
        printf("\n");
    }   
}

bool Day4::Grid::checkAround(int x, int y){
    int direction[] = {
        -1, -1,
         0, -1,
         1, -1,
        -1,  0,
         1,  0,
        -1,  1,
         0,  1,
         1,  1
    };
    
    int dx, dy;
    int nbRolls = 0;
    char value;
    for (int i = 0; i < 8; ++i){
        dx = x + direction[i * 2];
        dy = y + direction[i * 2 + 1];
        if ((dx < 0 || dx >= width) || (dy < 0 || dy >= height)) continue;
        value = grid[dy][dx];
        if (value == 1 || value == 2) ++nbRolls;
        if (nbRolls == 4) return false;
    }
    return true;
}

void Day4::Grid::removeAccessibleRolls(){
    for (int y = 0; y < height; ++y){
        for (int x = 0; x < width; ++x){
            if (grid[y][x] == 2) grid[y][x] = 0;
        }
    }
}

int Day4::Grid::getPos(int x, int y){
    return x + width * y;
}

char& Day4::Grid::operator[](int pos){
    int x, y;
    x = pos % width;
    y = pos / width;
    return grid[y][x];
}


int Day4::part1(){
    Grid grid = getData();
    int pos;
    int nbAccessibleRolls = 0;
    for (int y = 0; y < grid.height; ++y){
        for (int x = 0; x < grid.width; ++x){
            pos = grid.getPos(x, y);
            if (!grid[pos]) continue;
            if (!grid.checkAround(x, y)) continue;
            grid[pos] = 2;
            ++nbAccessibleRolls;
        }
    }
    return nbAccessibleRolls;
}

int Day4::part2(){
    Grid grid = getData();
    int removeRolls = 0;
    int removedRolls; 
    do{
        removedRolls = 0;
        int pos;
        for (int y = 0; y < grid.height; ++y){
            for (int x = 0; x < grid.width; ++x){
                pos = grid.getPos(x, y);
                if (!grid[pos]) continue;
                if (!grid.checkAround(x, y)) continue;
                grid[pos] = 2;
                ++removedRolls;
            }
        }
        removeRolls += removedRolls;
        grid.removeAccessibleRolls();
    } while(removedRolls);
    return removeRolls;
}

Grid Day4::getData(){
    std::string line;
    Grid grid;
    
    int width = 0;
    int height = 0;
    std::ifstream file("./data/day4.txt");
    while (std::getline(file, line)){
        if (!width) width = line.size();
        ++height;
    }
    grid.width = width;
    grid.height = height;
    
    grid.grid = new char*[height];
    for (int y = 0; y < height; ++y){
        grid.grid[y] = new char[width];
    }

    int y = 0;
    file.clear();
    file.seekg(0);
    while (std::getline(file, line)){
        for (unsigned long i = 0; i < line.size(); ++i){
            grid.grid[y][i] = line[i] == '.' ? 0 : 1;
        }
        ++y;
    }
    file.close();
    return grid;
}




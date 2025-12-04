#ifndef DAY4_HPP
#define DAY4_HPP

#include <map>

namespace Day4{
    class Grid{
        public:
            int width;
            int height;
            char** grid;
            
            Grid();
            ~Grid();
            
            void showGrid();
            bool checkAround(int x, int y);
            void removeAccessibleRolls();

            int getPos(int x, int y);
            char& operator[](int pos);

    };
    
    int part1();
    int part2();
    Grid getData();
}



#endif

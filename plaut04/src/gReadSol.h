#ifndef GREADSOL_H
#define GREADSOL_H

#include <queue>

class solutions {
  public:
    void parse(const char* sFileName, bool & blOpenFile, long int &total, 
               long int &totalNumPoints);
    bool read(const char* sFileName, int varIndices[]);
  private:
    std::queue<long> positions;
};

#endif

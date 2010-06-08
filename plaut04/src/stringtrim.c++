#include "stringtrim.h"

#include <cctype>
#include "gplaut04.h"

void strrighttrim(std::string& str)
{
    std::string::iterator it;

    for (it=str.end()-1; it > str.begin() && std::isspace(*it); it--);
    /* it now points to the right-most non-space character */
    str.erase(it+1, str.end());
}



void strlefttrim(std::string& str)
{
    std::string::iterator it;

    for (it=str.begin(); it < str.end() && std::isspace(*it); it++);
    // it now points to the first non-space character,
    str.erase(str.begin(), it);
}

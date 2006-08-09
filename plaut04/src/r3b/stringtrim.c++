#include <string.h>
#include <ctype.h>
/////////////////////////////////////////////////////
//
char* strrighttrim(char * arr)
//
/////////////////////////////////////////////////////
{
    int i;
    for (i=strlen(arr)-1; i >= 0 && isspace(arr[i]); i--);
    arr[i+1] = '\0';

    return arr;
}


/////////////////////////////////////////////////////
//
char* strlefttrim(char * arr)
//
/////////////////////////////////////////////////////
{
    int i, j, n, pos;
    n = strlen(arr);
    for (i=0; i < n && isspace(arr[i]); i++);

    if (i != 0)
    {
        if (i == n)
            arr[0] = '\0';
        else
        {
            pos = i;
            n = strlen(arr) - pos + 1;
            for (i=pos, j=0; i < n+pos; i++, j++) arr[j] = arr[i];
            arr[n] = '\0';
        }
    }
    return arr;
}

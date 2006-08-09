#include <string.h>
#include <ctype.h>

char* strrighttrim(char * arr)
{
    int i;
    for (i=strlen(arr)-1; i >= 0 && isspace(arr[i]); i--);
    /* i now points to the right-most non-space character, or is -1 if the string were all spaces */
    arr[i+1] = '\0';
    /* if i == -1, then arr now contains empty string */
    return arr;
}



char* strlefttrim(char * arr)
{
    int i, j, n, pos;
    n = strlen(arr);
    for (i=0; i < n && isspace(arr[i]); i++);
    // i now points to the first non-space character,
    // or is n if the string were all spaces

    if (i != 0) // there are no leading spaces then do nothing
    {
        if (i == n) // test for all spaces
            arr[0] = '\0';
        else
        {
            pos = i;
            n = strlen(arr) - pos + 1; // include null terminator
            for (i=pos, j=0; i < n+pos; i++, j++) arr[j] = arr[i];
            // move string to the beginning
            arr[n] = '\0';
        }
     }
    return arr;
}


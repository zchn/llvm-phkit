#include <stdio.h>

void deja_vu()
{
    char door[8];
    printf("location of deja_vu: %p\n", deja_vu);
    printf("location of door: %p\n", door);
    scanf("%s", door);
    printf("%s", door);
}

int main()
{
    deja_vu();
    return 0;
}

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

char deja_vu(uint64_t len)
{
    char* door = malloc(8);
    printf("location of deja_vu: %p\n", deja_vu);
    printf("location of door: %p\n", door);
    fread(door, len, 1, stdin);

    char door_0 = door[0];
    free(door);
    return door_0;
}

int main(int argc, char* argv[])
{
    printf("%c\n", deja_vu(strtoull(argv[1], NULL, 10)));
    return 0;
}

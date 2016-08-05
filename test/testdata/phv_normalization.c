#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

struct MsgPrinter {
    int (*fptr)(const char* msg);
};

int printstr(const char* msg)
{
    return printf("%s", msg);
}

char deja_vu(uint64_t len)
{
    char* phv1987 = malloc(8);
    struct MsgPrinter* msg_printer = malloc(sizeof(struct MsgPrinter));
    msg_printer->fptr = printstr;
    printf("location of deja_vu: %p\n", deja_vu);
    printf("location of phv1987: %p\n", phv1987);
    printf("location of msg_printer->fptr before fread: %p\n",
           msg_printer->fptr);
    fread(phv1987, len, 1, stdin);
    printf("location of msg_printer->fptr after fread: %p\n",
           msg_printer->fptr);
    msg_printer->fptr("pwn me here\n");
    char phv1987_0 = phv1987[0];
    free(phv1987);
    return phv1987_0;
}

int main(int argc, char* argv[])
{
    if (argc <= 1) {
        return -1;
    }
    printf("%c\n", deja_vu(strtoull(argv[1], NULL, 10)));
    return 0;
}

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

const char * filename = "Klondike";

int main(void) {
    int error = rmdir(filename);
    if (error == 0) {
        return 0;
    }

    if (errno == EACCES) {
        puts("cuck");
    } else {
        puts("nope)");
    }

    printf("error: %s\n", strerror(errno));
    return -1;
}

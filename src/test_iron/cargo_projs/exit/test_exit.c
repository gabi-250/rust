#include <stdio.h>
#include <stdlib.h>

extern void call_exit(int);

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Expected at least one argument. Exiting.\n");
        return EXIT_FAILURE;
    } else {
        call_exit(atoi(argv[1]));
    }
}

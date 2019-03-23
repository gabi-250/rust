#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

extern uint64_t fibonacci(uint64_t);

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Expected at least one argument. Exiting.\n");
        return EXIT_FAILURE;
    } else {
        printf("%lld\n", fibonacci(atol(argv[1])));
    }
}

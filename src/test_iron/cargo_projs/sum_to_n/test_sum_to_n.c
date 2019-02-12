#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

extern uint16_t sum_to_n(uint16_t);
extern uint16_t iter_sum_to_n(uint16_t);

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Expected at least one argument. Exiting.\n");
        return EXIT_FAILURE;
    } else {
        uint16_t x = atoi(argv[1]);
        printf("%lld %lld", sum_to_n(x), iter_sum_to_n(x));
    }
}

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

extern uint8_t add_u8(uint8_t, uint8_t);
extern uint16_t add_u16(uint16_t, uint16_t);
extern uint32_t add_u32(uint32_t, uint32_t);
extern uint64_t add_u64(uint64_t, uint64_t);

extern int8_t add_i8(int8_t, int8_t);
extern int16_t add_i16(int16_t, int16_t);
extern int32_t add_i32(int32_t, int32_t);
extern int64_t add_i64(int64_t, int64_t);

int main(int argc, char **argv) {
    if (argc < 5) {
        fprintf(stderr, "Expected at least 4 arguments. Exiting.\n");
        return EXIT_FAILURE;
    } else {
        uint64_t x = strtoull(argv[1], NULL, 10);
        uint64_t y = strtoull(argv[2], NULL, 10);
        uint8_t bits = atoi(argv[3]);
        int signed_ints = argv[4][0] == 'i' ? 1 : 0;
        if (signed_ints) {
            if (bits == 8) {
                printf("%d", add_i8(x, y));
            } else if (bits == 16) {
                printf("%d", add_i16(x, y));
            } else if (bits == 32) {
                printf("%d", add_i32(x, y));
            } else if (bits == 64) {
                printf("%lld", add_i64(x, y));
            } else {
                fprintf(stderr, "Unsupported instruction size %d\n", bits);
            }
        } else {
            if (bits == 8) {
                printf("%llu", add_u8(x, y));
            } else if (bits == 16) {
                printf("%llu", add_u16(x, y));
            } else if (bits == 32) {
                printf("%llu", add_u32(x, y));
            } else if (bits == 64) {
                printf("%llu", add_u64(x, y));
            } else {
                fprintf(stderr, "Unsupported instruction size %d\n", bits);
            }
        }
    }
}


#include <stdio.h>
#include <stdlib.h>

extern int inc(int *);
extern int references(int);
extern int *references2(int *);
extern int *references3(int *, int);

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "Expected at least two arguments. Exiting.\n");
        return EXIT_FAILURE;
    }
    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    printf("%d ", x);
    printf("%d ", inc(&x));
    printf("%d ", x);
    printf("%d ", references(x));
    printf("%d ", x);
    printf("%d ", *references2(&x));
    printf("%d ", x);
    printf("%d ", *references3(&x, y));
    printf("%d %d\n", x, y);
    return 0;
}

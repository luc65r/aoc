#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

typedef uint64_t u64;

u64 nextMove(u64 current, u64 *next, u64 max) {
    u64 picked[3];
    picked[0] = next[current];
    picked[1] = next[picked[0]];
    picked[2] = next[picked[1]];
    next[current] = next[picked[2]];

    u64 destination = current - 1;
    destination = destination ? destination : max;
    while (picked[0] == destination
            || picked[1] == destination
            || picked[2] == destination)
        destination = --destination ? destination : max;

    next[picked[2]] = next[destination];
    next[destination] = picked[0];

    return next[current];
}

int main() {
    char labels[10];

    if (!fgets(labels, sizeof labels, stdin)) {
        fprintf(stderr, "Couldn't read stdin");
        return EXIT_FAILURE;
    }

    u64 next1[10];
    u64 next2[1000001];

    /* Make the array from what we got from stdin */
    for (int i = 0; i < 9; i++)
        next1[labels[i] - '0'] = labels[i + 1] - '0';
    /* The arrays share the same base */
    memcpy(next2, next1, sizeof next1);

    /* Make the rest of the second array */
    next2[labels[8] - '0'] = 10;
    for (int i = 10; i < 1000000; i++)
        next2[i] = i + 1;

    u64 current1, current2 = current1 = labels[0] - '0';
    /* Make sure they wrap */
    next1[labels[8] - '0'] = current1;
    next2[1000000] = current2;

    /* Run the 100 moves for part 1 */
    for (int i = 0; i < 100; i++)
        current1 = nextMove(current1, next1, 9);
    /* Print the labels */
    for (int i = next1[1]; i != 1; i = next1[i])
        putchar('0' + i);
    putchar('\n');

    /* Run the 10 000 000 moves for part 2 */
    for (int i = 0; i < 10000000; i++)
        current2 = nextMove(current2, next2, 1000000);
    printf("%" PRIu64 "\n", next2[1] * next2[next2[1]]);

    return EXIT_SUCCESS;
}

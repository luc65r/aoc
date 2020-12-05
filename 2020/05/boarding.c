#include <stdlib.h>
#include <stdio.h>

#define LSIZE 12 /* 10 chars + '\n' + '\0' */
#define NBIDS (1 << 10)

int seatID(char str[LSIZE]) {
    int id = 0;
    for (int i = 0; i < 7; i++) {
        id <<= 1;
        id += str[i] == 'B' ? 1 : 0;
    }
    for (int i = 7; i < 10; i++) {
        id <<= 1;
        id += str[i] == 'R' ? 1 : 0;
    }
    return id;
}

int largestID(char ids[NBIDS]) {
    int id;
    for (id = NBIDS - 1; id && !ids[id]; id--);
    return id;
}

int missingID(char ids[NBIDS], int largest) {
    int id;
    for (id = largest; id && ids[id]; id--);
    return id;
}

int main() {
    char ids[NBIDS] = {0};

    char buf[LSIZE];
    while (fgets(buf, sizeof buf, stdin)) {
        if (buf[10] != '\n') {
            fprintf(stderr, "Error in parsing.\n");
            return EXIT_FAILURE;
        }
        ids[seatID(buf)] = 1;
    }

    int largest = largestID(ids);
    printf("%d\n%d\n", largest, missingID(ids, largest));
}

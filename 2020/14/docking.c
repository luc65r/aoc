#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>

#define u64 uint64_t
#define LSIZE (4 + 3 + 36 + 2)

void toMask(u64 mask[2], const char *str) {
    mask[0] = UINT64_MAX;
    mask[1] = 0;
    for (int i = 0; i < 36; i++) {
        switch (str[i]) {
            case 'X':
                break;
            case '0':
                mask[0] &= ~((u64)1 << (35 - i));
                break;
            case '1':
                mask[1] |= (u64)1 << (35 - i);
                break;
        }
    }
}

int toMem(const u64 mask[2], u64 **mem, u64 *mem_size, const char *str) {
    char *bracket;
    u64 place = strtoumax(str + 4, &bracket, 10);
    u64 value = strtoumax(bracket + 4, NULL, 10);
    value &= mask[0];
    value |= mask[1];

    u64 new_size = place + 1;
    if (new_size >= *mem_size) {
        *mem = realloc(*mem, new_size * sizeof(u64));
        if (!*mem) return 0;
        memset(*mem + *mem_size, 0, (new_size - *mem_size) * sizeof(u64));
        *mem_size = new_size;
    }

    (*mem)[place] = value;

    return 1;
}

int main() {
    u64 mem_size = 0;
    u64 *mem = malloc(mem_size);
    if (!mem) {
        return EXIT_FAILURE;
    }

    u64 mask[2];
    char line[LSIZE];
    while (fgets(line, sizeof line, stdin)) {
        if (line[1] == 'a') {
            toMask(mask, line + 7);
        } else {
            if (!toMem(mask, &mem, &mem_size, line)) {
                return EXIT_FAILURE;
            }
        }
    }

    u64 sum = 0;
    for (int i = 0; i < mem_size; i++) {
        sum += mem[i];
    }

    printf("%" PRIu64 "\n", sum);
}

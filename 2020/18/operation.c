#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

typedef uint64_t u64;

typedef enum {
    NONE, ADD, MUL
} op;

u64 doOp(op o, u64 a, u64 b) {
    switch (o) {
        case NONE:
            return b;
        case ADD:
            return a + b;
        case MUL:
            return a * b;
    }

    return 0;
}


u64 calculate(char **str, int precedence) {
    op o = NONE;
    u64 res = 0;
    for (; **str != '\n'; (*str)++) {
        switch (**str) {
            case ' ':
                continue;
            case '(':
                (*str)++;
                res = doOp(o, res, calculate(str, precedence));
                break;
            case ')':
                return res;
            case '+':
                o = ADD;
                break;
            case '*':
                o = MUL;
                if (precedence) {
                    (*str)++;
                    res = doOp(o, res, calculate(str, precedence));
                    (*str)--;
                }
                break;
            default:
                res = doOp(o, res, strtoumax(*str, str, 10));
                (*str)--;
        }
    }

    return res;
}

int main() {
    int str_size = 1024;
    char *str = malloc(str_size);
    if (!str) {
        fprintf(stderr, "Couldn't allocate memory to read stdin!\n");
        return EXIT_FAILURE;
    }

    u64 sum1 = 0;
    u64 sum2 = 0;
    while (fgets(str, str_size, stdin)) {
        while (str[strlen(str) - 1] != '\n') {
            /* fgets couldn't read the whole line */
            str_size *= 2;
            str = realloc(str, str_size);
            if (!str) {
                fprintf(stderr, "Couldn't reallocate memory to read stdin!\n");
                return EXIT_FAILURE;
            }
            if (!fgets(str + str_size / 2 - 1, str_size / 2 + 1, stdin)) {
                fprintf(stderr, "Unexpected EOF\n");
                return EXIT_FAILURE;
            }
        }

        char *str2 = str;
        sum1 += calculate(&str2, 0);
        str2 = str;
        sum2 += calculate(&str2, 1);
    }

    free(str);

    printf("%" PRIu64 "\n%" PRIu64 "\n", sum1, sum2);

    return EXIT_SUCCESS;
}

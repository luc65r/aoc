#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <inttypes.h>

#define umax uintmax_t

int findEntries(umax *entries, int count, umax *m2, umax *m3) {
    /* m2 and m3 will be the result of the multiplications.
       They stay at 0 until the sum of the entries is eqal to 2020.
       We assume that there isn't any entry equal to 0.
     */
    *m2 = 0, *m3 = 0;
    for (int i = 0; i < count; i++) {
        for (int j = i + 1; j < count; j++) {
            if (entries[i] + entries[j] == 2020) {
                *m2 = entries[i] * entries[j];
                if (*m3) return 1;
            }

            /* We don't want to enter this loop if m3 is already set */
            for (int k = j + 1; !*m3 && k < count; k++) {
                if (entries[i] + entries[j] + entries[k] == 2020) {
                    *m3 = entries[i] * entries[j] * entries[k];
                    if (*m2) return 1;
                }
            }
        }
    }

    return 0;
}

int main() {
    int str_size = log10(INTMAX_MAX) + 2; /* for '\n' and '\0' */
    char *str = malloc(str_size);
    if (!str) {
        fprintf(stderr, "Couldn't allocate %d bytes of memory!\n", str_size);
        return EXIT_FAILURE;
    }

    int report_size = 1000;
    umax *entries = malloc(report_size * sizeof(umax));
    if (!entries) {
        fprintf(stderr, "Couldn't allocate memory!\n");
        return EXIT_FAILURE;
    }

    /* Read the entries from stdin */
    int count;
    for (count = 0; fgets(str, str_size, stdin); count++) {
        if (count >= report_size) {
            /* We do not have enough space in the buffer */
            report_size *= 2;
            entries = realloc(entries, report_size);
            if (!entries) {
                fprintf(stderr, "Couldn't reallocate memory!\n");
                return EXIT_FAILURE;
            }
        }

        entries[count] = strtoumax(str, NULL, 10);
    }

    free(str);

    umax m2, m3;
    if (findEntries(entries, count, &m2, &m3)) {
        printf("%" PRIuMAX "\n%" PRIuMAX "\n", m2, m3);
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "Couldn't find a solution!\n");
        return EXIT_FAILURE;
    }
}

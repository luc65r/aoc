#include <stdlib.h>
#include <stdio.h>

typedef struct cup cup;
struct cup {
    int label;
    cup *next;
};

cup *toList(char label[10]) {
    cup *buf = malloc(9 * sizeof(cup));
    if (!buf) return NULL;

    for (int i = 0; i < 9; i++) {
        buf[i] = (cup){
            .label = label[i] - '0',
            .next = buf + (i + 1) % 9
        };
    }

    return buf;
}

int oneLabelEq(cup *c, int n) {
    for (; c; c = c->next)
        if (c->label == n)
            return 1;

    return 0;
}

cup *findLabel(cup *c, int n) {
    if (c->label == n)
        return c;

    for (cup *d = c->next; d != c; d = d->next)
        if (d->label == n)
            return d;

    return NULL;
}

cup *nextMove(cup *current) {
    cup *picked = current->next;
    cup *pickedLast = picked->next->next;
    current->next = pickedLast->next;
    pickedLast->next = NULL;

    int n = current->label - 1;
    n = n ? n : 9;
    while (oneLabelEq(picked, n))
        n = --n ? n : 9;

    cup *destination = findLabel(current, n);
    if (!destination) return NULL;

    pickedLast->next = destination->next;
    destination->next = picked;

    return current->next;
}

void printLabels(cup *c) {
    cup *one = findLabel(c, 1);
    for (c = one->next; c != one; c = c->next)
        putchar(c->label + '0');

    putchar('\n');
}

int main() {
    char labels[10];

    if (!fgets(labels, sizeof labels, stdin)) {
        fprintf(stderr, "Couldn't read stdin");
        return EXIT_FAILURE;
    }

    cup *current = toList(labels);
    for (int i = 0; i < 100; i++) {
        current = nextMove(current);
        if (!current) {
            fprintf(stderr, "Can't play next move");
            return EXIT_FAILURE;
        }
    }
    printLabels(current);

    return EXIT_SUCCESS;
}

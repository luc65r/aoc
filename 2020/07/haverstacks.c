#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct inside inside;
typedef struct bag bag;

struct inside {
    int nb;
    bag *bag;
};

struct bag {
    char *color;
    int nb_in;
    inside *in;
};

/* Return pointers to words. Spaces are transformed into '\0'.
   Last pointer is NULL.
 */
char **toWords(char *str) {
    int buf_words = 32;
    char **words = malloc(buf_words * sizeof(char *));
    if (!words) return NULL;

    *words = str;
    int nb_words = 1;
    for (; *str; str++) {
        if (nb_words >= buf_words) {
            buf_words *= 2;
            words = realloc(words, buf_words * sizeof(char *));
            if (!words) return NULL;
        }

        if (*str == ' ') {
            *str = '\0';
            words[nb_words++] = str + 1;
        }
    }

    words[nb_words] = NULL;
    return words;
}

/* Join 2 NTBS with a space in between */
char *join(const char *restrict word1, const char *restrict word2) {
    int len1 = strlen(word1);
    int len2 = strlen(word2);
    char *words = malloc(len1 + len2 + 2);
    if (!words) return NULL;

    strcpy(words, word1);
    words[len1] = ' ';
    strcpy(words + len1 + 1, word2);

    return words;
}

bag *findBag(const char *color, const bag *bags, int nb_bags) {
    for (int i = 0; i < nb_bags; i++) {
        if (!strcmp(color, bags[i].color))
            return bags + i;
    }

    return NULL;
}

/* Parses str and adds what can be added */
int addBag(char *str, bag *bags, int *nb_bags) {
    char **words = toWords(str);
    if (!words) return 0;

    int nb_words = 0;
    while (words[++nb_words]);

    if (nb_words < 7) return 0;

    char *color = join(words[0], words[1]);
    if (!color) return 0;

    int nb_in = strcmp(words[4], "no") ? nb_words / 4 - 1 : 0;
    inside *in = NULL;
    if (nb_in) {
        in = malloc(nb_in * sizeof(inside));
        if (!in) return 0;

        for (int i = 0; i < nb_in; i++) {
            int n = strtol(words[i * 4 + 4], NULL, 10);
            char *c = join(words[i * 4 + 5], words[i * 4 + 6]);
            if (!c) return 0;

            bag *b = findBag(c, bags, *nb_bags);
            if (b) {
                /* We already have this bag */
                free(c);
            } else {
                /* We don't, we need to create it */
                b = bags + (*nb_bags)++;
                b->color = c;
                b->in = NULL;
                b->nb_in = 0;
            }

            in[i].bag = b;
            in[i].nb = n;
        }
    }

    free(words);

    bag *bag = findBag(color, bags, *nb_bags);
    if (bag) {
        free(color);
    } else {
        bag = bags + (*nb_bags)++;
        bag->color = color;
    }
    bag->in = in;
    bag->nb_in = nb_in;

    return 1;
}

int containsGold(const bag *b) {
    for (int i = 0; i < b->nb_in; i++) {
        bag *s = b->in[i].bag;
        if (!strcmp(s->color, "shiny gold"))
            return 1;
        if (containsGold(s))
            return 1;
    }

    return 0;
}

int bagsIn(const bag *b) {
    int sum = 0;

    for (int i = 0; i < b->nb_in; i++) {
        inside in = b->in[i];
        sum += in.nb * (bagsIn(in.bag) + 1);
    }

    return sum;
}

int main() {
    int str_size = 1024;
    char *str = malloc(str_size);
    if (!str) {
        fprintf(stderr, "Couldn't allocate memory to read stdin!\n");
        return EXIT_FAILURE;
    }

    int buf_bags = 1024;
    int nb_bags = 0;
    bag *bags = malloc(buf_bags * sizeof(bag));
    if (!bags) {
        fprintf(stderr, "Couldn't allocate memory to store bags!\n");
        return EXIT_FAILURE;
    }

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

        if (!addBag(str, bags, &nb_bags)) {
            fprintf(stderr, "Couldn't add bag\n");
            return EXIT_FAILURE;
        }
    }

    free(str);

    int sum = 0;
    for (int i = 0; i < nb_bags; i++)
        sum += containsGold(bags + i);

    printf("%d\n%d\n", sum, bagsIn(findBag("shiny gold", bags, nb_bags)));

    return EXIT_SUCCESS;
}

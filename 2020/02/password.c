#include <stdlib.h>
#include <stdio.h>

#define COUNT 1000

typedef struct {
    int first;
    int second;
    char character;
    char *password;
} policy;

int countChar(char c, char *s) {
    int n = 0;
    while (*s != '\n')
        if (*s++ == c)
            n++;
    return n;
}

int firstValid(policy p) {
    int n = countChar(p.character, p.password);
    return p.first <= n && n <= p.second;
}

int secondValid(policy p) {
    return (p.password[p.first - 1] == p.character)
        ^ (p.password[p.second - 1] == p.character);
}

policy toPolicy(char *str) {
    policy p;
    char *end;
    p.first = strtol(str, &end, 10);
    str = end + 1;
    p.second = strtol(str, &end, 10);
    str = end + 1;
    p.character = *str;
    p.password = str + 3;

    return p;
}

int main() {
    int buflen = COUNT;
    char *buf = malloc(buflen * sizeof(char));
    char *pos = buf;

    while (fread(pos, sizeof(char), COUNT, stdin) == COUNT) {
        buflen += COUNT;
        if (!(buf = realloc(buf, buflen))) {
            fprintf(stderr, "Couldn't reallocate memory.\n");
            return EXIT_FAILURE;
        }
        pos = buf + buflen - COUNT;
    }

    pos = buf;
    int valid1 = 0, valid2 = 0;
    while (*pos) {
        policy p = toPolicy(pos);
        valid1 += firstValid(p);
        valid2 += secondValid(p);
        while (*pos++ != '\n');
    }

    printf("%d\n%d\n", valid1, valid2);
}

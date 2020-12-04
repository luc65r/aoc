#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define COUNT 1024

typedef struct {
    char *byr, *iyr, *eyr,
        *hgt, *hcl, *ecl,
        *pid, *cid; /* We can ignore this one */
} passport;


int whichField(char *colon) {
    switch (colon[-3]) {
        case 'b': return 0; /* byr */
        case 'i': return 1; /* iyr */
        case 'p': return 6; /* pid */
        case 'c': return 7; /* cid */
        case 'e': if (colon[-2] == 'y')
                      return 2; /* eyr */
                  else
                      return 5; /* ecl */
        case 'h': if (colon[-2] == 'g')
                      return 3; /* hgt */
                  else
                      return 4; /* hcl */
        default: return -1;
    }
}

char *setField(passport *p, int field, char *str) {
    *((char **)p + field) = str;
    while (*str != ' ' && *str != '\n' && *str)
        str++;
    *str = '\0';
    return str + 1;
}

passport *makePassport(char *str) {
    passport *p = calloc(1, sizeof(passport));
    char *colon;
    while ((colon = strchr(str, ':'))) {
        int field = whichField(colon);
        if (field == -1) {
            fprintf(stderr, "Invalid field!\n");
            goto error;
        }
        str = setField(p, field, colon + 1);
    }

    /* Go through each field except the last one and if NULL error */
    for (char **i = (char **)p;
            i < (char **)p + sizeof(passport) / sizeof(char *) - 1; i++)
        if (!*i) goto error;

    return p;

error:
    free(p);
    return NULL;
}

int validatePassport(passport *p) {
    if (strlen(p->byr) != 4) return 0;
    int byr = strtol(p->byr, NULL, 10);
    if (byr < 1920 || 2002 < byr) return 0;

    if (strlen(p->iyr) != 4) return 0;
    int iyr = strtol(p->iyr, NULL, 10);
    if (iyr < 2010 || 2020 < iyr) return 0;

    if (strlen(p->eyr) != 4) return 0;
    int eyr = strtol(p->eyr, NULL, 10);
    if (eyr < 2020 || 2030 < eyr) return 0;

    int lght = strlen(p->hgt);
    if (lght != 4 && lght != 5) return 0;
    char *shgt;
    int hgt = strtol(p->hgt, &shgt, 10);
    if (!strcmp(shgt, "in")) {
        if (hgt < 59 || 76 < hgt) return 0;
    } else if (!strcmp(shgt, "cm")) {
        if (hgt < 150 || 193 < hgt) return 0;
    } else {
        return 0;
    }

    if (p->hcl[0] != '#') return 0;
    if (p->hcl[7] != '\0') return 0;
    for (int i = 1; i <= 6; i++)
        if (!isxdigit(p->hcl[i])) return 0;

    if (strlen(p->ecl) != 3) return 0;
    int ecl = 0;
    for (int i = 0; i < 7; i++)
        if (!strcmp(p->ecl, (char [7][4]){
                    "amb", "blu", "brn", "gry", "grn", "hzl", "oth"
                }[i])) {
            ecl = 1;
            break;
        }
    if (!ecl) return 0;

    if (p->pid[9] != '\0') return 0;
    for (int i = 0; i < 9; i++)
        if (!isdigit(p->pid[i])) return 0;

    return 1;
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

    int nbPass1 = 0;
    int nbPass2 = 0;

    char *prec = buf;

    for (pos = buf; *pos; pos++) {
        if (pos[0] == '\n' && pos[1] == '\n') {
            pos[0] = '\0'; pos[1] = '\0';
            pos++; /* To skip the '\0' we just made */

            /* We are ready to create the passport */
            passport *p = makePassport(prec);
            if (p) {
                nbPass1++;
                nbPass2 += validatePassport(p);
                free(p); /* I almost forgot... */
            }

            prec = pos + 1;
        }
    }

    passport *p = makePassport(prec);
    if (p) {
        nbPass1++;
        nbPass2 += validatePassport(p);
    }

    printf("%d\n%d\n", nbPass1, nbPass2);

    return EXIT_SUCCESS;
}

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *DIGITS[] = { "1",    "2",    "3",   "4",     "5",     "6",
                         "7",    "8",    "9",   "one",   "two",   "three",
                         "four", "five", "six", "seven", "eight", "nine" };
#define VALUE(i) ((i >= 9) ? (i - 9 + 1) : ((size_t)(DIGITS[i][0] - 0x30)))

char *strstr_last(const char *haystack, const char *needle) {
    char *last = NULL;

    for (;;) {
        char *m = strstr(haystack, needle);
        if (m == NULL)
            break;
        last = m;
        haystack = m + 1;
    }

    return last;
}

typedef char *(*search_func_t)(const char *, const char *);
typedef struct {
    search_func_t search;
    enum Order { LHS, RHS } order;
} Search_Type;

// Get value w.r.t. search type
int get(char *line, size_t ub, Search_Type st) {
    assert(ub <= sizeof(DIGITS) / sizeof(DIGITS[0]));

    size_t diff_run = (st.order == RHS) ? 0 : strlen(line);
    int res = -1;

    for (size_t i = 0; i < ub; ++i) {
        char *match = st.search(line, DIGITS[i]);
        if (match == NULL)
            continue;

        size_t diff = match - line;

        if ((st.order == RHS && diff >= diff_run) ||
            (st.order == LHS && diff <= diff_run)) {
            diff_run = diff;
            res = VALUE(i);
        }
    }
    return res;
}

int main() {
    char *line = NULL;
    size_t size = 0;
    int p1 = 0, p2 = 0;

    Search_Type leftmost = {
        .search = strstr,
        .order = LHS,
    };
    Search_Type rightmost = {
        .search = strstr_last,
        .order = RHS,
    };

    while (getline(&line, &size, stdin) > 0) {
        // use only digits
        p1 += 10 * get(line, 9, leftmost) + get(line, 9, rightmost);
        // use both digits and words
        p2 += 10 * get(line, 18, leftmost) + get(line, 18, rightmost);
    }

    printf("Part 1: %d\n", p1);
    printf("Part 2: %d\n", p2);

    free(line);

    return 0;
}

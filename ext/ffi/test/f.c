#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void print(char* t) {
    printf("[C PRINT] %s\n", t);
}

int test1() {
    return 1;
}

int test2() {
    return 2;
}

int plus(int a, int b) {
    return a + b;
}

char* string_in_string_out(char* t) {
    return t;
}

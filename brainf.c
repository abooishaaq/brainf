#include <stdio.h>
#include <stdlib.h>

long forward(char *src, size_t len, long src_ix) {
    long depth = 1;
    while (depth > 0) {
        src_ix++;
        if (src_ix >= len) {
            puts("Out of Bounds");
            exit(-1);
        }
        switch (src[src_ix]) {
        case '[':
            depth += 1;
            break;
        case ']':
            depth -= 1;
            break;
        default:;
        }
    }
    return src_ix;
}

long backward(char *src, size_t len, long src_ix) {
    long depth = 1;
    while (depth > 0) {
        src_ix--;
        if (src_ix < 0) {
            puts("Out of Bounds");
            exit(-1);
        }
        switch (src[src_ix]) {
        case '[':
            depth -= 1;
            break;
        case ']':
            depth += 1;
            break;
        default:;
        }
    }
    return src_ix;
}

void brainf(char *src, size_t len) {
    char *mem = calloc(30000, sizeof(char));
    long src_ix = 0;
    long mem_ix = 0;
    while (src_ix < len) {
        switch (src[src_ix]) {
        case '+':
            mem[mem_ix]++;
            break;
        case '-':
            mem[mem_ix]--;
            break;
        case '>':
            mem_ix = (mem_ix + 1) % 30000;
            break;
        case '<':
            if (mem_ix == 0) {
                mem_ix = 29999;
            } else {
                mem_ix--;
            }
            break;
        case '.':
            printf("%c", mem[mem_ix]);
            break;
        case ',':
            scanf("%c", &mem[mem_ix]);
            break;
        case '[':
            if (mem[mem_ix] == 0)
                src_ix = forward(src, len, src_ix);
            break;
        case ']':
            if (mem[mem_ix] != 0)
                src_ix = backward(src, len, src_ix);
            break;
        default:;
        }
        src_ix++;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        puts("usage: ./brainf <filepath>");
        return 0;
    }
    FILE *f = fopen(argv[1], "r");
    fseek(f, 0L, SEEK_END);
    long size = ftell(f);
    char buffer[size];
    rewind(f);
    size_t len = fread(buffer, sizeof(char), size, f);
    brainf(buffer, len);
}

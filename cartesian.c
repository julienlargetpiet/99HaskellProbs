#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <time.h>

bool get_total_length(
    const size_t *lengths,
    size_t ndim,
    size_t *out_total
) {
    size_t total = 1;

    for (size_t i = 0; i < ndim; ++i) {
        if (lengths[i] == 0) {
            *out_total = 0;
            return true;
        }

        if (total > SIZE_MAX / lengths[i]) {
            return false; // overflow protection
        }

        total *= lengths[i];
    }

    *out_total = total;
    return true;
}

size_t *cartesian_indices(
    const size_t *lengths,
    size_t ndim,
    size_t *out_rows
) {
    size_t total = 0;

    if (!get_total_length(lengths, ndim, &total)) {
        return NULL;
    }

    *out_rows = total;

    if (total == 0) {
        return NULL;
    }

    if (ndim == 0) {
        return NULL;
    }

    if (total > SIZE_MAX / ndim) {
        return NULL;
    }

    size_t n_elems = total * ndim;

    if (n_elems > SIZE_MAX / sizeof(size_t)) {
        return NULL;
    }

    size_t *indices = malloc(n_elems * sizeof(*indices));
    if (indices == NULL) {
        return NULL;
    }

    size_t *cur_indices = calloc(ndim, sizeof(*cur_indices));
    if (cur_indices == NULL) {
        free(indices);
        return NULL;
    }

    size_t row = 0;
    size_t dim = ndim - 1;

    while (row < total) {
        const size_t row_idx = ndim * row;

        for (size_t i = 0; i < ndim; ++i) {
            indices[row_idx + i] = cur_indices[i];
        }

        ++row;

        if (row == total) {
            break;
        }

        while (dim > 0 && cur_indices[dim] + 1 == lengths[dim]) {
            cur_indices[dim] = 0;
            --dim;
        }

        ++cur_indices[dim];
        dim = ndim - 1;
    }

    free(cur_indices);
    return indices;
}

void print_cartesian_values(
    size_t *indices,
    size_t rows,
    size_t ndim,
    const size_t **data
) {
    for (size_t i = 0; i < rows; ++i) {
        const size_t row_idx = i * ndim;
        for (size_t j = 0; j < ndim; ++j) {
            printf("%zu ", data[j][indices[row_idx + j]]);
        }

        putchar('\n');
    }
}

static uint64_t now_ns(void) {
    struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);

    return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}

int main(void) {
    //const size_t a[] = {1, 2};
    //const size_t b[] = {3, 4, 5};
    //const size_t c[] = {6, 7};

    //const size_t lengths[] = {
    //    sizeof(a) / sizeof(a[0]),
    //    sizeof(b) / sizeof(b[0]),
    //    sizeof(c) / sizeof(c[0])
    //};

    const size_t a[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    const size_t b[] = {10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
    const size_t c[] = {20, 21, 22, 23, 24, 25, 26, 27, 28, 29};
    const size_t d[] = {30, 31, 32, 33, 34, 35, 36, 37, 38, 39};
    const size_t e[] = {40, 41, 42, 43, 44, 45, 46, 47, 48, 49};
    
    const size_t *data[] = {a, b, c, d, e};
    
    const size_t lengths[] = {
        sizeof(a) / sizeof(a[0]),
        sizeof(b) / sizeof(b[0]),
        sizeof(c) / sizeof(c[0]),
        sizeof(d) / sizeof(d[0]),
        sizeof(e) / sizeof(e[0])
    };

    const size_t ndim = sizeof(lengths) / sizeof(lengths[0]);

    const size_t warmup = 1000;
    const size_t iterations = 100000;

    volatile size_t sink = 0;

    for (size_t i = 0; i < warmup; ++i) {
        size_t rows = 0;
        size_t *indices = cartesian_indices(lengths, ndim, &rows);

        if (indices == NULL && rows != 0) {
            fprintf(stderr, "cartesian_indices failed during warmup\n");
            return 1;
        }

        free(indices);
    }

    uint64_t start = now_ns();

    for (size_t i = 0; i < iterations; ++i) {
        size_t rows = 0;
        size_t *indices = cartesian_indices(lengths, ndim, &rows);

        if (indices == NULL && rows != 0) {
            fprintf(stderr, "cartesian_indices failed during benchmark\n");
            return 1;
        }

        free(indices);
    }

    uint64_t end = now_ns();

    uint64_t elapsed_ns = end - start;

    double ns_per_iter = (double)elapsed_ns / (double)iterations;

    printf("iterations:       %zu\n", iterations);
    printf("elapsed:          %" PRIu64 " ns\n", elapsed_ns);
    printf("ns / call:        %.2f\n", ns_per_iter);

    //{

    //    size_t rows = 0;
    //    size_t *indices = cartesian_indices(lengths, ndim, &rows);

    //    if (indices == NULL && rows != 0) {
    //        fprintf(stderr, "cartesian_indices failed during benchmark\n");
    //        return 1;
    //    }

    //    print_cartesian_values(
    //        indices,
    //        rows,
    //        ndim,
    //        data
    //    );

    //    free(indices);

    //}

    return 0;
}

// size_t *data1[] = {a, b, c};
// size_t *data2[] = {a, b, c};
// size_t *data3[] = {a, b, c};
// 
// size_t **outer_data[] = {data1, data2, data3}



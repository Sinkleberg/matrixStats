#include <R.h>
#include <Rinternals.h>
#include <limits.h>
#include <time.h>

SEXP steady_seconds(void) {
    struct timespec t;
    if (clock_gettime(CLOCK_MONOTONIC, &t) != 0) error("monotonic clock unavailable");
    return ScalarReal((double)t.tv_sec + t.tv_nsec / 1e9);
}

/* Private benchmark input: NULL or one-based integer selections (NA allowed). */
static int selection_size(SEXP selection, int bound) {
    if (selection == R_NilValue) return bound;
    if (TYPEOF(selection) != INTSXP || XLENGTH(selection) > INT_MAX)
        error("expected a short integer selection");
    for (int i = 0; i < LENGTH(selection); i++) {
        int x = INTEGER(selection)[i];
        if (x != NA_INTEGER && (x < 1 || x > bound))
            error("index outside matrix");
    }
    return LENGTH(selection);
}

SEXP fused(SEXP x, SEXP rows, SEXP cols, SEXP by_row, SEXP want_all) {
    if (!isMatrix(x)) error("expected matrix");
    int type = TYPEOF(x);
    if (type != REALSXP && type != INTSXP && type != LGLSXP)
        error("expected double, integer or logical matrix");
    int byrow = asLogical(by_row), all = asLogical(want_all);
    if (byrow == NA_LOGICAL || all == NA_LOGICAL) error("missing mode");
    int nr = nrows(x), nc = ncols(x);
    int n = selection_size(rows, nr), m = selection_size(cols, nc);
    const int *ri = rows == R_NilValue ? NULL : INTEGER(rows);
    const int *ci = cols == R_NilValue ? NULL : INTEGER(cols);
    const double *d = type == REALSXP ? REAL(x) : NULL;
    const int *v = type == INTSXP ? INTEGER(x) : type == LGLSXP ? LOGICAL(x) : NULL;
    SEXP res = PROTECT(allocVector(LGLSXP, byrow ? n : m));
    int *out = LOGICAL(res);
    for (int k = 0; k < LENGTH(res); k++) out[k] = all;
    for (int j = 0; j < m; j++) {
        int col = ci ? ci[j] : j + 1;
        for (int i = 0; i < n; i++) {
            int *result = out + (byrow ? i : j);
            if (*result == !all) {
                if (!byrow) break;
                continue;
            }
            int row = ri ? ri[i] : i + 1;
            if (row == NA_INTEGER || col == NA_INTEGER) {
                *result = NA_LOGICAL;
            } else {
                R_xlen_t offset = (R_xlen_t)(col - 1) * nr + (row - 1);
                if (d ? !R_FINITE(d[offset]) : v[offset] == NA_INTEGER)
                    *result = !all;
            }
        }
    }
    UNPROTECT(1);
    return res;
}

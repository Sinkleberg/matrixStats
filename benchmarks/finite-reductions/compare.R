library(matrixStats)
dyn.load(paste0("fused", .Platform$dynlib.ext))
set.seed(282L)
fused <- function(x, rows, cols, byrow, all) {
  result <- .Call("fused", x, rows, cols, byrow, all)
  labels <- if (byrow) rownames(x) else colnames(x)
  idx <- if (byrow) rows else cols
  if (length(result) && !is.null(labels))
    names(result) <- if (is.null(idx)) labels else labels[idx]
  result
}
reference <- function(x, rows, cols, byrow, all) {
  reduction <- if (byrow) {
    if (all) rowAlls else rowAnys
  } else {
    if (all) colAlls else colAnys
  }
  mask <- is.finite(x)
  reduction(if (all) mask else !mask, rows = rows, cols = cols)
}
check <- function(x, rows = NULL, cols = NULL) {
  for (byrow in c(TRUE, FALSE)) for (all in c(TRUE, FALSE)) {
    got <- fused(x, rows, cols, byrow, all)
    expected <- reference(x, rows, cols, byrow, all)
    if (!identical(got, expected))
      stop(sprintf("mismatch: %s %s x %s, byrow=%s, all=%s: %s",
                   typeof(x), nrow(x), ncol(x), byrow, all,
                   paste(all.equal(got, expected), collapse = " ")))
  }
}
for (v in list(c(1, Inf, -Inf, NaN, NA, 0), c(1L, NA_integer_), c(TRUE, NA, FALSE))) {
  x <- matrix(rep_len(v, 12L), 3L, dimnames = list(letters[1:3], LETTERS[1:4]))
  check(x)
  check(x, c(2L, NA_integer_, 2L, 1L), c(4L, 1L, NA_integer_))
  check(x, integer(), c(2L, 2L))
  check(x, c(1L, 1L), integer())
  for (shape in list(c(0L, 7L), c(5L, 0L), c(0L, 0L)))
    check(matrix(v[integer()], shape[1L], shape[2L]))
}
# Complex is a reference-only control. It is outside the prototype's domain.
z <- matrix(c(1+0i, NA_complex_, Inf+0i, 0+0i), 2L)
stopifnot(identical(reference(z, NULL, NULL, TRUE, TRUE), c(FALSE, FALSE)))
stopifnot(isTRUE(capabilities("profmem")))
allocated <- function(fun) {
  path <- tempfile()
  on.exit(unlink(path))
  Rprofmem(path)
  tryCatch(fun(), finally = Rprofmem(NULL))
  lines <- grep("^[0-9]+ ", readLines(path, warn = FALSE), value = TRUE)
  sum(as.numeric(sub(" .*", "", lines)))
}
make_matrix <- function(shape, type, pattern) {
  n <- prod(shape)
  v <- switch(type, double = runif(n), integer = sample.int(100L, n, TRUE),
              logical = sample(c(TRUE, FALSE), n, TRUE))
  bad <- switch(type, double = c(NA_real_, NaN, Inf, -Inf),
                integer = NA_integer_, logical = NA)
  x <- matrix(v, shape[1], shape[2])
  idx <- switch(pattern, finite = integer(), sparse = sample.int(n, n %/% 1000L),
                dense = sample.int(n, n %/% 4L), first = seq_len(shape[1]),
                last = n - shape[1] + seq_len(shape[1]))
  if (length(idx)) x[idx] <- rep_len(bad, length(idx))
  x
}
print(sessionInfo())
shapes <- list(tall = c(8000L, 100L), square = c(1000L, 1000L),
               wide = c(100L, 8000L))
con <- file("timings.csv", "w")
writeLines("shape,type,pattern,axis,reduce,method,repeat,batch,seconds_per_call,allocated_bytes", con)
for (shape in names(shapes)) for (type in c("double", "integer", "logical")) {
  for (pattern in c("finite", "sparse", "dense", "first", "last")) {
    x <- make_matrix(shapes[[shape]], type, pattern)
    check(x)
    for (byrow in c(TRUE, FALSE)) for (all in c(TRUE, FALSE)) {
      methods <- list(fused = function() fused(x, NULL, NULL, byrow, all),
                      reference = function() reference(x, NULL, NULL, byrow, all))
      invisible(lapply(methods, function(f) f()))
      bytes <- vapply(methods, allocated, numeric(1L))
      for (i in seq_len(5L)) for (method in sample(names(methods))) {
        f <- methods[[method]]
        batch <- 8L
        t <- .Call("steady_seconds")
        for (k in seq_len(batch)) invisible(f())
        elapsed <- (.Call("steady_seconds") - t) / batch
        writeLines(paste(shape, type, pattern, if (byrow) "row" else "col",
                         if (all) "all" else "any", method, i, batch,
                         elapsed, bytes[[method]], sep = ","), con)
      }
    }
    flush(con)
  }
}
close(con)
cat("C213_FIXED_SEED_COMPARISON_PASS\n")

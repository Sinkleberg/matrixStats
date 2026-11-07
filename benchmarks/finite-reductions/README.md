# Fused finiteness checks

This standalone benchmark accompanies matrixStats issue #282. It compares a private C reduction with the existing row and column reductions over `is.finite(x)`. It records timings and allocations for double, integer and logical matrices.

## Reproduce on Linux

Run from the repository root with R, a C compiler and make installed. The recorded run used R 4.5.1, GCC 13.3.0 and one CPU. The package source is pinned to the tested develop commit.

```sh
set -eu
scratch=$(mktemp -d)
mkdir "$scratch/package" "$scratch/library" "$scratch/probe"
git archive 12e4e1609642ba86f6df796fa29d16a5636f0f57 | tar -x -C "$scratch/package"
export R_LIBS_USER="$scratch/library" MAKEFLAGS=-j1 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
R CMD INSTALL --no-multiarch --library="$R_LIBS_USER" "$scratch/package"
(cd "$scratch/package/tests" && Rscript --vanilla rowAllAnys.R && Rscript --vanilla rowAllAnys_subset.R)
cp benchmarks/finite-reductions/fused.c benchmarks/finite-reductions/compare.R "$scratch/probe/"
cd "$scratch/probe"
R CMD SHLIB fused.c
Rscript --vanilla compare.R
```

Each group has five randomized pairs of eight calls after warm-up. The CSV reports elapsed seconds per call and one Rprofmem allocation measurement per configuration and method. The benchmark checks each result against the installed package before timing. Names, empty dimensions, duplicate indices and missing indices have separate correctness controls.

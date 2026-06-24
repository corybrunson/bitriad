# Combinatorial bijections for affiliation network triad indexing

These functions biject among partitions of at most 3 parts, 3-subsets of
natural numbers, and indices for the lexicographic total orders on them.

## Usage

``` r
index_subset(i)

subset_index(vec)

subset_partition(vec)

partition_subset(lambda)

index_partition(i)

partition_index(lambda)

indexSubset(i)

indexPartition(i)

subsetIndex(vec)

subsetPartition(vec)

partitionIndex(lambda)

partitionSubset(lambda)
```

## Arguments

- i:

  Integer; an index in the total order, starting at 0.

- vec:

  Integer vector; a set of 3 distinct non-negative integers, in
  decreasing order.

- lambda:

  Integer vector; a partition of at most 3 parts, with parts in
  non-increasing order.

## Value

Numeric vectors.

## Examples

``` r
index_subset(2)
#> [1] 3 2 0
index_partition(2)
#> [1] 1 1 0
subset_index(c(3, 2, 0))
#> [1] 2
subset_partition(c(3, 2, 0))
#> [1] 1 1 0
partition_index(c(1, 1, 0))
#> [1] 2
partition_subset(c(1, 1, 0))
#> [1] 3 2 0
```

# Take the dual of an affiliation network

This function obtains the dual of an affiliation network, in which the
actors and events have swapped roles. To do this, it negates the logical
values of the node `type` attribute and reorders the node ids
accordingly.

## Usage

``` r
dualize(graph)

dual_an(graph)

dual.an(graph)
```

## Arguments

- graph:

  An affiliation network.

## Value

The input `graph` with the `"type"` attribute logically negated.

## See also

Other modal queries and manipulations:
[`mode_addition`](http://corybrunson.github.io/bitriad/reference/mode_addition.md),
[`mode_counts`](http://corybrunson.github.io/bitriad/reference/mode_counts.md),
[`modes`](http://corybrunson.github.io/bitriad/reference/modes.md),
[`schedule()`](http://corybrunson.github.io/bitriad/reference/schedule.md)

## Examples

``` r
data(women_clique)
( tab <- table(V(women_clique)$type) )
#> 
#> FALSE  TRUE 
#>     5     5 
( proj <- actor_projection(dualize(women_clique)) )
#> IGRAPH 9912073 UNW- 5 9 -- 
#> + attr: name (v/c), weight (e/n)
#> + edges from 9912073 (vertex names):
#> [1] Bridge--Movies   Bridge--Dance    Bridge--Dinner   Bridge--Visiting
#> [5] Dinner--Visiting Dinner--Movies   Dinner--Dance    Movies--Dance   
#> [9] Dance --Visiting
vcount(proj) == tab[2]
#> TRUE 
#> TRUE 
```

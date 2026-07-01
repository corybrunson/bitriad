# Dynamic affiliation network structure

An affiliation network is *dynamic*, for present purposes, if its event
nodes have time stamps, recorded as a numeric vertex attribute `"time"`
in non-decreasing order.

## Usage

``` r
is_dynamic_an(graph)

is.dyn(graph)

as_dynamic_an(graph, use_attr = NULL)
```

## Arguments

- graph:

  An affiliation network.

- use_attr:

  Character; the vertex attribute to coerce to numeric if necessary and
  use as the `"time"` attribute. If `NULL`, the default, time takes the
  values `seq(event_count(graph))`, reflecting the order of the event
  node IDs.

## Value

The input `graph` with a new `"time"` vertex attribute and its event
nodes permuted to respect this attribute.

## See also

Other network testing and coercion:
[`affiliation_network`](http://corybrunson.github.io/bitriad/reference/affiliation_network.md)

## Examples

``` r
data(women_group)
is_dynamic_an(women_group)
#> [1] TRUE
data(women_clique)
is_dynamic_an(women_clique)
#> [1] FALSE
as_dynamic_an(women_clique)
#> IGRAPH f0002c9 UN-B 10 14 -- 
#> + attr: type (v/l), name (v/c), time (v/n)
#> + edges from f0002c9 (vertex names):
#>  [1] Miss A--Bridge   Miss A--Movies   Miss A--Dance    Miss B--Movies  
#>  [5] Miss B--Dance    Miss C--Bridge   Miss C--Dinner   Miss C--Visiting
#>  [9] Miss D--Bridge   Miss D--Dinner   Miss D--Movies   Miss E--Dinner  
#> [13] Miss E--Dance    Miss E--Visiting
data(nmt_meetings)
nmt_meetings <- set_event_attr(
  nmt_meetings, "meet",
  value = gsub("meet", "", V2(nmt_meetings)$name)
)
as_dynamic_an(nmt_meetings, use_attr = "meet")
#> IGRAPH 65bdf35 UN-B 46 64 -- 
#> + attr: type (v/l), name (v/c), meet (v/c), time (v/n)
#> + edges from 65bdf35 (vertex names):
#>  [1] Abdullah Sunata  --meet12 Abu Dujanah      --meet1 
#>  [3] Abu Dujanah      --meet2  Abu Fida         --meet5 
#>  [5] Abu Fida         --meet6  Abu Fida         --meet13
#>  [7] Adung            --meet5  Ahmad Rofiq Ridho--meet11
#>  [9] Ahmad Rofiq Ridho--meet12 Ahmad Rofiq Ridho--meet14
#> [11] Ahmad Rofiq Ridho--meet15 Ahmad Rofiq Ridho--meet16
#> [13] Ahmad Rofiq Ridho--meet18 Akram            --meet17
#> [15] Asep Jaja        --meet12 Azhari Husin     --meet1 
#> + ... omitted several edges
```

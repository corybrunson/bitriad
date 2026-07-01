# Project an affiliation network onto its actors

These functions use
[`bipartite_projection()`](https://r.igraph.org/reference/bipartite_projection.html)
to compute the projections of an affiliation network onto the actor or
event nodes.

## Usage

``` r
mode_projection(graph, mode = 1, name = "name")

actor_projection(graph, ...)

event_projection(graph, ...)

actor.projection(graph, ...)

event.projection(graph, ...)
```

## Arguments

- graph:

  An affiliation network.

- mode:

  Numeric or character; whether to project onto actors (`1` or
  `"actors"`) or onto events (`2` or `"events"`).

- name:

  Character; the attribute of the actor or event nodes in `graph` to use
  as names for the nodes in the projection. If `NA`, node IDs are
  converted to characters and used. If `NULL`, no names are assigned.

- ...:

  Arguments passed to `mode_projection`.

## Value

An `igraph` object with nodes corresponding to one `"type"` of the input
`graph`.

## See also

Original **igraph** functions:
[`bipartite_projection()`](https://r.igraph.org/reference/bipartite_projection.html)

## Examples

``` r
data(chicago1960s)
( tab <- table(V(chicago1960s)$type) )
#> 
#> FALSE  TRUE 
#>    20    24 
( proj <- actor_projection(chicago1960s) )
#> IGRAPH a3b5f50 UNW- 20 137 -- 
#> + attr: name (v/c), weight (e/n)
#> + edges from a3b5f50 (vertex names):
#>  [1] Barr --Block      Barr --Gale       Barr --Kennedy    Barr --Ward      
#>  [5] Barr --Goodrich   Barr --Jarvis     Barr --McCormick  Barr --McDowell  
#>  [9] Barr --Oates      Barr --Clark      Barr --Eberhard   Barr --Freeman   
#> [13] Barr --Ingersoll  Barr --Livingston Block--Gale       Block--Kennedy   
#> [17] Block--Ward       Block--Cushman    Block--Eberhard   Block--Freeman   
#> [21] Block--Ingersoll  Block--Livingston Block--McCormick  Block--McDowell  
#> [25] Block--Oates      Block--Prince     Block--Swearingen Block--Jarvis    
#> [29] Block--Goodrich  
#> + ... omitted several edges
vcount(proj) == tab[1]
#> FALSE 
#>  TRUE 
( proj <- event_projection(chicago1960s) )
#> IGRAPH 3ce884c UNW- 24 134 -- 
#> + attr: name (v/c), weight (e/n)
#> + edges from 3ce884c (vertex names):
#>  [1] Amour      --John.Hancock.Mutual           
#>  [2] Amour      --Chicago                       
#>  [3] Amour      --First.National.Bank.of.Chicago
#>  [4] Amour      --Inland.Steel                  
#>  [5] Amour      --Sears.and.Roebuck             
#>  [6] Amour      --Standard.Oil                  
#>  [7] Amour      --Art.Institute.of.Chicago      
#>  [8] Amour      --Univ..of.Chicago              
#> + ... omitted several edges
vcount(proj) == tab[2]
#> TRUE 
#> TRUE 
```

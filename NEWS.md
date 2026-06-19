# bitriad 0.4.0

## Code (re)generation

OpenCode was used for the following tasks, which were manually reviewed and minimally revised, with summaries copied below.

### Update C++ and R code, especially {igraph} syntax, per changes to languages and packages since the last revision.

```
All tests pass, examples run, and vignette knits successfully. Here's a summary of all bugs found and fixed:
C++ Compilation Errors
src/censuses.cpp — 14 errors, ~50 warnings:
- Ambiguous std::vector<int> assignment from Rcpp NameProxy: The code assigned p_ego["d1"] (a NameProxy) to std::vector<int> variables. With C++17/libc++, this is ambiguous because there are multiple valid conversion paths. Fixed by changing the receiving variables to IntegerVector (Rcpp's own type).
- Ambiguous std::back_inserter/std::inserter on IntegerVector: IntegerVector variables used as output targets of std::set_intersection/std::set_difference via back_inserter/inserter fail with current libc++. Fixed by changing these variables to std::vector<int>.
- Bitwise operators on booleans: | and & used instead of || and && throughout. Fixed ~50 instances.
- Unused variables: 6 int tot = 0 variables declared but never read. Removed.
src/combinatorial-bijections.h — 4 warnings:
- Same bitwise-on-bool pattern (| → ||). Fixed.
R-Level Bugs
- R/utils.r: wedgeReturn() had no default for add.names, but three call sites omitted it (would crash at runtime). Added default add.names = NULL.
- R/utils.r: census_scheme() had duplicate conditions — both "binary" and "simple" branches checked c(4, 1), making "simple" unreachable. Fixed "binary" to check c(4, 2).
- R/triad.R: degree(graph, V(graph)$type == TRUE) passed a logical vector instead of vertex IDs, checking only vertex 1. Fixed to degree(graph, V(graph)[V(graph)$type == TRUE]).
Deprecated igraph API
- neighborhood() → ego() + as_ids(): 26 occurrences across 8 files. Replaced all.
- layout.fruchterman.reingold() → layout_with_fr(): 3 occurrences in vignette/doc files.
- as.directed() → as_directed(): 1 occurrence in R/triad-census.R.
- as.undirected() → as_undirected(): 1 occurrence in R/an.R.
- are.connected() → are_adjacent(): 2 occurrences in R/dynamic-triad-closure.R.
Test Incompatibilities
- tests/testthat/test-empty-graphs.r: expect_false(a, b) and expect_true(a, b) patterns — expect_* only takes one argument in testthat 3.x. Split into separate calls.
- tests/testthat/test-empty-graphs.r: Multi-argument expect_equal(a, b, c, d, e, NaN) — invalid in testthat 3.x. Replaced with individual expect_true(is.nan(...)) calls.
- tests/testthat/test-censuses.r: Multi-argument expect_equal(a, b, c) and expect_equal(a, b, c, d) — invalid in testthat 3.x. Split into separate expect_equal pairs.
Other
- R/triadic-internal.R: Contained a hardcoded .Random.seed assignment that would clobber R's RNG state on package load. Cleared the file.
```

### Convert {roxygen2} documentation to Markdown syntax.

```
All done. Here's the summary:
- Converted all 20 R files and 2 template files from LaTeX-style roxygen markup to markdown
- Added Config/roxygen2/markdown: TRUE to DESCRIPTION
- Regenerated all .Rd files via roxygen2::roxygenize()
- Verified: All 32 tests pass, 0 failures/warnings/skips
Key conversions:
- \code{x} →  `x` 
- \link[package]{target} → [target]
- \link{target} → [target]
- \strong{x} → **x**
- \emph{x} → *x*
- \pkg{x} → **x**
- \eqn{}/\deqn{} preserved as-is (native roxygen2 support)
- Nested \code{\link{...}} → [...]
- Removed empty @details tags from template files
```

## Other changes

The vignette was rebuilt after some restyling of the code.

Package description was revised and hyperlinks were updated per checks.

'NEWS.md' was initiated.

# bitriad <= 0.3

The package was orphaned for roughly 9 years at version `0.3`.

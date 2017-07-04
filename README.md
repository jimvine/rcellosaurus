# rcellosaurus: Cellosaurus for R

The goal of rcellosaurus is to provide some convenience functions for accessing the Cellosaurus dataset in R. [Cellosaurus](http://web.expasy.org/cellosaurus/) is a knowledge resource on cell lines. This R package is not associated with Cellosaurus, and the good people at Cellosaurus are not responsible for it in any way.

## Installation

```r
devtools::install_github("jimvine/rcellosaurus")
```

## Use

First download the XML version of the Cellosaurus dataset:

ftp://ftp.expasy.org/databases/cellosaurus

Load that into R with `read_cellosaurus_xml()`:
``` r
cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
```

To find a cell-line and identify the sex according to the Cellosaurus record use the following:

``` r
CVCL_E548 <- cell_line_find_first(cellosaurus, "CVCL_E548")
cell_line_sex(CVCL_E548)
```

Or simply nest these functions to get the sex directly:

```r
cell_line_sex(cell_line_find_first(cellosaurus, "CVCL_E548"))
```

The `rcellosaurus` package is powered by the `xml2` package, so returned results are typically XML nodes or nodesets that can be further processed using `xml2` if desired.

## References

Bairoch A. The Cellosaurus: a cell line knowledge resource. 
http://web.expasy.org/cellosaurus/

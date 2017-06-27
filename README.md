# rcellosaurus

The goal of rcellosaurus is to provide some convenience functions for accessing the Cellosaurus dataset in R.

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

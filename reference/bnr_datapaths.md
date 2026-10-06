# Table with default paths to commonly used spatial input files

This dataset contains path names to commonly-used spatial data files.
Given that those files are usually quite large, we here only describe
where to find them internally and not upload the data itself.
Medium-long-term this could be improved by relying on github LFS systems
or our own gitlab instance.

## Usage

``` r
bnr_datapaths
```

## Format

A [data.frame](https://rdrr.io/r/base/data.frame.html) containing paths
to key spatial data sources.

## Source

Manually updated and curated by BNR researchers

## Details

The file has the following columns:

[\*](https://rdrr.io/r/base/Arithmetic.html) 'drive': The path to drive
where the data is stored (Default: `'P:/bnr/'`). Can be system dependent
(Windows/Linux). [\*](https://rdrr.io/r/base/Arithmetic.html) 'access':
A non-structured field containing the list of people that have access
(for example `'IBF'` or `'bnr'`).
[\*](https://rdrr.io/r/base/Arithmetic.html) 'group': A field entry
describing to what this file belongs to (i.e. `"EPIC"`).
[\*](https://rdrr.io/r/base/Arithmetic.html) 'filename': The actual
filename

## Note

To update or overwrite, load the file and update, then apply
`usethis::use_data(bnr_datapaths, overwrite = TRUE) `.

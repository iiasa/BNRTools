# Check if a directory is empty (and create it if it doesn't exist)

This function checks whether a specified directory is empty. If the
directory does not exist, it will be created.

## Usage

``` r
misc_emptyfolder(dir_path)
```

## Arguments

- dir_path:

  A character string specifying the path to the directory.

## Value

A logical value: `TRUE` if the directory is empty (or newly created),
`FALSE` otherwise.

## Details

It also checks whether the directory is newly created or already
existed.

## Author

Martin Jung

Contributors: ChatGPT

## Examples

``` r
# Check and create a directory
dir_path <- tempfile()
is_empty <- misc_emptyfolder(dir_path)
print(is_empty)
#> [1] TRUE
unlink(dir_path, recursive = TRUE)
```

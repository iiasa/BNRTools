# Shows size of objects in the R environment

Shows the size of the objects currently in the R environment. Helps to
locate large objects cluttering the R environment and/or causing memory
problems during the execution of large workflows.

## Usage

``` r
misc_objectSize(n = 10)
```

## Arguments

- n:

  Number of objects to show, Default: `10`

## Value

A data frame with the row names indicating the object name, the field
'Type' indicating the object type, 'Size' indicating the object size,
and the columns 'Length/Rows' and 'Columns' indicating the object
dimensions if applicable.

## Author

Bias Benito

## Examples

``` r
if(interactive()){

 #creating dummy objects
 x <- matrix(runif(100), 10, 10)
 y <- matrix(runif(10000), 100, 100)

 #reading their in-memory size
 misc_objectSize()

}
```

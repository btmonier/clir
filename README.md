# clir

An R package providing a command-line argument parser inspired by Python's 
`argparse` and Kotlin's `clikt` libraries implemented using **pure R** and the 
S7 class system.


## Installation

``` r
pak::pak("btmonier/clir")
```

## Quick Start

``` r
library(clir)

# Create a parser
parser <- ArgumentParser(
  description = "Process some integers.",
  epilog = "Example usage: Rscript script.R --sum 1 2 3 4"
)

# Add arguments
parser <- add_argument(
    parser, 
    name    = "--integers", 
    help    = "an integer for the accumulator",
    type    = "integer",
    nargs   = "+",
    metavar = "N"
)

parser <- add_argument(
    parser, 
    name   = "--sum",
    dest   = "accumulate",
    action = "store_true",
    help   = "sum the integers (default: find the max)"
)

# Parse arguments
args <- parse_args(parser)

# Use parsed arguments
if (args$accumulate) {
  result <- sum(args$integers)
} else {
  result <- max(args$integers)
}

cat("Result:", result, "\n")
```


# Tests for parse_args function

test_that("parse_args fails with non-parser object", {
    expect_error(
        parse_args(list()),
        "parser must be an ArgumentParser object"
    )
})

test_that("parse_args returns empty list with no arguments", {
    parser <- ArgumentParser(prog = "test")
    result <- parse_args(parser, args = character())
    
    expect_type(result, "list")
})

test_that("parse_args handles store_true action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--verbose", action = "store_true")
    
    # Without flag
    result <- parse_args(parser, args = character())
    expect_false(result$verbose)
    
    # With flag
    result <- parse_args(parser, args = c("--verbose"))
    expect_true(result$verbose)
})

test_that("parse_args handles store_false action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--no-cache", dest = "cache", action = "store_false")
    
    # Without flag (default is TRUE for store_false)
    result <- parse_args(parser, args = character())
    expect_true(result$cache)
    
    # With flag
    result <- parse_args(parser, args = c("--no-cache"))
    expect_false(result$cache)
})

test_that("parse_args handles count action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "-v", dest = "verbosity", action = "count")
    
    # No flags
    result <- parse_args(parser, args = character())
    expect_equal(result$verbosity, 0L)
    
    # One flag
    result <- parse_args(parser, args = c("-v"))
    expect_equal(result$verbosity, 1L)
    
    # Multiple flags
    result <- parse_args(parser, args = c("-v", "-v", "-v"))
    expect_equal(result$verbosity, 3L)
})

test_that("parse_args handles string argument with value", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--input", type = "character")
    
    result <- parse_args(parser, args = c("--input", "data.csv"))
    expect_equal(result$input, "data.csv")
})

test_that("parse_args handles integer type conversion", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--count", type = "integer")
    
    result <- parse_args(parser, args = c("--count", "42"))
    expect_equal(result$count, 42L)
    expect_type(result$count, "integer")
})

test_that("parse_args handles numeric type conversion", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--value", type = "numeric")
    
    result <- parse_args(parser, args = c("--value", "3.14"))
    expect_equal(result$value, 3.14)
    expect_type(result$value, "double")
})

test_that("parse_args handles logical type conversion", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--flag", type = "logical")
    
    result <- parse_args(parser, args = c("--flag", "TRUE"))
    expect_true(result$flag)
    
    result <- parse_args(parser, args = c("--flag", "FALSE"))
    expect_false(result$flag)
})

test_that("parse_args uses default values", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--output", default = "output.txt")
    
    result <- parse_args(parser, args = character())
    expect_equal(result$output, "output.txt")
})

test_that("parse_args validates choices", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--format", choices = c("csv", "json"))
    
    # Valid choice
    result <- parse_args(parser, args = c("--format", "csv"))
    expect_equal(result$format, "csv")
    
    # Invalid choice
    expect_error(
        parse_args(parser, args = c("--format", "xml")),
        "Invalid choice"
    )
})

test_that("parse_args handles required arguments", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--input", required = TRUE)
    
    # Missing required argument
    expect_error(
        parse_args(parser, args = character()),
        "Required argument.*not provided"
    )
    
    # Provided required argument
    result <- parse_args(parser, args = c("--input", "file.txt"))
    expect_equal(result$input, "file.txt")
})

test_that("parse_args handles nargs '+' (one or more)", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--files", nargs = "+")
    
    # Multiple values
    result <- parse_args(parser, args = c("--files", "a.txt", "b.txt", "c.txt"))
    expect_equal(result$files, c("a.txt", "b.txt", "c.txt"))
    
    # Single value
    result <- parse_args(parser, args = c("--files", "single.txt"))
    expect_equal(result$files, "single.txt")
    
    # No values (error)
    expect_error(
        parse_args(parser, args = c("--files")),
        "requires at least one value"
    )
})

test_that("parse_args handles nargs '*' (zero or more)", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--items", nargs = "*")
    
    # Multiple values
    result <- parse_args(parser, args = c("--items", "a", "b", "c"))
    expect_equal(result$items, c("a", "b", "c"))
    
    # No values (empty)
    result <- parse_args(parser, args = c("--items"))
    expect_equal(result$items, character(0))
})

test_that("parse_args handles nargs '?' (optional value)", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--config", nargs = "?", default = "default.cfg")
    
    # With value
    result <- parse_args(parser, args = c("--config", "custom.cfg"))
    expect_equal(result$config, "custom.cfg")
    
    # Without value (uses default)
    result <- parse_args(parser, args = c("--config"))
    expect_equal(result$config, "default.cfg")
})

test_that("parse_args handles multiple arguments", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--input", required = TRUE)
    parser <- add_argument(parser, name = "--output", default = "out.txt")
    parser <- add_argument(parser, name = "--verbose", action = "store_true")
    parser <- add_argument(parser, name = "--count", type = "integer", default = 1L)
    
    result <- parse_args(parser, args = c(
        "--input", "in.csv",
        "--verbose",
        "--count", "5"
    ))
    
    expect_equal(result$input, "in.csv")
    expect_equal(result$output, "out.txt")  # default
    expect_true(result$verbose)
    expect_equal(result$count, 5L)
})

test_that("parse_args requires value for store action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--input", nargs = "1")
    
    expect_error(
        parse_args(parser, args = c("--input")),
        "requires a value"
    )
})

test_that("parse_args skips unknown arguments gracefully", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--known")
    
    # Unknown arguments are skipped
    result <- parse_args(parser, args = c("--unknown", "--known", "value"))
    expect_equal(result$known, "value")
})

test_that("parse_args handles argument order independence", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--first", type = "character")
    parser <- add_argument(parser, name = "--second", type = "integer")
    
    # Different order
    result1 <- parse_args(parser, args = c("--first", "a", "--second", "1"))
    result2 <- parse_args(parser, args = c("--second", "1", "--first", "a"))
    
    expect_equal(result1$first, result2$first)
    expect_equal(result1$second, result2$second)
})

test_that("parse_args handles mixed flag and value arguments", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--verbose", action = "store_true")
    parser <- add_argument(parser, name = "--file", type = "character")
    parser <- add_argument(parser, name = "--debug", action = "store_true")
    
    result <- parse_args(parser, args = c("--verbose", "--file", "test.txt", "--debug"))
    
    expect_true(result$verbose)
    expect_equal(result$file, "test.txt")
    expect_true(result$debug)
})


# Tests for Argument class

test_that("Argument can be created with minimal arguments", {
    arg <- Argument(name = "--test")
    
    expect_s3_class(arg, "clir::Argument")
    expect_equal(arg@name, "--test")
    expect_equal(arg@dest, "--test")  # defaults to name
    expect_equal(arg@type, "character")
    expect_equal(arg@action, "store")
    expect_equal(arg@nargs, "?")
    expect_false(arg@required)
})

test_that("Argument sets dest to name if not provided", {
    arg <- Argument(name = "--verbose")
    expect_equal(arg@dest, "--verbose")
})

test_that("Argument preserves explicit dest", {
    arg <- Argument(name = "--output-file", dest = "output")
    expect_equal(arg@dest, "output")
})

test_that("Argument sets metavar to uppercase name by default", {
    arg <- Argument(name = "--input")
    expect_equal(arg@metavar, "--INPUT")
})

test_that("Argument preserves explicit metavar", {
    arg <- Argument(name = "--input", metavar = "FILE")
    expect_equal(arg@metavar, "FILE")
})

test_that("Argument stores default value", {
    arg <- Argument(name = "--count", default = 10L)
    expect_equal(arg@default, 10L)
})
    
test_that("Argument stores type correctly", {
    arg_char <- Argument(name = "--name", type = "character")
    arg_int <- Argument(name = "--count", type = "integer")
    arg_num <- Argument(name = "--value", type = "numeric")
    arg_log <- Argument(name = "--flag", type = "logical")
    
    expect_equal(arg_char@type, "character")
    expect_equal(arg_int@type, "integer")
    expect_equal(arg_num@type, "numeric")
    expect_equal(arg_log@type, "logical")
})

test_that("Argument stores action correctly", {
    arg_store <- Argument(name = "--value", action = "store")
    arg_true <- Argument(name = "--verbose", action = "store_true")
    arg_false <- Argument(name = "--no-cache", action = "store_false")
    arg_count <- Argument(name = "-v", action = "count")
    
    expect_equal(arg_store@action, "store")
    expect_equal(arg_true@action, "store_true")
    expect_equal(arg_false@action, "store_false")
    expect_equal(arg_count@action, "count")
})

test_that("Argument stores nargs correctly", {
    arg_optional <- Argument(name = "--file", nargs = "?")
    arg_one_plus <- Argument(name = "--files", nargs = "+")
    arg_zero_plus <- Argument(name = "--items", nargs = "*")
    
    expect_equal(arg_optional@nargs, "?")
    expect_equal(arg_one_plus@nargs, "+")
    expect_equal(arg_zero_plus@nargs, "*")
})

test_that("Argument stores required flag correctly", {
    arg_optional <- Argument(name = "--output", required = FALSE)
    arg_required <- Argument(name = "--input", required = TRUE)
    
    expect_false(arg_optional@required)
    expect_true(arg_required@required)
})

test_that("Argument stores choices correctly", {
    arg <- Argument(name = "--format", choices = c("csv", "json", "xml"))
    expect_equal(arg@choices, c("csv", "json", "xml"))
})

test_that("Argument stores help text correctly", {
    arg <- Argument(name = "--verbose", help = "Enable verbose output")
    expect_equal(arg@help, "Enable verbose output")
})

test_that("Argument can be created with all properties", {
    arg <- Argument(
        name = "--format",
        dest = "output_format",
        help = "Output format",
        default = "csv",
        type = "character",
        action = "store",
        nargs = "?",
        required = FALSE,
        choices = c("csv", "json"),
        metavar = "FMT"
    )
    
    expect_equal(arg@name, "--format")
    expect_equal(arg@dest, "output_format")
    expect_equal(arg@help, "Output format")
    expect_equal(arg@default, "csv")
    expect_equal(arg@type, "character")
    expect_equal(arg@action, "store")
    expect_equal(arg@nargs, "?")
    expect_false(arg@required)
    expect_equal(arg@choices, c("csv", "json"))
    expect_equal(arg@metavar, "FMT")
})


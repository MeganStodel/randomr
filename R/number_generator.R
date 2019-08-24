
#' Generate random numbers.
#'
#' @param sample_size A number indicating how many random numbers to return.
#' @param distribution Either "normal", "poisson" or "binomial" to indicate distribution type.
#' @param ... Other arguments passed on.
#' @return A vector of random numbers distributed as specified.
#' @examples
#' generate_numbers(100, "normal")
#' generate_numbers(100, "poisson", lambda = 10)
#' @export

generate_numbers <- function(sample_size, distribution, ...) {
    if (distribution == "normal") {
        numbers <- stats::rnorm(n = sample_size)
    } else if (distribution == "poisson") {
        numbers <- stats::rpois(n = sample_size, ...)
    } else if (distribution == "binomial") {
        numbers <- stats::rbinom(n = sample_size, ...)
    } else {
        stop("Please supply the distribution argument with one of 'normal', 'poisson' or 'binomial'.")
    }
    class(numbers) <- "numbers_class"
    return(numbers)
}


#' Summary function for numbers_class objects.
#'
#' @param object A numbers_class object.
#' @param ... Additional arguments affecting the summary produced - not in use.
#' @return A summary of the object, including min, max, median, mean and IQR.
#' @examples
#' summary(generate_numbers(100, "normal"))
#' @export


summary.numbers_class <- function(object, ...) {
    stopifnot(inherits(object, "numbers_class"))
    return(cat("Summary of", deparse(substitute(object)), ": ",
               "\t\nThe minimum value is:", min(object),
               "\t\nThe maximum value is:", max(object),
               "\t\nThe median is:", stats::median(object),
               "\t\nThe mean is:", mean(object),
               "\t\nThe interquartile range is", stats::IQR(object), "and goes from",
               stats::quantile(object)[[2]], "to", stats::quantile(object)[[4]]))
}

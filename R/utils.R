#' Convert an annual interest rate to a monthly rate
#'
#' Brazilian lenders quote mortgage rates either as an effective annual rate
#' (*taxa efetiva*) or as a nominal annual rate (*taxa nominal*, i.e. 12 times
#' the monthly rate). This function converts either form to the monthly rate
#' used in amortization formulas.
#'
#' @param i Annual interest rate as a decimal (e.g. `0.10` for 10% a.a.).
#'   Vectorized.
#' @param rate_type Either `"effective"` (default), which compounds:
#'   `(1 + i)^(1/12) - 1`, or `"nominal"`, which divides: `i / 12`.
#'
#' @return Numeric vector of monthly rates.
#' @export
#' @examples
#' annual_to_monthly(0.10)
#' annual_to_monthly(0.10, rate_type = "nominal")
annual_to_monthly <- function(i, rate_type = c("effective", "nominal")) {
  rate_type <- rlang::arg_match(rate_type)
  check_rate(i)
  if (rate_type == "effective") (1 + i)^(1 / 12) - 1 else i / 12
}

# Validation helpers -----------------------------------------------------------
# All checks are vectorized: they fail if *any* element is invalid and report
# the offending positions. NAs pass through and propagate to the output.

check_rate <- function(
  i,
  arg = rlang::caller_arg(i),
  call = rlang::caller_env()
) {
  check_numeric(i, arg = arg, call = call)
  bad <- which(!is.na(i) & i < 0)
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a non-negative annual rate in decimal form.",
        "x" = "Invalid value(s) at position(s) {bad}: {.val {i[bad]}}."
      ),
      call = call
    )
  }
  invisible(i)
}

check_unit_interval <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_numeric(x, arg = arg, call = call)
  bad <- which(!is.na(x) & (x <= 0 | x > 1))
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be in (0, 1].",
        "x" = "Invalid value(s) at position(s) {bad}: {.val {x[bad]}}."
      ),
      call = call
    )
  }
  invisible(x)
}

check_positive <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_numeric(x, arg = arg, call = call)
  bad <- which(!is.na(x) & x <= 0)
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be positive.",
        "x" = "Invalid value(s) at position(s) {bad}: {.val {x[bad]}}."
      ),
      call = call
    )
  }
  invisible(x)
}

check_non_negative <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_numeric(x, arg = arg, call = call)
  bad <- which(!is.na(x) & x < 0)
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be zero or positive.",
        "x" = "Invalid value(s) at position(s) {bad}: {.val {x[bad]}}."
      ),
      call = call
    )
  }
  invisible(x)
}

check_term <- function(
  n,
  arg = rlang::caller_arg(n),
  call = rlang::caller_env()
) {
  check_numeric(n, arg = arg, call = call)
  bad <- which(!is.na(n) & (!is.finite(n) | n < 1 | n != round(n)))
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a whole number of months, at least 1.",
        "x" = "Invalid value(s) at position(s) {bad}: {.val {n[bad]}}."
      ),
      call = call
    )
  }
  invisible(n)
}

# A bare `NA` is logical; accept it so missing values propagate.
check_numeric <- function(x, arg, call) {
  if (!is.numeric(x) && !(is.logical(x) && all(is.na(x)))) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  invisible(x)
}

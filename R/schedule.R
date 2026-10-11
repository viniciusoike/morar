#' Full amortization schedule
#'
#' Month-by-month schedule for a single loan under SAC, Price or mixed
#' amortization, including insurance and fees.
#'
#' @param price Property price (scalar).
#' @inheritParams loan-args
#'
#' @return A tibble of class `morar_schedule` with one row per month and
#'   columns `period`, `balance_start`, `interest`, `principal`, `payment`
#'   (interest plus principal), `charges` (MIP, DFI and the administrative
#'   fee), `total_payment` and `balance_end`. Loan parameters are stored in
#'   the `loan` attribute.
#' @export
#' @examples
#' sched <- amortization_table(500000, i = 0.10)
#' sched
#'
#' # Compare methods
#' sac <- amortization_table(500000, i = 0.10, ltv = 0.7, method = "sac")
#' price <- amortization_table(500000, i = 0.10, ltv = 0.7, method = "price")
#' sum(price$interest) - sum(sac$interest)
amortization_table <- function(
  price,
  i,
  ltv = NULL,
  n = 360,
  method = c("sac", "price", "mixed"),
  rate_type = c("effective", "nominal"),
  mip_rate = 0,
  dfi_rate = 0,
  admin_fee = 0
) {
  method <- rlang::arg_match(method)
  rate_type <- rlang::arg_match(rate_type)
  ltv <- ltv %||% default_ltv(method)
  scalars <- list(
    price = price,
    i = i,
    ltv = ltv,
    n = n,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee
  )
  if (any(lengths(scalars) != 1)) {
    cli::cli_abort(
      c(
        "{.fn amortization_table} builds one schedule at a time.",
        "i" = "All numeric arguments must be scalars.",
        ">" = "Use {.fn loan_summary} for many loans at once."
      )
    )
  }
  check_positive(price)
  check_unit_interval(ltv)
  check_term(n)
  check_non_negative(mip_rate)
  check_non_negative(dfi_rate)
  check_non_negative(admin_fee)
  missing <- names(scalars)[vapply(scalars, anyNA, logical(1))]
  if (length(missing) > 0) {
    cli::cli_abort("{.arg {missing[[1]]}} must not be missing in a schedule.")
  }

  r <- annual_to_monthly(i, rate_type)
  d0 <- price * ltv
  t <- seq_len(n)

  # SAC: balance falls linearly.
  balance_sac <- d0 * (1 - (t - 1) / n)
  payment_sac <- d0 / n + r * balance_sac

  # Price: constant payment.
  payment_price <- rep(d0 * payment_factor(r, n, "price"), n)

  payment <- switch(
    method,
    sac = payment_sac,
    price = payment_price,
    mixed = (payment_sac + payment_price) / 2
  )

  # The balance recursion is linear in the payment stream, so one pass works
  # for every method, including mixed.
  balance_start <- numeric(n)
  balance <- d0
  for (k in t) {
    balance_start[k] <- balance
    balance <- balance * (1 + r) - payment[k]
  }
  interest <- r * balance_start
  principal <- payment - interest
  balance_end <- balance_start - principal
  # Clean floating-point residue on the last balance
  balance_end[abs(balance_end) < 1e-6 * d0] <- 0
  charges <- mip_rate * balance_start + dfi_rate * price + admin_fee

  out <- tibble::tibble(
    period = t,
    balance_start = balance_start,
    interest = interest,
    principal = principal,
    payment = payment,
    charges = charges,
    total_payment = payment + charges,
    balance_end = balance_end
  )
  attr(out, "loan") <- list(
    price = price,
    loan = d0,
    i = i,
    rate_monthly = r,
    n = n,
    ltv = ltv,
    method = method,
    rate_type = rate_type
  )
  class(out) <- c("morar_schedule", class(out))
  out
}

#' @export
print.morar_schedule <- function(x, ...) {
  info <- attr(x, "loan")
  fmt <- function(v) {
    format(
      round(v, 2),
      big.mark = ".",
      decimal.mark = ",",
      nsmall = 2,
      scientific = FALSE
    )
  }
  cli::cli_text(
    "{.strong {toupper(info$method)}} schedule: ",
    "R$ {fmt(info$loan)} over {info$n} months at ",
    "{round(100 * info$i, 2)}% a.a. ({info$rate_type})"
  )
  cli::cli_text(
    "First payment R$ {fmt(x$total_payment[1])} | ",
    "last R$ {fmt(x$total_payment[nrow(x)])} | ",
    "total interest R$ {fmt(sum(x$interest))}"
  )
  NextMethod()
  invisible(x)
}

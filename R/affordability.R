#' Shared loan arguments
#'
#' @param i Annual interest rate in decimal form (e.g. `0.10`). Use the
#'   all-in rate: for SFH loans indexed to TR, add the expected TR to the
#'   contract spread.
#' @param ltv Loan-to-value ratio, in (0, 1]. `NULL` (default) uses
#'   [default_ltv()] for the chosen `method`.
#' @param n Loan term in months. Default `360` (30 years).
#' @param pti Maximum payment-to-income ratio: the share of monthly income
#'   the first payment may take, in (0, 1]. Default `0.3`.
#' @param method Amortization method: `"sac"` (constant amortization, default),
#'   `"price"` (constant payment) or `"mixed"` (average of SAC and Price
#'   payments, the *Sistema de Amortização Misto*).
#' @param rate_type How `i` is quoted: `"effective"` (default) or `"nominal"`.
#'   See [annual_to_monthly()].
#' @param mip_rate Monthly death and disability insurance (MIP) rate, applied
#'   to the outstanding loan balance. Default `0`.
#' @param dfi_rate Monthly property damage insurance (DFI) rate, applied to
#'   the property price. Default `0`.
#' @param admin_fee Monthly administrative fee in reais. Default `0`.
#'
#' @details
#' Affordability is evaluated at the **first** payment, which is the highest
#' one under SAC. The calculation assumes a fixed monthly interest rate; it
#' does not model indexation of the outstanding balance or determine bank
#' approval. The first payment includes MIP, DFI and the administrative fee.
#' MIP rates depend on the borrower's age, so charges default to zero.
#'
#' All numeric arguments are vectorized and recycled to a common length
#' following tidyverse recycling rules (length 1 or the common length), so the
#' functions can be used directly inside `dplyr::mutate()`.
#'
#' @name loan-args
#' @keywords internal
NULL

#' Default loan-to-value ratio by amortization method
#'
#' Caixa SBPE convention checked in September 2026: 80% for SAC and 70% for
#' Price. The mixed method uses the Price limit. Pass `ltv` explicitly for
#' another product or lender.
#'
#' @inheritParams loan-args
#'
#' @return A number in (0, 1].
#' @export
#' @examples
#' default_ltv("sac")
#' default_ltv("price")
default_ltv <- function(method = c("sac", "price", "mixed")) {
  method <- rlang::arg_match(method)
  ltv_defaults[[method]]
}

# Caixa SBPE limits, checked 2026-09-30. Update when the rules change.
ltv_defaults <- c(sac = 0.8, price = 0.7, mixed = 0.7)

#' First-payment factor
#'
#' First monthly payment, excluding insurance and fees, per unit of debt:
#' `payment = debt * factor`.
#'
#' @param rate_m Monthly interest rate (decimal).
#' @inheritParams loan-args
#'
#' @return Numeric vector.
#' @export
#' @examples
#' payment_factor(annual_to_monthly(0.10), n = 360)
#' payment_factor(annual_to_monthly(0.10), n = 360, method = "price")
payment_factor <- function(
  rate_m,
  n = 360,
  method = c("sac", "price", "mixed")
) {
  method <- rlang::arg_match(method)
  check_rate(rate_m)
  check_term(n)
  args <- vctrs::vec_recycle_common(rate_m = rate_m, n = n)
  rate_m <- args$rate_m
  n <- args$n

  sac <- 1 / n + rate_m
  price <- ifelse(rate_m == 0, 1 / n, rate_m / (1 - (1 + rate_m)^(-n)))

  switch(method, sac = sac, price = price, mixed = (sac + price) / 2)
}

#' First monthly payment for a property
#'
#' The first payment of the loan, including insurance and fees. Banks check
#' this payment against income for credit approval. Under SAC it is the
#' *highest* payment of the schedule.
#'
#' @param price Property price. Vectorized.
#' @inheritParams loan-args
#'
#' @return Numeric vector of first-month payments.
#' @export
#' @examples
#' first_payment(c(300000, 500000), i = 0.10)
#' first_payment(500000, i = 0.10, mip_rate = 0.0003, admin_fee = 25)
first_payment <- function(
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
  check_positive(price)
  terms <- loan_terms(
    price = price,
    i = i,
    ltv = ltv,
    n = n,
    method = method,
    rate_type = rate_type,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee
  )
  terms$price * terms$coef + terms$admin_fee
}

#' Minimum income to finance a property
#'
#' Minimum monthly household income such that the first payment does not
#' exceed `pti` of income.
#'
#' @inheritParams first_payment
#' @inheritParams loan-args
#'
#' @return Numeric vector of minimum monthly incomes.
#' @seealso [max_price()] for the inverse, [loan_summary()] for all
#'   components at once.
#' @export
#' @examples
#' min_income(500000, i = 0.10)
#' min_income(c(250000, 350000, 500000), i = 0.10, method = "price")
min_income <- function(
  price,
  i,
  ltv = NULL,
  n = 360,
  pti = 0.3,
  method = c("sac", "price", "mixed"),
  rate_type = c("effective", "nominal"),
  mip_rate = 0,
  dfi_rate = 0,
  admin_fee = 0
) {
  check_positive(price)
  check_unit_interval(pti)
  terms <- loan_terms(
    price = price,
    pti = pti,
    i = i,
    ltv = ltv,
    n = n,
    method = method,
    rate_type = rate_type,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee
  )
  (terms$price * terms$coef + terms$admin_fee) / terms$pti
}

#' Maximum financeable property price for a given income
#'
#' Inverts [min_income()]: the highest property price whose first payment
#' fits within `pti` of the given monthly income. When `cash` is supplied,
#' the price must also fit the available down payment. The lower limit binds.
#' The loan-to-value ratio stays fixed; unused cash does not reduce the loan.
#' Purchase taxes, registration and other upfront costs are not included.
#' Returns `0` when the income does not cover the administrative fee.
#'
#' @param income Monthly household income. Vectorized.
#' @param cash Cash available for the down payment, in reais. `NULL` (default)
#'   applies no cash constraint. Vectorized; may be zero.
#' @inheritParams min_income
#'
#' @return Numeric vector of maximum property prices.
#' @export
#' @examples
#' max_price(10000, i = 0.10)
#'
#' # Over an income distribution
#' max_price(c(3000, 6000, 12000), i = c(0.05, 0.07, 0.10))
max_price <- function(
  income,
  i,
  ltv = NULL,
  n = 360,
  pti = 0.3,
  method = c("sac", "price", "mixed"),
  rate_type = c("effective", "nominal"),
  mip_rate = 0,
  dfi_rate = 0,
  admin_fee = 0,
  cash = NULL
) {
  check_positive(income)
  check_unit_interval(pti)
  if (!is.null(cash)) {
    check_non_negative(cash)
  }
  terms <- loan_terms(
    income = income,
    cash = cash,
    pti = pti,
    i = i,
    ltv = ltv,
    n = n,
    method = method,
    rate_type = rate_type,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee
  )
  income_limit <- pmax(
    (terms$income * terms$pti - terms$admin_fee) / terms$coef,
    0
  )
  if (is.null(cash)) {
    return(income_limit)
  }

  cash_limit <- ifelse(
    terms$ltv == 1 & !is.na(terms$cash),
    Inf,
    terms$cash / (1 - terms$ltv)
  )
  return(pmin(income_limit, cash_limit))
}

#' Maximum loan amount for a given income
#'
#' The largest debt (financed amount) whose first payment fits within `pti`
#' of income and, when `cash` is supplied, whose down payment is affordable.
#' Equal to `max_price() * ltv`.
#'
#' @inheritParams max_price
#'
#' @return Numeric vector of maximum loan amounts.
#' @export
#' @examples
#' max_loan(10000, i = 0.10)
max_loan <- function(
  income,
  i,
  ltv = NULL,
  n = 360,
  pti = 0.3,
  method = c("sac", "price", "mixed"),
  rate_type = c("effective", "nominal"),
  mip_rate = 0,
  dfi_rate = 0,
  admin_fee = 0,
  cash = NULL
) {
  method <- rlang::arg_match(method)
  ltv <- ltv %||% default_ltv(method)
  price <- max_price(
    income,
    i = i,
    ltv = ltv,
    n = n,
    pti = pti,
    method = method,
    rate_type = rate_type,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee,
    cash = cash
  )
  return(price * vctrs::vec_recycle(ltv, length(price)))
}

#' Loan summary
#'
#' All affordability components for one or more properties, as a tibble.
#'
#' @inheritParams min_income
#'
#' @return A tibble with one row per property and columns `price`,
#'   `down_payment`, `loan`, `charges` (first-month insurance and fees),
#'   `first_payment`, `min_income`, `rate_monthly`, `term` and `method`.
#' @export
#' @examples
#' loan_summary(c(300000, 500000), i = 0.10)
loan_summary <- function(
  price,
  i,
  ltv = NULL,
  n = 360,
  pti = 0.3,
  method = c("sac", "price", "mixed"),
  rate_type = c("effective", "nominal"),
  mip_rate = 0,
  dfi_rate = 0,
  admin_fee = 0
) {
  check_positive(price)
  check_unit_interval(pti)
  terms <- loan_terms(
    price = price,
    pti = pti,
    i = i,
    ltv = ltv,
    n = n,
    method = method,
    rate_type = rate_type,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee
  )
  loan <- terms$price * terms$ltv
  payment <- terms$price * terms$coef + terms$admin_fee

  tibble::tibble(
    price = terms$price,
    down_payment = terms$price - loan,
    loan = loan,
    charges = payment - loan * terms$factor,
    first_payment = payment,
    min_income = payment / terms$pti,
    rate_monthly = terms$rate_m,
    term = terms$n,
    method = terms$method
  )
}

# Validates the shared loan arguments, recycles them together with the
# vectors passed in `...`, and adds the first-payment cost per R$ 1 of price
# (`coef`), excluding the flat fee. Every affordability function is a
# rearrangement of `first_payment = price * coef + admin_fee`.
loan_terms <- function(
  ...,
  i,
  ltv,
  n,
  method,
  rate_type,
  mip_rate,
  dfi_rate,
  admin_fee,
  call = rlang::caller_env()
) {
  method <- rlang::arg_match(
    method,
    c("sac", "price", "mixed"),
    error_call = call
  )
  rate_type <- rlang::arg_match(
    rate_type,
    c("effective", "nominal"),
    error_call = call
  )
  ltv <- ltv %||% default_ltv(method)
  check_unit_interval(ltv, call = call)
  check_non_negative(mip_rate, call = call)
  check_non_negative(dfi_rate, call = call)
  check_non_negative(admin_fee, call = call)

  rate_m <- annual_to_monthly(i, rate_type)
  factor <- payment_factor(rate_m, n = n, method = method)

  terms <- vctrs::vec_recycle_common(
    ...,
    rate_m = rate_m,
    ltv = ltv,
    n = n,
    factor = factor,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee,
    .call = call
  )
  terms$coef <- terms$ltv * (terms$factor + terms$mip_rate) + terms$dfi_rate
  terms$method <- method
  terms
}

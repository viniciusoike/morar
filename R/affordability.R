#' Shared loan arguments
#'
#' @param i Annual interest rate in decimal form (e.g. `0.10`).
#' @param ltv Loan-to-value ratio, in (0, 1]. Default `0.7` (30% down payment).
#' @param n Loan term in months. Default `360` (30 years).
#' @param comp Maximum share of income committed to the first payment, in
#'   (0, 1]. Default `0.3`.
#' @param method Amortization method: `"sac"` (constant amortization, default),
#'   `"price"` (constant payment) or `"misto"` (average of SAC and Price
#'   payments).
#' @param rate_type How `i` is quoted: `"effective"` (default) or `"nominal"`.
#'   See [annual_to_monthly()].
#'
#' @details
#' Affordability is evaluated at the **first** payment, which is the binding
#' one for bank approval: under SAC it is the highest payment of the loan;
#' under Price all payments are equal.
#'
#' All numeric arguments are vectorized and recycled to a common length
#' following tidyverse recycling rules (length 1 or the common length), so the
#' functions can be used directly inside `dplyr::mutate()`.
#'
#' @name loan-args
#' @keywords internal
NULL

#' First-payment factor
#'
#' First monthly payment per unit of debt. This is the single quantity that
#' links debt, payment, income and price across all functions in the package:
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
payment_factor <- function(rate_m, n = 360,
                           method = c("sac", "price", "misto")) {
  method <- rlang::arg_match(method)
  check_term(n)
  args <- vctrs::vec_recycle_common(rate_m = rate_m, n = n)
  rate_m <- args$rate_m
  n <- args$n

  sac <- 1 / n + rate_m
  price <- ifelse(rate_m == 0, 1 / n, rate_m / (1 - (1 + rate_m)^(-n)))

  switch(method,
    sac = sac,
    price = price,
    misto = (sac + price) / 2
  )
}

#' Minimum income to finance a property
#'
#' Minimum monthly household income such that the first payment does not
#' exceed `comp` of income.
#'
#' @param p Property price. Vectorized.
#' @inheritParams loan-args
#'
#' @return Numeric vector of minimum monthly incomes.
#' @seealso [inc_to_price()] for the inverse, [loan_summary()] for all
#'   components at once.
#' @export
#' @examples
#' renda_minima(500000, i = 0.10)
#' renda_minima(c(250000, 350000, 500000), i = 0.10, method = "price")
renda_minima <- function(p, i, ltv = 0.7, n = 360, comp = 0.3,
                         method = c("sac", "price", "misto"),
                         rate_type = c("effective", "nominal")) {
  check_positive(p)
  check_unit_interval(comp)
  parcela_minima(p, i = i, ltv = ltv, n = n, method = method,
                 rate_type = rate_type) / comp
}

#' Maximum financeable property price for a given income
#'
#' Inverts [renda_minima()]: the highest property price whose first payment
#' fits within `comp` of the given monthly income.
#'
#' @param renda Monthly household income. Vectorized.
#' @inheritParams loan-args
#'
#' @return Numeric vector of maximum property prices.
#' @export
#' @examples
#' inc_to_price(10000, i = 0.10)
#'
#' # Over an income distribution
#' inc_to_price(c(3000, 6000, 12000), i = c(0.05, 0.07, 0.10))
inc_to_price <- function(renda, i, ltv = 0.7, n = 360, comp = 0.3,
                         method = c("sac", "price", "misto"),
                         rate_type = c("effective", "nominal")) {
  check_positive(renda)
  check_unit_interval(ltv)
  check_unit_interval(comp)
  rate_m <- annual_to_monthly(i, rate_type)
  factor <- payment_factor(rate_m, n = n, method = method)
  args <- vctrs::vec_recycle_common(
    renda = renda, ltv = ltv, comp = comp, factor = factor
  )
  args$renda * args$comp / (args$ltv * args$factor)
}

#' Maximum loan amount for a given income
#'
#' The largest debt (financed amount) whose first payment fits within `comp`
#' of income. Equal to `inc_to_price() * ltv`.
#'
#' @inheritParams inc_to_price
#'
#' @return Numeric vector of maximum loan amounts.
#' @export
#' @examples
#' emprestimo_max(10000, i = 0.10)
emprestimo_max <- function(renda, i, ltv = 0.7, n = 360, comp = 0.3,
                           method = c("sac", "price", "misto"),
                           rate_type = c("effective", "nominal")) {
  preco <- inc_to_price(renda, i = i, ltv = ltv, n = n, comp = comp,
                        method = method, rate_type = rate_type)
  preco * vctrs::vec_recycle(ltv, length(preco))
}

#' First monthly payment for a property
#'
#' The first payment of the loan, which is the one evaluated for credit
#' approval. Under SAC it is the *highest* payment of the schedule.
#'
#' @param p Property price. Vectorized.
#' @inheritParams loan-args
#'
#' @return Numeric vector of first-month payments.
#' @export
#' @examples
#' parcela_minima(c(300000, 500000), i = 0.10)
parcela_minima <- function(p, i, ltv = 0.7, n = 360,
                           method = c("sac", "price", "misto"),
                           rate_type = c("effective", "nominal")) {
  check_positive(p)
  check_unit_interval(ltv)
  rate_m <- annual_to_monthly(i, rate_type)
  factor <- payment_factor(rate_m, n = n, method = method)
  args <- vctrs::vec_recycle_common(p = p, ltv = ltv, factor = factor)
  args$p * args$ltv * args$factor
}

#' Loan summary
#'
#' All affordability components for one or more properties, as a tibble.
#'
#' @inheritParams renda_minima
#'
#' @return A tibble with one row per property and columns `preco`, `entrada`
#'   (down payment), `financiamento` (financed amount), `parcela` (first
#'   payment), `renda_minima`, `taxa_mensal` and `method`.
#' @export
#' @examples
#' loan_summary(c(300000, 500000), i = 0.10)
loan_summary <- function(p, i, ltv = 0.7, n = 360, comp = 0.3,
                         method = c("sac", "price", "misto"),
                         rate_type = c("effective", "nominal")) {
  method <- rlang::arg_match(method)
  rate_type <- rlang::arg_match(rate_type)
  parcela <- parcela_minima(p, i = i, ltv = ltv, n = n, method = method,
                            rate_type = rate_type)
  args <- vctrs::vec_recycle_common(
    p = p, i = i, ltv = ltv, n = n, comp = comp, parcela = parcela
  )
  tibble::tibble(
    preco = args$p,
    entrada = args$p * (1 - args$ltv),
    financiamento = args$p * args$ltv,
    parcela = args$parcela,
    renda_minima = args$parcela / args$comp,
    taxa_mensal = annual_to_monthly(args$i, rate_type),
    prazo = args$n,
    method = method
  )
}

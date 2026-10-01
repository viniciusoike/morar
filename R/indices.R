#' Housing Affordability Index (HAI)
#'
#' Ratio between household income and the income required to finance a
#' property, times 100. An HAI of 100 means the household earns exactly the
#' qualifying income; above 100 it has room to spare.
#'
#' @param price Property price (e.g. a median price). Vectorized.
#' @param income Monthly household income (e.g. a median income). Vectorized.
#' @inheritParams min_income
#'
#' @return Numeric vector of index values.
#' @export
#' @examples
#' calc_hai(price = 400000, income = 8000, i = 0.10)
#'
#' # Same market under different rates
#' calc_hai(price = 400000, income = 8000, i = c(0.08, 0.10, 0.12))
calc_hai <- function(
  price,
  income,
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
  check_positive(income)
  qualifying <- min_income(
    price,
    i = i,
    ltv = ltv,
    n = n,
    pti = pti,
    method = method,
    rate_type = rate_type,
    mip_rate = mip_rate,
    dfi_rate = dfi_rate,
    admin_fee = admin_fee
  )
  args <- vctrs::vec_recycle_common(income = income, qualifying = qualifying)
  100 * args$income / args$qualifying
}

#' Price-to-income ratio (PIR)
#'
#' Property price divided by **annual** household income.
#'
#' @param price Property price. Vectorized.
#' @param income Monthly household income. Vectorized.
#' @param annual_factor Number of monthly incomes in a year. Default `12`.
#'   Use `40 / 3` (about 13.33) to include the 13th salary and the vacation
#'   bonus of formal Brazilian workers.
#'
#' @return Numeric vector.
#' @seealso [pir_category()]
#' @export
#' @examples
#' calc_pir(price = 500000, income = 10000)
#' calc_pir(price = 500000, income = 10000, annual_factor = 40 / 3)
calc_pir <- function(price, income, annual_factor = 12) {
  check_positive(price)
  check_positive(income)
  check_positive(annual_factor)
  args <- vctrs::vec_recycle_common(
    price = price,
    income = income,
    annual_factor = annual_factor
  )
  args$price / (args$annual_factor * args$income)
}

#' Classify price-to-income ratios
#'
#' Uses the Demographia International Housing Affordability thresholds:
#' up to 3 "Affordable", 3-4 "Moderately unaffordable", 4-5 "Seriously
#' unaffordable", above 5 "Severely unaffordable".
#'
#' @param pir Numeric vector of price-to-income ratios, as returned by
#'   [calc_pir()].
#'
#' @return An ordered factor.
#' @export
#' @examples
#' pir_category(c(2.5, 3.5, 4.5, 8))
pir_category <- function(pir) {
  check_numeric(pir, arg = "pir", call = rlang::current_env())
  cut(
    pir,
    breaks = c(-Inf, 3, 4, 5, Inf),
    labels = c(
      "Affordable",
      "Moderately unaffordable",
      "Seriously unaffordable",
      "Severely unaffordable"
    ),
    right = TRUE,
    ordered_result = TRUE
  )
}

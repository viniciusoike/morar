#' Housing Affordability Index (HAI)
#'
#' Ratio between household income and the income required to finance a
#' property, times 100. An HAI of 100 means the household earns exactly the
#' qualifying income; above 100 it has room to spare.
#'
#' @param p Property price (e.g. a median price). Vectorized.
#' @param renda Monthly household income (e.g. a median income). Vectorized.
#' @inheritParams loan-args
#'
#' @return Numeric vector of index values.
#' @export
#' @examples
#' calc_hai(p = 400000, renda = 8000, i = 0.10)
#'
#' # Same market under different rates
#' calc_hai(p = 400000, renda = 8000, i = c(0.08, 0.10, 0.12))
calc_hai <- function(p, renda, i, ltv = 0.7, n = 360, comp = 0.3,
                     method = c("sac", "price", "misto"),
                     rate_type = c("effective", "nominal")) {
  check_positive(renda)
  qualifying <- renda_minima(p, i = i, ltv = ltv, n = n, comp = comp,
                             method = method, rate_type = rate_type)
  args <- vctrs::vec_recycle_common(renda = renda, qualifying = qualifying)
  100 * args$renda / args$qualifying
}

#' Price-to-income ratio (PIR)
#'
#' Property price divided by **annual** household income.
#'
#' @param p Property price. Vectorized.
#' @param renda Monthly household income. Vectorized.
#'
#' @return Numeric vector.
#' @seealso [pir_category()]
#' @export
#' @examples
#' calc_pir(p = 500000, renda = 10000)
calc_pir <- function(p, renda) {
  check_positive(p)
  check_positive(renda)
  args <- vctrs::vec_recycle_common(p = p, renda = renda)
  args$p / (12 * args$renda)
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
      "Affordable", "Moderately unaffordable",
      "Seriously unaffordable", "Severely unaffordable"
    ),
    right = TRUE,
    ordered_result = TRUE
  )
}

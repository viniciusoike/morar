#' Full amortization schedule
#'
#' Month-by-month schedule for a single loan under SAC, Price or mixed
#' amortization. Replaces the old `loan()` function.
#'
#' @param p Property price (scalar).
#' @inheritParams loan-args
#'
#' @return A tibble of class `morar_schedule` with one row per month and
#'   columns `periodo`, `saldo_inicial`, `juros`, `amortizacao`, `parcela` and
#'   `saldo_final`. Loan parameters are stored as attributes.
#' @export
#' @examples
#' sched <- amortization_table(500000, i = 0.10)
#' sched
#'
#' # Compare methods
#' sac <- amortization_table(500000, i = 0.10, method = "sac")
#' price <- amortization_table(500000, i = 0.10, method = "price")
#' sum(sac$juros) - sum(price$juros)
amortization_table <- function(p, i, ltv = 0.7, n = 360,
                               method = c("sac", "price", "misto"),
                               rate_type = c("effective", "nominal")) {
  method <- rlang::arg_match(method)
  rate_type <- rlang::arg_match(rate_type)
  for (arg in list(p, i, ltv, n)) {
    if (length(arg) != 1) {
      cli::cli_abort(
        c(
          "{.fn amortization_table} builds one schedule at a time.",
          "i" = "{.arg p}, {.arg i}, {.arg ltv} and {.arg n} must be scalars.",
          ">" = "Use {.fn loan_summary} for many loans at once."
        )
      )
    }
  }
  check_positive(p)
  check_unit_interval(ltv)
  check_term(n)

  r <- annual_to_monthly(i, rate_type)
  d0 <- p * ltv
  t <- seq_len(n)

  # Closed forms: no loops needed.
  # SAC: balance falls linearly.
  saldo_sac <- d0 * (1 - (t - 1) / n)
  parcela_sac <- d0 / n + r * saldo_sac

  # Price: constant payment.
  pmt_price <- d0 * payment_factor(r, n, "price")
  parcela_price <- rep(pmt_price, n)

  parcela <- switch(method,
    sac = parcela_sac,
    price = parcela_price,
    misto = (parcela_sac + parcela_price) / 2
  )

  # Balance recursion is linear in the payment stream, so a single
  # cumulative pass works for every method (including misto).
  saldo_inicial <- numeric(n)
  saldo <- d0
  for (k in t) {
    saldo_inicial[k] <- saldo
    saldo <- saldo * (1 + r) - parcela[k]
  }
  juros <- r * saldo_inicial
  amortizacao <- parcela - juros
  saldo_final <- saldo_inicial - amortizacao
  # Clean floating-point residue on the last balance
  saldo_final[abs(saldo_final) < 1e-6 * d0] <- 0

  out <- tibble::tibble(
    periodo = t,
    saldo_inicial = saldo_inicial,
    juros = juros,
    amortizacao = amortizacao,
    parcela = parcela,
    saldo_final = saldo_final
  )
  attr(out, "loan") <- list(
    preco = p, financiamento = d0, i = i, taxa_mensal = r,
    n = n, ltv = ltv, method = method, rate_type = rate_type
  )
  class(out) <- c("morar_schedule", class(out))
  out
}

#' @export
print.morar_schedule <- function(x, ...) {
  info <- attr(x, "loan")
  fmt <- function(v) format(round(v, 2), big.mark = ".", decimal.mark = ",",
                            nsmall = 2, scientific = FALSE)
  cli::cli_text(
    "{.strong {toupper(info$method)}} schedule: ",
    "R$ {fmt(info$financiamento)} over {info$n} months at ",
    "{round(100 * info$i, 2)}% a.a. ({info$rate_type})"
  )
  cli::cli_text(
    "First payment R$ {fmt(x$parcela[1])} | ",
    "last R$ {fmt(x$parcela[nrow(x)])} | ",
    "total interest R$ {fmt(sum(x$juros))}"
  )
  NextMethod()
  invisible(x)
}

test_that("annual_to_monthly handles effective and nominal rates", {
  expect_equal(annual_to_monthly(0.12, "nominal"), 0.01)
  expect_equal((1 + annual_to_monthly(0.10))^12 - 1, 0.10)
  expect_equal(annual_to_monthly(c(0, 0.12), "nominal"), c(0, 0.01))
})

test_that("rates in percentage form are rejected, not silently converted", {
  expect_error(renda_minima(500000, i = 10), "decimal form")
  expect_error(inc_to_price(5000, i = c(0.1, 12)), "position\\(s\\) 2")
})

test_that("SAC first payment matches the hand calculation", {
  r <- annual_to_monthly(0.10)
  d <- 500000 * 0.7
  expect_equal(parcela_minima(500000, i = 0.10), d / 360 + r * d)
})

test_that("Price payment matches the annuity formula", {
  r <- 0.01
  d <- 100000
  expected <- d * r / (1 - (1 + r)^-120)
  expect_equal(
    parcela_minima(d, i = 0.12, ltv = 1, n = 120, method = "price",
                   rate_type = "nominal"),
    expected
  )
})

test_that("zero interest reduces to straight-line repayment", {
  for (m in c("sac", "price", "misto")) {
    expect_equal(
      parcela_minima(120000, i = 0, ltv = 1, n = 120, method = m), 1000
    )
  }
})

test_that("inc_to_price is the exact inverse of renda_minima", {
  prices <- c(150000, 350000, 500000, 1e6)
  for (m in c("sac", "price", "misto")) {
    rendas <- renda_minima(prices, i = 0.09, method = m)
    expect_equal(inc_to_price(rendas, i = 0.09, method = m), prices)
  }
})

test_that("emprestimo_max returns the loan, not the price", {
  expect_equal(
    emprestimo_max(10000, i = 0.10, ltv = 0.8),
    inc_to_price(10000, i = 0.10, ltv = 0.8) * 0.8
  )
})

test_that("SAC is the most demanding at the first payment", {
  args <- list(p = 500000, i = 0.10)
  sac <- do.call(renda_minima, c(args, method = "sac"))
  price <- do.call(renda_minima, c(args, method = "price"))
  misto <- do.call(renda_minima, c(args, method = "misto"))
  expect_gt(sac, misto)
  expect_gt(misto, price)
})

test_that("functions are vectorized and recycle arguments", {
  out <- inc_to_price(c(3000, 6000, 12000), i = c(0.05, 0.07, 0.10))
  expect_length(out, 3)
  expect_true(all(diff(out) > 0))
  expect_length(renda_minima(c(2e5, 3e5), i = 0.1, ltv = c(0.8, 0.7)), 2)
  expect_error(renda_minima(c(2e5, 3e5, 4e5), i = c(0.1, 0.2)))
})

test_that("NA inputs propagate instead of erroring", {
  out <- inc_to_price(c(5000, NA), i = 0.1)
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
})

test_that("loan_summary is internally consistent", {
  s <- loan_summary(c(300000, 500000), i = 0.10, comp = 0.3)
  expect_s3_class(s, "tbl_df")
  expect_equal(s$entrada + s$financiamento, s$preco)
  expect_equal(s$renda_minima * 0.3, s$parcela)
})

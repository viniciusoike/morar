test_that("annual_to_monthly handles effective and nominal rates", {
  expect_equal(annual_to_monthly(0.12, "nominal"), 0.01)
  expect_equal((1 + annual_to_monthly(0.10))^12 - 1, 0.10)
  expect_equal(annual_to_monthly(c(0, 0.12), "nominal"), c(0, 0.01))
})

test_that("rates above 100% a.a. are accepted, negative rates are not", {
  expect_no_error(min_income(500000, i = 1.5))
  expect_error(max_price(5000, i = c(0.1, -0.01)), "position\\(s\\) 2")
})

test_that("SAC first payment matches the hand calculation", {
  r <- annual_to_monthly(0.10)
  d <- 500000 * 0.7
  expect_equal(first_payment(500000, i = 0.10, ltv = 0.7), d / 360 + r * d)
})

test_that("Price payment matches the annuity formula", {
  r <- 0.01
  d <- 100000
  expected <- d * r / (1 - (1 + r)^-120)
  expect_equal(
    first_payment(
      d,
      i = 0.12,
      ltv = 1,
      n = 120,
      method = "price",
      rate_type = "nominal"
    ),
    expected
  )
})

test_that("zero interest reduces to straight-line repayment", {
  for (m in c("sac", "price", "mixed")) {
    expect_equal(
      first_payment(120000, i = 0, ltv = 1, n = 120, method = m),
      1000
    )
  }
})

test_that("default LTV depends on the method", {
  expect_equal(default_ltv("sac"), 0.8)
  expect_equal(default_ltv("price"), 0.7)
  expect_equal(default_ltv("mixed"), 0.7)
  expect_equal(
    first_payment(500000, i = 0.1, method = "price"),
    first_payment(500000, i = 0.1, ltv = 0.7, method = "price")
  )
  expect_equal(
    first_payment(500000, i = 0.1),
    first_payment(500000, i = 0.1, ltv = 0.8)
  )
})

test_that("charges add MIP on the loan, DFI on the price and a flat fee", {
  base <- first_payment(500000, i = 0.1, ltv = 0.8)
  with_charges <- first_payment(
    500000,
    i = 0.1,
    ltv = 0.8,
    mip_rate = 0.0002,
    dfi_rate = 0.0001,
    admin_fee = 25
  )
  expect_equal(with_charges - base, 0.0002 * 400000 + 0.0001 * 500000 + 25)
})

test_that("max_price is the exact inverse of min_income", {
  prices <- c(150000, 350000, 500000, 1e6)
  for (m in c("sac", "price", "mixed")) {
    incomes <- min_income(
      prices,
      i = 0.09,
      method = m,
      mip_rate = 0.0003,
      dfi_rate = 0.0001,
      admin_fee = 25
    )
    expect_equal(
      max_price(
        incomes,
        i = 0.09,
        method = m,
        mip_rate = 0.0003,
        dfi_rate = 0.0001,
        admin_fee = 25
      ),
      prices
    )
  }
})

test_that("max_price is zero when income does not cover the fixed fee", {
  expect_equal(max_price(50, i = 0.1, pti = 0.3, admin_fee = 25), 0)
})

test_that("max_loan returns the loan, not the price", {
  expect_equal(
    max_loan(10000, i = 0.10, ltv = 0.6),
    max_price(10000, i = 0.10, ltv = 0.6) * 0.6
  )
})

test_that("SAC is the most demanding at the first payment", {
  args <- list(price = 500000, i = 0.10, ltv = 0.7)
  sac <- do.call(min_income, c(args, method = "sac"))
  price <- do.call(min_income, c(args, method = "price"))
  mixed <- do.call(min_income, c(args, method = "mixed"))
  expect_gt(sac, mixed)
  expect_gt(mixed, price)
})

test_that("functions are vectorized and recycle every argument", {
  out <- max_price(c(3000, 6000, 12000), i = c(0.05, 0.07, 0.10))
  expect_length(out, 3)
  expect_true(all(diff(out) > 0))
  expect_length(min_income(c(2e5, 3e5), i = 0.1, ltv = c(0.8, 0.7)), 2)
  expect_error(min_income(c(2e5, 3e5, 4e5), i = c(0.1, 0.2)))
  expect_error(min_income(c(2e5, 3e5), i = 0.1, pti = c(0.3, 0.3, 0.3)))
})

test_that("NA inputs propagate instead of erroring", {
  out <- max_price(c(5000, NA), i = 0.1)
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
  expect_true(is.na(max_price(5000, i = NA)))
})

test_that("invalid terms and shares are rejected", {
  expect_error(payment_factor(0.01, n = Inf), "whole number")
  expect_error(payment_factor(0.01, n = 0), "whole number")
  expect_error(loan_summary(3e5, i = 0.1, pti = 0), "pti")
  expect_error(loan_summary(3e5, i = 0.1, pti = 2), "pti")
})

test_that("loan_summary is internally consistent", {
  s <- loan_summary(c(300000, 500000), i = 0.10, pti = 0.3, admin_fee = 25)
  expect_s3_class(s, "tbl_df")
  expect_equal(s$down_payment + s$loan, s$price)
  expect_equal(s$min_income * 0.3, s$first_payment)
  expect_equal(s$first_payment, first_payment(s$price, i = 0.1, admin_fee = 25))
  expect_equal(s$charges, c(25, 25))
})

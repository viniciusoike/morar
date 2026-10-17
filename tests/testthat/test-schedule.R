test_that("schedules fully repay the loan for every method", {
  for (m in c("sac", "price", "mixed")) {
    s <- amortization_table(500000, i = 0.10, ltv = 0.7, method = m)
    expect_equal(nrow(s), 360)
    expect_equal(sum(s$principal), 350000)
    expect_equal(s$balance_end[360], 0, tolerance = 1e-6)
    expect_equal(s$payment, s$interest + s$principal)
  }
})

test_that("SAC has constant principal, Price has constant payment", {
  sac <- amortization_table(500000, i = 0.10, ltv = 0.7, method = "sac")
  price <- amortization_table(500000, i = 0.10, ltv = 0.7, method = "price")
  expect_equal(unique(round(sac$principal, 8)), round(350000 / 360, 8))
  expect_equal(length(unique(round(price$payment, 8))), 1)
})

test_that("first scheduled total payment matches first_payment", {
  for (m in c("sac", "price", "mixed")) {
    s <- amortization_table(
      400000,
      i = 0.11,
      n = 420,
      method = m,
      mip_rate = 0.0003,
      dfi_rate = 0.0001,
      admin_fee = 25
    )
    expect_equal(
      s$total_payment[1],
      first_payment(
        400000,
        i = 0.11,
        n = 420,
        method = m,
        mip_rate = 0.0003,
        dfi_rate = 0.0001,
        admin_fee = 25
      )
    )
  }
})

test_that("MIP falls with the balance, DFI and the fee stay flat", {
  s <- amortization_table(
    500000,
    i = 0.10,
    ltv = 0.8,
    mip_rate = 0.0003,
    dfi_rate = 0.0001,
    admin_fee = 25
  )
  expect_equal(s$charges, 0.0003 * s$balance_start + 0.0001 * 500000 + 25)
  expect_true(all(diff(s$charges) < 0))
  expect_equal(s$total_payment, s$payment + s$charges)
})

test_that("Price pays more total interest than SAC", {
  sac <- amortization_table(500000, i = 0.10, ltv = 0.7, method = "sac")
  price <- amortization_table(500000, i = 0.10, ltv = 0.7, method = "price")
  expect_gt(sum(price$interest), sum(sac$interest))
})

test_that("amortization_table rejects vector inputs", {
  expect_error(amortization_table(c(1e5, 2e5), i = 0.1), "one schedule")
})

test_that("amortization_table rejects missing scalar inputs", {
  expect_error(amortization_table(500000, i = 0.1, n = NA), "missing")
  expect_error(amortization_table(500000, i = NA), "missing")
})

test_that("print method works", {
  s <- amortization_table(500000, i = 0.10)
  expect_output(print(s), "period")
})

test_that("schedules fully repay the loan for every method", {
  for (m in c("sac", "price", "misto")) {
    s <- amortization_table(500000, i = 0.10, method = m)
    expect_equal(nrow(s), 360)
    expect_equal(sum(s$amortizacao), 350000)
    expect_equal(s$saldo_final[360], 0, tolerance = 1e-6)
    expect_equal(s$parcela, s$juros + s$amortizacao)
  }
})

test_that("SAC has constant amortization, Price has constant payment", {
  sac <- amortization_table(500000, i = 0.10, method = "sac")
  price <- amortization_table(500000, i = 0.10, method = "price")
  expect_equal(unique(round(sac$amortizacao, 8)), round(350000 / 360, 8))
  expect_equal(length(unique(round(price$parcela, 8))), 1)
})

test_that("first scheduled payment matches parcela_minima", {
  for (m in c("sac", "price", "misto")) {
    s <- amortization_table(400000, i = 0.11, n = 420, method = m)
    expect_equal(s$parcela[1], parcela_minima(400000, i = 0.11, n = 420,
                                              method = m))
  }
})

test_that("Price pays more total interest than SAC", {
  sac <- amortization_table(500000, i = 0.10, method = "sac")
  price <- amortization_table(500000, i = 0.10, method = "price")
  expect_gt(sum(price$juros), sum(sac$juros))
})

test_that("amortization_table rejects vector inputs", {
  expect_error(amortization_table(c(1e5, 2e5), i = 0.1), "one schedule")
})

test_that("print method works", {
  s <- amortization_table(500000, i = 0.10)
  expect_output(print(s), "periodo")
})

test_that("HAI equals 100 at the qualifying income", {
  q <- min_income(400000, i = 0.10)
  expect_equal(calc_hai(400000, q, i = 0.10), 100)
  expect_equal(calc_hai(400000, 2 * q, i = 0.10), 200)
})

test_that("HAI falls as rates rise", {
  h <- calc_hai(400000, 8000, i = c(0.08, 0.10, 0.12))
  expect_true(all(diff(h) < 0))
})

test_that("PIR annualizes monthly income with annual_factor", {
  expect_equal(calc_pir(600000, 10000), 5)
  expect_equal(calc_pir(400000, 10000, annual_factor = 40 / 3), 3)
})

test_that("PIR categories follow Demographia thresholds", {
  cats <- pir_category(c(2.5, 3, 3.5, 4.5, 8, NA))
  expect_equal(
    as.character(cats),
    c(
      "Affordable",
      "Affordable",
      "Moderately unaffordable",
      "Seriously unaffordable",
      "Severely unaffordable",
      NA
    )
  )
  expect_true(is.ordered(cats))
})

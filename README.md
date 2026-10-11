
<!-- README.md is generated from README.Rmd. Please edit that file -->

# morar <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/viniciusoike/morar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/viniciusoike/morar/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

morar computes housing affordability for fixed-rate Brazilian mortgage
scenarios. It answers two questions: how much income a household needs
to buy a given property, and how expensive a property a given income and
down payment can support. It handles SAC, Price and mixed amortization,
effective and nominal rates, insurance (MIP and DFI) and administrative
fees. Every core function is vectorized, so it works on income or price
distributions inside `dplyr::mutate()`.

## Installation

``` r
# install.packages("pak")
pak::pak("viniciusoike/morar")
```

## Income and price

`min_income()` returns the monthly income needed to finance a property,
and `max_price()` inverts it. By default the first payment may take 30%
of income. The default loan-to-value ratio uses a Caixa SBPE convention
checked in September 2026; pass `ltv` for another product or lender.

``` r
library(morar)

# Income needed for a R$ 500k property at 11% a.a., SAC, 80% LTV
min_income(500000, i = 0.11)
#> [1] 15349.83

# Most expensive property each income can finance
max_price(c(5000, 10000, 20000), i = 0.11)
#> [1] 162868.3 325736.5 651473.1
```

Pass `cash` to cap the price at what the available down payment
supports. The lower of the income and cash limits binds.
`loan_summary()` reports the required down payment for a property. The
chosen `ltv` stays fixed; extra cash does not reduce the modeled loan.

``` r
max_price(10000, i = 0.11, ltv = 0.8)
#> [1] 325736.5
max_price(10000, i = 0.11, ltv = 0.8, cash = 50000)
#> [1] 250000
loan_summary(250000, i = 0.11, ltv = 0.8)$down_payment
#> [1] 50000
```

The first payment includes insurance and fees. MIP depends on the
borrower’s age, so charges default to zero; pass your own rates to
include them.

``` r
min_income(
  500000,
  i = 0.11,
  mip_rate = 0.0003,
  dfi_rate = 0.00008,
  admin_fee = 25
)
#> [1] 15966.5
```

`loan_summary()` returns every component at once:

``` r
loan_summary(c(300000, 500000, 800000), i = 0.11, method = "price")
#> # A tibble: 3 × 9
#>    price down_payment   loan charges first_payment min_income rate_monthly  term
#>    <dbl>        <dbl>  <dbl>   <dbl>         <dbl>      <dbl>        <dbl> <dbl>
#> 1 300000        90000 210000       0         1918.      6394.      0.00873   360
#> 2 500000       150000 350000       0         3197.     10656.      0.00873   360
#> 3 800000       240000 560000       0         5115.     17049.      0.00873   360
#> # ℹ 1 more variable: method <chr>
```

## Amortization schedules

`amortization_table()` builds the month-by-month schedule for one loan.

``` r
amortization_table(500000, i = 0.11, n = 420, admin_fee = 25)
#> SAC schedule: R$ 400.000,00 over 420 months at 11% a.a. (effective)
#> First payment R$ 4.471,22 | last R$ 985,70 | total interest R$ 735.452,80
#> # A tibble: 420 × 8
#>    period balance_start interest principal payment charges total_payment
#>     <int>         <dbl>    <dbl>     <dbl>   <dbl>   <dbl>         <dbl>
#>  1      1       400000     3494.      952.   4446.      25         4471.
#>  2      2       399048.    3486.      952.   4438.      25         4463.
#>  3      3       398095.    3477.      952.   4430.      25         4455.
#>  4      4       397143.    3469.      952.   4421.      25         4446.
#>  5      5       396190.    3461.      952.   4413.      25         4438.
#>  6      6       395238.    3452.      952.   4405.      25         4430.
#>  7      7       394286.    3444.      952.   4396.      25         4421.
#>  8      8       393333.    3436.      952.   4388.      25         4413.
#>  9      9       392381.    3427.      952.   4380.      25         4405.
#> 10     10       391429.    3419.      952.   4371.      25         4396.
#> # ℹ 410 more rows
#> # ℹ 1 more variable: balance_end <dbl>
```

## Affordability indices

The Housing Affordability Index (HAI) compares a household’s income with
the qualifying income: 100 means it qualifies exactly. The
price-to-income ratio (PIR) divides the price by annual income and maps
to the Demographia categories.

``` r
calc_hai(price = 400000, income = 8000, i = c(0.08, 0.11, 0.14))
#> [1] 81.41724 65.14731 54.51917

pir <- calc_pir(price = 500000, income = 10000, annual_factor = 40 / 3)
pir
#> [1] 3.75
pir_category(pir)
#> [1] Moderately unaffordable
#> 4 Levels: Affordable < Moderately unaffordable < ... < Severely unaffordable
```

## Conventions

- Rates are decimals: `0.11` means 11% a.a. The schedule uses a fixed
  monthly interest rate. It does not model TR or IPCA adjustment of the
  outstanding balance.
- `rate_type = "effective"` (default) compounds the annual rate;
  `"nominal"` divides it by 12.
- Affordability uses the first payment, the highest one under SAC. This
  is an estimate of qualifying income, not a bank approval decision.
- `ltv = NULL` uses `default_ltv(method)`: 80% for SAC, 70% for Price
  and mixed, under the dated Caixa SBPE convention above.
- `cash` covers the down payment only. Purchase taxes, registration and
  other upfront costs are outside the calculation.
- `calc_pir()` annualizes income with `annual_factor = 12`; use `40 / 3`
  to include the 13th salary and the vacation bonus.

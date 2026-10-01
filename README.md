# morar

Housing affordability analysis for Brazil. `morar` implements Brazilian
mortgage conventions (SAC, Price and mixed amortization, effective or nominal
rates) and the inversion between income and maximum financeable price. Every
core function is vectorized, so it works directly on income or price
distributions inside `dplyr::mutate()`.

## Installation

```r
# install.packages("pak")
pak::pak("viniciusoike/morar")
```

## Usage

```r
library(morar)

# Minimum income to finance a R$500k property at 10% a.a. (SAC, 70% LTV)
renda_minima(500000, i = 0.10)
#> [1] 12543.9

# Maximum price over an income distribution
inc_to_price(c(3000, 6000, 12000), i = 0.10)
#> [1] 119580 239160 478320

# All components at once
loan_summary(c(300000, 500000), i = 0.10)

# Full schedule
amortization_table(500000, i = 0.10, method = "price")

# Affordability indices
calc_hai(p = 400000, renda = 8000, i = c(0.08, 0.10, 0.12))
pir_category(calc_pir(p = 500000, renda = 10000))
```

## Functions

| Function | Returns |
|---|---|
| `renda_minima()` | Minimum monthly income to finance a price |
| `inc_to_price()` | Maximum price for a monthly income |
| `emprestimo_max()` | Maximum loan (financed amount) for an income |
| `parcela_minima()` | First (binding) monthly payment |
| `loan_summary()` | Tibble with all components per property |
| `amortization_table()` | Month-by-month schedule (`morar_schedule`) |
| `calc_hai()` | Housing Affordability Index (100 = exactly qualifies) |
| `calc_pir()`, `pir_category()` | Price-to-income ratio and Demographia class |
| `payment_factor()`, `annual_to_monthly()` | Building blocks |

## Conventions

- Rates are always decimals (`0.10`, not `10`); percentage inputs error.
- `rate_type = "effective"` (default) compounds the annual rate;
  `"nominal"` divides by 12.
- Affordability is evaluated at the first payment, the highest under SAC.
- Defaults: LTV 70%, 360 months, 30% income commitment.

## License

MIT

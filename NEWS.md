# morar (development version)

* Added an optional down-payment cash limit to `max_price()` and `max_loan()`.
* Clarified that affordability estimates use a fixed-rate, first-payment
  model and do not determine bank approval or model balance indexation.
* Rejected non-finite numeric inputs and missing scalar schedule inputs with
  explicit errors.

# morar 0.2.0

* Renamed `renda_minima()`, `inc_to_price()`, `emprestimo_max()` and
  `parcela_minima()` to `min_income()`, `max_price()`, `max_loan()` and
  `first_payment()`.
* Renamed arguments and output columns to English: `p` to `price`, `renda`
  to `income`, `comp` to `pti`, and `method = "misto"` to `"mixed"`.
* Added `mip_rate`, `dfi_rate` and `admin_fee` to every affordability
  function. `amortization_table()` gains `charges` and `total_payment`
  columns.
* Added `default_ltv()`. `ltv` now defaults to the Caixa SBPE limit for the
  chosen method.
* Added `annual_factor` to `calc_pir()`.
* Fixed `min_income()` and `calc_hai()` silently recycling `comp` with base
  R rules.
* Fixed `loan_summary()` accepting `comp` outside (0, 1].
* Fixed `n = Inf` passing validation.
* A bare `NA` rate now propagates instead of erroring.
* Rates above 100% a.a. are no longer rejected.

# morar 0.1.0

* First version, ported from an affordability project.

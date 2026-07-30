nzs_source_model()

testthat::test_that("LTFM projection input derives model age and period", {
  projections <- data.frame(
    year = c(2026, 2027, 2028),
    ltfm_cpi_growth_projection = c(0, 0.02, 0.02),
    ltfm_wage_growth_projection = c(0, 0.03, 0.03)
  )

  path <- nzs_economic_path_from_ltfm(projections)

  testthat::expect_equal(path$age, c(65, 66, 67))
  testthat::expect_equal(path$period, c(0, 1, 2))
})

testthat::test_that("rate path uses LTFM projection columns", {
  projections <- data.frame(
    year = c(2026, 2027),
    ltfm_cpi_growth_projection = c(0, 0.01),
    ltfm_wage_growth_projection = c(0, 0.03)
  )
  path <- nzs_rate_path(nzs_economic_path_from_ltfm(projections))

  testthat::expect_equal(
    path$net_aotwe_weekly[1],
    nzs_base_net_aotwe_weekly(nzs_default_inputs())
  )
  testthat::expect_equal(path$net_aotwe_weekly[2], path$net_aotwe_weekly[1] * 1.03)
  testthat::expect_equal(path$couple_cpi[2], path$couple_cpi[1] * 1.01)
  testthat::expect_equal(path$couple_wage[2], nzs_default_inputs()$nzc_floor * path$net_aotwe_weekly[2])
})

testthat::test_that("base net AOTWE follows the QES, standard-tax and ACC construction", {
  defaults <- nzs_default_inputs()
  path <- nzs_rate_path()

  testthat::expect_equal(path$net_aotwe_weekly[1], 1291.52)
  testthat::expect_equal(defaults$nzc_floor * path$net_aotwe_weekly[1], 852.4032)
  testthat::expect_equal(path$couple_current_law[1], defaults$current_couple_net_weekly)
  testthat::expect_equal(path$couple_wage[1], 852.4032)
  testthat::expect_gt(path$couple_current_law[1], path$couple_wage[1])
})

testthat::test_that("current-formula rates remain within the statutory corridor", {
  defaults <- nzs_default_inputs()
  path <- nzs_rate_path()

  testthat::expect_true(all(path$couple_current_law >= defaults$nzc_floor * path$net_aotwe_weekly))
  testthat::expect_true(all(path$couple_current_law <= defaults$nzc_ceiling * path$net_aotwe_weekly))
})

testthat::test_that("policy regime letters map to user-facing definitions", {
  path <- nzs_rate_path()

  testthat::expect_equal(nzs_selected_regime(path, "A"), path$couple_current_law)
  testthat::expect_equal(nzs_selected_regime(path, "B"), path$couple_cpi)
  testthat::expect_equal(nzs_selected_regime(path, "C"), path$couple_wage)
})

testthat::test_that("unknown indexation regime fails clearly", {
  path <- nzs_rate_path()

  testthat::expect_error(
    nzs_selected_regime(path, "bad"),
    "Unknown indexation regime"
  )
})

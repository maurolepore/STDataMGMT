test_that("the strucutre of the datasets remains the same", {
  expect_snapshot(names(scenarios_geographies))

  expect_snapshot(lapply(scenarios_geographies, typeof))

  expect_snapshot(names(production_types))

  expect_snapshot(lapply(production_types, typeof))

  expect_snapshot(names(synthetic_company_activities))

  expect_snapshot(lapply(synthetic_company_activities, typeof))

  expect_snapshot(names(synthetic_company_emissions))

  expect_snapshot(lapply(synthetic_company_emissions, typeof))

  expect_snapshot(names(synthetic_eikon_data))

  expect_snapshot(lapply(synthetic_eikon_data, typeof))
})

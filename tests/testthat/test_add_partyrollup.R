test_that("add_partyrollup maps all 7 levels from character input", {
  out <- add_partyrollup(new_party_responses())

  expect_equal(as.integer(out$partyid_7), c(1L, 2L, 7L, 6L, 3L, 4L, 5L, NA))
  expect_snapshot(out$partyid_7)
})


test_that("add_partyrollup reads value labels from haven_labelled input", {
  df <- tibble::tibble(
    pid = haven::labelled(
      c(1, 1, 2, 2, 3, 3),
      c("Republican" = 1, "Democrat" = 2, "Independent" = 3)
    ),
    strength = haven::labelled(
      c(1, 2, 1, 2, NA, NA),
      c("Strong" = 1, "Not so strong" = 2)
    ),
    lean = haven::labelled(
      c(NA, NA, NA, NA, 1, 2),
      c("The Republican Party" = 1, "The Democratic Party" = 2)
    )
  )

  out <- add_partyrollup(
    df,
    party_id = pid,
    party_lean = lean,
    party_strength = strength
  )

  expect_equal(as.integer(out$partyid_7), c(1L, 2L, 7L, 6L, 3L, 5L))
  expect_snapshot(out$partyid_7)
})


test_that("add_partyrollup reads factor input", {
  df <- tibble::tibble(
    party_id = factor(c("Democrat", "Independent")),
    party_strength = factor(c("Strong", NA)),
    party_lean = factor(c(NA, "Lean Democrat"))
  )

  out <- add_partyrollup(df)
  expect_equal(as.integer(out$partyid_7), c(7L, 5L))
})


test_that("add_partyrollup output is haven_labelled with the expected labels", {
  out <- add_partyrollup(new_party_responses())

  expect_s3_class(out$partyid_7, "haven_labelled")
  expect_snapshot(haven::labelled(attr(out$partyid_7, "labels")))
})


test_that("add_partyrollup auto-detects variables named differently", {
  df <- tibble::tibble(
    pid3 = c("Republican", "Independent"),
    party_strength_w1 = c("Strong", NA),
    leaning = c(NA, "Lean Democrat")
  )

  out <- add_partyrollup(df)
  expect_equal(as.integer(out$partyid_7), c(1L, 5L))
})


test_that("add_partyrollup honors a custom new_name", {
  out <- add_partyrollup(new_party_responses(), new_name = pid7)
  expect_named(
    out,
    c("party_id", "party_strength", "party_lean", "pid7")
  )
})


test_that("add_partyrollup recognizes 'Not very strong' phrasing", {
  df <- tibble::tibble(
    party_id = c("Republican", "Democrat"),
    party_strength = c("Not very strong", "Not very strong"),
    party_lean = c(NA, NA)
  )

  out <- add_partyrollup(df)
  expect_equal(as.integer(out$partyid_7), c(2L, 6L))
})


test_that("add_partyrollup returns NA when the party id is missing", {
  df <- tibble::tibble(
    party_id = c(NA, "Independent"),
    party_strength = c(NA, NA),
    party_lean = c(NA, NA)
  )

  out <- add_partyrollup(df)
  expect_equal(as.integer(out$partyid_7), c(NA_integer_, NA_integer_))
})


test_that("add_partyrollup allows a partisan leaning toward their own party", {
  df <- tibble::tibble(
    party_id = c("Republican", "Democrat"),
    party_strength = c("Strong", "Not so strong"),
    party_lean = c("Lean Republican", "Lean Democrat")
  )

  out <- add_partyrollup(df)
  expect_equal(as.integer(out$partyid_7), c(1L, 6L))
})


test_that("add_partyrollup errors when party id and lean name different parties", {
  df <- tibble::tibble(
    party_id = c("Republican", "Democrat", "Democrat"),
    party_strength = c("Strong", "Strong", "Not so strong"),
    party_lean = c("Lean Democrat", NA, "Lean Republican")
  )

  expect_snapshot(error = TRUE, add_partyrollup(df))
})


test_that("add_partyrollup errors when a variable cannot be found", {
  df <- tibble::tibble(
    party_id = "Republican",
    party_strength = "Strong"
  )

  expect_snapshot(error = TRUE, add_partyrollup(df))
})


test_that("add_partyrollup errors when multiple variables match", {
  df <- tibble::tibble(
    party_id = "Republican",
    party_affiliation = "x",
    party_strength = "Strong",
    party_lean = "Neither"
  )

  expect_snapshot(error = TRUE, add_partyrollup(df))
})


test_that("add_partyrollup errors with hints when a supplied column is missing", {
  df <- tibble::tibble(
    party_id = "Republican",
    party_strength = "Strong",
    party_lean = "Neither"
  )

  expect_snapshot(error = TRUE, add_partyrollup(df, party_id = party_idd))
})

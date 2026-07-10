# Reusable data covering every branch of the 7-point scale plus a missing case.
# Lives in helper.R so it is sourced before every test and is available when a
# single test_that() block is run in isolation.
new_party_responses <- function() {
  tibble::tibble(
    party_id = c(
      "Republican",
      "Republican",
      "Democrat",
      "Democrat",
      "Independent",
      "Independent",
      "Something else",
      NA
    ),
    party_strength = c(
      "Strong",
      "Not so strong",
      "Strong",
      "Not so strong",
      NA,
      NA,
      NA,
      NA
    ),
    party_lean = c(
      NA,
      NA,
      NA,
      NA,
      "Lean Republican",
      "Neither",
      "Lean Democrat",
      NA
    )
  )
}

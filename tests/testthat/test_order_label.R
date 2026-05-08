# Errors and Warnings -----------------------------------------------------

test_that("Errors and Warnings on grouped data", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .15, .15, .2, .4)
  )

  expect_error(
    frequencies |> order_label(inherent_order_group = TRUE),
    'You specified a grouping argument but not the group_var. Either add in a variable for group_var or do not run other grouping arguments.'
  )
  expect_error(
    frequencies |> order_label(rev_group = TRUE),
    'You specified a grouping argument but not the group_var. Either add in a variable for group_var or do not run other grouping arguments.'
  )
  expect_error(
    frequencies |> order_label(group_first = 's_test'),
    'You specified a grouping argument but not the group_var. Either add in a variable for group_var or do not run other grouping arguments.'
  )
  expect_error(
    frequencies |> order_label(group_last = 's_test'),
    'You specified a grouping argument but not the group_var. Either add in a variable for group_var or do not run other grouping arguments.'
  )
  expect_warning(
    frequencies |> order_label(stacked = 'ms'),
    'You used a "stacked" ordering system without specifying group_var. Is your data grouped?'
  )
})


test_that("Errors and Warnings on grouped data", {
  expect_error(
    tibble::tibble(
      variable = rep('s_test', 5),
      value = c('1', '2', '3', '4', '5'),
      label = c('One', 'Two', 'Three', 'Four', 'Five'),
      n = c(10, 15, 15, 20, 40),
      stat = rep('percent', 5),
      result = c(.1, .3, .4, .05, .01)
    ) |>
      order_label(topbox = 2),
    'You cannot use the topbox argument on ungrouped data.'
  )
})


test_that("Error num_fmt not percent/general", {
  df_stat <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(1, 2, 3, 4, 5),
    n = rep(100, 5)
  )

  expect_error(
    df_stat |> order_label(num_fmt = "gnral")
  )
})


test_that("Error stacked not NULL, ms, gg", {
  df_stat <- tibble::tibble(
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    result = c(1, 2, 3, 4, 5),
    n = rep(100, 5)
  )

  expect_error(
    df_stat |> order_label(stacked = "mschart")
  )
})


test_that("Grouped, stacked gg, inherent_order_group, rev_group", {
  expect_error(
    tibble::tibble(
      group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
      variable = rep('s_test', 9),
      value = rep(c('1', '2', '3'), 3),
      label = rep(c('One', 'Two', 'Three'), 3),
      n = rep(10, 9),
      stat = rep('percent', 9),
      result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
    ) |>
      order_label(
        group_var = group_var,
        stacked = 'gg',
        inherent_order_group = TRUE,
        topbox = 2
      ),
    'You cannot use the topbox argument when inherent_order_group is TRUE.'
  )
})


# Overall -----------------------------------------------------------------

test_that("Output structure", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .15, .15, .2, .4)
  ) |>
    order_label(
      label_var = value
    )

  expect_equal(
    class(frequencies)[1],
    c('tbl_df')
  )
  expect_equal(
    class(frequencies$label),
    c('factor')
  )
  expect_equal(
    class(frequencies$value),
    c('numeric')
  )
})


test_that("Adds values", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('One', 'Two', 'Three', 'Four', 'Five'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .2, .3, .4, .5)
  ) |>
    order_label(
      label_var = value
    )
  expect_equal(
    frequencies |> dplyr::pull(value),
    c(5, 4, 3, 2, 1)
  )
})


test_that("label_var", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .15, .15, .2, .4)
  ) |>
    order_label(
      label_var = value
    )
  expect_equal(
    names(frequencies),
    c('variable', 'value', 'label', 'n', 'stat', 'result', 'percent_label')
  )
})


# Ungrouped Data - one argument only -------------------------------------------

# num_fmt & percent_all in single but not multiple arguments because...
# they don't have to do with ordering, just formatting

# NO ARGUMENTS *******************************************************
test_that("Ungrouped, no arguments", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label()

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'Two', 'One', 'Four', 'Five')
  )
  expect_equal(
    frequencies |> dplyr::pull(label) |> levels(),
    c('Three', 'Two', 'One', 'Four', 'Five')
  )
})


# ONE ARGUMENT *******************************************************
test_that("Ungrouped, inherent_order_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(inherent_order_label = TRUE)

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Two', 'Three', 'Four', 'Five')
  )
  expect_equal(
    frequencies |> dplyr::pull(label) |> levels(),
    c('One', 'Two', 'Three', 'Four', 'Five')
  )
})

test_that("Ungrouped, label_first", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(label_first = 'Two')

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'Three', 'One', 'Four', 'Five')
  )
})


test_that("Ungrouped, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(rev_label = TRUE)

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Five', 'Four', 'One', 'Two', 'Three')
  )
})


test_that("Ungrouped, label_last", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(label_last = 'Two')

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'One', 'Four', 'Five', 'Two')
  )
})


test_that("Ungrouped, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(direction = 'horizontal')

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Five', 'Four', 'One', 'Two', 'Three')
  )
})


test_that("Ungrouped, stacked gg", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  )

  expect_snapshot(
    frequencies |>
      order_label(stacked = 'gg') |>
      dplyr::pull(label) |>
      as.character()
  )
})


test_that("Ungrouped, stacked ms", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  )

  expect_snapshot(
    frequencies |>
      order_label(stacked = 'ms') |>
      dplyr::pull(label) |>
      as.character()
  )
})


test_that("Ungrouped, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .31, .9)
  )

  freqs_other_true <- frequencies |>
    order_label(none_other = TRUE)
  expect_equal(
    freqs_other_true |> dplyr::pull(label) |> as.character(),
    c('Three', 'Two', 'One', 'Other', 'None of the above')
  )

  freqs_other_false <- frequencies |>
    order_label(none_other = FALSE)
  expect_equal(
    freqs_other_false |> dplyr::pull(label) |> as.character(),
    c('None of the above', 'Three', 'Other', 'Two', 'One')
  )
})


test_that("Ungrouped, num_fmt", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .31, .9)
  )

  freqs_numfmt_true <- frequencies |>
    order_label(num_fmt = 'percent')
  expect_equal(
    freqs_numfmt_true |> dplyr::pull(percent_label),
    c('90%', '40', '31', '30', '10')
  )

  freqs_numfmt_false <- frequencies |>
    order_label(num_fmt = 'general')
  expect_equal(
    freqs_numfmt_false |> dplyr::pull(percent_label),
    c('0.9', '0.4', '0.31', '0.3', '0.1')
  )
})

test_that("Ungrouped, percent_all", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .31, .9)
  ) 

  expect_snapshot(
    frequencies |> order_label(percent_all = TRUE)
  )

})


# Ungrouped Data - descending, 2 arguments -------------------------------

# TWO ARGUMENTS *******************************************************
test_that("Ungrouped, label_first, label_last", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Four'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'Three', 'One', 'Five', 'Four')
  )
})


test_that("Ungrouped, label_first, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'Five', 'Four', 'One', 'Three')
  )
})

test_that("Ungrouped, label_first, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Five', 'Four', 'One', 'Three', 'Two')
  )
  expect_equal(
    frequencies$percent_label[5],
    c('30%')
  )
})


test_that("Ungrouped, label_first, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'None of the above', 'Three', 'Other', 'One')
  )
})


test_that("Ungrouped, label_last, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_last = 'Two',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Five', 'Four', 'One', 'Three', 'Two')
  )
})


test_that("Ungrouped, label_last, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_last = 'Two',
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'Five', 'Four', 'One', 'Three')
  )
})


test_that("Ungrouped, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'Two', 'One', 'Four', 'Five')
  )
})


test_that("Ungrouped, rev_label, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      rev_label = TRUE,
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Other', 'Two', 'Three', 'None of the above')
  )
})


test_that("Ungrouped, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Other', 'Two', 'Three', 'None of the above')
  )
})


# Ungrouped Data - descending, 3 arguments -------------------------------

# THREE ARGUMENTS **********************************************
test_that("Ungrouped, label_first, label_last, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Four',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'Five', 'One', 'Three', 'Four')
  )
})


test_that("Ungrouped, label_first, label_last, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Four',
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Four', 'Five', 'One', 'Three', 'Two')
  )
})


test_that("Ungrouped, label_first, label_last, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Other',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'None of the above', 'Three', 'One', 'Other')
  )
})


test_that("Ungrouped, label_first, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'One', 'Four', 'Five', 'Two')
  )
})


test_that("Ungrouped, label_first, rev_label, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      rev_label = TRUE,
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'One', 'Other', 'Three', 'None of the above')
  )
})


test_that("Ungrouped, label_first, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Other', 'Three', 'None of the above', 'Two')
  )
})


test_that("Ungrouped, label_last, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_last = 'Two',
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'Three', 'One', 'Four', 'Five')
  )
})


test_that("Ungrouped, label_last, rev_label, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_last = 'Two',
      rev_label = TRUE,
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Other', 'Three', 'None of the above', 'Two')
  )
})


test_that("Ungrouped, label_last, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_last = 'Two',
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'One', 'Other', 'Three', 'None of the above')
  )
})


test_that("Ungrouped, rev_label, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      rev_label = TRUE,
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('None of the above', 'Three', 'Two', 'Other', 'One')
  )
})


# Ungrouped Data - descending, 4-5 arguments -------------------------------

# FOUR ARGUMENTS **********************************************
test_that("Ungrouped, label_first, label_last, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Four',
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Four', 'Three', 'One', 'Five', 'Two')
  )
})


test_that("Ungrouped, label_first, label_last, rev_label, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Other',
      rev_label = TRUE,
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'One', 'Three', 'None of the above', 'Other')
  )
})


test_that("Ungrouped, label_first, label_last, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Other',
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    rev(c('Two', 'None of the above', 'Three', 'One', 'Other'))
  )
})


test_that("Ungrouped, label_first, rev_label, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      rev_label = TRUE,
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('None of the above', 'Three', 'Other', 'One', 'Two')
  )
})


test_that("Ungrouped, label_last, rev_label, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_last = 'Two',
      rev_label = TRUE,
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Two', 'None of the above', 'Three', 'Other', 'One')
  )
})


# FIVE ARGUMENTS **********************************************
test_that("Ungrouped, label_first, label_last, rev_label, horizontal, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Other', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .29, .9)
  ) |>
    order_label(
      label_first = 'Two',
      label_last = 'Other',
      rev_label = TRUE,
      direction = 'horizontal',
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Other', 'None of the above', 'Three', 'One', 'Two')
  )
})


# Ungrouped Data - inherent_order, 2 arguments -------------------------

# Will not be including none_other in inherent_order sections after this
# because none_other options should come at the end anyway

# TWO ARGUMENTS *************************************************
test_that("Ungrouped, inherent_order_label, label_first", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'One', 'Two', 'Four', 'Five')
  )
})


test_that("Ungrouped, inherent_order_label, label_last", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_last = 'Three'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Two', 'Four', 'Five', 'Three')
  )
})


test_that("Ungrouped, inherent_order_label, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    rev(c('One', 'Two', 'Three', 'Four', 'Five'))
  )
})


test_that("Ungrouped, inherent_order_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    rev(c('One', 'Two', 'Three', 'Four', 'Five'))
  )
})


test_that("Ungrouped, inherent_order_label, none_other", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'None of the above'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .5)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      none_other = FALSE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Two', 'Three', 'Four', 'None of the above')
  )
})


# Ungrouped Data - inherent_order, 3 arguments -------------------------

# THREE ARGUMENTS *************************************************
test_that("Ungrouped, inherent_order_label, label_first, label_last", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      label_last = 'Four'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'One', 'Two', 'Five', 'Four')
  )
})


test_that("Ungrouped, inherent_order_label, label_first, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'Five', 'Four', 'Two', 'One')
  )
})


test_that("Ungrouped, inherent_order_label, label_first, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    rev(c('Three', 'One', 'Two', 'Four', 'Five'))
  )
})


test_that("Ungrouped, inherent_order_label, label_last, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_last = 'Three',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Five', 'Four', 'Two', 'One', 'Three')
  )
})


test_that("Ungrouped, inherent_order_label, label_last, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_last = 'Three',
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'Five', 'Four', 'Two', 'One')
  )
})


test_that("Ungrouped, inherent_order_label, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    rev(c('Five', 'Four', 'Three', 'Two', 'One'))
  )
})


# Ungrouped Data - inherent_order, 4-5 arguments -------------------------

# FOUR ARGUMENTS *************************************************
test_that("Ungrouped, inherent_order_label, label_first, label_last, rev_label", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      label_last = 'Four',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Three', 'Five', 'Two', 'One', 'Four')
  )
})


test_that("Ungrouped, inherent_order_label, label_first, label_last, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      label_last = 'Four',
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Four', 'Five', 'Two', 'One', 'Three')
  )
})


test_that("Ungrouped, inherent_order_label, label_last, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('One', 'Two', 'Four', 'Five', 'Three')
  )
})


# FIVE ARGUMENTS *************************************************
test_that("Ungrouped, inherent_order_label, label_first, label_last, rev_label, horizontal", {
  frequencies <- tibble::tibble(
    variable = rep('s_test', 5),
    value = c('1', '2', '3', '4', '5'),
    label = c('One', 'Two', 'Three', 'Four', 'Five'),
    n = c(10, 15, 15, 20, 40),
    stat = rep('percent', 5),
    result = c(.1, .3, .4, .05, .01)
  ) |>
    order_label(
      inherent_order_label = TRUE,
      label_first = 'Three',
      label_last = 'Four',
      rev_label = TRUE,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character(),
    c('Four', 'One', 'Two', 'Five', 'Three')
  )
})


# Grouped Data - one argument only ----------------------------------------

# NO ARGUMENTS *******************************************************
test_that("Grouped, no other arguments", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Two', 'One', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


# ONE ARGUMENT *******************************************************
test_that("Grouped, inherent_order_label", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      inherent_order_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('One', 'Two', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


test_that("Grouped, inherent_order_group", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      inherent_order_group = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Three', 'Two', 'One')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, label_first", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      label_first = 'Three'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Three', 'Two', 'One')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, label_last", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      label_last = 'Three'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Two', 'One', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


test_that("Grouped, group_first", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      group_first = 'Group 3'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Two', 'One', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


test_that("Grouped, group_last", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      group_last = 'Group 3'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Two', 'One', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, rev_label", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Three', 'One', 'Two')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      rev_group = TRUE
    )

  # Reverses groups as expected, but then do labels order after first group?
  # Or do labels order after the originally first group, which is now last?
  # Or do they order after overall highest numbers, no matter which group
  # expect_equal(
  #   frequencies |> dplyr::pull(label) |> as.character() |> unique(),
  #   c('Three', 'One', 'Two')
  # )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 2', 'Group 1', 'Group 3')
  )
})


test_that("Grouped, horizontal", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      direction = 'horizontal'
    )

  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Three', 'One', 'Two')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 2', 'Group 1', 'Group 3')
  )
})


test_that("Grouped, none_other", {
  # none_other TRUE
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Other', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      none_other = TRUE
    )
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('One', 'Three', 'Other')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )

  # none_other FALSE
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Other', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      none_other = FALSE
    )
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('Other', 'One', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


# Grouped Data - descending, 2 arguments ---------------------------------------

# TWO ARGUMENTS *******************************************************

# Stacked data, gg - one argument only ----------------------------------------

# ONE ARGUMENT *********************************************************
# For stacked, arguments to test are:
# inherent_order_group, group_first, group_last, rev_group, topbox
test_that("Grouped, stacked gg", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg'
    )

  # Stacked gg is like horizontal in that bottom of table is first on chart
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    rev(c('One', 'Two', 'Three'))
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 2', 'Group 1', 'Group 3')
  )
})


test_that("Grouped, stacked gg, inherent_order_group", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE
    )

  # Stacked gg is like horizontal in that bottom of table is first on chart
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    rev(c('One', 'Two', 'Three'))
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 2', 'Group 1')
  )
})


test_that("Grouped, stacked gg, group_first", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_first = 'Group 1'
    )

  # Stacked gg is like horizontal in that bottom of table is first on chart
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    rev(c('One', 'Two', 'Three'))
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 2', 'Group 3', 'Group 1')
  )
})


test_that("Grouped, stacked gg, group_last", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_last = 'Group 1'
    )

  # Stacked gg is like horizontal in that bottom of table is first on chart
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    rev(c('One', 'Two', 'Three'))
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, stacked gg, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      rev_group = TRUE
    )

  # Stacked gg is like horizontal in that bottom of table is first on chart
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    rev(c('One', 'Two', 'Three'))
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


test_that("Grouped, stacked gg, topbox", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .4, .4, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      topbox = 2
    )

  # Stacked gg is like horizontal in that bottom of table is first on chart
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    rev(c('One', 'Two', 'Three'))
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


# Stacked data, gg - 2 arguments ----------------------------------------------

# TWO ARGUMENTS *********************************************************
# For stacked, arguments to test are:
# inherent_order_group, group_first, group_last, rev_group, topbox

test_that("Grouped, stacked gg, inherent_order_group, group_first", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE,
      group_first = 'Group 2'
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 1', 'Group 2')
  )
})


test_that("Grouped, stacked gg, inherent_order_group, group_last", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE,
      group_last = 'Group 1'
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 3', 'Group 2')
  )
})


test_that("Grouped, stacked gg, inherent_order_group, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE,
      rev_group = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, stacked gg, group_first, group_last", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_first = 'Group 1',
      group_last = 'Group 3'
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 2', 'Group 1')
  )
})


test_that("Grouped, stacked gg, group_first, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_first = 'Group 1',
      rev_group = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 3', 'Group 2', 'Group 1')
  )
})


test_that("Grouped, stacked gg, group_first, topbox", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_first = 'Group 1',
      topbox = 2
    )

  # Come back to this
  # expect_equal(
  #   frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
  #   c('Group 3', 'Group 2', 'Group 1')
  # )
})


test_that("Grouped, stacked gg, group_last, rev_label", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_last = 'Group 1',
      rev_label = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, stacked gg, group_last, topbox", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_last = 'Group 2',
      topbox = 2
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 2', 'Group 1', 'Group 3')
  )
})


test_that("Grouped, stacked gg, rev_group, topbox", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      rev_group = TRUE,
      topbox = 2
    )

  # expect_equal(
  #   frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
  #   c('Group 2', 'Group 1', 'Group 3')
  # )
})


# Stacked data, gg - 3-4 arguments ----------------------------------------------

# THREE ARGUMENTS *********************************************************
# For stacked, arguments to test are:
# inherent_order_group, group_first, group_last, rev_group

test_that("Grouped, stacked gg, inherent_order_group, group_first, group_last", {
  frequencies <- tibble::tibble(
    group_var = c(
      rep('Group 1', 3),
      rep('Group 2', 3),
      rep('Group 3', 3),
      rep('Group 4', 3)
    ),
    variable = rep('s_test', 12),
    value = rep(c('1', '2', '3'), 4),
    label = rep(c('One', 'Two', 'Three'), 4),
    n = rep(10, 12),
    stat = rep('percent', 12),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33, .4, .4, .2)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE,
      group_first = 'Group 3',
      group_last = 'Group 1'
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 4', 'Group 2', 'Group 3')
  )
})


test_that("Grouped, stacked gg, inherent_order_group, group_first, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(
      rep('Group 1', 3),
      rep('Group 2', 3),
      rep('Group 3', 3),
      rep('Group 4', 3)
    ),
    variable = rep('s_test', 12),
    value = rep(c('1', '2', '3'), 4),
    label = rep(c('One', 'Two', 'Three'), 4),
    n = rep(10, 12),
    stat = rep('percent', 12),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33, .4, .4, .2)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE,
      group_first = 'Group 3',
      rev_group = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 4', 'Group 3')
  )
})


test_that("Grouped, stacked gg, group_first, group_last, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(
      rep('Group 1', 3),
      rep('Group 2', 3),
      rep('Group 3', 3),
      rep('Group 4', 3)
    ),
    variable = rep('s_test', 12),
    value = rep(c('1', '2', '3'), 4),
    label = rep(c('One', 'Two', 'Three'), 4),
    n = rep(10, 12),
    stat = rep('percent', 12),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33, .4, .4, .2)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      group_first = 'Group 3',
      group_last = 'Group 1',
      rev_group = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 4', 'Group 2', 'Group 3')
  )
})


# FOUR ARGUMENTS *********************************************************
test_that("Grouped, stacked gg, inherent_order_group, group_first, group_last, rev_group", {
  frequencies <- tibble::tibble(
    group_var = c(
      rep('Group 1', 3),
      rep('Group 2', 3),
      rep('Group 3', 3),
      rep('Group 4', 3)
    ),
    variable = rep('s_test', 12),
    value = rep(c('1', '2', '3'), 4),
    label = rep(c('One', 'Two', 'Three'), 4),
    n = rep(10, 12),
    stat = rep('percent', 12),
    result = c(.25, .25, .5, .2, .5, .3, .33, .33, .33, .4, .4, .2)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'gg',
      inherent_order_group = TRUE,
      group_first = 'Group 3',
      group_last = 'Group 1',
      rev_group = TRUE
    )

  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 1', 'Group 2', 'Group 4', 'Group 3')
  )
})

# GG TESTING
# chart <- gg_grouped_y2(
#   font_family = 'sans',
#   fills = c('red', 'orange', 'yellow'),
#   direction = 'horizontal'
# )
# chart <- gg_stacked_y2(
#   font_family = 'sans',
#   fills = c('red', 'orange', 'yellow')
# )
# print(chart)
# rm(chart)

# Stacked data, ms - one argument --------------------------------------------------------

test_that("Grouped, stacked ms", {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  ) |>
    order_label(
      group_var = group_var,
      stacked = 'ms'
    )

  # Stacked ms has groups reversed but labels descending
  expect_equal(
    frequencies |> dplyr::pull(label) |> as.character() |> unique(),
    c('One', 'Two', 'Three')
  )
  expect_equal(
    frequencies |> dplyr::pull(group_var) |> as.character() |> unique(),
    c('Group 2', 'Group 1', 'Group 3')
  )
})


# MS TESTING
# text_settings_stacked <- set_text_settings_y2()
# color_settings_stacked <- set_color_settings_y2('blue', 'yellow', 'red')
# chart <- ms_stacked_y2(
#   font_family = 'sans',
#   direction = 'horizontal'
# )
# print(chart, preview = TRUE)

# Testing deprication ---------
test_that('horizontal = TRUE is deprecated', {
  frequencies <- tibble::tibble(
    group_var = c(rep('Group 1', 3), rep('Group 2', 3), rep('Group 3', 3)),
    variable = rep('s_test', 9),
    value = rep(c('1', '2', '3'), 3),
    label = rep(c('One', 'Two', 'Three'), 3),
    n = rep(10, 9),
    stat = rep('percent', 9),
    result = c(.1, .2, .3, .01, .02, .03, .5, .8, .01)
  )

  expect_snapshot(
    order_label(
      frequencies,
      group_var = group_var,
      horizontal = TRUE,
      stacked = 'ms'
    )
  )
})

# percent_var / percent_filter --------------------------------------------

# Shared fixture: two countries x three brands. Italy's max result is on
# Brand B (.50, value=2). USA's max result is on Brand C (.40, value=3).
# Italy's value=1 max is Brand A (.40); USA's value=1 max is Brand A (.35).
make_pv_fixture <- function() {
  tibble::tibble(
    group_var = c(rep('Italy', 3), rep('USA', 3)),
    value     = rep(c(1, 2, 3), 2),
    label     = rep(c('Brand A', 'Brand B', 'Brand C'), 2),
    result    = c(.40, .50, .10,   .35, .25, .40)
  )
}

test_that("percent_var puts % on max-result row per group", {
  frequencies <- make_pv_fixture() |>
    order_label(
      group_var = group_var,
      percent_var = group_var
    )

  pct_rows <- frequencies |>
    dplyr::filter(stringr::str_detect(.data$percent_label, '%'))

  expect_equal(nrow(pct_rows), 2)
  expect_setequal(as.character(pct_rows$group_var), c('Italy', 'USA'))
  expect_equal(
    as.character(pct_rows$label[pct_rows$group_var == 'Italy']),
    'Brand B'
  )
  expect_equal(
    as.character(pct_rows$label[pct_rows$group_var == 'USA']),
    'Brand C'
  )
})


test_that("percent_var with inherent_order_label picks the label where value = 1, ties broken by max result", {
  frequencies <- make_pv_fixture() |>
    order_label(
      group_var = group_var,
      percent_var = group_var,
      inherent_order_label = TRUE
    )

  pct_rows <- frequencies |>
    dplyr::filter(stringr::str_detect(.data$percent_label, '%'))

  # Both countries' value=1 row (Brand A) gets the %, since each country has
  # exactly one row at value=1 here.
  expect_equal(nrow(pct_rows), 2)
  expect_true(all(pct_rows$value == 1))
  expect_true(all(as.character(pct_rows$label) == 'Brand A'))
})


test_that('percent_var = "All" puts a % on every row', {
  frequencies <- make_pv_fixture() |>
    order_label(
      group_var = group_var,
      percent_var = "All"
    )

  expect_true(all(stringr::str_detect(frequencies$percent_label, '%')))
})


test_that("percent_all = TRUE is deprecated and maps to percent_var = 'All'", {
  expect_snapshot({
    frequencies <- make_pv_fixture() |>
      order_label(
        group_var = group_var,
        percent_all = TRUE
      )
  })
  # Despite the deprecation warning, behavior should match percent_var = "All".
  frequencies <- suppressWarnings(
    make_pv_fixture() |>
      order_label(
        group_var = group_var,
        percent_all = TRUE
      )
  )
  expect_true(all(stringr::str_detect(frequencies$percent_label, '%')))
})


test_that("percent_var has no effect when num_fmt = 'general'", {
  frequencies <- make_pv_fixture() |>
    dplyr::mutate(result = result * 100) |>
    order_label(
      group_var = group_var,
      percent_var = group_var,
      num_fmt = 'general'
    )

  expect_false(any(stringr::str_detect(frequencies$percent_label, '%')))
})


test_that("default behavior is unchanged when percent_var is NULL", {
  frequencies <- make_pv_fixture() |>
    order_label(group_var = group_var)

  pct_rows <- frequencies |>
    dplyr::filter(stringr::str_detect(.data$percent_label, '%'))

  # Existing behavior: only the very first row gets a %
  expect_equal(nrow(pct_rows), 1)
})


test_that("percent_filter restricts which rows are eligible for %", {
  # Add a gender layer; female rows always have larger results, but we
  # want the % on the male top per country.
  frequencies <- tibble::tibble(
    group_var  = c(rep(1L, 6), rep(2L, 6)),
    group_var2 = rep(c('USA', 'USA', 'USA', 'Italy', 'Italy', 'Italy'), 2),
    label      = rep(c('Brand A', 'Brand B', 'Brand C'), 4),
    value      = rep(c(1, 2, 3), 4),
    result     = c(.30, .20, .10, .25, .35, .15,
                   .50, .40, .20, .55, .45, .30)
  ) |>
    order_label(
      group_var = group_var,
      percent_var = group_var2,
      percent_filter = group_var == 1
    )

  pct_rows <- frequencies |>
    dplyr::filter(stringr::str_detect(.data$percent_label, '%'))

  expect_equal(nrow(pct_rows), 2)
  expect_true(all(as.character(pct_rows$group_var) == '1'))
  expect_equal(
    as.character(pct_rows$label[pct_rows$group_var2 == 'USA']),
    'Brand A'
  )
  expect_equal(
    as.character(pct_rows$label[pct_rows$group_var2 == 'Italy']),
    'Brand B'
  )
})


test_that("percent_filter with no eligible rows in a level skips that level", {
  # Italy has no rows where group_var == 1. USA does.
  frequencies <- tibble::tibble(
    group_var  = c(2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L),
    group_var2 = c('Italy', 'Italy', 'Italy', 'USA', 'USA', 'USA', 'USA', 'USA', 'USA'),
    label      = c('Brand A', 'Brand B', 'Brand C', 'Brand A', 'Brand B', 'Brand C', 'Brand A', 'Brand B', 'Brand C'),
    value      = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    result     = c(.40, .35, .25, .30, .20, .10, .50, .25, .25)
  ) |>
    order_label(
      group_var = group_var,
      percent_var = group_var2,
      percent_filter = group_var == 1
    )

  pct_rows <- frequencies |>
    dplyr::filter(stringr::str_detect(.data$percent_label, '%'))

  # Only USA gets a % (on its single male top: Brand A, .30)
  expect_equal(nrow(pct_rows), 1)
  expect_equal(as.character(pct_rows$group_var2), 'USA')
  expect_equal(as.character(pct_rows$label), 'Brand A')
})

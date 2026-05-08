# Executive function ----------------------------------------------------------
#' Order your data and add percent labels
#'
#' Takes a dataframe (frequencies) and orders the labels and groups while adding percent labels for use in ggplot
#' @param dataset The name of the data frame for the function to modify, usually piped in after running freqs
#' @param label_var DEFAULT = label; name of variable to be ordered
#' @param group_var DEFAULT = 'NULL'; Add the unquoted name of the grouping variable if your data is grouped
#' @param percent_var DEFAULT = NULL; Unquoted name of a variable in the dataset (e.g., `group_var` or `country`) that controls where the \% symbol appears. Be sure to specify "group_var = group_var" when using this argument. By default, the \% is placed on the highest result of each percent_var value. When inherent_order_label is set to TRUE, the \% sign is assigned to each unique group_var where value = 1. Useful for grouped charts where you want one \% per group rather than just one \% overall. Pass `percent_var = "All"` (the string) to put a \% next to every row \(replaces the deprecated `percent_all` argument\). Ignored when `num_fmt = "general"`.
#' @param percent_filter DEFAULT = NULL; Optional unquoted dplyr-style expression (e.g., `group_var == 1`) restricting which rows are eligible to receive the \%. For example, if your data was grouped by country and gender, with the percent_var equal to country, but you only wanted the \% sign to go with men, then you could set percent_filter to "Male". If there is no eligible row for that percent_var, then it will be skipped. The "top" row within each `percent_var` level is selected only from rows where this evaluates to TRUE. Levels with no eligible rows get no \%. Has no effect unless `percent_var` is also set.
#' @param inherent_order_label DEFAULT = FALSE; If FALSE, puts labels in descending order. If TRUE, puts labels in the inherent order from survey (e.g., Strongly agree to strongly disagree). Specifying stacked = 'gg' or 'ms' automatically makes inherent_order_label = TRUE
#' @param inherent_order_group DEFAULT = FALSE; If FALSE, puts groups in descending order. If TRUE, puts groups in the order they are factored (e.g., District 1, District 2...)
#' @param label_first DEFAULT = NA; If specified, puts the specified label first. ex: 'brand1' would put label called brand1 before all other labels
#' @param group_first DEFAULT = NA; If specified, puts the specified group first. ex: 'brand1' would put group called brand1 before all other groups
#' @param rev_label DEFAULT = FALSE; To reverse the order of labels in a chart, use rev_label = TRUE
#' @param rev_group DEFAULT = FALSE; To reverse the order of groups in a chart, use rev_group = TRUE
#' @param label_last DEFAULT = NA; If specified, puts the specified label last ex: 'brand1' would put label called brand1 after all other labels
#' @param group_last DEFAULT = NA; If specified, puts the specified group last ex: 'brand1' would put group called brand1 after all other groups
#' @param horizontal (Deprecated) Use direction instead.
#' @param direction DEFAULT = "vertical"; Use "vertical" (default) or "horizontal". Replaces the deprecated `horizontal` argument.
#' @param stacked DEFAULT = 'NULL'; For stacked barcharts, use stacked = 'gg' for ggplot and 'ms' for mschart
#' @param topbox DEFAULT = NULL; Can be set to a numeric value, ex: topbox = 2 to order by top2box instead of topbox
#' @param none_other DEFAULT = TRUE; Automatically puts "Other", "None of the above", and "Prefer not to say" options at the bottom. Change to FALSE to let them stay ordered elsewhere in the chart
#' @param num_fmt DEFAULT = "percent"; Other option is "general", use this when working with whole numbers rather than percents/proportions
#' @param percent_all (Deprecated) Use `percent_var = "All"` instead.
#' @keywords order label arrange
#' @importFrom rlang .data
#' @examples
#' # Ungrouped, put in descending order of the result
#' frequencies <- tibble::tibble(
#'   label = c('Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5'),
#'   result = c(.25, .15, .20, .10, .30),
#'   value = c(1, 2, 3, 4, 5)
#' ) |> order_label()
#'
#' # Grouped, with an inherent order for the label, or the brand
#' frequencies <- tibble::tibble(
#'   label = rep(c('Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5'), 2),
#'   result = c(.20, .20, .30, .10, .20, .20, .30, .20, .20, .10),
#'   value = rep(c(1, 2, 3, 4, 5), 2),
#'   group_var = c(rep('Group A', 5), rep('Group B', 5))
#' ) |> order_label(
#'   group_var = group_var,
#'   inherent_order_label = TRUE
#' )
#'
#' # Stacked, will be using this frequencies in an mschart later on
#' frequencies <- tibble::tibble(
#'   label = rep(c('Promoter', 'Passive', 'Detractor'), 3),
#'   result = c(.33, .33, .34, .20, .30, .50, .25, .50, .25),
#'   value = rep(c(1, 2, 3), 3),
#'   group_var = c(rep('Group A', 3), rep('Group B', 3), rep('Group C', 3))
#' ) |> order_label(
#'   group_var = group_var,
#'   stacked = 'ms'
#' )
#' @export

order_label <- function(
  dataset, # will likely be frequencies
  label_var = label,
  group_var = 'NULL',
  percent_var = NULL,
  percent_filter = NULL,
  inherent_order_label = FALSE,
  inherent_order_group = FALSE,
  label_first = NA,
  group_first = NA,
  rev_label = FALSE,
  rev_group = FALSE,
  label_last = NA,
  group_last = NA,
  horizontal = lifecycle::deprecated(),
  direction = c("vertical", "horizontal"),
  stacked = c('NULL', 'ms', 'gg'),
  topbox = NULL,
  none_other = TRUE,
  num_fmt = c("percent", "general"),
  percent_all = lifecycle::deprecated()
) {
  # Soft-deprecate `horizontal` if user supplied it, mapping to `direction`.
  if (lifecycle::is_present(horizontal)) {
    lifecycle::deprecate_warn(
      when = "0.4.3",
      what = "order_label(horizontal)",
      with = "order_label(direction = 'horizontal')"
    )
    if (!rlang::is_missing(direction)) {
      if (isTRUE(horizontal)) {
        direction <- "horizontal"
      } else {
        direction <- "vertical"
      }
    } else {
      stop('Please remove the horizontal argument')
    }
  }

  # Soft-deprecate `percent_all` if user supplied it, mapping to
  # `percent_var = "All"`.
  if (lifecycle::is_present(percent_all)) {
    lifecycle::deprecate_warn(
      when = "0.4.3",
      what = "order_label(percent_all)",
      with = 'order_label(percent_var = "All")'
    )
    if (percent_all == TRUE) {
      percent_var <- "All"
    }
  } 

  ### Test matching arguments
  num_fmt <- rlang::arg_match(num_fmt)
  stacked <- rlang::arg_match(stacked)
  direction <- rlang::arg_match(direction)

  ### Flags
  # Enquo flags
  label <- NULL
  label_var_flag <- dplyr::enquo(label_var)
  group_var_flag <- dplyr::enquo(group_var)
  group_var_char <- rlang::as_name(group_var_flag)
  percent_var_flag <- dplyr::enquo(percent_var)
  percent_filter_flag <- dplyr::enquo(percent_filter)
  # `percent_var = "All"` is a magic string meaning "% on every row" — same
  # behavior as the deprecated `percent_all = TRUE`. Decode it here so the
  # rest of the function only has to deal with the boolean.
  percent_var_expr <- rlang::quo_get_expr(percent_var_flag)
  if (
    is.character(percent_var_expr) &&
      length(percent_var_expr) == 1 &&
      percent_var_expr == "All"
  ) {
    percent_all <- TRUE
    percent_var_flag <- rlang::quo(NULL)
  }
  # Stacked flags: bars always inherently ordered
  inherent_order_label <- ifelse(
    stacked != 'NULL',
    TRUE,
    inherent_order_label
  )
  # Grouping flags
  if (group_var_char == 'NULL') {
    grouped <- FALSE
  } else {
    grouped <- TRUE
  }
  # Flags for putting a specific label/group first
  specifically_ordered <- ifelse(
    is.character(label_first) == TRUE,
    TRUE,
    FALSE
  )
  specifically_ordered_group <- ifelse(
    is.character(group_first) == TRUE,
    TRUE,
    FALSE
  )

  ### Stop if group_var not specified but other grouping arguments are
  if (
    (inherent_order_group == TRUE |
      rev_group == TRUE |
      !is.na(group_first) |
      !is.na(group_last)) &
      grouped == FALSE
  ) {
    stop(
      "You specified a grouping argument but not the group_var. Either add in a variable for group_var or do not run other grouping arguments."
    )
  }
  if (stacked != 'NULL' & grouped == FALSE) {
    warning(
      'You used a "stacked" ordering system without specifying group_var. Is your data grouped?'
    )
  }

  if (grouped == FALSE & !is.null(topbox)) {
    stop('You cannot use the topbox argument on ungrouped data.')
  }

  if (inherent_order_group == TRUE & !is.null(topbox)) {
    stop(
      'You cannot use the topbox argument when inherent_order_group is TRUE.'
    )
  }

  ### Prep work
  dataset <- dataset |> dplyr::ungroup()
  dataset <- dataset |> factors(grouped, !!group_var_flag, !!label_var_flag)
  # dataset <- reverse_label(dataset, grouped, !!group_var_flag, !!label_var_flag, rev_label)

  ### (1) ungrouped Section
  if (grouped == FALSE) {
    dataset <- section_ungrouped(
      dataset,
      grouped,
      specifically_ordered,
      inherent_order_label,
      stacked,
      label_first,
      rev_label
    )
    ### Arranging WITH grouping variables
  } else {
    # (2) Grouped Section: arranging for specific group and label to be first
    dataset <- section_grouped_specifics(
      dataset,
      specifically_ordered,
      label_first,
      inherent_order_label,
      group_var,
      inherent_order_group,
      group_first,
      specifically_ordered_group,
      rev_group
    )
    # (3) Grouped Section: arranging for specific group to be first
    dataset <- section_grouped_specifics_nolab(
      dataset,
      specifically_ordered,
      inherent_order_label,
      group_var,
      inherent_order_group,
      group_first,
      specifically_ordered_group,
      rev_group,
      rev_label
    )
    # (4) Grouped Section: inherent order of grouping variables
    dataset <- section_grouped_ordered(
      dataset,
      specifically_ordered,
      label_first,
      inherent_order_label,
      group_var,
      inherent_order_group,
      group_first,
      specifically_ordered_group,
      rev_group,
      rev_label
    )
    # (5) Grouped Section: arranging grouping variables if group NOT inherently ordered
    dataset <- section_grouped_unordered(
      dataset,
      specifically_ordered,
      label_first,
      inherent_order_label,
      group_var,
      inherent_order_group,
      group_first,
      specifically_ordered_group,
      rev_group,
      rev_label
    )
  }
  ### topbox
  dataset <- topbox_fun(dataset, topbox)
  ### label_last & group_last
  dataset <- label_last_fun(dataset, label_last, direction, stacked)
  dataset <- group_last_fun(dataset, group_last, direction, stacked)
  ### Put "None" & "Other" at bottom
  dataset <- none_other_fun(dataset, none_other, grouped)
  ### Horizontal
  dataset <- horizontal_chart(dataset, direction, grouped)
  ### Stacked
  dataset <- stacked_chart(
    dataset,
    stacked,
    grouped,
    inherent_order_group,
    specifically_ordered_group
  )
  dataset <- stacked_chart_ms(
    dataset,
    stacked,
    grouped,
    inherent_order_group,
    specifically_ordered_group
  )
  ### num_fmt
  dataset <- num_fmt_orderlabel(dataset, num_fmt, percent_all)
  ### arrange_by_factor
  dataset <- arrange_by_factor(dataset, grouped)
  ### percent_var override (after final arrange so row 1 per group is the visual top)
  dataset <- percent_var_fun(
    dataset,
    percent_var_flag,
    percent_filter_flag,
    percent_all,
    num_fmt,
    inherent_order_label
  )
  return(dataset)
}


#### ***** Private order_label Functions ***** ####
#### Blank values ####
blank_values <- function(
  dataset
) {
  # If no value, add in place filler
  if (any(names(dataset) == 'value') == FALSE) {
    dataset$value <- 'x'
  } else {
    dataset <- dataset
  }

  # If no label, add in place filler
  if (any(names(dataset) == 'label') == FALSE) {
    dataset$label <- 'x'
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Add label ####
add_label <- function(
  dataset,
  label_var
) {
  dataset <- blank_values(dataset)
  label_var_flag <- dplyr::enquo(label_var)
  dataset <- dataset |>
    dplyr::mutate(
      label = !!label_var_flag
    ) |>
    dplyr::relocate(
      'label',
      .after = 'value'
    )
}

#### Add group ####
add_group <- function(
  dataset,
  grouped,
  group_var,
  label_var
) {
  # Frequencies with a grouping variable must be grouped for following section
  if (grouped == TRUE) {
    label_var_flag <- dplyr::enquo(label_var)
    dataset <- add_label(dataset, !!label_var_flag)
    group_var_flag <- dplyr::enquo(group_var)
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(
        group_var = !!group_var_flag
      ) |>
      dplyr::group_by(
        group_var
      )
  } else {
    label_var_flag <- dplyr::enquo(label_var)
    dataset <- add_label(dataset, !!label_var_flag)
  }
}

#### Factors ####
# When inherent_order == TRUE, labels are ordered by value #s
# But factored variables won't have #s in value column. Let's give them some
factors <- function(
  dataset,
  grouped,
  group_var,
  label_var
) {
  label_var_flag <- dplyr::enquo(label_var)
  group_var_flag <- dplyr::enquo(group_var)
  dataset <- add_group(dataset, grouped, !!group_var_flag, !!label_var_flag)
  # When "value" is factored, value needs to be changed to .number
  # When "value" was completely missing or all the same, values needs to be changed to distinct .number
  if (
    dataset$value[1] == dataset$label[1] |
      length(unique(dataset$value)) == 1 |
      dataset$value[1] != '1'
  ) {
    # Assign each label a consistent integer based on factor order of appearance.
    # Done ungrouped so groups with different row counts (e.g., multi-select
    # freqs across countries with different option presence) don't fail.
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(
        value = as.integer(
          forcats::fct_inorder(as.character(.data$label))
        )
      )
  } else {
    dataset <- dataset
  }

  # Now convert value to numeric for inherent_orders
  dataset <- dataset |>
    dplyr::mutate(
      value = gsub("[^0-9.]", "", .data$value) |>
        as.character() |>
        as.numeric()
    )
  return(dataset)
}

#### Reverse label inherent order ####
reverse_label <- function(
  dataset,
  grouped,
  group_var,
  label_var,
  rev_label
) {
  label_var_flag <- dplyr::enquo(label_var)
  group_var_flag <- dplyr::enquo(group_var)
  dataset <- factors(dataset, grouped, !!group_var_flag, !!label_var_flag)
  if (rev_label == TRUE) {
    max_val <- max(dataset$value)
    min_val <- min(dataset$value)
    dataset <- dataset |>
      dplyr::mutate(
        value = rev(.data$value)
      )
  } else {
    dataset <- dataset
  }
}

#### Reverse label unordered ####
reverse_label_unordered <- function(
  dataset,
  rev_label
) {
  if (rev_label == TRUE) {
    dataset <- dataset |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::arrange(
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::arrange(
        label = forcats::fct_rev(.data$label)
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label)
      )
  } else {
    dataset <- dataset
  }
}

#### (1) Ungrouped Section ####
section_ungrouped <- function(
  dataset,
  grouped,
  specifically_ordered,
  inherent_order_label,
  stacked,
  label_first,
  rev_label
) {
  if (grouped == FALSE) {
    dataset <- ungrouped1(
      dataset,
      specifically_ordered,
      inherent_order_label,
      label_first,
      rev_label
    )
    dataset <- ungrouped2(
      dataset,
      specifically_ordered,
      inherent_order_label,
      label_first,
      rev_label
    )
    dataset <- ungrouped3(dataset, stacked, rev_label)
    dataset <- ungrouped4(
      dataset,
      specifically_ordered,
      inherent_order_label,
      rev_label
    )
    dataset <- ungrouped5(
      dataset,
      specifically_ordered,
      inherent_order_label,
      rev_label
    )
  } else {
    data <- dataset
  }
  return(dataset)
}

#### ungrouped 1 ####
# Arranging with specific label first
ungrouped1 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  label_first,
  rev_label
) {
  if (
    specifically_ordered == TRUE &
      inherent_order_label == FALSE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(.data$label == label_first)
    freqs2 <- dataset |>
      dplyr::filter(.data$label != label_first) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered(rev_label)
    dataset <- dplyr::bind_rows(freqs1, freqs2) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == label_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      )
  } else {
    dataset <- dataset
  }
}

#### ungrouped 2 ####
ungrouped2 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  label_first,
  rev_label
) {
  if (
    specifically_ordered == TRUE &
      inherent_order_label == TRUE
  ) {
    # Arranging with specific label first, then inherent
    freqs1 <- dataset |>
      dplyr::filter(.data$label == label_first)
    freqs2 <- dataset |>
      dplyr::filter(.data$label != label_first) |>
      dplyr::arrange(
        .data$value
      ) |>
      reverse_label_unordered(rev_label)
    dataset <- dplyr::bind_rows(freqs1, freqs2) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == label_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      )
  } else {
    dataset <- dataset
  }
}

#### ungrouped 3 ####
ungrouped3 <- function(
  dataset,
  stacked,
  rev_label
) {
  if (stacked != 'NULL') {
    dataset <- dataset |>
      dplyr::arrange(
        .data$value,
        .data$result
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        label = forcats::fct_rev(.data$label)
      ) |>
      reverse_label_unordered(rev_label)
  } else {
    dataset <- dataset
  }
}

#### ungrouped 4 ####
# Arranging for non inherent order labels
ungrouped4 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  rev_label
) {
  if (
    inherent_order_label == FALSE &
      specifically_ordered == FALSE
  ) {
    dataset <- dataset |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      reverse_label_unordered(rev_label)
  } else {
    dataset <- dataset
  }
}

#### ungrouped 5 ####
ungrouped5 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  rev_label
) {
  if (
    inherent_order_label == TRUE &
      specifically_ordered == FALSE
  ) {
    # Arranging for inherent order labels
    dataset <- dataset |>
      dplyr::arrange(
        .data$value,
        .data$result
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      reverse_label_unordered(rev_label)
  } else {
    dataset <- dataset
  }
}


#### Reverse Group ####
reverse_group <- function(
  dataset,
  rev_group
) {
  if (rev_group == TRUE) {
    withr::local_options(list(warn = -1)) # Warning about unknown levels of Other, NOA, PNTS, that's fine
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::arrange(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      dplyr::arrange(
        group_var = forcats::fct_rev(.data$group_var)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      dplyr::arrange(
        group_var = forcats::fct_relevel(
          .data$group_var,
          "Other",
          'None of the above',
          "Prefer not to say",
          after = Inf
        )
      ) |>
      dplyr::mutate(group_var = forcats::fct_inorder(.data$group_var))
  } else {
    dataset <- dataset
  }
}


#### Reverse label unordered pt 2 ####
reverse_label_unordered2 <- function(
  dataset,
  rev_label
) {
  if (rev_label == TRUE) {
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::arrange(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      dplyr::arrange(
        group_var = forcats::fct_rev(.data$group_var)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      dplyr::group_by(.data$group_var)
  } else {
    dataset <- dataset
  }
}

#### (2) Grouped Specifics for group and label ####
section_grouped_specifics <- function(
  dataset,
  specifically_ordered,
  label_first,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_first,
  specifically_ordered_group,
  rev_group # fix reversed groups in this section
) {
  if (
    specifically_ordered_group == TRUE &
      specifically_ordered == TRUE
  ) {
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(
        group_var = forcats::as_factor(group_var)
      )

    dataset <- grouped_specific1(
      dataset,
      inherent_order_label,
      inherent_order_group,
      label_first,
      group_first
    )
    dataset <- grouped_specific2(
      dataset,
      inherent_order_label,
      inherent_order_group,
      label_first,
      group_first
    )
    dataset <- grouped_specific3(
      dataset,
      inherent_order_label,
      inherent_order_group,
      label_first,
      group_first
    )
    dataset <- grouped_specific4(
      dataset,
      inherent_order_label,
      inherent_order_group,
      label_first,
      group_first
    )
    dataset <- dataset |> reverse_group(rev_group)
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Grouped specifics 1 ####
grouped_specific1 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_first,
  group_first
) {
  if (
    inherent_order_label == FALSE & #No inherent order for group/label
      inherent_order_group == FALSE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first &
          .data$label == label_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label == label_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs4 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label != label_first
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3, freqs4) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$label == label_first & .data$group_var == group_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped specifics 2 ####
grouped_specific2 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_first,
  group_first
) {
  if (
    inherent_order_label == TRUE & #label inherent
      inherent_order_group == FALSE
  ) {
    #group NOT
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first &
          .data$label == label_first
      )
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label == label_first
      ) |>
      dplyr::arrange(
        .data$value,
        dplyr::desc(.data$result)
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first &
          .data$label != label_first
      ) |>
      dplyr::arrange(
        .data$value
      )
    freqs4 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label != label_first
      ) |>
      dplyr::arrange(
        .data$value
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3, freqs4) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$group_var == group_first & .data$label == label_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped specifics 3 ####
grouped_specific3 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_first,
  group_first
) {
  if (
    inherent_order_label == TRUE & #Both label/group inherently ordered
      inherent_order_group == TRUE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first &
          .data$label == label_first
      )
    freqs2 <- dataset |>
      dplyr::filter(
        .data$label != label_first &
          .data$group_var == group_first
      ) |>
      dplyr::arrange(
        .data$value
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first
      ) |>
      dplyr::arrange(
        .data$value,
        .data$group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == label_first & .data$group_var == group_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped specifics 4 ####
grouped_specific4 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_first,
  group_first
) {
  if (
    inherent_order_label == FALSE &
      inherent_order_group == TRUE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first &
          .data$label == label_first
      )
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first &
          .data$label != label_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label == label_first
      ) |>
      dplyr::arrange(
        .data$group_var
      )
    freqs4 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label != label_first
      ) |>
      dplyr::arrange(
        .data$group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3, freqs4) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$group_var == group_first & .data$label == label_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### (3) Grouped, specific for group only ####
section_grouped_specifics_nolab <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_first,
  specifically_ordered_group,
  rev_group, #fix reversed groups in this section
  rev_label
) {
  if (
    specifically_ordered_group == TRUE &
      specifically_ordered == FALSE
  ) {
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(
        group_var = forcats::as_factor(.data$group_var)
      )

    dataset <- grouped_specific5(
      dataset,
      inherent_order_label,
      inherent_order_group,
      group_first,
      rev_label,
      rev_group
    )
    dataset <- grouped_specific6(
      dataset,
      inherent_order_label,
      inherent_order_group,
      group_first,
      rev_label,
      rev_group
    )
    dataset <- grouped_specific7(
      dataset,
      inherent_order_label,
      inherent_order_group,
      group_first,
      rev_label,
      rev_group
    )
    dataset <- grouped_specific8(
      dataset,
      inherent_order_label,
      inherent_order_group,
      group_first,
      rev_label,
      rev_group
    )
    dataset <- dataset |>
      reverse_group(rev_group) |>
      dplyr::mutate(
        group_var = forcats::fct_relevel(
          .data$group_var,
          group_first
        )
      )
  } else {
    dataset <- dataset
  }
  return(dataset)
}


#### Grouped Specifics 5 ####
grouped_specific5 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_first,
  rev_label,
  rev_group
) {
  if (
    inherent_order_label == FALSE &
      inherent_order_group == FALSE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered(rev_label)
    group1 <- freqs1$label[1]
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label == group1
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered2(rev_label)
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label != group1
      )
    freqs_combo <- dplyr::bind_rows(freqs2, freqs3)
    dataset <- dplyr::bind_rows(freqs1, freqs_combo) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == group_first,
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped Specifics 6 ####
grouped_specific6 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_first,
  rev_label,
  rev_group
) {
  if (
    inherent_order_label == TRUE & #No inherent order for group/label
      inherent_order_group == FALSE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first
      ) |>
      dplyr::arrange(
        .data$value,
        dplyr::desc(.data$result)
      )
    group1 <- freqs1$label[1]
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label == group1
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first &
          .data$label != group1
      )
    freqs_combo <- dplyr::bind_rows(freqs2, freqs3)
    dataset <- dplyr::bind_rows(freqs1, freqs_combo) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$group_var == group_first & .data$label == .data$label[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped Specifics 7 ####
grouped_specific7 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_first,
  rev_label,
  rev_group
) {
  if (
    inherent_order_label == TRUE &
      inherent_order_group == TRUE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first
      ) |>
      dplyr::arrange(
        .data$value
      )
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first
      ) |>
      dplyr::arrange(
        .data$value,
        .data$group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped Specifics 8 ####
grouped_specific8 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_first,
  rev_label,
  rev_group
) {
  if (
    inherent_order_label == FALSE &
      inherent_order_group == TRUE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == group_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered(rev_label)
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var != group_first
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### (4) Grouped Ordered Section ####
section_grouped_ordered <- function(
  dataset,
  specifically_ordered,
  label_first,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_first,
  specifically_ordered_group,
  rev_group,
  rev_label
) {
  if (
    specifically_ordered_group == FALSE &
      inherent_order_group == TRUE
  ) {
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(group_var = forcats::fct_inorder(.data$group_var))

    dataset <- grouped_ordered1(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      label_first,
      rev_group
    )
    dataset <- grouped_ordered2(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      label_first,
      rev_group,
      rev_label
    )
    dataset <- grouped_ordered3(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      rev_group,
      rev_label
    )
    dataset <- grouped_ordered4(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      rev_group
    )
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Grouped Ordered 1 ####
grouped_ordered1 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_first,
  rev_group
) {
  if (
    specifically_ordered == TRUE &
      inherent_order_label == TRUE
  ) {
    # Arranging for specific label first
    freqs1 <- dataset |>
      dplyr::filter(
        .data$label == label_first
      ) |>
      dplyr::arrange(
        .data$group_var
      ) |>
      reverse_group(rev_group)
    freqs2 <- dataset |>
      dplyr::filter(
        .data$label != label_first
      ) |>
      dplyr::arrange(
        .data$value,
        .data$group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped Ordered 2 ####
grouped_ordered2 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_first,
  rev_group,
  rev_label
) {
  if (
    specifically_ordered == TRUE &
      inherent_order_label == FALSE
  ) {
    # Arranging for specific label first
    dataset <- dataset |> reverse_group(rev_group)
    freqs1 <- dataset |>
      dplyr::filter(
        .data$group_var == .data$group_var[1] &
          .data$label == label_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs2 <- dataset |>
      dplyr::filter(
        .data$group_var == .data$group_var[1] &
          .data$label != label_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered(rev_label)
    freqs3 <- dataset |>
      dplyr::filter(
        .data$group_var != .data$group_var[1]
      )

    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == label_first & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Grouped Ordered 3 ####
grouped_ordered3 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group,
  rev_label
) {
  if (
    inherent_order_label == FALSE &
      specifically_ordered == FALSE
  ) {
    dataset <- dataset |>
      dplyr::arrange(
        .data$group_var,
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered(rev_label) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      reverse_group(rev_group)
  } else {
    dataset <- dataset
  }
}

#### Grouped Ordered 4 ####
grouped_ordered4 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group
) {
  if (
    inherent_order_label == TRUE &
      specifically_ordered == FALSE
  ) {
    dataset <- dataset |>
      dplyr::arrange(
        .data$group_var,
        .data$value,
        .data$result
      ) |>
      reverse_group(rev_group) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}


#### (5) Grouped Unordered Section ####
section_grouped_unordered <- function(
  dataset,
  specifically_ordered,
  label_first,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_first,
  specifically_ordered_group,
  rev_group,
  rev_label
) {
  if (
    specifically_ordered_group == FALSE &
      inherent_order_group == FALSE
  ) {
    dataset <- dataset |>
      dplyr::ungroup() |>
      dplyr::mutate(
        group_var = forcats::as_factor(.data$group_var)
      )

    dataset <- grouped_unordered1(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      label_first,
      rev_group,
      rev_label
    )
    dataset <- grouped_unordered2(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      label_first,
      rev_group
    )
    dataset <- grouped_unordered3(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      rev_group,
      rev_label
    )
    dataset <- grouped_unordered4(
      dataset,
      inherent_order_label,
      inherent_order_group,
      specifically_ordered,
      rev_group
    )
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Grouped Unordered 1 ####
grouped_unordered1 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_first,
  rev_group,
  rev_label
) {
  if (
    specifically_ordered == TRUE &
      inherent_order_label == FALSE
  ) {
    freqs1 <- dataset |>
      dplyr::filter(
        .data$label == label_first
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      reverse_label_unordered(rev_label)
    group1 <- freqs1$group_var[1]
    freqs2 <- dataset |>
      dplyr::filter(
        .data$label != label_first &
          .data$group_var == group1
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$label != label_first &
          .data$group_var != group1
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$label == label_first & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      reverse_group(rev_group)
  } else {
    dataset <- dataset
  }
}

#### Grouped Unordered 2 ####
grouped_unordered2 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_first,
  rev_group
) {
  if (
    specifically_ordered == TRUE &
      inherent_order_label == TRUE
  ) {
    #Arranging for specific label first
    freqs1 <- dataset |>
      dplyr::filter(
        .data$label == label_first
      ) |>
      dplyr::arrange(
        .data$value,
        dplyr::desc(.data$result),
        .data$group_var
      )
    group1 <- freqs1$group_var[1]
    freqs2 <- dataset |>
      dplyr::filter(
        .data$label != label_first &
          .data$group_var == group1
      ) |>
      dplyr::arrange(
        .data$value
      )
    freqs3 <- dataset |>
      dplyr::filter(
        .data$label != label_first &
          .data$group_var != group1
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) |>
      dplyr::mutate(
        percent_label = ifelse(
          .data$label == label_first & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        ),
        label = forcats::fct_inorder(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      reverse_group(rev_group)
  } else {
    dataset <- dataset
  }
}

#### Grouped Unordered 3 ####
grouped_unordered3 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group,
  rev_label
) {
  if (
    inherent_order_label == FALSE &
      specifically_ordered == FALSE
  ) {
    freqs1 <- dataset |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label)
      ) |>
      reverse_label_unordered(rev_label) |>
      dplyr::filter(
        .data$label == .data$label[1]
      )
    group1 <- freqs1$label[1]
    freqs2 <- dataset |>
      dplyr::filter(
        .data$label != group1
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$result)
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label)
      ) |>
      reverse_label_unordered(rev_label)
    dataset <- dplyr::bind_rows(freqs1, freqs2) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var),
        percent_label = ifelse(
          .data$label == group1 & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label)
      ) |>
      reverse_group(rev_group)
  } else {
    dataset <- dataset
  }
}

#### Grouped Unordered 4 ####
grouped_unordered4 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group
) {
  if (
    inherent_order_label == TRUE &
      specifically_ordered == FALSE
  ) {
    dataset <- dataset |>
      dplyr::arrange(
        .data$value,
        dplyr::desc(.data$result),
        .data$group_var
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var)
      ) |>
      reverse_group(rev_group)
  } else {
    dataset <- dataset
  }
}

#### topbox ####
topbox_fun <- function(
  dataset,
  topbox = NULL
) {
  if (is.null(topbox) == FALSE) {
    top2box <- topbox

    dataset <- spreading_top2(dataset, top2box)
    dataset <- ordering_top2(dataset)
  } else {
    # is.null(topbox)
    dataset <- dataset
  }
}

### spreading_top2
spreading_top2 <- function(dataset, top2box) {
  . <- NULL
  top2_plus1 <- top2box + 1 # Need to add 1 because 1st column is group_var, not top box

  test1 <- dataset |>
    dplyr::select(
      -'result'
    ) |>
    dplyr::mutate(value = as.character(.data$value))

  test2 <- dataset |>
    dplyr::mutate(value1 = .data$value) |>
    dplyr::select('group_var', 'value1', 'result') |>
    tidyr::pivot_wider(names_from = 'value1', values_from = 'result') |>
    dplyr::mutate(
      topbox = rowSums(dplyr::pick(2:top2_plus1), na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(
      cols = -c('group_var', 'topbox'),
      names_to = "value",
      values_to = "result"
    ) |>
    dplyr::mutate(value = as.character(.data$value))

  dataset <- dplyr::left_join(
    test1,
    test2
  )
}

### ordering_top2
ordering_top2 <- function(dataset) {
  dataset <- dataset |>
    dplyr::arrange(
      dplyr::desc(.data$topbox)
    ) |>
    dplyr::mutate(
      group_var = forcats::fct_inorder(.data$group_var)
    ) |>
    dplyr::mutate(
      label = forcats::fct_inorder(.data$label)
    ) |>
    dplyr::mutate(
      percent_label = ifelse(
        .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
        stringr::str_c(.data$result * 100, '%'),
        stringr::str_c(.data$result * 100)
      )
    )
}


#### Horizontal ####
horizontal_chart <- function(
  dataset,
  direction,
  grouped
) {
  if (direction == 'horizontal') {
    if (grouped == FALSE) {
      dataset <- dataset |>
        dplyr::mutate(
          label = forcats::fct_rev(.data$label)
        )
    } else {
      #grouped == TRUE
      dataset <- dataset |>
        dplyr::mutate(
          label = forcats::fct_rev(.data$label)
        ) |>
        dplyr::mutate(
          group_var = forcats::fct_rev(.data$group_var)
        )
    }
  } else {
    #direction == 'vertical
    dataset <- dataset
  }
}

#### GG Stacked ####
stacked_chart <- function(
  dataset,
  stacked,
  grouped,
  inherent_order_group,
  specifically_ordered
) {
  if (
    stacked == 'gg' &
      grouped == FALSE
  ) {
    dataset <- dataset |>
      dplyr::mutate(
        label = forcats::fct_rev(.data$label)
      )
  } else if (
    stacked == 'gg' &
      grouped == TRUE
  ) {
    dataset <- dataset |>
      dplyr::mutate(
        label = forcats::fct_rev(.data$label)
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_rev(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### MS Stacked ####
stacked_chart_ms <- function(
  dataset,
  stacked,
  grouped,
  inherent_order_group,
  specifically_ordered_group
) {
  if (
    stacked == 'ms' &
      grouped == TRUE
  ) {
    dataset <- dataset |>
      dplyr::mutate(
        group_var = forcats::fct_rev(.data$group_var)
      )
  } else {
    dataset <- dataset
  }
}


#### none_other ####
none_other_fun <- function(
  dataset,
  none_other,
  grouped
) {
  if (none_other == TRUE) {
    withr::local_options(list(warn = -1)) # Warning about unknown levels of Other, NOA, PNTS, that's fine
    dataset <- dataset |>
      dplyr::arrange(
        label = forcats::fct_relevel(
          .data$label,
          "Other",
          'None of the above',
          "Prefer not to say",
          after = Inf
        )
      ) |>
      dplyr::mutate(
        label = forcats::fct_inorder(.data$label),
        percent_label = ifelse(
          .data$label == .data$label[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      )
  } else {
    dataset <- dataset
  }
  #For grouped
  if (grouped == TRUE & none_other == TRUE) {
    withr::local_options(list(warn = -1)) # Warnings here are about levels not existing, but probs won't in most cases
    dataset <- dataset |>
      dplyr::arrange(
        group_var = forcats::fct_relevel(
          .data$group_var,
          "Other",
          'None of the above',
          "Prefer not to say",
          after = Inf
        )
      ) |>
      dplyr::mutate(
        group_var = forcats::fct_inorder(.data$group_var),
        percent_label = ifelse(
          .data$label == .data$label[1] & .data$group_var == .data$group_var[1],
          stringr::str_c(.data$result * 100, '%'),
          stringr::str_c(.data$result * 100)
        )
      )
  } else {
    dataset <- dataset
  }
}


#### label_last ####
label_last_fun <- function(
  dataset,
  label_last,
  direction,
  stacked
) {
  label_last_dummy = ifelse(
    is.character(label_last) == TRUE,
    TRUE,
    FALSE
  )

  # Put label_last last
  if (label_last_dummy == TRUE & direction == 'vertical') {
    dataset <- dataset |>
      dplyr::arrange(
        label = forcats::fct_relevel(
          .data$label,
          label_last,
          after = Inf
        )
      ) |>
      dplyr::mutate(label = forcats::fct_inorder(.data$label))
    return(dataset)
    # Put label_last "first" to be last for horizontal
  } else if (label_last_dummy == TRUE & direction == 'horizontal') {
    data_rest <- dataset |> dplyr::filter(.data$label != label_last)
    data_label <- dataset |> dplyr::filter(.data$label == label_last)
    data_rest <- data_rest |>
      dplyr::arrange(
        label = forcats::fct_relevel(
          .data$label
        )
      ) |>
      dplyr::arrange(label = forcats::fct_rev(.data$label))
    dataset <- dplyr::bind_rows(data_label, data_rest) |>
      dplyr::mutate(label = forcats::fct_inorder(.data$label))
    # Horizontal and NOT stacked, one more reverse...
    if (stacked == 'NULL') {
      dataset <- dataset |>
        dplyr::arrange(label = forcats::fct_rev(.data$label)) |>
        dplyr::mutate(label = forcats::fct_inorder(.data$label))
      return(dataset)
    }
  } else {
    dataset <- dataset
  }
}

#### group_last ####
group_last_fun <- function(
  dataset,
  group_last,
  direction,
  stacked
) {
  group_last_dummy = ifelse(
    is.character(group_last) == TRUE,
    TRUE,
    FALSE
  )

  # put group_last last
  if (group_last_dummy == TRUE & direction == 'vertical') {
    dataset <- dataset |>
      dplyr::arrange(
        group_var = forcats::fct_relevel(
          .data$group_var,
          group_last,
          after = Inf
        )
      ) |>
      dplyr::mutate(group_var = forcats::fct_inorder(.data$group_var))
    return(dataset)
    # Put group_last "first" to be last for horizontal
  } else if (group_last_dummy == TRUE & direction == 'horizontal') {
    data_rest <- dataset |> dplyr::filter(.data$group_var != group_last)
    data_label <- dataset |> dplyr::filter(.data$group_var == group_last)
    data_rest <- data_rest |>
      dplyr::arrange(
        group_var = forcats::fct_relevel(
          .data$group_var
        )
      ) |>
      dplyr::arrange(group_var = forcats::fct_rev(.data$group_var))
    dataset <- dplyr::bind_rows(data_label, data_rest) |>
      dplyr::mutate(group_var = forcats::fct_inorder(.data$group_var))
    # Horizontal and NOT stacked, one more reverse...
    if (stacked == 'NULL') {
      dataset <- dataset |>
        dplyr::arrange(group_var = forcats::fct_rev(.data$group_var)) |>
        dplyr::mutate(group_var = forcats::fct_inorder(.data$group_var))
      return(dataset)
    }
  } else {
    dataset <- dataset
  }
}


#### num_fmt ####
num_fmt_orderlabel <- function(
  dataset,
  num_fmt,
  percent_all
) {
  if (num_fmt == "percent") {
    if (percent_all == TRUE) {
      dataset <- dataset |>
        dplyr::mutate(percent_label = stringr::str_c(.data$result * 100, '%'))
    }
    dataset <- dataset
  } else {
    dataset <- dataset |>
      dplyr::mutate(percent_label = as.character(.data$result))
  }
}


#### percent_var ####
# Override percent_label so % appears on the "top" row within each level of
# percent_var, where "top" honors the same ordering criterion as the rest of
# the function: largest `result` by default, or smallest `value` when
# inherent_order_label = TRUE. No-op when percent_var isn't set, when
# percent_all = TRUE (full % override wins), or when num_fmt = "general".
percent_var_fun <- function(
  dataset,
  percent_var_flag,
  percent_filter_flag,
  percent_all,
  num_fmt,
  inherent_order_label
) {
  if (
    rlang::quo_is_null(percent_var_flag) ||
      isTRUE(percent_all) ||
      num_fmt != "percent"
  ) {
    return(dataset)
  }

  existing_groups <- dplyr::groups(dataset)
  has_filter <- !rlang::quo_is_null(percent_filter_flag)

  dataset <- dataset |>
    dplyr::ungroup() |>
    dplyr::group_by(!!percent_var_flag)

  # Eligibility mask: TRUE for rows that may receive the %. When no filter is
  # provided, every row is eligible.
  if (has_filter) {
    dataset <- dataset |>
      dplyr::mutate(
        .pct_eligible = dplyr::coalesce(!!percent_filter_flag, FALSE)
      )
  } else {
    dataset <- dataset |>
      dplyr::mutate(.pct_eligible = TRUE)
  }

  if (isTRUE(inherent_order_label)) {
    # Among eligible rows in each percent_var level, pick the smallest value,
    # breaking ties by largest result.
    dataset <- dataset |>
      dplyr::mutate(
        .pct_target_val = suppressWarnings(min(
          ifelse(.data$.pct_eligible, .data$value, NA_real_),
          na.rm = TRUE
        )),
        .pct_target_result = suppressWarnings(max(
          ifelse(
            .data$.pct_eligible &
              .data$value == .data$.pct_target_val,
            .data$result,
            NA_real_
          ),
          na.rm = TRUE
        )),
        .pct_is_top = dplyr::coalesce(
          .data$.pct_eligible &
            .data$value == .data$.pct_target_val &
            .data$result == .data$.pct_target_result,
          FALSE
        ),
        .pct_rank = cumsum(.data$.pct_is_top)
      )
  } else {
    # Among eligible rows in each percent_var level, pick the largest result.
    dataset <- dataset |>
      dplyr::mutate(
        .pct_target_result = suppressWarnings(max(
          ifelse(.data$.pct_eligible, .data$result, NA_real_),
          na.rm = TRUE
        )),
        .pct_is_top = dplyr::coalesce(
          .data$.pct_eligible &
            .data$result == .data$.pct_target_result,
          FALSE
        ),
        .pct_rank = cumsum(.data$.pct_is_top)
      )
  }

  dataset <- dataset |>
    dplyr::mutate(
      percent_label = ifelse(
        .data$.pct_is_top & .data$.pct_rank == 1,
        stringr::str_c(.data$result * 100, '%'),
        stringr::str_c(.data$result * 100)
      )
    ) |>
    dplyr::select(-tidyselect::starts_with(".pct_")) |>
    dplyr::ungroup()

  if (length(existing_groups) > 0) {
    dataset <- dataset |> dplyr::group_by(!!!existing_groups)
  }
  dataset
}


# arrange_by_factor: mschart htmltools updated ordering --------------------------------------
arrange_by_factor <- function(
  dataset,
  grouped
) {
  if (grouped == FALSE) {
    dataset <- dataset |>
      dplyr::arrange(.data$label)
  } else {
    dataset <- dataset |>
      dplyr::arrange(
        .data$label,
        .data$group_var
      )
  }
}

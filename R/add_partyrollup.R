# add_partyrollup primary function ----------------------------------------
#' Add a 7-level party identification variable to a data frame
#'
#' Combines a party identification variable, a party strength variable, and a
#' party leaning variable into a single 7-point party rollup, the standard ANES
#' branching scale. Respondents who said Republican or Democrat on the party
#' question were asked how strongly they identify (levels 1-2 and 6-7);
#' respondents who said independent or something else were asked which party
#' they lean toward (levels 3-5).
#'
#' The new variable is a [haven::labelled()] numeric vector:
#'
#' 1. Strong Republican
#' 2. Not so strong Republican
#' 3. Lean Republican
#' 4. Independent
#' 5. Lean Democrat
#' 6. Not so strong Democrat
#' 7. Strong Democrat
#'
#' The function errors if any respondent's `party_id` and `party_lean` point to
#' different parties (e.g. a Republican who leans Democratic), since such a row
#' cannot be classified unambiguously.
#'
#' @param df The data frame for the function to modify, usually piped in.
#' @param party_id DEFAULT = NULL; the party identification variable, as a bare
#'   column name. When `NULL`, the function searches `df` for a likely match by
#'   name. Values may be numeric, character, factor, or `haven_labelled`; the
#'   value labels are matched case-insensitively for "republican" / "democrat".
#' @param party_lean DEFAULT = NULL; the party leaning variable, used for
#'   respondents who did not say Republican or Democrat. When `NULL`, searched
#'   for by name. Matched for "republican" / "democrat".
#' @param party_strength DEFAULT = NULL; the party strength variable, used for
#'   respondents who said Republican or Democrat. When `NULL`, searched for by
#'   name. Matched for "strong" vs. "not so strong".
#' @param new_name DEFAULT = partyid_7; the name of the new variable, as a bare
#'   name not a string.
#' @export
#' @examples
#' responses <- tibble::tibble(
#'   party_id = c("Republican", "Democrat", "Independent", "Something else"),
#'   party_strength = c("Strong", "Not so strong", NA, NA),
#'   party_lean = c(NA, NA, "Lean Republican", "Neither")
#' )
#'
#' add_partyrollup(responses)
#' add_partyrollup(
#'   responses,
#'   party_id = party_id,
#'   party_lean = party_lean,
#'   party_strength = party_strength
#' )

add_partyrollup <- function(
  df,
  party_id = NULL,
  party_lean = NULL,
  party_strength = NULL,
  new_name = partyid_7
) {
  party_id_col <- resolve_party_var(
    df,
    rlang::enquo(party_id),
    "party_id",
    keywords = "party|pid",
    exclude = "lean|stren|strong",
    arg_name = "party_id"
  )
  party_lean_col <- resolve_party_var(
    df,
    rlang::enquo(party_lean),
    "party_lean",
    keywords = "lean",
    exclude = NULL,
    arg_name = "party_lean"
  )
  party_strength_col <- resolve_party_var(
    df,
    rlang::enquo(party_strength),
    "party_strength",
    keywords = "stren|strong",
    exclude = "lean",
    arg_name = "party_strength"
  )

  party_text <- labels_to_text(dplyr::pull(df, dplyr::all_of(party_id_col)))
  lean_text <- labels_to_text(dplyr::pull(df, dplyr::all_of(party_lean_col)))
  strength_text <- labels_to_text(
    dplyr::pull(df, dplyr::all_of(party_strength_col))
  )

  is_rep <- detect_ci(party_text, "republican")
  is_dem <- detect_ci(party_text, "democrat")
  # "not so strong" / "not very strong" both contain "not"
  is_not_strong <- detect_ci(strength_text, "not")
  is_strong <- detect_ci(strength_text, "strong") & !is_not_strong
  leans_rep <- detect_ci(lean_text, "republican")
  leans_dem <- detect_ci(lean_text, "democrat")

  # A partisan should never lean toward the opposite party; such rows cannot be
  # classified unambiguously. which() drops the NA comparisons for us.
  conflict_rows <- which((is_rep & leans_dem) | (is_dem & leans_rep))
  if (length(conflict_rows) > 0L) {
    cli::cli_abort(c(
      "Contradictory party classifications detected.",
      "x" = "{cli::qty(length(conflict_rows))}Row{?s} {conflict_rows}: {.arg party_id} and {.arg party_lean} point to different parties.",
      "i" = "Fix these rows in the source data or supply cleaned variables."
    ))
  }

  rollup <- dplyr::case_when(
    is_rep & is_strong ~ 1,
    is_rep & is_not_strong ~ 2,
    !is_rep & !is_dem & leans_rep ~ 3,
    !is_rep & !is_dem & !leans_rep & !leans_dem ~ 4,
    !is_rep & !is_dem & leans_dem ~ 5,
    is_dem & is_not_strong ~ 6,
    is_dem & is_strong ~ 7,
    .default = NA_real_
  )

  rollup <- haven::labelled(
    rollup,
    labels = c(
      "Strong Republican" = 1,
      "Not so strong Republican" = 2,
      "Lean Republican" = 3,
      "Independent" = 4,
      "Lean Democrat" = 5,
      "Not so strong Democrat" = 6,
      "Strong Democrat" = 7
    )
  )

  new_name_str <- rlang::as_name(rlang::enquo(new_name))
  df |>
    dplyr::mutate("{new_name_str}" := rollup)
}


# private functions -------------------------------------------------------
### Convert a column to character text for matching, reading value labels when
### the variable is haven_labelled so numeric survey codes match on their text.
labels_to_text <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return(as.character(haven::as_factor(x)))
  }
  as.character(x)
}


### Case-insensitive, NA-preserving string detection.
detect_ci <- function(x, pattern) {
  stringr::str_detect(x, stringr::regex(pattern, ignore_case = TRUE))
}


### Rank columns of a data frame by how well they match a target name, using
### Levenshtein string similarity plus a keyword fallback. Used both to
### auto-detect variables and to suggest alternatives in error messages.
col_match <- function(target, col_names, keywords = NULL, exclude = NULL) {
  if (length(col_names) == 0L) {
    return(character(0))
  }

  candidates <- col_names
  if (!is.null(exclude)) {
    candidates <- candidates[
      !grepl(exclude, candidates, ignore.case = TRUE)
    ]
  }
  if (length(candidates) == 0L) {
    return(character(0))
  }

  distances <- utils::adist(target, candidates, ignore.case = TRUE)[1, ]
  names(distances) <- candidates

  # Allow edits up to ~35% of the target length; cap short names at 1 edit to
  # avoid spurious matches.
  threshold <- if (nchar(target) <= 3L) {
    1L
  } else {
    max(2L, floor(nchar(target) * 0.35))
  }
  fuzzy_hits <- candidates[distances <= threshold]
  if (length(fuzzy_hits) > 0L) {
    fuzzy_hits <- fuzzy_hits[order(distances[fuzzy_hits], -nchar(fuzzy_hits))]
  }

  keyword_only <- if (!is.null(keywords)) {
    kw_hits <- candidates[grepl(keywords, candidates, ignore.case = TRUE)]
    kw_only <- setdiff(kw_hits, fuzzy_hits)
    if (length(kw_only) > 0L) kw_only[order(-nchar(kw_only))] else character(0)
  } else {
    character(0)
  }

  c(fuzzy_hits, keyword_only)
}


### Resolve one variable argument to a column name string. When the quosure is
### NULL, auto-detect via col_match (erroring on zero or multiple matches);
### otherwise verify the supplied column exists, suggesting alternatives if not.
resolve_party_var <- function(
  df,
  var_quo,
  target,
  keywords,
  exclude,
  arg_name
) {
  col_names <- names(df)

  if (rlang::quo_is_null(var_quo)) {
    matches <- col_match(target, col_names, keywords = keywords, exclude = exclude)

    if (length(matches) == 0L) {
      cli::cli_abort(c(
        "Could not find a {.arg {arg_name}} variable in {.arg df}.",
        "i" = "Specify the column directly with {.arg {arg_name}}."
      ))
    }
    if (length(matches) > 1L) {
      cli::cli_abort(c(
        "Found multiple possible {.arg {arg_name}} variables: {.val {matches}}.",
        "i" = "Specify which to use with {.arg {arg_name}}."
      ))
    }
    return(matches)
  }

  supplied <- rlang::as_name(var_quo)
  if (!supplied %in% col_names) {
    hints <- col_match(supplied, col_names, keywords = keywords)
    msg <- c("Column {.val {supplied}} not found in {.arg df}.")
    if (length(hints) > 0L) {
      msg <- c(msg, "i" = "Did you mean {.val {hints}}?")
    }
    cli::cli_abort(msg)
  }
  supplied
}

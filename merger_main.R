
library(tidyverse)
library(stringdist)
library(diffobj)
library(haven)
library(labelled)
library(moments)
library(magrittr)

do_if <- function(.cond, .if, .else = ...) {
  if (.cond) {
    .if
  } else if (!is.null(.else)) {
    .else
  }
}

null_to_na <- function(.x) {
  if (!length(.x)) {
    .x <- NA
  }
  .x
}

has_names <- function(x) length(names(x)) != 0

cbind_unequal_rows <- function(.l, .alternate = FALSE) {
  map_int(.l, ~ nrow(.)) %>%
    max(na.rm = TRUE) %>%
    map2(.l, ~ .y[1:.x, ]) %>%
    reduce(bind_cols) %>%
    select(do_if(
      .alternate,
      map2(length(.l), max(lengths(.l)), ~ sequence(
        rep(.x, .y),
        seq(.y),
        rep(.y, .y)
        # 1:12 %>% split(., rep_len(., length(.)/2))
      )) %>% simplify(),
      everything()
    ))
}

to_str_alnum <- function(x, .lower = TRUE) {
  gsub("[^[:alnum:] ]", " ", x) %>%
    do_if(.lower, tolower(.), .) %>%
    paste0(., collapse = " ") %>%
    str_squish()
}

str_remove_squish <- compose(~ str_remove(.x, as.character(.y)) %>% str_squish())

separate_na_labels <- function(l, a) {
  if (all(is.na(l[a[5]])) | all(is.na(l[a[3:4]]))) {
    x <- l[a[4:5]]
  } else {
    x <- split(
      l[[a[5]]],
      not(or(do_if(
        all(is.na(l[[a[3]]])), rep_along(l[[a[5]]], FALSE),
        between(l[[a[5]]], min(l[[a[3]]]), max(l[[a[3]]]))
      ), l[[a[5]]] %in% l[[a[4]]]))
    )
  }
  set_names(x, map_chr(names(x), ~ str_replace_all(., c(`FALSE` = a[[4]], `TRUE` = a[[5]]))[[1]]))
}

get_attr <- function(.df, .attr = NULL, .name = NULL) {
  a <- c(`1` = "name", `2` = "label", `3` = "na_range", `4` = "na_values", `5` = "labels")
  as.list(.df) %>%
    map2(names(.), ~ c(name = .y, attributes(.x)) %$%
      set_names(.[a], a) %>%
      map(~ null_to_na(.)) %>%
      list_modify(label = str_remove_squish(.[["label"]], .[["name"]])) %>%
      modify_if(~ has_names(.), ~ set_names(., str_remove_squish(names(.), .))) %>%
      modify_if(~ is.numeric(.), ~ keep(., is.finite(.))) %>%
      c(separate_na_labels(., a)) %>%
      `[`(!duplicated(names(.), fromLast = TRUE)) %>%
      list_modify(
        values_labels = labels,
        na_labels = na_values,
        values = as.numeric(.[[a[5]]]),
        labels = do_if(has_names(.[[a[5]]]), names(.[[a[5]]]), NA_character_),
        length = length(.[[a[5]]])
      )) -> attrs
  if (!is.null(.attr)) {
    do_if(is.null(.name), substitute(.df) %>%
      str_remove("`.") %>%
      list(`==`(., ".")) %$%
      ifelse(.[[2]], "var", .[[1]]), .name) %>%
      paste0(".", c("name", .attr)) -> header
    map_df(attrs, ~ `[`(., c("name", .attr)) %>%
      map_at(., .attr, ~ to_str_alnum(.)) %>%
      flatten_df()) %>%
      mutate(
        across(where(is.character), ~ str_replace(., "^$", "NA") %>% na_if("NA")),
        across(contains("length"), as.numeric)
      ) %>%
      set_names(header) -> attrs
  }
  attrs
}

is_numeric_str <- function(..., .value = FALSE) {
  empty <- "^[NA|NaN|Inf|[-]Inf|NULL]$"
  map(list(...), ~ map(., ~ str_split(.x, " ", simplify = TRUE) %>%
    str_replace(empty, "NA") %>%
    na_if("NA") %>%
    list(
      str_detect(., "[0-9\\s]") %>%
        all(na.rm = TRUE) %>%
        replace_na(FALSE)
    )) %>%
    flatten()) %>%
    transpose() %$%
    do_if(
      and(.value, all(unlist(.[[2]]))),
      map(.[[1]], ~ as.numeric(.)), .[[2]]
    )
}

col_diff <- function(.df, .a, .b, .method = "cosine") {
  .df %$%
    map2(
      .[[.a]], .[[.b]],
      ~ do_if(
        and(is.numeric(.x), is.numeric(.y)),
        abs(diff(.x - .y)),
        is_numeric_str(.x, .y, .value = TRUE) %$% do_if(
          is.numeric(unlist(.)),
          map_if(., ~ all(is.na(.)), ~0, .else = ~ remove_na(.)) %>%
            transpose() %>%
            map_depth(2, ~ ifelse(is.null(.), 0, .)) %>%
            map_dbl(~ diff(unlist(.))) %>%
            abs() %>% mean(na.rm = TRUE),
          stringdist::stringdist(.x, .y)
        )
      )
    )
}

attr_diff <- function(.a, .b, .attr, .n = c("a", "b")) {
  map2(list(.a, .b), .n, ~ get_attr(.x, .attr, .y)) %>%
    cbind_unequal_rows(.alternate = TRUE) %$%
    map(list(.[1:2], .[3:4]), ~ cross_df(.)) %>%
    bind_cols() %>%
    mutate("{.n[1]}.{.n[2]}.{.attr}.diff" := col_diff(., 3, 4))
}

remove_na <- function(.x, .rm_user_na = TRUE) {
  if (.rm_user_na) {
    .x %<>% labelled::user_na_to_na()
  }
  as.numeric(na.omit(.x))
}

get_descr <- function(.x, .scale = TRUE, .abs = TRUE) {
  .x %<>% do_if(vec_depth(.) == 2, `[[`(., 1), .)
  if (is.numeric(.x)) {
    .x %<>% remove_na()
    if (.scale) .x %<>% scale(center = FALSE)
    if (.abs) .x %<>% abs()
    .x %<>% list(1, mean(.), stats::sd(.), moments::kurtosis(.), moments::skewness(.)) %>% tail(-1)
  } else {
    .x <- as.list(c(0, rep(NA_real_, 4)))
  }
  set_names(.x, c("numeric", "mean", "sd", "kurtosis", "skewness"))
}

descr_diff <- function(.a, .b, .n) {
  map2(
    list(.a, .b), .n,
    ~ map2_df(.x, names(.x), ~ c(name = .y, get_descr(.))) %>%
      mutate(across(.names = "{.y}.{.col}"), .keep = "none")
  ) %>%
    cbind_unequal_rows(.alternate = TRUE) %>%
    full_join(cross_df(.[1:2])) %T>%
    assign(x = ".ab", ., envir = parent.frame()) %>%
    map_lgl(~ is.numeric(.)) %>%
    which() %>%
    as.list() %>%
    split(map_lgl(., ~ mod(., 2) == 0)) %>%
    map(~ set_names(., paste(
      gsub("^.*\\.", "", names(.)),
      paste(.n, collapse = "."), "diff",
      sep = "."
    ))) %$%
    map2_df(.[[1]], .[[2]], ~ abs(.ab[[.x]] - .ab[[.y]])) %$%
    bind_cols(select(.ab, where(is.character)), .)
}

all_attr_diff <- function(.a, .b) {
  .n <- as.character(substitute(c(.a, .b)))[-1]
  c(
    list(descr_diff(.a, .b, .n)),
    map(
      list("label", "labels", "values", "length"),
      ~ attr_diff(.a, .b, ., .n)
    )
  ) %>%
    reduce(bind_cols, .name_repair = "minimal") %>%
    select(which(!duplicated(names(.))))
}

find_match <- function(.x, .y) {
  min_exclude_na <- compose(~ ifelse(all(is.na(.x)), .x, min(.x, na.rm = TRUE)))
  all_attr_diff(.x, .y) %>%
    distinct(.keep_all = TRUE) -> xy
  names(xy) %>%
    str_match("name$|diff$") %>%
    is.na() %>%
    not() %>%
    which() %>%
    extract(names(xy), .) %>%
    list(str_replace(., "diff", "min")) -> ind
  select(all_of(xy), ind[[1]]) %$%
    list(.) %>%
    list(as.list(tail(seq_along(ind[[1]]), -2))) %$%
    map2(
      .[[1]], .[[2]],
      ~ group_by(.x, .x[1]) %>%
        mutate(!!ind[[2]][.y[[1]]] := min_exclude_na(.data[[ind[[1]][.y]]])) %>%
        ungroup() %>%
        select(last_col())
    ) %>%
    reduce(bind_cols) %$%
    bind_cols(xy, .) %>%
    set_names(names(.) %>%
      str_remove(".x..y.|^\\."))
}

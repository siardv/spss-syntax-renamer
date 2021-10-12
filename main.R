
invisible(sapply(c("tidyverse", "magrittr", "rlang", "diffobj", "labelled", "levitate", "crayon"), library, character.only = TRUE))

`%@%` <- rlang::`%@%`

options(digits = 4)
options(pillar.sigfig = 4)
options(scipen = 99)

compare_attrs <- function(.x, .y, sentence_case = TRUE) {
  list(.x, .y) %>%
    map(~ paste0(., ": ", names(.) %>%
      {
        if (sentence_case) str_to_sentence(.)
      })) %>%
    {
      diffChr(
        target = .[[1]],
        current = .[[2]],
        mode = "sidebyside",
        context = "auto",
        word.diff = TRUE,
        ignore.white.space = TRUE,
        style = StyleAnsi8NeutralRgb()
      )
    } %>%
    tail(-2)
}

str_to_vec <- function(.s, .sep = ", ", remove.na = FALSE) {
  str_detect(.s, paste0("^[0-9]+$|", .sep)) %>%
    when(., all(., na.rm = TRUE) ~ str_split(.s, .sep) %>%
      map(~ as.numeric(.) %>%
        when(., remove.na ~ remove_na(.), .)), .s)
}

df2[1:5, 1:5] %>% map(~chr_to_int(.))

null_to_na <- function(.x) {
  if (!length(.x) | is.null(.x)) {
    .x <- NA
  } else if (is.character(.x)) {
    if (nchar(.x) == 0) {
      .x <- NA
    }
  }
  .x
}

has_names <- function(.x, .value = FALSE) {
  .n <- length(names(.x)) > 0
  if (!.value) {
    .n
  } else if (.n) {
    .x %<>% names()
  } else {
    NA
  }
}

cbind_unequal_rows <- function(.l, .alternate = FALSE) {
  map_int(.l, ~ nrow(.)) %>%
    max(na.rm = TRUE) %>%
    map2(.l, ~ .y[1:.x, ]) %>%
    reduce(bind_cols) -> .df
  if (.alternate) {
    .df %<>%
      select(map2(length(.l), max(lengths(.l)), ~ sequence(
        rep(.x, .y),
        seq(.y),
        rep(.y, .y)
      ))) %>% simplify()
  }
  .df
}

str_remove_squish <- compose(~ str_remove(.x, as.character(.y)) %>% str_squish())

remove_na <- function(.x, .rm_user_na = TRUE) {
  if (.rm_user_na) {
    .x %<>% labelled::user_na_to_na()
  }
  as.vector(na.omit(.x))
}

parent_pipe_name <- compose(~ rlang::as_name(sys.calls()[[1]][[2]]))

str_alnum <- function(.x, .lower = TRUE, .str = FALSE) {
    gsub("[^[:alnum:] ]", " ", .x) %>%
    when(., .lower ~ tolower(.), .) %>%
    when(., .str ~ paste0(., collapse = " "), .) %>%
    str_squish()
}

clean_attrs <- function(.df) {
  map2(.df, names(.df), ~ map2(attributes(.x), .y, ~ when(
    .x,
    is.character(.) ~ str_remove_squish(., .y) %>% str_alnum(),
    has_names(.) ~ set_names(., str_remove_squish(str_alnum(names(.)), .)),
    ~.x
  )))
}

user_na <- function(.x) {
  labs <- .x %@% labels
  c(
    .x %@% na_range %>%
      when(
        ., is.null(.) ~ vector(),
        ~ range(., na.rm = TRUE, finite = FALSE) %>%
          when(., !is_empty(.) ~ labs[labs >= .[1] & labs <= .[2]], vector())
      ),
    .x %@% na_values %>%
      when(
        ., is.null(.) ~ vector(),
        ~ labs[labs %in% .]
      )
  ) %>% null_to_na()
}

all_user_na <- function(.df, .list = FALSE) {
  map(.df, ~ user_na(.) %>%
    cbind()) %>%
    unique() %>%
    reduce(rbind) %>%
    data.frame(rownames(.), row.names = NULL) %>%
    set_names(c("na_values", "na_labels")) %>%
    distinct() %>%
    rowwise() %>%
    mutate(na_labels = str_remove_squish(na_labels, na_values) %>%
      tolower()) %>%
    ungroup() %>%
    drop_na() %>%
    arrange(-desc(na_values)) %>%
    when(., .list ~ set_names(.[[1]], .[[2]]), .)
}

get_attrs <- function(.df, .attr = NULL, .str = FALSE) {
  df_na <- all_user_na(.df, .list = TRUE)
  clean_attrs(.df) %>%
    map2(names(.df), ~ discard(., str_detect(names(.), "class|format|note|width")) %>%
      list_modify(
        na_labels = .$labels %>% keep(. %in% df_na),
        labels = .$labels %>% discard(. %in% df_na)
      ) %>%
      list_modify(
        name = .y,
        values = as.numeric(.$labels),
        labels = names(.$labels),
        na_values = as.numeric(.$na_labels),
        na_labels = names(.$na_labels),
        length = length(.$labels),
        mode = mode(.df[[.y]]),
        mean = remove_na(.df[[.y]]) %>% {ifelse(is.null(.), NA, mean(.))},
        sd = remove_na(.df[[.y]]) %>% {ifelse(is.null(.), NA, mean(.))}
      ))
}

rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  return(rbind(input1[, column.names], input2[, column.names]))
}

attrs_to_df <- function(.df) {
  get_attrs(.df) %>%
    map_depth(2, ~ toString(.) %>%
      type.convert()) %>%
    map(~ bind_cols(.)) %>%
    reduce(rbind.match.columns)# %>%
   # set_names(paste0(parent_pipe_name(.), ".", names(.)))
}

select(!na_range) %>%
  select(name, mode, label, labels, values, length, everything())

pair_df <- function(.a, .b) {
  .ab <- list(a.name = names(.a), b.name = names(.b)) %>% cross_df()
  list(.a, .b) %>%
    map2(1:2, ~ attrs_to_df(.) %>%
      set_names(paste0(letters[.y], ".", names(.))) %>%
      list(.ab[, .y]) %>%
      reduce(full_join)) %>%
    reduce(cbind) %>%
    distinct(.keep_all = TRUE)
}

str_to_vec <- function(.x, .sep = ", ") {
  str_detect(.x, paste0("^[0-9]+$|", .sep)) %>%
    when(., all(., na.rm = TRUE) ~ str_split(.x, .sep) %>% map(~ as.numeric(.)), .x)
}

is_numeric_string <- function(.x, .sep = ", ") {
  remove_na(.x) %>%
    as_vector() %>%
    str_detect(paste0("^[0-9", .sep, "]+$")) %>%
    all(na.rm = TRUE)
}

num_str_diff <- function(.df, .cols) {
  .df[c(.cols)] %>%
    map_depth(2, ~ is_numeric_string(.) %>%
      when(., . ~ suppressWarnings(eval(parse(text = paste0("c(", .x, ")")))), NA_real_)) %>%
    transpose() %>%
    map_df(~ reduce(., setdiff) %>%
      when(., !length(.) ~ NA, sum(.)) %>%
      set_names(paste0(names(.df[, .cols]), collapse = ".")))
}

alternate_cols <- function(.df, .n) {
  n <- ncol(.df) / .n
  seq(1, ncol(.df), n) %>%
    map(~ seq(., . + n - 1)) %>%
    transpose() %>%
    unlist() %>%
    select(.data = .df)
}

attr_diff <- function(.a, .b) {
  .ab <- pair_df(.a, .b)
  .ab %>%
    names() %>%
    sort() %>%
    split(rep_len(., length(.) / 2)) %>%
    map(., ~ select(.ab, .) %>%
      when(
        .,
        all(map_lgl(., ~ is.numeric(.))) ~ abs(.[1] - .[2]),
        is_numeric_string(.) ~ num_str_diff(., 1:2) %>% set_names(.x),
        all(map_lgl(., ~ is.character(.))) ~ map2_df(.[[1]], .[[2]], ~ levitate::lev_partial_ratio(.x, .y)),
        NA
      )) %>%
    reduce(cbind) %>%
    set_names(str_replace(names(.), "a.", "ab.")) %>%
    list(tibble(.ab))# %>% reduce(bind_cols)
}

attr_diff(.a, .b) %>%
alternate_cols(.n = 3) %>%
  select(matches("name"), everything()) %>%
  group_by(.[1]) %>%
  filter(ab.label == max(ab.label))

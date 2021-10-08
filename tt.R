

invisible(sapply(c("tidyverse", "stringdist", "diffobj", "haven", "labelled", "moments", "magrittr"), library, character.only = TRUE))

path <- "/Users/siard/Library/Mobile Documents/com~apple~CloudDocs/Documents/RU/SKON/Data/waves/"
y1971 <- haven::read_spss(paste0(path, "w1971.sav"), user_na = TRUE)
y2006 <- haven::read_spss(paste0(path, "w2006.sav"), user_na = TRUE)
y2010 <- haven::read_spss(paste0(path, "w2010.sav"), user_na = TRUE)
y2012 <- haven::read_spss(paste0(path, "w2012.sav"), user_na = TRUE)
y2017 <- haven::read_spss(paste0(path, "w2017.sav"), user_na = TRUE)
y2021 <- haven::read_spss(paste0(path, "w2021.sav"), user_na = TRUE)


do_when <- function(.x, .cond, .if, .else = NULL) {
  if (.cond) {
    .if
  } else if (!is.null(.else)) {
    .else
  } else {
    .x
  }
}
take_out <- function(.x, ...) {
  list2(...)
}

use <- funcion(.x, ...) {
  .x %>% {
    list2(...)
  }
}

do_if <- function(.cond, .if, .else = NULL) {
  if (.cond) {
    .if
  } else if (!is.null(.else)) {
    .else
  } else {
    .x
  }
}

null_to_na <- function(.x) {
  if (!length(.x) | is.null(.x)) {
    .x <- NA
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
    .x
  }
}

str_alnum <- function(x, .lower = TRUE) {
  gsub("[^[:alnum:] ]", " ", x) %>%
    do_if(.lower, tolower(.), .) %>%
    paste0(., collapse = " ") %>%
    str_squish()
}

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
        #1:12 %>% split(., rep_len(., length(.)/2))
      )) %>% simplify(),
      everything()
    ))
}



str_remove_squish <- compose(~ str_remove(.x, as.character(.y)) %>% str_squish())

separate_na_labels <- function(l, attr.names) {
  a <- attr.names
  if (all(is.na(l[a[5]])) | all(is.na(l[a[3:4]]))) {
    x <- l[a[4:5]]
  } else {
    x <- split(
      l[[a[5]]],
      not(or(do_if(
        all(is.na(l[[a[3]]])), rep_along(l[[a[5]]], FALSE),
        between(l[[a[5]]], min(l[[a[3]]]), max(l[[a[3]]]))
      ), do_if(
        all(is.na(l[[a[4]]])),
        FALSE,
        is_in(l[[a[5]]], l[[a[4]]]) %>%
          list(any(.)) %$%
          do_if(.[[2]], .[[1]], l[[a[5]]] > l[[a[4]]])
      )))
    )
  }
  set_names(x, map_chr(names(x), ~ str_replace_all(., c(`FALSE` = a[[4]], `TRUE` = a[[5]]))[[1]]))
}

get_attr <- function(.df, .attr = NULL, .name = NULL) {
  a <- c(`1` = "name", `2` = "label", `3` = "na_range", `4` = "na_values", `5` = "labels")
  as.list(.df) %>%
    map2(names(.), ~ c(name = .y, attributes(.x)) %$% set_names(.[a], a) %>%
      map(~ null_to_na(.)) %>%
      list_modify(label = str_remove_squish(.[["label"]], .[["name"]])) %>%
      modify_if(~ has_names(.), ~ set_names(., str_remove_squish(names(.), .))) %>%
      modify_if(~ is.numeric(.), ~ discard(., is.infinite(.) & !has_names(.)))) %>%
    unname() %>%
    map(~ c(., separate_na_labels(., a)) %>%
      discard(duplicated(names(.), fromLast = TRUE)) %>%
      as.list() %>%
      update_list(
        values = as.numeric(.[[a[5]]]),
        labels = has_names(.[[a[5]]], .value = TRUE),
        na_values = as.numeric(.[[a[4]]]),
        na_labels = has_names(.[[a[4]]], .value = TRUE),
        length = length(.[[a[5]]])
      )) -> attrs
  if (!is.null(.attr)) {
    do_if(is.null(.name), substitute(.df) %>%
      str_remove("`.") %>%
      list(`==`(., ".")) %$%
      ifelse(.[[2]], "var", .[[1]]), .name) %>%
      paste0(".", c("name", .attr)) -> header
    map_df(attrs, ~ `[`(., c("name", .attr)) %>%
      map_at(., .attr, ~ str_alnum(.)) %>%
      flatten_df()) %>%
      mutate(
        across(where(is.character), ~ str_replace(., "^$", "NA") %>%
          na_if("NA")),
        across(contains("length"), as.numeric)
      ) %>%
      set_names(header) -> attrs
  }
  attrs
}

numeric_vec_col <- function(..., .n = ) {
  empty <- "^[NA|NaN|Inf|\\-Inf|NULL]$"
  map2_dfc(as.list(...), names(...), ~ str_remove_all(., paste0(empty, "|\\s+")) %>%
    str_detect("^[0-9]+$") %>%
    all(na.rm = TRUE) %>%
    do_if(str_split(.x, " ") %>%
      map_df(~ str_replace_all(.x, empty, "NA") %>%
        na_if("NA") %>%
        as.numeric() %>%
        set_names(seq_along(.))), bind_cols(.x))
    %>%
    set_names(., make.unique(rep_along(., .y))))
}
  
#   empty <- "^[NA|NaN|Inf|\\-Inf|NULL]$"
#   map(list(...), ~ map(., ~ str_split(.x, " ", simplify = TRUE) %>%
#     str_replace(empty, "NA") %>%
#     na_if("NA") %>%
#     list(
#       str_detect(., "[0-9\\s]") %>%
#         all(na.rm = TRUE) %>%
#         replace_na(FALSE)
#     )) %>%
#     flatten()) %>%
#     transpose() %$%
#     do_if(
#       and(.value, all(unlist(.[[2]]))),
#       map(.[[1]], ~ as.numeric(.)), .[[2]]
#     )
# }

pair_df <- function(.x, .y, .attr) {
  substitute(.x)
  map(list(.x, .y), ~ get_attr(., .attr)) %>%
    cbind_unequal_rows(.alternate = TRUE) %$%
    map2(list(.[1:2], .[3:4]), c("name", .attr), ~ cross_df(.) %>%
      set_names(paste0(c("x", "y"), ".", .y))) %>%
    bind_cols()
}

col_dist <- function(.df, .a, .b, .method = "cosine") {
  .df %$%
    map2(
      .[[.a]], .[[.b]],
      ~ do_if(
        and(is.numeric(.x), is.numeric(.y)),
        abs(diff(.x - .y)),
        is_numeric_col(.x, .y, .value = TRUE) %$% do_if(
          is.numeric(unlist(.)),
          map_if(., ~ all(is.na(.)), ~0, .else = ~ remove_na(.)) %>%
            transpose() %>%
            map_depth(2, ~ ifelse(is.null(.), 0, .)) %>%
            map_dbl(~ diff(unlist(.))) %>%
            abs() %>% mean(na.rm = TRUE),
          stringdist::stringdist(.x, .y, method = .method)
        )
      )
    )
}

attr_dist <- function(.a, .b, .attr, .n = c("a", "b")) {
  map2(list(.a, .b), .n, ~ get_attr(.x, .attr, .y)) %>%
    cbind_unequal_rows(.alternate = TRUE) %$%
    map(list(.[1:2], .[3:4]), ~ cross_df(.)) %>%
    bind_cols() %>%
    mutate("{.n[1]}.{.n[2]}.{.attr}.diff" := col_dist(., 3, 4))
}

remove_na <- function(.x, .rm_user_na = TRUE, .mode) {
  if(.rm_user_na){
    .x %<>% labelled::user_na_to_na()
  }
  as.vector(na.omit(.x), mode = .mode)
}

parent_pipe_name <- compose(~ rlang::as_name(sys.calls()[[1]][[2]]))

descr_stats <- function(.df, .cols = NULL, .scale = TRUE, .abs = TRUE) {
  do_if(is.null(.cols), .df, .df[, .cols]) %>%
    map2(names(.), ~ {
      if (is.numeric(.)) {
        remove_na(.) %>%
          do_if(.scale, scale(., center = FALSE), .) %>% {
          c(1, mean(.), sd(.), kurtosis(.), skewness(.))}
      } else { c(0, rep(NA, 4)) }
    } %>%
      map(~ as.numeric(.) %>%
            ifelse(.abs, abs(.), .)) %>%
      c(.y, .) %>%
      set_names(paste0(
        parent_pipe_name(), ".",
        c("name", "numeric", "mean", "sd", "kurtosis", "skewness")
      )) %>%
      as_tibble_row()) %>%
    reduce(bind_rows)
}

descr_dist <- function(.a, .b, .n = c("a", "b")) {
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

all_attr_dist <- function(.a, .b) {
  .n <- as.character(substitute(c(.a, .b)))[-1]
  c(
    list(descr_dist(.a, .b, .n)),
    map(
      list("label", "labels", "values", "length"),
      ~ attr_dist(.a, .b, ., .n)
    )
  ) %>%
    reduce(bind_cols, .name_repair = "minimal") %>%
    select(which(!duplicated(names(.))))
}

start <- Sys.time()
df_all <- all_attr_dist(x1971, x2006)
end <- Sys.time()
difftime(end, start)

find_match <- function(.x, .y) {
  min_skip_na <- compose(~ ifelse(all(is.na(.x)), .x, min(.x, na.rm = TRUE)))
  all_attr_dist(.x, .y) %>%
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
        mutate(!!ind[[2]][.y[[1]]] := min_skip_na(.data[[ind[[1]][.y]]])) %>%
        ungroup() %>%
        select(last_col())
    ) %>%
    reduce(bind_cols) %$%
    bind_cols(xy, .) %>%
    set_names(names(.) %>%
      str_remove(".x..y.|^\\."))
}

start <- Sys.time()
df_all <- find_match(y1971, y2006)
end <- Sys.time()
difftime(end, start)
  
df_all %>% names() %>% str_remove(".x..y.|^\\.")
df_all %>%
  group_by(x.name) %>%
  filter(!is.na(x.name)) %>%
  filter(!is.na(x.label) & label.diff == label.min) %>%
  filter(!is.na(x.labels) & labels.diff == labels.min) %>%
  filter(!is.na(x.length) & length.diff == length.min) %>%
  filter(!is.na(x.values) & values.diff == values.min) %>%
  filter(mean.diff == mean.min) %>%
  filter(sd.diff == sd.min) %>%
  filter(skewness.diff == skewness.min) %>%
  filter(kurtosis.diff == kurtosis.min) %>%
  ungroup()



  mutate(lbs.min.dif = min(lbs.dif, na.rm = TRUE)) %>%
  filter(lbs.dif == lbs.min.dif) %>% 
  mutate(sts.sim = ifelse(is.finite(desc.sim), max(desc.sim, na.rm = TRUE), 0)) %>%
  filter(desc.sim == sts.sim) %>%
  ungroup() %>%

  set_names(get_attr(y1971, "labels"), c("x", "labels.x")) -> labs.x
  set_names(get_attr(y2006, "labels"), c("y", "labels.y")) -> labs.y
  set_names(get_attr(y1971, "length"), c("x", "length.x")) -> len.x
  set_names(get_attr(y2006, "length"), c("y", "length.y")) -> len.y
  set_names(get_attr(y1971, "labels"), c("x", "labels.x")) -> labs.x
  set_names(get_attr(y2006, "labels"), c("y", "labels.y")) -> labs.y
  reduce(list(tt5, labs.x, labs.y), full_join) -> tt6
  tt8 %>% distinct(.keep_all = TRUE) %>%
    mutate(mean_xy = mean.xy == min_skip_na(mean.xy),
           sd_xy = sd.xy == min_skip_na(sd.xy),
           kurt_xy = kurtosis.xy == min_skip_na(kurtosis.xy),
           skew_xy = skewness.xy == min_skip_na(skewness.xy),
           labs_xy = stringdist(labels.x, labels.y, "cosine"),
           lab_xy = stringdist(label.x, label.y, "cosine")) %>% group_by(x) %>%
    mutate(min_lab = lab_xy == min_skip_na(lab_xy),
           min_labs = labs_xy == min_skip_na(labs_xy)) %>% select(-c(numeric.x:skewness.y)) %>% filter(numeric.xy == 0) %>% filter(min_lab == TRUE) %>% summary(mean)
  
  
tt %>% mutate(strsim = str_sim(., 3, 4))
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

shift_match <- function(x, y, method = "right") {
  if (method == "left") {
    y <- rev(y)
  }
  y[[2]][match(x, y[[1]])]
}
str_alnum <- function(x, trim = TRUE, lower = TRUE, null = FALSE, string = FALSE) {
  gsub("[^[:alnum:] ]", " ", x) %>%
    do_if(trim, str_squish(.), .) %>%
    do_if(lower, tolower(.), .) %>%
    do_if(!null, null_to_na(.), .) %>%
    do_if(string, toString(str_squish(.)), .)
}



# 
# attrs2_df <- function(.x, .y, .attr, .names = FALSE, .length = FALSE) {
#   map(as.list(match.call())[2:3],~paste(.x, c("name", .attr), sep = ".") %>% c(case_when(
#     num(.length) == 1 ~ ".l",
#     num(.names) == 1 ~ ".n",
#     TRUE ~ "")))# %>% transpose()
# }

# map(as.list(match.call())[2:3], ~ paste0(.x, ".", c("name", paste0(.attr, 
# case_when(
#   .return == "values" ~ length(.),
#   .return == "names" ~ ".names",
#   .return == "length" ~ ".names",
#   TRUE ~ .)
# ))))) %>% flatten_chr() -> tbl_names

#.a=y1971; .b=y2010; .attr="labels"; .names = FALSE; .length = FALSE

# attrs2_df <- function(.a, .b, .attr, .return = c("names", "values", "length"), .to.string = TRUE) {
#   ab_names <- as.list(match.call())[2:3] %>%
#     map(~ paste0(., ".", c(gsub("[.]$", "", paste0(.attr, ".", .return)), "name")))
# 
#   return_is <- compose(~ equals(paste0(.return[1], ""), ...))
#   to_string <- compose(~ do_if(.to.string, toString(...), ...))
# 
#   list(.a, .b) %>%
#     map2(ab_names, ~ attrs(.x, .attr) %>%
#       map_df(~ do_if(
#         has_names(.) & return_is("names"),
#         names(.) %>% str_alnum() %>% to_string(),
#         do_if(
#           return_is("values"), unname(.) %>% to_string(),
#           do_if(return_is("length"), length(.), .)
#         )
#       )) %>%
#       pivot_longer(everything(), values_to = .y[1], .y[2])) %>%
#     cbind_unequal_rows()


attrs2_df <- function(.a, .b, .attr, .return = c("names", "values", "length"), .to.string = TRUE) {
  as.list(match.call())[2:3] %>%
    map(~ paste0(., ".", "name", c(gsub("[.]$", "", paste0(.attr, "." , .return))))) -> ab_names
  
  return_is <- compose(~ equals(paste0(.return[1], ""), ...))
  to_string <- compose(~ do_if(.to.string, toString(...), ...))
  
  list(.a, .b) %>%
    map2(ab_names, ~ attrs(.x, .attr) %>%
      map_df(~ do_if(
        has_names(.) & return_is("names"),
        names(.) %>% str_alnum() %>% to_string(),
        do_if(
          return_is("values"), unname(.) %>% to_string(),
          do_if(return_is("length"), length(.), .)
        )
      )) %>%
      pivot_longer(everything(), values_to = .y[1], .y[2])) %>%
    cbind_unequal_rows()
}

get_a <- compose(~ attrs2_df(.df1, .df2, ...))
map2(c("label", rep("labels",3)), c("", "names", "values", "length"), ~get_a(.x, .y))


sQuote(list("ok", "okee"), "") %>%
  toString(.) %>%
  paste0("c(", .,")") %>%
  parse(text = .) %>%
  eval()

is_blank <- function(x, n = 1, invert = FALSE) {
  strsplit(as.character(null_to_na(x)), "")[[1]] %>%
    grepl("[A-z]", .) %>%
    length() %>%
    is_greater_than(n) %>%
    ifelse(invert, ., not(.))
}


na_lenght <- function(.x, .i = 0, invert = FALSE){
  length(which(is.na(.x)))
}

# stats_sim <- function(.df, .id = c("x.", "y.")) {
#   map(.id, ~ paste0(., "(me|sd|ku|sk)") %>%
#     grep(names(.df))) -> .cols
#   map(.cols, ~ select(.df, .x) %>%
#     split(sort(as.numeric(rownames(.))))) %>%
#     transpose() -> .l
#   map_depth(.l, 2, ~ is.na(.) %>%
#     which() %>%
#     length()) %>%
#     unlist() %>%
#     diff() %>%
#     equals(0) %>%
#     map2_df(., list(.l), ~ do_if(., map(.y, ~ unlist(.) %>%
#       replace_na(0) %>%
#       scale(center = FALSE)), 0)) %>%
#     map(~ subtract(.[1:4], .[5:8]) %>%
#       scale(center = FALSE) %>%
#       abs() %>%
#       sum() %>%
#       divide_by(4))
# }

# stand <- function(.x){
#   scale(.x, center = FALSE, scale = TRUE) %>%
#     abs() %>%
#     as.data.frame() %>%
#     tibble()
# }


# subtract_pair <- function(.ind){
#   split(i, cut(., 2))
# }

# rowsum_rel <- function(.x) {
#   abs(rowSums(.x, na.rm = TRUE) / rowSums(!is.na(.x)))
# }

# get_mode <- function(.x) {
#   ifelse(is.na(.x), "", mode(unclass(.x)))
# }

has_names <- function(x, ) {
  length(names(x)) != 0
}

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
      )) %>% simplify(),
      everything()
    ))
}

# get_match <- function(.x, .table, .df, .cols) {
#   .df[match(.x, .table), .cols]
# }

# str_sim <- function(.data, .a, .b, .method = "cosine", ...) {
#   map(c(.a, .b), ~ .data[[.x]] %>% replace_na("")) %$%
#     stringdist::stringsim(.[[1]], .[[2]], method = .method, ...) %>%
#     replace_na(0)
# }


  
  df_all[[1,'x.values']] %>%
    str_split(x, " ", simplify = TRUE) %>%
    str_replace(not_num, "NA") %>%
    na_if("NA") %>% do_if(
      all(str_detect(.[[1]], paste0("[0-9\\s]|", not_num))), as.numeric(.), .)
  
  str_split(x, "", simplify = TRUE) %>%
    str_detect(paste0("[0-9\\s]|", not_num)) %>%
    all() %>%
    ifelse(.,
      ifelse(is.null(replace.na), .,
        gsub(not_num, replace.na, .) %>%
          type.convert(as.is = TRUE)
      ), x
    )
}

#if_na <- function(check.na, ...) is.na(check.na) %>% do_if(., ...) # wrapper around do_if() which is a pipe friendly equivalent of %>% {if(){}else{}} %>%

#str_split_len <- compose(~ list(...) %>% pmap_dbl(~ do_if(is.na(.), list(vector()), str_split(., ", ")) %>% lengths()))


#.df1=y1971; .df2=y2010; .method = "cosine"; .keep_all = FALSE

match_attrs <- function(.df1, .df2, .method = "cosine", .keep_all = FALSE) {
  map_int(list(.df1, .df2), ~ names(.) %>%
    anyDuplicated()) %>%
    sum() %>%
    equals(0) %>%
    not() %>%
    do_if(stop("Column names must be unqiue.", call. = FALSE))


  list(x = .df1, y = .df2) %>%
    map2(names(.), ~ map2_df(., names(.), ~ c(name = .y, mode = mode(.), descr(.))) %>%
           mutate(across(names(.), ~do_if(is.numeric(.), abs(scale(., center=FALSE)[,1]), .),
                         .names = "{.y}.{.col}"), .keep="none")) %>%
    cbind_unequal_rows()
  
  # list(x = .df1, y = .df2) %>%
  #   map2(names(.), ~ map2_df(., names(.), ~ c(name = .y, mode = mode(.), descr(.))) %>%
  #          mutate(across(.names = "{.y}.{.col}"), .keep = "none"))
  # 
  # map(c("y.", "x."), ~ select(df.descr, matches(.) & where(is.double)) %>%
  #   rowwise() %>%
  #   stand()) %>%
  #   c(list(select(df.descr, matches("name|mode")))) %>%
  #   rev() %>%
  #   reduce(bind_cols) %>%
  #   select(matches(c("name", "x")), everything()) -> cd

  get_a <- compose(~ attrs2_df(.df1, .df2, ...))
  map2(c("label", rep("labels",3)), c("", "names", "values", "length"), ~get_a(.x, .y))
  
  list(
    get_a("label"),
    get_a("labels"),
    get_a("labels", .use.names = TRUE),
    get_a("labels", .use.values = TRUE) %>%
      bind_cols(
        select(., matches(".values")) %>%
          mutate(
            x.len = .[[1]] %>% str_split_len(),
            y.len = .[[2]] %>% str_split_len(),
            len.diff = abs(x.len - y.len)
          ),
        .name_repair = "minimal"
      )
  ) %>%
    reduce(bind_cols, .name_repair = "minimal") %>%
    select(which(!duplicated(names(.)))) %>%
    select(matches(c("name", "df"), everything())) %>%
    set_names(c(names(cd)[c(1:2)], names(.)[-c(1:2)])) %>%
    full_join(cd) -> ef

  grep("x.|df1", names(ef)) %>% list(setdiff(seq_along(ef), .)) -> .cols
  select(cd, contains("name")) %>%
    cross_df() %>%
    bind_cols() %>%
    distinct(.keep_all = TRUE) %>%
    bind_cols(as.list(.) %>%
      map2(1:2, ~ get_match(.x, ef[[.y]], ef, .cols[[.y]][-1])) %>%
      reduce(bind_cols)) %>%
    set_names(names(.) %>%
      str_remove_all("^[.]|[.]{2,}|\\d$") %>%
      str_replace_all(c("df1" = "x", "df2" = "y"))) -> gh
  
  desc_names <- "mean|sd|kurt|skew"
  map(c("x.", "y."), ~
  select(gh, matches(desc_names) & contains(.x)) %>%
    mutate(nna = rowSums(is.na(.)))) %>%
    reduce(bind_cols) %>%
    select(contains("nna")) %>%
    set_names(c("x.nna", "y.nna")) %>%
    bind_cols(gh) %>%
    list(select(., matches(desc_names)) %$%
      tibble(.[1:4], .[5:8]) %>%
      map_df(~ scale(., center = FALSE) %>%
        abs()) %>%
      mutate(desc.sim = rowsum_rel(.)) %>%
      select(last_col())) %>%
    reduce(bind_cols) -> ij
  
  select(ij, matches(c(".label$", ".names", ".values"))) %>%
    mutate(lab.sim = str_sim(., 1, 2)) %>%
    mutate(across(3:4, ~ type.convert(., as.is = FALSE, na.strings = "^[NA|NaN|Inf|[-]Inf|NULL]$"))) %>%
    mutate(across(where(is.factor), ~ as.numeric(.))) %>%
    mutate(lbs.dif = row_diff(.[3:4])) %>%
    mutate(lbs.sim = ifelse(lbs.dif == 0, str_sim(., 5, 6), NA)) %>%
    select(matches("sim|dif")) %>%
    bind_cols(ij)
}

chr_to_num <- function(.x, .col){
  .x[[.col]] %>%
    str_split(" ", simplify = TRUE) %>%
    as.numeric()
}

row_diff <- function(.cols, .abs = TRUE) {
  .cols %>%
    array_branch(1) %>%
    map_dbl(~
    replace_na(., 0) %>%
      diff()) -> d
  if (.abs) {
    abs(d) -> d
  }
  return(d)
}

attrs2_df <- function(.a, .b, .attr, .return = "names") {
  as.character(match.call())[2:3] %>%
    merge(c("name", paste0(.attr, ".", .return))) %>%
    reduce(paste, sep = ".") -> tbl_names
  
  map(list(.a, .b), ~ attrs(., .attr) %>%
        map_if(~ and(has_names(.), equals(.return, "names")),
               names(.) %>% str_alnum() %>% toString(),
               .else = do_if(equals(.return == "values"), toString(),
                             do_if(equals(.return, "length"), length(), .))))
  map2_df(names(.), ~ bind_cols(name = .y) %>%
            mutate(attr = .x)) %>%
  cbind_unequal_rows() %>%
  set_names(tbl_names)
}
attrs2_df(y1971, y2010, "labels")


match_attrs(y1971, y2010) -> tt
select(tt, matches(c("name", "sim", "dif", "label$", "^..labels$"))) %>%
  group_by(x.name) %>%
  mutate(lab.max.sim = max(lab.sim, na.rm = TRUE)) %>%
  filter(lab.sim == lab.max.sim) %>%
  mutate(lbs.min.dif = min(lbs.dif, na.rm = TRUE)) %>%
  filter(lbs.dif == lbs.min.dif) %>% 
  mutate(sts.sim = ifelse(is.finite(desc.sim), max(desc.sim, na.rm = TRUE), 0)) %>%
  filter(desc.sim == sts.sim) %>%
  ungroup() %>%
  #select(matches(c("name","label$","lab.s","lbs.d","sts"))) 


n_diff <- function(.data, .cols) {
  map2(.cols, 1:2, ~replace_na(pluck(.data, .x, .y), 0)) %>%
    diff() %>% abs()
}

str_sim <- function(.data, .a, .b, .method = "cosine", ...) {
  map(c(.a, .b), ~ .data[[.x]] %>% replace_na("")) %$%
    stringdist::stringsim(.[[1]], .[[2]], method = .method, ...) %>%
    replace_na(0)
}


str_sim_all <- function(.data, .a, .b) {
  map_lgl(c(.a, .b), ~ .data[[.x]] %>%
    is.na() %>%
    all()) %>%
    which() %>%
    length() %>%
    {
      do_if(
        equals(., 0), lmap(c(.a, .b), ~ str_split(.data[[.x]], ", ") %>%
          list(.)) %>% transpose() %>%
          map(~ cross2(.[[1]], .[[2]]) %>%
            reduce(rbind) %>%
            data.frame() %>%
            str_sim(., 1, 2) %>%
            mean()),
        ifelse(equals(., 1), 0, 1)
      ) %>% unlist()
    }
}


ab %>%
  select(matches(c(".label$", ".l.labels", ".n.labels"))) %>%
  mutate(label.sim = str_sim(., 1, 2)) %>%
  mutate(across(3:4, ~ type.convert(., as.is = FALSE, na.strings = "^[NA|NaN|Inf|[-]Inf|NULL]$"))) %>%
  mutate(across(is.factor, ~ as.numeric(.) %>% replace_na(0))) %>%
  mutate(labels.len.diff = abs_diff(., 3, 4)) %>%
  mutate(labels.str.sim = str_sim(., 5, 6))

# with_progress <- function(..x, ..f, .f, ...) {
#   .f <- add_progress(.f, length(..x))
#   ..f(..x, .f, ...)
# }

desc_sim <- function(.xy) {
  
}

pb <- progress_estimated(nrow(xy))

xy2 <- xy %>%
  map_df(~desc_sim(.))

select(xy, matches(c("mean|sd|kurt|skew", .x))) %>%
  mutate(stats = rowSums(.)) %>%
  select(last_col())

   
       bind_cols(
         map2(as.list(.), 1:2, ~ get_match(.x, xy[[.y]], xy, .cols[[.y]][-1])) %>%
           c(list(z)) %>%
           reduce(bind_cols)) -> xy
     
     
       map(c("x.", "y."), ~select(xy, matches("mean|sd|kurt|skew") & contains(.)) %>%
             rowwise() %>%
             mutate(nna = count_na(c_across())) %>%
             unnest(everything()))
       
       reduce(stats_scale) %>%
          tibble() %>%
          mutate(stats = rowSums(.)) %>%
          select(last_col())
        
          
        rowwise() %>%
          mutate(stats = stats_sim(4:7, 9:12)) %>%
          mutate(mode = ifelse(equals(x.mode, y.mode), 1, 0)) %>%
          select(x.name, y.name, mode, stats) %>%
          add_column(
            z[match(.[[1]], z[[1]]), seq(3, ncol(z), 2)],
            z[match(.[[2]], z[[2]]), seq(4, ncol(z), 2)]
          )
      }
    }
}

#

#map2(c(1, 2), c(1,7), ~xy[match(xy.name[[.x]], xy[[.y]]), ]) %$% bind_cols(.[[2]][,1:6],.[[1]][,7:12])
            mutate(mode = xy[match(x.name, xy[[1]]),  grep("mode", names(xy))] %$%
                     ifelse(equals(.[[1]], .[[2]]), 1, 0))
        
        map(list(.df1[1:5,], .df2[1:5,]), ~map2(.x, names(.x), ~ data.frame(name = .y, mode = mode(.), get_stats(.) %>% bind_cols())))
        map(list(.df1, .df2), ~map(.x, ~ data.frame(names(.), get_mode(), get_stats(.))))
        
        
        
          cbind( 
            map2(.[[1]], .[[2]], ~ list(list(.df1, .x), list(.df2, .y)) %$%
                   data.frame(equal_mode(.[[1]], .[[2]]), stats_sim(.[[1]], .[[2]])) %>%
                   set_names(c("mode", "stats"))) %>%
              bind_rows()) -> .sim1

    map(.attr, ~ attrs2_df(.l = .l, .x) %>%
      as.list() %$%
      list(.[c(1, 3)], .[c(2, 4)]) %>%
      map(~ cross_df(.)) %>%
      bind_cols()) %>%
      c(list(.sim1)) %>%
      reduce(full_join)
    
        # %>%
             # bind_rows()) %>%
         # mutate("stringsim.{.attr}" := stringsim(.[[1]], .[[2]], method = "cosine"))
        
          #group_by(.[1]) %>%
          #slice_max(stringsim, with_ties = FALSE) %>%
          #ungroup() %>%
          #mutate(id.x = shift_match(.[[1]], .df1[1:2]), id.y = shift_match(.[[2]], .df1[3:4])) -> .df2
          #select(id.x, id.y, "{.attr}.x" := .a, "{.attr}.y" := .b, "{.attr}.stringsim" := stringsim) %>%
          #distinct(id.x, .keep_all = TRUE) %>%
          #mutate(id.y = ifelse(map2_lgl(.[[3]], .[[4]], ~ is_blank(.)), NA_real_, id.y)) %>%
         # mutate(across(contains("stringsim"), ~ ifelse(is.na(id.y), NA_real_, .))) %>%
          #drop_na() -> .df2
        if (.keep_all) {
          .df2 %>%
            bind_rows(tibble(id.x = .df0[[2]], "{.attr}.x" := .df0[[1]])) %>%
            distinct(id.x, .keep_all = TRUE) %>%
            bind_rows(tibble(id.y = .df0[[4]], "{.attr}.y" := .df0[[3]])) %>%
            distinct(id.y, .keep_all = TRUE) %>%
            filter_all(any_vars(!is.na(.)))
        } else {
          .df2
        }
      } 
    }
}

full_join(match_attrs(y1971, y2010, "label"),
match_attrs(y2010, y1971, "labels")) %>% group_by(id.y) %>%
  summarise(across(everything(), ~ first(na.omit(.))))

reduce(list(df1, df2, df3), full_join, by = c("id.x", "id.y")) %>%

  mutate(stringsim = rowSums(across(contains("stringsim")), na.rm = TRUE)) -> df4



map2(c(2,4), 1:2, ~.df[[.x]] %>% na.omit() %>% unique() %>% .[is_in(., testt[[.y]]) %>% not()] %>% bind_cols(id = ., id2 = NA_character_))# %$% full_join(.[[1]],.[[2]], copy=TRUE,keep=TRUE) -> test3# %>% left_join(testt)
map2(c(2,4), 1:2, ~.df[[.x]] %>%
       na.omit() %>%
       unique() %>%
       .[is_in(., testt[[.y]]) %>% not()] %>%
       bind_cols(id = .)) %$%
  full_join(.[[1]],.[[2]], keep=TRUE) %>%
  left_join(testt)

df1 <- align_attrs(y2021[, 1:50], y2012[, 1:50], "label") %>%
  stringsim_row(c(1, 3), "cosine") %>%
  mutate(stringsim.name = ifelse(equals(id.x,id.y), 1, 0))
df2 <- align_attrs(y2021[, 1:50], y2012[, 1:50], "labels") %>%
  stringsim_row(c(1, 3), "cosine")
  
  align_attrs(y2021[, 1:50], y2012[, 1:50], "label") %>%
  stringsim_row(c(1, 3), "cosine")
  

  # df1 <- attrs_df(y2021[, 1:50], y2012[, 1:50], "name") %>%
  #   str_sim(c(1, 3), "cosine") %>%
  #   dplyr::rename(., str_sim_name = str_sim)

    
  
  
  
  #group_by(id.x, id.y) %>%
  mutate(attr_sim = rowSums(.[grep("^str_sim", names(.))], na.rm = TRUE)) %>%
    select(contains(c("id", "sim"))) %>%
    filter(duplicated(id.x, id.y) == FALSE)


list(.x = y1971[,1:10], .y = y2012[,1:11]) %>%
  imap(~attrs(., c("name","label")) %>%
         bind_rows(.) %>%
         rownames_to_column(.)) %$%
  full_join(.[[1]], .[[2]], by = "rowname")
  
  list(...) %>%
    map(~ attr_values(., "label")) %>%
    cross(.) %>%
    do.call(what = "rbind") %>%
    data.frame(.) %>%
    unnest(.) %>%
    mutate(across(everything(.), ~tolower(.)),
           str_sim = combn(., 2) %>%
             data.frame(.) %>%
             map_df(~ stringsim(.[[1]], .[[2]], method = "jw")) %>%
             rowSums(.)) %>%
    group_by(.[2]) %>%
    mutate(max_sim = max(str_sim)) %>%
    filter(str_sim == max_sim) %>%
    ungroup(.) %>%
    select(-last_col()) %>% set_names(c(..., "max"))
}



maxsim <- function(x){
  unlist(x) %>%
  equals(max(., na.rm = TRUE)) %>%
  which(.)
}

s_trim <- function(x, lowercase = TRUE){
  gsub("\\s+", " ", x) %>%
    trimws(.) %>%
    ifelse(lowercase, tolower(.), .)
}

y %>%
  map(~ pluck(., "label") %>%
        stringsim(b = pluck(x, "label"))) %>%
  unlist(.) %>%
  equals(max(., na.rm = TRUE)) %>%
  which(.)



# pick <- function(.ls, .x, .r, .f) {
#   if (all(!is.na(.ls[[.x]])) & any(!is.na(.ls[.r]))) {
#     .range <- range(unlist(.ls[.r]), na.rm = TRUE)
#     .f(.ls[[.x]], between(.ls[[.x]], .range[1], .range[2]))
#   } else if (all(is.na(.ls[.r]))) {
#     .f(.ls[[.x]], all(!is.na(.ls[.r])) %>%
#       rep(times = length(.ls[[.x]]))) %>%
#       null_to_na(.)
#   } else {
#     .ls[.x]
#   }
# }
# 
# attrs <- function(df, what = NULL) {
#   set <- c("name", "na_range", "na_values", "label", "labels")
#   as.list(set) %>% set_names(rep_along(., NA), .)
#  
#   
#   is_user_na <- compose(~ grepl("^na_", names(...)))
#   map2(names(.), ~c(name = .y, attributes(.x) %$% set_names(.[set], set) %>%
#                       list_modify(na_values = keep(., is_user_na(.)) %>% flatten_dbl()) %>%
#                       list_modifysplit(., is_greater_than(.x, 994))
#                     
#    set <- c("na_range", "na_values", "label", "labels")# %>% set_names(rep_along(., NA), .)# %>% as.list()
#   as.list(.df2[,100:105]) %>% 
#     map2(names(.), ~c(name = .y, attributes(.x) %$%
#                         set_names(.[set], set) %>%
#                         map(~null_to_na(.))))
#   split(.[["labels"]], is_greater_than(.,994)))))
#   
#   seq_along(df) %>%
#     map(~ c(name = names(df)[.], attributes(df[[.]])) %>%
#       magrittr::extract(set) %>%
#       map(~ null_to_na(.)) %>%
#       set_names(set) %>%
#       list_modify(
#         na_values = pick(., 5, 2:3, keep),
#         labels = pick(., 5, 2:3, discard),
#         label = gsub(.$name, "", .$label) %>% str_squish(.)
#       ) %>%
#       map_if(
#         ~ is.character(names(.)),
#         ~ set_names(., gsub("^\\d.*?\\W", "", names(.)) %>%
#           str_squish(.))
#       )) %>%
#     {
#       if (!is.null(what)) {
#         map_depth(., 1, what) %>% set_names(names(df))
#       } else {
#         .
#       }
#     }
# }

# zoomR <- function(){
#   resolution <- sub("^.*([0-9]{4}\\sx\\s[0-9]{4}).*$", "\\1", 
#                     system("system_profiler SPDisplaysDataType | grep Resolution", intern = TRUE))
#   
#   k <- c(reset = 29, out = 27, `in` = 24, width = 19, "command down", "control down, shift down")
#   
#   system(paste("osascript -e 'tell application \"RStudio\" to activate \n tell application \"System Events\" to tell process \"RStudio\" \n repeat", n, "times \n key code", k[1], "using {", k[5], "} \n end repeat \n end tell'"))
#   
# 
# }

# na_range <- df %>%
#   map(~ attributes(.)[c("na_values", "na_range")]) %>%
#   range(na.rm = TRUE)
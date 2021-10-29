
# 1)  Install and load packages:
#     install.packages("tidyverse")
#     install.packages("magrittr")
#
# 2)  Load remote function:
#     arrange_rename_sps <- eval(parse(text = source("https://raw.githubusercontent.com/siardv/merger/main/arrange_rename_sps.R")[1]))
#
# 3)  Add path to .sps file:
#     arrange_rename_sps("/Users/siard/Desktop/rename_syntax.sps")

arrange_rename_sps_f <- function(path) {
  require(tidyverse)
  require(magrittr)
  read_lines(path) %>%
    map_chr(~ str_replace_all(., c("=" = " = ", "\\s+" = " ", "^ " = "", "\\)\\(" = "\\) \\("))) %>%
    str_extract_all("^RENAME VARIABLES.{1,12}=.{1,}.|^COMPUTE.{1,}=") %>%
    map(~ str_remove_all(., "RENAME VARIABLES|=|\\(|\\)|\\.") %>%
      str_split(" ", simplify = TRUE) %>%
      na_if("") %>%
      rev() %>%
      {
        .[!is.na(.)]
      }) %>%
    map_depth(2, ~.) %>%
    {
      .[lengths(.) > 0]
    } %>%
    transpose() %>%
    map(~ reduce(., rbind)) %>%
    {
      suppressMessages(reduce(., bind_cols))
    } -> df_lines

  df_lines %>%
    select(2) %>%
    na_if("COMPUTE") %>%
    na.omit() %>%
    .[duplicated(.)] %>%
    unlist() -> has_dups

  if (length(has_dups) > 0) {
    message("Variables can only be renamed once. Remove duplicates: ", has_dups)
  } else {
    df_lines %>%
      set_names(c("to", "from")) %>%
      mutate(postfix = str_extract(to, "[_].*$")) %>%
      mutate(main = str_remove(to, "[_].*$")) %>%
      group_by(main) %>%
      mutate(dup = is_in(to, to[duplicated(to)])) %>%
      rowid_to_column() %>%
      mutate(key = ifelse(dup, rowid, 0)) %>%
      ungroup() %>%
      group_by(main, key) %>%
      mutate(id = cur_group_id()) %>%
      mutate(recode = paste0(sprintf("N%04d", id), ifelse(is.na(postfix), "", postfix))) %>%
      ungroup() %>%
      select(2, 3, 9) %>%
      mutate(
        pattern = ifelse(equals(from, "COMPUTE"),
          paste0("COMPUTE ", to, " = "),
          paste0("RENAME VARIABLES (", from, " = ", to, ").")
        ),
        replacement = ifelse(equals(from, "COMPUTE"),
          paste0("COMPUTE ", recode, " = "),
          paste0("RENAME VARIABLES (", from, " = ", recode, ").")
        )
      ) %>%
      select(rev(seq_along(.))[1:2]) %>%
      set_names(c("a", "b")) -> ren

    read_lines(path) %>%
      cbind.data.frame() %>%
      rowwise() %>%
      map_dfr(~ ren$a[match(., ren$b)] %>%
        {
          ifelse(is.na(.), .x, .)
        }) %>%
      flatten_chr() %>%
      write_lines("rename.txt")
    file.show("rename.txt")
  }
}



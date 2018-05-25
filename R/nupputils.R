library(tidyverse)
assuntos_filtrar <- function() {
  c(
    "Advocacia administrativa",
    "Concussão",
    "Excesso de exação",
    "Extravio,? sonegação",
    "Inserção de dados falsos",
    "corrupção(?! de menor)",
    "Lavagem ou Ocultação de Bens",
    "não autorizada",
    "Peculato",
    "Contra a Administração em Geral",
    "Tráfico de influência"
  ) %>%
    tibble::enframe() %>%
    dplyr::mutate(value = value %>%
                    stringr::str_to_upper() %>%
                    abjutils::rm_accent())
}
txt_clean <- function(txt) {
  txt %>%
    stringr::str_to_upper() %>%
    abjutils::rm_accent() %>%
    stringr::str_squish()
}
all_lawsuits_file <- function(file) {
  message(file)
  pag <- readr::read_rds(file)
  pag_clean <- pag %>%
    dplyr::mutate(txt = txt_clean(txt))

  p <- assuntos_filtrar() %>%
    dplyr::mutate(res = purrr::map(value, pesquisar_assunto,
                                   txt_data = pag_clean)) %>%
    tidyr::unnest(res) %>%
    dplyr::mutate(
      cnj = purrr::map(txt, stringr::str_extract_all, pattern = padrao_cnj())
    ) %>%
    tidyr::unnest(cnj)

  if (nrow(p) == 0) return(dplyr::select(p, id, tipo = value, page, txt))

  p %>%
    tidyr::unnest(cnj) %>%
    dplyr::mutate(cnj = cnj %>%
                    stringr::str_remove_all("[^0-9]") %>%
                    stringr::str_pad(20, "left", "0")) %>%
    dplyr::select(cnj, id, tipo = value, page, txt)
}
pesquisar_assunto <- function(x, txt_data = all_texts_pags_clean) {
  message(x)
  filter(txt_data, str_detect(txt, x))
}
padrao_cnj <- function(){
  glue::glue(
    "[0-9]{{3,7}}-?",
    "[0-9]{{2}}\\.?",
    "[0-9]{{4}}\\.?",
    "[0-9]{{1}}\\.?",
    "[0-9]{{2}}\\.?",
    "[0-9]{{4}}"
  ) %>% as.character()
}
read_all_txt <- function(all_text_files) {
  all_text_files %>%
    set_names(.) %>%
    {pb <<- progress::progress_bar$new(total = length(.));.} %>%
    purrr::map_dfr(~{
      pb$tick()
      tibble::tibble(txt = readr::read_file(.x))
    }, .id = "file")
}
break_pags <- function(all_texts) {
  all_texts %>%
    dplyr::mutate(txt = stringr::str_split(txt, pattern = '\\f')) %>%
    tidyr::unnest(txt) %>%
    dplyr::group_by(file) %>%
    dplyr::mutate(pag = 1:n()) %>%
    dplyr::ungroup()
}
read_all_txt_parallel <- function(all_text_files) {
  all_text_files %>%
    set_names(.) %>%
    {pb <<- progress::progress_bar$new(total = length(.));.} %>%
    purrr::map_dfr(~{
      pb$tick()
      tibble::tibble(txt = readr::read_file(.x))
    }, .id = "file")
}
build_pages <- function(files, cores = 10) {
  files %>%
    purrr::set_names(.) %>%
    purrr::map_chr(readr::read_file) %>%
    tibble::enframe("id", "output") %>%
    dplyr::mutate(id = as.character(files)) %>%
    tidyr::unnest() %>%
    dplyr::mutate(output = stringr::str_split(output, "\f")) %>%
    tidyr::unnest(output) %>%
    dplyr::mutate(output = stringr::str_trim(output)) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(page = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(id, page, txt = output)
}
write_pages <- function(arqs_list, chunks = 100) {
  purrr::iwalk(arqs_list, ~{
    message(.y)
    chunk_size <- ceiling(length(.x) / 1000)
    indices <- list(
      0:(chunks - 1) * chunk_size + 1,
      c(1:(chunks - 1) * chunk_size, length(.x))
    ) %>%
      purrr::transpose() %>%
      purrr::map(purrr::flatten_dbl)
    files <- .x
    path <- .y
    purrr::iwalk(indices, ~{
      message(glue::glue("Chunk {.y}"))
      f <- files[.x[1]:.x[2]]
      # f <- files[1:3]
      pages <- build_pages(f)
      rds_file <- glue::glue("{path}/pages_{sprintf('%04d',.y)}.rds")
      message(glue::glue("Saving file to {rds_file}..."))
      readr::write_rds(pages, rds_file, compress = "none")
    })
  })
}
get_lawsuits <- function(arqs_list) {
  # arqs_list <- fs::dir_ls("diarios", type = "directory") %>%
  #   purrr::map(fs::dir_ls, regexp = "pages") %>%
  #   purrr::map(fs::path_abs)
  purrr::imap_dfr(arqs_list, ~{
    message(.y)
    res <- purrr::map_dfr(purrr::set_names(.x, .x),
                          all_lawsuits_file, .id = "chunk") %>%
      dplyr::select(chunk, cnj, id, tipo, page, raw_page = txt)
    f <- glue::glue("{.y}/lawsuits.rds")
    message(glue::glue("Saving {f} to disk..."))
    readr::write_rds(res, f, compress = "bz2")
    res
  }, .id = "tribunal")
}

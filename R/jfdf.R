
#' Baixar processo da JFDF
#'
#' @param id Numero do processo
#' @param path Caminho onde salvar HTMLs
#'
#' @examples
#' \dontrun{
#' download_jfdf("0036535-45.2000.4.01.3400")
#' }
#'
#' @export
download_jfdf <- function(id, path = ".") {

  url_mov_proc <- "https://processual.trf1.jus.br/consultaProcessual/processo.php"
  url_partes <- "https://processual.trf1.jus.br/consultaProcessual/arquivo/partes.php"
  id <- abjutils::build_id(id)

  query_mov_proc <- list(
    "secao"="DF",
    "enviar"="Pesquisar",
    "pg"=1,
    "proc"=id)

  query_partes <- list(
    "secao"="DF",
    "origem"="processual",
    "proc"=id)

  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)
  file_mov_proc <- stringr::str_c(path, "/", abjutils::clean_id(id), "_movimentacao.html")
  file_partes <- stringr::str_c(path, "/", abjutils::clean_id(id), "_partes.html")

  request_mov_proc <- httr::GET(
    url_mov_proc, query = query_mov_proc,
    httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(file_mov_proc, TRUE))
  request_partes <- httr::GET(
    url_partes, query = query_partes,
    httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(file_partes, TRUE))

  request <- list(movimentacao = request_mov_proc, partes = request_partes)

  return(c(file_mov_proc, file_partes))
}

#' Parsear processos da JFDF
#'
#' @param file Caminho para arquivo de movimentacoes
#'
#' @export
parse_jfdf <- function(file) {

  node <- file %>% xml2::read_html()

  aba_processo <- node %>%
    rvest::html_node(xpath = '//div[@id="aba-processo"]//table') %>%
    rvest::html_table(fill = T) %>%
    magrittr::set_names(c("key", "val")) %>%
    dplyr::mutate(key = stringr::str_to_lower(key),
      key = abjutils::rm_accent(key),
      key = stringr::str_replace(key, ":", ""),
      key = stringr::str_replace_all(key, " +", "_")) %>%
    tidyr::spread(key, val)

  aba_movimentacao <- node %>%
    rvest::html_node(xpath = '//div[@id="aba-movimentacao"]//table') %>%
    rvest::html_table() %>% {
      if(ncol(.) == 4){
        magrittr::set_names(., c("data", "cod", "decricao","complemento"))
      } else if(ncol(.) == 3){
        magrittr::set_names(., c("data", "cod", "decricao"))
      }} %>%
    tidyr::separate(data, into = c("data", "hora"), sep = " +") %>%
    dplyr::mutate(data = lubridate::dmy(data)) %>%
    tidyr::nest(.key = "movimentacao") %>%
    dplyr::mutate(file = file)

  tab <- dplyr::bind_cols(aba_processo, aba_movimentacao) %>% dplyr::as_tibble()

  return(tab)
}

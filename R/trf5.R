
#' Baixar processos do TRF5
#'
#' @param id Numero do processo
#' @param path Caminho para a pasta onde salvar os HTMLs
#'
#' @examples
#' \dontrun{
#' download_trf5("0000193-94.2016.4.05.8401")
#' }
#'
#' @export
download_trf5 <- function(id, path = "."){

  u0 <- "http://www5.trf5.jus.br/cp/"

  remDr <- seleniumPipes::remoteDr(port = 4445L, browserName = "chrome")
  remDr %>%
    seleniumPipes::go(url = u0) %>%
    seleniumPipes::findElement(using = 'name', value = 'filtro') %>%
    seleniumPipes::elementClick() %>%
    seleniumPipes::elementSendKeys(id)

  remDr %>% seleniumPipes::findElement(using = 'id', value = 'submitConsulta') %>%
    seleniumPipes::elementClick()

  #remDr %>% seleniumPipes::closeWindow()

  h <-remDr %>% seleniumPipes::getWindowHandles()
  #
  remDr %>% seleniumPipes::switchToWindow(h[[2]])

  p1 <- remDr %>% seleniumPipes::getPageSource()

  dir.create(path, FALSE, TRUE)
  path<- normalizePath(path)
  p1 %>% xml2::write_html(file = stringr::str_c(path, "/", abjutils::clean_id(id), ".html"))

  remDr %>% seleniumPipes::deleteSession()
  Sys.sleep(0.5)

  return(stringr::str_c(path, "/", abjutils::clean_id(id), ".html"))
}

#' Parsear processos do TRF5
#'
#' @param file Caminho para o processo
#'
#' @export
parse_trf5 <- function(file) {

  parse_info <- function(file){
    file %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//table') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(1) %>%
      dplyr::select(1) %>%
      magrittr::set_names(c("key"))  %>%
      tidyr::separate(key, into = c("key", "val"), sep = ":|\\(") %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/|\\\u00ba|\\.", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_replace_all(val, "\\)", "")) %>%
      tidyr::spread(key, val)
  }

  parse_2 <- function(file){
    file %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//table') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(2) %>%
      dplyr::select(1, 2) %>%
      magrittr::set_names(., c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/|\\\u00ba|\\.", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_replace(val, ":", "")) %>%
      tidyr::spread(key, val)
  }

  parse_3 <- function(file){
    file %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//table') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(3) %>%
      dplyr::select(1, 2) %>%
      magrittr::set_names(., c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\\u00ba|\\.", ""),
        key = stringr::str_replace_all(key, "\\/", "_"),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_replace(val, ":", "")) %>%
      tidyr::nest(.key = "partes")
  }

  parse_movimentacao <- function(file){
    file %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//table') %>% {
        data <- rvest::html_nodes(., xpath = '//a[contains(@name,"mov")]') %>% rvest::html_text() %>%
          stringr::str_replace_all("[E-e]m", "") %>% stringr::str_trim()
        descricao <- rvest::html_nodes(., xpath = '//td[@width="95%"]') %>% rvest::html_text()
        dplyr::tibble(data = data, descricao = descricao)
      } %>%
      tidyr::separate(data, into = c("data", "hora"), sep = " ") %>%
      tidyr::nest(.key = "movimentacao")
  }

  dplyr::bind_cols(
    parse_info(file),
    parse_movimentacao(file),
    parse_2(file),
    parse_3(file)) %>%
    dplyr::as_tibble()
}

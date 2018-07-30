
#' Baixar e parsear processos da JFRJ
#'
#' @description Diferentemente da maioria dos downloaders/parsers,
#' a organizacao da JFRJ faz com que esses passos precisem estar juntos.
#' Aqui o objeto salvo ja representa o processo parseado.
#'
#' @param id Numero do processo
#' @param path_rds Caminho onde salvar os RDSs
#'
#' @examples
#' \dontrun{
#' download_and_parse_jfrj("01885848620174025101")
#' }
#'
#' @export
download_and_parse_jfrj <- function(id, path_rds = ".") {

  connect <- function(id) {
    u0 <- "http://procweb.jfrj.jus.br/portal/consulta/cons_procs.asp"
    remDr <- seleniumPipes::remoteDr(port = 4568L, browserName = "phantomjs")
    seleniumPipes::go(remDr, url = u0)
    src <- seleniumPipes::getPageSource(remDr)
    gabarito <- src %>%
      xml2::xml_find_first("//input[@id='gabarito']") %>%
      xml2::xml_attr("value")
    remDr %>%
      seleniumPipes::findElement("css", "#NumProc") %>%
      seleniumPipes::elementSendKeys(id)
    remDr %>%
      seleniumPipes::findElement("css", "#captchacode") %>%
      seleniumPipes::elementSendKeys(gabarito)
    remDr %>%
      seleniumPipes::findElement("css", "#Pesquisar") %>%
      seleniumPipes::elementClick()
    remDr
  }

  get_lawsuit_ids <- function(id) {
    rem_dr <- connect(id)
    id_doc <- rem_dr %>%
      seleniumPipes::switchToFrame("esq", retry = FALSE) %>%
      seleniumPipes::getPageSource() %>%
      xml2::xml_find_first("//input[@type='hidden']") %>%
      xml2::xml_attr("value")
    rem_dr <- connect(id)
    id_lawsuit <- rem_dr %>%
      seleniumPipes::switchToFrame("esq") %>%
      seleniumPipes::getPageSource() %>%
      xml2::xml_find_all("//select[@id='Procs']//option") %>%
      xml2::xml_attr("value")
    list(id_doc = id_doc, id_lawsuit = id_lawsuit)
  }

  p_tab <- function(r_tab) {
    r_tab %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//table") %>%
      dplyr::nth(3) %>%
      rvest::html_table(header = TRUE, fill = TRUE) %>%
      janitor::clean_names() %>%
      tibble::as_tibble() %>%
      purrr::set_names(abjutils::rm_accent)
  }

  p_txt <- function(r_txt) {
    r_txt %>%
      xml2::read_html() %>%
      xml2::xml_find_first("//textarea") %>%
      xml2::xml_text()
  }

  download_and_parse_jfrj_ <- function(id) {
    lwst_ids <- get_lawsuit_ids(id)
    u_lawsuit <- list(
      info = "resinfoproc.asp",
      movs = "resinfomov2.asp",
      complemento = "resinfocompl2.asp",
      vinculados = "resinfoprocvinc2.asp",
      partes = "resinfopartes2.asp",
      pecas = "resinfopecas2.asp",
      recurso = "resinforecurso2.asp",
      naojuntada = "resinfopetnaojuntada2.asp"
    ) %>%
      purrr::map(~sprintf("%s%s?CodDoc=%s&IDNumConsProc=%s",
        "http://procweb.jfrj.jus.br/portal/consulta/",
        .x, lwst_ids$id_lawsuit, lwst_ids$id_doc))
    list_parsers <- purrr::prepend(purrr::map(1:7, ~p_tab), p_txt)
    res <- purrr::map2(u_lawsuit, list_parsers, ~.y(httr::GET(.x))) %>%
      purrr::map_if(~purrr::is_list(.x), list) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(id_lawsuit = lwst_ids$id_lawsuit,
        id_doc = lwst_ids$id_doc, result = "OK")
    res
  }

  fs::dir_create(path_rds)
  safe_dl <- purrr::possibly(download_and_parse_jfrj_,
    tibble::tibble(result = "error"))
  id <- unique(stringr::str_remove_all(id, "[^0-9]"))
  names(id) <- id
  pb <- progress::progress_bar$new(
    total = length(id),
    format = "<:bar> :percent eta: :eta")

  purrr::map_dfr(id, ~{
    pb$tick()
    f <- stringr::str_glue("{path_rds}/{.x}.rds")
    if (!file.exists(f)) {
      res <- safe_dl(.x)
      readr::write_rds(res, f)
    } else {
      res <- tibble::tibble(result = "ja foi")
    }
    res
  }, .id = "id")
}


#' Baixar e parsear processos da JFAL
#'
#' @description Diferentemente da maioria dos downloaders/parsers,
#' a organizacao da JFAL faz com que esses passos precisem estar juntos.
#' Aqui o objeto salvo ja representa o processo parseado.
#'
#' @param id Numero do processo
#' @param path_rds Caminho onde salvar os RDSs
#'
#' @examples
#' \dontrun{
#' download_and_parse_jfal("0009214-22.2005.4.05.8000")
#' }
#'
#' @export
download_and_parse_jfal <- function(id, path_rds = ".") {
  fs::dir_create(path_rds)
  safe_dl <- purrr::possibly(download_and_parse_jfal_,
    tibble::tibble(result = "error"))
  id <- unique(stringr::str_remove_all(id, "[^0-9]"))
  names(id) <- id
  pb <- progress::progress_bar$new(total = length(id))

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

# Funcao auxiliar para o download
download_and_parse_jfal_ <- function(id) {

  body <- list('Validar' = '',
    'CampoFoco' = '',
    'Botao' = 'Pesquisar',
    'EstatCont' = 'EstatisticaContada',
    'NumProc' = id,
    'CodDoc' = '',
    'TipDocPess' = '0',
    'NumDocPess' = '',
    'CodTipDocPess' = '',
    'NomeParte' = '',
    'CodOAB' = '',
    'NomeAdv' = '',
    'CodAdv' = '',
    'NumInq' = '',
    'NumProcOrig' = '')

  u <- "http://tebas.jfal.jus.br/consulta/cons_procs.asp"
  r <- httr::POST(u, body = body, encode = "form")
  # writeBin(r$content, "aff.html")
  u_cods <- "http://tebas.jfal.jus.br/consulta/reslistproc.asp"
  r_cods <- httr::GET(u_cods)

  id_doc <- r_cods %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//input[@type='hidden']") %>%
    xml2::xml_attr("value")
  id_lawsuit <- r_cods %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//select[@id='Procs']//option") %>%
    xml2::xml_attr("value")

  p_tab <- function(r_tab) {
    r_tab %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//table") %>%
      dplyr::last() %>%
      rvest::html_table(header = TRUE, fill = TRUE) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      purrr::set_names(abjutils::rm_accent)
  }
  p_txt <- function(r_txt) {
    r_txt %>%
      xml2::read_html() %>%
      xml2::xml_find_first("//textarea") %>%
      xml2::xml_text()
  }

  u_lawsuit <- list(
    info = "http://tebas.jfal.jus.br/consulta/resinfoproc.asp",
    vinculados = "http://tebas.jfal.jus.br/consulta/resinfoprocvinc.asp",
    audiencias = "http://tebas.jfal.jus.br/consulta/resinfoproctermaud.asp",
    sentenca = "http://tebas.jfal.jus.br/consulta/resinfosentintegra.asp",
    partes = "http://tebas.jfal.jus.br/consulta/lista_partes.asp"
  ) %>%
    purrr::map(~sprintf("%s?CodDoc=%s&IDNumConsProc=%s", .x, id_lawsuit, id_doc))
  list_parsers <- list(p_txt, p_tab, p_txt, p_txt, p_tab)

  purrr::map2(u_lawsuit, list_parsers, ~.y(httr::GET(.x))) %>%
    purrr::map_if(~purrr::is_list(.x), list) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id_lawsuit = id_lawsuit, id_doc = id_doc,
      result = "OK")
}

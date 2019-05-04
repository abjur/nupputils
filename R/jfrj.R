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

  fs::dir_create(path_rds)
  id <- unique(stringr::str_remove_all(id, "[^0-9]"))
  f <- stringr::str_glue("{path_rds}/{id}.rds")

  if (!file.exists(f)) {
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
      purrr::map(~sprintf("%s%s?CodDoc=%s&IDNumConsProc=1",
                          "http://procweb.jfrj.jus.br/portal/consulta/",
                          .x, lwst_ids$id_doc))
    list_parsers <- purrr::prepend(purrr::map(1:7, ~p_tab), p_txt)
    res <- purrr::map2(u_lawsuit, list_parsers, ~.y(httr::GET(.x))) %>%
      purrr::map_if(~purrr::is_list(.x), list) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(id_lawsuit = lwst_ids$id_lawsuit,
                    id_doc = lwst_ids$id_doc, result = "OK")
    readr::write_rds(res, f)
  }
  f
}

get_lawsuit_ids <- function(id_lawsuit) {
  id <- gsub("[^0-9]", "", id_lawsuit)
  u_base <- "http://procweb.jfrj.jus.br/portal/consulta"
  u_consulta <- paste0(u_base, "/cons_procs.asp")
  u_processo <- paste0(u_base, "/reslistproc.asp?SelectProc=")
  body <- list("Botao" = "Pesquisar", "gabarito" = "0",
               "resposta" = "0", "NumProc" = id_lawsuit)
  httr::POST(u_consulta, body = body, encode = "form")
  id_doc <- httr::GET(u_processo) %>%
    xml2::read_html(encoding = "latin1") %>%
    xml2::xml_find_first("//*[@id='Procs']//option") %>%
    xml2::xml_attr("value")
  list(id_doc = id_doc, id_lawsuit = id_lawsuit)
}

cells_scrape <- function(r) {
  xml2::xml_text(xml2::xml_children(r), trim = TRUE)
}

html_table2 <- function (x) {
  values <- purrr::map(x, cells_scrape)
  df <- as.data.frame(do.call(rbind, values[-1]), stringsAsFactors = FALSE)
  if (length(values[[1]]) == ncol(df)) df <- stats::setNames(df, values[[1]])
  return(df)
}

p_tab <- function(r_tab) {
  r_tab %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//table") %>%
    dplyr::nth(3) %>%
    xml2::xml_find_all("./tr") %>%
    html_table2() %>%
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





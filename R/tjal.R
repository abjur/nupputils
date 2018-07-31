
#' Baixar processos do TJAL
#'
#' @param id Numero do processo
#' @param path Caminho para a pasta onde salvar os HTMLs
#' @param inst Numero da instancia (1 ou 2)
#'
#' @examples
#' \dontrun{
#' download_tjal("0803813-44.2018.8.02.0000", inst = 2)
#' }
#'
#' @export
download_tjal <- function(id, path = ".", inst = "1") {

  primeira_instancia <- function(query, path){

    url <- "https://www2.tjal.jus.br/cpopg/search.do"

    query_GET <- list(
      "conversationId"="",
      "dadosConsulta.localPesquisa.cdLocal"="-1",
      "cbPesquisa"="NUMPROC",
      "dadosConsulta.tipoNuProcesso"="UNIFICADO",
      "dadosConsulta.valorConsultaNuUnificado"= query,
      "dadosConsulta.valorConsulta"="")

    proc <- stringr::str_replace_all(query, "[[:punct:]]", "") %>%
      stringr::str_c(.,"_PRIMEIRA.html")
    dir.create(path, FALSE, TRUE)
    path <- normalizePath(path)
    file <- stringr::str_c(path, "/")
    PathFile <- stringr::str_c(file, proc)
    httr::GET(url, query = query_GET,
      httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(PathFile, overwrite = T))

    return(PathFile)
  }

  segunda_instancia <- function(query, path){

    url <- "https://www2.tjal.jus.br/cposg5/search.do"
    Sys.sleep(2)

    query_GET <- list(
      "conversationId" = "",
      "paginaConsulta" = "1",
      "cbPesquisa" = "NUMPROC",
      "tipoNuProcesso" = "UNIFICADO",
      "numeroDigitoAnoUnificado" = stringr::str_remove(query, ".{10}$"),
      "foroNumeroUnificado" = stringr::str_extract(query, ".{4}$"),
      "dePesquisaNuUnificado" = query,
      "dePesquisa" = "",
      "uuidCaptcha" = "",
      "pbEnviar" = "Pesquisar")

    proc <- stringr::str_replace_all(query, "[[:punct:]]", "") %>%
      stringr::str_c(.,"_SEGUNDA.html")
    dir.create(path, FALSE, TRUE)
    path <- normalizePath(path)
    file <- stringr::str_c(path, "/")
    PathFile <- stringr::str_c(file, proc)
    httr::GET(url, query = query_GET,
      httr::write_disk(PathFile, overwrite = T))

    return(PathFile)
  }

  if (as.character(inst) == "1") {
    return(primeira_instancia(id, path))
  }
  else if (as.character(inst) == "2") {
    return(segunda_instancia(id, path))
  } else {
    stop("O argumento inst deve ser igual a 1 ou 2")
  }
}

#' Parsear processos do TJAL
#'
#' @param file Caminho para o processo
#' @param inst Numero da instancia (1 ou 2)
#'
#' @export
parse_tjal <- function(file, inst = 1) {

  parse_info_pri <- function(file){
    node <- file %>% xml2::read_html()
    info <-  node %>%
      rvest::html_nodes(xpath = '//table[@class="secaoFormBody"]') %>%
      rvest::html_table(fill = T) %>%
      {if(length(.) >= 2){t <- purrr::pluck(., 2)} else{t <- purrr::pluck(., 1)}} %>%
      dplyr::select(1, 2) %>%
      magrittr::set_names(c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_trim(key)) %>%
      tidyr::unite(c("key", "val"), sep = " ") %>%
      magrittr::set_names("key") %>%
      dplyr::filter(!stringr::str_detect(key, "NA")) %>%
      dplyr::mutate(key = dplyr::case_when(stringr::str_detect(key, "[V-v]ara|[F-f]oro|[C-c]omarca") ~ stringr::str_c("Local:", key),
        T ~ key),
        key = dplyr::case_when(stringr::str_detect(key, "Origem|Distribuição") ~ stringr::str_replace_all(key, "Local:", ""),
          T~key)) %>%
      tidyr::separate(key, into = c("key", "val"), sep = ":", extra = "merge") %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_trim(val),
        val = stringr::str_replace_all(val, "\\n|\\t", "")) %>%
      tidyr::spread(key, val)

    return(info)
  }

  parse_delegacia <- function(file){
    node <- file %>% xml2::read_html()
    nam <- node %>%
      rvest::html_nodes(xpath = '//tr[@class="label"]') %>%
      rvest::html_text(trim = T) %>%
      .[stringr::str_detect(., "Distrito policial")] %>%
      stringr::str_replace_all("\\t", "") %>%
      stringr::str_split(pattern = "\\n") %>%
      purrr::pluck(1)

    tem_delegacia <- node %>%
      rvest::html_nodes(xpath = '//tbody[@id="dadosDaDelegacia"]') %>%
      rvest::html_text()

    if (length(tem_delegacia) == 0) {
      tem_delegacia <- NA_character_
    }

    if(is.na(tem_delegacia)|purrr::is_null(tem_delegacia)){
      tab <- dplyr::tibble(resultado_delegacia = "error") %>%
        tidyr::nest(.key = "delegacia")
      return(tab)
    } else {
      obter_delegacia_obs(node, nam)
    }
  }

  obter_delegacia_obs <- function(node, nam){
    delegacia_tab_claro <- node %>%
      rvest::html_nodes(xpath = '//tbody[@id="dadosDaDelegacia"]//tr[@class="fundoClaro"]//td')

    delegacia_tab_escuro <- node %>%
      rvest::html_nodes(xpath = '//tbody[@id="dadosDaDelegacia"]//tr[@class="fundoEscuro"]//td')

    if(purrr::is_empty(delegacia_tab_escuro %>% rvest::html_text())){
      t0 <- delegacia_tab_claro %>%
        rvest::html_text() %>%
        dplyr::tibble(val = .) %>%
        dplyr::bind_cols(dplyr::tibble(key = as.character(seq(1, nrow(.), 1)))) %>%
        tidyr::spread(key, val)

      purrr::map_dfr(list(t0), ~dplyr::bind_rows(.x)) %>%
      {if(ncol(.) == length(nam)){magrittr::set_names(., nam)}} %>%
        tidyr::nest(.key = "delegacia")
    } else {
      t0 <- delegacia_tab_claro %>%
        rvest::html_text() %>%
        dplyr::tibble(val = .) %>%
        dplyr::bind_cols(dplyr::tibble(key = as.character(seq(1, nrow(.), 1)))) %>%
        tidyr::spread(key, val)

      t1 <- delegacia_tab_escuro %>%
        rvest::html_text() %>%
        dplyr::tibble(val = .) %>%
        dplyr::bind_cols(dplyr::tibble(key = as.character(seq(1, nrow(.), 1)))) %>%
        tidyr::spread(key, val)

      purrr::map_dfr(list(t0, t1), ~dplyr::bind_rows(.x)) %>%
      {if(ncol(.) == length(nam)){magrittr::set_names(., nam)}} %>%
        tidyr::nest(.key = "delegacia")
    }
  }

  parse_partes <- function(file){
    node <- file %>% xml2::read_html()

    node %>% rvest::html_node(xpath = '//table[@id="tablePartesPrincipais"]') %>%
      rvest::html_table() %>%
      {if(ncol(.)==2){magrittr::set_names(., c("key", "val"))}} %>%
      tidyr::nest(.key = "partes")
  }

  parse_movimentacao <- function(file){
    node <- file %>% xml2::read_html()

    data <- node %>%
      rvest::html_nodes(xpath = '//tbody[@id="tabelaTodasMovimentacoes"]//td[@style="vertical-align: top"]') %>% rvest::html_text(trim = T) %>%
      stringr::str_replace_all("\\n|\\t", "")
    descricao <- node %>%
      rvest::html_nodes(xpath = '//tbody[@id="tabelaTodasMovimentacoes"]//td[@style="vertical-align: top; padding-bottom: 5px"]') %>%
      rvest::html_text(trim = T)
    dplyr::tibble(data = data, descricao = descricao) %>%
      tidyr::nest(.key = "movimentacao")
  }

  parse_info_seg <- function(file){
    node <- file %>% xml2::read_html()
    node %>%
      rvest::html_nodes(xpath = '//table[@class="secaoFormBody"]') %>%
      rvest::html_table(fill = T) %>%
      {if(length(.) >= 2){t <- purrr::pluck(., 2)} else{t <- purrr::pluck(., 1)}} %>%
      dplyr::select(1, 2) %>%
      magrittr::set_names(c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_trim(key)) %>%
      tidyr::unite(c("key", "val"), sep = " ") %>%
      magrittr::set_names("key") %>%
      dplyr::filter(!stringr::str_detect(key, "NA")) %>%
      dplyr::mutate(key = dplyr::case_when(stringr::str_detect(key, "[V-v]ara|[F-f]oro|[C-c]omarca") ~ stringr::str_c("Local:", key),
        T ~ key),
        key = dplyr::case_when(stringr::str_detect(key, "Origem|Distribuição") ~ stringr::str_replace_all(key, "Local:", ""),
          T~key)) %>%
      tidyr::separate(key, into = c("key", "val"), sep = ":", extra = "merge") %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_trim(val),
        val = stringr::str_replace_all(val, "\\n|\\t", "")) %>%
      tidyr::spread(key, val)
  }

  julgamento <- function(file){

    p <- file %>% xml2::read_html() %>%
      rvest::html_nodes(xpath = '//td/h2[@class="sutitle"]|//tr[@class="fundoClaro"]/td[@align="left"]') %>%
      rvest::html_text() %>%
      stringr::str_replace_all("\\n|\\t", "") %>%
      stringr::str_trim()
    if(purrr::is_empty(p)){
      p <- ""
    }
    s <- ""
    n <- ""
    for(i in 1:length(p)){
      if(stringr::str_detect(p[i], "[J-j]ulgado") == T){
        s <- "SIM"
      } else {
        n <- "NAO"
      }
    }

    if(s == "SIM"){
      fim <- length(p)
      inicio <- fim - 2
      tam <- seq(inicio, fim, 1)
      decisao <- p[tam]
      data <- dplyr::tibble(val = decisao, key = c("data_julgamento", "situacao_julgamento", "decisao_julgamento")) %>%
        tidyr::spread(key, val)
    } else{
      data <- dplyr::tibble(val = c("Sem informacao",
        "Sem informacao",
        "Sem informacao"),
        key = c("data_julgamento",
          "situacao_julgamento",
          "decisao_julgamento")) %>%
        tidyr::spread(key, val)
    }
    return(data)
  }

  if (as.character(inst) == "1") {
    info <- parse_info_pri(file)
    delegacia <- parse_delegacia(file)
    movimentacao <- parse_movimentacao(file)
    partes <- parse_partes(file)
    out <- dplyr::bind_cols(info, delegacia, movimentacao, partes) %>%
      dplyr::mutate(resultado_parser = "ok", file = file) %>%
      dplyr::as_tibble()
    return(out)
  }
  else if (as.character(inst) == "2") {
    info <- parse_info_seg(file)
    delegacia <- parse_delegacia(file)
    movimentacao <- parse_movimentacao(file)
    partes <- parse_partes(file)
    julgamento <- julgamento(file)
    out <- dplyr::bind_cols(info, delegacia, movimentacao, partes, julgamento) %>%
      dplyr::mutate(resultado_parser = "ok",
        file = file) %>%
      dplyr::as_tibble()
    return(out)
  }
  else {
    stop("O argumento inst deve ser igual a 1 ou 2")
  }
}

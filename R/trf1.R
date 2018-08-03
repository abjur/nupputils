
#' Baixar processos do TRF1
#'
#' @param id Numero do processo
#' @param path Caminho para a pasta onde salvar os HTMLs
#'
#' @examples
#' \dontrun{
#' download_trf1("0009384-30.2011.4.01.3300")
#' }
#'
#' @export
download_trf1 <- function(id, path = "."){

  # Obtendo o arquivo html
  url_mov_proc <- "https://processual.trf1.jus.br/consultaProcessual/processo.php"
  url_partes <- "https://processual.trf1.jus.br/consultaProcessual/arquivo/partes.php"
  #proc <- abjutils::build_id(proc)

  query_mov_proc <- list(
    "secao"="TRF1",
    "enviar"="Pesquisar",
    "pg"=1,
    "proc"=id)

  query_partes <- list(
    "secao"="TRF1",
    "origem"="juris",
    "proc"=id)

  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)
  file_mov_proc <- stringr::str_c(path,"/",abjutils::clean_id(id), "_movEinfoprocesso.html")
  file_partes <- stringr::str_c(path,"/",abjutils::clean_id(id), "_partes.html")

  request_mov_proc <- httr::GET(url_mov_proc, query = query_mov_proc, httr::config(ssl_verifypeer = FALSE), httr::write_disk(file_mov_proc, T))
  request_partes <- httr::GET(url_partes, query = query_partes, httr::config(ssl_verifypeer = FALSE), httr::write_disk(file_partes, T))

  request <- list(movimentacao = request_mov_proc,
    partes = request_partes)

  return(c(file_mov_proc, file_partes))
}

#' Parsear processos do TRF1
#'
#' @param file_movs Caminho para os movimentos do processo
#'
#' @export
parse_trf1 <- function(file_movs) {

  # aba-movimentacao
  parse_mov <- function(node){
    node %>%
      rvest::html_node(xpath = '//div[@id="aba-movimentacao"]/table') %>%
      rvest::html_table(fill = T) %>%
      magrittr::set_names(c("data", "codigo", "descricao", "complemento")) %>%
      tidyr::separate(data, into = c("data", "hora"), sep = " ") %>%
      #dplyr::mutate(processo = processo) %>%
      #dplyr::group_by(processo) %>%
      tidyr::nest(.key = "movimentacao")
  }

  # aba-processo
  parse_processo <- function(node){
    node %>%
      rvest::html_node(xpath = '//div[@id="aba-processo"]/table') %>%
      rvest::html_table(fill = T) %>%
      {if(ncol(.)==2){magrittr::set_names(.,c("key", "val"))} else{print("error")}} %>%
      dplyr::mutate(key = key %>%
          stringr::str_replace_all("[[:punct:]]", "") %>%
          stringr::str_replace_all(" +", "_") %>%
          stringr::str_to_lower() %>%
          abjutils::rm_accent()) %>%
      tidyr::spread(key, val)
  }

  # aba-distribuicao
  parse_distribuicao <- function(node){
    node %>%
      rvest::html_node(xpath = '//div[@id="aba-distribuicao"]/table') %>%
      rvest::html_table(fill = T) %>%
      {if(ncol(.)==3){magrittr::set_names(.,c("data", "descricao", "juiz"))} else{print("error")}} %>%
      #dplyr::mutate(processo = processo) %>%
      #dplyr::group_by(processo) %>%
      tidyr::nest(.key = "distribuicao")
  }

  # aba-peticoes
  parse_peticoes <- function(node){
    node <- node %>%
      rvest::html_node(xpath = '//div[@id="aba-peticoes"]/table')

    if(is.na(node)){
      dplyr::tibble()
    }else{
      tab <- node %>% rvest::html_table(fill = T)
      if(is.null(tab)){
        tab %>% tidyr::nest(.key = "peticoes")
      }
      #dplyr::mutate(processo = processo) %>%
      #dplyr::group_by(processo) %>%

    }
  }

  node <- file_movs %>% xml2::read_html()
  processo_info <-node %>%
    parse_processo() %>%
    dplyr::bind_cols(
      parse_mov(node),
      parse_peticoes(node),
      parse_distribuicao(node)
    ) %>%
    dplyr::mutate(file_path = file_movs) %>%
    dplyr::as_tibble()

  return(processo_info)
}

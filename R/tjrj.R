
#' Baixar processos do TJRJ
#'
#' @param id Numero do processo
#' @param path Caminho para a pasta onde salvar os HTMLs
#'
#' @examples
#' \dontrun{
#' download_tjrj("00427318920128190000")
#' }
#'
#' @export
download_tjrj <- function(id, path = "."){
  u0 <- "http://www4.tjrj.jus.br/numeracaoUnica/faces/index.jsp?numProcesso="
  id <- abjutils::build_id(id)
  u0 <- stringr::str_c(u0, id)
  remDr <- RSelenium::rsDriver(port = 4445L, browser = 'firefox', verbose = FALSE)
  remDr$client$navigate(u0)
  Sys.sleep(0.3)
  remDr %>% mov()
  Sys.sleep(5.5)

  w3 <- remDr$client$findElement(using = 'xpath', value = '//a[@href="javascript:exibeListaPersonagens();"]')
  w3$clickElement()
  p <- remDr$client$getPageSource()
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)
  new_file <- stringr::str_c(path, "/", abjutils::clean_id(id), ".html")
  p %>% purrr::pluck(1) %>% readr::write_lines(new_file)
  Sys.sleep(0.2)
  return(new_file)
}

#' Parsear processos do TJRJ
#'
#' @param file Caminho para o processo
#'
#' @export
parse_tjrj <- function(file) {

  obter_fase_atual <- function(file){
    node <- file %>% xml2::read_html()
    node %>%
      rvest::html_nodes(xpath = '//table[@width="100%"][2]') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(1) %>%
      dplyr::select(1, 2) %>%
      dplyr::as_tibble() %>%
      purrr::set_names(c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        flag_info = stringr::str_detect(key, "fase atual:|fase:"),
        group = NA_integer_) %>% {
          group <- 1
          for(i in 1:nrow(.)){
            if(.[i,'flag_info'] == TRUE){
              .[i, 'group'] <- group
              group <- group + 1
            }
          }
          .
        } %>%
      tidyr::fill(group) %>%
      dplyr::filter(group == 1) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/|\\:", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_trim(val),
        val = stringr::str_replace_all(val, "\\n|\\t", ""),
        key = dplyr::case_when(key == "fase_atual" ~ "fase",
          T~ key)) %>%
      dplyr::select(1,2) %>%
      tidyr::spread(key, val)
  }

  parse_movimentacao <- function(file) {
    node <- file %>% xml2::read_html()

    fase_atual <- obter_fase_atual(file)

    inst <- node %>%
      rvest::html_node(xpath = '//div[@id="conteudo"]/span/h3') %>% rvest::html_text(trim = T) %>%
      stringr::str_replace_all(".*\\-", "") %>%
      stringr::str_trim()

    proc <- file %>%
      stringr::str_replace_all("[^0-9.html]", "") %>%
      stringr::str_replace_all("[A-Z]|[a-z]|\\.", "")

    mov <- node %>%
      rvest::html_nodes(xpath = '//span[@id="Movimentos"]//table') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(1) %>%
      magrittr::set_names(c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/|\\:", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_trim(val),
        val = stringr::str_replace_all(val, "\\n|\\t", "")) %>%
      dplyr::filter(key != "") %>%
      dplyr::mutate(fase_flag = dplyr::case_when(key == "fase" ~ TRUE,
        T~FALSE),
        group = NA_character_)

    group <- 1
    for(i in 1:nrow(mov)){
      if(mov$fase_flag[i] == TRUE){
        mov$group[i] <- group
        group <- group + 1
      }
    }
    tab_mov <- mov %>% tidyr::fill(group) %>%
      dplyr::select(-fase_flag) %>%
      dplyr::distinct(key, group, .keep_all = T) %>%
      tidyr::spread(key, val) %>%
      dplyr::bind_rows(fase_atual) %>%
      tidyr::nest(.key = "movimentacao") %>%
      dplyr::mutate(proc = proc,
        instancia = inst,
        result = "ok")

    return(tab_mov)
  }
  parse_movimentacao <- purrr::possibly(parse_movimentacao, otherwise = dplyr::tibble(result = "error"))

  parse_info <- function(file){

    node <- file %>% xml2::read_html()

    proc <- file %>%
      stringr::str_replace_all("[^0-9.html]", "") %>%
      stringr::str_replace_all("[A-Z]|[a-z]|\\.", "")

    info <- node %>% rvest::html_nodes(xpath = '//table[@width="100%"][2]') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(1) %>%
      dplyr::select(1, 2) %>%
      magrittr::set_names(c("key", "val")) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        flag_info = stringr::str_detect(key, "personagem"),
        group = NA_integer_) %>% {
          group <- 1
          for(i in 1:nrow(.)){
            if(.[i,'key'] == ""){
              .[i, 'group'] <- group
              group <- group + 1
            }
          }
          .
        } %>%
      tidyr::fill(group) %>%
      dplyr::filter(group %in% c(NA_integer_,1, 2)) %>%
      dplyr::select(1, 2) %>%
      dplyr::mutate(key = stringr::str_to_lower(key),
        key = stringr::str_trim(key),
        key = abjutils::rm_accent(key),
        key = stringr::str_replace_all(key, "\\/|\\:", ""),
        key = stringr::str_replace_all(key, " +", "_"),
        val = stringr::str_trim(val),
        val = stringr::str_replace_all(val, "\\n|\\t|:", "")) %>%
      tidyr::drop_na() %>% {
        for(i in 1:nrow(.)){
          if(stringr::str_detect(.[i,1], "") != T){
            .[i,1] <- NA
          }
        }
        .
      } %>%
      tidyr::drop_na() %>%
      tidyr::spread(key, val) %>%
      dplyr::mutate(proc = proc,
        result = "ok")

    return(dplyr::as_tibble(info))
  }
  parse_info <- purrr::possibly(parse_info, otherwise = dplyr::tibble(result = "error"))

  parse_partes <- function(file){
    node <- file %>% xml2::read_html()

    proc <- file %>%
      stringr::str_replace_all("[^0-9.html]", "") %>%
      stringr::str_replace_all("[A-Z]|[a-z]|\\.", "")

    partes <- node %>% rvest::html_nodes(xpath = '//div[@id="listaPersonagens"]/table') %>%
      rvest::html_table(fill = T) %>%
      purrr::pluck(1) %>%
      magrittr::set_names(c("key", "val")) %>%
      dplyr::mutate(flag = stringr::str_detect(key, "[I-m]mprimir")) %>%
      dplyr::filter(flag != TRUE | key == "") %>%
      dplyr::select(-flag) %>%
      tidyr::nest(.key = "partes") %>%
      dplyr::mutate(processo = proc,
        result = "ok")

    return(dplyr::as_tibble(partes))
  }

  parse_partes <- purrr::possibly(parse_partes, otherwise = dplyr::tibble(result = "error"))

  mov <- parse_movimentacao(file)
  info <- parse_info(file)
  partes <- parse_partes(file)

  dplyr::bind_cols(mov, info, partes)
}


#' Baixar processos do TJDFT
#'
#' @param id Numero do processo
#' @param path Caminho para a pasta onde salvar os HTMLs
#' @param inst Numero da instancia (1 ou 2)
#'
#' @examples
#' \dontrun{
#' download_tjdft("0003100-78.2016.8.07.0018")
#' }
#'
#' @export
download_tjdft <- function(id, path = ".", inst = "1") {

  primeira_instancia <- function(proc, path){
    url <- "http://www.tjdft.jus.br/consultas"
    session <- rvest::html_session(url)
    forms <- session %>% rvest::html_form() %>% .[[2]]
    values <- rvest::set_values(forms,`SELECAO` ="1", `CIRC` = "ZZ",`CHAVE`= proc)
    submit <- rvest::submit_form(session, values)
    url <- submit %>% purrr::pluck('url')

    proc <- stringr::str_replace_all(proc, "[[:punct:]]", "") %>%
      stringr::str_c(.,"_PRIMEIRA.html")
    dir.create(path, FALSE, TRUE)
    path <- normalizePath(path)
    file <- stringr::str_c(path, "/")
    PathFile <- stringr::str_c(file, proc)
    httr::GET(url, httr::config(ssl_verifypeer = FALSE)) %>%
      httr::content() %>%
      base::as.character() %>%
      stringr::str_extract("(?<=URL=).+(?=\")") %>%
      stringr::str_remove_all("amp;") %>%
      httr::GET(httr::write_disk(PathFile, overwrite = T))

    return(PathFile)
  }

  segunda_instancia <- function(proc, path){
    url <- "http://www.tjdft.jus.br/consultas"
    session <- rvest::html_session(url)
    forms <- session %>% rvest::html_form() %>% .[[5]]
    values <- rvest::set_values(forms,`SELECAO` ="1",`CHAVE`= proc)
    submit <- rvest::submit_form(session, values)
    url <- submit %>% purrr::pluck('url')

    proc <- stringr::str_replace_all(proc, "[[:punct:]]", "") %>%
      stringr::str_c(.,"_SEGUNDA.html")
    dir.create(path, FALSE, TRUE)
    path <- normalizePath(path)
    file <- stringr::str_c(path, "/")
    PathFile <- stringr::str_c(file, proc)
    httr::GET(url,
      httr::config(ssl_verifypeer = FALSE),
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

#' Parsear processos do TJDFT
#'
#' @param file Caminho para o processo
#' @param inst Numero da instancia (1 ou 2)
#'
#' @export
parse_tjdft <- function(file, inst = "1") {

  parse_tjdft_1 <- function(file) {

    parse_mov <- function(node){
      mov <- node %>%
        xml2::xml_find_all('//*[@id="detalhamentoDeProcesso"]//table') %>%
        rvest::html_table(fill = T) %>%
        purrr::pluck(1) %>%
        dplyr::mutate(X1 = stringr::str_trim(X1),
          verificar_string = ifelse(stringr::str_detect(X1, "Data"), TRUE, FALSE)) %>%
        dplyr::filter(verificar_string != TRUE) %>%
        dplyr::mutate(verificar_string = ifelse(stringr::str_detect(X1, ""), TRUE, FALSE)) %>%
        dplyr::filter(verificar_string != FALSE) %>%
        dplyr::select(1,2,3) %>%
        magrittr::set_names(c("data", "andamento", "complemento")) %>%
        tidyr::separate(data, into = c("data", "hora"), sep = "-")
      return(mov)
    }

    parse_info <- function(node){
      xml2::xml_find_all(node, '//*[@id="detalhamentoDeProcesso"]//span') %>% {
        key <- rvest::html_attr(., 'id')
        val <- rvest::html_text(.)
        dplyr::tibble(key = key, val = val)
      } %>%
        dplyr::mutate(detect_hora = ifelse(stringr::str_detect(key, "(dataHoraAndamento)|(descricaoAndamento)"), TRUE, FALSE)) %>%
        dplyr::filter(detect_hora != TRUE) %>%
        dplyr::select(-detect_hora) %>%
        tidyr::spread(key, val)
    }

    sentenca_prim_inst <- function(file){

      sentenca_link <- file %>%
        xml2::read_html() %>%
        rvest::html_nodes(xpath = '//a') %>% {
          a <- rvest::html_attr(.,'href')
          t <- rvest::html_text(., trim = T)
          dplyr::tibble(link = a, descr = t) %>%
            dplyr::filter(descr == "Consulta Sentença")
        } %>% dplyr::select(1) %>%
        purrr::as_vector()

      if(length(sentenca_link) == 0){
        tab <- dplyr::tibble(sentenca = "Sem informação")
        return(tab)
      } else {
        sentenca <- xml2::read_html(sentenca_link) %>%
          rvest::html_text(trim = T) #%>% stringr::str_extract_all("(Sentença \\:).*") preocupar-se com a limpeza
        tab <- dplyr::tibble(sentenca = sentenca)
        return(tab)
      }
    }

    node <- file %>% xml2::read_html()
    mov <- parse_mov(node) %>% tidyr::nest(.key = "movimentacao")
    info <- parse_info(node)
    sentecas <- sentenca_prim_inst(file)
    tab <- dplyr::bind_cols(info, mov) %>% dplyr::mutate(result = "ok")
    return(tab)
  }

  parse_tjdft_2 <- function(file) {

    ref_outro_proc <- function(file){
      node <- file %>% xml2::read_html()
      node %>% rvest::html_node(xpath = '//table[@cellspacing="0"]') %>% rvest::html_text() %>% {
        if(is.na(.)){
          t <- dplyr::tibble(file = file, referencia_outros_proc = TRUE)
          return(t)
          break()
        }else{
          t <- dplyr::tibble(file = file, referencia_outros_proc = FALSE)
          return(t)
        }
      }
    }

    parse_info_seg <- function(file){
      node <- file %>% xml2::read_html()

      node %>% xml2::xml_find_all(xpath = '//hidden[@id]') %>% {
        key <- rvest::html_attr(., 'id')
        val <- rvest::html_attr(., 'value')
        dplyr::tibble(key = key, val = val)
      } %>%
        dplyr::mutate(detect_hora = ifelse(stringr::str_detect(key, "(dataHoraAndamento)|(descricao_andamento)"), TRUE, FALSE)) %>%
        dplyr::filter(detect_hora != TRUE) %>%
        dplyr::select(-detect_hora) %>%
        dplyr::mutate(val = dplyr::case_when(val == "" ~NA_character_,
          T~val)) %>%
        tidyr::spread(key, val)
    }

    sentenca_seg_inst <- function(file){
      sentenca_link <- file %>% xml2::read_html() %>% rvest::html_nodes(xpath = '//a') %>% {
        a <- rvest::html_attr(.,'href')
        t <- rvest::html_text(., trim = T)
        dplyr::tibble(link = a, descr = t) %>% dplyr::filter(descr == "Decisão") # Função parecida com a de primeira instanvcia
      } %>% dplyr::select(1) %>%
        purrr::as_vector()

      if(length(sentenca_link) == 0| purrr::is_null(sentenca_link)|is.na(sentenca_link)){
        tab <- dplyr::tibble(sentenca = "Sem informação")
        return(tab)
      } else {
        sentenca <- xml2::read_html(sentenca_link) %>%
          rvest::html_text(trim = T) #%>% stringr::str_extract_all("(Sentença \\:).*") preocupar-se com a limpeza
        tab <- dplyr::tibble(sentenca = sentenca)
        return(tab)
      }
    }

    sent <- sentenca_seg_inst(file)
    info <- parse_info_seg(file)
    ref <- ref_outro_proc(file)
    tab <- dplyr::bind_cols(sent, info, ref) %>% dplyr::mutate(result = "ok")

    return(tab)

  }

  if (as.character(inst) == "1") {
    return(parse_tjdft_1(file))
  }
  else if (as.character(inst) == "2") {
    return(parse_tjdft_2(file))
  } else {
    stop("O argumento inst deve ser igual a 1 ou 2")
  }
}


#' Baixar e parsear processos da JFSP
#'
#' @description Diferentemente da maioria dos downloaders/parsers,
#' a organizacao da JFSP faz com que esses passos precisem estar juntos.
#' Aqui o objeto salvo ja representa o processo parseado.
#'
#' @param id Numero do processo
#' @param path_rds Caminho onde salvar os RDSs
#' @param detalhe_fases Se deve ou nao extrair os detalhes das fases
#'
#' @examples
#' \dontrun{
#' download_and_parse_jfsp("0001274-57.1993.4.03.6100")
#' }
#'
#' @export
download_and_parse_jfsp <- function(id, path_rds = ".", detalhe_fases = FALSE) {
  fs::dir_create(path_rds)
  safe_dl <- purrr::possibly(jfsp_selenium,
    tibble::tibble(result = "error"))
  id <- unique(stringr::str_remove_all(id, "[^0-9]"))
  names(id) <- id
  pb <- progress::progress_bar$new(total = length(id))

  purrr::map_dfr(id, ~{
    pb$tick()
    f <- stringr::str_glue("{path_rds}/{.x}.rds")
    if (!file.exists(f)) {
      res <- safe_dl(.x, detalhe_fases)
      readr::write_rds(res, f)
    } else {
      res <- tibble::tibble(result = "ja foi")
    }
    res
  }, .id = "id")
}

# Funcao auxiliar
jfsp_selenium <- function(query, detalhe_fases){

  remDR <- RSelenium::rsDriver(port = 4445L, verbose = FALSE)
  remDR$client$navigate("http://csp.jfsp.jus.br/csp/consulta/consinternet.csp")

  webElem <- remDR$client$findElement(using = 'name', value = "num_processo")
  webElem$highlightElement()
  webElem$sendKeysToElement(list(query))

  webElem1 <- remDR$client$findElement(using = "xpath", '//input[@type="button" and @value="Pesquisar"]')
  webElem1$clickElement()

  Page1 <- remDR$client$getPageSource() %>%  unlist() %>% xml2::read_html()

  webElem2 <- remDR$client$findElement(using = "xpath", '//input[@value="Todas Peti\u00e7\u00f5es"]')
  webElem2$clickElement()
  TodasPeticoes <- remDR$client$getPageSource() %>%  unlist() %>% xml2::read_html()

  remDR$client$goBack()
  webElem3 <- remDR$client$findElement(using = "xpath", '//input[@value="Todas Fases"]')
  webElem3$clickElement()
  TodasFases <- remDR$client$getPageSource() %>%  unlist()%>% xml2::read_html()

  ultima_fase <- remDR$client$findElement(using = "xpath",
    '//table[position()=3]//tr[position()=2]//a')$getElementText() %>%
    unlist()

  i <- 2
  l <- 1
  DetalheFases <- list()

  if (detalhe_fases) {
    while(i != as.numeric(ultima_fase)+2){
      xpath <- stringr::str_replace('//table[position()=3]//tr[position()=VALOR]//a', "VALOR",
        as.character(i))
      webElem4 <- remDR$client$findElement(using = "xpath", xpath)
      webElem4$clickElement()
      DetalheFases[[l]] <- remDR$client$getPageSource() %>%  unlist()%>% xml2::read_html()
      remDR$client$goBack()
      i = i + 1
      l = l + 1
      Sys.sleep(1)
    }
  }

  remDR$client$goBack()
  webElem5 <- remDR$client$findElement(using = "xpath", '//input[@value="Todas Partes"]')
  webElem5$clickElement()
  TodasPartes <- remDR$client$getPageSource() %>%  unlist()%>% xml2::read_html()

  remDR$client$closeall()

  Sources <- list(
    page_1 = Page1,
    todas_peticoes = TodasPeticoes,
    todas_fases = TodasFases,
    todas_partes = TodasPartes,
    detalhes_fases = DetalheFases,
    num_processo = query)

  parse_descricao <- function(page1, xpath = NULL){
    if(is.null(xpath)==T){xpath <- '//table[@width="533"]'}
    page1 <- Sources$page_1 %>%
      rvest::html_nodes(xpath = '//table[@width="533"]') %>%
      rvest::html_table(fill = T) %>%
      dplyr::first() %>%
      dplyr::mutate(X1 = stringr::str_trim(X1),
        X1 = ifelse(X1 == "", NA, X1),
        X2 = stringr::str_trim(X2),
        X2 = ifelse(X2 == "", NA, X2)) %>%
      tidyr::drop_na() %>%
      t()

    colnames(page1) <- page1[1,]

    page1 <- as.data.frame(page1)

    page1 <- page1[-1,]

    return(page1)
  }

  parse_peticoes <- function(TodasPeticoes){
    TodasPeticoes <- TodasPeticoes %>%
      rvest::html_table(header = T, fill = T) %>% dplyr::last() %>%
      dplyr::mutate(Data = lubridate::dmy(Data),
        Processo = query) %>%
      dplyr::group_by(Processo) %>%
      tidyr::nest(.key = peticoes) %>%
      dplyr::as_tibble()

    return(TodasPeticoes$peticoes[[1]])
  }

  parse_fases <- function(TodasFases, num_processo){
    TodasFases <- TodasFases %>%
      rvest::html_table(header = T, fill = T) %>%
      dplyr::last() %>%
      dplyr::mutate(Processo = num_processo) %>%
      dplyr::group_by(Processo) %>%
      tidyr::nest(.key = fases) %>%
      dplyr::as_tibble()

    return(TodasFases$fases[[1]])
  }

  parse_partes <- function(TodasPartes){
    TodasPartes <- TodasPartes %>%
      rvest::html_table( fill = T ) %>% dplyr::last() %>%
      dplyr::mutate(X1 = stringr::str_trim(X1),
        X1 = ifelse(X1 == "", NA, X1),
        X2 = stringr::str_trim(X2),
        X2 = ifelse(X2 == "", NA, X2),
        X1 = stringr::str_to_lower(X1),
        X2 = stringr::str_to_lower(X2)) %>%
      tidyr::drop_na() %>%
      magrittr::set_names(c("key", "val")) %>%
      tidyr::spread(key, val) %>%
      dplyr::as_tibble()

    return(TodasPartes)
  }

  page_1 <- parse_descricao(Sources$page_1)
  fases <- parse_fases(Sources$todas_fases, Sources$num_processo)
  partes <- parse_partes(Sources$todas_partes)
  peticoes <- parse_peticoes(Sources$todas_peticoes)

  info <- dplyr::tibble(infos = list(page_1), fases = list(fases), partes = list(partes), peticoes = list(peticoes))

  return(info)
}

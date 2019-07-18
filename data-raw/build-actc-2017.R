#' ---
#' filename:     build-actc-2017.R
#' created:      2017-09-28
#' updated:      <2019-07-18 23:51:17 david at grover>
#' author:       David Mitchell <david.p.mitchell@homemail.com.au>
#' description:  Build Australian Customs Tariff Classification
#'               Script automatically downloads the ACTC from the DIBP
#'               Tariff Classification (Schedule 3)
#' notes:        Check `base_url` is still current
#' ---

### Set ACTC base URL
base_url <- "https://www.border.gov.au/Busi/cargo-support-trade-and-goods/importing-goods/tariff-classification-of-goods/current-tariff-classification/schedule-3";


######  Section 0 - Libraries & settings
library(magrittr);
library(rvest);        ## Web scraping functions
library(urltools);     ## URL parsing and composing
library(XML);          ## 
library(httr);         ## 
library(readxl);       ## Read Excel files
library(dplyr);
library(tidyr);

### Settings
DEBUG <- FALSE


### Functions
#' @name which_link
#' @title return URL link number matching specified \code{pattern}
#' @description return the link numbers
#' @import xml2::read_html rvest::html_nodes, rvest::html_text
#' @param x a session
#' @param node a node set or a single mode
#' @param pattern character string containing a regular expression to be matched against the node set string
#' @return returns integer specifying the number of the matching link
which_link <- function(x, node, pattern) {
  x %>% xml2::read_html(.) %>%
  rvest::html_nodes(node) %>%
  rvest::html_text() %>%
  grep(pattern, .)
}

#' @name capwords
#' @title Capitalise words
#' @param s string
#' @param split character on which to split the original string
#' @param strict enforce capitalisation
#' @return returns capitalised character string
function (s, split = c("\\W"), strict = FALSE) 
{
    if (length(s) == 0) 
        stop("Cannot capitalise a blank string.")
    if (!is.character(s)) 
        s <- as.character(s)
    replace(s, is.na(s), "")
    cap <- function(s) paste(toupper(substring(s, 1, 1)), {
        s <- substring(s, 2)
        if (strict) 
            tolower(s)
        else s
    }, sep = "", collapse = " ")
    sapply(strsplit(s, split = split), cap, USE.NAMES = !is.null(names(s)))
}

## 
httr::set_config(config(ssl_verifypeer = 0L));

######  Section 1 - Import ACTC Schedule 3

####  Return Schedule 3 - Section numbers, names and URLs
actc_sections <- base_url %>%
  html_session %>%
  read_html %>%
  html_nodes("a") %>%
  Map(function(x) data.frame(href = html_attr(x, "href"),
                             text = html_text(x)),
      .) %>%
  bind_rows %>%
  filter(grepl("section-\\d+\\.aspx", href)) %>%
  mutate(type = ifelse(grepl("Section\\W*\\w+", text, ignore.case=TRUE), "Number", "Name")) %>%
  spread(type, text) %>%
  mutate(section_number = sub("Section\\W*(\\w+)", "\\1", Number),
         section_name = Name) %>%
  rename(section_href = href) %>%
  select(section_href, section_number, section_name);

if (DEBUG) {
  actc_sections %>%
    write.csv(file=file.path(tempdir(), "Sections.csv"), row.names=FALSE)
}


####  Return Schedule 3 - Chapter numbers, names and hrefs
raw_actc_chapters <- actc_sections$section_href %>%
  Map(function(x) base_url %>% url_parse %>%
                  inset("path", value=x) %>%
                  url_compose %>%
                  html_session %>%
                  read_html %>%
                  html_nodes("a"),
      .);

actc_chapters <- Map(function(x, i) data.frame(section_href = i,
                                          href = html_attr(x, "href"),
                                          text = html_text(x)),
                raw_actc_chapters, names(raw_actc_chapters)) %>%
  bind_rows %>%
  filter(grepl("chapter-\\d+\\.aspx$", href)) %>%
  mutate(type = ifelse(grepl("Chapter\\W*\\w+", text, ignore.case=TRUE), "Number", "Name")) %>%
  distinct(section_href, href, text, type) %>%              ## Remove duplicates
  filter(!grepl("^\\d+$", text) & !grepl("^$", text)) %>%   ## Remove blank/numeric-only text
  filter(!grepl("^actc_chapters\\W*\\d+", text)) %>%             ## Remove text == 'actc_chapters xx'
  spread(type, text) %>%
  filter(!is.na(Name)) %>%                                  ## Remove NA names
  mutate(chapter_number = sub("Chapter\\W*(\\w+)", "\\1", Number),
         chapter_name = Name) %>%
  rename(chapter_href = href) %>%
  arrange(chapter_number) %>%
  select(section_href, chapter_href, chapter_number, chapter_name);

if (DEBUG) {
  actc_chapters %>% 
    write.csv(file=file.path(tempdir(), "actc_chapters.csv"), row.names=FALSE)
}

  
####  Return Schedule 3 - Tables
Tables <- 
  Map(function(x)
    base_url %>% url_parse %>%
    inset("path", value=x) %>%
    url_compose %>%
    html_session %>%
    read_html %>%
    html_nodes("table") %>%
    html_table(fill=TRUE) %>%
    ## Rename table columns - note use of seq_along(..) to allow for column-varying tables
    lapply(., 
           function(y)
             y %>% set_names(c("reference_number","statistical_code","unit","goods","rate",
                               "X_1","X_2")[seq_along(names(.))]) %>%
             mutate(chapter_href = x) %>%
             mutate_at(vars(statistical_code, reference_number), funs(as.character))
           ),
    actc_chapters$chapter_href) %>%
  unlist(recursive=FALSE) %>%
  bind_rows;

if (DEBUG) {
  Tables %>% 
    write.csv(file=file.path(tempdir(), "Tables.csv"), row.names=FALSE)
}


####  Create ACTC data frame
actc <- Tables %>%
  ## Join Section and Chapter numbrers/names
  left_join(actc_chapters, by="chapter_href") %>%
  left_join(actc_sections, by="section_href") %>%
  ## Create 8-character reference_number
  mutate(clean_reference_number = reference_number %>%
           gsub("\\.", "", .),
         tmp_reference_number = clean_reference_number %>%
           stringr::str_pad(width=8, side="right", pad="0")) %>%
  ## Clean up 'goods'' text: i) remove leading '-';  ii) remove trailing ':';
  ##      iii) capitalise first letter; iv) remove \r\n and multiple whitespace (also in 'rate')
  mutate_at(vars(goods), funs(sub("^\\s*-*\\s*(.+)$", "\\1", .))) %>%
  mutate_at(vars(goods), funs(sub("^(.+):$", "\\1", .))) %>%
  mutate_at(vars(goods), funs(paste0(toupper(substring(., 1, 1)),
                                    tolower(substring(., 2))))) %>%
  mutate_at(vars(goods, rate), funs(gsub("\\s+", " ", .))) %>%
  mutate(heading_number = tmp_reference_number %>% gsub(".{4}$", "", .),
         subheading_number_5digit = tmp_reference_number %>% gsub(".{3}$", "", .),
         subheading_number_6digit = tmp_reference_number %>% gsub(".{2}$", "", .),
         heading_name = ifelse(grepl("0{4}$", tmp_reference_number), goods, NA_character_) %>%
           zoo::na.locf(na.rm=FALSE),
         subheading_name_5digit = ifelse(grepl("[1-9]0{3}$", tmp_reference_number), goods, NA_character_) %>%
           zoo::na.locf(na.rm=FALSE),
         subheading_name_6digit = ifelse(grepl("[1-9]0{2}$", tmp_reference_number), goods, NA_character_) %>%
           zoo::na.locf(na.rm=FALSE),
         commodity_name = paste(heading_name, subheading_name_5digit, subheading_name_6digit,
                                sep="; ") %>% gsub("; NA", "", .))
## Clean and refine 
actc <- actc %>%
  filter(nchar(actc$clean_reference_number) == 8) %>%
  mutate(reference_number = tmp_reference_number %>% as.integer) %>%
  rename(notes = X_1) %>%
  select(section_number, section_name,
         chapter_number, chapter_name,
         heading_number, heading_name,
         subheading_number_5digit, subheading_name_5digit,
         subheading_number_6digit, subheading_name_6digit,
         reference_number, statistical_code, unit, commodity_name, rate, notes);

if (DEBUG) {
  actc %>% 
    write.csv(file=file.path(tempdir(), "ACTC.csv"), row.names=FALSE)
}

### Write results to actc_2017 data frame
actc_2017 <- actc;

###### Section 4 - Write data sets files
usethis::use_data(actc_2017, overwrite=TRUE);

## -------------------------------- EOF ---------------------------------------

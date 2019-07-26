#' ---
#' filename:     build-actc-2019.R
#' created:      2017-09-28
#' updated:      <2019-07-26 11:32:16 david at grover>
#' author:       David Mitchell <david.p.mitchell@homemail.com.au>
#' description:  Build Australian Customs Tariff Classification
#'               Script automatically downloads the ACTC from the DIBP
#'               Tariff Classification (Schedule 3)
#' notes:        Check `base_url` is still current
#' ---

### Set ACTC base URL
base_url <- "https://www.abf.gov.au/importing-exporting-and-manufacturing/tariff-classification/current-tariff/schedule-3"

######  Section 0 - Libraries & settings
library(magrittr);
library(rvest);        ## Web scraping functions
library(urltools);     ## URL parsing and composing
library(XML);          ## 
library(httr);         ## 
library(readxl);       ## Read Excel files
library(dplyr);
library(tidyr);
library(purrr);

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

## Turn off SSL verification
## Avoids following error:  'SSL certificate problem: unable to get local issuer certificate'
httr::set_config(config(ssl_verifypeer = 0L));


######  Section 1 - Import ACTC Schedule 3
####  Return Schedule 3 section numbers, names and URLs
actc_sections <- base_url %>%
  html_session %>%
  read_html %>%
  html_nodes("a") %>%
  Map(function(x) data.frame(section_href = html_attr(x, "href") %>% trimws,
                             text = html_text(x) %>% trimws),
      .) %>%
  bind_rows %>%
  filter(grepl("section-[mdclxvi]+$", section_href, ignore.case=TRUE, perl=TRUE)) %>%
  mutate(type = ifelse(grepl("^(?=[mdclxvi])m*(c[md]|d?c{0,3})(x[cl]|l?x{0,3})(i[xv]|v?i{0,3})$",
                             text, ignore.case=TRUE, perl=TRUE),
                       "section_number", "section_name")) %>%
  spread(type, text) %>%
  mutate_at(vars(section_number), ~ as.roman(.) %>% as.numeric) %>%
  select(section_href, section_number, section_name);

if (DEBUG)
  actc_sections %>% write.csv(file.path(tempdir(), "actc_sections.csv"));


####  Return Schedule 3 chapter numbers, names and hrefs
raw_actc_chapters <- actc_sections$section_href %>%
  Map(function(x) {
    cat(x, "\n");
    base_url %>%
      url_parse %>%
      inset("path", value=x) %>%
      url_compose %>%
      html_session %>%
      read_html %>%
      html_nodes("dl.dl-horizontal") %>% ## Return only dl objects with class="dl-horizontal"
      html_nodes("a")                    ## Return only href links
  }, .);


## Process chapter information into data frame
actc_chapters <-
  Map(function(x, i) data.frame(section_href = i,
                                chapter_href = html_attr(x, "href") %>% trimws,
                                chapter_text = html_text(x) %>% trimws),
      raw_actc_chapters, names(raw_actc_chapters)) %>%
  bind_rows %>%
  filter(grepl("chapter-\\d+$", chapter_href, ignore.case=TRUE)) %>%
  mutate(type = ifelse(grepl("chapter\\s*\\d+", chapter_text, ignore.case=TRUE),
                       "chapter_number", "chapter_name")) %>%
  distinct(section_href, chapter_href, chapter_text, type) %>%  ## Remove duplicates
  spread(type, chapter_text) %>%
  mutate_at(vars(chapter_number),
            ~ sub("^chapter\\s*(\\d+)", "\\1", ., ignore.case=TRUE) %>%
              as.integer) %>%
  arrange(chapter_number) %>%
  select(section_href, chapter_href, chapter_number, chapter_name);

if (DEBUG)
  actc_chapters %>% 
    write.csv(file=file.path(tempdir(), "actc_chapters.csv"), row.names=FALSE);

 
####  Return Schedule 3 tables
source("rvest-table-bug-fix.R");  ## Note: Requires bug fixed version of html_table from source
raw_actc_tables <- actc_chapters$chapter_href %>%
  Map(function(x) {
    cat(x, "\n");
    base_url %>%
      url_parse %>%
      inset("path", value=x) %>%
      url_compose %>%
      html_session %>%
      read_html %>%
      html_nodes("table") %>%
      html_table(fill=TRUE)
  }, .);


## Process table information into single data frame
actc_table <- raw_actc_tables %>%
  set_names(sprintf("%s_", names(.))) %>%  ## Modify list element names to clean later
  unlist(recursive = FALSE) %>%            ## Unlist modifies names 
  Map(function(x, y)
    x %>% set_names(c("reference_number", "statistical_code", "unit",
                      "goods", "rate", "tariff_concession_orders",
                      paste("x", 1:10, sep="_"))[seq_along(names(.))]) %>%
    mutate(chapter_href = y) %>%
    mutate_at(vars(statistical_code, reference_number), as.character),
    ., names(.)) %>%
  bind_rows %>%
  mutate_at(vars(chapter_href), ~ sub("_\\d+$", "", .)) %>% ## Re-clean chapter_ref names
  select(-matches("^x_\\d+", ignore.case=TRUE));

if (DEBUG)
  actc_table %>%
    write.csv(file=file.path(tempdir(), "actc_table.csv"), row.names=FALSE);

####  Create ACTC data frame
actc <- actc_table %>%
  ## Join Section and Chapter numbers/names
  left_join(actc_chapters, by="chapter_href") %>%
  left_join(actc_sections, by="section_href") %>%
  ## Create 8-character reference_number:
  ##   i) remove all periods (.) from reference_number
  ##  ii) remove trailing asterisks (*) from reference_number
  mutate(clean_reference_number = reference_number %>%
           gsub("\\.", "", .) %>%
           gsub("\\W", "", .) %>%
           trimws,
         tmp_reference_number = clean_reference_number %>%
           stringr::str_pad(width=8, side="right", pad="0")) %>%
  ## Clean up 'goods' text:
  ##   i) remove leading '-';
  ##  ii) remove trailing ':';
  ## iii) capitalise first letter;
  ##  iv) remove \r\n and multiple whitespace (also in 'rate').
  mutate_at(vars(goods), ~ sub("^\\s*-*\\s*(.+)$", "\\1", .)) %>%
  mutate_at(vars(goods), ~ sub("^(.+):$", "\\1", .)) %>%
  mutate_at(vars(goods), ~ paste0(toupper(substring(., 1, 1)),
                                  tolower(substring(., 2)))) %>%
  mutate_at(vars(goods, rate), ~ gsub("\\s+", " ", .)) %>%
  mutate(heading_number = tmp_reference_number %>% gsub(".{4}$", "", .),
         subheading_number_5digit = tmp_reference_number %>% gsub(".{3}$", "", .),
         subheading_number_6digit = tmp_reference_number %>% gsub(".{2}$", "", .),
         heading_name = ifelse(grepl("0{4}$", tmp_reference_number), goods, NA_character_) %>%
           zoo::na.locf(na.rm=FALSE),
         subheading_name_5digit = ifelse(grepl("[1-9]0{3}$", tmp_reference_number),
                                         goods, NA_character_) %>%
           zoo::na.locf(na.rm=FALSE),
         subheading_name_6digit = ifelse(grepl("[1-9]0{2}$", tmp_reference_number),
                                         goods, NA_character_) %>%
           zoo::na.locf(na.rm=FALSE),
         commodity_name = paste(heading_name, subheading_name_5digit, subheading_name_6digit,
                                sep="; ") %>%
           gsub("; NA", "", .)) %>%
  mutate_at(vars(contains("_number")), as.integer) %>%  ## Convert all *_number columns to integer ..
  drop_na(contains("_number"));                         ## .. and remove any NA entries


## Clean and finalise ACTC data frame 
actc %<>%
  filter(nchar(actc$clean_reference_number) == 8) %>%
  mutate(reference_number = tmp_reference_number %>% as.integer) %>%
  ## rename(notes = X_1) %>%
  select(section_number, section_name,
         chapter_number, chapter_name,
         heading_number, heading_name,
         subheading_number_5digit, subheading_name_5digit,
         subheading_number_6digit, subheading_name_6digit,
         reference_number, statistical_code, unit,
         commodity_name, rate, tariff_concession_orders);



## Append Schedule 4 codes
### Load Schedule 4 codes (from separate file)
actc_2019_schedule4 <- read.csv("ACTC-Schedule-IV-Jul2019.csv", colClasses="character",
                                stringsAsFactors=FALSE) %>%
  mutate_at(vars(contains("_number")), as.integer);
### Bind Schedule 3 & Schedule 4
actc <- actc_2019 %>%
  mutate_at(vars(contains("number")), as.integer) %>%
  drop_na(contains("number")) %>%     ## .. and remove any NA entries
  bind_rows(actc_2019_schedule4);


if (DEBUG) ## Check results
  actc %>%
    select(contains("number"), contains("name")) %>%
    write.csv(file=file.path(tempdir(), "actc.csv"), row.names=FALSE);


### Write results to actc_2019 data frame
actc_2019 <- actc;

###### Section 4 - Write data sets files
usethis::use_data(actc_2019, overwrite=TRUE);

## -------------------------------- EOF ---------------------------------------

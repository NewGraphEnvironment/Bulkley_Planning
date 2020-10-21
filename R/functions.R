conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'postgis',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = 'postgres'
)

##make a function to retrieve the watershed info 
get_watershed <- function(fish_habitat_info){
  mapply(fwapgr::fwa_watershed, blue_line_key = fish_habitat_info$blue_line_key,
         downstream_route_measure = fish_habitat_info$downstream_route_measure) %>%
    purrr::set_names(nm = fish_habitat_info$pscis_model_combined_id) %>%
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="pscis_model_combined_id") %>%
    distinct(pscis_model_combined_id, .keep_all = T) %>% ##there are duplicates we should get rid of
    st_as_sf()
}


##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}
##function to import pscis info
import_pscis <- function(workbook_name = 'pscis_phase1.xlsm'){ ##new template.  could change file back to .xls
  readxl::read_excel(path = paste0(getwd(),"/data/", workbook_name), 
                     sheet = 'PSCIS Assessment Worksheet') %>% 
    # purrr::set_names(janitor::make_clean_names(names(.))) %>% 
    at_trim_xlsheet2() %>% ##recently added function above and pulled the altools package as it was a week link
    rename(date = date_of_assessment_yyyy_mm_dd) %>% 
    mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>% 
    filter(!is.na(date)) %>% 
    readr::type_convert()  ##guess the type!!
}


import_fish_data <- function(workbook_name = 'habitat_assessments.xls'){
  readxl::excel_sheets(path = paste0(getwd(),"/data/", workbook_name)) %>% 
  purrr::set_names() %>% 
  purrr::map(readxl::read_excel, 
             path = paste0(getwd(),"/data/", workbook_name), 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(readr::type_convert)
}

##the colors don't seem to work yet.  Might need to put a case_when for the actual google icon symbol.  Posting cutom symbols on a url and pointing to them will work too.
make_kml_col <- function(df){
  df %>% 
    mutate(pscis_crossing_id = as.integer(pscis_crossing_id),
           my_crossing_reference = as.integer(my_crossing_reference),
           color = case_when(barrier_result == 'Barrier' ~ 'red',
                             barrier_result == 'Passable' ~ 'green',
                             barrier_result == 'Potential' ~ 'purple',
                             T ~ 'white'),
           color = plotKML::col2kml(color),
           site_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                               is.na(pscis_crossing_id) ~ my_crossing_reference),
           label = paste(site_id, stream_name, barrier_result, habitat_value, sep = '-')) 
  # mutate(across(where(is.numeric), round(.,2)))
  
}

##this is how we make html tables.  Can add colors or whatever -https://stackoverflow.com/questions/50199845/converting-dataframe-in-required-html-table-format-in-r
make_html_tbl <- function(df) {
  df %>% 
    mutate(html_tbl = knitr::kable(df) %>% 
             # All cells get a border
             row_spec(0:nrow(df), extra_css = "border: 1px solid black;") %>% 
             row_spec(0, background = "yellow")) # header row in blue)
}


## add a line to the function to make the comments column wide enough
make_html_tbl_hab <- function(df) {
  df %>% 
    mutate(html_tbl = knitr::kable(df) %>% 
             kableExtra::row_spec(0:nrow(df), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>% 
             kableExtra::column_spec(column = 7, width_min = '4in')
    )
}

##here is how we get the watershed codes - thanks Simon!!!!
wsheds <- dbGetQuery(conn, "SELECT DISTINCT ON (pscis_model_combined_id)
    a.pscis_model_combined_id, 
    a.linear_feature_id,
    b.watershed_code_50k,
    substring(b.watershed_code_50k from 1 for 3)
      ||'-'||substring(b.watershed_code_50k from 4 for 6)
      ||'-'||substring(b.watershed_code_50k from 10 for 6)
      ||'-'||substring(b.watershed_code_50k from 16 for 6)
      ||'-'||substring(b.watershed_code_50k from 24 for 6)
      ||'-'||substring(b.watershed_code_50k from 30 for 6)
      ||'-'||substring(b.watershed_code_50k from 36 for 6) as watershed_code_50k_parsed,
    b.blue_line_key_20k,
    b.watershed_key_20k,
    b.blue_line_key_50k,
    b.watershed_key_50k,
    b.match_type
FROM fish_passage.pscis_model_combined a
LEFT OUTER JOIN whse_basemapping.fwa_streams_20k_50k b
ON a.linear_feature_id = b.linear_feature_id_20k
WHERE a.watershed_group_code IN ('BULK','MORR')
ORDER BY a.pscis_model_combined_id, b.match_type;")

DBI::dbDisconnect(conn = conn)
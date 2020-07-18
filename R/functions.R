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
import_pscis <- function(workbook_name = 'PSCIS_Assessment_Form_OCBAB0614_FP.xls'){ ##new template.  could change file back to .xls
  readxl::read_excel(path = paste0(getwd(),"/data/", workbook_name), 
                     sheet = 'PSCIS Assessment Worksheet') %>% 
    # purrr::set_names(janitor::make_clean_names(names(.))) %>% 
    at_trim_xlsheet2() %>% ##recently added function above and pulled the altools package as it was a week link
    rename(date = date_of_assessment_yyyy_mm_dd) %>% 
    mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>% 
    filter(!is.na(date))
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
#####################################################################################################################################
# Title: R script file for DHAMIS data                                                #
# Author: Tiwonge Chimpandule                                                                                                       #
#####################################################################################################################################
#load packages----
if(!require(pacman))
  install.packages("pacman")
pacman::p_load(
  janitor,
  ggplot2,
  zoo,
  plotly,
  dplyr,
  tidyr,
  data.table,
  writexl,
  summarytools,
  odbc,
  RODBC,
  DBI,
  feather,
  Hmisc,
  doParallel,
  foreach,
  RSQLite,
  readr,
  lubridate,
  janitor,
  tidyverse,
  forcats,
  openxlsx,
  esquisse,
  lme4,
  sjPlot,
  here
)


# connection to offline db----


con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "ODBC Driver 11 for SQL Server", 
                      Server = "(localdb)\\MSSQLLocalDB", 
                      Database = "HIVData-live", 
                      Authentication = "Windows")

# Load data----
# The copy of pulled data is available in the data folder

code_hfacility <-
  dbGetQuery(
    con,"SELECT DISTINCT code_hdepartment.ID,
                concept_1.concept_name AS District,
                code_hfacility.hfacility_name AS Facility,
                code_hfacility.Site_code,
                code_hfacility.gps_y AS Lat,
                code_hfacility.gps_x AS [Long],
                code_hdepartment.eds_start,
                concept_2.concept_name AS pepfar_ip,
                map_hdepartment_ID_IP.OrgUnit,
                concept_3.concept_name AS hfactype,
                concept_4.concept_name AS rururb,
                concept_5.concept_name AS hservice,
                concept_6.concept_name AS hauthority,
                concept_7.concept_name AS hsector,
                concept_8.concept_name AS freepay,
                concept_9.concept_name AS hservmode,
                concept_10.concept_name AS electronicsysm
FROM (((((((((map_hdepartment_ID_IP
        RIGHT JOIN (((code_hdepartment
        RIGHT JOIN code_hfacility ON code_hdepartment.hfacility_id = code_hfacility.ID)
        LEFT JOIN concept ON code_hdepartment.hservice = concept.ID)
        LEFT JOIN concept AS concept_1 ON code_hfacility.district = concept_1.ID) ON map_hdepartment_ID_IP.hdepartment_id = code_hdepartment.ID)
        LEFT JOIN concept AS concept_2 ON map_hdepartment_ID_IP.pepfar_ip = concept_2.ID)
        INNER JOIN concept AS concept_3 ON code_hfacility.hfactype = concept_3.ID)
        INNER JOIN concept AS concept_4 ON code_hfacility.rururb = concept_4.ID)
        LEFT JOIN concept AS concept_5 ON code_hdepartment.hservice = concept_5.ID)
        LEFT JOIN concept AS concept_6 ON code_hdepartment.hauthority = concept_6.ID)
        LEFT JOIN concept AS concept_7 ON code_hdepartment.hsector = concept_7.ID)
        LEFT JOIN concept AS concept_8 ON code_hdepartment.freepay = concept_8.ID)
        LEFT JOIN concept AS concept_9 ON code_hdepartment.hservmode = concept_9.ID)
        LEFT JOIN concept AS concept_10 ON code_hdepartment.electronic_sys = concept_10.ID
WHERE concept.concept_name = 'ART'
ORDER BY concept_1.concept_name, code_hfacility.hfacility_name;

")

# file_path <- here("data", "code_hfacility.xlsx")
# write_xlsx(code_hfacility, path = file_path)

# Data prepalation----

code_hfacility <- code_hfacility %>%
  mutate_at(vars(hfactype, rururb, hservice, hservmode, hauthority, freepay, hservmode, electronicsysm,District,pepfar_ip,hsector), as.factor)











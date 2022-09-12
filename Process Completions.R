# ======================================================================
# FUNCTIONS
# ======================================================================

# ----------------------------------------------------------------------
#Total number of degrees awarded by a University's Department per year.
# ----------------------------------------------------------------------

#' Number of Degrees Awarded by a University's Department per Year
#' 
#' @description 
#' Computes the number of degrees awarded by a department
#' 
#' @details 
#' Data is analyzed from https://nces.ed.gov/ipeds/use-the-data >> Survey Data
#' (Complete Data Files) >> Completions. 
#' 
#' Relevant filters: 
#' UNITID = 110635 (UC Berkeley)
#' CIPCODE = 23.0101 (English Language and Literature, General)
#' AWLEVEL =  5 (Bachelor's Degrees)
#' 
#' @param IPEDS.ID vector of IPEDS IDs for universities of interest. 
#' @param department.CIP the CIP code for a department with decimals removed. 
#' @param data_directory directory of downloaded data.
#' @param full.data if TRUE, gathers the complete range of data (i.e. 1984-2020). 
#' If FALSE, only the range 2000-2020 is gathered.
#' 
#' @examples
#' degrees_awarded()
#' 
degrees_awarded <- function (IPEDS.ID = c(110635), 
                             department.CIP = 230101, 
                             deg_type = c("bachelors", "masters", "doctorate"),
                             data_directory = "data/IPEDS_Completions/",
                             full.data = FALSE) {

  deg_type = match.arg(deg_type)
  
  if (deg_type == "bachelors") {aw_level = 5} else 
    if (deg_type == "masters") {aw_level = 7} else 
    if (deg_type == "doctorate") {aw_level = c(17, 18, 19)} else
    {aw_level = c(5, 7, 17, 18, 19)}
  
  # create an empty data frame if a year returns no results
  empty_df <- function(id, year) {
    df = data.frame(UNITID = id, 
                    n.students = rep(0,length(id)),
                    year = rep(year,length(id)))
  }
  
  # ----------------------------------------------------------------------
  # 2011 - 2020
  # ----------------------------------------------------------------------
  total_degs <- data.frame(matrix(ncol = 3, nrow = 0))
  
  for (y in c(2011:2020)){
    file_name <- paste0(data_directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(UNITID %in% IPEDS.ID,
                    CIPCODE == department.CIP,  
                    AWLEVEL %in% aw_level)  %>% # code for bachelor's degree
      dplyr::group_by(UNITID) %>%
      # Var definition (CTOTAL): Grand total
      dplyr::summarise(n.students = sum(CTOTALT)) %>%
      dplyr::mutate(year = y)
    
    # if no results are returned (empty df), return a data frame with 0 students
    if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
    total_degs <- rbind(total_degs, df)
  }
  
  # ----------------------------------------------------------------------
  # 2008 - 2010
  # ----------------------------------------------------------------------
  # Note that before 2011, doctoral degrees are classified under the award
  # level 9. See https://nces.ed.gov/ipeds/report-your-data/data-tip-sheet-reporting-graduate-awards
  # for more. 
  if (deg_type == "doctorate") {aw_level = c(9)}
  
  for (y in c(2008:2010)){
    file_name <- paste0(data_directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(UNITID %in% IPEDS.ID,
                    CIPCODE == department.CIP,  
                    AWLEVEL %in% aw_level) %>%
      dplyr::group_by(UNITID) %>%
      # Var definition (CTOTAL): Grand total
      dplyr::summarise(n.students = sum(CTOTALT)) %>%
      dplyr::mutate(year = y)
    
    # if no results are returned (empty df), return a data frame with 0 students
    if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
    total_degs <- rbind(total_degs, df)
  }
  
  # ----------------------------------------------------------------------
  # 2003 - 2007
  # ----------------------------------------------------------------------
  
  for (y in c(2003:2007)) {
    file_name <- paste0(data_directory, "c",  
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    names(df)[1] <- "UNITID"
    names(df)[2] <- "CIPCODE"
    names(df)[4] <- "AWLEVEL"
    names(df)[52] <- "CRACE24"
    
    df <- df %>%
      dplyr::filter(UNITID %in% IPEDS.ID,
                    CIPCODE == department.CIP,  
                    AWLEVEL %in% aw_level) %>% 
      dplyr::group_by(UNITID) %>%
      # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
      # across all race/ethnicity and both genders
      dplyr::summarise(n.students = sum(CRACE24)) %>%
      dplyr::mutate(year = y)
    
    # if no results are returned (empty df), return a data frame with 0 students
    if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
    total_degs <- rbind(total_degs, df)
  }
  
  # ----------------------------------------------------------------------
  # 2002
  # ----------------------------------------------------------------------
  
  df <- data.table::fread(file = paste0(data_directory, "c2002_a_data_stata.csv"), 
          na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                         '"H"', '"J"', '"K"', '"L"', '"N"',
                         '"P"', '"R"', '"Z"', "NA"))
  df <- df %>%
    dplyr::filter(UNITID %in% IPEDS.ID,
                  CIPCODE == department.CIP,  
                  AWLEVEL %in% aw_level) %>%
    dplyr::group_by(UNITID) %>%
    # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
    # across all race/ethnicity and both genders
    dplyr::summarise(n.students = sum(CRACE24)) %>%
    dplyr::mutate(year = 2002)
  
  # if no results are returned (empty df), return a data frame with 0 students
  if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
  total_degs <- rbind(total_degs, df)

  # ----------------------------------------------------------------------
  # 2000 - 2001
  # ----------------------------------------------------------------------
  
  for (y in c(2000:2001)) {
    file_name <- paste0(data_directory, "c",  
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
                            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                                           '"H"', '"J"', '"K"', '"L"', '"N"',
                                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(unitid %in% IPEDS.ID,
                    cipcode == department.CIP,  
                    awlevel %in% aw_level) %>%
      dplyr::group_by(unitid) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
      # match case of unitid so that `rbind` works
      dplyr::rename(UNITID = unitid) %>%
      dplyr::mutate(year = y)

    # if no results are returned (empty df), return a data frame with 0 students
    if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
    total_degs <- rbind(total_degs, df)
  }
  
  # ======================================================================
  # Pre-2000 data
  # ======================================================================
  
  if(full.data){
    # ----------------------------------------------------------------------
    # 1995 - 1999
    # ----------------------------------------------------------------------
    
    for (y in c(95:99)) {
      file_name <- paste0(data_directory, "c",  
                          as.character(y-1), as.character(y), "_a_data_stata.csv")
      df <- data.table::fread(file = file_name, 
              na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                             '"H"', '"J"', '"K"', '"L"', '"N"',
                             '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        dplyr::filter(unitid %in% IPEDS.ID,
                      cipcode == department.CIP,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid) %>%
        dplyr::mutate(year = 1900+y)
      
      # if no results are returned (empty df), return a data frame with 0 students
      if (nrow(df) == 0) {df = empty_df(IPEDS.ID, 1900+y)}
      total_degs <- rbind(total_degs, df)
    }
    
    # ----------------------------------------------------------------------
    # 1991 - 1994
    # ----------------------------------------------------------------------
    
    for (y in c(1991:1994)) {
      file_name <- paste0(data_directory, "c", 
                          as.character(y), "_cip_data_stata.csv")
      df <- data.table::fread(file = file_name, 
              na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                             '"H"', '"J"', '"K"', '"L"', '"N"',
                             '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        dplyr::filter(unitid %in% IPEDS.ID,
                      cipcode == department.CIP,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid) %>%
        dplyr::mutate(year = y)
      
      # if no results are returned (empty df), return a data frame with 0 students
      if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
      total_degs <- rbind(total_degs, df)
    }
    
    # ----------------------------------------------------------------------
    # 1990
    # ----------------------------------------------------------------------
    
    df <- data.table::fread(file = paste0(data_directory, "c8990cip_data_stata.csv"), 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(unitid %in% IPEDS.ID,
                    cipcode == department.CIP,  
                    awlevel %in% aw_level) %>%
      dplyr::group_by(unitid) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
      # match case of unitid so that `rbind` works
      dplyr::rename(UNITID = unitid) %>%
      dplyr::mutate(year = 1990)
    
    # if no results are returned (empty df), return a data frame with 0 students
    if (nrow(df) == 0) {df = empty_df(IPEDS.ID, 1990)}
    total_degs <- rbind(total_degs, df)
    
    # ----------------------------------------------------------------------
    # 1984 - 1989
    # ----------------------------------------------------------------------
    
    for (y in c(1984:1989)) {
      file_name <- paste0(data_directory, "c", 
                          as.character(y), "_cip_data_stata.csv")
      df <- data.table::fread(file = file_name, 
              na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                             '"H"', '"J"', '"K"', '"L"', '"N"',
                             '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        dplyr::filter(unitid %in% IPEDS.ID,
                      cipcode == department.CIP,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid) %>%
        dplyr::mutate(year = y)
      
      # if no results are returned (empty df), return a data frame with 0 students
      if (nrow(df) == 0) {df = empty_df(IPEDS.ID, y)}
      total_degs <- rbind(total_degs, df)
    }
  }
  total_degs %>% arrange(-year)
}

zzz <- degrees_awarded(deg_type = "doctorate", full.data = TRUE)
zzz

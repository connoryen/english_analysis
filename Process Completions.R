# ----------------------------------------------------------------------
# Packages
# ----------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table) # add view() function
library(roxygen2) # function documentation

# ======================================================================
# FUNCTIONS
# ======================================================================

# ----------------------------------------------------------------------
#Total number of BA degrees awarded by a University's Department per year.
# ----------------------------------------------------------------------

#' Number of Degrees Awarded by a University's Department per Year
#' 
#' @description 
#' Computes the number of degrees awarded by a department from 1984 to 2020. 
#' 
#' @details 
#' Data is analyzed from ps://nces.ed.gov/ipeds/use-the-data >> Survey Data >> 
#' Complete Data Files >> Completions. 
#' 
#' Relevant filters: 
#' UNITID = 110635 (UC Berkeley)
#' CIPCODE = 23.0101 (English Language and Literature, General)
#' AWLEVEL =  5 (Bachelor's Degrees)
#' 
#' @param IPEDS.ID vector of IPEDS IDs for universities of interest. 
#' @param department.CIP the CIP code for a department with decimals removed. 
#' @param department.name name of department for writing csv.
#' @param full.data if TRUE, gathers the complete range of data (i.e. 1984-2020). 
#' If FALSE, only the range 2002-2020 is gathered 
#' @param return.R if TRUE, returns the data frame in R. 
#' @param save if TRUE, saves file as csv in the specified directory. 
#' @param directory of downloaded data.
#' 
#' @examples
#' degrees_awarded()
#' 
degrees_awarded <- function (IPEDS.ID = c(110635), 
                             department.CIP = 230101, 
                             department.name = "English",
                             full.data = FALSE, 
                             return.R = TRUE, save = FALSE,
                             data.directory = "data/Completions/") {
  
  # ----------------------------------------------------------------------
  # 2008 - 2020
  # ----------------------------------------------------------------------
  bachelors_degrees_awarded <- data.frame(matrix(ncol = 3, nrow = 0))
  
  for (y in c(2020:2008)){
    file_name <- paste0(data.directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- fread(file = file_name, 
                na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                               '"H"', '"J"', '"K"', '"L"', '"N"',
                               '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      filter(UNITID %in% IPEDS.ID,
             CIPCODE == department.CIP,  
             AWLEVEL == 5)  %>% # code for bachelor's degree
      group_by(UNITID) %>%
      # Var definition (CTOTAL): Grand total
      summarise(n.bachelors = sum(CTOTALT)) %>%
      mutate(year = y)
    
    bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
  }
  
  # ----------------------------------------------------------------------
  # 2003 - 2007
  # ----------------------------------------------------------------------
  
  for (y in c(2007:2003)) {
    file_name <- paste0(data.directory, "c",  
                        as.character(y), "_a_data_stata.csv")
    df <- fread(file = file_name, 
                na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                               '"H"', '"J"', '"K"', '"L"', '"N"',
                               '"P"', '"R"', '"Z"', "NA"))
    names(df)[1] <- "UNITID"
    names(df)[2] <- "CIPCODE"
    names(df)[4] <- "AWLEVEL"
    names(df)[52] <- "CRACE24"
    
    df <- df %>%
      filter(UNITID %in% IPEDS.ID,
             CIPCODE == department.CIP,  
             AWLEVEL == 5)  %>% # code for bachelor's degree
      group_by(UNITID) %>%
      # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
      # across all race/ethnicity and both genders
      summarise(n.bachelors = sum(CRACE24)) %>%
      mutate(year = y)
    
    bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
  }
  
  # ----------------------------------------------------------------------
  # 2002
  # ----------------------------------------------------------------------
  
  df <- fread(file = paste0(data.directory, "c2002_a_data_stata.csv"), 
              na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                             '"H"', '"J"', '"K"', '"L"', '"N"',
                             '"P"', '"R"', '"Z"', "NA"))
  df <- df %>%
    filter(UNITID %in% IPEDS.ID,
           CIPCODE == department.CIP,  
           AWLEVEL == 5)  %>% # code for bachelor's degree
    group_by(UNITID) %>%
    # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
    # across all race/ethnicity and both genders
    summarise(n.bachelors = sum(CRACE24)) %>%
    mutate(year = 2002)
  
  bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
  
  # ======================================================================
  # Pre-2002 data
  # ======================================================================
  
  if(full.data){
    # ----------------------------------------------------------------------
    # 2000 - 2001
    # ----------------------------------------------------------------------
    
    for (y in c(2001:2000)) {
      file_name <- paste0(data.directory, "c",  
                          as.character(y), "_a_data_stata.csv")
      df <- fread(file = file_name, 
                  na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                                 '"H"', '"J"', '"K"', '"L"', '"N"',
                                 '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        filter(UNITID %in% IPEDS.ID,
               CIPCODE == department.CIP,  
               AWLEVEL == 5)  %>% # code for bachelor's degree
        group_by(UNITID) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        summarise(n.bachelors = sum(crace15) + sum(crace16)) %>%
        mutate(year = y)
      
      bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
    }
    
    # ----------------------------------------------------------------------
    # 1995 -1999
    # ----------------------------------------------------------------------
    
    for (y in c(99:95)) {
      file_name <- paste0(data.directory, "c",  
                          as.character(y-1), as.character(y), "_a_data_stata.csv")
      df <- fread(file = file_name, 
                  na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                                 '"H"', '"J"', '"K"', '"L"', '"N"',
                                 '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        filter(UNITID %in% IPEDS.ID,
               CIPCODE == department.CIP,  
               AWLEVEL == 5)  %>% # code for bachelor's degree
        group_by(UNITID) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        summarise(n.bachelors = sum(crace15) + sum(crace16)) %>%
        mutate(year = y)
      
      bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
    }
    
    # ----------------------------------------------------------------------
    # 1991 - 1994
    # ----------------------------------------------------------------------
    
    for (y in c(1994:1991)) {
      file_name <- paste0(data.directory, "c", 
                          as.character(y), "_cip_data_stata.csv")
      df <- fread(file = file_name, 
                  na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                                 '"H"', '"J"', '"K"', '"L"', '"N"',
                                 '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        filter(UNITID %in% IPEDS.ID,
               CIPCODE == department.CIP,  
               AWLEVEL == 5)  %>% # code for bachelor's degree
        group_by(UNITID) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        summarise(n.bachelors = sum(crace15) + sum(crace16)) %>%
        mutate(year = y)
      
      bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
    }
    
    # ----------------------------------------------------------------------
    # 1990
    # ----------------------------------------------------------------------
    
    df <- fread(file = paste0(data.directory, "c8990cip_data_stata.csv"), 
                na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                               '"H"', '"J"', '"K"', '"L"', '"N"',
                               '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      filter(UNITID %in% IPEDS.ID,
             CIPCODE == department.CIP,  
             AWLEVEL == 5)  %>% # code for bachelor's degree
      group_by(UNITID) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      summarise(n.bachelors = sum(crace15) + sum(crace16)) %>%
      mutate(year = 1990)
    
    bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
    
    # ----------------------------------------------------------------------
    # 1984 - 1989
    # ----------------------------------------------------------------------
    
    for (y in c(1984:1989)) {
      file_name <- paste0(data.directory, "c", 
                          as.character(y), "_cip_data_stata.csv")
      df <- fread(file = file_name, 
                  na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                                 '"H"', '"J"', '"K"', '"L"', '"N"',
                                 '"P"', '"R"', '"Z"', "NA"))
      df <- df %>%
        filter(UNITID %in% IPEDS.ID,
               CIPCODE == department.CIP,  
               AWLEVEL == 5)  %>% # code for bachelor's degree
        group_by(UNITID) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        summarise(n.bachelors = sum(crace15) + sum(crace16)) %>%
        mutate(year = y)
      
      bachelors_degrees_awarded <- rbind(bachelors_degrees_awarded, df)
    }
  }
  
  
  # ----------------------------------------------------------------------
  # names
  # ----------------------------------------------------------------------
  
  if(save){
    fileName <- paste0("data cleaning/", department.name,
                       "-bachelors-degrees-awarded.csv")
    write.csv(bachelors_degrees_awarded, fileName, row.names = FALSE)
  }
  
  if(return.R){
    bachelors_degrees_awarded
  }
}

# ----------------------------------------------------------------------
# Preliminaries
# ----------------------------------------------------------------------

# Suppress summarize info
options(dplyr.summarise.inform = FALSE)

# ----------------------------------------------------------------------
# get IDs of schools
# ----------------------------------------------------------------------

# get name and ids of inst. w/ an English PhD granting program
ids <- read.csv("data/NCES_Inst_Details/English PhD granting Universities 2017-18.csv")
ids <- ids %>% 
  dplyr::select(UnitID, Institution.Name) %>%
  dplyr::rename(id = UnitID, name = Institution.Name)

# ----------------------------------------------------------------------
# Total number of degrees awarded by a University's Department per year.
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
#' @param inst name of institution (can be a rough string). 
#' @param department.CIP the CIP code for a department with decimals removed. 
#' @param deg_type type of degree that is of interest.
#' @param data_directory directory of downloaded data.
#' @param full.data if TRUE, gathers the complete range of data (i.e. 1984-2020). 
#' If FALSE, only the range 2000-2020 is gathered.
#' 
#' @examples
#' degrees_awarded(deg_type = "doctorate", full.data = TRUE)
#' 
degrees_awarded <- function (inst = c("Berkeley"),
                             department.CIP = 230101, 
                             deg_type = c("bachelors", "masters", "doctorate"),
                             data_directory = "data/IPEDS_Completions/",
                             full.data = FALSE) {
  # ----------------------------------------------------------------------
  # Check Inputs
  # ----------------------------------------------------------------------
  aw_level <- data.frame(aw_level = c(5, 7, 17),
                         type = c("bachelors", "masters", "doctorate"))
  aw_level <- dplyr::filter(aw_level, type %in% deg_type)[["aw_level"]]
  
  
  inst_id <- paste(inst, collapse = "|")
  inst_id <- ids %>%
    dplyr::filter(grepl(inst_id, name))
  
  inst_id <- data.frame(short_name = inst) %>%
    rowwise() %>%
    mutate(inst_name = ids[["name"]][grep(short_name, ids[["name"]])],
           id = ids[["id"]][grep(short_name, ids[["name"]])])
  
  # check that inst_id is as expected
  if (nrow(inst_id) != length(inst)) stop("`inst` is invalid")
  # check if degree type is one of bachelors, masters, or doctorate
  if(! all(deg_type %in% c("bachelors", "doctorate", "masters"))) stop("check `deg_type`")
  # check if data_directory is a valid directory
  if(!dir.exists(data_directory)) stop("`data_directory` does not exist")
  
  # ----------------------------------------------------------------------
  # Preliminaries
  # ----------------------------------------------------------------------
  total_degs <- data.frame(matrix(ncol = 4, nrow = 0))
  
  # NEED DOCCUMENTATION
  fill_missing <- function(df, id, level, year){
    empty <- DescTools::CombPairs(id, level) %>%
      mutate(n.students = 0) %>%
      rename(UNITID = Var1, level = Var2)
    
    # handle cases where AWLEVEL column is in lower case
    df <- rename(df, AWLEVEL = 2)
    left_join(empty, df, by=c('UNITID'='UNITID', 'level'='AWLEVEL')) %>%
      dplyr::mutate(n.students.y = replace_na(n.students.y, 0),
                    year = year) %>%
      rowwise() %>%
      dplyr::mutate( n.students = max(n.students.x, n.students.y)) %>%
      select(-n.students.x, -n.students.y)
  }

  # ----------------------------------------------------------------------
  # 2011 - 2020
  # ----------------------------------------------------------------------
  
  for (y in c(2011:2020)){
    file_name <- paste0(data_directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(UNITID %in% inst_id[["id"]],
                    CIPCODE == department.CIP,  
                    AWLEVEL %in% aw_level)  %>% # code for bachelor's degree
      dplyr::group_by(UNITID, AWLEVEL) %>%
      # Var definition (CTOTAL): Grand total
      dplyr::summarise(n.students = sum(CTOTALT))
    
    df <- fill_missing(df, inst_id[["id"]], aw_level, y)
    total_degs <- rbind(total_degs, df)
  }
  
  # ----------------------------------------------------------------------
  # 2008 - 2010
  # ----------------------------------------------------------------------
  # Note that before 2011, doctoral degrees are classified under the award
  # level 9. See https://nces.ed.gov/ipeds/report-your-data/data-tip-sheet-reporting-graduate-awards
  # for more. 
  aw_level <- data.frame(aw_level = c(5, 7, 9),
                         type = c("bachelors", "masters", "doctorate"))
  aw_level <- dplyr::filter(aw_level, type %in% deg_type)[["aw_level"]]
  
  for (y in c(2008:2010)){
    file_name <- paste0(data_directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(UNITID %in% inst_id[["id"]],
                    CIPCODE == department.CIP,  
                    AWLEVEL %in% aw_level) %>%
      dplyr::group_by(UNITID, AWLEVEL) %>%
      # Var definition (CTOTAL): Grand total
      dplyr::summarise(n.students = sum(CTOTALT))
    
    df <- fill_missing(df, inst_id[["id"]], aw_level, y)
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
      dplyr::filter(UNITID %in% inst_id[["id"]],
                    CIPCODE == department.CIP,  
                    AWLEVEL %in% aw_level) %>% 
      dplyr::group_by(UNITID, AWLEVEL) %>%
      # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
      # across all race/ethnicity and both genders
      dplyr::summarise(n.students = sum(CRACE24))
    
    df <- fill_missing(df, inst_id[["id"]], aw_level, y)
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
    dplyr::filter(UNITID %in% inst_id[["id"]],
                  CIPCODE == department.CIP,  
                  AWLEVEL %in% aw_level) %>%
    dplyr::group_by(UNITID, AWLEVEL) %>%
    # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
    # across all race/ethnicity and both genders
    dplyr::summarise(n.students = sum(CRACE24))
  
  df <- fill_missing(df, inst_id[["id"]], aw_level, 2002)
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
      dplyr::filter(unitid %in% inst_id[["id"]],
                    cipcode == department.CIP,  
                    awlevel %in% aw_level) %>%
      dplyr::group_by(unitid, awlevel) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
      # match case of unitid so that `rbind` works
      dplyr::rename(UNITID = unitid)

    df <- fill_missing(df, inst_id[["id"]], aw_level, y)
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
        dplyr::filter(unitid %in% inst_id[["id"]],
                      cipcode == department.CIP,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid, awlevel) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid)
      
      df <- fill_missing(df, inst_id[["id"]], aw_level, 1900+y)
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
        dplyr::filter(unitid %in% inst_id[["id"]],
                      cipcode == department.CIP,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid, awlevel) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid)
      
      df <- fill_missing(df, inst_id[["id"]], aw_level, y)
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
      dplyr::filter(unitid %in% inst_id[["id"]],
                    cipcode == department.CIP,  
                    awlevel %in% aw_level) %>%
      dplyr::group_by(unitid, awlevel) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
      # match case of unitid so that `rbind` works
      dplyr::rename(UNITID = unitid)
    
    df <- fill_missing(df, inst_id[["id"]], aw_level, 1990)
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
        dplyr::filter(unitid %in% inst_id[["id"]],
                      cipcode == department.CIP,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid, awlevel) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid)
      
      df <- fill_missing(df, inst_id[["id"]], aw_level, y)
      total_degs <- rbind(total_degs, df)
    }
  }
  
  total_degs <- total_degs %>% 
    dplyr::rename(id = UNITID) %>%
    dplyr::arrange(id, -year) %>%
    # add name of institution (match based on `id`)
    dplyr::inner_join(inst_id, by = "id") %>%
    dplyr::select(-inst_name) %>% 
    # add deg type (match based on `level`)
    dplyr::inner_join(data.frame(level = c(5,7,9,17),
                          deg = c("bachelors", "masters", "doctorate", "doctorate")),
                      by = "level") %>%
    # change order
    dplyr::select(short_name, deg, year, n.students) %>%
    rename(name = short_name)
}

zzz <- degrees_awarded(inst = c("Berkeley", "Harvard"), 
                       deg_type = c("doctorate", "bachelors"), full.data = TRUE)

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
#' @param my_names name of institution (can be a rough string). 
#' @param department_cip the CIP code for a department with decimals removed. 
#' @param deg_type type of degree that is of interest.
#' @param data_directory directory of downloaded IPEDS data.
#' @param id_loc location and name of school ids.
#' @param full_data if TRUE, gathers the complete range of data (i.e. 1984-2020). 
#' If FALSE, only the range 2000-2020 is gathered.
#' 
#' @examples
#' degrees_awarded(my_names = c("Berkeley", "Harvard"), deg_type = c("doctorate"))
degrees_awarded <- function (my_names = c("Berkeley"),
                             my_ids = c(),
                             department_cip = 230101, 
                             deg_type = c("bachelors", "masters", "doctorate"),
                             data_directory = "data/IPEDS_Completions/",
                             id_loc = "data/NCES_Inst_Details/English PhD granting Universities 2017-18.csv",
                             full_data = FALSE) {
  
  # ----------------------------------------------------------------------
  # Preliminaries
  # ----------------------------------------------------------------------
  
  # Suppress summarize info
  options(dplyr.summarise.inform = FALSE)
  
  aw_level <- data.frame(aw_level = c(5, 7, 17),
                         type = c("bachelors", "masters", "doctorate"))
  # get the award level numbers for the selected degrees.
  aw_level <- dplyr::filter(aw_level, type %in% deg_type)[["aw_level"]]
  
  # get name and ids of institutions w/ an English PhD granting program
  ids <- read.csv(id_loc) %>% 
    dplyr::select(UnitID, Institution.Name) %>%
    dplyr::rename(id = UnitID, name = Institution.Name)
  
  if (length(my_ids) == 0){  # this is when a vector of institution names is entered
    # create a regex pattern
    inst_info <- paste(my_names, collapse = "|")
    # get the names and ids of the entered institutions
    inst_info <- ids %>%
      dplyr::filter(grepl(inst_info, name))
    # add short names to the `inst_info` dataframe which currently only contains
    # columns for the full names and ids of the entered institutions
    inst_info <- data.frame(short_name = my_names) %>%
      dplyr::rowwise() %>%
      # for each short name, find the `ids[["name"]]` (i.e. full name) of the 
      # institution who when the short name is greped with returns TRUE. 
      dplyr::mutate(inst_name = ids[["name"]][grep(short_name, ids[["name"]])],
                    id = ids[["id"]][grep(short_name, ids[["name"]])])
  } else {  # this is when a vector of institution ids is entered
    inst_info <- ids %>%
      dplyr::filter(id %in% my_ids) %>%
      dplyr::rename(inst_name = name)
  }
  
  # ----------------------------------------------------------------------
  # Check Inputs
  # ----------------------------------------------------------------------
  
  # check that one of my_ids or my_names is empty
  if(length(my_ids)*length(my_names) != 0) stop("one of `my_ids` or `my_names` must be empty")
  # check if degree type is one of bachelors, masters, or doctorate
  if(! all(deg_type %in% c("bachelors", "doctorate", "masters"))) stop("check `deg_type`")
  # check if data_directory is a valid directory
  if(!dir.exists(data_directory)) stop("`data_directory` does not exist")
  # check if id file exists
  if(!file.exists(id_loc)) stop("id_loc is invalid")
  
  # ----------------------------------------------------------------------
  # Preliminary functions
  # ----------------------------------------------------------------------
  total_degs <- data.frame(matrix(ncol = 4, nrow = 0))
  
  # For a data frame with columns for `UNITID`, `level`,`year`, `n.students`
  # with year range XXXX-YYYY, add 0 values for years with no n.students. 
  fill_missing <- function(df, id, level, year){
    # get all permutations of the vectors `id` and `level` and add the column
    # `n.students` with value 0. 
    empty <- DescTools::CombPairs(id, level) %>%
      mutate(n.students = 0) %>%
      rename(UNITID = Var1, level = Var2)
    # handle cases where AWLEVEL column is in lower case
    df <- rename(df, AWLEVEL = 2)
    # join analyzed `df` with the 0 dataframe `empty`. 
    left_join(empty, df, by=c('UNITID'='UNITID', 'level'='AWLEVEL')) %>%
      dplyr::mutate(n.students.y = replace_na(n.students.y, 0),
                    year = year) %>%
      rowwise() %>%
      # each row (year for each permutation of `id` and `level`) contains
      # n.students.x which is 0 and n.students.y which is the actual 
      # number of conferred degrees if at least 1 degree is present. Take
      # the maximum to get the true number. 
      dplyr::mutate( n.students = max(n.students.x, n.students.y)) %>%
      select(-n.students.x, -n.students.y)
  }

  # ----------------------------------------------------------------------
  # 2011 - 2020
  # ----------------------------------------------------------------------
  
  for (y in c(2010:2020)){
    file_name <- paste0(data_directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(UNITID %in% inst_info[["id"]],
                    CIPCODE == department_cip,  
                    AWLEVEL %in% aw_level)  %>% # code for bachelor's degree
      dplyr::group_by(UNITID, AWLEVEL) %>%
      # Var definition (CTOTAL): Grand total
      dplyr::summarise(n.students = sum(CTOTALT))
    
    df <- fill_missing(df, inst_info[["id"]], aw_level, y)
    total_degs <- rbind(total_degs, df)
  }
  
  # ----------------------------------------------------------------------
  # 2008 - 2009
  # ----------------------------------------------------------------------
  # Note that before 2011, doctoral degrees are classified under the award
  # level 9. See https://nces.ed.gov/ipeds/report-your-data/data-tip-sheet-reporting-graduate-awards
  # for more. 
  aw_level <- data.frame(aw_level = c(5, 7, 9),
                         type = c("bachelors", "masters", "doctorate"))
  aw_level <- dplyr::filter(aw_level, type %in% deg_type)[["aw_level"]]
  
  for (y in c(2008:2009)){
    file_name <- paste0(data_directory, "c", 
                        as.character(y), "_a_data_stata.csv")
    df <- data.table::fread(file = file_name, 
            na.strings = c('"A"', '"B"', '"C"', '"D"', '"G"',
                           '"H"', '"J"', '"K"', '"L"', '"N"',
                           '"P"', '"R"', '"Z"', "NA"))
    df <- df %>%
      dplyr::filter(UNITID %in% inst_info[["id"]],
                    CIPCODE == department_cip,  
                    AWLEVEL %in% aw_level) %>%
      dplyr::group_by(UNITID, AWLEVEL) %>%
      # Var definition (CTOTAL): Grand total
      dplyr::summarise(n.students = sum(CTOTALT))
    
    df <- fill_missing(df, inst_info[["id"]], aw_level, y)
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
      dplyr::filter(UNITID %in% inst_info[["id"]],
                    CIPCODE == department_cip,  
                    AWLEVEL %in% aw_level) %>% 
      dplyr::group_by(UNITID, AWLEVEL) %>%
      # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
      # across all race/ethnicity and both genders
      dplyr::summarise(n.students = sum(CRACE24))
    
    df <- fill_missing(df, inst_info[["id"]], aw_level, y)
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
    dplyr::filter(UNITID %in% inst_info[["id"]],
                  CIPCODE == department_cip,  
                  AWLEVEL %in% aw_level) %>%
    dplyr::group_by(UNITID, AWLEVEL) %>%
    # Var definition (CRACE24): Awards/degrees conferred to all recipients, 
    # across all race/ethnicity and both genders
    dplyr::summarise(n.students = sum(CRACE24))
  
  df <- fill_missing(df, inst_info[["id"]], aw_level, 2002)
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
      dplyr::filter(unitid %in% inst_info[["id"]],
                    cipcode == department_cip,  
                    awlevel %in% aw_level) %>%
      dplyr::group_by(unitid, awlevel) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
      # match case of unitid so that `rbind` works
      dplyr::rename(UNITID = unitid)

    df <- fill_missing(df, inst_info[["id"]], aw_level, y)
    total_degs <- rbind(total_degs, df)
  }
  
  # ======================================================================
  # Pre-2000 data
  # ======================================================================
  
  if(full_data){
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
        dplyr::filter(unitid %in% inst_info[["id"]],
                      cipcode == department_cip,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid, awlevel) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid)
      
      df <- fill_missing(df, inst_info[["id"]], aw_level, 1900+y)
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
        dplyr::filter(unitid %in% inst_info[["id"]],
                      cipcode == department_cip,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid, awlevel) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid)
      
      df <- fill_missing(df, inst_info[["id"]], aw_level, y)
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
      dplyr::filter(unitid %in% inst_info[["id"]],
                    cipcode == department_cip,  
                    awlevel %in% aw_level) %>%
      dplyr::group_by(unitid, awlevel) %>%
      # Var definition (CRACE15): Grand total men
      # Var definition (CRACE16): Grand total women
      dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
      # match case of unitid so that `rbind` works
      dplyr::rename(UNITID = unitid)
    
    df <- fill_missing(df, inst_info[["id"]], aw_level, 1990)
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
        dplyr::filter(unitid %in% inst_info[["id"]],
                      cipcode == department_cip,  
                      awlevel %in% aw_level) %>%
        dplyr::group_by(unitid, awlevel) %>%
        # Var definition (CRACE15): Grand total men
        # Var definition (CRACE16): Grand total women
        dplyr::summarise(n.students = sum(crace15) + sum(crace16)) %>%
        # match case of unitid so that `rbind` works
        dplyr::rename(UNITID = unitid)
      
      df <- fill_missing(df, inst_info[["id"]], aw_level, y)
      total_degs <- rbind(total_degs, df)
    }
  }
  
  total_degs <- total_degs %>% 
    dplyr::rename(id = UNITID) %>%
    # add name of institution (match based on `id`)
    dplyr::inner_join(inst_info, by = "id") %>%
    # add deg type (match based on `level`)
    dplyr::inner_join(data.frame(level = c(5,7,9,17),
                                 deg = c("bachelors", "masters", "doctorate", "doctorate")),
                      by = "level") %>%
    dplyr::arrange(id, -year)
  
  if(length(my_ids) != 0){
    total_degs %>%
      dplyr::rename(name = inst_name) %>%
      # change order
      dplyr::select(name, deg, year, n.students) %>%
      dplyr::rename(deg_type = deg)
  } else {
    total_degs %>%
      # change order
      dplyr::select(short_name, deg, year, n.students) %>%
      dplyr::rename(name = short_name,
                    deg_type = deg)
  }
}

# ==============================================================================
# Preliminaries
# ==============================================================================

options(timeout=120)

# ids for institutions with PhD programs
phd_ids <- read.csv("data/NCES_Inst_Details/English PhD granting Universities 2017-18.csv") %>% 
  dplyr::select(UnitID, Institution.Name) %>%
  dplyr::rename(id = UnitID, name = Institution.Name)

file_loc = read.csv("data/IPEDS_completions_data_file_location.csv")
na_vals = c('"A"', '"B"', '"C"', '"D"', '"G"', '"H"', '"J"', '"K"', '"L"', 
            '"N"', '"P"', '"R"', '"Z"', "NA")

df <- data.frame(matrix(ncol = 5, nrow = 0))            # create an empty data frame

# ==============================================================================
# Pull data
# ==============================================================================

for (y in c(1984:2020)) {
  zip_file = file_loc$zip[file_loc$year == y]
  target_file = file_loc$target[file_loc$year == y]
  
  temp <- tempfile()
  # fetch zip file into temp file
  download.file(zip_file, temp)
  # extract target file from temp file
  my_df <- read.table(unz(temp, target_file), sep = ",", header = TRUE, 
                      na.strings = na_vals) %>%
    # change column names to upper case (pre-2004 column names are lower case)
    dplyr::rename_all(.funs = toupper) %>%
    dplyr::filter(UNITID %in% phd_ids[["id"]],
                  CIPCODE %in% c(23.0101, 230101),  
                  # 5: bachelors, 7: masters, c(9,17): doctorate
                  AWLEVEL %in% c(5, 7, 9, 17))  %>%
    dplyr::group_by(UNITID, CIPCODE, AWLEVEL) %>%
    # If statements are necessary as columns for "total" differ depending on the year
    
    # Var definition (CTOTAL): Grand total
    {if (y%in%c(2008:2021)) dplyr::summarise(., n.students=sum(CTOTALT)) else .} %>%
    # Var definition (CRACE24): Awards/degrees conferred to all recipients
    # across all race/ethnicity and both genders
    {if (y%in%c(2002:2007)) dplyr::summarise(., n.students=sum(CRACE24)) else .} %>%
    # Var definition (CRACE15): Grand total men
    # Var definition (CRACE16): Grand total women
    {if (y%in%c(1984:2001)) dplyr::summarise(., n.students=sum(CRACE15)+sum(CRACE16)) else .} %>%
    
    dplyr::mutate(year = y)
  
  # remove temp file
  unlink(temp)
  # add yearly data to the total data frame
  df <- rbind(df, my_df)
}

# ==============================================================================
# Clean pulled data
# ==============================================================================

# add deg column and collapse by deg instead of awlevel (handles casses where
# there are different entries under 9 and 17 awlevels)
df <- df %>%
  mutate_at('CIPCODE', as.character) %>%              # change CIPCODE to string
  mutate(CIPCODE = gsub("\\.", "", CIPCODE)) %>%      # remove "." in cipcode
  inner_join(data.frame(AWLEVEL = c(5,7,9,17),
                        deg = c("bachelors", "masters", "doctorate", "doctorate")), 
             by = "AWLEVEL") %>%                      # add labels for AWLEVEL
  group_by(UNITID, CIPCODE, year, deg) %>%            # group by deg, to collapse AWLEVELS
  summarise(n.students) 

# get all possible 4-combinations from ids, deg-types, years, and CIPCODE
empty <- expand.grid(phd_ids[["id"]], c('230101'),
                     c(1984:2021), c("bachelors", "masters", "doctorate")) %>%
  dplyr::mutate(n.students = 0) %>%
  dplyr::rename(UNITID = Var1, CIPCODE = Var2, year = Var3, deg = Var4)

# populate missing n.student rows
df <- empty %>%
  dplyr::left_join(df, by=c('UNITID'='UNITID', 'CIPCODE'='CIPCODE', 
                             'year'='year', 'deg'='deg')) %>%
  dplyr::mutate(n.students.y = replace_na(n.students.y, 0)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(n.students = max(n.students.x, n.students.y)) %>%
  dplyr::select(-n.students.x, -n.students.y) %>%     # drop intermediate rows
  dplyr::rename(id = UNITID, cip = CIPCODE) %>%
  dplyr::inner_join(phd_ids, by = "id") %>%           # add institution names
  dplyr::select(name, id, cip, year, deg, n.students) # reorder columns

# ==============================================================================
# Save data
# ==============================================================================
write.csv(df,"data/processed_completions.csv", row.names = FALSE)

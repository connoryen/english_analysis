df <- read.csv("data/processed_completions.csv")

# ======================================================================
# Ratio between bachelors and doctorate degrees
# ======================================================================

# Begin plot: private inst comparison ----------------------------------
df_rat <- df %>%
  dplyr::filter(year %in% c(2000:2020)) %>%
  dplyr::group_by(id, name, cip, year) %>%
  dplyr::summarise(ratio = n.students[deg == "bachelors"]/n.students[deg == "doctorate"]) %>%
  dplyr::mutate(ratio = replace_na(ratio, 0)) %>%
  dplyr::mutate(ratio = ifelse(is.infinite(ratio), 0, ratio))

pat = paste0(c("Berkeley", 
               "Yale",
               "Princeton",
               "Stanford",
               "Cornell",
               "University of Chicago"), 
             collapse = "|")
df_rat %>%
  dplyr::filter(grepl(pat, name)) %>%
  ggplot(aes(x=year, y=ratio, color = name)) +
  geom_line() + 
  geom_point() + 
  labs(title = "BA:PhD Private") + 
  theme_minimal()

# Begin plot: public inst comparison -----------------------------------
pat = paste0(c("Berkeley", 
               "University of California-Los Angeles",
               "University of California-Irvine",
               "University of Michigan-Ann Arbor",
               "University of Illinois Urbana-Champaign",
               "University of Virginia-Main Campus"), 
             collapse = "|")

df_rat %>%
  dplyr::filter(grepl(pat, name)) %>%
  ggplot(aes(x=year, y=ratio, color = name)) +
  geom_line() + 
  geom_point() + 
  labs(title = "BA:PhD Public") + 
  theme_minimal()

# ======================================================================
# Percent of doctorate degrees awarded by Berkeley
# ======================================================================

#' Plot the percent of total national awarded English degrees contributed 
#' by a university (or group of universities). 
#'
#' @param my_inst a regex pattern of institutions
#' 
percent_of_national <- function(df, my_inst) {
  my_df <- df %>%
    dplyr::filter(grepl(my_inst, name),
                  year %in% c(1984:2020)) %>%
    dplyr::group_by(cip,year,deg) %>%
    dplyr::summarise(n.students = sum(n.students))
  
  df_nat <- df %>%
    dplyr::filter(year %in% c(1984:2020)) %>%
    dplyr::group_by(cip, year, deg) %>%
    dplyr::summarise(n.students = sum(n.students))
  
  df_per_nat <- my_df %>%
    dplyr::inner_join(df_nat, by = c('year'='year', 'deg'='deg', 'cip'='cip')) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(per = n.students.x/n.students.y*100) %>%
    dplyr::select(year, cip, deg, per)
  
  df_per_nat %>%
    ggplot(aes(x=year, y=per, color=deg)) +
    geom_line() + 
    geom_point() + 
    labs(title = my_inst,
         y = "Percent of National (%)") + 
    theme_minimal()
}

percent_of_national(df, "University of California-Irvine")

# ======================================================================
# Compare number of degrees awarded at Berkeley to national statistics
# ======================================================================
df_ber <- df %>%
  dplyr::filter(grepl("Berkeley", name),
                year %in% c(2000:2020)) %>%
  dplyr::arrange(year, deg)

t = df_ber$year[df_ber$deg == "bachelors"]
bac_ber = df_ber$n.students[df_ber$deg == "bachelors"]
doc_ber = df_ber$n.students[df_ber$deg == "doctorate"]

df_nat <- df %>%
  dplyr::filter(year %in% c(2000:2020)) %>%
  dplyr::group_by(cip, year, deg) %>%
  dplyr::summarise(n.students = sum(n.students))

bac_nat = df_nat$n.students[df_nat$deg == "bachelors"]
doc_nat = df_nat$n.students[df_nat$deg == "doctorate"]

# Begin plot for PhDs ---------------------------------------------------
# space for second y-axis - change margins (`mar`) parameter
par(mar = c(4, 4, 2, 4) + 0.3)
# Create first plot
plot(x=t, y=doc_nat,
     type = "o", lwd = 2, col = "dodgerblue",
     cex.axis = 0.7,
     main = "PhDs: Berkeley vs. National", 
     xlab = "year", ylab = "National")
# Add new plot
par(new = TRUE)
# Create second plot without axes
plot(x=t, y=doc_ber,
     type = "o", lwd = 2, col = "hotpink", 
     axes = FALSE, xlab = "", ylab = "")
# add second axis. Note that `las` parameter changes rotation
axis(side = 4, at = pretty(range(doc_ber)), cex.axis = 0.7, las = 1)
# Add second axis label
mtext("Berkeley", side = 4, line = 3, las = 3)
#legend
legend(x = "bottomleft", legend=c("National", "Berkeley"),
       col=c("dodgerblue", "hotpink"), lty=c(1,1), 
       pch = c(1,1), lwd = c(2,2), cex = 0.75)

# Begin plot for BAs ----------------------------------------------------
# space for second y-axis - change margins (`mar`) parameter
par(mar = c(4, 4, 2, 4) + 0.3)
# Create first plot
plot(x=t, y=bac_nat,
     type = "o", lwd = 2, col = "dodgerblue",
     cex.axis = 0.7,
     main = "BAs: Berkeley vs. National", 
     xlab = "year", ylab = "National")
# Add new plot
par(new = TRUE)
# Create second plot without axes
plot(x=t, y=bac_ber,
     type = "o", lwd = 2, col = "hotpink", 
     axes = FALSE, xlab = "", ylab = "")
# add second axis. Note that `las` parameter changes rotation
axis(side = 4, at = pretty(range(bac_ber)), cex.axis = 0.7, las = 1)
# Add second axis label
mtext("Berkeley", side = 4, line = 3, las = 3)
#legend
legend(x = "bottom", legend=c("National", "Berkeley"),
       col=c("dodgerblue", "hotpink"), lty=c(1,1), 
       pch = c(1,1), lwd = c(2,2), cex = 0.75)

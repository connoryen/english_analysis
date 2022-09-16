source("Process Completions.R")

# ======================================================================
# Compare number of awarded doctoral and bachelors degrees
# ======================================================================
df_ber <- degrees_awarded(my_names = c("Berkeley"), 
                           deg_type = c("doctorate", "bachelors"))

t = df_ber$year[df_ber$deg_type == "bachelors"]
bac_ber = df_ber$n.students[df_ber$deg_type == "bachelors"]
doc_ber = df_ber$n.students[df_ber$deg_type == "doctorate"]

# Begin plot -----------------------------------------------------------
# space for second y-axis - change margins (`mar`) parameter
par(mar = c(4, 4, 2, 4) + 0.3)
# Create first plot
plot(x=t, y=bac_ber,
     type = "o", lwd = 2, col = "dodgerblue",
     cex.axis = 0.7,
     main = "Awarded Degrees Nationally", 
     xlab = "year", ylab = "bachelors")
# Add new plot
par(new = TRUE)
# Create second plot without axes
plot(x=t, y=doc_ber,
     type = "o", lwd = 2, col = "hotpink", 
     axes = FALSE, xlab = "", ylab = "")
# add second axis. Note that `las` parameter changes rotation
axis(side = 4, at = pretty(range(doc_ber)), cex.axis = 0.7, las = 1)
# Add second axis label
mtext("doctorate", side = 4, line = 3, las = 3)
#legend
legend(x = "bottomleft", legend=c("bachelors", "doctorate"),
       col=c("dodgerblue", "hotpink"), lty=c(1,1), 
       pch = c(1,1), lwd = c(2,2), cex = 0.75)

# ======================================================================
# Compare number of degrees awarded at Berkeley to national statistics
# ======================================================================

all_ids <- read.csv("data/NCES_Inst_Details/English PhD granting Universities 2017-18.csv") %>%
  dplyr::select(UnitID) %>%
  dplyr::rename(id = UnitID)

# bachelors and doctoral degrees awarded nationally
# We only consider universities with PhD granting programs. 
df_nat <- degrees_awarded(my_names = c(), my_ids = all_ids[["id"]],
                          deg_type = c("doctorate", "bachelors")) %>%
  dplyr::group_by(year, deg_type) %>%
  dplyr::summarise(n.students = sum(n.students))

bac_nat = df_nat$n.students[df_nat$deg_type == "bachelors"]
doc_nat = df_nat$n.students[df_nat$deg_type == "doctorate"]

# Begin plot for PhDs ---------------------------------------------------
# space for second y-axis - change margins (`mar`) parameter
par(mar = c(4, 4, 2, 4) + 0.3)
# Create first plot
plot(x=t, y=doc_nat,
     type = "o", lwd = 2, col = "dodgerblue",
     cex.axis = 0.7,
     main = "PhDs: Berkeley vs. National", 
     xlab = "year", ylab = "bachelors")
# Add new plot
par(new = TRUE)
# Create second plot without axes
plot(x=t, y=doc_ber,
     type = "o", lwd = 2, col = "hotpink", 
     axes = FALSE, xlab = "", ylab = "")
# add second axis. Note that `las` parameter changes rotation
axis(side = 4, at = pretty(range(doc_ber)), cex.axis = 0.7, las = 1)
# Add second axis label
mtext("doctorate", side = 4, line = 3, las = 3)
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
     xlab = "year", ylab = "bachelors")
# Add new plot
par(new = TRUE)
# Create second plot without axes
plot(x=t, y=bac_ber,
     type = "o", lwd = 2, col = "hotpink", 
     axes = FALSE, xlab = "", ylab = "")
# add second axis. Note that `las` parameter changes rotation
axis(side = 4, at = pretty(range(bac_ber)), cex.axis = 0.7, las = 1)
# Add second axis label
mtext("doctorate", side = 4, line = 3, las = 3)
#legend
legend(x = "bottom", legend=c("National", "Berkeley"),
       col=c("dodgerblue", "hotpink"), lty=c(1,1), 
       pch = c(1,1), lwd = c(2,2), cex = 0.75)

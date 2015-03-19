# Create lines table to display in popup
kable(data.frame(Variable = c("Total commutes", "N. Cyclists", "% Cyclists", "CLC", "PLC", "ECP", "Euclidean Distance (km)", "Fastest distance (km)", "Longest Distance (km)"), Value = rep("%s", 9)), format = "html")

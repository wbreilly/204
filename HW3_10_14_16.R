# Walter Reilly
# 10_14_16
# PSC 204 HW3
library("tidyverse")
HW3D = read.csv("HW03data.csv")

# 4a Gather mean, median, and SD summary statistics about these samples. 
HW3Dsum = data.frame(HW3D %>%
                        summarise(davis.means = mean(davis),
                                  davis.medians = median(davis),
                                  davis.variance = var(davis, na.rm = FALSE),
                                  la.means = mean(la),
                                  la.medians = median(la),
                                  la.variance = var(la, na.rm = FALSE),
                                  merced.means = mean(merced),
                                  merced.medians = median(merced),
                                  merced.variance = var(merced, na.rm = FALSE)
                      ))
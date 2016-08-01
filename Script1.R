# Overall
library(ggplot2, psych)
library(lsr, Hmisc)
setwd("C:/Users/Nils/Desktop/Steph") # set the wd to where the files are

##### Study

files = list.files("C:/Users/Nils/Desktop/Steph", pattern=".txt", full.names = T) # create a vector of the file paths
combined_files_Study <- do.call("rbind", lapply(files, header =T, read.table)) # apply the function read.table over each item in the list files. The output is a list. Call the function rbind on all of the output, and assign it to combined_files
New_Files_Study <- subset(combined_files_Study, resp != "noresponse")

# Line plot with x = distCat and y= RT, and divider = resp
plot1 <- ggplot(New_Files_Study, 
  aes(distCat,
      RT,
      color = resp))
plot1 + 
  stat_summary(fun.y = mean, 
               geom = "line", 
               aes(group = resp)) + 
  stat_summary(fun.y = mean, 
               geom = "point") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0)
  #coord_cartesian(ylim = c(0.95, 1.25)) 
  #scale_y_continuous(limits = c(0.9, 1.25)), breaks = c(1.106, 1.08, 1.10, 1.12, 1.14, 1.16, 1.18, 1.20, 1.22, 1.24))

# Line plot with x = distCond and y = RT and divider = resp
plot2 <- ggplot(New_Files_Study, 
                aes(
                  distCond,
                  RT,
                  color= resp))
plot2 +
  stat_summary(fun.y = mean, 
               geom = "line", 
               aes(
                 group = resp)) +
  stat_summary(fun.y = mean, 
               geom = "point") +
  stat_summary(fun.data = mean_cl_normal, 
               geom="errorbar", 
               width = 0) +
  scale_color_manual(values = c("blue", "green"))
  #scale_color_brewer(palette = "Set5")

#### Test
files1 = list.files("C:/Users/Nils/Desktop/Steph/Test", pattern=".txt", full.names = T) # create a vector of the file paths
combined_filesTest <- do.call("rbind", lapply(files1, header =T, read.table)) # apply the function read.table over each item in the list files. The output is a list. Call the function rbind on all of the output, and assign it to combined_files
New_Files_Test <- subset(combined_filesTest, distCat != "NEW")

# Line plot with x = distCat and y = RT
plot3 <- ggplot(New_Files_Test, 
                aes(
                  distCat, 
                  RT))
plot3 + stat_summary(fun.y = mean, 
                     geom = "line", 
                     group=1) + 
  stat_summary(fun.y = mean, 
               geom="point") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom ="errorbar", 
               width = 0)

# Line plot with x = distCond and y = RT
New_Files_Test <- subset(combined_filesTest, 
                         distCond != "NEW")
plot4 <- ggplot(New_Files_Test, 
                aes(
                  distCond, 
                  RT))
plot4 + stat_summary(fun.y = mean, 
                     geom = "line", group = 1) + 
  stat_summary(fun.y = mean, 
               geom="point") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0)

###################### ========================================================= ######################
######################  Repair maistake in a looping modeller using wrong names  ######################
###################### ========================================================= ######################

#' Below is an example of how to ammend a mistake in incorrectly naming files in a loop

## It is important to find the offending loop and  (without any actual data or modelling) reproduce the names of the output
## containers that the loop created.

#' This loop prints out the variable names that the modeller was assigning results to, as well as the names that were used
#'to save the file, i.e. how the output files appeared on disk. This printed output should be copied and transformed into the code,
#' which: loads, renames and saves the data to new files with the correct variable names contained. The actual file names remain
#' unchanged, hence why correctly named variables are saved to a new folder.
#'

for(i in 1:5) {
    
    for(j in 1:5) {
        
        print(paste0("input_data :", names(input_data[[i]])[[j]]))

        to_save <- paste0("glm_results_", as.character(names(input_data)[[j]]))
        file_name <- paste0("glm_results.L", as.character(i), ".",
                            as.character(names(input_data[[i]])[[j]]), ".rda")

        print(paste0("var_name : ", to_save))
        print(paste0("file_name : ", file_name))
        
        
    }
}

## ------------------------ ##
##  correct the file names  ##
## ------------------------ ##
## Everything below is pasted and edited from the output of the above loop

input_folder <- "/Volumes/Mac\ OS\ Drive/Documents/Dropbox/ultra/B_cutoff70_frame60/"
output_folder <- "/Volumes/Mac\ OS\ Drive/Documents/Dropbox/ultra/B_cutoff70_frame60/correctly_named/"

## ## Example of code chunk for one file
## A <- "trad_small"                       #input name
## B <- "glm_results_Lag.1"                #variable/data name in workspace
## C <- "glm_results.L1.trad_small.rda"    #file saved name
## load(paste0(input_folder, C))        #load the data into the workspace
## to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
## assign(to_save, get(B))
## ## Save the data with new vairable name to new folder
## save(list = to_save, file = paste0(output_folder, C))
## rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions


A <- "trad_small"                             
B <- "glm_results_Lag.1"                       
C <- "glm_results.L1.trad_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_large"                             
B <- "glm_results_Lag.2"                       
C <- "glm_results.L1.trad_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_small"                             
B <- "glm_results_Lag.3"                       
C <- "glm_results.L1.sent_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_large"                             
B <- "glm_results_Lag.4"                       
C <- "glm_results.L1.sent_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "combined"                               
B <- "glm_results_Lag.5"                       
C <- "glm_results.L1.combined.rda"            
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_small"                             
B <- "glm_results_Lag.1"                       
C <- "glm_results.L2.trad_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_large"                             
B <- "glm_results_Lag.2"                       
C <- "glm_results.L2.trad_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_small"                             
B <- "glm_results_Lag.3"                       
C <- "glm_results.L2.sent_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_large"                             
B <- "glm_results_Lag.4"                       
C <- "glm_results.L2.sent_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "combined"                               
B <- "glm_results_Lag.5"                       
C <- "glm_results.L2.combined.rda"            
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_small"                             
B <- "glm_results_Lag.1"                       
C <- "glm_results.L3.trad_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_large"                             
B <- "glm_results_Lag.2"                       
C <- "glm_results.L3.trad_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_small"                             
B <- "glm_results_Lag.3"                       
C <- "glm_results.L3.sent_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_large"                             
B <- "glm_results_Lag.4"                       
C <- "glm_results.L3.sent_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "combined"                               
B <- "glm_results_Lag.5"                       
C <- "glm_results.L3.combined.rda"            
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_small"                             
B <- "glm_results_Lag.1"                       
C <- "glm_results.L4.trad_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_large"                             
B <- "glm_results_Lag.2"                       
C <- "glm_results.L4.trad_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_small"                             
B <- "glm_results_Lag.3"                       
C <- "glm_results.L4.sent_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_large"                             
B <- "glm_results_Lag.4"                       
C <- "glm_results.L4.sent_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "combined"                               
B <- "glm_results_Lag.5"                       
C <- "glm_results.L4.combined.rda"            
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_small"                             
B <- "glm_results_Lag.1"                       
C <- "glm_results.L5.trad_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "trad_large"                             
B <- "glm_results_Lag.2"                       
C <- "glm_results.L5.trad_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_small"                             
B <- "glm_results_Lag.3"                       
C <- "glm_results.L5.sent_small.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "sent_large"                             
B <- "glm_results_Lag.4"                       
C <- "glm_results.L5.sent_large.rda"          
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

A <- "combined"                               
B <- "glm_results_Lag.5"                       
C <- "glm_results.L5.combined.rda"
load(paste0(input_folder, C))        #load the data into the workspace
to_save <- substr(C, 1, nchar(C)-4) #desired new name in workspace. the -4 loses ".rda"
assign(to_save, get(B))
## Save the data with new vairable name to new folder
save(list = to_save, file = paste0(output_folder, C))
rm(list=setdiff(ls(), c("input_folder", "output_folder"))) #only keep the directories between actions

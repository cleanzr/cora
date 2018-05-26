load("../data/cora_gold.RData")
load("../data/cora.RData")

#initial set up
n_cora <- nrow(cora) #get total number of rows
unique_id <- rep(0,1697) #number of unique numbers in cora_gold
unique_id[1] <- 1
unique_id_value <- 1
cora_ids <- seq(1,n_cora,1)

cora_gold2 <- cora_gold[order(cora_gold$id1),]  #sort cora gold ascending

tabs <- table(cora_gold2$id1)
matched_names <- names(tabs)
matched_names <- as.numeric(matched_names) #get names of table
tabs <- unname(tabs) #unname the match freq table

diffs <- as.vector((diff(as.matrix(tabs)))) #get diff in values between values in freq
diffs <- c(0,diffs)

information <- data.frame(matched_names, tabs, diffs)
information <- information[,-2] #create initiation gold data frame

#first implmenetation of getting unique ids of current numbers in data frame
for (j in (2:nrow(information))){
  if(information[j,"diffs"]>=0){
    unique_id_value <- unique_id_value + 1
    unique_id[j] <- unique_id_value
  }
  else{
    unique_id[j] <- unique_id_value
  }
}

information$unique_id <- unique_id #add unique id into data frame
#head(information)
#nrow(information)

unnamed_tabs <- table(cora_gold2$id1)
#View(unnamed_tabs)

ones_id <- c()
name <- names(unnamed_tabs) ##corresponding id to the freq

#get all the values that have freq of 1, 
#this means there is a value after this that is a match, should have same id
for (i in 1:length(unnamed_tabs)){
  if (unnamed_tabs[i]==1){
    ones_id <- c(ones_id, name[i])
  }
}
#ones_id


ids_missing <- as.numeric(ones_id) + 1 #get that missing value with same id
missing_total <- setdiff(cora_ids, information$matched_names) #get all the missing ids

already_have_existing_ids <- intersect(missing_total, ids_missing) #get all the missing, but known ids

#length(ids_missing)
#length(already_have_existing_ids)
###^^^THERE IS A DIFFERENCE IN LENGTH HERE, AND THAT SHOULDN'T HAPPEN.... WHY IS THIS.. might lead to error



#get the unique id of the values that had freq of 1 and add it to the next value
uniques <- c()
for (i in 1:length(already_have_existing_ids)){
  row <- which(information$matched_names == already_have_existing_ids[i]-1)
  uniques <- c(uniques, information[row,"unique_id"])
}

#update cora ids known and corresponding unique ids
current_cora_ids <- c(information$matched_names, already_have_existing_ids)
current_unique_ids <- c(information$unique_id, uniques)

#create vector of last set of missing ids
#these ids don't match with anything, so have unique id of their own
last_set_of_missing_ids <- setdiff(cora_ids, current_cora_ids)

#give the remaning cora ids a unqiue id of their own
unique_id_set2<- c()
for (i in 1:length(last_set_of_missing_ids)){
  unique_id_value <- unique_id_value + 1
  unique_id_set2 <- c(unique_id_set2, unique_id_value)
}

#combining all cora ids and corresponding unique ids
finalized_cora_ids <- c(current_cora_ids, last_set_of_missing_ids)
finalized_unique_ids <- c(current_unique_ids, unique_id_set2)

#create finalized data set of cora ids and unique ids for updated gold. 
cora_gold_update <- data.frame(finalized_cora_ids, finalized_unique_ids)
cora_gold_update <- cora_gold_update[order(cora_gold_update$finalized_cora_ids),] 
names(cora_gold_update) <- c("cora_id", "unique_id")

#nrow(cora_gold_update)

save(cora_gold_update, file="../data/cora_gold_update.RData")


#remaining_cora_ids <- setdiff(cora_ids, set2_cora_ids)
#
#length(set2_cora_ids) + length(remaining_cora_ids)
#
#length(cora_ids)
#
#unique_id_set2 <- c()
#
#
#for (i in 1:length(remaining_cora_ids)){
#  unique_id_value <- unique_id_value + 1
#  unique_id_set2 <- c(unique_id_set2, unique_id_value)
#}
#
#nrow(information) + length(unique_id_set2)
#
#
#cora_id_gold <- c(information$matched_names, remaining_cora_ids)
#unique_id_gold <- c(information$unique_id, unique_id_set2)
#
#almost_finalize_gold <- data.frame(cora_id_gold, unique_id_gold)
#
#
#almost_finalize_gold <- almost_finalize_gold[order(almost_finalize_gold$cora_id_gold),] 


#existing_ids <- unique(information$unique_id)
#
#for (i in 2:nrow(information)){
#  if (information[i,"unique_id"]-information[i-1,"unique_id"]==1){
#    unique_values_set2 <- c(unique_values_set2, (information[i-1,"matched_names"]+1))
#    unique_id_set2 <- c(unique_id_set2, information[i-1,"unique_id"])
#  }
#}
#unique_values_set2
#intersect(information$matched_names,unique_values_set2)
#
#
#unique_values_set3 <- c(information$matched_names, unique_values_set2)
#unique_id_set3 <- c(information$unique_id, unique_id_set2)
#
#remaining_unique_values_set4 <- setdiff(cora_ids, unique_values_set3)
#unique_id_set4 <- c()
#
#
#for (i in 1:length(remaining_unique_values_set4)){
#  unique_id_value <- unique_id_value + 1
#  unique_id_set4 <- c(unique_id_set4, unique_id_value)
#}
#
#unique_cora_id_final <- c(unique_values_set3, remaining_unique_values_set4)
#unique_matched_id_final <- c(unique_id_set3, unique_id_set4)
#length(unique_cora_id_final)
#
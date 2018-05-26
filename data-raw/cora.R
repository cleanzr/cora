library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(methods)
library(readxl)
library(qdapRegex)

coraxcel <- read_excel("data/coraxcel.xlsx", 
                       col_names = FALSE)

combined <- with(coraxcel, paste0(X__1, X__2))

words = c()
words_unique = c()
for (i in 1:5639){
  words <- c(words,rm_between(combined[i], '<', '>', extract=TRUE)[[1]])
}
words_unique<-unique(words, incomparables = FALSE)

id = c()
for (i in 1:5639){
  id <- c(id,rm_between(combined[i], '<NEWREFERENCE id=', '>', extract=TRUE)[[1]])
}
id<- sub("\\\"", '', id)
id<- sub("\\\"", '', id)
id<-noquote(id)

positions = c()
for (i in 1:5639){
  if (!is.na(id[i])){
    positions <- c(positions, i)
  }
}

authors = c()
title = c()
type = c()
address = c()
year = c()
date = c()
note = c()
editor = c()
journal = c()
volume = c()
pages = c()
booktitle = c()
publisher = c()
institution = c()
booktitle = c() #there is misspelling --> booktile = c()... in the data set
tech = c()

c_authors = c()
c_title = c()
c_type = c()
c_address = c()
c_year = c()
c_date = c()
c_note = c()
c_editor = c()
c_journal = c()
c_volume = c()
c_pages = c()
c_booktitle = c()
c_publisher = c()
c_institution = c()
c_booktitle = c() #there is misspelling --> booktile = c()... in the data set
c_tech = c()

for (i in positions)
{
  authors <- c(authors,rm_between(combined[i+1], '<author>', ' </author>', extract=TRUE)[[1]])
  c_authors <- c(c_authors, length(authors))
  title <- c(title,rm_between(combined[i+1], '<title>', ' </title>', extract=TRUE)[[1]]) 
  c_title <- c(c_title, length(title))
  type <- c(type,rm_between(combined[i+1], '<type>', ' </type>', extract=TRUE)[[1]])
  c_type <- c(c_type, length(type))
  address <- c(address,rm_between(combined[i+1], '<address>', ' </address>', extract=TRUE)[[1]])
  c_address <- c(c_address, length(address))
  year <- c(year,rm_between(combined[i+1], '<year>', ' </year>', extract=TRUE)[[1]]) 
  c_year <- c(c_year, length(year))
  date <- c(date,rm_between(combined[i+1], '<date>', ' </date>', extract=TRUE)[[1]]) 
  c_date <- c(c_date, length(date))
  note <- c(note,rm_between(combined[i+1], '<note>', ' </note>', extract=TRUE)[[1]])
  c_note <- c(c_note, length(note))
  editor <- c(editor,rm_between(combined[i+1], '<editor>', ' </editor>', extract=TRUE)[[1]])
  c_editor <- c(c_editor, length(editor))
  journal <- c(journal,rm_between(combined[i+1], '<journal>', ' </journal>', extract=TRUE)[[1]])
  c_journal <- c(c_journal, length(journal))
  volume <- c(volume,rm_between(combined[i+1], '<volume>', ' </volume>', extract=TRUE)[[1]])
  c_volume <- c(c_volume, length(volume))
  pages <- c(pages,rm_between(combined[i+1], '<pages>', ' </pages>', extract=TRUE)[[1]])
  c_pages <- c(c_pages, length(pages))
  booktitle <- c(booktitle,rm_between(combined[i+1], '<booktitle>', ' </booktitle>', extract=TRUE)[[1]]) 
  c_booktitle <- c(c_booktitle, length(booktitle))
  publisher <- c(publisher,rm_between(combined[i+1], '<publisher>', ' </publisher>', extract=TRUE)[[1]]) 
  c_publisher <- c(c_publisher, length(publisher))
  institution <- c(institution,rm_between(combined[i+1], '<institution>', ' </institution>', extract=TRUE)[[1]]) 
  c_institution <- c(c_institution, length(institution))
  tech <- c(tech,rm_between(combined[i+1], '<tech>', ' </tech>', extract=TRUE)[[1]]) 
  c_tech <- c(c_tech, length(tech))
}

#### each should be 1879 in length for the matrix
authors<-noquote(authors) #1880
title<-noquote(title) #1883
type<-noquote(type) #1879
address<-noquote(address) #1899
year<-noquote(year) #1893
date<-noquote(date) #1979
note<-noquote(note) #1879
editor<-noquote(editor) #1879
journal<-noquote(journal) #1879
volume<-noquote(volume) #1879
pages<-noquote(pages) #1884
booktitle<-noquote(booktitle) #1879
publisher<-noquote(publisher) #1880
institution<-noquote(institution) #1880
tech<-noquote(tech) #1882

c_a <- diff(c_authors)
c_t <- diff(c_title)
c_ad <- diff(c_address)
c_y <- diff(c_year)
c_p <- diff(c_pages)
c_pu <- diff(c_publisher)
c_i <- diff(c_institution)
c_te <- diff(c_tech)
c_e <- diff(c_editor)
c_d <- diff(c_date)

p_authors = c()
p_title = c()
p_address = c()
p_year = c()
p_pages = c()
p_publisher = c()
p_institution = c()
p_tech = c()
p_editor = c()
p_date2 = c()
p_date3 = c()
#
#for (i in 1:length(c_a)){
#  if (c_a[i]!=1)
#  {
#    p_authors <- c(p_authors, i+1)
#  }
#}
#p_authors
#
#for (i in 1:length(c_t)){
#  if (c_t[i]!=1)
#  {
#    p_title <- c(p_title, i+1)
#  }
#}
#p_title
#
#for (i in 1:length(c_ad)){
#  if (c_ad[i]!=1)
#  {
#    p_address <- c(p_address, i+1)
#  }
#}
#p_address
#
#for (i in 1:length(c_y)){
#  if (c_y[i]!=1)
#  {
#    p_year <- c(p_year, i+1)
#  }
#}
#p_year
#
#for (i in 1:length(c_p)){
#  if (c_p[i]!=1)
#  {
#    p_pages <- c(p_pages, i+1)
#  }
#}
#p_pages
#
#for (i in 1:length(c_pu)){
#  if (c_pu[i]!=1)
#  {
#    p_publisher <- c(p_publisher, i+1)
#  }
#}
#p_publisher
#
#for (i in 1:length(c_i)){
#  if (c_i[i]!=1)
#  {
#    p_institution <- c(p_institution, i+1)
#  }
#}
#p_institution
#
#for (i in 1:length(c_te)){
#  if (c_te[i]!=1)
#  {
#    p_tech <- c(p_tech, i+1)
#  }
#}
#p_tech

#for (i in 1:length(c_e)){
#  if (c_e[i]!=1)
#  {
#    p_editor <- c(p_editor, i+1)
#  }
#}
#p_editor

for (i in 1:length(c_d)){
  if (c_d[i]==2)
  {
    p_date2 <- c(p_date2, i+1)
  }
  if(c_d[i]==3){
    p_date3 <- c(p_date3, i+1)
  }
}


stuff <- paste(authors[24], authors[25], sep=" ")
authors[24] <- stuff
remove <- c(25)
authors <- authors[-remove]
######

stuff <- paste(title[24], title[25], sep=" ")
title[24] <- stuff

stuff <- paste(title[27], title[28], sep=" ")
title[27] <- stuff

stuff <- paste(title[1869], title[1870] , sep=" ")
title[1869] <- stuff

stuff <- paste(title[1871], title[1872], sep=" ")
title[1871] <- stuff

remove <- c(25, 28, 1870, 1872)
title <- title[-remove]
###

remove <- c(115, 117, 128, 368, 439, 441, 443, 445, 447, 449, 451, 453, 455, 457, 459, 461, 463, 467, 1298, 1300)

address[114] <- paste(address[114] , address[115] , sep=", ")
address[116] <- paste(address[116] , address[117] , sep=", ")
address[127] <- paste(address[127] , address[128] , sep=", ")
address[367] <- paste(address[367] , address[368] , sep=", ")
address[438] <- paste(address[438] , address[439] , sep=", ")
address[440] <- paste(address[440] , address[441] , sep=", ")
address[442] <- paste(address[442] , address[443] , sep=", ")
address[444] <- paste(address[444] , address[445] , sep=", ")
address[446] <- paste(address[446] , address[447] , sep=", ")
address[448] <- paste(address[448] , address[449] , sep=", ")
address[450] <- paste(address[450] , address[451] , sep=", ")
address[452] <- paste(address[452] , address[453] , sep=", ")
address[454] <- paste(address[454] , address[455] , sep=", ")
address[456] <- paste(address[456] , address[457] , sep=", ")
address[458] <- paste(address[458] , address[459] , sep=", ")
address[460] <- paste(address[460] , address[461] , sep=", ")
address[462] <- paste(address[462] , address[463] , sep=", ")
address[466] <- paste(address[466] , address[467] , sep=", ")
address[1297]<- paste(address[1297], address[1298], sep=", ")
address[1299]<- paste(address[1299], address[1300], sep=", ")

address <- address[-remove]
#####

remove <- c(573, 575, 725, 732, 772, 1395, 1550, 1570, 1572, 1574, 1576, 1578, 1825, 1883)
year <- year[-remove]
########

stuff <- paste(pages[438], pages[439], sep=", ")
pages[438] <- stuff

stuff <- paste(pages[440], pages[441], sep=", ")
pages[440] <- stuff

stuff <- paste(pages[446], pages[447], sep=", ")
pages[446] <- stuff

stuff <- paste(pages[580], pages[581], sep=", ")
pages[580] <- stuff

stuff <- paste(pages[1776], pages[1777], sep=" ")
pages[1776] <- stuff

remove <- c(439, 441, 447, 581, 1777)
pages <- pages[-remove]
####

stuff <- paste(publisher[144], publisher[145], sep=" ")
remove <- c(145)
publisher[144] <- stuff
publisher <- publisher[-remove]
###

stuff <- paste(institution[796], institution[797], sep=" ")
remove <- c(797)
institution[796] <- stuff
institution <- institution[-remove]
####

stuff <- paste(tech[482], tech[483], sep=" ")
tech[482] <- stuff

stuff <- paste(tech[484], tech[485], sep=" ")
tech[484] <- stuff

stuff <- paste(tech[505], tech[506], sep=" ")
tech[505] <- stuff
remove <- c(438, 485, 506)
tech <- tech[-remove]
####
stuff <- paste(editor[27],editor[28], sep=" ")
editor[27] <- stuff

stuff <- paste(editor[29], editor[30], sep=" ")
editor[29] <- stuff

stuff <- paste(editor[31], editor[32], sep=" ")
editor[31] <- stuff
remove <- c(28, 30, 32)
editor <- editor[-remove]
########

#pre triple dates
count <- 0
stuff_pre3 = c()
for (i in 1:50){
  stuff_pre3 <- c(stuff_pre3, paste(date[p_date2[i]+count],date[p_date2[i]+count+1], sep = " "))
  count <- count + 1
}
stuff_pre3[2] <- "June 1994"
stuff_pre3[3] <- "1993"
stuff_pre3[40] <- "1990"
stuff_pre3[39] <- "1990"
stuff_pre3[42] <- "1990"
stuff_pre3[43] <- "1990"
stuff_pre3[44] <- "1991"

#triple dates
count <- 0
stuff_3 = c()
for (i in 1:length(p_date3)){
  stuff_3 <- c(stuff_3, paste(date[p_date3[i]+count+50], date[p_date3[i]+1+count+50], sep = " "))
  count <- count + 2
}

#post trip dates
count <- 0
stuff_post3 = c()
for (i in 51:72){
  stuff_post3 <- c(stuff_post3, paste(date[p_date2[i]+count+50+14*2],date[p_date2[i]+count+1+50+14*2], sep = " "))
  count <- count + 1
}
stuff_post3[17] <- 1991

remove1 = c()
replace1 = c()
count <- 0
for (i in 1:50){
  remove1 <- c(remove1, p_date2[i]+count+1)
  count <- count+1
}
replace1 <- remove1-1

remove2 = c()
remove4 = c()
replace2 = c()
count <- 0
for (i in 1:14){
  remove2 <- c(remove2, p_date3[i] + count + 1+50)
  remove4 <- c(remove4, p_date3[i] + count + 2+50)
  count <- count+2
}
replace2 <- remove2-1

remove3 = c()
replace3 = c()
count <- 0
for (i in 51:72){
  remove3<- c(remove3, p_date2[i]+count+1+50+14*2)
  count <- count + 1
}
replace3 <- remove3-1

for (i in 1:50){
  date[replace1[i]] <- stuff_pre3[i]
}
for (i in 1:14){
  date[replace2[i]] <- stuff_3[i]
}
for (i in 1:22){
  date[replace3[i]] <- stuff_post3[i]
}

remove <- c(remove1, remove2, remove4, remove3)
date <- date[-remove]

title <- gsub("[[:punct:]]$", "",title)

booktitle <- gsub("[[:punct:]]$", "",booktitle)

authors <- gsub("[[:punct:]]$", "",authors)

address <- gsub("[[:punct:]]$", "",address)
address <- gsub(")$", "",address)

date <- gsub(")", "",date)
date <- gsub("\\(", "",date)
date <- gsub("[[:punct:]]$", "",date)

year <- gsub(")", "",year)
year <- gsub("\\(", "",year)
year <- gsub("[[:punct:]]$", "",year)

editor <- gsub("[[:punct:]]$", "",editor)

journal <- gsub("[[:punct:]]$", "",journal)

volume <- gsub("[[:punct:]]$", "",volume)

pages <- gsub("[[:punct:]]$", "",pages)

publisher <- gsub("[[:punct:]]$", "",publisher)

institution <- gsub("[[:punct:]]$", "",institution)

type <- gsub("[[:punct:]]$", "",type)

tech <- gsub(")", "",tech)
tech <- gsub("\\(", "",tech)
tech <- gsub("[[:punct:]]$", "",tech)

note <- gsub("[[:punct:]]$", "",note)

title<-noquote(title)
booktitle<-noquote(booktitle)
authors<-noquote(authors)
address<-noquote(address)
date<-noquote(date)
year<-noquote(year)
editor<-noquote(editor)
journal<-noquote(journal)
volume<-noquote(volume)
pages<-noquote(pages)
publisher<-noquote(publisher)
institution<-noquote(institution)
type<-noquote(type)
tech<-noquote(tech)
note<-noquote(note)

title <- trimws(title, which = c("both"))
booktitle <- trimws(booktitle, which = c("both"))
authors <- trimws(authors, which = c("both"))
address <- trimws(address, which = c("both"))
date <- trimws(date, which = c("both"))
year <- trimws(year, which = c("both"))
editor <- trimws(editor, which = c("both"))
journal <- trimws(journal, which = c("both"))
volume <- trimws(volume, which = c("both"))
pages <- trimws(pages, which = c("both"))
publisher <- trimws(publisher, which = c("both"))
institution <- trimws(institution)
type <- trimws(type, which = c("both"))
tech <- trimws(tech, which = c("both"))
note <- trimws(note, which = c("both"))

id <- c()
for (i in 1:1879){
  id[i] <- i
}

cora <- data.frame(id, title, booktitle, authors, address, date, year, editor, journal, volume, pages, publisher, institution, type, tech, note)
col_headings <- c('id', 'title', 'book_title', 'authors', 'address', 'date', 'year', 'editor', 'journal', 'volume', 'pages', 'publisher', 'institution', 'type', 'tech', 'note')
names(cora) <- col_headings

#save object
save(cora,file="cora.RData")
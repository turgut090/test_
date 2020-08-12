
#time = read_html('https://www.timeanddate.com/time/zone/azerbaijan') %>% html_nodes('#ct') %>% html_text()
#date = read_html('https://www.timeanddate.com/worldclock/azerbaijan/baku') %>% html_nodes('#ctdat') %>% html_text()

library(dplyr)
library(rvest)
library(stringr)
library(lubridate)
library(glue)
library(plyr)
library(doParallel)
library(data.table)

result11 <- data.frame(matrix(NA, nrow = 25, ncol = 1))

# get pages
colnames(result11) <- c("reference")

for (i in 1:nrow(result11)) {
  q <- enexpr(i)
  url <- glue('https://bina.az/alqi-satqi/menziller?page={enexpr(q)}')
  result11[i, 1] <- url
}

# get links per page
datalist11 = list()

for (i in 1:nrow(result11)) {
  page <- read_html(result11[i,])
  df <- html_attr(html_nodes(page, ".item_link"), "href") %>% as.character() %>% 
    as.data.frame() %>% 
    `colnames<-`(c('link'))
  datalist11[[i]] <- df
  print(i)
}

big_data13 = do.call(rbind, datalist11) %>% as.data.frame()

as.character(paste0('https://bina.az',as.character(big_data13$link))) -> big_data13$link

total = big_data13
#total = bind_rows(big_data11,big_data12,big_data13)
total %>% distinct(link,.keep_all = T)->total

total$link[!str_detect(total$link,pattern = 'yasayis')] %>% as.data.frame() %>% 
  `colnames<-`(c('links'))->total

bina_links = total$links

rm(big_data13,datalist11,df,i,page,q,result11,total,url)

for (i in 1:length(bina_links)) {
  txt = read_html(bina_links[i]) %>% html_nodes('.item_info') %>% html_text() 
  bina_links_ = ifelse(str_detect(txt,'Bugün'), bina_links[i], NA)
  bina_links[i] = bina_links_
  print(paste(i,'out of',length(bina_links)))
}

bina_links = bina_links[!is.na(bina_links)]

length(bina_links)

#txt %>% str_extract("Elanın nömrəsi: [0-9]+")
#txt %>% str_extract("Baxışların sayı: [0-9]+")
#txt %>% str_extract("Yeniləndi: ([0-9]+ [aA-zZ]+ [0-9]+)") #%>% str_extract('[0-9]+ [aA-zZ]+ [0-9]+')
rm(i,txt,bina_links_)



# Links
concat2 = as.data.frame(bina_links) 
colnames(concat2) = 'links'

n <- 100
nr <- nrow(concat2)
smp = split(concat2, rep(1:ceiling(nr/n), each=n, length.out=nr))
list2 = list()

for (i in 1:length(smp)) {
  new = mclapply(1:nrow(smp[[i]]), function(k) {
    html_page = read_html(smp[[i]]$links[k])
    res = html_page %>% 
      html_nodes("td") %>%
      html_text() %>% .[seq(2,12,2)] %>% .[1:5] %>% append(.,
                                                           html_page %>% 
                                                             html_nodes(".azn") %>%
                                                             html_text()) %>% 
      # ownership
      append(.,html_page %>% 
               html_nodes(".ownership") %>%
               html_text() ) %>% 
      # description
      append(.,html_page %>% 
               html_nodes("article") %>%
               html_text() ) %>% 
      
      #views
      append(.,html_page %>% 
               html_nodes(".item_info") %>%
               html_text() %>% gsub(.,replacement = ' ',pattern = '\\D+') %>% str_squish() %>% 
               str_split(.,' ') %>% unlist() %>% .[2] )%>% 
      #locations
      append(.,html_page %>% 
               html_nodes(".locations") %>%
               html_text() %>% str_split(.,'   ',simplify = T))
    return(res)
  },mc.cores = 20)
  list2[[i]]<-new
  print(i)
}


new_df2 <-rbind.fill(lapply(do.call(rbind,list2[1:9]),function(y){as.data.frame(t(y),stringsAsFactors=FALSE)})) %>% 
  distinct() %>% mutate(id = str_extract(concat2$links,pattern = '[0-9]+'),
                        date = Sys.Date()) %>% 
  select(id,date,everything())

####################################################################################################################

if(!file.exists('houses.csv')) {
  fwrite(new_df2,'houses.csv')
} else {
  dataset = fread('houses.csv')
  total = bind_rows(dataset,new_df2)
  fwrite(total,'houses.csv')
}


if(!dir.exists('imgs')) {
  dir.create('imgs')
}

setwd('imgs')
img_add_gather = list()

for (i in 1:length(bina_links)) {
  imgs = read_html(bina_links[i]) %>% 
    html_nodes('.thumbnail') %>% html_attr('data-mfp-src')
  idx = sample(1:length(imgs), floor(length(imgs)*0.6), replace=TRUE)
  dir_name = paste('id_',str_extract(bina_links[i],'[0-9]+'),sep = '')
  dir.create(dir_name)
  for(j in idx) {
    download.file(imgs[j],destfile = file.path(
      dir_name,
      basename( paste('id_',str_extract(bina_links[i],'[0-9]+'),
                      '_',
                      sample.int(1e6,1),sample.int(1e6,1),sample.int(1e6,1),
                      '_img.jpg',sep='')
      )
    ))
  }
  print(paste('Done',i,'out of', length(bina_links)))
}






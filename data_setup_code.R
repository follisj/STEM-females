library(tidyverse)
library(rvest)
library(scales)


#######################

## create all data sets used for visualizations (scraping data and cleaning)

#######################

# grouping fields for plotting
STEM.field <- c("Biological/Biomedical","Computer Science","Engineering","Mathematics/Statistics","Physical Science")
stem_over <- c("Computer Science","Physical Science")
non_stem_over <- c("Education","Public Administration","Visual/Performing Arts")

# for labels
STEM.field2 <- c("Biological/\nBiomedical","Computer\nScience","Engineering",
                 "Mathematics/\nStatistics","Physical\nScience")


# read in file links to scrape the male/female/race data for 1996-2021
file_links <- read_csv("table_url2.csv")

# create the data set
# three parts since the format of the data changed over time
# each part has a function to get the data for a given year; 
# used apply and do.call to get the data for all years

## 1-8 1996-2003
educ1 <- function(url) {
  download.file(url,"test.xls")
  educ.data <- readxl::read_xls("test.xls")
  educ.data.col <- ncol(educ.data)
  educ.data <- educ.data %>%
    drop_na() %>%
    select(seq(1,educ.data.col,2)) %>%
    'colnames<-'(c(
      "field","total","t_white","t_black","t_hisp","t_api","t_ai","t_nra",
      "m_total","m_white","m_black","m_hisp","m_api","m_ai","m_nra",
      "f_total","f_white","f_black","f_hisp","f_api","f_ai","f_nra"
    )) %>%
    filter(!grepl("_",field)) %>%
    filter(!grepl("Major field of study",field)) %>%
    filter(!row_number() %in% c(1:2)) %>%
    mutate(field=str_replace(field," [:punct:]+","")) %>%
    mutate(field=str_replace(field,"[:punct:]+$","")) %>%
    mutate(year=file_links[which(file_links$url_all==url),1])
  return(educ.data)
}

educ_96_03 <- apply(file_links[1:8,2],1,educ1)
educ_96_03.b <- do.call(rbind,educ_96_03)


# 9-15. 2004-2010 post 2005. start 04 and 05. (2006 year) through 09 and 10 (2012 year). row 9

educ2 <- function(url) {
  educ.data <- (read_html(url) %>%
                  html_nodes("table") %>%
                  html_table(fill=T))[[5]] %>%
    `colnames<-`(c(
      "field","total","t_white","t_black","t_hisp","t_api","t_ai","t_nra",
      "m_total","m_white","m_black","m_hisp","m_api","m_ai","m_nra",
      "f_total","f_white","f_black","f_hisp","f_api","f_ai","f_nra"
    )) %>%
    filter(
      field != "", field != "Major field of study", field != 1, field != "All fields, total",
      field != "Field of study"
    ) %>% 
    mutate(year=file_links$Year[which(file_links$url_all==url)])
  return(educ.data)
}

educ_04_10 <- apply(file_links[9:15,2],1,educ2)
educ_04_10.b <- do.call(rbind,educ_04_10)

## 16-26 2011-2021 

educ3 <- function(url_row) {
  xxx <- (read_html(file_links$url_f[url_row]) %>% html_nodes("table") %>% 
            html_table(fill=T))[[5]]
  xxx.f <- xxx[-c(1:2),c(1,(ncol(xxx)-9):ncol(xxx))] %>%
    `colnames<-`(c("field",
                   "f_total","f_white","f_black","f_hisp",
                   "f_api","f_asian","f_pi","f_aiak","f_two_plus","f_nra")) %>%
    filter(field != "",field != "Field of study",field != 1,field != "All fields, total")
  
  xxx <- (read_html(file_links$url_m[url_row]) %>% html_nodes("table") %>% 
            html_table(fill=T))[[5]]
  xxx.m <- xxx[-c(1:2),c(1,(ncol(xxx)-9):ncol(xxx))] %>%
    `colnames<-`(c("field",
                   "m_total","m_white","m_black","m_hisp",
                   "m_api","m_asian","m_pi","m_aiak","m_two_plus","m_nra")) %>%
    filter(field != "",field != "Field of study",field != 1,field != "All fields, total")  %>%
    mutate(year=file_links$Year[url_row])
  
  xxx <- cbind(xxx.f,xxx.m[,-1])
  return(xxx)
}

educ_11_21 <- apply(data.frame(x=c(16:26)),1,educ3)
educ_11_21.b <- do.call(rbind,educ_11_21)
educ_11_21.b <- educ_11_21.b %>%
  mutate(across(f_total:m_nra,~str_replace(.,",",""))) %>%
  mutate(across(f_total:m_nra,as.numeric)) %>%
  mutate(
    total=f_total+m_total,
    t_white=f_white+m_white,
    t_black=f_black+m_black,
    t_hisp=f_hisp+m_hisp,
    t_api=f_api+m_api,
    t_asian=f_asian+m_asian,
    t_pi=f_pi+m_pi,
    t_aiak=f_aiak+m_aiak,
    t_two_plus=f_two_plus+m_two_plus,
    t_nra=f_nra+m_nra
  )

educ_11_21.b2 <- educ_11_21.b %>%
  select(!c(f_asian,f_pi,f_two_plus,m_asian,m_pi,m_two_plus,t_asian,t_pi,t_two_plus)) %>%
  rename(
    f_ai=f_aiak,
    m_ai=m_aiak,
    t_ai=t_aiak
  )


names(educ_96_03.b)
names(educ_04_10.b)
names(educ_11_21.b)
names(educ_11_21.b2)

## putting all the data into one dataset

educ_all <- rbind(educ_96_03.b,educ_04_10.b,educ_11_21.b2)
table(educ_all$field)

educ_all <- educ_all %>%
  filter(!grepl("Engineering technologies",field)) %>%
  filter(!grepl("Engineering-related",field)) %>%
  filter(!field %in% c("Social sciences","History")) %>%
  mutate(field=ifelse(grepl("Agriculture",field),"Agriculture",field),
         field=ifelse(grepl("Architecture",field),"Architecture",field),
         field=ifelse(grepl("Business",field),"Business",field),
         field=ifelse(grepl("Computer",field),"Computer Science",field),
         field=ifelse(grepl("Foreign",field),"Foreign Language",field),
         field=ifelse(grepl("Mathematics",field),"Math/Stat",field),
         field=ifelse(grepl("Biological",field),"Biological/Biomedical",field),
         field=ifelse(grepl("Liberal",field) | grepl("and humanities",field),"Liberal Arts",field),
         field=ifelse(grepl("Area, ethnic",field),"AECG Studies",field),
         field=ifelse(grepl("Communications ",field),"Comm. Tech",field),
         field=ifelse(grepl("Communication",field),"Communication",field),
         field=ifelse(grepl("Family",field),"Fam and Con/Hum Studies",field),
         field=ifelse(grepl("Home economics",field),"Fam and Con/Hum Studies",field),
         field=ifelse(grepl("Health professions",field),"Health Professions",field),
         field=ifelse(grepl("Homeland",field),"Homeland Sec, LE, FF",field),
         field=ifelse(grepl("Protective Services",field,ignore.case=T),"Homeland Sec, LE, FF",field),
         field=ifelse(grepl("legal",field,ignore.case=T),"Legal Professions and Studies",field),
         field=ifelse(grepl("Mechanic",field),"Mech and Repair Tech",field),
         field=ifelse(grepl("Military",field,ignore.case=T),"Military Tech",field),
         field=ifelse(grepl("classified",field),"Other and not classified",field),
         field=ifelse(grepl("Parks",field),"Parks and Rec",field),
         field=ifelse(grepl("Philosophy",field),"Philosophy and Religion",field),
         field=ifelse(grepl("Precision",field),"Precision Production",field),
         field=ifelse(grepl("Public admin",field),"Public Administration",field),
         field=ifelse(grepl("Theolo",field),"Theology and Religious Vocations",field),
         field=ifelse(grepl("Transportation",field),"Transportation and Materials Moving",field),
         field=ifelse(grepl("English",field),"English",field),
         field=ifelse(grepl("Social sciences and history",field),"SocSci/Hist",field),
         field=ifelse(grepl("Visual",field),"Vis/Perf Arts",field),
         field=ifelse(grepl("professions",field),"Public Administration",field),
         field=ifelse(field=="Public Administration","PubAdmin",field),
         field=ifelse(grepl("Physical sciences",field),"Physical Science",field),
         year=year$Year) %>%
  mutate_at(c(2:22),~str_replace(.,",","")) %>%
  mutate_at(c(2:22),as.numeric)


# Scraping data from the nces.ed.gov website
# https://nces.ed.gov/programs/digest/d19/tables_3.asp#Ch3Sub25
#####################################################
# set up variables need to scrape the different tables

data.col.names <- c("Year","B.no","B.males","B.females","B.pct.female",
                    "M.tot","M.males","M.females",
                    "D.tot","D.males","D.females","field")

fields1 <- c("325.10","325.15","325.20","325.25",
             "325.30","325.35","325.40","325.45",
             "325.50","325.55","325.60",
             "325.65","325.70","325.80","325.85",
             "325.90","325.95")

fields2 <- c("Agriculture","Architecture","Biological/Biomedical","Business",
             "Communication","Computer Science","Education","Engineering",
             "English","Foreign Language","Health Professions",
             "Math/Stat","Physical Science","Psych","PubAdmin",
             "SocSci/Hist","Vis/Perf Arts")

#create a data frame for the scraped data
all.fields <- data.frame()
#scrape the data
for(i in 1:length(fields1)) {
  address <- paste0("https://nces.ed.gov/programs/digest/d19/tables/dt19_",fields1[i],".asp")
  
  h <- read_html(address)
  h2 <- h %>% html_nodes("table") %>% html_table(fill=T)
  h3 <- h2[[5]][-(1:3),-3]
  h3$names <- fields2[i]
  names(h3) <- data.col.names
  h3$Year <- ifelse(h3$Year=="",NA,h3$Year)
  h3$B.no <- ifelse(h3$B.no=="",NA,h3$B.no)
  h3 <- h3 %>% drop_na(Year) %>% drop_na(B.no)
  h3 <- h3[-grep("to",h3$Year),]
  
  all.fields <- rbind(all.fields,h3)
}

#clean up the data
all.fields[,2:11] <- data.frame(lapply(all.fields,function(x) {gsub(",","",x)})) %>% #remove commas from the numbers
  mutate_if(is.factor,as.character) %>% #change factor variables to character variables
  select(2:11) %>% #drop the first column
  mutate_if(is.character,as.numeric) #change character variables to numeric
all.fields$Year <- gsub("1999-2000","1999-00",all.fields$Year) #change 1999-00 to 1999-2000

year.all <- data.frame(table(all.fields$Year)) %>% filter(Freq > 16) #create a data set that has the years of data needed
names(year.all) <- c("Year","Freq") #rename the columns
all.fields <- left_join(year.all,all.fields) #join the data sets



educ_all <- rbind(educ_96_03.b,educ_04_10.b,educ_11_21.b2)

educ_all <- educ_all %>%
  filter(!grepl("Engineering technologies",field)) %>%
  filter(!grepl("Engineering-related",field)) %>%
  filter(!field %in% c("Social sciences","History")) %>%
  mutate(field=ifelse(grepl("Agriculture",field),"Agriculture",field),
         field=ifelse(grepl("Architecture",field),"Architecture",field),
         field=ifelse(grepl("Business",field),"Business",field),
         field=ifelse(grepl("Computer",field),"Computer Science",field),
         field=ifelse(grepl("Foreign",field),"Foreign Language",field),
         field=ifelse(grepl("Mathematics",field),"Math/Stat",field),
         field=ifelse(grepl("Biological",field),"Biological/Biomedical",field),
         field=ifelse(grepl("Liberal",field) | grepl("and humanities",field),"Liberal Arts",field),
         field=ifelse(grepl("Area, ethnic",field),"AECG Studies",field),
         field=ifelse(grepl("Communications ",field),"Comm. Tech",field),
         field=ifelse(grepl("Communication",field),"Communication",field),
         field=ifelse(grepl("Family",field),"Fam and Con/Hum Studies",field),
         field=ifelse(grepl("Home economics",field),"Fam and Con/Hum Studies",field),
         field=ifelse(grepl("Health professions",field),"Health Professions",field),
         field=ifelse(grepl("Homeland",field),"Homeland Sec, LE, FF",field),
         field=ifelse(grepl("Protective Services",field,ignore.case=T),"Homeland Sec, LE, FF",field),
         field=ifelse(grepl("legal",field,ignore.case=T),"Legal Professions and Studies",field),
         field=ifelse(grepl("Mechanic",field),"Mech and Repair Tech",field),
         field=ifelse(grepl("Military",field,ignore.case=T),"Military Tech",field),
         field=ifelse(grepl("classified",field),"Other and not classified",field),
         field=ifelse(grepl("Parks",field),"Parks and Rec",field),
         field=ifelse(grepl("Philosophy",field),"Philosophy and Religion",field),
         field=ifelse(grepl("Precision",field),"Precision Production",field),
         field=ifelse(grepl("Public admin",field),"Public Administration",field),
         field=ifelse(grepl("Theolo",field),"Theology and Religious Vocations",field),
         field=ifelse(grepl("Transportation",field),"Transportation and Materials Moving",field),
         field=ifelse(grepl("English",field),"English",field),
         field=ifelse(grepl("Social sciences and history",field),"SocSci/Hist",field),
         field=ifelse(grepl("Visual",field),"Vis/Perf Arts",field),
         #field=ifelse(field=="Psychology","Psych",field),
         field=ifelse(grepl("professions",field),"Public Administration",field),
         field=ifelse(field=="Public Administration","PubAdmin",field),
         field=ifelse(grepl("Physical sciences",field),"Physical Science",field),
         year=year$Year) %>%
  mutate_at(c(2:22),~str_replace(.,",","")) %>%
  mutate_at(c(2:22),as.numeric)

# creating other data sets that will be used for plots

educ_all_red <- educ_all %>% filter(field %in% unique(all.fields$field)) %>%
  mutate(B.pct.female=round(100*f_total/total,1))

educ_r2 <- educ_all_red %>% 
  filter(year >= 2019) %>%
  mutate(Freq=NA) %>%
  mutate(year=case_when(
    year==2019 ~ "2018-19",
    year==2020 ~ "2019-20",
    year==2021 ~ "2020-21"
  )) %>%
  select(year,Freq,total,m_total,f_total,B.pct.female,field) %>%
  rename(Year=year,B.no=total,B.males=m_total,B.females=f_total)


all.fields.21 <- rbind(all.fields %>% select(Year,Freq,B.no,B.males,B.females,
                                             B.pct.female,field),
                       educ_r2) %>%
  mutate(field=ifelse(field=="PubAdmin","Public Administration",field),
         field=ifelse(field=="Vis/Perf Arts","Visual/Performing Arts",field),
         field=ifelse(field=="Psych","Psychology",field),
         field=ifelse(field=="Math/Stat","Mathematics/Statistics",field),
         field=ifelse(field=="SocSci/Hist","Social Science/History",field)
  )

yr.num <- data.frame(Year=unique(all.fields.21$Year),ord=seq(1,length(unique(all.fields.21$Year))))

all.fields2.21 <- all.fields.21 %>% filter(Year=="2020-21") %>% 
  mutate(field2=fct_reorder(field,B.pct.female)) %>%
  select(field,field2) %>%
  right_join(all.fields.21,by="field") %>%
  left_join(yr.num, by="Year")



educ_bar_data <- educ_all %>%
  select(field,year,f_total) %>%
  mutate(f_total=as.numeric(str_replace(f_total,"\\,","")),
         year=as.character(year),
         field=ifelse(field=="Math/Stat","Mathematics/Statistics",field),
         field=ifelse(grepl("Multi",field),"Multi/Interdisciplinary",field)) %>% 
  pivot_wider(names_from = year,values_from = f_total) %>%
  mutate_at(vars(`1996`:`2021`),~100*(./sum(.,na.rm=T))) %>% 
  mutate_at(vars(`1996`:`2021`),~ifelse(is.na(.),0,.)) %>% 
  mutate(STEM=ifelse(field %in% STEM.field,"STEM","non-STEM"),
         field2=ifelse(`2021`<2 & STEM != "STEM","Other",field)) %>%
  group_by(field2,STEM) %>%
  summarize(across(`1996`:`2021`,sum)) %>%
  ungroup() %>%
  arrange(desc(STEM),`2021`) %>%
  mutate(order=seq(nrow(.),1)) %>%
  mutate(order=ifelse(field2=="Other",21,order)) %>%
  arrange(desc(order)) %>%
  mutate(field2=fct_reorder(field2,order)) %>%
  select(-order) %>%
  pivot_longer(cols = -c(field2,STEM),names_to="year",values_to="pct") %>%
  mutate(pct2=round(pct,1))

educ_bar_data_xlab <- educ_all %>%
  select(field,year,f_total) %>%
  mutate(f_total=as.numeric(str_replace(f_total,"\\,","")),
         year=as.character(year),
         field=ifelse(field=="Math/Stat","Mathematics/Statistics",field)) %>% 
  pivot_wider(names_from = year,values_from = f_total) %>% 
  mutate_at(vars(`1996`:`2021`),~ifelse(is.na(.),0,.)) %>% 
  summarize(across(`1996`:`2021`,sum)) %>% 
  select('1996':'2021') %>%
  ungroup() %>%
  pivot_longer(cols=`1996`:`2021`,names_to = "year",values_to = "f_total")

# color palette for bar plot

my.pal3=c("#000FFF","#177e89","#084c61","#e4b100","#db3a34",
          "gray60","gray70","gray60",rep("gray70",2),rep("gray60",6),"gray70","gray70",
          "gray60","gray70")



educ_bar_data_m <- 
  educ_all %>%
  select(field,year,m_total) %>%
  mutate(m_total=as.numeric(str_replace(m_total,"\\,","")),
         year=as.character(year),
         field=ifelse(field=="Math/Stat","Mathematics/Statistics",field),
         field=ifelse(grepl("Multi",field),"Multi/Interdisciplinary",field)) %>% 
  pivot_wider(names_from = year,values_from = m_total) %>%
  mutate_at(vars(`1996`:`2021`),~100*(./sum(.,na.rm=T))) %>% 
  mutate_at(vars(`1996`:`2021`),~ifelse(is.na(.),0,.)) %>% 
  mutate(STEM=ifelse(field %in% STEM.field,"STEM","non-STEM"),
         field2=ifelse(field %in% educ_bar_data$field2,field,"Other")) %>%
  group_by(field2,STEM) %>%
  summarize(across(`1996`:`2021`,sum)) %>%
  ungroup() %>%
  arrange(desc(STEM),`2021`) %>%
  mutate(order=seq(nrow(.),1)) %>%
  mutate(order=ifelse(field2=="Other",21,order)) %>%
  arrange(desc(order)) %>%
  mutate(field2=fct_reorder(field2,order)) %>%
  select(-order) %>%
  pivot_longer(cols = -c(field2,STEM),names_to="year",values_to="pctm") %>%
  mutate(pctm2=round(pctm,1))

educ_bar_data_all <- left_join(
  educ_bar_data%>% filter(year=="2021" | year=="1996"),
  educ_bar_data_m %>% filter(year=="2021" | year=="1996"),
  by=c("field2"="field2","year"="year","STEM"="STEM"))




educ_21 <- educ3(26) %>%
  mutate_at(c(2:22),~str_replace(.,",","")) %>%
  mutate_at(c(2:22),as.numeric) %>%
  mutate(total=f_total+m_total,B.pct.female=round(100*f_total/total,1))
educ_21 <- educ_21[-c(30,31,35),] %>%
  mutate(field=ifelse(grepl("Agriculture",field),"Agriculture",field),
         field=ifelse(grepl("Architecture",field),"Architecture",field),
         field=ifelse(grepl("Business",field),"Business",field),
         field=ifelse(grepl("Computer",field),"Computer Science",field),
         field=ifelse(grepl("Foreign",field),"Foreign Language",field),
         field=ifelse(grepl("Mathematics",field),"Mathematics/Statistics",field),
         field=ifelse(grepl("Biological",field),"Biological/Biomedical",field),
         field=ifelse(grepl("Liberal",field) | grepl("and humanities",field),"Liberal Arts",field),
         field=ifelse(grepl("Area, ethnic",field),"AECG Studies",field),
         field=ifelse(grepl("Communications ",field),"Comm. Tech",field),
         field=ifelse(grepl("Communication",field),"Communication",field),
         field=ifelse(grepl("Family",field),"Fam and Con/Hum Studies",field),
         field=ifelse(grepl("Home economics",field),"Fam and Con/Hum Studies",field),
         field=ifelse(grepl("Health professions",field),"Health Professions",field),
         field=ifelse(grepl("Homeland",field),"Homeland Sec, LE, FF",field),
         field=ifelse(grepl("Protective Services",field,ignore.case=T),"Homeland Sec, LE, FF",field),
         field=ifelse(grepl("legal",field,ignore.case=T),"Legal Professions and Studies",field),
         field=ifelse(grepl("Mechanic",field),"Mech and Repair Tech",field),
         field=ifelse(grepl("Military",field,ignore.case=T),"Military Tech",field),
         field=ifelse(grepl("classified",field),"Other and not classified",field),
         field=ifelse(grepl("Parks",field),"Parks and Rec",field),
         field=ifelse(grepl("Philosophy",field),"Philosophy and Religion",field),
         field=ifelse(grepl("Precision",field),"Precision Production",field),
         field=ifelse(grepl("Public admin",field),"Public Administration",field),
         field=ifelse(grepl("Theolo",field),"Theology and Religious Vocations",field),
         field=ifelse(grepl("Transportation",field),"Transportation and Materials Moving",field),
         field=ifelse(grepl("English",field),"English",field),
         field=ifelse(grepl("Social sciences and history",field),"SocSci/Hist",field),
         field=ifelse(grepl("Visual",field),"Vis/Perf Arts",field),
         field=ifelse(field=="Psychology","Psych",field),
         field=ifelse(grepl("professions",field),"Public Administration",field),
         field=ifelse(field=="Public Administration","PubAdmin",field),
         field=ifelse(grepl("Physical sciences",field),"Physical Science",field),
         field=ifelse(grepl("3",field),"Engineering Technologies",field)
  )

educ_21_wo <- educ_21 %>% select(field:f_nra,-f_api)
educ_21_wo_stem <- educ_21_wo %>% filter(field %in% STEM.field)
educ_21_wo_stem_no <- educ_21_wo %>% filter(!field %in% STEM.field)

demo.stem.c <- data.frame(educ_21_wo_stem[,-c(1,2)] %>% t()) %>%
  'colnames<-'(STEM.field) %>%
  mutate(b.pct=round(100*`Biological/Biomedical`/sum(`Biological/Biomedical`),1),
         c.pct=round(100*`Computer Science`/sum(`Computer Science`),1),
         e.pct=round(100*`Engineering`/sum(Engineering),1),
         m.pct=round(100*`Mathematics/Statistics`/sum(`Mathematics/Statistics`),1),
         p.pct=round(100*`Physical Science`/sum(`Physical Science`),1)
  ) %>%
  mutate(race=c("White","Black","Hispanic","Asian","Pacific Islander",
                "Am. Ind./AK Nat.","Two or more","Non-Res. Alien")) %>%
  select(race,`Biological/Biomedical`,b.pct,`Computer Science`,c.pct,Engineering,e.pct,
         `Mathematics/Statistics`,m.pct,`Physical Science`,p.pct)


demo.stem.r <- data.frame(
  cbind(educ_21_wo_stem[,-c(1,2)] %>% t(),
        round(100*(educ_21_wo_stem[,-c(1,2)] %>% t())/rowSums(educ_21_wo_stem[,-c(1,2)] %>% t()),1))) %>%
  'colnames<-'(c(STEM.field,"b.pct","c.pct","e.pct","m.pct","p.pct")) %>%
  mutate(race=c("White","Black","Hispanic","Asian","Pacific Islander",
                "Am. Ind./AK Nat.","Two or more","Non-Res. Alien")) %>%
  select(race,`Biological/Biomedical`,b.pct,`Computer Science`,c.pct,Engineering,e.pct,
         `Mathematics/Statistics`,m.pct,`Physical Science`,p.pct)



demo.race <- data.frame(
  race=c("White","Black","Hispanic","Asian","Pacific Islander",
         "Am. Ind./AK Nat.","Two or more","Non-Res. Alien"),
  total=educ_21_wo %>% select(-field,-f_total) %>%
    colSums(),
  pct=round(100*(educ_21_wo %>% select(-field,-f_total) %>%
                   colSums()/sum(educ_21_wo$f_total)),1),
  total.stem = demo.stem.r %>% select(!ends_with("pct"),-race) %>% rowSums()) %>% 
  mutate(stem.pct=round(100*total.stem/sum(total.stem),1),
         total.non.stem = total-total.stem,
         non.stem.pct=round(100*total.non.stem/sum(total.non.stem),1))


educ_96 <- educ_all %>%
  filter(year==1996) %>% 
  select(field,total,f_total,f_white,f_black,f_hisp,f_api,f_ai,f_nra) %>%
  mutate(across(total:f_nra,as.numeric)) %>%
  mutate(across(f_total:f_nra,~round(.x*100/total,0)))

educ_96_wo <- educ_all %>%
  filter(year==1996) %>% 
  select(field,f_white,f_black,f_hisp,f_api,f_ai,f_nra) %>%
  mutate(across(f_white:f_nra,as.numeric),
         field=ifelse(field=="Math/Stat","Mathematics/Statistics",field),
         STEM=ifelse(field %in% STEM.field,"STEM","Non-STEM")) %>%
  `colnames<-`(c("field","White","Black","Hispanic","Asian/Pacific Islander",
                 "Am. Ind./AK Nat.","Non-Res. Alien","STEM")) %>%
  pivot_longer(-c(field,STEM),values_to="tots",names_to="race") %>% 
  group_by(STEM,race) %>%
  summarise(tot=sum(tots)) %>%
  ungroup() %>%
  pivot_wider(names_from=STEM,values_from=tot) %>%
  mutate(total=STEM+`Non-STEM`,
         pct.stem=round(100*STEM/total,1),
         pct.non.stem=round(100*`Non-STEM`/total,1)) %>%
  pivot_longer(cols=c(pct.stem,pct.non.stem),names_to="col",values_to="tot")


row_labs <- demo.stem.r %>% 
  select(race,all_of(STEM.field)) %>% 
  pivot_longer(!race,names_to="field",values_to="count") %>% 
  group_by(field) %>% summarize(n=sum(count)) %>% 
  #mutate(field_lab=paste0(field,"\nn=",n)) %>%
  mutate(field_no=paste0("n = ",comma(n)))


col_labs <- demo.stem.r %>%
  select(race,all_of(STEM.field)) %>%
  pivot_longer(!race,names_to="field",values_to="count") %>% 
  group_by(race) %>% 
  summarize(tot=sum(count))

col.pal <- my.pal3[1:5]

educ_21.f <- educ_21 %>%
  select(f_total:f_nra)
educ_21.m <- educ_21 %>%
  select(m_total:m_nra)

educ_pct <- round((educ_21.f/(educ_21.f+educ_21.m))*100,1) %>%
  mutate(field=educ_21$field) %>%
  select(field,f_white:f_nra,-f_api) %>% 
  filter(field %in% STEM.field) %>%
  pivot_longer(!field,names_to="race",values_to = "pct") %>%
  mutate(race2=str_replace(race,"f_","")) %>%
  mutate(race2=case_when(
    race2=="aiak" ~ "Am. Ind./AK Nat.",
    race2=="asian" ~ "Asian",
    race2=="pi" ~ "Pacific Islander",
    race2=="black" ~ "Black",
    race2=="hisp" ~ "Hispanic",
    race2=="white" ~ "White",
    race2=="nra" ~ "Non-Res. Alien",
    race2=="two_plus" ~ "Two or more"
  ))


my.pal2 = c("gray60","gray60","#000FFF","gray60","gray60","#177e89",
           "gray70","#084c61","gray70","gray70","gray60","#db3a34","gray60",
           "#e4b100","gray60","gray60","gray70","gray60")

educ_all %>% 
  select(total,f_total,field,year) %>%
  mutate(field=ifelse(field %in% unique(educ_all_red$field),field,"Other"),
         f_total=as.numeric(str_replace(f_total,"\\,","")),
         field=ifelse(grepl("Math/Stat",field),"Mathematics/Statistics",field)) %>%
  mutate(STEM=ifelse(field %in% STEM.field,"STEM","non-STEM")) %>%
  arrange(desc(STEM),desc(f_total)) %>%
  mutate(order=seq(nrow(.),1)) %>%
  mutate(field2=fct_reorder(field,order)) %>%
  group_by(year,field2) %>%
  summarize(f_total=sum(f_total)) %>%
  
  ggplot(aes(x=as.character(year),y=f_total,fill=field2)) +
  geom_bar(position="fill",stat="identity",col="white")+
  scale_fill_manual(values=my.pal2)+
  scale_color_manual(values=c("#000FFF","#177e89","#084c61","#db3a34","#e49000"))+
  geom_text(data=. %>% filter(year==2021,field2 %in% STEM.field),
            aes(x=28.25,y=pt.labs3,label=field2,
                col=field2),size=9,hjust=0) +
  geom_segment(data=data.frame(
    x=rep(-1,5),xend=rep(.5,5),y=seq(.94,1.01,length.out=5),yend=c(.9475,.9615,.97,.97,.987),field2=NA),
    aes(x=x,xend=xend,y=y,yend=yend))





my.pal3=c("#000FFF","#177e89","#084c61","#e49000","#db3a34",
          "gray70","gray50","gray70",rep("gray50",2),rep("gray70",2),"gray50",rep("gray70",4),"gray50",
          "gray50","gray70")

educ_all %>% 
  select(total,f_total,field,year) %>%
  mutate(#field=ifelse(field %in% unique(educ_all_red$field),field,"Other"),
    f_total=as.numeric(str_replace(f_total,"\\,","")),
    field=ifelse(grepl("Math/Stat",field),"Mathematics/Statistics",field)) %>%
  mutate(STEM=ifelse(field %in% STEM.field,"STEM","non-STEM")) %>%
  group_by(year,STEM,field) %>%
  summarize(f_total=sum(f_total)) %>% filter(year==1996) %>% 
  arrange(desc(STEM),desc(f_total)) %>% ungroup() %>% 
  mutate(pct=round(100*f_total/sum(f_total),2))

educ_all_21 <- educ_all %>% 
  select(total,f_total,field,year) %>%
  mutate(#field=ifelse(field %in% unique(educ_all_red$field),field,"Other"),
    f_total=as.numeric(str_replace(f_total,"\\,","")),
    field=ifelse(grepl("Math/Stat",field),"Mathematics/Statistics",field)) %>%
  mutate(STEM=ifelse(field %in% STEM.field,"STEM","non-STEM")) %>%
  group_by(year,STEM,field) %>%
  summarize(f_total=sum(f_total)) %>% filter(year==2021) %>% 
  arrange(desc(STEM),desc(f_total)) %>% 
  ungroup() %>% 
  mutate(pct=round(100*f_total/sum(f_total),2)) %>%
  select(-f_total) %>%
  pivot_wider(names_from = year,values_from = pct) %>%
  drop_na() %>%
  mutate(field2=ifelse(`2021`<2 & STEM != "STEM","Other",field)) %>%
  group_by(STEM,field2) %>%
  summarize(pct=sum(`2021`)) %>% 
  arrange(desc(STEM),pct) %>%
  ungroup() %>%
  mutate(order=seq(nrow(.),1)) %>%
  mutate(field2=fct_reorder(field2,order))



  ggplot(aes(x=1,y=pct,fill=field2)) +
  geom_bar(stat="identity",col="white")+
  scale_fill_manual(values=my.pal3)


  
educ_all %>%
  select(field,year,f_total) %>%
  mutate(f_total=as.numeric(str_replace(f_total,"\\,","")),
         field=ifelse(field=="Math/Stat","Mathematics/Statistics",field)) %>%
  pivot_wider(names_from = field,values_from = f_total) %>%
  mutate(tots=rowSums(.[,2:ncol(.)],na.rm=T)) %>%
  pivot_longer(cols = -c(year,tots)) %>%
  mutate(value=ifelse(is.na(value),0,value)) %>%
  mutate(pct=round(100*value/tots,2),
         STEM=ifelse(name %in% STEM.field,"STEM","non-STEM")) %>%
  view()


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

my.pal3=c("#000FFF","#177e89","#084c61","#e4b100","#db3a34",
          "gray60","gray70","gray60",rep("gray70",2),rep("gray60",6),"gray70","gray70",
          "gray60","gray70")

educ_bar_data %>% left_join(educ_bar_data_xlab) %>%
  ggplot(aes(x=year,y=pct,fill=field2)) +
  geom_bar(stat="identity",col="white")+
  #geom_text(aes(x=year,y=-1,label=year),angle=90,hjust=1,size=4)+
  geom_text(aes(x=year,y=-1,label=scales::comma(f_total)),angle=90,hjust=1,size=4.5)+
  geom_text(aes(x=year,y=101,label=year),angle=90,hjust=0,size=4.5)+
  scale_fill_manual(values=my.pal3)+
  #expand_limits(x=c(-11.5,38.5))+
  expand_limits(x=c(-13.5,40.5))+
  
  geom_text(aes(x=-5.75,y=105,label="1996"),size=6.5)+
  geom_text(aes(x=-5.75,y=102.5,label="(Total Degrees: 640,171)"),size=4.5)+
  geom_text(aes(x=32.75,y=105,label="2021"),size=6.5)+
  geom_text(aes(x=32.75,y=102.5,label="(Total Degrees: 1,202,926)"),size=4.5)+
  #geom_text(aes(x=-2,y=-4,label="Total Degrees"),size=5.5)+
  #geom_text(aes(x=29,y=-4,label="Total Degrees"),size=5.5)+
  geom_text(aes(x=13.5,y=-10,label="Total Degrees Awarded"),size=5.5)+
  
  
  
  #STEM Labels
  geom_text(data=. %>% filter(year==2021,STEM=="STEM"),
            aes(x=29.25,y=c(85.5,88,90.5,93,96.4),
                label=paste0(field2," (",format(pct2,2),"%)")),
                col=my.pal3[5:1],size=5.5,hjust=0) +
  geom_text(data=. %>% filter(year==1996,STEM=="STEM"),
            aes(x=-2.25,y=c(89.5,92,94.5,97,99.5),
                label=paste0(field2," (",format(pct2,2),"%)")),
            col=my.pal3[5:1],size=5.5,hjust=1) +
  
  
  #Non-STEM Labels
  geom_text(data=. %>% filter(year==2021,STEM=="non-STEM"),
            aes(x=29.25,y=c(2.25,5.5,7.6,9.8,12.1,14.4,16.85,19.6,
                            23.5,28.2,
                            33.7,40.3,48,59.8,76.9),
                label=paste0(field2," (",format(pct2,2),"%)")),
            col=c(rep("gray50",10),rep("gray20",5)),size=5.5,hjust=0) +
  geom_text(data=. %>% filter(year==1996,STEM=="non-STEM"),
            aes(x=-2.25,y=c(2.85,6.3,9.5,13.7,15.85,17.6,
                            19.55,21.65,25.3,29.8,38.2,
                            49.15,58.1,70.9,84.85),
                label=paste0(field2," (",format(pct2,2),"%)")),
            col=c(rep("gray50",10),rep("gray20",5)),size=5.5,hjust=1) +
  
  ## draw some line segments from the text to the block
  geom_segment(data=. %>% filter(year==2021,STEM=="STEM"),
               aes(x=-2,y=c(89.5,92,94.5,97,99.5),
                   xend=.4,yend=c(90.75,91.7,92.75,94.15,97.5)),
               col=my.pal3[5:1]) +
  geom_segment(data=. %>% filter(year==1996,STEM=="STEM"),
               aes(x=29,y=c(85.5,88,90.5,93,96.4),
                   xend=26.6,yend=c(86.85,87.85,89.35,91.55,96.4)),
               col=my.pal3[5:1]) +
  geom_segment(data=. %>% filter(year==1996,STEM=="non-STEM"),
               aes(x=-2,y=c(2.85,6.3,9.5,13.7,15.85,17.6,
                            19.55,21.65,25.3,29.8,38.2,
                            49.15,58.1,70.9,84.85),
                   xend=.4,yend=c(2.85,6.3,9.5,13.7,15.85,17.6,
                                  19.55,21.65,25.3,29.8,38.2,
                                  49.15,58.1,70.9,84.85)),
               col="gray40") +
  geom_segment(data=. %>% filter(year==2021,STEM=="non-STEM"),
               aes(x=29,y=c(2.25,5.5,7.6,9.8,12.1,14.4,16.85,19.6,
                            23.5,28.2,
                            33.7,40.3,48,59.8,76.9),
                   xend=26.6,yend=c(2.25,5.5,7.6,9.8,12.1,14.4,16.85,19.6,
                                    23.5,28.2,
                                    33.7,40.3,48,59.8,76.9)),
               col="gray40") +
  
  ## left side text
  geom_segment(aes(x=-11,y=89,xend=-11,yend=100),col="#660000") +
  geom_text(aes(x=-11.5,y=94.5,label="STEM - 9.7%"),size=5.5,hjust=.5,angle=90,col="#660000") +
  geom_segment(aes(x=-11,y=37.2,xend=-11,yend=85.9),col="gray50") +
  geom_text(aes(x=-11.5,y=61.55,label="Popular Non-STEM - 58.2%"),size=5.5,hjust=.5,angle=90,col="gray20") +
  #geom_segment(aes(x=-11,y=38.2,xend=-11,yend=49.1),col="gray40") +
  #geom_text(aes(x=-11.5,y=43.65,label="non-STEM"),size=5.5,hjust=.5,angle=90) +
  geom_segment(aes(x=-11,y=1.85,xend=-11,yend=30.8),col="gray40") +
  geom_text(aes(x=-11.5,y=16.325,label="Other Non-STEM - 32.0%"),size=5.5,hjust=.5,angle=90,col="gray50") +
  #geom_segment(aes(x=-11,y=.6,xend=-11,yend=15.95),col="gray40") +
  #geom_text(aes(x=-11.5,y=8.25,label="non-STEM"),size=5.5,hjust=.5,angle=90) +
  
  ## right side text
  geom_segment(aes(x=38,y=84.5,xend=38,yend=97.4),col="#660000") +
  geom_text(aes(x=38.5,y=90.5,label="STEM - 13.6%"),size=5.5,hjust=.5,angle=270,col="#660000") +
  geom_segment(aes(x=38,y=32.7,xend=38,yend=77.9),col="gray40") +
  geom_text(aes(x=38.5,y=55.3,label="Popular Non-STEM - 55.7%"),size=5.5,hjust=.5,angle=270,col="gray20") +
  geom_segment(aes(x=38,y=1.25,xend=38,yend=29.2),col="gray40") +
  geom_text(aes(x=38.5,y=15.225,label="Other Non-STEM - 30.6%"),size=5.5,hjust=.5,angle=270,col="gray50") +
  #geom_segment(aes(x=38,y=18.9,xend=38,yend=28.2),col="gray40") +
  #geom_text(aes(x=38.5,y=23.55,label="non-STEM"),size=5.5,hjust=.5,angle=270) +
  #geom_segment(aes(x=38,y=1,xend=38,yend=15.1),col="gray40") +
  #geom_text(aes(x=38.5,y=8.05,label="non-STEM"),size=5.5,hjust=.5,angle=270) +

  
  labs(title="Bachelor's Degrees Awarded to Females by Discipline",
       subtitle="STEM vs. Non-STEM, 1996-2021",
       caption="Source: National Center for Education Statistics") +

  theme_void()+
  theme(legend.position="none",
        plot.title=element_text(size=32,hjust=.5),
        plot.subtitle=element_text(size=22,hjust=.5),
        plot.caption=element_text(size=12,hjust=.5),
        panel.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.margin = margin(.5,0,.5,0, "cm"))

ggsave("educ_bar9621_010224.png",width=20,height=17,dpi=300)

educ_bar_data %>% filter(year==2021,STEM=="STEM") %>% 
  arrange(desc(pct)) %>% 
  mutate(tots1=cumsum(pct)) %>%
  mutate(tots=100-cumsum(pct)) %>% View()


write.csv(.,"educ_bar_data21.csv",row.names=F)
  

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

test.text <- c("Since 1996 there has been an increase in females being awarded STEM degrees,
led by increases in Biological/Biomedical, Computer Science and Engineering degrees;
however, they still lag behind the five most popular non-STEM degrees
(even though there has been a decrease in Business, Social Sciece/History and Education degrees).
English and Liberals Arts have also decreased.")

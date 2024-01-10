library(tidyverse)
library(scales)
library(patchwork)
library(gt)

#######################

### visualizations are in order of appearance in the blog post

#######################

#######################

# % female bachelor's degrees 1971-2021

ggplot(all.fields2.21,aes(x=Year,y=B.pct.female,group=field2))+
  geom_point(col='gray60')+
  geom_line(data=all.fields2.21 %>% filter(field2 %in% STEM.field),
            aes(x=Year,y=B.pct.female,group=field2,col=field2),size=1)+
  geom_line(data=all.fields2.21 %>% filter(!field2 %in% STEM.field),
            aes(x=Year,y=B.pct.female,group=field2), col='gray60',lty=2)+
  expand_limits(x=c(-10.5,62.5))+
  labs(title="Percentage of Bachelor's Degrees Awarded to Females", 
       subtitle="STEM vs. non-STEM, 1971-2021\n",
       caption="\nSource: National Center for Education Statistics")+
  
  ## adding labels
  ### Left Side
  geom_text(data=all.fields2.21 %>% 
              filter(field2 %in% STEM.field) %>% 
              filter(field2 != "Physical Science"),
            aes(x=0.5,y=ifelse(Year=="1970-71",B.pct.female,NA),
                label=paste0(field2," ",format(B.pct.female,digits=2),"%"),color=field2),size=8,hjust=1)+ ## STEM labels (w/o Physical Science)
  geom_text(data=all.fields2.21 %>% filter(field2 == "Physical Science"),
            aes(x=0.5,y=ifelse(Year=="1970-71",as.numeric(B.pct.female)+1.5,NA),
                label=paste0(field2," ",format(B.pct.female,digits=3),"%"),color=field2),size=8,hjust=1)+ ## Adding Physical Science (remove overlap with Computer Science)
  geom_text(data=all.fields2.21 %>% filter(!field2 %in% STEM.field,!field2 %in% c("Education","Foreign Language")),
            aes(x=0.5,y=ifelse(Year=="1970-71",B.pct.female,NA),
                label=paste0(field2," ",format(B.pct.female,digits=2),"%")),size=7,col="gray50",hjust=1)+  ## Other Disciplines
  geom_text(data=all.fields2.21 %>% filter(field2=="Foreign Language", Year=="1970-71"),
            aes(x=0.5,y=B.pct.female-.2,label=paste0(field2," ",format(B.pct.female,digits=3),"%")),
            col="gray50",size=7,hjust=1)+
  geom_text(data=all.fields2.21 %>% filter(field2=="Education", Year=="1970-71"),
            aes(x=0.5,y=B.pct.female+.4,label=paste0(field2," ",format(B.pct.female,digits=3),"%")),
            col="gray50",size=7,hjust=1)+
  
  ### Right Side
  geom_text(data=all.fields2.21 %>% filter(field2 %in% STEM.field, !field2 %in% non_stem_over, Year=="2020-21"),
            aes(x=51.5,y=B.pct.female,label=paste0(format(B.pct.female,digits=3),"% ",field2),col=field2),size=8,hjust=0)+
  geom_text(data=all.fields2.21 %>% filter(!field2 %in% STEM.field, !field2 %in% non_stem_over, Year=="2020-21"),
            aes(x=51.5,y=B.pct.female,label=paste0(format(B.pct.female,digits=3),"% ",field2)),col="gray50",size=7,hjust=0)+
  geom_text(data=all.fields2.21 %>% filter(field2=="Public Administration", Year=="2020-21"),
            aes(x=51.5,y=B.pct.female+.2,label=paste0(format(B.pct.female,digits=3),"% ",field2)),col="gray50",size=7,hjust=0)+
  geom_text(data=all.fields2.21 %>% filter(field2=="Education", Year=="2020-21"),
            aes(x=51.5,y=B.pct.female-.5,label=paste0(format(B.pct.female,digits=3),"% ",field2)),col="gray50",size=7,hjust=0)+
  geom_text(data=all.fields2.21 %>% filter(field2=="Visual/Performing Arts", Year=="2020-21"),
            aes(x=51.5,y=B.pct.female-.4,label=paste0(format(B.pct.female,digits=3),"% ",field2)),col="gray50",size=7,hjust=0)+
  
  
  scale_color_manual(values=c("#000FFF","#084c61","#177e89","#e4b100","#db3a34"))+
  theme_minimal()+
  theme(legend.position="none",
        axis.text.x=element_text(angle=90,vjust=.5),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.grid.major.x=element_line(color="gray"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5,size=40),
        plot.subtitle=element_text(hjust=.5,size=30),
        plot.caption=element_text(size=12,hjust=.5),
        axis.text.y=element_blank(),
        plot.background = element_rect(fill = "#E8E8E8"),
        plot.margin = margin(.5,0,.5,0, "cm")
  )

ggsave("female.ba.pct21b1_010224.png",dpi=320,height=22,width=30)

##########################################################################################

# bachelor's degrees awarded to females 96-21
educ_bar_data %>% left_join(educ_bar_data_xlab) %>%
  ggplot(aes(x=year,y=pct,fill=field2)) +
  geom_bar(stat="identity",col="white")+
  geom_text(aes(x=year,y=-1,label=scales::comma(f_total)),angle=90,hjust=1,size=4.5)+
  geom_text(aes(x=year,y=101,label=year),angle=90,hjust=0,size=4.5)+
  scale_fill_manual(values=my.pal3)+
  expand_limits(x=c(-13.5,40.5))+
  
  geom_text(aes(x=-5.75,y=105,label="1996"),size=6.5)+
  geom_text(aes(x=-5.75,y=102.5,label="(Total Degrees: 640,171)"),size=4.5)+
  geom_text(aes(x=32.75,y=105,label="2021"),size=6.5)+
  geom_text(aes(x=32.75,y=102.5,label="(Total Degrees: 1,202,926)"),size=4.5)+
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
  geom_segment(aes(x=-11,y=1.85,xend=-11,yend=30.8),col="gray40") +
  geom_text(aes(x=-11.5,y=16.325,label="Other Non-STEM - 32.0%"),size=5.5,hjust=.5,angle=90,col="gray50") +
  
  ## right side text
  geom_segment(aes(x=38,y=84.5,xend=38,yend=97.4),col="#660000") +
  geom_text(aes(x=38.5,y=90.5,label="STEM - 13.6%"),size=5.5,hjust=.5,angle=270,col="#660000") +
  geom_segment(aes(x=38,y=32.7,xend=38,yend=77.9),col="gray40") +
  geom_text(aes(x=38.5,y=55.3,label="Popular Non-STEM - 55.7%"),size=5.5,hjust=.5,angle=270,col="gray20") +
  geom_segment(aes(x=38,y=1.25,xend=38,yend=29.2),col="gray40") +
  geom_text(aes(x=38.5,y=15.225,label="Other Non-STEM - 30.6%"),size=5.5,hjust=.5,angle=270,col="gray50") +
  
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


##########################################################################################

# bar chart comparing males and females

educ_bar_data_all %>%
  filter(year=="2021") %>%
  ggplot()+
  geom_col(aes(x=-pctm-10,y=field2,fill=field2,width=pctm/max(pctm+5)))+
  geom_text(aes(x=-pctm-12,y=field2,label=paste0(format(pctm2,digits=2),"%"),size=log(pctm)),hjust=1)+
  geom_col(aes(x=pct+10,y=field2,fill=field2,width=pct/max(pct+5)))+
  geom_text(aes(x=pct+12,y=field2,label=paste0(format(pct2,digits=2),"%"),size=log(pct)),hjust=0)+
  geom_col(data=data.frame(
    pctm=rep(10,20),field2=(educ_bar_data_all %>%
                              filter(year=="2021"))$field2),
    aes(x=pctm,y=field2),fill="#E8E8E8",col="#E8E8E8"
  )+
  geom_col(data=data.frame(
    pctm=rep(-10,20),field2=(educ_bar_data_all %>%
                               filter(year=="2021"))$field2),
    aes(x=pctm,y=field2),fill="#E8E8E8",col="#E8E8E8"
  )+
  geom_text(aes(x=0,y=field2,label=field2,col=field2),size=5)+
  geom_text(aes(x=0,y=22.5,label="Field of Study"),size=7)+
  geom_text(data=data.frame(
    x=c(-22.5,22.5),y=c(22.5,22.5),label=c("Males","Females")
  ),
  aes(x=x,y=y,label=label),size=6)+
  geom_text(data=data.frame(
    x=c(-22.5,22.5),y=c(21.5,21.5),label=c("Total Degrees: 844,515","Total Degrees: 1,202,926")
  ),
  aes(x=x,y=y,label=label),size=4.5)+
  #scale_fill_manual(values=my.pal3)+
  scale_fill_manual(values=c(my.pal3[1:5],rep("gray70",15)))+
  scale_color_manual(values=c(rep("#660000",5),rep("gray40",15)))+
  expand_limits(x=c(-45,45),y=c(0,24))+
  scale_y_discrete(limits=rev)+
  labs(title="Bachelor's Degrees Awarded by Discipline and Sex",
       subtitle="2021",
       caption="Source: National Center for Education Statistics")+
  theme_void()+
  theme(legend.position="none",
        plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=18,hjust=.5),
        plot.caption=element_text(size=10,hjust=.5),
        panel.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.margin = margin(.5,0,.5,0, "cm"))

ggsave("educ_deg_mvf_21_010224.png",width=12,height=9,dpi=300)


##########################################################################################

# table with race distribution

demo.race.tab <- demo.race %>%
  mutate_if(is.numeric,scales::comma) %>%
  gt() %>%
  tab_header(
    title="Distribution of Degrees by Race",
    subtitle="2021"
  ) %>%
  tab_spanner(
    label="Overall",
    columns=c(total,pct)
  ) %>%
  tab_spanner(
    label="STEM",
    columns=c(total.stem,stem.pct)
  ) %>%
  tab_spanner(
    label="Non-STEM",
    columns=c(total.non.stem,non.stem.pct)
  ) %>%
  cols_label(
    race="Race",
    total="Total",
    stem.pct="Percent",
    total.stem="Total",
    pct="Percent",
    total.non.stem="Total",
    non.stem.pct="Percent"
  ) %>%
  tab_footnote(
    footnote="Source: National Center for Education Statistics"
  ) %>%
  tab_style(
    list(cell_text(color="gray40")),
    locations=cells_body(
      columns=c(total.non.stem,non.stem.pct)
    )
  ) %>%   tab_style(
    list(cell_text(color="gray40")),
    locations=cells_column_labels(
      columns=c(total.non.stem,non.stem.pct)
    )
  ) %>%   tab_style(
    list(cell_text(color="gray40")),
    locations=cells_column_spanners(
      spanners="Non-STEM"
    )
  ) %>%
  tab_style(
    list(cell_text(color="#660000")),
    locations=cells_body(
      columns=c(total.stem,stem.pct)
    )
  ) %>%
  tab_style(
    list(cell_text(color="#660000")),
    locations=cells_column_labels(
      columns=c(total.stem,stem.pct)
    )
  ) %>% 
  tab_style(
    list(cell_text(color="#660000")),
    locations=cells_column_spanners(
      spanners='STEM'
    )
  ) %>%
  tab_options(
    table.background.color="#E8E8E8",
    table.width=pct(100),
    data_row.padding = px(5),
  )

gtsave(demo.race.tab,"demo.race.tab.png")


##########################################################################################

# STEM vs non-STEM by race 1996 & 2021


educ_96_wo %>%
  ggplot()+
  geom_col(aes(tot,race,fill=col),width=.75)+
  scale_fill_manual(values=c("gray60","#660000"))+
  expand_limits(x=c(-80,150),y=c(0,7.75))+
  #geom_text(data=data.frame(x=c(0,0),y=c(1.5,1.5),label=c("STEM","Non-STEM")),
  #aes(x=x,y=y,label=label),size=5,hjust=0)
  geom_text(data = . %>% filter(col=="pct.stem"),
            aes(x=rep(-10,6),y=race,label=paste0(format(tot,digits=2),"%")),
            size=7,hjust=1,col="#660000") +
  geom_text(data = . %>% filter(col=="pct.non.stem"),
            aes(x=rep(110,6),y=race,label=paste0(format(tot,digits=3),"%")),
            size=7,hjust=0,col="gray30")+
  
  geom_text(aes(x=-10,y=6.75,label="STEM"),size=7,hjust=1,col="#660000")+
  geom_text(aes(x=110,y=6.75,label="Non-STEM"),size=7,hjust=0,col="gray30")+
  
  geom_text(aes(x=-75,y=race,label=race),size=7,hjust=0)+
  geom_text(aes(x=-75,y=6.75,label="Race"),size=7,hjust=0)+
  
  labs(title="Female STEM and Non-STEM Degrees, 1996",
       caption="Source: National Center for Education Statistics")+
  theme_void()+
  theme(legend.position="none",
        plot.title=element_text(size=32,hjust=.5),
        plot.subtitle=element_text(size=24,hjust=.5),
        plot.caption=element_text(size=12,hjust=.5),
        panel.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.margin = margin(.5,0,.5,0, "cm"))

ggsave("demo.race.bar96.png",width=14,height=8,dpi=300)



demo.race %>% #filter(race=="White") %>%
  mutate(pct.stem=total.stem/total,pct.non.stem=total.non.stem/total) %>%
  pivot_longer(cols=c(pct.stem,pct.non.stem),names_to="col",values_to="tot") %>%
  ggplot()+
  geom_col(aes(tot,race,fill=col),width=.75)+
  scale_fill_manual(values=c("gray60","#660000"))+
  expand_limits(x=c(-.8,1.5),y=c(0,9.75))+
  geom_text(data = . %>% filter(col=="pct.stem"),
            aes(x=rep(-.1,8),y=race,label=paste0(format(round(100*tot,1),digits=2),"%")),
            size=6,hjust=1,col="#660000") +
  geom_text(data = . %>% filter(col=="pct.non.stem"),
            aes(x=rep(1.1,8),y=race,label=paste0(format(round(100*tot,1),digits=3),"%")),
            size=6,hjust=0,col="gray30")+
  
  geom_text(aes(x=-.1,y=8.75,label="STEM"),size=6,hjust=1,col="#660000")+
  geom_text(aes(x=1.1,y=8.75,label="Non-STEM"),size=6,hjust=0,col="gray30")+
  
  geom_text(aes(x=-.75,y=race,label=race),size=6,hjust=0)+
  geom_text(aes(x=-.75,y=8.75,label="Race"),size=6,hjust=0)+
  
  labs(title="Female STEM and Non-STEM Degrees, 2021",
       caption="Source: National Center for Education Statistics")+
  theme_void()+
  theme(legend.position="none",
        plot.title=element_text(size=28,hjust=.5),
        plot.subtitle=element_text(size=22,hjust=.5),
        plot.caption=element_text(size=12,hjust=.5),
        panel.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.background = element_rect(fill="#E8E8E8",color="#E8E8E8"),
        plot.margin = margin(.5,0,.5,0, "cm"))

ggsave("demo.race.bar.png",width=11,height=8,dpi=300)


##########################################################################################

# stem within race
for(i in 1:5){
  ex.plot <- demo.stem.r %>% select(race,b.pct,c.pct,e.pct,m.pct,p.pct) %>%
    pivot_longer(cols=b.pct:p.pct,names_to="col",values_to="pct") %>%
    mutate(field_no=rep(row_labs$field_no,8),
           field=rep(row_labs$field,8),
           field2=rep(STEM.field2,8),
           pct2=format(pct,digits=2)) %>% 
    filter(field==STEM.field[i]) %>%
    left_join(col_labs) %>%
    mutate(race=c("White","Black","Hispanic","Asian",
                  "Pacific\nIslander","Am. Ind/\nAlaska Nat",
                  "Two or More","Non-Res.\nAlien")) %>%
    mutate(race_lab=paste(race,"\nn = ",comma(tot))) %>%
    ggplot(aes(race_lab,field2))+
    geom_tile(aes(alpha=pct/max(pct,na.rm=T)),col="gray10",fill=col.pal[i])+
    #scale_fill_gradient(low="#FFFFB7",high="#FFD400")+
    geom_text(data=.%>% filter(max(pct)>10,pct/max(pct)>=.70),aes(label=paste0(pct2,"%")),size=5,color="gray90",fontface="bold")+
    geom_text(data=.%>% filter(pct/max(pct)<.70),aes(label=paste0(pct2,"%")),size=5,color="gray10",fontface="bold")+
    geom_text(data=.%>% filter(max(pct)<10),aes(label=paste0(pct2,"%")),size=5,color="gray10",fontface="bold")+
    geom_text(aes(x=.25,y=1,label=field2),size=5,col=col.pal[i],hjust=1)+
    geom_text(aes(x=9.125,y=1,label=field_no),size=4.5,col="gray30")+
    expand_limits(x=c(-1,10))+
    theme_minimal()+
    theme(legend.position = "none",
          panel.background = element_rect(color="#E8E8E8",fill="#E8E8E8"),
          plot.background = element_rect(color="#E8E8E8",fill="#E8E8E8"),
          axis.title=element_blank(),
          axis.text.x = element_text(size=11,face="bold",color="black"),
          axis.text.y=element_blank())
  nam=STEM.field[i]
  assign(nam,ex.plot)
}

((`Biological/Biomedical`+scale_x_discrete(position="top")+
    theme(axis.text.x=element_text(vjust=1)))/
    (`Computer Science`+theme(axis.text.x=element_blank()))/
    (Engineering+theme(axis.text.x=element_blank()))/
    (`Mathematics/Statistics`+theme(axis.text.x=element_blank()))/
    `Physical Science`)+plot_annotation(
      title="Females Receiving STEM Degrees, 2021",
      subtitle="Discipline Distribution by Race",
      caption="Source: National Center for Education Statistics"
    ) & 
  theme(plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=18,hjust=.5),
        plot.margin = margin(.5,0,.5,0, "cm"),
        plot.caption=element_text(size=12,hjust=.5),
        plot.background = element_rect(color="#E8E8E8",fill="#E8E8E8"))


ggsave("stem.by.race_010224.png",dpi=320,height=8,width=12)


# race within discipline
for(i in 1:5){
  ex.plot <- demo.stem.c %>% select(race,b.pct,c.pct,e.pct,m.pct,p.pct) %>%
    pivot_longer(cols=b.pct:p.pct,names_to="col",values_to="pct") %>%
    mutate(field_no=rep(row_labs$field_no,8),
           field=rep(row_labs$field,8),
           field2=rep(STEM.field2,8),
           pct2=format(pct,digits=2)) %>% 
    filter(field==STEM.field[i]) %>%
    left_join(col_labs) %>%
    mutate(race=c("White","Black","Hispanic","Asian",
                  "Pacific\nIslander","Am. Ind/\nAlaska Nat",
                  "Two or More","Non-Res.\nAlien")) %>%
    mutate(race_lab=paste(race,"\nn = ",comma(tot))) %>%
    ggplot(aes(race_lab,field2))+
    geom_tile(aes(alpha=pct/max(pct,na.rm=T)),col="gray10",fill=col.pal[i])+
    geom_text(data=.%>% filter(max(pct)>10,pct/max(pct)>=.70),aes(label=paste0(pct2,"%")),size=5,color="gray90",fontface="bold")+
    geom_text(data=.%>% filter(pct/max(pct)<.70),aes(label=paste0(pct2,"%")),size=5,color="gray10",fontface="bold")+
    geom_text(data=.%>% filter(max(pct)<10),aes(label=paste0(pct2,"%")),size=5,color="gray10",fontface="bold")+
    geom_text(aes(x=.25,y=1,label=field2),size=5,col=col.pal[i],hjust=1)+
    geom_text(aes(x=9.125,y=1,label=field_no),size=4.5,col="gray30")+
    expand_limits(x=c(-1,10))+
    theme_minimal()+
    theme(legend.position = "none",
          panel.background = element_rect(color="#E8E8E8",fill="#E8E8E8"),
          plot.background = element_rect(color="#E8E8E8",fill="#E8E8E8"),
          axis.title=element_blank(),
          axis.text.x = element_text(size=11,face="bold",color="black"),
          axis.text.y=element_blank())
  nam=STEM.field[i]
  assign(nam,ex.plot)
}

((`Biological/Biomedical`+scale_x_discrete(position="top")+theme(axis.text.x=element_text(vjust=3)))/
    (`Computer Science`+theme(axis.text.x=element_blank()))/
    (Engineering+theme(axis.text.x=element_blank()))/
    (`Mathematics/Statistics`+theme(axis.text.x=element_blank()))/
    `Physical Science`)+plot_annotation(
      title="Females Receiving STEM Degrees, 2021",
      subtitle="Race Distribution by Discipline",
      caption="Source: National Center for Education Statistics"
    ) & 
  theme(plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=18,hjust=.5),
        plot.caption=element_text(size=12,hjust=.5),
        plot.margin = margin(.5,0,.5,0, "cm"),
        plot.background = element_rect(color="#E8E8E8",fill="#E8E8E8"))

ggsave("stem.race.by.disc_010224.png",dpi=320,height=8,width=12)

#####################################################################################

# circle bar chart

rbind(educ_pct,
      data.frame(
        field=rep(STEM.field,2),
        race=rep(c("A","z"),each=5),
        pct=NA,
        race2=rep(c("A","z"),each=5)
      )
) %>%
  arrange(field,race2) %>%
  mutate(order=1:50,
         race3=ifelse(is.na(pct),NA,paste0(race2," - ",format(pct,digits=3),"%")),
         angle=case_when(
           field=="Biological/Biomedical" ~ 90-order*360/52,
           field=="Computer Science" ~ 90-order*360/52,
           field=="Engineering" ~ 90-order*360/52,
           field=="Mathematics/Statistics" ~ -95-order*360/52,
           field=="Physical Science" ~ -95-order*360/52,
         )) %>%
  ggplot(aes(order,10*pct,fill=field)) +
  
  geom_segment(data=data.frame(
    x=c(2:9,12:19,22:29,32:39,42:49),
    xend=c(2:9,12:19,22:29,32:39,42:49),
    y=rep(0,40),
    yend=rep(775,40),
    group=c(1:40),
    field=NA
  ),aes(x=x,xend=xend,y=y,yend=yend,group=group),linetype=2)+
  
  geom_col(position="dodge") +
  scale_fill_manual(values=c("#000FFF","#177e89","#084c61","#db3a34","#e4b100")) +
  labs(x="",y="Percent",
       title="Percentage of STEM Bachelor's Degrees Awarded to Females, 2021",
       subtitle="Race within Discipline",
       caption="\nSource: National Center for Education Statistics") +
  
  geom_segment(data=data.frame(
    x=c(0,10.5,20.5,30.5,40.5),
    xend=c(0,10.5,20.5,30.5,40.5),
    y=rep(-200,5),
    yend=rep(1000,5),
    group=c(1,2,3,4,5),
    field=NA
  ),aes(x=x,xend=xend,y=y,yend=yend,group=group)
  )+
  
  geom_text(data=. %>% mutate(hjust=c(rep(0,30),rep(1,20))),
            aes(x=order,y=800,label=race3,angle=angle,hjust=hjust,col=field),size=10
  )+
  scale_color_manual(values=c("#000FFF","#177e89","#084c61","#db3a34","#e4b100")) +
  
  geom_segment(data=data.frame(
    x=c(2.5,12.5,22.5,32.5,42.5),
    xend=c(8.5,18.5,28.5,38.5,48.5),
    y=rep(-50,5),
    yend=rep(-50,5),
    group=c(1,2,3,4,5),
    field=NA
  ),aes(x=x,xend=xend,y=y,yend=yend,group=group))+
  geom_segment(data=data.frame(
    x=c(3,13,23,33,43),
    xend=c(8,18,28,38,48),
    y=rep(-325,5),
    yend=rep(-325,5),
    group=c(1,2,3,4,5),
    field=NA
  ),aes(x=x,xend=xend,y=y,yend=yend,group=group))+
  
  geomtextpath::geom_textpath(data=data.frame(
    x=c(5.5,15.5,25.5,35.5,45.5),
    y=rep(-180,5),
    group=c(1,2,3,4,5),
    field=NA,
    label=c("Biological/Biomedical\n65.8%","21.9%\nComputer Science",
            "24.2%\nEngineering","42.0%\nMathematics/Statistics","Physical Science\n44.6%")
  ),aes(x=x,y=y,label=label,group=group),size=12,col=c("#000FFF","#177e89","#084c61","#db3a34","#e4b100"))+
  geom_text(aes(x=0,y=-800,label="39.2% of\nSTEM degrees\nwere awarded to\nfemales\nin 2021"),size=15)+
  
  theme_void()+
  theme(legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=8))+
  ylim(-800,1000)+
  xlim(0,51)+
  theme(legend.position="none",
        plot.title=element_text(size=52,hjust=.5),
        plot.subtitle=element_text(size=44,hjust=.5),
        plot.caption=element_text(size=24),
        panel.background = element_rect(fill="gray90",color="gray90"),
        plot.background = element_rect(fill="gray90",color="gray90"),
        plot.margin = margin(.5,0,.5,0, "cm")
  )+
  coord_polar()

ggsave("circ.bar_010224.png",dpi=320,width=34,height=32)

































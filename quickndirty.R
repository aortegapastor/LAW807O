library (dplyr)
library(readxl)
library (ggplot2)
library (tidyr)

#Read in data
wb17 <- read.csv("~/Downloads/worldbank2017.csv")
wb <- wb17[(!is.na(wb17$emissions..kton.year.)),] %>%
  rename(COUNTRY = Country.1) %>%
  mutate(gton = emissions..kton.year./10^6)
wb_long<- wb[,c("COUNTRY","gton","Project.Name")]%>%
  slice(rep(row_number(),2051-2017))
TIME<- 2017:2050

wb_long<- dplyr::mutate(wb_long, TIME = rep(TIME, each = 254)) %>%
  rename (level = Project.Name, emit = gton)

vivid <- read_excel("BRI_data extract for Stanford_Vivid.xlsx", 
                    sheet = "GDP_BRI_TT_by_country")
vivid$COUNTRY<-factor(vivid$COUNTRY)
vivid_long <- gather (vivid, level, emit,
                      c("LOW_Total_CO2_Power","BASE_Total_CO2_Power"), 
                      factor_key=T)
vivid_long<- vivid_long[,c("COUNTRY","emit","level","TIME")]
vivid_side<- vivid[,c("COUNTRY","TIME","BASE_Total_CO2_Power","LOW_Total_CO2_Power")]

merge <- rbind (vivid_long,wb_long)
merge_side <-merge(vivid_side, wb_long, by = c("COUNTRY","TIME"))
  
sort(unique(merge$COUNTRY))

indo <- subset (merge, COUNTRY == "Indonesia")

#plot
pdf (file = "per_cty.pdf")
for (i in unique(merge$COUNTRY)){
  print(
    ggplot (data = merge[merge$COUNTRY %in% i,], aes (x = TIME, y = emit, fill = level)) +
    geom_area(position=position_jitter(w=0, h=5), alpha=0.6) +
#    theme(legend.position = "none")+
    theme_classic() +
    ggtitle(i) + 
  scale_fill_viridis_d()
    )
}
dev.off()

#prop
merge_side <- mutate(merge_side,
  low100 = LOW_Total_CO2_Power/LOW_Total_CO2_Power*100,
  base100 = BASE_Total_CO2_Power/BASE_Total_CO2_Power*100,
  emit.low = emit/LOW_Total_CO2_Power*100,
  emit.base = emit/BASE_Total_CO2_Power*100
)
merge.prop.long <- gather (merge_side, source, low.prop,
                           c("low100","emit.low"),  
                           factor_key=T)
merge.prop.long<- gather (merge.prop.long, source.base, base.prop, c("base100","emit.base"), factor_key=T)

indo.prop <- subset (merge.prop.long, COUNTRY == "Indonesia")

ggplot (data = indo.prop, aes (fill = source, y = base.prop, x = TIME)) +
  geom_area(position=position_jitter(w=0, h=5), alpha=0.6) +
  #    theme(legend.position = "none")+
  theme_classic() +
  ggtitle(indo) + 
  scale_fill_viridis_d()

#2017 only
colnames (wb)
wb$code <- substring (wb$Project.Name, 1, 5)
wb <- wb[with(wb,order(COUNTRY, gton)),]
ggplot (data = wb, aes (y = gton, x = code, fill = COUNTRY)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()+
  theme(legend.position = "none")
  

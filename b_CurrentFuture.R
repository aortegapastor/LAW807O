#Load libraries----
library (tidyverse)
library (dplyr)
library (ggplot2)
library (readxl)
library (cowplot)
library (gridExtra)
library (grid)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#Load data----
df <- read.csv("wb17_em_ut.csv")
colnames(df)
summary (df$Technology)
df<- df %>% filter(Technology != "Heat") %>% filter(Technology != "Steam")#removed Heat and Steam plants because fuel inputs are not stated
df$h<- 8760
summary (df$EmUnits)

unit <- df %>% filter (EmUnits == "kg CO2 per mmBtu") %>% #convert units 1 MMBTU = 0.293071 MWh
  mutate(EmiF = EmiF/0.293071)
unit1 <- df %>% 
  filter(is.na(EmUnits) | EmUnits != "kg CO2 per mmBtu") %>%
  bind_rows(unit) %>%
  mutate (GtC = EmiF * Capacity * h * mean_ut * 10^(-12)) %>%
  mutate (generation.TW = Capacity * h * mean_ut * 10^(-6))
summary (unit1$GtC)
summary (unit1$generation.TW)

write.csv(unit1, "wb17_emit.csv")

vivid <- read_excel("BRI_data extract for Stanford_Vivid.xlsx", 
                    sheet = "GDP_BRI_TT_by_country")
vivid$COUNTRY<-factor(vivid$COUNTRY)
vivid$HIGH_Total_CO2_Power <- vivid$HIGH_Total_CO2_Power_CO + vivid$HIGH_Total_CO2_Power_G #dataset missing summed High co2 power emissions
vivid_long <- gather (vivid, level, emit,
                      c("LOW_Total_CO2_Power","BASE_Total_CO2_Power", "HIGH_Total_CO2_Power"), 
                      factor_key=T) 
vivid_long<- vivid_long[,c("COUNTRY","emit","level","TIME")]
vivid_long <- rename (vivid_long, Country = "COUNTRY", emitGt = "emit", Time = "TIME")

write.csv (vivid_long, "vivid_long.csv")

#Current Emissions----
unit1$code <- substring (unit1$Project.Name, 1, 5)
indo <- subset (unit1, Country == "Indonesia")

wb17_Gt<-unit1%>%
  ggplot (aes (y = GtC, x = fct_reorder2(code,GtC, Country), fill = Country)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_classic()+
    theme(legend.position="bottom") +
    labs (x = "Projects", y = "Carbon Emissions (Gt)") +
    ggtitle (" World Bank 2017 Electricity Generation Projects")
legend <- get_legend(wb17_Gt)

wb17_MW<-unit1%>%
  ggplot (aes (y = generation.TW, x = fct_reorder2(code,GtC, Country), fill = Country)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_classic()+
    theme(legend.position="none") +
    labs (x = "Projects", y = "Generation (TW)")

indo17_Gt<-indo %>%
  ggplot (aes (y = GtC, x = fct_reorder(code, GtC))) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_classic()+
    labs (x = "Projects", y = "Carbon Emissions (Gt)") + 
    ggtitle ("Case Study: Indonesia")

indo17_MW<-indo %>%
  ggplot (aes (y = generation.TW, x = fct_reorder(code, GtC))) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_classic()+
    labs (x = "Projects", y = "Generation (TW)")

pdf("CurrentEmissions.pdf") 
wb17_Gt+theme(legend.position = "none")
grid.newpage()
grid.draw(legend)
wb17_MW
indo17_Gt
indo17_MW
dev.off()

#Future Emissions----
#Business as Usual
wb17_long<- unit1[,c("Country","GtC","code")]%>%
  slice(rep(row_number(),2051-2017))
Time<- 2017:2050
wb17_long<- dplyr::mutate(wb17_long, Time = rep(Time, each = nrow(unit1))) %>%
  rename (level = code, emitGt = GtC)
merge <- rbind (vivid_long,wb17_long)
write.csv(merge, "BAU.csv")
indo_bau <- subset (merge, Country == "Indonesia")

pdf (file = "FutureEmissions.pdf")
for (i in unique(merge$Country)){
  print(
    ggplot (data = merge[merge$Country %in% i,], aes (x = Time, y = emitGt, fill = level)) +
      geom_area(position=position_jitter(w=0, h=5), alpha=0.6) +
      theme_classic() +
      ggtitle(i) + 
      scale_fill_viridis_d()
  )
}
dev.off()
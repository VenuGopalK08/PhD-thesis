#_______________________________________________________________________________________
############ Uploading the data files-----##########################

library(scales)
library(velociraptr)
library(fossil)
library(countrycode)
library(readxl)
library(dplyr)
library (vegan)
library(stats)

setwd("D:/Venu Thesis/PhD thesis/Thesis/Chapter 2")
op<-par(no.readonly = TRUE) #save default plotting parameters
final.data <- read.csv("Data1.csv")

glimpse(final.data)


summary_table <- final.data %>%
  filter(!is.na(Epoch_grad)) %>%   # remove NA Epoch_grad rows
  group_by(Epoch, Epoch_grad, Province, phylum) %>%
  summarise(
    genus.UniqueCount = n_distinct(genus),
    genus.TotalCount = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Epoch = factor(
      Epoch,
      levels = c("Oligocene", "Miocene")
    ),
    Epoch_grad = factor(
      Epoch_grad,
      levels = c(
        "Early Oligocene",
        "Late Oligocene",
        "Early Miocene",
        "Middle Miocene",
        "Late Miocene"
      )
    ),
    Province = factor(
      Province,
      levels = c(
        "EAAP",
        "MIP",
        "Paratethys",
        "WIP"
      )
    )
  ) %>%
  arrange(phylum, Epoch, Epoch_grad, Province) %>%
  mutate(
    Epoch = as.character(Epoch),
    Epoch_grad = as.character(Epoch_grad),
    Province = as.character(Province)
  )

print(summary_table)


summary_table <- summary_table %>%
  mutate(
    phylum = factor(
      phylum,
      levels = c(
        "Mollusca",
        "Echinodermata",
        "Cnidaria",
        "Foraminifera"
      )
    )
  ) %>%
  arrange(phylum, Epoch, Epoch_grad, Province)


data.div <- summary_table

############# Summary statistics #############################################################

final.data=data.frame(final.data)

summary(final.data)
phylum_list=(unique(final.data$phylum))
class_list=(unique(final.data$class))
order_list=(unique(final.data$order))
family_list=(unique(final.data$family))
genus_list=(unique(final.data$genus))
species_list=(unique(final.data$species))
reference=(length(unique(final.data$primary_reference)))

####number of occurrences per category
library(dplyr)

occ_ref <- aggregate(final.data$primary_reference ~ Province , data = final.data, FUN = function(x) c(UniqueCount = length(unique(x)), TotalCount = length(x)))

table_all=table(final.data$Province, final.data$Epoch, final.data$phylum) # phyla per province
table_all=as.data.frame(table_all)
table_all=rename(table_all,Province=Var1,Epoch=Var2,Phylum=Var3, total_frequency=Freq)

oligocene <- subset(final.data, final.data$Epoch =="Oligocene")
miocene <- subset(final.data, final.data$Epoch =="Miocene")

table_oligocene=table(oligocene$phylum,oligocene$Epoch,oligocene$Province)
table_oligocene=as.data.frame(table_oligocene)
table_oligocene=rename(table_oligocene,Province=Var3,Epoch=Var2,Phylum=Var1, total_frequency=Freq)

table_miocene=table(miocene$phylum,miocene$Epoch,miocene$Province)
table_miocene=as.data.frame(table_miocene)
table_miocene=rename(table_miocene,Province=Var3,Epoch=Var2,Phylum=Var1, total_frequency=Freq)


#########Plotting Figure 2#########
library(ggmap)
library(maps)
library(ggplot2)
library(dplyr)

oligocene <- subset(final.data, final.data$Epoch =="Oligocene")
miocene <- subset(final.data, final.data$Epoch =="Miocene")

world_map <- map_data("world")

# Create a vector for color mapping
province_colors <- c( "Paratethys" = "purple", "MIP" = "orange", "EAAP" = "green4", "WIP" = "dodgerblue")
oligocene$Province <- factor(oligocene$Province, levels = c("Paratethys", "MIP", "EAAP", "WIP"))
miocene$Province <- factor(miocene$Province, levels = c("Paratethys", "MIP", "EAAP", "WIP"))

# Location plot with province
Fig.2 <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#F5F5F5", color = "grey") +
  geom_point(data = oligocene, aes(x = lng, y = lat, color = Province), size = 1.5, pch = 16) +
  geom_point(data = miocene, aes(x = lng, y = lat, color = Province), size = 1.5, pch = 2) +
  geom_path(data = world_map, aes(x = long, y = lat, group = group), color = "grey") +  # Add boundaries
  coord_cartesian(xlim = c(-18, 80), ylim = c(-18, 70)) +  
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_color_manual(name = "", values = province_colors) +  
  labs(title = "", fill = "Province") +  
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(size = 0.5),
        axis.line = element_line(color = "black"),  # Set axis line color
        panel.border = element_rect(color = "black", fill = NA),  
        panel.background = element_rect(fill = "white")) +  
  scale_x_continuous(breaks = seq(-20, 90, by = 20), minor_breaks = NULL) +  
  scale_y_continuous(breaks = seq(-20, 60, by = 20), minor_breaks = NULL) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 3))) +
  geom_segment(aes(x = -10, xend = 32, y = -20.5, yend = -20.5), color = "#00bfc4", linewidth = 6) +
  geom_segment(aes(x = 37, xend = 80, y = -20.5, yend = -20.5), color = "#f9766e", linewidth = 6) +
  geom_text(aes(x = 11, y = -23.5, label = "Western provinces"), color = "black", size = 4, vjust = -1) +
  geom_text(aes(x = 60, y = -23.5, label = "Eastern provinces"), color = "black", size = 4, vjust = -1)

windows()
Fig.2


#########Diversity through time (Figure 3)#########
fig4<-read.csv("Data2.csv")
### Mollusca###

Mol_div<- subset(fig4, fig4$phylum == "Mollusca")
### for Scale###
maximum = max(c(Mol_div$genus.TotalCount))
###ordering the Age from oldest to youngest###
custom_order <- c("Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene", "Late Miocene")
Mol_div$Epoch_grad <- factor(Mol_div$Epoch_grad, levels = custom_order)
Mol_div<- Mol_div[order(Mol_div$Epoch_grad), ]
#plot
p1<- ggplot(data=Mol_div, aes(x=genus.UniqueCount, y=Epoch_grad, group=Province, color=Province))+ 
  scale_x_log10(limits = c(1, 1000))+
  geom_point(aes(size=genus.TotalCount, ))+
  scale_size_area(limits=c(1,maximum),
                  breaks=c(100, 500, 1000, 1500),
                  labels=c(expression("< 100"), 500, 1000,expression("> 1500")))+
  geom_line(orientation="y", size = .6, alpha=1, linetype="dotted") +
  geom_hline(yintercept = "Early Miocene", size=8, alpha= 0.1)+
  labs(y = "", x = "Genus richness", size= "Genus Occurrence") +
  ggtitle("Mollusca") +
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_manual(values = 
                       c("MIP"="orange",
                         "WIP"="blue",
                         "EAAP"="green4",
                         "Paratethys"="purple"))+
  theme_classic()+
  theme( plot.title = element_text(face = "bold", hjust = 0.5))+ 
  theme( axis.text.y = element_text(color = "white"))

p1

###Echinoid ##
Echi_div<-subset(fig4, fig4$phylum == "Echinodermata")
### for Scale###
maximum = max(c(Echi_div$genus.TotalCount))
###ordering the Age from oldest to youngest###
custom_order <- c("Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene", "Late Miocene")
Echi_div$Epoch_grad <- factor(Echi_div$Epoch_grad, levels = custom_order)
Echi_div <- Echi_div[order(Echi_div$Epoch_grad), ]
#plot
p2<- ggplot(data=Echi_div, aes(x=genus.UniqueCount, y=Epoch_grad, group=Province, color=Province))+ 
  scale_x_log10(limits = c(1, 1000))+
  geom_point(aes(size=genus.TotalCount, ))+
  scale_size_area(limits=c(1,maximum),
                  breaks=c(100, 500, 1000, 1500),
                  labels=c(expression("< 100"), 500, 1000,expression("> 1500")))+
  geom_line(orientation="y", size = .6, alpha=1, linetype="dotted") +
  geom_hline(yintercept = "Early Miocene", size=8, alpha= 0.1)+
  labs(y = "", x = "", size= "Genus Occurrence") +
  ggtitle("Echinodermata") +
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_manual(values  = 
                       c("MIP"="orange",
                         "WIP"="blue",
                         "EAAP"="green4",
                         "Paratethys"="purple"))+
  theme_classic()+
  theme(legend.position = "none",plot.title = element_text(face = "bold", hjust = 0.5))+ 
  theme( axis.text.y = element_text(color = "white"))
p2


### Cnidaria ###
Cnid_div<-subset(fig4, fig4$phylum == "Cnidaria")
### for Scale###
maximum = max(c(Cnid_div$genus.TotalCount))
###ordering the Age from oldest to youngest###
custom_order <- c("Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene", "Late Miocene")
Cnid_div$Epoch_grad <- factor(Cnid_div$Epoch_grad, levels = custom_order)
Cnid_div <- Cnid_div[order(Cnid_div$Epoch_grad), ]
#plot
p3<- ggplot(data=Cnid_div, aes(x=genus.UniqueCount, y=Epoch_grad, group=Province, color=Province))+ 
  scale_x_log10(limits = c(1, 1000))+
  geom_point(aes(size=genus.TotalCount, ))+
  scale_size_area(limits=c(1,maximum),
                  breaks=c(100, 500, 1000, 1500),
                  labels=c(expression("< 100"), 500, 1000,expression("> 1500")))+
  geom_line(orientation="y", size = .6, alpha=1, linetype="dotted") +
  geom_hline(yintercept = "Early Miocene", size=8, alpha= 0.1)+
  labs(y = "", x = "", size= "Genus Occurrence") +
  ggtitle("Cnidaria") +
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_manual(values  = 
                       c("MIP"="orange",
                         "WIP"="blue",
                         "EAAP"="green4",
                         "Paratethys"="purple"))+
  theme_classic()+
  theme(legend.position = "none",plot.title = element_text(face = "bold", hjust = 0.5))
p3

### Foraminifera ###
Foram_div<-subset(fig4, fig4$phylum == "Foraminifera")
### for Scale###
maximum = max(c(Foram_div$genus.TotalCount))
###ordering the Age from oldest to youngest###
custom_order <- c("Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene", "Late Miocene")
Foram_div$Epoch_grad <- factor(Foram_div$Epoch_grad, levels = custom_order)
Foram_div <- Foram_div[order(Foram_div$Epoch_grad), ]
#plot
p4<- ggplot(data=Foram_div, aes(x=genus.UniqueCount, y=Epoch_grad, group=Province, color=Province))+ 
  scale_x_log10(limits = c(1, 1000))+
  geom_point(aes(size=genus.TotalCount, ))+
  scale_size_area(limits=c(1,maximum),
                  breaks=c(100, 500, 1000, 1500),
                  labels=c(expression("< 100"), 500, 1000,expression("> 1500")))+
  geom_line(orientation="y", size = .6, alpha=1, linetype="dotted") +
  geom_hline(yintercept = "Early Miocene", size=8, alpha= 0.1)+
  labs(y = "", x = "Genus richness", size= "Genus Occurrence") +
  ggtitle("Foraminifera") +
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_manual(values  = 
                       c("MIP"="orange",
                         "WIP"="blue",
                         "EAAP"="green4",
                         "Paratethys"="purple"))+
  theme_classic()+
  theme(legend.position = "none",plot.title = element_text(face = "bold", hjust = 0.5))
p4

library(ggpubr)  
Figure_3= ggarrange(p3, p2,p4, p1, ncol =2 , nrow = 2, common.legend = TRUE, legend="bottom")
ggsave("Figure_3.jpeg", plot = Figure_3, dpi = 600)

###########Similarity of groups between provinces ####################
library(dplyr)
library(tidyverse)
## counting groups that are shared across GLB##
oligocene_MIP <- subset(oligocene, oligocene$Province.2 %in% c('west Gomp'))
miocene_MIP <- subset(miocene, miocene$Province.2 %in% c('west Gomp'))

oligocene_nonMIP <- subset(oligocene, oligocene$Province.2 %in% c('east Gomp'))
miocene_nonMIP <- subset(miocene, miocene$Province.2 %in% c('east Gomp'))

common_genera_oligocene <- intersect(oligocene_MIP$genus, oligocene_nonMIP$genus)
common_genera_oligocene=as.data.frame(common_genera_oligocene)

common_genera_miocene <- intersect(miocene_MIP$genus, miocene_nonMIP$genus)
common_genera_miocene=as.data.frame(common_genera_miocene)

complete_common_oligocene=filter(oligocene,oligocene$genus %in% common_genera_oligocene$common_genera_oligocene)
complete_common_miocene=filter(miocene,miocene$genus %in% common_genera_miocene$common_genera_miocene)

complete_common_MIPandnonMIP=rbind.data.frame(complete_common_oligocene,complete_common_miocene)

table2=table(complete_common_MIPandnonMIP$Province, complete_common_MIPandnonMIP$phylum, complete_common_MIPandnonMIP$Epoch)
table2=as.data.frame(table2)
table2=table2 %>% rename( "Province"= "Var1",  "Phylum"="Var2" ,  "Epoch"= "Var3","Freq_common" ="Freq" )
table_all=merge.data.frame(table_all,table2)
Freq_unique= ((table_all$total_frequency)-(table_all$Freq_common))
table_all$relative_contribution_common = (((table_all$Freq_common)/(table_all$total_frequency))*100)
table_all$relative_contribution_unique = (100-(table_all$relative_contribution_common))

###Epoch_grad comparison of similarity (Fig 4)#####
table_all_epoch_grad=table(final.data$Province, final.data$phylum, final.data$Epoch_grad) # phyla per province
table_all_epoch_grad=as.data.frame(table_all_epoch_grad)
table_all_epoch_grad=rename(table_all_epoch_grad,Province=Var1,Phylum=Var2,Epoch_grad=Var3, total_frequency=Freq)

table_MIPandnonMIP_epoch_grad=table(complete_common_MIPandnonMIP$Province, complete_common_MIPandnonMIP$phylum, complete_common_MIPandnonMIP$Epoch_grad)
table_MIPandnonMIP_epoch_grad=as.data.frame(table_MIPandnonMIP_epoch_grad)
table_MIPandnonMIP_epoch_grad=table_MIPandnonMIP_epoch_grad %>% rename( "Province"= "Var1",  "Phylum"="Var2" ,  "Epoch_grad"= "Var3","Freq_common" ="Freq" )
table_all_epoch_grad=merge.data.frame(table_all_epoch_grad,table_MIPandnonMIP_epoch_grad)

table_all_epoch_grad$relative_contribution_common = (((table_all_epoch_grad$Freq_common)/(table_all_epoch_grad$total_frequency))*100)
table_all_epoch_grad$relative_contribution_unique = (100-(table_all_epoch_grad$relative_contribution_common))
table_all_epoch_grad[is.na(table_all_epoch_grad)] = 0

library(ggplot2)
library(dplyr)
table_all_epoch_grad$Epoch_grad <- factor(table_all_epoch_grad$Epoch_grad,                                    # Change ordering manually
                                          levels = c("Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene", "Late Miocene"))
my_labels <- c('EO', 'LO', 'EM', 'MM','LM')

F7.1a=ggplot(subset(table_all_epoch_grad, Province %in% "Paratethys"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(color="purple")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("Paratethys") +
  ylim(0, 100)

F7.2a=ggplot(subset(table_all_epoch_grad, Province %in% "MIP"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(color="orange")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("MIP") +
  ylim(0, 100)

F7.3a=ggplot(subset(table_all_epoch_grad, Province %in% "EAAP"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(color="green4")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("EAAP") +
  ylim(0, 100)

F7.4a=ggplot(subset(table_all_epoch_grad, Province %in% "WIP"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(color="dodgerblue")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative prportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("WIP") +
  ylim(0, 100)


table_all_epoch_grad$Epoch_grad <- factor(table_all_epoch_grad$Epoch_grad,                                    # Change ordering manually
                                          levels = c("Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene", "Late Miocene"))
my_labels <- c('EO', 'LO', 'EM', 'MM','LM')

F7.1b=ggplot(subset(table_all_epoch_grad, Phylum %in% "Cnidaria"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(fill="#FF6A6A")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("Cnidaria") +
  ylim(0, 100)

F7.2b=ggplot(subset(table_all_epoch_grad, Phylum %in% "Echinodermata"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(fill="chartreuse4")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("Echinodermata") +
  ylim(0, 100)

F7.3b=ggplot(subset(table_all_epoch_grad, Phylum %in% "Foraminifera"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(fill="cyan2")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("Foraminifera") +
  ylim(0, 100)

F7.4b=ggplot(subset(table_all_epoch_grad, Phylum %in% "Mollusca"), aes(x=Epoch_grad, y=relative_contribution_common))+ 
  geom_boxplot(fill="darkorchid1")+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y = "Relative proportion of shared genera", x = " ")+
  scale_x_discrete(labels=my_labels)+
  ggtitle("Mollusca") +
  ylim(0, 100)

library(gridExtra)
Figure_4= grid.arrange(F7.1a,F7.2a,F7.3a,F7.4a,F7.1b,F7.3b,F7.2b,F7.4b, nrow = 2)
pairwise.wilcox.test(table_all_epoch_grad$relative_contribution_common, table_all_epoch_grad$Province, p.adjust.method = "bonferroni")
pairwise.wilcox.test(table_all_epoch_grad$relative_contribution_common, table_all_epoch_grad$Phylum, p.adjust.method = "bonferroni")
ggsave("Figure_4.jpeg", plot = Figure_4, dpi = 600)

###################Plotting Non-shared genera across closure (Fig 7)###########
complete_common_MIPandnonMIP 
'%notin%' <- Negate("%in%")
complete_non_shared_MIPandnonMIP=filter(final.data,final.data$genus %notin% complete_common_MIPandnonMIP$genus)
complete_non_shared_MIPandnonMIP=subset(complete_non_shared_MIPandnonMIP, complete_non_shared_MIPandnonMIP$Province.2!="NA")
complete_non_shared_MIPandnonMIP$province_type=if_else(complete_non_shared_MIPandnonMIP$Province.2 == "east Gomp","East","West")

table_nonshared_taxa=table(complete_non_shared_MIPandnonMIP$family, complete_non_shared_MIPandnonMIP$province_type)
table_nonshared_taxa=as.data.frame(table_nonshared_taxa)
table_nonshared_taxa=table_nonshared_taxa %>% rename( "family"= "Var1",  "province"="Var2")
table_nonshared_taxa$freq_type=c("nonshared")

table_nonshared_taxa=subset(table_nonshared_taxa, table_nonshared_taxa$family!=c("NO_FAMILY_SPECIFIED"))

taxa=final.data[4:7]
taxa=unique(taxa)
table_nonshared_taxa=table_nonshared_taxa[-which(table_nonshared_taxa$Freq<5),]

library(dplyr)
table_nonshared_taxa=left_join(table_nonshared_taxa, taxa, by="family")

### Finding groups that are exclusively found in the east or west (pie chart)##
east = subset(table_nonshared_taxa,table_nonshared_taxa$province %in% c("East"))
west = subset(table_nonshared_taxa,table_nonshared_taxa$province %in% c("West")) 
exclusive_east = anti_join(east, west, by="family")
exclusive_west = anti_join(west, east, by="family")
exclusive_total=rbind.data.frame(exclusive_east,exclusive_west)
#table_exclusive=table(exclusive_total$phylum,exclusive_total$province)
#table_exclusive_family=as.data.frame(table_exclusive)
#table_exclusive_family=table_exclusive_family %>% rename( "phylum"= "Var1",  "province"="Var2", "ex_fam_freq"="Freq")

table_exclusive=table(exclusive_total$phylum,exclusive_total$class, exclusive_total$province)
table_exclusive_family=as.data.frame(table_exclusive)
table_exclusive_family=table_exclusive_family %>% rename("phylum"= "Var1", "class"= "Var2", "province"="Var3", "ex_fam_freq"="Freq")

biv_p=ggplot(subset(table_exclusive_family, class %in% "Bivalvia"), aes(x="", y=ex_fam_freq, fill=province)) +
  geom_bar(stat="identity", width=1) +
  labs(fill = " ") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

gas_p=ggplot(subset(table_exclusive_family, class %in% "Gastropoda"), aes(x="", y=ex_fam_freq, fill=province)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none")

cnid_p=ggplot(subset(table_exclusive_family, phylum %in% "Cnidaria"), aes(x="", y=ex_fam_freq, fill=province)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  theme(legend.position = "none")

echin_p=ggplot(subset(table_exclusive_family, phylum %in% "Echinodermata"), aes(x="", y=ex_fam_freq, fill=province)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none")

foram_p=ggplot(subset(table_exclusive_family, phylum %in% "Foraminifera"), aes(x="", y=ex_fam_freq, fill=province)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none")

#####plotting specific families###
##Plotting Cnidaria##
table_nonshared_taxa_cnidaria=subset(table_nonshared_taxa,table_nonshared_taxa$phylum %in% c("Cnidaria"))
Fig_9a_c= ggplot(table_nonshared_taxa_cnidaria, aes(x = reorder(family, Freq), y = Freq, fill = province))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Cnidaria")

##Plotting Echinodermata##
table_nonshared_taxa_echino=subset(table_nonshared_taxa,table_nonshared_taxa$phylum %in% c("Echinodermata"))
Fig_9a_e= ggplot(table_nonshared_taxa_echino, aes(x = reorder(family, Freq), y = Freq, fill = province))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Echinodermata")

##Plotting Foraminifera## 
table_nonshared_taxa_foram=subset(table_nonshared_taxa,table_nonshared_taxa$phylum %in% c("Foraminifera"))
Fig_9a_f= ggplot(table_nonshared_taxa_foram, aes(x = reorder(family, Freq), y = Freq, fill = province))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Foraminifera")

##Plotting Bivalvia## 
table_nonshared_taxa_bivalve=subset(table_nonshared_taxa,table_nonshared_taxa$class %in% c("Bivalvia"))
Fig_9a_bi= ggplot(table_nonshared_taxa_bivalve, aes(x = reorder(family, Freq), y = Freq, fill = province))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Bivalvia")

##Plotting Gastropoda## 
table_nonshared_taxa_gastropod=subset(table_nonshared_taxa,table_nonshared_taxa$class %in% c("Gastropoda"))
Fig_9a_ga= ggplot(table_nonshared_taxa_gastropod, aes(x = reorder(family, Freq), y = Freq, fill = province))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Gastropoda")


Fig_9a_bi=Fig_9a_bi+theme(legend.position = "none")+ theme(axis.title.x=element_blank(),
                                                           axis.text.x=element_blank(),
                                                           axis.ticks.x=element_blank())
Fig_9a_ga=Fig_9a_ga+theme(legend.position = "none")+ labs(y = "Contribution", x = "") + theme(axis.title.x=element_blank(),
                                                                                              axis.text.x=element_blank(),
                                                                                              axis.ticks.x=element_blank())
Fig_9a_c=Fig_9a_c+theme(legend.position = "none")+ labs(y = "Contribution", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                            axis.text.x=element_blank(),
                                                                                            axis.ticks.x=element_blank())
Fig_9a_e=Fig_9a_e+theme(legend.position = "none")+ labs(y = "Contribution", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                            axis.text.x=element_blank(),
                                                                                            axis.ticks.x=element_blank())
Fig_9a_f=Fig_9a_f+theme(legend.position = "none")+ labs(y = "Contribution", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                            axis.text.x=element_blank(),
                                                                                            axis.ticks.x=element_blank())

library(cowplot)
biv_11 = ggdraw() +
  draw_plot(Fig_9a_bi) +
  draw_plot(biv_p, x = 0.7, y = .001, width = .3, height = .3)

gas_11 = ggdraw() +
  draw_plot(Fig_9a_ga) +
  draw_plot(gas_p, x = 0.7, y = .001, width = .3, height = .3)

cnid_11 = ggdraw() +
  draw_plot(Fig_9a_c) +
  draw_plot(cnid_p, x = 0.7, y = .001, width = .3, height = .3)

echin_11 = ggdraw() +
  draw_plot(Fig_9a_e) +
  draw_plot(echin_p, x = 0.7, y = .001, width = .3, height = .3)

foram_11 = ggdraw() +
  draw_plot(Fig_9a_f) +
  draw_plot(foram_p, x = 0.7, y = .001, width = .3, height = .3)


legend_11 <- get_legend(biv_p + theme(legend.position="bottom"))

library(ggpubr)  
Figure_11= ggarrange(biv_11, gas_11, cnid_11, echin_11, foram_11, nrow=1)
Figure_7=plot_grid(Figure_11, legend_11, ncol = 1, rel_heights = c(1, .05))

ggsave("Figure_7.jpeg", plot = Figure_7,width = 14, height = 8, dpi = 600)

###########Taxonomic identity of groups in shared and nonshared category by Epoch.grad ############## 
table_all_taxa_Epoch_grad=table(final.data$Epoch_grad,final.data$family)
table_all_taxa_Epoch_grad=as.data.frame(table_all_taxa_Epoch_grad)
table_all_taxa_Epoch_grad=table_all_taxa_Epoch_grad %>% rename( "epoch_grad"= "Var1",  "family"="Var2" )
table_all_taxa_Epoch_grad$freq_type=c("Total")

table_common_taxa_Epoch_grad=table(complete_common_MIPandnonMIP$Epoch_grad, complete_common_MIPandnonMIP$family)
table_common_taxa_Epoch_grad=as.data.frame(table_common_taxa_Epoch_grad)
table_common_taxa_Epoch_grad=table_common_taxa_Epoch_grad %>% rename( "epoch_grad"= "Var1",  "family"="Var2")
table_common_taxa_Epoch_grad$freq_type=c("Common")

table_nonshared_taxa_epoch_grad=table(complete_non_shared_MIPandnonMIP$family, complete_non_shared_MIPandnonMIP$Epoch_grad)
table_nonshared_taxa_epoch_grad=as.data.frame(table_nonshared_taxa_epoch_grad)
table_nonshared_taxa_epoch_grad=table_nonshared_taxa_epoch_grad %>% rename( "family"= "Var1",  "epoch_grad"="Var2")
table_nonshared_taxa_epoch_grad$freq_type=c("nonshared")


table_all_taxa_Epoch_grad=rbind(table_all_taxa_Epoch_grad,table_common_taxa_Epoch_grad,table_nonshared_taxa_epoch_grad)
table_all_taxa_Epoch_grad=subset(table_all_taxa_Epoch_grad, table_all_taxa_Epoch_grad$family!=c("NO_FAMILY_SPECIFIED"))
table_all_taxa_Epoch_grad=table_all_taxa_Epoch_grad[-which(table_all_taxa_Epoch_grad$Freq<5),]

taxa=final.data[4:7]
taxa=unique(taxa)

library(dplyr)
table_all_taxa_Epoch_grad=left_join(table_all_taxa_Epoch_grad, taxa, by="family")
table_shared_taxa_epoch_grad = subset(table_all_taxa_Epoch_grad,table_all_taxa_Epoch_grad$freq_type %in% c("Common"))
table_nonshared_taxa_epoch_grad = subset(table_all_taxa_Epoch_grad,table_all_taxa_Epoch_grad$freq_type %in% c("nonshared"))
###########Taxonomic identity of groups in shared category by Epoch.grad############## 

##Plotting Bivalvia##
table_shared_taxa_bivalvia=subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$class %in% c("Bivalvia"))
table_shared_taxa_bivalvia$epoch_grad <- factor(table_shared_taxa_bivalvia$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))
Fig_13a_bi= ggplot(table_shared_taxa_bivalvia, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Bivalvia")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))


####Plotting Gastropoda###
table_shared_taxa_gastropod=subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$class %in% c("Gastropoda"))
table_shared_taxa_gastropod$epoch_grad <- factor(table_shared_taxa_gastropod$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_13a_ga= ggplot(table_shared_taxa_gastropod, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Gastropoda")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))


####Plotting Cnidaria###
table_shared_taxa_cnidaria=subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$phylum %in% c("Cnidaria"))
table_shared_taxa_cnidaria$epoch_grad <- factor(table_shared_taxa_cnidaria$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_13a_c= ggplot(table_shared_taxa_cnidaria, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Cnidaria")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))


##Plotting Echinodermata##
table_shared_taxa_echino=subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$phylum %in% c("Echinodermata"))
table_shared_taxa_echino$epoch_grad <- factor(table_shared_taxa_echino$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_13a_e= ggplot(table_shared_taxa_echino, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Echinodermata")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))

##Plotting Foraminifera##
table_shared_taxa_foram=subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$phylum %in% c("Foraminifera"))
table_shared_taxa_foram$epoch_grad <- factor(table_shared_taxa_foram$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_13a_f= ggplot(table_shared_taxa_foram, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Foraminifera")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))

#####Arranging all the plots with family names###
Fig_13a_bi=Fig_13a_bi+theme(legend.position = "none")+ labs(y = "", x = "Family") + theme(axis.title.x=element_blank(),
                                                                                          axis.text.x=element_blank(),
                                                                                          axis.ticks.x=element_blank())

Fig_13a_ga=Fig_13a_ga+theme(legend.position = "none") + labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                      axis.text.x=element_blank(),
                                                                                      axis.ticks.x=element_blank())
Fig_13a_c=Fig_13a_c+theme(legend.position = "none") + labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank())
Fig_13a_e=Fig_13a_e+theme(legend.position = "none")+ labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                   axis.text.x=element_blank(),
                                                                                   axis.ticks.x=element_blank())
Fig_13a_f=Fig_13a_f+ theme(legend.position = "none")+ labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank()) 

### Identifying exclusive occurrence in specific epoch_grad among shared groups (Fig.5)######
taxa_EO = subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$epoch_grad %in% c("Early Oligocene"))
taxa_LO = subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$epoch_grad %in% c("Late Oligocene"))
taxa_EM = subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$epoch_grad %in% c("Early Miocene"))
taxa_MM = subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$epoch_grad %in% c("Middle Miocene"))
taxa_LM = subset(table_shared_taxa_epoch_grad,table_shared_taxa_epoch_grad$epoch_grad %in% c("Late Miocene"))

taxa_ex_EO = subset(taxa_EO,taxa_EO$family %notin% c(taxa_LO$family,taxa_EM$family,taxa_MM$family,taxa_LM$family))
taxa_ex_LO = subset(taxa_LO,taxa_LO$family %notin% c(taxa_EO$family,taxa_EM$family,taxa_MM$family,taxa_LM$family))
taxa_ex_EM = subset(taxa_EM,taxa_EM$family %notin% c(taxa_EO$family,taxa_LO$family,taxa_MM$family,taxa_LM$family))
taxa_ex_MM = subset(taxa_MM,taxa_MM$family %notin% c(taxa_EO$family,taxa_LO$family,taxa_EM$family,taxa_LM$family))
taxa_ex_LM = subset(taxa_LM,taxa_LM$family %notin% c(taxa_EO$family,taxa_LO$family,taxa_EM$family,taxa_MM$family))
taxa_shared_ex_epoch_grad=rbind(taxa_ex_EO,taxa_ex_LO,taxa_ex_EM,taxa_ex_MM,taxa_ex_LM)

table_ex_shared_epoch_grad=table(taxa_shared_ex_epoch_grad$phylum,taxa_shared_ex_epoch_grad$class, taxa_shared_ex_epoch_grad$epoch_grad)
table_ex_shared_epoch_grad=as.data.frame(table_ex_shared_epoch_grad)
table_ex_shared_epoch_grad=table_ex_shared_epoch_grad %>% rename("phylum"= "Var1", "class"= "Var2", "epoch_grad"="Var3", "fam_freq"="Freq")
table_ex_shared_epoch_grad$epoch_grad <- factor(table_ex_shared_epoch_grad$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

### Adding the pie chart
p13_bi=ggplot(subset(table_ex_shared_epoch_grad, class %in% "Bivalvia"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  labs(fill = " ") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p13_ga=ggplot(subset(table_ex_shared_epoch_grad, class %in% "Gastropoda"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p13_cn=ggplot(subset(table_ex_shared_epoch_grad, phylum %in% "Cnidaria"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p13_ec=ggplot(subset(table_ex_shared_epoch_grad, phylum %in% "Echinodermata"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p13_fo=ggplot(subset(table_ex_shared_epoch_grad, phylum %in% "Foraminifera"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

##merging the two plots###
library(cowplot)
biv_13 = ggdraw() +
  draw_plot(Fig_13a_bi) +
  draw_plot(p13_bi, x = 0.7, y = .001, width = .3, height = .3)

gas_13 = ggdraw() +
  draw_plot(Fig_13a_ga) +
  draw_plot(p13_ga, x = 0.7, y = .001, width = .3, height = .3)

cnid_13 = ggdraw() +
  draw_plot(Fig_13a_c) +
  draw_plot(p13_cn, x = 0.7, y = .001, width = .3, height = .3)

echin_13 = ggdraw() +
  draw_plot(Fig_13a_e) +
  draw_plot(p13_ec, x = 0.7, y = .001, width = .3, height = .3)

foram_13 = ggdraw() +
  draw_plot(Fig_13a_f) +
  draw_plot(p13_fo, x = 0.7, y = .001, width = .3, height = .3)

legend_bi <- get_legend(p13_bi + theme(legend.position="bottom"))

library(ggpubr)  
Figure_13= ggarrange(biv_13, gas_13, cnid_13, echin_13, foram_13, nrow=1)
Figure_5=plot_grid(Figure_13, legend_bi, ncol = 1, rel_heights = c(1, .05))
ggsave("Figure_5.jpeg", plot = Figure_5,width = 14, height = 8, dpi = 600)
### Identifying exclusive occurrence in specific epoch_grad among nonshared groups (Fig.6)######
taxa_ns_EO = subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$epoch_grad %in% c("Early Oligocene"))
taxa_ns_LO = subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$epoch_grad %in% c("Late Oligocene"))
taxa_ns_EM = subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$epoch_grad %in% c("Early Miocene"))
taxa_ns_MM = subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$epoch_grad %in% c("Middle Miocene"))
taxa_ns_LM = subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$epoch_grad %in% c("Late Miocene"))

taxa_ns_ex_EO = subset(taxa_ns_EO,taxa_ns_EO$family %notin% c(taxa_ns_LO$family,taxa_ns_EM$family,taxa_ns_MM$family,taxa_ns_LM$family))
taxa_ns_ex_LO = subset(taxa_ns_LO,taxa_ns_LO$family %notin% c(taxa_ns_EO$family,taxa_ns_EM$family,taxa_ns_MM$family,taxa_ns_LM$family))
taxa_ns_ex_EM = subset(taxa_ns_EM,taxa_ns_EM$family %notin% c(taxa_ns_EO$family,taxa_ns_LO$family,taxa_ns_MM$family,taxa_ns_LM$family))
taxa_ns_ex_MM = subset(taxa_ns_MM,taxa_ns_MM$family %notin% c(taxa_ns_EO$family,taxa_ns_LO$family,taxa_ns_EM$family,taxa_ns_LM$family))
taxa_ns_ex_LM = subset(taxa_ns_LM,taxa_ns_LM$family %notin% c(taxa_ns_EO$family,taxa_ns_LO$family,taxa_ns_EM$family,taxa_ns_MM$family))
taxa_nonshared_ex_epoch_grad=rbind(taxa_ns_ex_EO,taxa_ns_ex_LO,taxa_ns_ex_EM,taxa_ns_ex_MM,taxa_ns_ex_LM)

table_ex_nonshared_epoch_grad=table(taxa_nonshared_ex_epoch_grad$phylum,taxa_nonshared_ex_epoch_grad$class, taxa_nonshared_ex_epoch_grad$epoch_grad)
table_ex_nonshared_epoch_grad=as.data.frame(table_ex_nonshared_epoch_grad)
table_ex_nonshared_epoch_grad=table_ex_nonshared_epoch_grad %>% rename("phylum"= "Var1", "class"= "Var2", "epoch_grad"="Var3", "fam_freq"="Freq")

table_ex_nonshared_epoch_grad$epoch_grad <- factor(table_ex_nonshared_epoch_grad$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

### Adding the pie chart (Fig.14)
p14_bi=ggplot(subset(table_ex_nonshared_epoch_grad, class %in% "Bivalvia"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  labs(fill = " ") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p14_ga=ggplot(subset(table_ex_nonshared_epoch_grad, class %in% "Gastropoda"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p14_cn=ggplot(subset(table_ex_nonshared_epoch_grad, phylum %in% "Cnidaria"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p14_ec=ggplot(subset(table_ex_nonshared_epoch_grad, phylum %in% "Echinodermata"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

p14_fo=ggplot(subset(table_ex_nonshared_epoch_grad, phylum %in% "Foraminifera"), aes(x="", y=fam_freq, fill=epoch_grad)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") 

###########Taxonomic identity of groups in nonshared category by Epoch.grad ### 


##Plotting Bivalvia##
table_nonshared_taxa_epoch_grad_bivalvia=subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$class %in% c("Bivalvia"))
table_nonshared_taxa_epoch_grad_bivalvia$epoch_grad <- factor(table_nonshared_taxa_epoch_grad_bivalvia$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))
Fig_14a_bi= ggplot(table_nonshared_taxa_epoch_grad_bivalvia, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Bivalvia")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))


####Plotting Gastropoda###
table_nonshared_taxa_epoch_grad_gastropod=subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$class %in% c("Gastropoda"))
table_nonshared_taxa_epoch_grad_gastropod$epoch_grad <- factor(table_nonshared_taxa_epoch_grad_gastropod$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_14a_ga= ggplot(table_nonshared_taxa_epoch_grad_gastropod, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Gastropoda")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))

####Plotting Cnidaria###
table_nonshared_taxa_epoch_grad_cnidaria=subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$phylum %in% c("Cnidaria"))
table_nonshared_taxa_epoch_grad_cnidaria$epoch_grad <- factor(table_nonshared_taxa_epoch_grad_cnidaria$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_14a_c= ggplot(table_nonshared_taxa_epoch_grad_cnidaria, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#DCF540','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Cnidaria")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))


##Plotting Echinodermata##
table_nonshared_taxa_epoch_grad_echino=subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$phylum %in% c("Echinodermata"))
table_nonshared_taxa_epoch_grad_echino$epoch_grad <- factor(table_nonshared_taxa_epoch_grad_echino$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_14a_e= ggplot(table_nonshared_taxa_epoch_grad_echino, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#F2E205','#FF9990'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Echinodermata")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))

##Plotting Foraminifera##
table_nonshared_taxa_epoch_grad_foram=subset(table_nonshared_taxa_epoch_grad,table_nonshared_taxa_epoch_grad$phylum %in% c("Foraminifera"))
table_nonshared_taxa_epoch_grad_foram$epoch_grad <- factor(table_nonshared_taxa_epoch_grad_foram$epoch_grad, levels = c("Late Miocene", "Middle Miocene", "Early Miocene", "Late Oligocene","Early Oligocene"))

Fig_14a_f= ggplot(table_nonshared_taxa_epoch_grad_foram, aes(x = reorder(family, Freq), y = Freq, fill = epoch_grad))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c('#CFFC90','#F2E205','#FF9990','#FF9900'))+
  coord_flip()+
  theme_classic() +
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+ 
  labs(y = "Contribution", x = "Family")+
  ggtitle("Foraminifera")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))

#####Arranging all the plots with family names###
Fig_14a_bi=Fig_14a_bi+theme(legend.position = "none")+ labs(y = "", x = "Family") + theme(axis.title.x=element_blank(),
                                                                                          axis.text.x=element_blank(),
                                                                                          axis.ticks.x=element_blank())
Fig_14a_ga=Fig_14a_ga+theme(legend.position = "none") + labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                      axis.text.x=element_blank(),
                                                                                      axis.ticks.x=element_blank())
Fig_14a_c=Fig_14a_c+theme(legend.position = "none") + labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank())
Fig_14a_e=Fig_14a_e+theme(legend.position = "none")+ labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                   axis.text.x=element_blank(),
                                                                                   axis.ticks.x=element_blank())
Fig_14a_f=Fig_14a_f+ theme(legend.position = "none")+ labs(y = " ", x = " ")+ theme(axis.title.x=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank()) 
##merging the two plots###
library(cowplot)

biv_14 = ggdraw() +
  draw_plot(Fig_14a_bi) +
  draw_plot(p14_bi, x = 0.7, y = .001, width = .3, height = .3)

gas_14 = ggdraw() +
  draw_plot(Fig_14a_ga) +
  draw_plot(p14_ga, x = 0.7, y = .001, width = .3, height = .3)

cnid_14 = ggdraw() +
  draw_plot(Fig_14a_c) +
  draw_plot(p14_cn, x = 0.7, y = .001, width = .3, height = .3)

echin_14 = ggdraw() +
  draw_plot(Fig_14a_e) +
  draw_plot(p14_ec, x = 0.7, y = .001, width = .3, height = .3)

foram_14 = ggdraw() +
  draw_plot(Fig_14a_f) +
  draw_plot(p14_fo, x = 0.7, y = .001, width = .3, height = .3)

legend_14 <- get_legend(p14_bi + theme(legend.position="bottom"))

library(ggpubr)  
Figure_14= ggarrange(biv_14, gas_14, cnid_14, echin_14, foram_14, nrow=1)
Figure_6=plot_grid(Figure_14, legend_14, ncol = 1, rel_heights = c(1, .05))
ggsave("Figure_6.jpeg", plot = Figure_6,width = 14, height = 8, dpi = 600)


########Compositional similarity (Fig. 8)####

##province composition##

###PCoA analysis ###

#PCoA of different phyllums at different provinces

Data<- read.csv("Data1.csv")
Data$lat <- round(Data$lat)  
Data$lng <- round(Data$lng)
Data$cood <- paste(Data$lat, Data$lng, sep = ",")

#PCoA plots of different Phyllum- Genus level
library(velociraptr)
library(glue)
library (vegan)


#Split the data frame in the form of Epoch_Grad column and store each subset as a separate data frame
Phyllum_list <- split(Data, Data$phylum)
# Create separate data frames for each subset
for (i in names(Phyllum_list)) {
  assign(paste0(i), Phyllum_list[[i]])
}


#Create a mapping table for location and province
mapping_table <- unique(Data[, c("cood", "Province")])
pro_order <- c( "Paratethys" = "purple", "MIP" = "orange", "EAAP" = "green4", "WIP" = "dodgerblue")
mapping_table <- mapping_table[order(match(mapping_table$Province, pro_order)), ]


# Split the data frame in the form of Epoch_Grad column and store each subset as a separate data frame
subset_list <- split(Data , Data$Epoch_grad)
# Create separate data frames for each subset
for (j in names(subset_list)) {
  assign(paste0(j), subset_list[[j]])
}

#local variable for defining group. replace this with genus if you want to plot PCoA at genus level.
x = "genus"

# Create a community matrix of genera by plates.
EO <- presenceMatrix(`Early Oligocene`, Rows = "cood", Columns = x)
LO <- presenceMatrix(`Late Oligocene`,Rows = "cood", Columns = x)
EM <- presenceMatrix(`Early Miocene`, Rows = "cood", Columns = x)
MM <- presenceMatrix(`Middle Miocene`, Rows = "cood", Columns = x)
LM <- presenceMatrix(`Late Miocene`, Rows = "cood", Columns = x)

dflist <- list(LM, MM, EM, LO, EO)
pltname <- c("Late Miocene","Middle Miocene","Early Miocene","Late Oligocene","Early Oligocene")
count <- 1

for(df in dflist){
  # Define the color palette based on unique provinces
  attr.envir_levels <- unique(mapping_table$Province)
  
  # Calculate dissimilarity matrix
  diss_matrix <- vegdist(df, method = "jaccard", binary = TRUE)
  
  # Perform PCoA
  pcoa <- cmdscale(diss_matrix, eig = TRUE)
  
  # Extract PCoA coordinates
  PC1 <- pcoa$points[, 1]
  PC2 <- pcoa$points[, 2]
  
  
  # Map cc values to unique province names
  pcoa_df <- data.frame(PC1 = PC1, PC2 = PC2, cood = rownames(pcoa$points))
  pcoa_df <- merge(pcoa_df, mapping_table, by = "cood", all.x = TRUE)
  per <- round(100*pcoa$eig/sum(pcoa$eig), digits= 1)
  labs <- c(glue("PCoA 1({per[1]}%)"), glue("PCoA 2({per[2]}%)"))
  
  # Assign colors to the colour column based on Province.2 using the named vector
  pcoa_df$colour <- pro_order[pcoa_df$Province]
  
  
  # Create a ggplot scatter plot with text annotations, legends, and polygons
  assign(paste0("alltaxa_", count), 
         ggplot(pcoa_df, aes(x = PC1, y = PC2)) +
           geom_point(aes(color = colour), size = 2) +
           labs(x = labs[1], y = labs[2]) +
           scale_color_identity(guide = "legend", labels = pro_order, name = "Province") +
           theme_classic() +
           coord_cartesian(xlim = c(-0.7, 0.7), ylim = c(-0.7, 0.7)) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
           geom_vline(xintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.5) +
           theme(legend.position = "center")
         
         
  )
  
  # Print the plot
  print(get(paste0("alltaxa_", count)))
  count <- count + 1
}


###west and east of the closure##
data_we <- Data[Data$Province.2 != 'NA', ]
mapping_table <- unique(data_we[, c("cood", "Province.2")])

# Create a vector for color mapping
pro_order2 <- c( "west Gomp" = "#00bfc4", "east Gomp" = "#f9766e")
#Convert the column to a factor with the custom order
mapping_table <- mapping_table[order(match(mapping_table$Province.2, pro_order)), ]


# Split the data frame in the form of Epoch_Grad column and store each subset as a separate data frame
subset_list <- split(data_we , data_we$Epoch_grad)
# Create separate data frames for each subset
for (j in names(subset_list)) {
  assign(paste0(j), subset_list[[j]])
}

#local variable for defining group. replace this with genus if you want to plot PCoA at genus level.
x = "genus"

# Create a community matrix of genera by plates.
EO <- presenceMatrix(`Early Oligocene`, Rows = "cood", Columns = x)
LO <- presenceMatrix(`Late Oligocene`,Rows = "cood", Columns = x)
EM <- presenceMatrix(`Early Miocene`, Rows = "cood", Columns = x)
MM <- presenceMatrix(`Middle Miocene`, Rows = "cood", Columns = x)
LM <- presenceMatrix(`Late Miocene`, Rows = "cood", Columns = x)


dflist <- list(LM, MM, EM, LO, EO)
pltname <- c("Late Miocene","Middle Miocene","Early Miocene","Late Oligocene","Early Oligocene")
count <- 1

for(df in dflist){
  # Define the color palette based on unique provinces
  attr.envir_levels <- unique(mapping_table$Province.2)
  # Remove singletons
  df<- df[rowSums(df) > 1, ]
  # Calculate dissimilarity matrix
  diss_matrix <- vegdist(df, method = "jaccard", binary = TRUE)
  
  # Perform PCoA
  pcoa <- cmdscale(diss_matrix, eig = TRUE)
  
  # Extract PCoA coordinates
  PC1 <- pcoa$points[, 1]
  PC2 <- pcoa$points[, 2]
  
  
  # Map cc values to unique province names
  pcoa_df <- data.frame(PC1 = PC1, PC2 = PC2, cood = rownames(pcoa$points))
  pcoa_df <- merge(pcoa_df, mapping_table, by = "cood", all.x = TRUE)
  per <- round(100*pcoa$eig/sum(pcoa$eig), digits= 1)
  labs <- c(glue("PCoA 1({per[1]}%)"), glue("PCoA 2({per[2]}%)"))
  
  # Assign colors to the colour column based on Province.2 using the named vector
  pcoa_df$colour <- pro_order2[pcoa_df$Province.2]
  # Create a ggplot scatter plot with text annotations, legends, and polygons
  
  assign(paste0("WEalltaxa_", count), 
         ggplot(pcoa_df, aes(x = PC1, y = PC2, color = colour)) +
           geom_point(size = 2, shape=15) +
           labs(x = labs[1], y = labs[2]) +
           scale_color_identity(guide = "legend", labels = pro_order, name = "Province") +
           theme_classic() +
           stat_ellipse(data = pcoa_df, aes(group = Province.2, color = colour), geom = "polygon", alpha = 0) +
           coord_cartesian(xlim = c(-0.7, 0.7), ylim = c(-0.7, 0.7)) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
           geom_vline(xintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.5) +
           theme(legend.position = "center")      
  )
  
  # Print the plot
  print(get(paste0("WEalltaxa_", count)))
  count <- count + 1
}

##final combined PCoA plot
windows()
Fig.8 <- grid.arrange(alltaxa_5, alltaxa_4, alltaxa_3, alltaxa_2, alltaxa_1, WEalltaxa_5, WEalltaxa_4, WEalltaxa_3, WEalltaxa_2, WEalltaxa_1, ncol=5, nrow = 2)
ggsave("Figure_8.jpeg", plot = Fig.8,width = 14, height = 8, dpi = 600)
############# END ####################################################################
##########################################

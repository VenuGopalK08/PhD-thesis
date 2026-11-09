library(dplyr)
library(palaeoverse)
library(divDyn)
library(tidyr)
library(purrr)

#set your working directory
#setwd("D:/Hotspot dynamics")

#Load the data##
fauna <- readRDS("Data/Data_S1.rds")
triton_clean <- readRDS("Data/Data_S2.rds")
pbdb <- read.csv("Data/Data_S3.csv")

bins <- time_bins()
#removinin echinodermata from analysis
fauna <- subset(fauna, fauna$phylum != "Echinodermata")
fauna$Province[fauna$Province == "IAA"] <- "Indo-Australian Archipelago"
triton_clean$Province[triton_clean$Province == "IAA"] <- "Indo-Australian Archipelago"

table(fauna$Province)

#Subsetting the Cenozoic taxa occurrence list on the basis of taxa
mollusca <- subset(fauna, fauna$phylum == "Mollusca")
foraminifera <- subset(fauna, fauna$phylum == "Foraminifera")
ben_foram <- subset(foraminifera, foraminifera$order != "Globigerinida")
plk_foram <- triton_clean
cnidaria<- subset(fauna, fauna$phylum == "Cnidaria")

bins <- time_bins()

# Set factor levels
epoch_order <- c(
  "Paleocene", "Early Eocene", "Middle Eocene", "Late Eocene",
  "Early Oligocene", "Late Oligocene",
  "Early Miocene", "Middle Miocene", "Late Miocene",
  "Pliocene", "Pleistocene"
)
province_order <- c("West Atlantic", "Western Tethys", "Northern Indian Ocean", "Indo-Australian Archipelago")


#Define province colors
province_colors <- c(
  "West Atlantic" = "orchid",
  "Western Tethys" = "orange3",
  "Northern Indian Ocean" = "dodgerblue",
  "Indo-Australian Archipelago" = "seagreen2"
)


# Create a list to store the occurrence data for each taxon group
taxa_list <- list(
  "Mollusca" = mollusca,
  "Cnidaria" = cnidaria, 
  "Benthic Foraminifera" = ben_foram,
  "Planktic Foraminifera" = plk_foram
)

# Taxa display labels
taxa_labels <- c(
  "Mollusca" = "Mollusca",
  "Cnidaria" = "Cnidaria",
  "Benthic Foraminifera" = "Benthic\nForaminifera",
  "Planktic Foraminifera" = "Planktic\nForaminifera"
)

#### Figure.S1:Plotting correlation of diversity and occurrences (first differences)#####
#Occ vs genus richness province vise
# Initialize master lists
occ_richness_all <- list()
genus_richness_all <- list()

# Loop over each taxon in the taxa list
for (taxon_name in names(taxa_list)) {
  
  data <- taxa_list[[taxon_name]] %>%
    filter(!Province %in% c("East Pacific", "Rest of world"),
           !is.na(genus), !is.na(bin_assignment))
  
  # Split by Province
  province_data <- split(data, data$Province)
  
  # Containers for this taxon
  occ_richness_taxon <- list()
  genus_richness_taxon <- list()
  
  for (province_name in names(province_data)) {
    province_df <- province_data[[province_name]]
    
    # === Occurrence richness ===
    occ_df <- province_df %>%
      group_by(bin_assignment) %>%
      summarise(occurrence_count = n(), .groups = "drop") %>%
      mutate(bin = bin_assignment) %>%
      left_join(bins[, c("bin", "mid_ma")], by = "bin")
    
    occ_richness_taxon[[province_name]] <- occ_df
    
    # === Genus richness ===
    genus_df <- province_df %>%
      group_by(bin_assignment) %>%
      summarise(genus_richness = n_distinct(genus), .groups = "drop") %>%
      mutate(bin = bin_assignment) %>%
      left_join(bins[, c("bin", "mid_ma")], by = "bin")
    
    genus_richness_taxon[[province_name]] <- genus_df
  }
  
  # Store taxon-level data
  occ_richness_all[[taxon_name]] <- occ_richness_taxon
  genus_richness_all[[taxon_name]] <- genus_richness_taxon
}

# Structure check
str(occ_richness_all)
str(genus_richness_all)


# Taxa order
taxa_order <- c("Mollusca", "Cnidaria", "Benthic Foraminifera", "Planktic Foraminifera")

#save the plot
png("plots/S.1.png", width = 12, height = 10, units = "in", res = 600)

#Set up plotting area: 4 rows (taxa) × 4 cols (provinces)
par(mfrow = c(4, 4), mar = c(2, 3, 2, 1), oma = c(2, 3, 3, 2))

# Loop through taxa and provinces in specified order
for (taxon in taxa_order) {
  for (province in province_order) {
    
    # Get occurrence and genus richness data
    occ_data <- occ_richness_all[[taxon]][[province]]
    genus_data <- genus_richness_all[[taxon]][[province]]
    
    # Order by time
    occ_data <- occ_data[order(occ_data$mid_ma), ]
    genus_data <- genus_data[order(genus_data$mid_ma), ]
    
    # Calculate first differences
    occ_diff <- diff(occ_data$occurrence_count)
    genus_diff <- diff(genus_data$genus_richness)
    mid_ma <- occ_data$mid_ma[-1]
    
    # Align lengths
    len <- min(length(occ_diff), length(genus_diff), length(mid_ma))
    occ_diff <- occ_diff[1:len]
    genus_diff <- genus_diff[1:len]
    mid_ma <- mid_ma[1:len]
    
    # Correlation test
    cor_test <- cor.test(occ_diff, genus_diff)
    cor_val <- round(cor_test$estimate, 2)
    p_val <- cor_test$p.value
    col_text <- if (p_val < 0.05) "red" else "black"
    
    # Plot
    plot(mid_ma, occ_diff, type = "l", col = "black", lwd = 2,
         xlab = "", ylab = "", main = "", axes = TRUE, xaxt = "n", xlim = c(60, 5),
         ylim = range(c(occ_diff, genus_diff), na.rm = TRUE))
    lines(mid_ma, genus_diff, col = province_colors[province], lwd = 2, lty = 2)
    axis_geo(side = 1, intervals = "epoch", abbr = TRUE)
    
    # Annotate correlation
    mtext(paste("r =", cor_val), side = 3, line = -1.5, adj = 0.08, col = col_text, cex = 0.8)
    # Add the geologic axis
    axis_geo(side = 1, intervals = "epoch", abbr = TRUE)
    
    
    # Column titles for provinces (only top row)
    if (which(taxon == taxa_order) == 1) {
      mtext(province, side = 3, line = 1, cex = 0.9)
    }
    
    # Row labels for taxa (only leftmost column)
    if (which(province == province_order) == 1) {
      mtext(taxa_labels[taxon], side = 2, line = 3, cex = 0.9)
    }
  }
}

dev.off()
# Common axis labels
#mtext("Geologic age", side = 1, outer = TRUE, line = 3)
#mtext("First Differences of occurrences vs genus richness", side = 2, outer = TRUE, line = 3)
#mtext("Regional Sampling Intensity", side = 3, outer = TRUE, line = 1.5)

#### Figure.S2:Calculate quorum values for each taxa########

coverage_summary <- function(df) {
  
  # coverage per province × bin
  coverage_bin <- df %>%
    group_by(Province, bin_assignment) %>%
    summarise(
      N = n(),
      f1 = sum(table(genus) == 1),
      coverage = 1 - (f1 / N),
      .groups = "drop"
    )
  
  # mean coverage per province
  coverage_province <- coverage_bin %>%
    group_by(Province) %>%
    summarise(
      mean_coverage = mean(coverage, na.rm = TRUE),
      .groups = "drop"
    )
  
  # median across provinces
  tibble(
    median_q = median(coverage_province$mean_coverage, na.rm = TRUE),
    mean_q   = mean(coverage_province$mean_coverage, na.rm = TRUE)
  )
}

datasets <- list(
  Mollusca               = mollusca,
  `Benthic Foraminifera` = ben_foram,
  `Planktic Foraminifera`= plk_foram,
  Cnidaria               = cnidaria
)

coverage_table <- lapply(names(datasets), function(taxon) {
  
  coverage_summary(datasets[[taxon]]) %>%
    mutate(Taxon = taxon)
  
}) %>%
  bind_rows() %>%
  select(Taxon, median_q, mean_q)

coverage_table





#### Calculate SQS for all taxa and group######

#Define taxon-specific q values
q_values <- c(
  "Mollusca" = 0.8,
  "Cnidaria" = 0.8,
  "Benthic Foraminifera" = 0.7,
  "Planktic Foraminifera" = 0.9
)

# Create a list to store the occurrence data for each taxon group
taxa_list <- list(
  "Mollusca" = mollusca,
  "Cnidaria" = cnidaria,
  "Benthic Foraminifera" = ben_foram,
  "Planktic Foraminifera" = plk_foram) 


# Create empty lists to store results
global_results <- list()
province_results <- list()

# Loop over each taxon in taxa_order
for (taxon_name in names(taxa_list)){
  
  # Extract the current taxon data
  taxa <- taxa_list[[taxon_name]]
  
  # Get q value for the current taxon
  q_val <- q_values[[taxon_name]]
  
  # Calculate global SQS diversity
  sqs_res <- subsample(taxa, iter = 1000, q = q_val, tax = "genus",
                       bin = "bin_assignment", type = "sqs",
                       appr = "over", output = "dist")
  
  # Mean, standard deviation, and confidence intervals
  res_mean <- apply(sqs_res$divSIB, 1, mean)
  res_std <- apply(sqs_res$divSIB, 1, sd)
  res_95lower <- res_mean - 1.96 * res_std
  res_95higher <- res_mean + 1.96 * res_std
  res_mean[res_mean == 0] <- 1
  
  # Handle NA values
  res_95lower <- ifelse(is.na(res_95lower), 0, res_95lower)
  res_95higher <- ifelse(is.na(res_95higher), 0, res_95higher)
  
  # Create data frame for global diversity
  res_ric_sqs <- data.frame(
    bin = as.integer(names(res_mean)),
    mid_ma = bins$mid_ma[match(as.integer(names(res_mean)), bins$bin)],
    div = res_mean,
    div_95lower = res_95lower,
    div_95higher = res_95higher
  )
  
  # Store into global_results list
  global_results[[taxon_name]] <- res_ric_sqs
  
  # Process provincial data
  province_data <- split(taxa, taxa$Province)
  province_data[["Rest of world"]] <- NULL
  province_data[["East Pacific"]] <- NULL
  
  province_results[[taxon_name]] <- list()
  
  for (province_name in names(province_data)) {
    sqs_res_province <- subsample(province_data[[province_name]], iter = 1000, q = q_val, tax = "genus",
                                  bin = "bin_assignment", type = "sqs",
                                  appr = "over", output = "dist")
    
    res_mean_province <- apply(sqs_res_province$divSIB, 1, mean)
    res_std_province <- apply(sqs_res_province$divSIB, 1, sd)
    res_95lower_province <- res_mean_province - 1.96 * res_std_province
    res_95higher_province <- res_mean_province + 1.96 * res_std_province
    res_mean_province[res_mean_province == 0] <- 1
    
    res_95lower_province <- ifelse(is.na(res_95lower_province), 0, res_95lower_province)
    res_95higher_province <- ifelse(is.na(res_95higher_province), 0, res_95higher_province)
    
    res_ric_sqs_province <- data.frame(
      bin = as.integer(names(res_mean_province)),
      mid_ma = bins$mid_ma[match(as.integer(names(res_mean_province)), bins$bin)],
      div = res_mean_province,
      div_95lower = res_95lower_province,
      div_95higher = res_95higher_province
    )
    
    province_results[[taxon_name]][[province_name]] <- res_ric_sqs_province
  }
}

#Visual_inspection
str(global_results)
str(province_results)
str(occ_richness_all)


######SQS plot for each taxa across different provinces

par(mfrow = c(1, 4), mar = c(4, 4, 3, 1))


# Loop through each taxon
for (taxon_name in names(global_results)) {
  
  # Get and clean global data
  global_df <- global_results[[taxon_name]]
  global_df <- global_df[!is.na(global_df$div), ]
  
  # Plot global diversity with CI
  plot(global_df$mid_ma, global_df$div, type = "n",
       xlim = rev(range(global_df$mid_ma, na.rm = TRUE)),
       ylim = c(0, max(global_df$div_95higher, na.rm = TRUE) * 1.1),
       xlab = "Time (Ma)", ylab = "Genus Richness (SQS)", xaxt = "n",
       main = paste0(taxon_name, " (q = ", q_values[[taxon_name]], ")"),
       las = 1)
  
  # Global CI polygon
  polygon(c(global_df$mid_ma, rev(global_df$mid_ma)),
          c(global_df$div_95lower, rev(global_df$div_95higher)),
          col = adjustcolor("gray70", alpha.f = 0.5), border = NA)
  
  # Global mean line
  lines(global_df$mid_ma, global_df$div, col = "black", lwd = 2)
  
  # Add regional curves
  for (province_name in names(province_results[[taxon_name]])) {
    province_df <- province_results[[taxon_name]][[province_name]]
    col <- province_colors[province_name]
    
    polygon(c(province_df$mid_ma, rev(province_df$mid_ma)),
            c(province_df$div_95lower, rev(province_df$div_95higher)),
            col = adjustcolor(col, alpha.f = 0.3), border = NA)
    
    lines(province_df$mid_ma, province_df$div, col = col, lwd = 1.5)
  }
  
  axis_geo(side = 1, intervals = "epoch", abbr = TRUE)
  # Optional: Add legend to the first plot
  #if (taxon_name == names(global_results)[1]) {
  # legend("topright", legend = c("Global", names(province_colors)),
  #col = c("black", province_colors), lwd = 2, bty = "n", cex = 0.7)
  #}
}

#Save the std diversity data


df <- map_df(names(province_results), function(taxon) {
  map_df(names(province_results[[taxon]]), function(prov) {
    province_results[[taxon]][[prov]] %>%
      mutate(Taxon = taxon,
             Province = prov)
  })
}) %>%
  filter(!is.na(div))

write.csv(df, "Data/standardised_diversity_province.csv", row.names = FALSE)
#df <- read.csv("plots/standardised_diversity_province.csv")

#### Figure.S3:Plotting correlation of std. diversity and occurrences (first differences)########

png("plots/S.2.png", width = 12, height = 10, units = "in", res = 600)

par(mfrow = c(4, 4), mar = c(2, 3, 2, 1), oma = c(2, 3, 3, 2))

for (taxon in taxa_order) {
  for (province in province_order) {
    
    
    print(paste("Processing:", taxon, "-", province))
    div_df <- province_results[[taxon]][[province]]
    occ_df <- occ_richness_all[[taxon]][[province]]
    
    if (is.null(div_df) || is.null(occ_df)) {
      plot.new()
      next
    }
    
    merged_df <- merge(div_df[, c("mid_ma", "div")],
                       occ_df[, c("mid_ma", "occurrence_count")],
                       by = "mid_ma")
    
    merged_df <- merged_df[order(merged_df$mid_ma), ]
    
    d_mid_ma <- merged_df$mid_ma[-1]
    d_div <- diff(merged_df$div)
    d_occ <- diff(merged_df$occurrence_count)
    
    if (length(d_div) < 2 || length(d_occ) < 2) {
      plot.new()
      next
    }
    
    plot(d_mid_ma, d_occ, type = "l", col = "black", lwd = 2,
         xlab = "", ylab = "", main = "", axes = TRUE, xaxt = "n", xlim = c(60, 5),
         ylim = range(c(d_div, d_occ), na.rm = TRUE))
    
    lines(d_mid_ma, d_div, col = province_colors[province], lwd = 2, lty = 2)
    
    cor_test <- cor.test(d_div, d_occ)
    cor_val <- round(cor_test$estimate, 2)
    cor_p <- cor_test$p.value
    
    cor_col <- ifelse(cor_p < 0.05, "red", "black")
    
    axis_geo(side = 1, intervals = "epoch", abbr = TRUE)
    
    mtext(paste("r =", cor_val), side = 3, line = -1.5, adj = 0.08, col = cor_col, cex = 0.8)
    
    if (which(taxon == taxa_order) == 1) {
      mtext(province, side = 3, line = 1, cex = 0.9)
    }
    if (which(province == province_order) == 1) {
      mtext(taxa_labels[taxon], side = 2, line = 3.5, cex = 0.9)
    }
  }
}

dev.off()

back_up1 <- province_results


###Remoce the significant correlated provinces from the analysis: regions showing sampling bias.
#Remove "Northern Indian Ocean" from Mollusca and Cnidaria in province_results
province_results$Mollusca$`Northern Indian Ocean` <- NULL
province_results$Cnidaria$`Northern Indian Ocean` <- NULL



#### Figure.2: Raw richness vs Standardised richness #############

tiff("plots/Fig.2.tiff", width = 8, height = 5, units = "in", res = 600)

# Layout: 4 panels top + 4 panels middle + legend bottom
layout(matrix(c(1,2,3,4,
                5,6,7,8,
                9,9,9,9),
              nrow = 3, byrow = TRUE),
       heights = c(4,4,1))

#RAW GENUS RICHNESS PANELS

par(mar = c(4,4,2,1))

taxa_to_plot <- names(taxa_list)
i <- 1

for (taxon_name in taxa_to_plot) {
  
  taxa <- taxa_list[[taxon_name]]
  
  sqs_res <- divDyn(taxa, tax = "genus", bin = "bin_assignment")
  
  res_ric_sqs <- merge(sqs_res,
                       bins[,c("bin","mid_ma")],
                       by.x = "bin_assignment",
                       by.y = "bin",
                       all.x = TRUE)
  
  # Y-axis label only on first column
  ylab_text <- ifelse(i %% 4 == 1, "Raw genus richness", "")
  
  plot(res_ric_sqs$mid_ma,
       res_ric_sqs$div,
       type = "n",
       xlab = "", main = taxon_name,
       font.main = 1,
       cex.main = 1,
       ylab = ylab_text,
       xlim = c(63,5),
       ylim = range(c(0,res_ric_sqs$divSIB), na.rm = TRUE),
       xaxt = "n")
  
  lines(res_ric_sqs$mid_ma,
        res_ric_sqs$divSIB,
        col = "black",
        lwd = 2)
  
  province_data <- split(taxa, taxa$Province)
  province_data[["Rest of world"]] <- NULL
  
  if (taxon_name == "Echinodermata") {
    province_data[["IAA"]] <- NULL
  }
  
  for (province_name in names(province_data)) {
    
    sqs_res_province <- divDyn(province_data[[province_name]],
                               tax = "genus",
                               bin = "bin_assignment")
    
    res_ric_sqs_province <- merge(sqs_res_province,
                                  bins[,c("bin","mid_ma")],
                                  by.x = "bin_assignment",
                                  by.y = "bin",
                                  all.x = TRUE)
    
    lines(res_ric_sqs_province$mid_ma,
          res_ric_sqs_province$divSIB,
          col = province_colors[province_name],
          lwd = 2)
  }
  
  axis_geo(side = 1, intervals = "epoch", abbr = TRUE, lab_size = 0.8)
  
  i <- i + 1
}

#STD GENUS RICHNESS PANELS
par(mar = c(4,4,2,1))

i <- 1

for (taxon_name in names(taxa_list)) {
  
  global_df <- global_results[[taxon_name]]
  global_df <- global_df[!is.na(global_df$div),]
  
  ylab_text <- ifelse(i %% 4 == 1,
                      "Standardised genus richness",
                      "")
  
  plot(global_df$mid_ma,
       global_df$div,
       type = "n",
       xlim = c(63,5),
       ylim = c(0, max(global_df$div_95higher, na.rm = TRUE)*1.1),
       xlab = "Time (Ma)",
       ylab = ylab_text,
       xaxt = "n",
       las = 1)
  
  # Global CI polygon
  polygon(c(global_df$mid_ma, rev(global_df$mid_ma)),
          c(global_df$div_95lower, rev(global_df$div_95higher)),
          col = adjustcolor("gray70", alpha.f = 0.5),
          border = NA)
  
  # Global line
  lines(global_df$mid_ma,
        global_df$div,
        col = "black",
        lwd = 2)
  
  # Regional curves
  for (province_name in names(province_results[[taxon_name]])) {
    
    province_df <- province_results[[taxon_name]][[province_name]]
    col <- province_colors[province_name]
    
    polygon(c(province_df$mid_ma, rev(province_df$mid_ma)),
            c(province_df$div_95lower, rev(province_df$div_95higher)),
            col = adjustcolor(col, alpha.f = 0.3),
            border = NA)
    
    lines(province_df$mid_ma,
          province_df$div,
          col = col,
          lwd = 1.5)
  }
  
  axis_geo(side = 1, intervals = "epoch", abbr = TRUE, lab_size = 0.8)
  
  # q-value box
  legend("topleft",
         legend = paste0("q = ", q_values[[taxon_name]]),
         bty = "n",
         cex = 0.8)
  
  i <- i + 1
}


par(mar = c(0,0,0,0))
plot.new()

legend("top",
       inset = 0,
       horiz = TRUE,
       legend = c("Global", names(province_colors)),
       fill   = c("black", province_colors),
       border = NA,
       bty    = "n",
       cex    = 1,
       pt.cex = 2)

dev.off()


#### Table.S1: Initialize hotspot table and identification#########

hotspot_table_ci <- data.frame()

for (taxon_name in names(global_results)) {
  
  global_df <- global_results[[taxon_name]]
  
  for (province_name in names(province_results[[taxon_name]])) {
    
    province_df <- province_results[[taxon_name]][[province_name]]
    
    merged_df <- merge(province_df, global_df,
                       by = c("bin", "mid_ma"),
                       suffixes = c("_prov", "_glob"))
    
    merged_df <- merged_df[complete.cases(merged_df), ]
    
    merged_df$Taxon <- taxon_name
    merged_df$Province <- province_name
    
    hotspot_table_ci <- rbind(hotspot_table_ci, merged_df)
  }
}

# --- Assign epochs ---
hotspot_table_ci <- hotspot_table_ci %>%
  mutate(Epoch_grad = case_when(
    bin %in% 79:81 ~ "Paleocene",
    bin == 82 ~ "Early Eocene",
    bin == 83 ~ "Middle Eocene",
    bin %in% 84:85 ~ "Late Eocene",
    bin == 86 ~ "Early Oligocene",
    bin == 87 ~ "Late Oligocene",
    bin %in% 88:89 ~ "Early Miocene",
    bin %in% 90:91 ~ "Middle Miocene",
    bin %in% 92:93 ~ "Late Miocene",
    bin %in% 94:95 ~ "Pliocene",
    bin %in% 96:102 ~ "Pleistocene",
    TRUE ~ NA_character_
  ))

# --- Epoch-level averaging ---
epoch_hotspot_table <- hotspot_table_ci %>%
  group_by(Taxon, Province, Epoch_grad) %>%
  summarise(
    avg_div_95higher_prov = mean(div_95higher_prov, na.rm = TRUE),
    avg_div_prov = mean(div_prov, na.rm = TRUE),
    avg_div_95lower_prov = mean(div_95lower_prov, na.rm = TRUE),
    avg_div_95lower_glob = mean(div_95lower_glob, na.rm = TRUE),
    avg_div_glob = mean(div_glob, na.rm = TRUE),
    avg_div_95higher_glob = mean(div_95higher_glob, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # --- FINAL hotspot rule (choose ONE) ---
  
  mutate(
    # Option 1 (your original simple rule)
    epoch_hotspot = ifelse(avg_div_prov >= 0.5 * avg_div_glob, 1, 0)
    
    # Option 2 (recommended CI-based, more rigorous)
    # epoch_hotspot = ifelse(avg_div_95higher_prov >= avg_div_95lower_glob, 1, 0)
  )

# --- Filter and format ---
taxon_order <- c("Mollusca", "Benthic Foraminifera", 
                 "Planktic Foraminifera", "Cnidaria")

epoch_list <- c("Paleocene","Early Eocene", "Middle Eocene", "Late Eocene", 
                "Early Oligocene", "Late Oligocene", "Early Miocene", 
                "Middle Miocene", "Late Miocene", "Pliocene")

filtered_sorted_data <- epoch_hotspot_table %>%
  filter(epoch_hotspot == 1) %>%
  filter(!Epoch_grad %in% c("Pleistocene")) %>%
  arrange(
    factor(Taxon, levels = taxon_order),
    factor(Epoch_grad, levels = epoch_list)
  )

# --- Final table ---
formatted_data <- filtered_sorted_data %>%
  select(Epoch_grad, Province, Taxon) %>%
  mutate(
    Epoch_grad = factor(Epoch_grad, levels = epoch_list),
    Province = factor(Province, levels = province_order)
  ) %>%
  group_by(Epoch_grad, Province) %>%
  summarise(Taxon = paste(unique(Taxon), collapse = ", "), .groups = "drop") %>%
  pivot_wider(names_from = Province, values_from = Taxon) %>%
  arrange(Epoch_grad) %>%
  select(Epoch_grad, all_of(province_order))

write.csv(formatted_data, "Data/Table_1_hotspotidentification.csv")



#### Calculate the mechanism of hotspot#########
#Calculate total genus richness, ori, ext, mig and standing div of each province in each bin

table(pbdb$phylum)

hist(pbdb$max_ma)
pbdb <- subset(pbdb, max_ma < 541)

#binning the data according to age level 
bins <- time_bins()
pbdb <- bin_time(occdf = pbdb, bins = bins, method = "mid")
table(pbdb$bin_assignment)


#subsetting phanerozoic taxa on the basis of phylum
mol_phnz <- subset(pbdb, phylum == "Mollusca")
for_phnz <- subset(pbdb, phylum == "Foraminifera")
cnid_phnz <- subset(pbdb, phylum == "Cnidaria")

#calculating the FAD and LAD range of entire phanerozoic taxa
global_mol_range <- fadlad(mol_phnz, bin="bin_assignment", tax="genus")
global_bfor_range <- fadlad(for_phnz, bin="bin_assignment", tax="genus")
global_pfor_range <- fadlad(triton_clean, bin="bin_assignment", tax="genus")
global_cnid_range <- fadlad(cnid_phnz, bin="bin_assignment", tax="genus")


# Extract the genera in `not_in_pbdb` as row names for `global_pfor_range`
foram_not_in_pbdb <- c("Alicantina", "Astrorotalia", "Berggrenia", "Ciperoella", 
                       "Clavigerinella", "Cribrohantkenina", "Dentigloborotalia", 
                       "Dipsidripella", "Gallitellia", "Globigerinatella", 
                       "Globigerinoidesella", "Hastigerinella", "Hirsutella", 
                       "Igorina", "Jenkinsina", "Mutabella", "Orcadia", 
                       "Pearsonites", "Planoglobanomalina", "Protentella", 
                       "Pseudoglobigerinella", "Truncorotalia", "Zeauvigerina")

# Subset rows from `global_pfor_range` that match `not_in_pbdb`
subset_pfor <- global_pfor_range[rownames(global_pfor_range) %in% foram_not_in_pbdb, ]

# Combine with `global_for_range`
global_for_range <- rbind(global_bfor_range, subset_pfor)
epoch_list <- c(
  "Paleocene",
  "Early Eocene",
  "Middle Eocene",
  "Late Eocene",
  "Early Oligocene",
  "Late Oligocene",
  "Early Miocene",
  "Middle Miocene",
  "Late Miocene",
  "Pliocene",
  "Pleistocene"
)


# The main function to calculate diversity components
calculate_epoch_diversity_components <- function(taxa_data, global_FAD_range, province_list) {
  
  all_epochs <- unique(taxa_data$Epoch_grad)
  all_diversity_data <- data.frame()
  
  for (province_name in province_list) {
    
    province_data <- taxa_data %>% filter(Province == province_name)
    
    for (current_epoch in all_epochs) {
      
      epoch_data <- province_data %>% filter(Epoch_grad == current_epoch)
      
      if (nrow(epoch_data) == 0) next
      
      total_genera <- unique(epoch_data$genus)
      epoch_bins <- unique(epoch_data$bin_assignment)
      
      # Origination — FAD lies inside any bin of this Epoch_grad
      originating_genera <- total_genera[
        sapply(total_genera, function(genus_name) {
          genus_name %in% rownames(global_FAD_range) &&
            global_FAD_range[genus_name, "FAD"] %in% epoch_bins
        })
      ]
      
      # Extinction — LAD lies inside any bin of this Epoch_grad
      extinct_genera <- total_genera[
        sapply(total_genera, function(genus_name) {
          genus_name %in% rownames(global_FAD_range) &&
            global_FAD_range[genus_name, "LAD"] %in% epoch_bins
        })
      ]
      
      # Migration — originated elsewhere earlier, arrived here during this Epoch_grad
      migrated_genera <- total_genera[
        sapply(total_genera, function(genus_name) {
          if (!(genus_name %in% rownames(global_FAD_range))) return(FALSE)
          
          global_fad <- global_FAD_range[genus_name, "FAD"]
          if (global_fad >= min(epoch_bins)) return(FALSE)
          
          origin_province <- taxa_data %>%
            filter(genus == genus_name) %>%
            slice_min(bin_assignment, with_ties = FALSE) %>%
            pull(Province)
          
          if (length(origin_province) == 1 && origin_province != province_name) {
            first_appearance_in_province <- taxa_data %>%
              filter(genus == genus_name, Province == province_name) %>%
              pull(bin_assignment) %>%
              min(na.rm = TRUE)
            return(first_appearance_in_province %in% epoch_bins)
          }
          return(FALSE)
        })
      ]
      
      standing_genera <- setdiff(total_genera, c(originating_genera, extinct_genera, migrated_genera))
      
      diversity_counts <- data.frame(
        Category = c("Origination", "Extinction", "Migration", "Standing Diversity", "total_genera"),
        Count = c(
          length(originating_genera),
          length(extinct_genera),
          length(migrated_genera),
          length(standing_genera),
          length(total_genera)
        ),
        Epoch_grad = current_epoch,
        Province = province_name
      )
      
      all_diversity_data <- rbind(all_diversity_data, diversity_counts)
    }
  }
  
  diversity_wide <- all_diversity_data %>%
    pivot_wider(names_from = Category, values_from = Count) %>%
    distinct()
  
  return(diversity_wide)
}

# Now apply this function for each Taxon:

Province <- c("West Atlantic", "Western Tethys", "Northern Indian Ocean", "Indo-Australian Archipelago")

# Create your taxa list
taxa_list <- list(
  "Mollusca" = list(data = mollusca, global_FAD = global_mol_range),
  "Benthic Foraminifera" = list(data = ben_foram, global_FAD = global_for_range),
  "Planktic Foraminifera" = list(data = plk_foram, global_FAD = global_for_range),
  "Cnidaria" = list(data = cnidaria, global_FAD = global_cnid_range)
)


all_results <- data.frame()

for (taxon_name in names(taxa_list)) {
  
  cat("Processing:", taxon_name, "\n")
  
  taxon_data <- taxa_list[[taxon_name]]$data
  global_FAD <- taxa_list[[taxon_name]]$global_FAD
  
  this_result <- calculate_epoch_diversity_components(
    taxa_data = taxon_data,
    global_FAD_range = global_FAD,
    province_list = Province
  )
  
  this_result$Taxon <- taxon_name
  
  all_results <- rbind(all_results, this_result)
}

write.csv(all_results, "Data/macroevol_raw_genus.csv", row.names = FALSE)
epoch_macroevol <- all_results
epoch_macroevol$`StandingDiversity` <- epoch_macroevol$`Standing Diversity`



#### Combing the the macro-evolutionary processes with std diversity####

final_epoch_diversity_table <- epoch_macroevol
hotspot_mechanism <- final_epoch_diversity_table %>%
  inner_join(epoch_hotspot_table, by = c("Taxon", "Epoch_grad", "Province"))

# View result
head(hotspot_mechanism)
table(hotspot_mechanism$Province)
table(hotspot_mechanism$Taxon)
table(hotspot_mechanism$Epoch_grad)
# Create a data frame with Epoch_grad and corresponding mid_ma values
epoch_mid_ma <- data.frame(
  Epoch_grad = c("Paleocene", "Early Eocene", "Middle Eocene", "Late Eocene", 
                 "Early Oligocene", "Late Oligocene", "Early Miocene", 
                 "Middle Miocene", "Late Miocene", "Pliocene", "Pleistocene"),
  
  mid_ma = c(60.4, 51.9, 44.5, 37, 30.8, 25.4, 20, 13.8, 9.4, 3.0, 0.5)
)

# Merge using base R merge
hotspot_mechanism <- merge(hotspot_mechanism, epoch_mid_ma, by = "Epoch_grad", all.x = TRUE)


# Subset where hotspot is 1
hotspot_only <- hotspot_mechanism %>%
  filter(epoch_hotspot == 1) %>%
  select(-c(mid_ma, avg_div_prov, avg_div_glob))

# Check result
head(hotspot_only)

# Save as CSV
#write.csv(hotspot_only, "hotspot_province.csv", row.names = FALSE)
write.csv(hotspot_mechanism, "Data/hotspot_province_mechanism.csv", row.names = FALSE)
hotspot_mechanism <- read.csv("Data/hotspot_province_mechanism.csv")


hotspot_mechanism$Mechanism <- NA
hotspot_mechanism <- hotspot_mechanism %>%
  rowwise() %>%
  mutate(
    Mechanism = case_when(
      epoch_hotspot == 1 & Origination == max(c(Origination, Extinction, Migration), na.rm = TRUE) ~ "Origination",
      epoch_hotspot == 1 & Migration == max(c(Origination, Extinction, Migration), na.rm = TRUE)   ~ "Immigration",
      epoch_hotspot == 1 & Extinction == max(c(Origination, Extinction, Migration), na.rm = TRUE)  ~ "Extinction",
      TRUE ~ Mechanism
    )
  ) %>%
  ungroup()


#### Figure.S4:Plot mechanism of hotspot based on the province############
# Taxa positions (Y-axis)
taxa_order <- c("Planktic Foraminifera", "Benthic Foraminifera", "Cnidaria", "Mollusca")
taxa_labels <- c("Planktic\nForaminifera", "Benthic\nForaminifera", "Cnidaria", "Mollusca")
taxa_positions <- setNames(seq_along(taxa_order), taxa_order)

# Province order (for panels)
province_order <- c("West Atlantic", "Western Tethys", "Northern Indian Ocean", "Indo-Australian Archipelago")

# Mechanism colors
mechanism_colors <- c(
  "Origination"   = "#799ecb",
  "Immigration"  = "#CCFF99",
  #"Centre of Survival"      = "#FFD1A1",
  "Extinction"       = "#FF9999"
)

png("plots/S.3.png", width = 10, height = 3, units = "in", res = 600)

# Layout: 4 province panels, 1 legend panel
layout(matrix(c(1, 2, 3, 4, 5, 5, 5, 5), nrow = 2, byrow = TRUE), heights = c(4, 1))

# Fixed box width (Ma units)
box_width <- 4

# Loop for province panels
for (prov_name in province_order) {
  
  prov_data <- subset(hotspot_mechanism, Province == prov_name & Taxon %in% taxa_order)
  
  # Wider margin for the first province (to fit y-axis labels)
  if (prov_name == province_order[1]) {
    par(mar = c(2, 1, 1.8, 0.5)) 
  } else {
    par(mar = c(2, 1, 1.8, 0.5)) 
  }
  plot(1, type = "n", xlim = c(55, 5), ylim = c(0.5, length(taxa_order) + 0.5),
       xlab = "Time (Ma)", ylab = "", xaxt = "n", yaxt = "n",
       main = prov_name, font.main = 1, cex.main = 1.5)
  
  axis_geo(side = 1, intervals = "epoch", abbr = TRUE, cex.axis = 1.5)
  
  # Add taxon labels on first plot only
  if (prov_name == province_order[1]) {
    axis(2, at = seq_along(taxa_order), labels = taxa_labels, las = 2, cex.axis = 1.5)
  }
  
  for (i in 1:nrow(prov_data)) {
    row <- prov_data[i, ]
    taxon <- as.character(row$Taxon)
    
    if (!(taxon %in% names(taxa_positions))) next
    if (is.na(row$mid_ma)) next
    
    y_center <- taxa_positions[taxon]
    xmin <- row$mid_ma - box_width / 2
    xmax <- row$mid_ma + box_width / 2
    ymin <- y_center - 0.15
    ymax <- y_center + 0.15
    
    if (!is.na(row$epoch_hotspot) && row$epoch_hotspot == 1) {
      
      if (grepl("Centre of Survival/", row$Mechanism)) {
        primary_mech <- sub("Centre of Survival/", "", row$Mechanism)
        
        polygon(x = c(xmin, xmax, xmax),
                y = c(ymax, ymax, ymin),
                col = mechanism_colors[primary_mech], border = "black")
        
        polygon(x = c(xmin, xmin, xmax),
                y = c(ymax, ymin, ymin),
                col = mechanism_colors["Centre of Survival"], border = "black")
        
      } else {
        rect(xmin, ymin, xmax, ymax, col = mechanism_colors[row$Mechanism], border = "black")
      }
      
    } else {
      rect(xmin, ymin, xmax, ymax, col = "white", border = "black")
    }
  }
}

# Legend panel
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = names(mechanism_colors),
       fill = mechanism_colors,
       border = "black",
       bty = "n",
       #border = NA,
       cex = 2,
       pt.cex = 3,
       ncol = 3)



dev.off()


#### Add paleo-climate data to hotspot_province_ mechanism dataset####################

##created in macbook latest data
hotspot_dyn <- read.csv("Data/hotspot_province_mechanism.csv")

##Paleo-shelf_area calculation


# Load required libraries
library(ncdf4)
library(raster)



# Define file paths for NetCDF files  and read them
file_paths <- list(
  "Pliocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map01_PALEOMAP_1deg_Holocene_0Ma.nc",
  "Late Miocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map05_PALEOMAP_1deg_middle_Late_Miocene_10Ma.nc",
  "Middle Miocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map06_PALEOMAP_1deg_Middle_Miocene_15Ma.nc",
  "Early Miocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map07_PALEOMAP_1deg_Early_Miocene_20Ma.nc",
  "Late Oligocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map08_PALEOMAP_1deg_Late_Oligocene_25Ma.nc",
  "Early Oligocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map09_PALEOMAP_1deg_Early_Oligocene_30Ma.nc",
  "Late Eocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map10_PALEOMAP_1deg_Late_Eocene_35Ma.nc",
  "Middle Eocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map12_PALEOMAP_1deg_early_Middle_Eocene_45Ma.nc",
  "Early Eocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map13_PALEOMAP_1deg_Early_Eocene_50Ma.nc",
  "Paleocene" = "Data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/Map15_PALEOMAP_1deg_Paleocene_60Ma.nc"
)

# Define lat-lon ranges for each province
provinces <- list(
  "West Atlantic" = list(lat = c(10, 50), lng = c(-90, -55)),
  "Western Tethys" = list(lat = c(25, 60), lng = c(-20, 45)),
  "Northern Indian Ocean" = list(lat = c(-10, 50), lng = c(45, 100)),
  "Indo-Australian Archipelago" = list(lat = c(-15, 15), lng = c(100, 145))
)

# Function to calculate shelf area within a given province
calculate_shelf_area <- function(file_path, lat_range, lng_range) {
  nc_data <- raster(file_path)
  extent_region <- extent(lng_range[1], lng_range[2], lat_range[1], lat_range[2])
  nc_data <- crop(nc_data, extent_region)  # Crop to province
  
  # Define shelf depth range (0m to -1000m)
  shelf_cells <- nc_data[nc_data >= -1000 & nc_data <= 0]
  
  # Calculate area (assuming 1° x 1° grid cells)
  cell_area_km2 <- 111 * 111  # Approximate area of 1° x 1° grid cell in km²
  shelf_area <- length(shelf_cells) * cell_area_km2
  
  return(shelf_area)
}

# Initialize results dataframe
results <- data.frame()

# Loop through each file and province
for (time_period in names(file_paths)) {
  for (province in names(provinces)) {
    lat_range <- provinces[[province]]$lat
    lng_range <- provinces[[province]]$lng
    
    shelf_area_km2 <- calculate_shelf_area(file_paths[[time_period]], lat_range, lng_range)
    
    # Store results
    results <- rbind(results, data.frame(Time = time_period, Province = province, Shelf_Area_km2 = shelf_area_km2))
  }
}

# Convert Time to factor with correct order
results$Time <- factor(results$Time, levels = rev(names(file_paths)))
colnames(results)[colnames(results) == "Time"] <- "Epoch_grad"


merged_data2 <- merge(hotspot_dyn, results, by = c("Epoch_grad", "Province"), all = TRUE)




##Sealevel

sealvl <- read.csv("Data/Data_S4.csv")
glimpse(sealvl)

plot.new()
plot(sealvl$Age.Ma., sealvl$SeaLevel.m.Smoothed,
     type = "l", col = "blue", lwd = 2,
     xlab = "Age (Ma)", ylab = "Smoothed Sea Level (m)",
     main = "Smoothed Sea Level Through Time",
     xlim = c(60, 5))  # X-axis from 60 (left) to 5 (right)
abline(h = 0, col = "black", lty = 2)





# Assign epochs based on Age.Ma.
sealvl <- sealvl %>%
  mutate(
    Epoch_grad = case_when(
      Age.Ma. >= 56 & Age.Ma. < 66 ~ "Paleocene",
      Age.Ma. >= 47.8 & Age.Ma. < 56 ~ "Early Eocene",
      Age.Ma. >= 38 & Age.Ma. < 47.8 ~ "Middle Eocene",
      Age.Ma. >= 33.9 & Age.Ma. < 38 ~ "Late Eocene",
      Age.Ma. >= 27.8 & Age.Ma. < 33.9 ~ "Early Oligocene",
      Age.Ma. >= 23 & Age.Ma. < 27.8 ~ "Late Oligocene",
      Age.Ma. >= 15.97 & Age.Ma. < 23 ~ "Early Miocene",
      Age.Ma. >= 11.63 & Age.Ma. < 15.97 ~ "Middle Miocene",
      Age.Ma. >= 5.33 & Age.Ma. < 11.63 ~ "Late Miocene",
      Age.Ma. >= 2.58 & Age.Ma. < 5.33 ~ "Pliocene",
      TRUE ~ NA_character_
    )
  )

# Define midpoints for epochs
epoch_midpoints <- tibble(
  Epoch_grad = c("Paleocene", "Early Eocene", "Middle Eocene", "Late Eocene",
                 "Early Oligocene", "Late Oligocene", "Early Miocene", "Middle Miocene",
                 "Late Miocene", "Pliocene"),
  midpoint_age = c(61, 51.9, 42.9, 35.95, 30.85, 25.4, 19.5, 13.8, 8.48, 4)
)


summary_table <- epoch_midpoints %>%
  mutate(
    mean_sealevel = map_dbl(midpoint_age, function(age) {
      mean(sealvl$SeaLevel.m.Smoothed[
        sealvl$Age.Ma >= (age - 1) & sealvl$Age.Ma <= (age + 1)
      ], na.rm = TRUE)
    })
  )

print(summary_table)

merged_data3 <- merge(merged_data2, summary_table, by = c("Epoch_grad"), all = TRUE)




##Paleo-SST with respect to trophic and temperate

paleo_SST <- read.csv("Data/Data_S5.csv")

# Join merged_data3 with merged_epochs using Epoch_grad
merged_with_sst <- merged_data3 %>%
  left_join(paleo_SST, by = "Epoch_grad") %>%
  mutate(
    SST = case_when(
      Province %in% c("West Atlantic", "Northern Indian Ocean", "Indo-Australian Archipelago") ~ SST_tropic,
      Province == "Western Tethys" ~ SST_temperate,
      TRUE ~ NA_real_
    )
  )

# Drop the SST source columns from merged_epochs
merged_with_sst <- merged_with_sst[, !names(merged_with_sst) %in% c("SST_tropic", "SST_temperate", "SST_global")]

# Filter out unwanted Provinces and blank Epoch_grad
merged_with_sst <- subset(merged_with_sst, 
                          !(Province %in% c("East Pacific", "Rest of world")) &
                            !(Epoch_grad %in% c("Pleistocene")))



hotspot_dynamics <- merged_with_sst
hotspot_dynamics <- hotspot_dynamics[!is.na(hotspot_dynamics$epoch_hotspot),]
table(hotspot_dynamics$Province)
table(hotspot_dynamics$Epoch_grad)

write.csv(hotspot_dynamics,"Data/hotspot_dynamics.csv", row.names = FALSE)
#hotspot_dynamics <- read.csv("Data/hotspot_dynamics.csv")


glimpse(hotspot_dynamics)


#### Figure.3: Macro evolutionary process ooperating in hotspot vs non-hotspot##################


library(rstatix)
library(ggpubr)



hotspot_dynamics <- read.csv("Data/hotspot_dynamics.csv") %>%
  #recheck to remove regions with sampling bias
  filter(!(Province == "Northern Indian Ocean" & Taxon %in% c("Mollusca", "Cnidaria"))) %>%
  #filter(Taxon != "Mollusca") %>%
  mutate(epoch_hotspot = factor(epoch_hotspot,
                                levels = c(0,1),
                                labels = c("Non-hotspot", "Hotspot")),
         Taxon = factor(Taxon,
                        levels = c("Mollusca","Cnidaria", "Benthic Foraminifera", "Planktic Foraminifera"))
  )


## Helper function: reshape + significance table

prepare_long <- function(df, cols, labels){
  df %>%
    pivot_longer(cols = all_of(cols),
                 names_to = "Process",
                 values_to = "Value") %>%
    mutate(Process = factor(Process,
                            levels = cols,
                            labels = labels))
}

get_sig <- function(df){
  df %>%
    group_by(Taxon, Process) %>%
    wilcox_test(Value ~ epoch_hotspot) %>%
    add_significance("p") %>%
    filter(p < 0.05) %>%
    mutate(label = p.signif,
           y_position = max(df$Value, na.rm = TRUE) * 1.10)
}

##Raw macro‐evolutionary process contributions
proc_cols  <- c("Origination", "Extinction", "Migration")
proc_labels <- c("Origination", "Extinction", "Immigration")

hotspot_long1 <- prepare_long(hotspot_dynamics, proc_cols, proc_labels)
sig1 <- get_sig(hotspot_long1)

plot1 <- ggplot(hotspot_long1, aes(x = Process, y = Value, fill = epoch_hotspot)) +
  geom_boxplot(alpha = 0.7, width = 0.6, position = position_dodge(0.8),
               color = "black", outlier.shape = 21, outlier.size = 1.5) +
  geom_segment(data = sig1,
               aes(x = as.numeric(Process)-0.2,
                   xend = as.numeric(Process)+0.2,
                   y = y_position, yend = y_position),
               inherit.aes = FALSE, linewidth = 0.7) +
  geom_text(data = sig1,
            aes(x = as.numeric(Process),
                y = y_position*1.02,
                label = label),
            inherit.aes = FALSE, size = 5) +
  facet_wrap(~Taxon, nrow = 1) +
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000"))+
  scale_fill_manual(values = c("Hotspot"="#cc1100","Non-hotspot"="#6373c9"
  )) +
  labs(x="", y="Genera contributing to the processes",
       fill="Hotspot Status") +
  theme_classic(base_size=13) +
  theme(strip.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position="bottom")

## Save Plot 
ggsave("plots/Fig.3.tiff", plot1, width=12, height=5, dpi=600)



#### Table.S2-S3:BAYESIAN STRUCTURAL EQUATION MODELS##################################################


library(brms)
library(posterior)
library(ggpubr)
library(igraph)
library(scales)

#Data Loading & Cleaning ---
hotspot_dynamics <- read.csv("Data/hotspot_dynamics.csv")


#Scaling (remove Standing.Diversity completely) ---
hotspot_dynamics_scaled <- hotspot_dynamics %>%
  mutate(across(
    c(Origination, Extinction, Migration, SST, Shelf_Area_km2, mean_sealevel),
    ~ scale(.x)[, 1]
  ))

#  Define Bayesian SEM without Standing Diversity
orig_model <- bf(Origination ~ SST + Shelf_Area_km2 + mean_sealevel +
                   (1 + SST + Shelf_Area_km2 + mean_sealevel | Taxon))

ext_model <- bf(Extinction ~ SST + Shelf_Area_km2 + mean_sealevel +
                  (1 + SST + Shelf_Area_km2 + mean_sealevel | Taxon))

mig_model <- bf(Migration ~ SST + Shelf_Area_km2 + mean_sealevel +
                  (1 + SST + Shelf_Area_km2 + mean_sealevel | Taxon))

hotspot_model <- bf(epoch_hotspot ~ Origination + Extinction + Migration +
                      (1 + Origination + Extinction + Migration | Taxon),
                    family = bernoulli())


fit_sem <- brm(
  orig_model + ext_model + mig_model + hotspot_model + set_rescor(FALSE),
  data = hotspot_dynamics_scaled,
  chains = 4,
  cores = 4,
  iter = 10000,
  warmup = 200,
  control = list(adapt_delta = 0.995, max_treedepth = 18),
  seed = 123
)


fit_sem

#Extract posterior draws 
posterior_draws <- as_draws_df(fit_sem)

taxa <- rownames(ranef(fit_sem)$Taxon)
env_vars <- c("SST", "Shelf_Area_km2", "mean_sealevel")
processes <- c("Origination", "Extinction", "Migration")

#Compute Direct and Indirect Effects 
causal_list <- list()
idx <- 1

for (taxon in taxa) {
  for (env in env_vars) {
    for (proc in processes) {
      
      env_proc_term <- paste0("b_", proc, "_", env)
      proc_hotspot_term <- paste0("b_epochhotspot_", proc)
      re_env_proc_term <- paste0("r_Taxon__", proc, "[", taxon, ",", env, "]")
      re_proc_hotspot_term <- paste0("r_Taxon__epochhotspot[", taxon, ",", proc, "]")
      
      if (!(env_proc_term %in% colnames(posterior_draws)) ||
          !(proc_hotspot_term %in% colnames(posterior_draws))) next
      
      fe_env_proc <- posterior_draws[[env_proc_term]]
      fe_proc_hotspot <- posterior_draws[[proc_hotspot_term]]
      
      re_env_proc <- if (re_env_proc_term %in% colnames(posterior_draws)) posterior_draws[[re_env_proc_term]] else 0
      re_proc_hotspot <- if (re_proc_hotspot_term %in% colnames(posterior_draws)) posterior_draws[[re_proc_hotspot_term]] else 0
      
      est_env_proc <- fe_env_proc + re_env_proc
      est_proc_hotspot <- fe_proc_hotspot + re_proc_hotspot
      
      mean_direct <- mean(est_proc_hotspot)
      ci_direct <- quantile(est_proc_hotspot, c(0.025, 0.975))
      sig_direct <- !(ci_direct[1] <= 0 & ci_direct[2] >= 0)
      
      indirect_effect <- est_env_proc * est_proc_hotspot
      mean_indirect <- mean(indirect_effect)
      ci_indirect <- quantile(indirect_effect, c(0.025, 0.975))
      sig_indirect <- !(ci_indirect[1] <= 0 & ci_indirect[2] >= 0)
      
      causal_list[[idx]] <- data.frame(
        Taxon = taxon,
        Env_Var = env,
        Process = proc,
        Mean_Indirect_Effect = mean_indirect,
        Q2.5_Indirect = ci_indirect[1],
        Q97.5_Indirect = ci_indirect[2],
        Significant_Indirect = sig_indirect,
        Mean_Direct_Effect = mean_direct,
        Q2.5_Direct = ci_direct[1],
        Q97.5_Direct = ci_direct[2],
        Significant_Direct = sig_direct
      )
      idx <- idx + 1
    }
  }
}

causal_df_posterior <- bind_rows(causal_list) %>%
  mutate(across(where(is.numeric), round, 3))

# Save results
write.csv(causal_df_posterior, "Data/causal_effects.csv", row.names = FALSE)

#causal_df_posterior <- read.csv("Data/causal_effects.csv")


#### Figure.4: single plot for all the mechanisms################

# --- 1. Prepare edges ---
edges_env_proc <- causal_df_posterior %>%
  mutate(
    from = paste0(Env_Var, "_", Taxon),
    to = paste0(Process, "_", Taxon)
  ) %>%
  select(from, to, weight = Mean_Indirect_Effect, sig = Significant_Indirect)

edges_proc_hotspot <- causal_df_posterior %>%
  mutate(
    from = paste0(Process, "_", Taxon),
    to = "Hotspot"
  ) %>%
  select(from, to, weight = Mean_Direct_Effect, sig = Significant_Direct) %>%
  distinct(from, to, .keep_all = TRUE)

edges_all <- bind_rows(edges_env_proc, edges_proc_hotspot)

# --- 2. Build igraph object ---
vertices <- data.frame(name = unique(c(edges_all$from, edges_all$to)), stringsAsFactors = FALSE)
g <- graph_from_data_frame(edges_all, vertices = vertices, directed = TRUE)

# --- 3. Define layout manually ---
layout <- matrix(NA, nrow = length(V(g)), ncol = 2)
rownames(layout) <- V(g)$name

# Center node
layout["Hotspot", ] <- c(0,0)

# --- Mollusca (left) ---
layout["Origination_Mollusca", ] <- c(-1,0.5)
layout["Extinction_Mollusca", ] <- c(-1,0)
layout["Migration_Mollusca", ] <- c(-1,-0.5)

layout["SST_Mollusca", ] <- c(-2,1)
layout["Shelf_Area_km2_Mollusca", ] <- c(-2,0)
layout["mean_sealevel_Mollusca", ] <- c(-2,-1)

# --- Cnidaria (top) ---
layout["Origination_Cnidaria", ] <- c(-0.5,1)
layout["Extinction_Cnidaria", ] <- c(0,1)
layout["Migration_Cnidaria", ] <- c(0.5,1)

layout["SST_Cnidaria", ] <- c(-1,2)
layout["Shelf_Area_km2_Cnidaria", ] <- c(0,2)
layout["mean_sealevel_Cnidaria", ] <- c(1,2)

# --- Benthic Foraminifera (right) ---
layout["Origination_Benthic Foraminifera", ] <- c(1,0.5)
layout["Extinction_Benthic Foraminifera", ] <- c(1,0)
layout["Migration_Benthic Foraminifera", ] <- c(1,-0.5)

layout["SST_Benthic Foraminifera", ] <- c(2,1)
layout["Shelf_Area_km2_Benthic Foraminifera", ] <- c(2,0)
layout["mean_sealevel_Benthic Foraminifera", ] <- c(2,-1)

# --- Planktic Foraminifera (bottom) ---
layout["Origination_Planktic Foraminifera", ] <- c(-0.5,-1)
layout["Extinction_Planktic Foraminifera", ] <- c(0,-1)
layout["Migration_Planktic Foraminifera", ] <- c(0.5,-1)

layout["SST_Planktic Foraminifera", ] <- c(-1,-2)
layout["Shelf_Area_km2_Planktic Foraminifera", ] <- c(0,-2)
layout["mean_sealevel_Planktic Foraminifera", ] <- c(1,-2)


# --- 4. Node colors and labels ---
V(g)$label <- sapply(V(g)$name, function(x){
  if(grepl("Origination", x)) return("Origination")
  if(grepl("Extinction", x)) return("Extinction")
  if(grepl("Migration", x)) return("Immigration")
  if(grepl("SST", x)) return("SST")
  if(grepl("Shelf_Area_km2", x)) return("Shelf Area")
  if(grepl("mean_sealevel", x)) return("Mean\nSea Level")
  if(x == "Hotspot") return("Hotspot")
  return(x)
})

V(g)$color <- sapply(V(g)$name, function(x){
  if(grepl("SST|Shelf_Area_km2|mean_sealevel", x)) return("#FFD580")   # yellow for env
  if(grepl("Origination|Extinction|Migration", x)) return("#ADD8E6")   # blue for process
  if(x == "Hotspot") return("gray")
  return("white")
})

# --- 5. Edge aesthetics ---
edge_color <- ifelse(E(g)$weight > 0,"red",
                     ifelse(E(g)$weight < 0,"blue","black"))
edge_width <- scales::rescale(abs(E(g)$weight), to=c(1,6))
edge_lty <- ifelse(edges_all$sig,"solid","dashed")



tiff("plots/Fig.4_combined_causal_network.tiff", width = 7, height = 7, units = "in", res = 600)

par(mar = c(2.3,2.3,2.3,2.3))
range(layout)

# allow drawing outside plot region
plot(g,
     layout = layout,
     vertex.size = 19,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.frame.color = "black",
     edge.arrow.size = 0.4,
     edge.curved = 0.2,
     edge.width = edge_width,
     edge.color = edge_color,
     edge.lty = edge_lty,
     xlim = c(-1,1),   # expand plot area
     ylim = c(-1,1))



text(-1.15, 0, "Mollusca", cex = 1, font = 2, srt = 90)       # vertical left
text( 1.15, 0, "Benthic Foraminifera", cex = 1, font = 2, srt = 270)      # vertical right
text( 0, 1.15, "Cnidaria", cex = 1, font = 2)     # horizontal top
text( 0,-1.15, "Planktic Foraminifera", cex = 1, font = 2)    # horizontal bottom
dev.off()
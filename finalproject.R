library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo)

# Set to where the project folder is located
setwd('~/Academic/FALL 2022/MATH 208/Project')

cpu_gpu_data <- read_csv('chip_dataset.csv')
str(cpu_gpu_data)

# Chapter 2: Understanding the Data

# Remove index column
cpu_gpu_data <- select(cpu_gpu_data, -c('...1'))

# Replace space by _ and remove paranthesis
names(cpu_gpu_data)[3] = "Release_Date"
names(cpu_gpu_data)[4] = "Process_Size_nm"
names(cpu_gpu_data)[5] = "TDP_W"
names(cpu_gpu_data)[6] = "Die_Size_mm2"
names(cpu_gpu_data)[7] = "Transistors_million"
names(cpu_gpu_data)[8] = "Freq_MHz"
names(cpu_gpu_data)[11] = "FP16_GFLOPS"
names(cpu_gpu_data)[12] = "FP32_GFLOPS"
names(cpu_gpu_data)[13] = "FP64_GFLOPS"

# Count NA values
cpu_gpu_data %>% sapply(function(x) sum(is.na(x)))

# Replace NA GFLOPS with -1
cpu_gpu_data[["FP16_GFLOPS"]][is.na(cpu_gpu_data[["FP16_GFLOPS"]])] <- -1
cpu_gpu_data[["FP32_GFLOPS"]][is.na(cpu_gpu_data[["FP32_GFLOPS"]])] <- -1
cpu_gpu_data[["FP64_GFLOPS"]][is.na(cpu_gpu_data[["FP64_GFLOPS"]])] <- -1

# Replace other NA values by mean
cpu_gpu_data <- replace(cpu_gpu_data, TRUE, lapply(cpu_gpu_data, na.aggregate))

# Drop rows with NaT release dates
cpu_gpu_data <- cpu_gpu_data[cpu_gpu_data$Release_Date != "NaT",]

# Drop rows with 0 process size
cpu_gpu_data <- cpu_gpu_data[cpu_gpu_data$Process_Size_nm != 0,]

# Plot class distribution piechart
examples_CPU <= nrow(cpu_gpu_data[cpu_gpu_data$Type == 'CPU',])
examples_GPU <- nrow(cpu_gpu_data) - examples_CPU
slices <- c(examples_CPU, examples_GPU)
pct <- round(slices/sum(slices)*100)
lbls <- c('CPU', 'GPU')
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices, labels = lbls)


# Chapter 3: Summarizing the Data
# Process Size: Key statistics summary
cpu_gpu_data %>% group_by(Type) %>% summarize(mean = mean(Process_Size_nm),
  sd = sd(Process_Size_nm), median = median(Process_Size_nm), max = max(Process_Size_nm),
  min = min(Process_Size_nm))

# Process Size: Boxplot
cpu_gpu_data %>% ggplot(aes(x=Type, y=Process_Size_nm, fill=Type)) + 
  ylab("Process Size") + geom_boxplot() + coord_flip()


# TDP: Key statistics summary
cpu_gpu_data %>% group_by(Type) %>% summarize(mean = mean(TDP_W),
  sd = sd(TDP_W), median = median(TDP_W), max = max(TDP_W),
  min = min(TDP_W))

# TDP: Boxplot
cpu_gpu_data %>% ggplot(aes(x=TDP_W, y=Type, fill=Type)) + 
  geom_point()

# Die size: Summary
cpu_gpu_data %>% group_by(Type) %>% summarize(mean = mean(Die_Size_mm2),
  sd = sd(Die_Size_mm2), median = median(Die_Size_mm2), max = max(Die_Size_mm2),
  min = min(Die_Size_mm2))

# Die size: Boxplot
cpu_gpu_data %>% ggplot(aes(x=Type, y=Die_Size_mm2, fill=Type)) + 
  geom_boxplot() + coord_flip()


# TDP vs Die Size (CPU)
cpu_gpu_data[cpu_gpu_data$Type == 'CPU',] %>% 
  ggplot(aes(x=TDP_W, y=Die_Size_mm2)) + 
  geom_point(col = "dodgerblue", alpha = .5) +
  ggtitle("TDP & Die Size (CPU)")

# Pearson coefficient TDP vs Die Size (CPU)
tmp_cpu <- cpu_gpu_data[cpu_gpu_data$Type == 'CPU',]
cor(tmp_cpu$TDP_W, tmp_cpu$Die_Size_mm2, method = "pearson")

# TDP vs Die Size (GPU)
cpu_gpu_data[cpu_gpu_data$Type == 'GPU',] %>% 
  ggplot(aes(x=TDP_W, y=Die_Size_mm2)) + 
  geom_point(col = "darkseagreen", alpha = .3) +
  coord_flip() +
  ggtitle("TDP & Die Size (GPU)")

# Pearson coefficient TDP vs Die Size (GPU)
tmp_gpu <- cpu_gpu_data[cpu_gpu_data$Type == 'GPU',]
cor(tmp_gpu$TDP_W, tmp_gpu$Die_Size_mm2, method = "pearson")


# Transistors: Summary
cpu_gpu_data %>% group_by(Type) %>% summarize(mean = mean(Transistors_million),
  sd = sd(Transistors_million), median = median(Transistors_million), max = max(Transistors_million),
  min = min(Transistors_million))

# Transistors: Boxplot
cpu_gpu_data %>% ggplot(aes(x=Type, y=Transistors_million, fill=Type)) + 
  geom_boxplot() + coord_flip() +
  scale_y_continuous(limits = quantile(cpu_gpu_data$Transistors_million, c(0, 0.97)))

# Transistor outliers
cpu_gpu_data[cpu_gpu_data$Transistors_million > 25000,]$Release_Date
unique(cpu_gpu_data[cpu_gpu_data$Transistors_million > 25000,]$Type)
unique(cpu_gpu_data[cpu_gpu_data$Transistors_million > 25000,]$Vendor)
unique(cpu_gpu_data[cpu_gpu_data$Transistors_million > 25000,]$Foundry)

# Frequency: Summary
cpu_gpu_data %>% group_by(Type) %>% summarize(mean = mean(Freq_MHz),
  sd = sd(Freq_MHz), median = median(Freq_MHz), max = max(Freq_MHz),
  min = min(Freq_MHz))

# Frequency: density
cpu_gpu_data %>% ggplot(aes(x=Freq_MHz, fill=Type)) + 
  geom_density(alpha=.5)

# FP16_GFLOPS: Summary
cpu_gpu_data[cpu_gpu_data$FP16_GFLOPS > 0,] %>% summarize(mean = mean(FP16_GFLOPS),
  sd = sd(FP16_GFLOPS), median = median(FP16_GFLOPS), max = max(FP16_GFLOPS),
  min = min(FP16_GFLOPS))
cpu_gpu_data[cpu_gpu_data$FP32_GFLOPS > 0,] %>% summarize(mean = mean(FP32_GFLOPS),
  sd = sd(FP32_GFLOPS), median = median(FP32_GFLOPS), max = max(FP32_GFLOPS),
  min = min(FP32_GFLOPS))
cpu_gpu_data[cpu_gpu_data$FP64_GFLOPS > 0,] %>% summarize(mean = mean(FP64_GFLOPS),
  sd = sd(FP64_GFLOPS), median = median(FP64_GFLOPS), max = max(FP64_GFLOPS),
  min = min(FP64_GFLOPS))


cpu_gpu_data %>% mutate(Foundry = case_when(
  sum(cpu_gpu_data[cpu_gpu_data$Foundry < 100,])
))

table(cpu_gpu_data$Foundry)

# NVIDIA Foundry preferences
cpu_gpu_data %>% 
  ggplot(aes(x=Foundry, fill=Type)) +
  geom_bar(stat="count") + ggtitle("Foundry Preference") +
  facet_wrap(~Vendor)

# Intel Foundry preferences
p + cpu_gpu_data[cpu_gpu_data$Vendor == "Intel",] %>% ggplot(aes(x=Foundry, fill=Type)) +
  geom_bar(stat="count") + ggtitle("Intel Foundry Preference")

# AMD Foundry preferences
p + cpu_gpu_data[cpu_gpu_data$Vendor == "AMD",] %>% ggplot(aes(x=Foundry, fill=Type)) +
  geom_bar(stat="count") + ggtitle("AMD Foundry Preference")

p

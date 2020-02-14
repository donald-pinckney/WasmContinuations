#!/usr/local/bin/Rscript

library(ggplot2)

data <- read.csv('results.csv')
data <- data[!(names(data) %in% c("Build.command", "Exec.command"))]
data <- data[data['Runtime'] != "clang" & data["Runtime"] != "emcc",]
data <- droplevels.data.frame(data)

data$Benchmark <- factor(data$Benchmark, levels = c('pi_no_thread', 'hailstone', 'hailstone_rec'))
data$Runtime <- factor(data$Runtime, levels = c('clang -O3', 'Go', 'emcc -O3', 'Toy', 'Go Wasm', 'Toy Heap'))

for (bench in levels(data$Benchmark)) {
  cl_time <- data[data$Runtime == "clang -O3" & data$Benchmark == bench, 'Time']
  data[data$Benchmark == bench, 'Relative.Time'] <- data[data$Benchmark == bench, 'Time'] / cl_time
}

p <- ggplot(data, aes(fill=Runtime, y=Relative.Time, x=Benchmark)) +
  geom_col(position="dodge") +
  labs(y="Time relative to clang -O3")

print(p)

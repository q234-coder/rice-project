library(readr)
library(dplyr)
library(ggplot2)
library(agricolae)

dat <- read_csv("data/rice_nitrogen.csv")
dat$Treatment <- factor(dat$Treatment, levels = c("N0","N1","N2","N3"))

# 描述统计
desc <- dat %>%
  group_by(Treatment) %>%
  summarise(
    mean_ht = mean(Plant_Height),
    sd_ht = sd(Plant_Height),
    mean_spad = mean(SPAD),
    sd_spad = sd(SPAD),
    mean_yield = mean(Yield),
    sd_yield = sd(Yield)
  )
write_csv(desc, "output/水稻氮素描述统计.csv")

# 方差分析与多重比较
fit_ht <- aov(Plant_Height ~ Treatment, dat)
ht_lsd <- LSD.test(fit_ht, "Treatment")
capture.output(ht_lsd$groups, file="output/株高多重比较.txt")

fit_spad <- aov(SPAD ~ Treatment, dat)
spad_lsd <- LSD.test(fit_spad, "Treatment")
capture.output(spad_lsd$groups, file="output/SPAD多重比较.txt")

fit_yield <- aov(Yield ~ Treatment, dat)
yield_lsd <- LSD.test(fit_yield, "Treatment")
capture.output(yield_lsd$groups, file="output/产量多重比较.txt")

# 绘图
p1 <- ggplot(dat, aes(Treatment, Yield, fill=Treatment)) +
  geom_col(color="black") +
  ggtitle("不同施氮量对水稻产量的影响") +
  theme_bw()
ggsave("output/产量柱状图.png", p1, width=7, height=4, dpi=300)

p2 <- ggplot(dat, aes(Treatment, SPAD, fill=Treatment)) +
  geom_boxplot() +
  ggtitle("不同施氮量对SPAD的影响") +
  theme_bw()
ggsave("output/SPAD箱线图.png", p2, width=7, height=4, dpi=300)

message("✅ 全部分析完成！")
library(dplyr)
library(readr)

library(ggplot2)
library(ggmap)
library(Cairo)
library(rgdal)
library(broom)

aadt <- read_csv("data/TxDOT_AADT.csv") %>%
  filter(grepl("smith", T_CNTY_NM, ignore.case = TRUE)) %>%
  select(
    T_FLAG,
    X,
    Y,
    # F2007_TRAF,
    # F2008_TRAF,
    # F2009_TRAF,
    # F2010_TRAF,
    # F2011_TRAF,
    # F2012_TRAF,
    F2013_TRAF,
    F2014_TRAF,
    F2015_TRAF
  )

prelim2016 <- read_csv("data/smith_traffic_2016.csv") %>%
  select(
    T_FLAG,
    F2016
  )

joined <- aadt %>%
  full_join(prelim2016, by = "T_FLAG") %>%
  filter(!is.na(F2013_TRAF), !is.na(F2016)) %>%
  mutate(
    change_3y = F2016 - F2013_TRAF,
    avg_change_3y = change_3y / 3,
    pct_change_3y = change_3y / F2013_TRAF
  ) %>%
  arrange(desc(avg_change_3y))



joined$change_3y_quantile <- cut(joined$change_3y, include.lowest=FALSE,  right=TRUE,
                                 breaks=c(-12411, -336.666666666667, 41.3333333333333, 348, 845.333333333333, 2161.66666666667, 11070))


joined$avg_change_3y_quantile <- cut(joined$avg_change_3y, include.lowest=FALSE,  right=TRUE,
                                     breaks=c(-4137, -112.222222222222, 13.7777777777778, 116, 281.777777777778, 720.555555555555, 3690))



joined$pct_change_3y_quantile <- cut(joined$pct_change_3y, include.lowest=FALSE,  right=TRUE,
                                     breaks=c(-0.986028708133971, -0.0616962676857024, 0.0177397096921281, 0.100123001230012, 0.169342991668827, 0.23823810176815, 1.70985259891389))


## Cutting joined$change_3y into joined$change_3y_grouped
joined$change_3y_grouped <- cut(joined$change_3y, include.lowest=FALSE,  right=TRUE,
                                breaks=c(-15000, -5000, -2500, 0, 2500, 5000, 15000))

streets <- readOGR(dsn = "data/centerline/Centerline.shp", "Centerline")
streets_wsg84 <- spTransform(streets, CRS("+proj=longlat +datum=WGS84"))
streets_data <- as.data.frame(streets) %>%
  select(FULLNAME, ROADCLASS)

streets_df <- merge(tidy(streets_wsg84), streets_data, by.x = "id", by.y = 0) %>%
  filter(ROADCLASS %in% c("Freeway", "Highway", "Major Arterial", "Minor Arterial"))

# top <- slice(joined, 1:20)
top <- filter(joined, change_3y >= 3000)

xlim = c(-95.4, -95.2)
ylim = c(32.25, 32.43)

lims = SpatialPoints(
  coords = data_frame(x = xlim, y = ylim),
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

p <- ggplot() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  ) +
  geom_path(data = streets_df, aes(x = long, y = lat, group = group, size = ROADCLASS, color = ROADCLASS)) +
  scale_size_manual(values = c(0.75, 0.75, 0.5, 0.5)) + 
  scale_color_manual(values = c("#333333", "#333333", "#666666", "#666666")) +
  geom_point(data = top, aes(X, Y), size = 3) +
  coord_fixed(xlim = coordinates(lims)[,1], ylim = coordinates(lims)[,2])
  
  # geom_text(data = joined, aes(X, Y, label = T_FLAG), size=3, hjust = 0)

p
ggsave(p, file = "map.svg", width = 6, height = 4.5)



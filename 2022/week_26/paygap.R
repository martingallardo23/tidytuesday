library(tidyverse)
library(tidytuesdayR)
library(extrafont)
# font_import()
loadfonts()

tuesdata <- tidytuesdayR::tt_load(2022, week = 26)

paygap <- tuesdata$paygap

sicdata <- read_csv("data/uk-sic-2007-condensed.csv") |> 
  mutate(sic_code = str_pad(sic_code, 5, "left", 0)) 

levels <- c("Less than 250",
            "250 to 499", "500 to 999", "1000 to 4999",
            "5000 to 19,999", "20,000 or more")

labels <- c("<250",
            "250 - 499", "500 - 999", "1,000 - 4,999",
            "5,000 - 19,999", ">20,000")

paygap <- paygap |> 
  filter(employer_size != "Not Provided") |> 
  mutate(sics          = str_split(sic_codes,pattern = ":"),
         employer_size = factor(employer_size, 
                                levels = levels,
                                labels = labels)) |> 
  unnest(sics) |> 
  mutate(sics = str_pad(sics, 5, "left", 0)) |> 
  inner_join(sicdata, by = c("sics" = "sic_code"))

plotdata <- paygap |> 
  group_by(section_description, employer_size) |>
  summarise(mean = mean(diff_mean_hourly_percent, na.rm = TRUE)) |> 
  rowwise() |> 
  mutate(section_label = 
           case_when(section_description == 
                       "Water supply, sewerage, waste management and" ~
                       "Water supply, sewerage, waste management",
                     section_description == 
                       "Activities of extraterritorial organisations and" ~
                       "Activities of extraterritorial organisations",
                     TRUE ~ section_description),
         section_label = section_label |> 
           str_remove(";.*") |> 
           strwrap(25) |> 
           paste(collapse = '\n'))

gg <- plotdata |> 
  ggplot(aes(x = mean, y = employer_size, color = mean)) + 
  geom_segment(aes(x = -35,           xend = 35, 
                   y = employer_size, yend = employer_size),
               size = 0.2, color = "#F3D8BA") +
  geom_point() +
  geom_vline(xintercept = 0, alpha = 0.3) +
  geom_segment(aes(x = 0,             xend = mean, 
                   y = employer_size, yend = employer_size)) +
  geom_label(data = subset(plotdata, mean > 0), 
            aes(x = mean + 14, label = paste0("+", round(mean,1),"%"),
                y = employer_size, color = "red"), 
            size = 1.6, color = "#433B37", fill = "#FCF5EE", 
            family = "Fira Sans", inherit.aes = FALSE, label.size = NA) +
  geom_label(data = subset(plotdata, mean < 0), 
             aes(x = mean - 14, label = paste0(round(mean,1),"%"),
                 y = employer_size, color = "red"), 
             size = 1.6, color = "#917C78", fill = "#FCF5EE", 
             family = "Fira Sans", inherit.aes = FALSE, label.size = NA) +
  facet_wrap( ~ section_label, ncol = 4)  +
  scale_color_gradient(low = "#F57E00", high = "#419D78",
                       limits = c(-5, 35),
                       breaks = c(-5, 35),
                       labels = c("Women earn\nmore", "Men earn\nmore"),
                       name   = "Mean difference in hourly\npay",
                       guide  = guide_colorbar(title.position = "top",
                                               breaks         = c(-5, 35),
                                               barheight      = 0.5,
                                               barwidth       = 10)) +
  scale_x_continuous(breaks = c(-20, 20), labels = c("Women earn\nmore", 
                                                     "Men earn\nmore")) +
  coord_cartesian(xlim = c(-40, 40), clip = "off") +
  theme_void() +
  labs(y        = "Employer Size",
       title    = "Pay gap in UK industries",
       subtitle = "Mean difference in hourly pay between men and women, by employer\nsize and industry",
       caption  = "github.com/martingallardo23") +
  theme(plot.background    = element_rect(fill = "#FCF5EE", color = NA),
        plot.margin        = margin(30, 70, 30, 30),
        plot.title         = element_text(family = "Source Serif Pro",
                                          face   = "bold",
                                          color  = "#2D3047",
                                          size   = 30,
                                          hjust  = 0.5,
                                          margin = margin(0, 0, 10, 0)),
        plot.subtitle      = element_text(family = "Source Serif Pro",
                                          hjust  = 0.5,
                                          margin = margin(0, 0, 10, 0)),
        plot.caption       = element_text(family = "Source Serif Pro Light",
                                          hjust  = 0.5,
                                          vjust  = 0),
        legend.position    = c(0.66, 0.06),
        legend.direction   = "horizontal",
        legend.title.align = 0.5,
        legend.text        = element_text(family = "Fira Sans"),
        legend.title       = element_text(family = "Fira Sans",
                                          face   = "bold",
                                          color  = "#2D3047",
                                          size   = 10),
        strip.text.x       = element_text(size  =  7, family = "Fira Sans SemiBold"),
        axis.text.y        = element_text(size  =  6, family = "Fira Sans"),
        axis.text.x        = element_text(size  =  4, family = "Fira Sans"),
        axis.title.y       = element_text(angle = 90, family = "Fira Sans",
                                          margin = margin(0, 5, 0, 0)))

width  <- 1750
height <- 2000

ggsave("plot/paygap.png", gg, width = width, height = height, units = "px",
       dpi = 250)

ggsave("plot/paygap.pdf", gg, width = width, height = height, units = "px",
       dpi = 250, device = cairo_pdf)

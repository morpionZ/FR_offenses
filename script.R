#+ license, echo=FALSE
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("xlsx")
require("data.table")
require("dplyr")
require("animation")
require("ggplot2")
if (!require("graphZoo")) {
  require("devtools")
  install_github("morpionZ/graphZoo")
  require("graphZoo")
}
require("extrafont")
loadfonts()


#+ download.raw.data, echo=FALSE
if (!file.exists("data/2013-fc-pn.xlsx")) {
  download.file("http://www.data.gouv.fr/storage/f/2014-05-27T17-49-23/2013-fc-pn.xlsx",
                destfile = "data/2013-fc-pn.xlsx")
}
if (!file.exists("data/2013-fc-gn.xlsx")) {
  download.file("http://www.data.gouv.fr/storage/f/2014-05-27T18-31-21/2013-fc-gn.xlsx",
                destfile = "data/2013-fc-gn.xlsx")
}
if (!file.exists("data/population.xls")) {
  download.file("http://www.insee.fr/fr/ppp/bases-de-donnees/recensement/populations-legales/pages2013/xls/ensemble.xls",
                destfile = "data/population.xls")
}


#+ load.data, echo=FALSE
dep.names <- read.xlsx2("data/2013-fc-pn.xlsx", sheetIndex = 1, 
                        startRow = 1, endRow = 1, header = FALSE)
dep.names <- enc2native(as.vector(as.matrix(dep.names[4:108])))

if (file.exists("data/combined.csv")) {
  off.data <- fread("data/combined.csv")
} else {
  off.data <- data.table()
  
  for (i in 1:12) {
    raw1 <- read.xlsx2("data/2013-fc-pn.xlsx", sheetIndex = i,
                       startRow = 1, endRow = 104)
    procd1 <- data.table(YEAR = year(as.POSIXct(raw1$dat, format = "%d/%m/%Y")),
                         MONTH = month(as.POSIXct(raw1$dat, format = "%d/%m/%Y")),
                         OFFENSE.ID = raw1$infract,
                         OFFENSE.NAME = raw1$Désignation.de.l.infraction,
                         REPORT.BY = "Police",
                         DEPARTMENT = enc2native(rep(dep.names, each = nrow(raw1))),
                         COUNT = as.numeric(as.vector(as.matrix(raw1[,4:108]))))
    
    raw2 <- read.xlsx2("data/2013-fc-gn.xlsx", sheetIndex = i,
                       startRow = 1, endRow = 104)
    procd2 <- data.table(YEAR = year(as.POSIXct(raw1$dat, format = "%d/%m/%Y")),
                         MONTH = month(as.POSIXct(raw1$dat, format = "%d/%m/%Y")),
                         OFFENSE.ID = raw1$infract,
                         OFFENSE.NAME = raw1$Désignation.de.l.infraction,
                         REPORT.BY = "Gendarmerie",
                         DEPARTMENT = enc2native(rep(dep.names, each = nrow(raw1))),
                         COUNT = as.numeric(as.vector(as.matrix(raw1[,4:108]))))
    
    off.data <- rbind(off.data, procd1)
    off.data <- rbind(off.data, procd2)
  }
  
  pop <- read.xlsx2("data/population.xls", sheetIndex = 2,
                    startRow = 8, endRow = 108) %>%
    group_by(Nom.du.département) %>%
    mutate(DEPARTMENT = ifelse(!is.na(match(Nom.du.département, dep.names)), 
                               dep.names[match(Nom.du.département, dep.names)],
                               dep.names[which.min(adist(Nom.du.département, dep.names))])) %>%
    ungroup() %>%
    mutate(POPULATION = as.numeric(as.character(Population.totale))) %>%
    select(DEPARTMENT, POPULATION)
    
  off.data <- merge(off.data, pop, by = "DEPARTMENT")
  
  write.csv(off.data, "data/combined.csv", row.names = FALSE)
}


# create.html.animation, echo=FALSE
france.map <- as.data.table(map_data('france', project="albers", par = c(39, 45))) %>%
  group_by(region) %>%
  mutate(DEPARTMENT = ifelse(!is.na(match(region, dep.names)), 
                             dep.names[match(region, dep.names)],
                             dep.names[which.min(adist(region, dep.names))]))

offense.id <- unique(off.data$OFFENSE.ID)

saveHTML({
  for (i in 1:length(offense.id)) {
    tmp <- filter(off.data, OFFENSE.ID == offense.id[i]) %>%
      group_by(DEPARTMENT) %>%
      summarize(PERCAP = (sum(COUNT) / POPULATION[1]) * 100000) %>%
      ungroup() %>%
      merge(france.map, by = "DEPARTMENT")
    
    ti <- as.character(filter(off.data, OFFENSE.ID == offense.id[i])$OFFENSE.NAME[1])
    ti.words <- strsplit(ti, " ")[[1]]
    if (length(ti.words) > 8) {
      tmp1 <- paste(ti.words[1:6], collapse = " ")
      tmp2 <- paste(ti.words[7:length(ti.words)], collapse = " ")
      ti <- paste0(tmp1, "\n", tmp2)
    } else {
      ti <- paste0(ti, "\n")
    }
    
    g <- ggplot() +
      geom_polygon(data = tmp, 
                   aes(x = long, y = lat, group = group, fill = PERCAP),
                   color = "white") + 
      coord_fixed() +
      theme_graphzoo(base_size = 18, family = "Open Sans") +
      theme(axis.line = element_blank(), 
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(), 
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(1, 0, 0, -1), "lines"),
            legend.position = "bottom",
            legend.title.align = 0.5) +
      scale_fill_gradient(low = "dodgerblue", high = "red",
                          guide = guide_colorbar(title = element_text("Nombre d'infractions pour 100 000 habitants en 2013", hjust = 1),
                                                 label.position = "bottom",
                                                 title.position = "top",
                                                 barwidth = 25)) +
      ggtitle(ti)
    
    g <- addBanner(g, font.size = 5,
                   l.txt = "GRAPHZOO.TUMBLR.COM", 
                   r.txt = "SOURCE: DATA.GOUV.FR, INSEE")
    
    png(sprintf(ani.options('img.fmt'), i), width = 600, height = 600, bg = "#F0F0F0")
    print(g)
    dev.off()    
  }
}, img.name = "graph", imgdir = "animation", htmlfile = "animation.html", 
outdir = getwd(), autobrowse = FALSE, use.dev = FALSE, interval = 2,
verbose = FALSE, autoplay = FALSE, title = "La France du crime",
single.opts = "'width': 600")



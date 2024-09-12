library(haven)
library(ggplot2)
library(directlabels)
library(plotly)

ggplot(trvd, aes(x = WEEK, y = PCBSD, color = SUBJECT)) +
  geom_line(aes(color = SUBJECT)) +
  geom_point(aes(shape=OVRLRESP, size=SOD)) +
  geom_point(aes(fill=NLR), size=3, shape=21)+
  geom_dl(aes(x = WEEK,
              y = PCBSD, label = SUBJECT),
          color="black",
          method = list("last.points", cex = 0.7, rot = -25, hjust = -.3)) +
  coord_cartesian(xlim=c(0,55), ylim=c(-95,20)) +
  scale_size_continuous(range = c(4, 8))+
  scale_fill_manual(values=c("white","black","yellow2")) +
  labs(y="% Change SOD from Baseline",
       x="Weeks",
       title="1. Spider Plot for irResponder") +
  theme(legend.title = element_text(color = "black", size = 8, face="bold"),
        legend.text = element_text(color = "blue", size = 7),
        legend.position = 'right',
        legend.spacing.y = unit(0.01, 'cm'))

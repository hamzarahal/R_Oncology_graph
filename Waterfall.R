ggplot(subjw, aes(x = SUBJECT, y = BESTPCHG, col = SUBJECT)) +
  labs(title = "7. Waterfall Plot for Best Percent Change SOD from Baseline in Tumor Size(with BOR)",
       x = "Subjects",
       y = "Best Percent Change from Baseline") +
  coord_cartesian(ylim = c(-85,90)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_abline(slope=0, intercept=20, col = "red", lty=2) +
  geom_abline(slope=0, intercept=-30, col = "red", lty=2) +
  geom_text(aes(x = SUBJECT,
                y = BESTPCHX, label=BOR),
            color="black", size=3) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position="none")
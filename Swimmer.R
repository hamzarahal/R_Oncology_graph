#3a
ggplot(trvd, aes(x = WEEK, y = SUBJECT)) +
  geom_line()+
  geom_point(aes(color = OVRLRESP, size=SOD)) +
  geom_text(aes(label=NLR), color="black") +
  labs(y="Subject Who Has irPR but Not RECIST Responder",
       x="Weeks",
       title="3a. Swimmer Plot irResponder(New Lesion by Text)",
       caption = "New Lesion: Y, N") +
  scale_size_continuous(range = c(4, 8)) +
  coord_cartesian(xlim=c(0,50)) +
  6
theme_bw()
#3b
ggplot(trvd, aes(x = WEEK, y = SUBJECT)) +
  geom_line()+
  geom_point(aes(color = NLR ), size=7) +
  geom_text(aes(label=OVRLRESP))+
  scale_colour_discrete(name="NLR", labels=c("Screen", "N", "Y")) +
  labs(y="Subjects who are irRECIST Responders",
       x="Weeks",
       title="3b. Swimmer Plot for irResponders(New Lesion by Color)")+
  theme_bw() +
  guides(color=guide_legend("NEW LESION"))
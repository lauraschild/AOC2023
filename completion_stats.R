lines <- readLines("https://adventofcode.com/2023/stats")

lines <- lines[98:121]

numbers <- lapply(lines,
                  function(x)parse_number(unlist(strsplit(x,'"'))[c(3,5,7)]))

stats <- do.call(rbind,numbers) %>% 
  as.data.frame()

names(stats) <- c("Day","Gold","Silver")

stats %>% 
  filter(Day != 0) %>% 
  group_by(Day) %>% 
  mutate(total = Gold + Silver) %>% 
  pivot_longer(cols = 2:3,
               names_to = "level",
               values_to = "users") %>% 
  filter(level == "Gold") %>% 
  ggplot(aes(x = factor(Day),
             y= users/total,
             fill= total))+
  geom_bar(stat = "identity")+
  scico::scale_fill_scico(palette = "batlow")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "fraction of users that completed task two",
       x = "Day",
       y = "fraction of gold stars",
       fill = "total users")

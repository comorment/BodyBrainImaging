
alpha_p <- 0.5
size_p <-2

plot_scatter_density <- function 


# Plot density / scatter

g_density <- ggplot(df.in, aes(x = TTMV.2.0, fill = sex)) + geom_density(alpha=0.2) + 
  xlab("total thigh muscle volume")+theme_classic() + theme(legend.position = "none") # +  theme(legend.position = c(0.8,1)) +
 # scale_color_discrete(name = "Sex", labels = c("Women", "Men"))
g_density

g_scatter <- ggplot(df.in, aes(x=age.2.0, y = TTMV.2.0, color = sex)) + 
  geom_point(alpha = alpha_p, size=size_p) +
  theme_classic()   + # theme(legend.position = c(0.8,1))+
  labs(y="total thigh muscle volume", x="age")+
  scale_color_discrete(name = "sex", labels = c("women", "men")) 
g_scatter


g <- ggarrange(g_scatter,g_density, nrow = 2, ncol = 1)
ggsave(file="pics/TTMV.pdf",plot = g, 
       dpi = 300, units = "in", height = 5, width = 5)


# Plot density / scatter

g_density <- ggplot(df.in, aes(x = VAT.2.0, fill = sex)) + geom_density(alpha=0.2) + 
  xlab("visceral adipose tissue")+theme_classic() + theme(legend.position = "none") # +  theme(legend.position = c(0.8,1)) +
# scale_color_discrete(name = "Sex", labels = c("Women", "Men"))
g_density

g_scatter <- ggplot(df.in, aes(x=age.2.0, y = VAT.2.0, color = sex)) + 
  geom_point(alpha = alpha_p, size=size_p) +
  theme_classic()   + # theme(legend.position = c(0.8,1))+
  labs(y="visceral adipose tissue", x="age")+
  scale_color_discrete(name = "sex", labels = c("women", "men")) 
g_scatter


g <- ggarrange(g_scatter,g_density, nrow = 2, ncol = 1)
ggsave(file="pics/VAT.pdf",plot = g, 
       dpi = 300, units = "in", height = 5, width = 5)



g_density <- ggplot(df.in, aes(x = ASAT.2.0, fill = sex)) + geom_density(alpha=0.2) + 
  xlab("subcutaneous adipose tissue")+theme_classic() + theme(legend.position = "none") # +  theme(legend.position = c(0.8,1)) +
# scale_color_discrete(name = "Sex", labels = c("Women", "Men"))
g_density

g_scatter <- ggplot(df.in, aes(x=age.2.0, y = ASAT.2.0, color = sex)) + 
  geom_point(alpha = alpha_p, size=size_p) +
  theme_classic()   + # theme(legend.position = c(0.8,1))+
  labs(y="subcutaneous adipose tissue", x="age")+
  scale_color_discrete(name = "sex", labels = c("women", "men")) 
g_scatter

g <- ggarrange(g_scatter,g_density, nrow = 2, ncol = 1)
ggsave(file="pics/ASAT.pdf",plot = g, 
       dpi = 300, units = "in", height = 5, width = 5)



g_TTMV <- ggplot(df.in, aes(x=TotalGrayVol, y = TTMV.2.0, color = sex)) + 
  geom_point(alpha = alpha_p, size=size_p) +
  theme_classic()   + theme(legend.position = "none") +# theme(legend.position = c(0.8,1))+
  labs(y="Total gray matter volume", x="total thigh muscle volume")#+

g_vat <- ggplot(df.in, aes(x=TotalGrayVol, y = VAT.2.0, color = sex)) + 
  geom_point(alpha = alpha_p, size=size_p) +
  theme_classic()   + theme(legend.position = "none") +# theme(legend.position = c(0.8,1))+
  labs(y="Total gray matter volume", x="visceral adipose tissue")#+
#  scale_color_discrete(name = "sex", labels = c("women", "men")) 

g_asat <- ggplot(df.in, aes(x=TotalGrayVol, y = ASAT.2.0, color = sex)) + 
  geom_point(alpha = alpha_p, size=size_p) +
  theme_classic()   + # theme(legend.position = c(0.8,1))+
  labs(y="Total gray matter volume", x="subcutaneous adipose tissue")+
  scale_color_discrete(name = "sex", labels = c("women", "men"))

g <- ggarrange(g_TTMV,g_vat, g_asat, nrow = 1, ncol = 3)
ggsave(file="pics/TotalGrayMatterVol.pdf",plot = g, 
       dpi = 300, units = "in", height = 5, width = 15)



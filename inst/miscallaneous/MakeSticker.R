#library(hexSticker)
imgurl <- system.file("figures/world.png", package="CompInt")
s<-sticker(imgurl, package="CompInt",
           p_size=22, p_y=0.5, p_family = "Aller_Rg", p_color = "#1881C2",
           s_x=1, s_y=1.15,
           h_fill="#EFFEFF",h_color = "#1881C2",
           s_width=.6)
plot(s)

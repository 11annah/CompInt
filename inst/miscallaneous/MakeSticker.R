#library(hexSticker)
imgurl <- system.file("figures/world.png", package="CompInt")
s<-sticker(imgurl, package="CompInt",
           p_size=75, p_y=0.5, p_x=0.98 , p_family = "wqy-microhei", p_color = "#1C75BC", ##27AAE1
           s_x=1, s_y=1.175,
           h_fill="#EFFEFF",h_color = "#1C75BC",
           s_width=.6,
           dpi=1000,
           filename = "man/figures/logo.png")
plot(s)

plot(viz1,
     vertex.label = NA,
     vertex.size = 7,
     edge.width = 0.2,
     edge.arrow.size = 0.2)

install.packages('ggraph')
library(ggraph)

igraph::transitivity(viz1)
     
huhhh <- ggraph(df_no_iso_w1,
      layout = "fr") +
      geom_edge_link(color = "gray", width = 0.5) +
      geom_node_point(aes(size = indegree, color = h_index, 
                          shape = scale_size(range = c(3, 10)))) + 
      scale_color_gradient(low = "red", high = "blue") + 
      theme_void()

ggraph(net_no_iso_w1,
       layout = "fr") +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), end_cap = circle(1, 'mm'), color = "gray", width = 0.5) +
  geom_node_point(size = 3, aes(color = df_no_iso_w1$h_index, shape = as.factor(df_no_iso_w1$bigleague))) + 
  scale_size(range = c(3, 10)) + 
  scale_color_gradient(low = "red", high = "blue") + 
  theme_void()

table(df_no_iso_w1$h_index)
dim(df_no_iso_w1)

plot(model1gof1)
plot(model1gof2)
plot(model1gof3)

plot(model2gof1)
plot(model2gof2)
plot(model2gof3)

plot(model3gof1)
plot(model3gof2)
plot(model3gof3)

plot(model4gof1)
plot(model4gof2)
plot(model4gof3)

plot(model5gof1)
plot(model5gof2)
plot(model5gof3)

df_result3$ttest
df_result4$ttest
df_results5$ttest

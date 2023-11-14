

# 1 Define fonts for the charts ---------------------------------------------

    # Define fonts
          t <- list(
            family = "Arial",
            size = 12
          )
          
          
          t_8 <- list(
            family = "Arial",
            size = 8#,
            #face="bold"
          )
          
          t_10 <- list(
            family = "Arial",
            size = 10,
            face="bold"
          )
          
          # Define fonts
          t_11 <- list(
            family = "Arial",
            size = 11,
            face="bold"
          )
          
          
          t_16 <- list(
            family = "Arial",
            size = 16,
            face="bold"
          )
          
          
          # Define custom colors
          colors <- c('#1f77b4', '#ff7f0e')
          
          # Original colors
          colr <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#17becf', '#57a9e2',
                             '#ffb574', '#5fd35f', '#7f7f7f', '#e77c7c', '#c6aedc', '#bcbd22',
                             '#bc8b81', '#f4cce8', '#b2b2b2', '#9467bd', '#e2e362', '#e377c2',
                             '#5fe0ed', '#8c564b', '#103d5d', '#a74e00')
                             
          # Generate additional unique colors
          additional_colors <- colorRampPalette(c("#00FF00", "#FF0000"))(102 - length(colr))
          
          # Combine the original and additional colors
          colr <- c(colr, additional_colors)




# 2.Setting WB-LOGO -----------------------------------------------------------------

local_image_path <- paste(path, "wb-short.png", sep = "/")

# Step 2: Encode the local image as base64
base64_image <- dataURI(file = local_image_path, mime = "image/png")



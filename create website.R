require(blogdown)

blogdown::new_site(theme = "MarcusVirg/forty",
                   sample = TRUE,
                   theme_example = TRUE,
                   empty_dirs = TRUE,
                   to_yaml = TRUE)

# preview webbsite in viewer prompt
blogdown::serve_site()

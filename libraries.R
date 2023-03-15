install.packages(c('Rlab',
                    'dplyr',       # Data manipulation (0.8.0.1)
                    'fBasics',     # Summary statistics (3042.89)
                    'corrplot',    # Correlations (0.84)
                    'psych',      # Correlation p-values (1.8.12)
                    'grf',       # Generalized random forests (0.10.2)
                    'rpart',      # Classification and regression trees, or CART (4.1-13)
                    'rpart.plot',
                    'treeClust',  
                    'car',       
                    'remotes',    
                    'readr',       
                    'tidyr',       # Database operations (0.8.3)
                    'tibble',      # Modern alternative to data frames (2.1.1)
                    'knitr',       # RMarkdown (1.21)
                    'kableExtra',  # Prettier RMarkdown (1.0.1)
                    'ggplot2',     # general plotting tool (3.1.0)
                    'haven',       # read stata files (2.0.0)
                    'aod',         # hypothesis testing (1.3.1)
                    'evtree',      # evolutionary learning of globally optimal trees (1.0-7)
                    'estimatr'), repos = "http://cran.us.r-project.org")
                    
remotes::install_github('susanathey/causalTree') # Uncomment this to install the causalTree package
remotes::install_github('grf-labs/sufrep') # Uncomment this to install the sufrep package



library(devtools)
install_github('terraref/traits')

install_github('daattali/addinslist')

lapply(c('~/Team_1/doc', '~/Team_1/src', '~/Team_1/results', '~/Team_1/data'), dir.create)  
lapply(c('~/Team_1/data/raw_data_csv', '~/Team_1/results/plots'), dir.create)

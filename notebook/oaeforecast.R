if (!require("xfun")) install.packages("xfun")
pkgs <- c(
  'forecast', 'vars', 'urca', 'MLmetrics', 
  'lubridate', 'tsbox', 'timetk',
  'extrafont', 'patchwork', 'hrbrthemes', 'ggthemes', 'ggsci', 'scales', 
  'tidyverse', 'vroom', 'readxl', 'writexl', 'tsibble'
)
xfun::pkg_attach2(pkgs, message = FALSE)



library(tidyverse)
options("scipen"=100, "digits"=4)
photo_dir <- './test_dir/'
args <- paste("-common -ScaleFactor35efl -FocalLengthIn35mmFormat -FNumber -DateTimeOriginal -FileType -csv -t -r", photo_dir)
system2("exiftool",
        args = args,
        stdout = "photodata.csv")


data <- read_csv("photodata.csv") %>%
  select(-Quality) %>%
  mutate(across(.cols = starts_with('FocalLength'),~ str_remove(., " mm"))) %>%
  mutate(across(.cols = starts_with('FocalLength'), ~ as.numeric(.) %>% round(1))) %>%
  mutate(ShutterSpeed = ifelse(str_detect(ShutterSpeed, '/'), 
                               str_remove(ShutterSpeed, '1/(?=[:digit:])') %>% as.numeric() %>% .**(-1), 
                               as.numeric(ShutterSpeed))) %>%
  mutate(ShutterSpeed = round(ShutterSpeed, 5)) %>%
  mutate(LensType = cut(FocalLengthIn35mmFormat, c(0,35,80,135,400)) %>% factor(labels = c("Wide","Normal","Portrait","Telephoto"))) %>%
  separate(DateTimeOriginal, sep = ' ', c('Date', 'Time')) %>%
  mutate(Date = as.Date(Date, tryFormats = '%Y:%m:%d'))
  

  

levels(lensType) <- c("Wide","Normal","Portrait","Telephoto")
str(data)
table(complete.cases(data))

# which not complete cases

# option to remove complete cases

ggplot(data) + 
  geom_histogram(aes(x = FocalLengthIn35mmFormat), binwidth = 5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_classic()
#useful package and command
HH9_4 %>%
  filter(isRetweet==FALSE)%>%
  rowwise()  %>%
  mutate(houston = grepl("Houston|HTX", text, ignore.case=TRUE) %>%
           filter(houston==TRUE)
#
ggplot
gmcall
libbon
ggmap

no_state$city %in% city_ystate$city
left_join()

readin

location <- data.frame(loc = location[!is.na(location)], stringsAsFactors =  F) %>%
  group_by(loc) %>%
  tally() %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  mutate(longi = NA, latti = NA)
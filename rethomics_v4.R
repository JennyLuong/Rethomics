######## Get necessary packages ##########
{
  if (!require(lubridate, quietly=T)) {install.packages('lubridate');library(lubridate)}
  if (!require(dplyr, quietly=T)) {install.packages('dplyr');library(dplyr)}
  if (!require(tidyr, quietly=T)) {install.packages('tidyr');library(tidyr)}
  if (!require(timetk, quietly=T)) {install.packages('timetk');library(timetk)}
  if (!require(forcats, quietly=T)) {install.packages('forcats');library(forcats)}
  if (!require(behavr)) {install.packages('behavr');library(behavr)}
  if (!require(damr, quietly=T)) {install.packages('damr');library(damr)}
  if (!require(ggetho, quietly=T)) {install.packages('ggetho');library(ggetho)}
  if (!require(sleepr, quietly=T)) {install.packages('sleepr');library(sleepr)}
  if (!require(zeitgebr, quietly=T)) {install.packages('zeitgebr');library(zeitgebr)}
  if (!require(svDialogs, quietly=T)) {install.packages('svDialogs');library(svDialogs)}
  if (!require(stringr, quietly=T)) {install.packages('stringr');library(stringr)}
  if (!require(rstatix, quietly=T)) {install.packages('rstatix');library(rstatix)}
  if (!require(ggpubr, quietly=T)) {install.packages('ggpubr');library(ggpubr)}
  if (!require(ggprism, quietly=T)) {install.packages('ggprism');library(ggprism)}
  if (!require(forcats, quietly=T)) {install.packages('forcats');library(forcats)}
  
}

######### Choose experimental folder ########
{
wd <- dlg_dir(title="Choose the experiment folder")$res
setwd(wd)
}

######## Read in data and set up aesthetics #########
### read in metadata and link
{
metafile <- dlg_open(default = wd, title="Choose the metadata file")$res
metadata <- fread(metafile)
metadata <- link_dam_metadata(metadata, result_dir = wd)
# determine how long is the experiment in days
#metadata[,no_days := if_else((as.numeric(difftime(stop_datetime,start_datetime,units="days")))<1,1,round(as.numeric(difftime(stop_datetime,start_datetime,units="days"))))]
### set up aesthetics
by_colour <- dlg_input(message = "Which variable do you want to colour by? CASE SENSITIVE\n(For e.g. genotype, driver, age, treatment, etc.)", 
                        default="genotype")$res
by_facet <- dlg_input(message = "Which variable do you want to create multiple panels for? CASE SENSITIVE\n(For e.g. genotype, driver, age, treatment, etc.)", 
                      default="")$res
}

######## Load DAM data and choose what to include #########
{
### filter what to be included
by_colour_list <- metadata %>% select(contains(by_colour)) %>% unique() %>% pull()
by_facet_list <- NULL
if (length(by_facet)==0){
  to_plot <- dlg_list(choices = by_colour_list, multiple=T, title="Which one do you want to include to analyse?")$res
  metadata2 <- metadata[(get(by_colour) %in% to_plot)]
} else {
  by_facet_list <- metadata %>% select(contains(by_facet)) %>% unique() %>% pull()
  list <- c(by_colour_list, by_facet_list)
  to_plot <- dlg_list(choices = list, multiple=T, title="Which one do you want to include to analyse?")$res
  metadata2 <- metadata[(get(by_colour) %in% to_plot)&(get(by_facet) %in% to_plot)]
}
dt <- load_dam(metadata2, FUN = sleepr::sleep_dam_annotation, min_time_immobile = 300)
summary(dt)
ggetho(dt, aes(z=asleep)) +
  stat_tile_etho() +
  labs(fill="sleep value")
}

####### (Optional) Automatic curation of dead animals #######
{
dt_original <- dt
dt <- curate_dead_animals(dt)
dead_flies <- setdiff(dt_original[, meta=T],dt[, meta=T]) %>% select(-file_info)
fwrite(dead_flies, "dead_flies.csv")
summary(dt)
ggetho(dt, aes(z=asleep)) +
  stat_tile_etho() +
  labs(fill="sleep value")
}

##### (Optional) Filter by how many days they survived #####
{
day_cutoff <- as.numeric(dlg_input(message="At least how many days do the animals need to be alive to be included?", default="1")$res)
lifespan_dt <- dt[, .(lifespan = max(t)), by=id]        # calculate lifespan for each animals
valid_ids <- lifespan_dt[lifespan >= behavr::days(day_cutoff), id]                # filter for desirable lifespan
decision <- dlg_input(message=paste0(length(valid_ids)," animals meet the criteria, do you want to continue?\nY or N?"), 
                        default="Y")$res
  if (decision == "Y") {
    dt <- dt[id %in% valid_ids]    # apply this filter
  } else { dt <- dt}
  summary(dt)
  ggetho(dt, aes(z=asleep)) +
    stat_tile_etho() +
    labs(fill="sleep value")
}
 
###### (Optional) Filter to analyze only certain time period ######
{
  startday <- as.numeric(dlg_input(message=("Which day do you want to start analysis?"), 
                                   default="0")$res)
  endday <- as.numeric(dlg_input(message=("Which day do you want to end analysis?"))$res)
  dt <- dt[t %between% c(days(startday), days(endday)-1)]
  ggetho(dt, aes(z=asleep)) +
    stat_tile_etho() +
    labs(fill="sleep value")
}

##### (Optional) Count how many flies are being analysed in each group of interest #######
{
  summary <- rejoin(dt)[, .(n = uniqueN(.SD)), by = c(by_colour,by_facet), .SDcols = 'id']
  fwrite(summary, "number_of_flies_each_group.csv")
}

##### Choose time bin for analysis #######
{
  bin <- as.numeric(dlg_input(message="Number of hours to bin analysis?", default="12")$res)
  bin_hour <- behavr::hours(bin)
}


##### Calculate some necessary numbers ######
{
  no_of_days <- round(if_else((max(dt$t)-min(dt$t))/behavr::hours(24)<1,1,(max(dt$t)-min(dt$t))/behavr::hours(24)))
  decision2 <- "N"
}

####### Sleep traces ##### 
{
  plot_width <- no_of_days*6
  sleep_traces <- ggetho(dt, aes_string(y="asleep", colour=by_colour)) +
     stat_pop_etho() +
     stat_ld_annotations() +
     facet_wrap(c(by_facet)) +
     scale_y_continuous(name= "Fraction of time sleeping",labels = scales::percent) +
     theme_bw() +
     theme(legend.position="bottom")
   ggsave("Sleep_traces.pdf", sleep_traces, width=plot_width)
 }

###### (Optional) Average across days ######
{
  decision2 <- dlg_input(message="Do you want to average across days? Y or N?",
                         default="Y")$res
}



####### Sleep analysis #####
{
  # summarize sleep by chosen time bin
  rejoined_dt <- rejoin(dt)[,file_info:=NULL]  
  amount <- rejoined_dt %>%
    mutate(time=as_datetime(t)) %>%
    group_by_at(c("id",by_facet, by_colour,"start_datetime")) %>%
    summarise_by_time(
      .date_var = time,
      .by       = paste0(bin," hours"),
      # Summarization
      sleep  = sum(asleep)
    ) %>%
    mutate(hour = as.numeric(time)/3600)

  if (decision2 == "Y") {      # average across days
    amount <- amount %>%
      mutate(hour = as.factor(hour %% 24),
             hour=fct_inseq(hour)) %>%
      group_by_at(c("id",by_facet, by_colour,"hour")) %>%
      #group_by(id,hour,) %>%
      summarize(sleep=mean(sleep, na.rm=T))
  } else { 
    amount <- amount %>%
      mutate(hour=as.factor(as.numeric(time)/3600),
             hour=fct_inseq(hour),
             datetime = start_datetime + as.numeric(time))
    }
  amount_plot <- ggplot(amount, aes_string(x="hour", y="sleep", fill=by_colour)) +
    geom_boxplot(outlier.colour = NA, width=0.5, size=0.1, position=position_dodge(w=0.5)) +
    geom_jitter(alpha=.6, size=0.6, position=position_jitterdodge(dodge.width=0.5, jitter.width=0.05)) +
    facet_wrap(c(by_facet)) +
    scale_y_continuous(name= "Amount of sleep (minutes)") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.x = element_blank())
  ggsave("Sleep_amount.pdf",amount_plot)
}

#### Bout analysis #######
{
  # convert dam sleep data to bout data
  bout <- bout_analysis(asleep, dt)[asleep == TRUE, -"asleep"] %>%
    # identify bouts spanning the bin boundary
    rejoin() %>%
    mutate(spanning = if_else(duration>(bin_hour - t %% bin_hour),"Y","N"),
           # split the duration for spanning bouts
           duration = if_else(spanning == "Y",
                              paste((bin_hour-t %% bin_hour), duration-(bin_hour - t %% bin_hour)),
                              as.character(duration)),
           # add another t for spanning bouts
           t = if_else(spanning == "Y",
                       paste(t, t+(bin_hour - t %% bin_hour)),
                       as.character(t))
           ) %>%
    separate_rows(t, duration, sep=" ", convert=T) %>%
    mutate(time=as_datetime(t)) %>%
    group_by_at(c("id",by_facet, by_colour,"start_datetime")) %>%
    summarise_by_time(
      .date_var = time,
      .by       = paste0(bin," hours"),
      # Summarization
      number  = n(),
      duration = mean(duration, na.rm=T)/60
    ) %>%
    mutate(hour = as.numeric(time)/3600)
  
  if (decision2 == "Y") {      # average across days
    bout <- bout %>%
      mutate(hour = as.factor(hour %% 24),
             hour=fct_inseq(hour)) %>%
      group_by_at(c("id",by_facet, by_colour,"hour")) %>%
      #group_by(id,hour,) %>%
      summarize(number=mean(number, na.rm=T),
                duration=mean(duration, na.rm=T))
  } else { 
    bout <- bout %>%
      mutate(hour=as.factor(as.numeric(time)/3600),
             hour=fct_inseq(hour),
             datetime = start_datetime + as.numeric(time))
  }
  
  bout_number <- ggplot(bout, aes_string(x="hour", y="number", fill=by_colour)) + 
    geom_boxplot(outlier.colour = NA, width=0.5, size=0.1, position=position_dodge(w=0.5)) +
    geom_jitter(alpha=.6, size=0.6, position=position_jitterdodge(dodge.width=0.5, jitter.width=0.05)) +
    facet_wrap(c(by_facet)) +
    scale_y_continuous(name= "Number of sleep bouts") +
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave("Bouts_number.pdf", bout_number)
  
  bout_length <- ggplot(bout, aes_string(x="hour", y="duration", fill=by_colour)) + 
    geom_boxplot(outlier.colour = NA, width=0.5, size=0.1, position=position_dodge(w=0.5)) +
    geom_jitter(alpha=.6, size=0.6, position=position_jitterdodge(dodge.width=0.5, jitter.width=0.05)) +
    facet_wrap(c(by_facet)) +
    scale_y_continuous(name= "Average duration of sleep bouts") +
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave("Bouts_length.pdf", bout_length)
}


##### Analyze activity index #######
{
  # summarize activity by chosen time bin
  activity <- rejoin(dt)[,file_info:=NULL]  %>%
    mutate(time=as_datetime(t)) %>%
    group_by_at(c("id",by_facet, by_colour,"start_datetime")) %>%
    # summarize by chosen time bin
    summarise_by_time(
      .date_var = time,
      .by       = paste0(bin," hours"),
      # Summarization
      activity  = sum(activity),
      wake = n() - sum(asleep),
      activity_index = activity/wake
    ) %>%
    mutate(hour = as.numeric(time)/3600)
  
  if (decision2 == "Y") {      # average across days
    activity <- activity %>%
      mutate(hour = as.factor(hour %% 24),
             hour=fct_inseq(hour)) %>%
      group_by_at(c("id",by_facet, by_colour,"hour")) %>%
      #group_by(id,hour,) %>%
      summarize(activity_index = mean(activity_index, na.rm=T),
                wake = mean(wake, na.rm=T),
                activity = mean(activity, na.rm=T))
  } else { 
    activity <- activity %>%
      mutate(hour=as.factor(as.numeric(time)/3600),
             hour=fct_inseq(hour),
             datetime = start_datetime + as.numeric(time))
  }

  activity_index <- ggplot(activity, aes_string(x="hour", y="activity_index", fill=by_colour)) + 
    geom_boxplot(outlier.colour = NA, width=0.5, size=0.1, position=position_dodge(w=0.5)) +
    geom_jitter(alpha=.3, size=0.1, position=position_jitterdodge(dodge.width=0.5, jitter.width=0.05)) +
    facet_wrap(c(by_facet)) +
    scale_y_continuous(name= "Activity Index") +
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave("Activity_index.pdf", activity_index)
}



####### Output data as a csv file #####
{
  overall_summary <- full_join(amount, bout) %>%
    full_join(activity)%>%
    ungroup()
  fwrite(overall_summary, "Summary_sleep.csv")
}


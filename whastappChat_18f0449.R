library(rwhatsapp)
library(dplyr)
library("tidyr")

chat <- rwa_read(file.choose())
View(chat)
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
chat %>%
  +     mutate(day = date(time)) %>%
  +     count(day) %>%
  +     ggplot(aes(x = day, y = n)) +
  +     geom_bar(stat = "identity") +
  +     ylab("") + xlab("") +
  +     ggtitle("Messages per day")


chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")

#to show emojis
library("ggimage")
emoji_data <- rwhatsapp::emojis %>%
  +     mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% 
  +     mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                                  +                               tolower(hex_runes1), ".png"))

chat %>%
  +     unnest(emoji) %>%
  +     count(author, emoji, sort = TRUE) %>%
  +     group_by(author) %>%
  +     top_n(n = 6, n) %>%
  +     left_join(emoji_data, by = "emoji") %>% 
  +     ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  +     geom_col(show.legend = FALSE) +
  +     ylab("") +
  +     xlab("") +
  +     coord_flip() +
  +     geom_image(aes(y = n + 20, image = emoji_url)) +
  +     facet_wrap(~author, ncol = 2, scales = "free_y") +
  +     ggtitle("Most often used emojis") +
  +     theme(axis.text.y = element_blank(),
              +           axis.ticks.y = element_blank())
> 
  
  library("tidytext")
chat %>%
  +     unnest_tokens(input = text,
                      +                   output = word) %>%
  +     count(author, word, sort = TRUE) %>%
  +     group_by(author) %>%
  +     top_n(n = 6, n) %>%
  +     ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  +     geom_col(show.legend = FALSE) +
  +     ylab("") +
  +     xlab("") +
  +     coord_flip() +
  +     facet_wrap(~author, ncol = 2, scales = "free_y") +
  +     scale_x_reordered() +
  +     ggtitle("Most often used words")



to_remove <- c(stopwords(language = "de"),
               +                "media",
               +                "omitted",
               +                "i",
               +                "you",
               +                "it",
               +                "the",
               +                "and",
               +                "to",
               +                "a",
               +                "i'm", "that", "but", "do", "me")
> 
  > chat %>%
  +     unnest_tokens(input = text,
                      +                   output = word) %>%
  +     filter(!word %in% to_remove) %>%
  +     count(author, word, sort = TRUE) %>%
  +     group_by(author) %>%
  +     top_n(n = 6, n) %>%
  +     ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  +     geom_col(show.legend = FALSE) +
  +     ylab("") +
  +     xlab("") +
  +     coord_flip() +
  +     facet_wrap(~author, ncol = 2, scales = "free_y") +
  +     scale_x_reordered() +
  +     ggtitle("Most often used words")
> 
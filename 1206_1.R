URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE)

# 15是讀幾行的意思
head(txt, 15)

#把被分段的字串，和成一個長字串
txt_new = paste(txt, sep = "", collapse = " ")


TITLE.pos = gregexpr("<title>.*</title>", txt_new)
start.TITLE.pos = TITLE.pos[[1]][1]
end.TITLE.pos = start.TITLE.pos + attr(TITLE.pos[[1]], "match.length")[1] - 1

TITLE.word = substr(txt_new, start.TITLE.pos, end.TITLE.pos)

TITLE.word
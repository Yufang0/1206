URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE)

# 15是讀幾行的意思
head(txt, 15)

#把被分段的字串，和成一個長字串
txt_new = paste(txt, sep = "", collapse = " ")


#以正則表達式，擷取網頁標題
TITLE.pos = gregexpr("<title>.*</title>", txt_new)
#找到頭尾
start.TITLE.pos = TITLE.pos[[1]][1]
end.TITLE.pos = start.TITLE.pos + attr(TITLE.pos[[1]], "match.length")[1] - 1
#擷取標題
TITLE.word = substr(txt_new, start.TITLE.pos, end.TITLE.pos)
TITLE.word

# 將標題的 <title> </title> 拿掉
TITLE.word = gsub("<title>", "", TITLE.word)
TITLE.word = gsub("</title>", "", TITLE.word)
TITLE.word


#擷取等候掛號人數
#因為tr結尾的有非常多，所以必須分開擷取
start.pos = gregexpr("<tr>", txt_new)
end.pos = gregexpr("</tr>", txt_new)
#第一個就是等候掛號人數
i = 1
sub.start.pos = start.pos[[1]][i]
sub.end.pos = end.pos[[1]][i] + attr(end.pos[[1]], "match.length")[i] - 1
sub_txt = substr(txt_new, sub.start.pos, sub.end.pos)
sub_txt
#清理成只剩下 ?人
sub_txt = gsub('等候掛號人數：', '', sub_txt)
sub_txt = gsub('</?tr>', '', sub_txt)
sub_txt = gsub('</?td>', '', sub_txt)
sub_txt = gsub(' ', '', sub_txt)
sub_txt


#P01自訂函數抓臺大醫院的急診即時訊息
NTU_info = function () {
  
  result = data.frame(item = c('等候掛號人數', '等候看診人數', '等候住院人數', '等候ICU人數', '等候推床人數'),
                      info = NA,
                      stringsAsFactors = FALSE)
  
  URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"
  
  txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE)
  txt_new = paste(txt, sep = "", collapse = " ")
  
  start.pos = gregexpr("<tr>", txt_new)
  end.pos = gregexpr("</tr>", txt_new)
  
  for (i in 1:5) {
    
    sub.start.pos = start.pos[[1]][i]
    sub.end.pos = end.pos[[1]][i] + attr(end.pos[[1]], "match.length")[i] - 1
    
    sub_txt = substr(txt_new, sub.start.pos, sub.end.pos)
    sub_txt = gsub('等.*：', '', sub_txt)
    sub_txt = gsub('</?tr>', '', sub_txt)
    sub_txt = gsub('</?td>', '', sub_txt)
    result[i,'info'] = gsub(' ', '', sub_txt)
    
  }
  
  result
  
}
NTU_info()


#利用套件執行
library(rvest)
URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"
website = read_html(URL)
#「html_nodes」能幫助我們把某種標籤的文字萃取出來
#「html_text」能幫助我們把標籤通通去掉
needed_txt = website %>% html_nodes("tr") %>% html_text()
needed_txt


#擷取網站資訊
URL = "https://www.ptt.cc/bbs/AllTogether/index3245.html"
website = read_html(URL)
needed_html = website %>% html_nodes("a")
needed_html
#只擷取字
needed_txt = needed_html %>% html_text()
needed_txt
#只找"徵女"
intrested_pos = grep("[徵女]", needed_txt, fixed = TRUE)
needed_txt[intrested_pos]
#尋找某文章連結，文章連結藏在needed_html裡面
needed_link = needed_html[intrested_pos] %>% html_attr("href")

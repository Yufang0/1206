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


#知道其中一篇文章的基本資料
#讀取
i = 1
sub_link = paste("https://www.ptt.cc", needed_link[i], sep = "")
sub_website = read_html(sub_link) 
#擷取
article_info = sub_website %>% html_nodes(".article-meta-value")
article_info


#P02 找出最近的徵男文
URL = "https://www.ptt.cc/bbs/AllTogether/index.html"
website = read_html(URL)
website %>% html_nodes("a") %>% .[8] %>% html_attr("href")

#答案
#nrow數字跟著需要幾筆資料而更改
my_table = matrix("", nrow = 10, ncol = 4)
colnames(my_table) = c("Title", "url", "ID", "time")
URL = "https://www.ptt.cc/bbs/AllTogether/index.html"
current_id = 1

#找到10筆才停止，並找到發文者ID與時間
for (i in 1:10) {
  
  website = read_html(URL)
  needed_html = website %>% html_nodes("a")
  needed_txt = needed_html %>% html_text()
  intrested_pos = grep("[徵男]", needed_txt, fixed = TRUE)
  
  if (length(intrested_pos) > 0) {
    
    for (j in intrested_pos) {
      
      if (current_id <= 10) {
        my_table[current_id, 1] = needed_txt[j]
        my_table[current_id, 2] = needed_html[j] %>% html_attr("href")
      }
      
      current_id = current_id + 1
      
    }
    
  }
  
  if (current_id > 10) {
    break
  }
  
  next_page = website %>% html_nodes("a") %>% .[8] %>% html_attr("href")
  URL = paste0("https://www.ptt.cc", next_page, sep = "")
  
}

for (i in 1:nrow(my_table)) {
  
  sub_URL = paste("https://www.ptt.cc", my_table[i, 2], sep = "")
  sub_website = read_html(sub_URL)
  article_info = sub_website %>% html_nodes(".article-meta-value") %>% html_text()
  my_table[i, 3] = article_info[1]
  my_table[i, 4] = article_info[4]
  
}

my_table


#訪問八卦版(會有詢問滿18歲的部分，要特別讓電腦處理)
URL = 'https://www.ptt.cc/bbs/Gossiping/index.html'
website = read_html(URL)
website

#over18=1 表示我已滿18，請電腦幫忙打勾
library(RCurl)
URL = 'https://www.ptt.cc/bbs/Gossiping/index.html'
curl = getCurlHandle()
curlSetOpt(cookie = "over18=1", followlocation = TRUE, curl = curl)
#擷取資訊
html_character = getURL(URL, curl = curl)
website = read_html(html_character)
needed_html = website %>% html_nodes("a")
needed_txt = needed_html %>% html_text()
needed_txt
### HW5: 資料來源為PTT韓劇版「花遊記」的LIVE文（直播文）
> * 用到了很多前面學會的觀念：斷詞, tf_idf, crawling, ggplot2，還有資料處理
> * 在爬蟲的地方花了很多時間，參考 **台大資工訓練班郭老師** 的 [網站爬蟲的教學專案](https://github.com/yaojenkuo/r-crawler)，因為PTT也有些版面跟使用者的發文習慣有所改變，所以針對自己要爬的東西還有文字內容進行調整
> * 參考 **CH1** & **CH3** of [Text Mining with R](https://www.tidytextmining.com/)
> * 中文停止詞的內容來源為 [**停止詞**](https://github.com/chdd/weibo/blob/master/stopwords/%E4%B8%AD%E6%96%87%E5%81%9C%E7%94%A8%E8%AF%8D%E5%BA%93.txt)
> * 在project中會更仔細說明整個前因後果，這裡簡單說一下想做這個的原因是自己有看這齣韓劇，但編劇在結局處理得不好頗受人詬病，雖說如此，在板上的討論度還是很高，我也很喜歡這齣劇，從自己在追的時候就發現版上的風向變動很大，有一些有趣的觀察
> * 觀察詞頻可以發現高居榜首的包括劇名以及劇中幾名角色，但觀察tf_idf的結果則可以發現往往是每集劇情的特定角色(非固定角色)會居於前位，此外又因為後面幾集的劇本受到批評，所以編劇也擠進了前幾名

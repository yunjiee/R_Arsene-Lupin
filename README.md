# 🕵️‍♂️ Arsène Lupin Sentiment Analysis (亞森‧羅蘋小說文本情感分析)

![R](https://img.shields.io/badge/R-Language-blue)
![Tidyverse](https://img.shields.io/badge/Package-Tidyverse-orange)
![Tidytext](https://img.shields.io/badge/Package-Tidytext-green)
![ggplot2](https://img.shields.io/badge/Data_Viz-ggplot2-yellow)

> **簡介**：本專案使用 R 語言進行自然語言處理與文字探勘，對著名推理小說《亞森‧羅蘋》系列進行情感分析。透過比較不同的情感詞典，深入探討小說情節推進時的情感波動與文學張力。

## 🎯 專案目標

透過量化的文字探勘技術，研究亞森‧羅蘋系列推理小說。從情感分佈（Sentiment Analysis）的角度，深入探討故事情節的起伏、正負面情緒的轉折，藉此以數據視角重新解析亞森‧羅蘋作品的文學特點與懸疑氛圍的營造。

## 🛠️ 核心技術與分析流程

本專案完全基於 R 語言及其豐富的資料科學套件庫實作：

1. **📖 資料獲取 (Data Acquisition)**
   * 使用 `gutenbergr` 套件直接從 Project Gutenberg API 獲取指定編號的原文電子書（包含 813、The Teeth of the Tiger 等名著）。
2. **🧹 資料清洗與前處理 (Data Wrangling)**
   * 使用 `dplyr` 與 `stringr` 進行正規表達式 (Regex) 處理，精準切割書籍章節 (`chapter`) 與行號 (`linenumber`)。
   * 使用 `tidytext` 的 `unnest_tokens` 進行斷詞，並透過 `anti_join` 與自訂停用詞表 (Stop words) 過濾無意義雜訊。
3. **🧠 情感詞典交叉分析 (Sentiment Analysis)**
   * 引入並比較三種主流情感詞典：**AFINN** (數值化)、**Bing** (二元分類) 以及 **NRC** (情緒分類)。
   * 使用 `inner_join` 與 `pivot_wider` 將文本映射至情緒分數，量化每個詞彙的正負向情緒貢獻度。
4. **📊 資料視覺化 (Data Visualization)**
   * 使用 `ggplot2` 繪製分面長條圖 (`facet_wrap`)，動態呈現各章節隨著故事發展的情感淨值軌跡 (Sentiment Trajectory)。
   * 結合 `wordcloud` 與 `reshape2` 產生正負面情緒對比文字雲 (Comparison Cloud)。

## 📝 分析結果

* **正面氛圍的主導**：根據三種情感詞典的綜合比較結果，正向詞彙的出現頻率整體顯著高於負面詞彙，這為讀者塑造了偏向正面、機智且優雅的閱讀氛圍（符合亞森羅蘋怪盜紳士的形象）。
* **懸疑張力的營造**：資料顯示，在故事特定章節（通常是推理高潮或危機時刻），負面詞彙密度會出現劇烈的低谷（Spikes）。這些詞彙成功增強了故事情節的張力，提升了作品的懸疑感與深度。

## 📚 參考資料

* Text Mining with R: A Tidy Approach
* Project Gutenberg (古騰堡計畫)

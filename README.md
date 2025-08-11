

# 🌍 Aljazeera News 
**From Headlines to Insights — Mining Global Stories with NLP**  

This project turns raw global news into actionable insights.  
Using **Web Scraping**, **Natural Language Processing**, and **Topic Modeling**, we collect and analyze articles from [Al Jazeera](https://www.aljazeera.com) to uncover trends, themes, and narratives shaping our world.

***

## 🚀 What This Project Does

- **Scrape news across categories** — from Breaking News to Economy, Human Rights, Sport, Science & Tech, and Opinion pieces.  
- **Clean, normalize, and structure** text data using advanced preprocessing (tokenization, lemmatization, stemming, stopword removal).  
- **Analyze language and context** through **Latent Dirichlet Allocation (LDA)** to reveal hidden topics and patterns.  
- **Visualize results** with rich data graphics — word clouds, bar plots, category distributions, and correlation maps.  
- **Export clean datasets** ready for further NLP or machine learning work.

***

## 🛠 Tech Stack

- **Language:** R  
- **Web Scraping:** `rvest`, `httr`  
- **Text Processing:** `tm`, `tidytext`, `textstem`, `SnowballC`  
- **Visualization:** `ggplot2`, `wordcloud`, `igraph`, `ggraph`  
- **Topic Modeling:** `topicmodels` (LDA)  

***

## 📊 Workflow in Action

1. **Scraping:**  
   Grab real-time news from Al Jazeera across multiple categories, complete with headlines, dates, authors, and URLs.  

2. **Preprocessing:**  
   Transform raw text into clean, tokenized data — no numbers, punctuation, or noise.

3. **Text Mining:**  
   Build Document-Term Matrices, filter sparse terms, and prepare datasets for modeling.

4. **Topic Modeling:**  
   Apply **LDA** to extract major thematic clusters defining the news cycle.

5. **Insights & Visualization:**  
   Generate:
   - Top 10 term plots per topic
   - Category-based distribution charts
   - Word clouds for key topics
   - Term-association maps (e.g. words related to "Palestine")  

***

## 📂 Outcomes

- **Clean Corpus:** CSV with preprocessed Al Jazeera articles  
- **Topic Model Outputs:** Keywords per topic + topic probability per article  
- **Visual Reports:** Graphs and clouds that make trends obvious at a glance  

***

## 🌟 Why It’s Cool

This isn’t just another scraping project — it’s a **news intelligence engine**.  
With this setup, you can:
- Monitor evolving narratives  
- Track issue prominence over time  
- Predict shifts in public focus  
- Feed topic insights into ML classifiers or sentiment models  

Whether you’re a **data scientist**, **journalist**, **researcher**, or **policy analyst**, this tool gives you a major head start in decoding the world’s conversations.


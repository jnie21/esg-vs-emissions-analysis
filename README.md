# ESG vs. Emissions: Does Corporate Reputation Reflect Reality?

**TL;DR**  
- ❓ **Question:** Do higher ESG environmental scores mean lower emissions?  
- 📊 **Method:** Fuzzy-matched ESG and emissions data using R; regression and correlation analysis  
- 📉 **Finding:** Weak, statistically insignificant correlation (r = –0.068, p = 0.75)  
- 🧠 **Insight:** ESG scores may reflect disclosure efforts more than actual performance

---

## ✅ Phase 1: Complete (R-Based Analysis)

Phase 1 of this project investigates whether ESG environmental scores align with real-world carbon emissions, focusing on the world’s largest fossil fuel producers.

- 📎 Public data from ESG rating providers and the Carbon Majors emissions database
- 🔄 Fuzzy join (Jaro-Winkler) to match ESG and emissions company names
- 📈 Log transformation of emissions; regression and diagnostics
- 🧪 **Result:** No statistically significant relationship (r = –0.068, p = 0.75; R² ≈ 0.005)
- 📌 **Interpretation:** ESG scores may not reflect real environmental impact—raising concerns about greenwashing

📄 [Full Report (PDF)](./ESG%20_%20Quantitative%20Analysis.pdf)

---

## 🔄 Phase 2: SQL Extension (In Progress)

This project is now being extended using **SQL** to enable scalable, relational analysis of emissions trends and ESG performance gaps.

- 🗃️ Designing normalized tables for ESG and emissions data
- 🔍 Running SQL queries to explore multi-year emissions patterns
- 🚩 Creating flags for ESG-emissions mismatch ("greenwashing risk")

📁 Coming soon: `schema.sql`, `queries.sql`, and updated documentation

---

## 📂 Data Sources

- [Public Company ESG Ratings Dataset (Kaggle)](https://www.kaggle.com/datasets/parulpandey/esg-scores-for-public-companies)  
- [Carbon Majors Emissions Dataset (Kaggle)](https://www.kaggle.com/datasets/dgomonov/carbonmajors)  

---

## 🧰 Tools & Methods

- **Languages:** R (tidyverse, ggplot2, fuzzyjoin), SQL (PostgreSQL in Phase 2)  
- **Techniques:** Fuzzy matching, log transformation, linear regression, residual analysis  
- **Planned (SQL):** GROUP BY sector, join by year, trend analysis, risk flags

---

## 📫 Contact

**Nelly Nie**  
MPP, University of Chicago Harris School of Public Policy  
🔗 [GitHub](https://github.com/jnie21) | [LinkedIn](https://www.linkedin.com/in/nellynie)  
🗓️ *Last updated: August 6, 2025*

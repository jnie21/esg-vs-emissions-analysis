
# ESG vs. Emissions: Does Corporate Reputation Match Reality?
## This repo contains Phase 1 and Phase 2 of my ESG impact analysis TL;DR
- ❓ Question: Do higher ESG environmental scores mean lower emissions?
- 📊 Method: Public ESG data + CO₂ emissions data (fuzzy matched, cleaned)
- 📉 Finding: Statistically significant but very weak negative association
- 🧠 Insight: ESG scores may reflect effort, not outcomes — especially in carbon-heavy sectors
- 📎 [📄 Full PDF Report: ESG _ Quantitative Analysis.pdf](./ESG%20_%20Quantitative%20Analysis.pdf)

This project examines how ESG environmental scores correlate with actual emissions among the highest-emitting industries, using fossil fuel producers as a critical test case, measured by carbon emissions. Using real-world data, I analyze patterns across industries to identify gaps between ESG ratings and emissions output.

## 🎯 Why It Matters
ESG scores guide over $35 trillion in global assets — shaping investment flows, regulatory frameworks, and corporate strategy. Yet critics argue that ESG scores can be misleading — rewarding companies for disclosure rather than performance. If ESG scores fail to reflect actual environmental impact — such as carbon emissions — the entire foundation of ESG investing may be vulnerable to greenwashing, mispriced risk, and public mistrust. This project investigates whether those concerns are backed by data.

## 📊 What This Project Does
- Imports and cleans a dataset of ESG scores and emissions from 25 largest fossil producing companies worldwide
- Visualizes ESG vs. CO₂ emissions across companies and sectors
- Identifies industries and firms where ESG scores are misaligned with actual impact
- Summarizes implications for investors, regulators, and public-sector economists
## 🔄 Phase 2: SQL Extension (In Progress)
- This project is currently being extended using SQL to scale the analysis.
- Upcoming additions will explore emissions trends, ESG-performance mismatches, and greenwashing risk using structured queries.
- 📁 Files coming soon: schema.sql, queries.sql, and Phase 2 notes.

## 📁 Data Source
- Public Company ESG Ratings Dataset (Kaggle)
- Carbon Majors Emissions Dataset (Medium Granularity, Kaggle)
- Both datasets were cleaned, merged, and filtered to remove duplicates and improve company name alignment using fuzzy joins.

## 🔍 Key Questions
- Are higher ESG scores consistently associated with lower emissions?
- Which sectors or firms score high but pollute heavily?
- What does this suggest about ESG as a regulatory or investment tool?
  
## 🧰 Methodology
- Fuzzy matching of ESG company names with parent entities in emissions data
- Log transformation of emissions for normalization
- Bivariate linear regressions (environment, social, governance)
- Correlation analysis and diagnostics (residuals, Q-Q plots, Cook’s distance)

## 📌 Relevance
This project sits at the intersection of environmental policy, corporate transparency, and economic analysis. It aims to inform discussions on sustainable finance, greenwashing, and regulatory reform — making it relevant to public-sector analysts, consulting firms, and financial institutions alike. While only 25 firms from our ESG dataset matched with verifiable emissions data, this subset includes many of the world’s largest fossil fuel producers — precisely the companies where alignment between ESG scores and actual environmental performance matters most. The limited match rate itself signals a broader issue: ESG scoring systems often operate independently of empirical impact data, raising concerns about comparability and credibility.

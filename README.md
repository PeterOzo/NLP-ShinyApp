# NLP-ShinyApp
## Covid Misinformation Detector ShinyApp

### Deployment-ready for ShinyApps.io

🚀 Overview
This interactive Shiny dashboard implements a state-of-the-art, research-driven system for detecting COVID-19 “real” vs. “misinformation” content. At its core, we leverage a finely tuned ensemble of keyword- and pattern-based indicators derived from peer-reviewed publications, clinical-trial references, and known conspiracy lexicons to give each user-supplied snippet a verdict and confidence score. The app is split into multiple tabs so you can:

### Dashboard

Real vs. Fake Distribution: A dynamic pie chart showing the current share of reliable vs. misinformation articles.

Articles Over Time: A time-series plot illustrating how real and fake content volumes ebb and flow.

Model Performance: A summary block highlighting accuracy, precision/recall, and AUC-ROC metrics based on the CoAID corpus.

### Data Explorer

Browse the entire dataset (over 700+ labeled articles)

Filter by publication date and content type (real/fake)

Download the filtered subset as a CSV

### Live Monitoring

Simulated “live” feed of incoming social‐media posts (Twitter, Facebook, Reddit, etc.)

Hourly summary plot of detection counts, showing both reliable and misinformation trends

### Classify Text

Paste or type any COVID-19–related text snippet

Click Analyze Text to receive:

Verdict (Likely Reliable / Likely Misinformation / Possibly Misinformation / Insufficient Information)

Confidence Score (0–100 %)

Detailed Breakdown of strong vs. moderate indicators on both sides

Explanatory Guidance—tips on how to verify or interpret the result

About Research

Detailed methodology: dataset, model architecture (RoBERTa fine-tuning, class balancing, statistical validation), and key performance metrics

### Author

Profile and contact info for Peter Chika Ozo-Ogueji, the data scientist behind this work

Educational background, professional experience, featured projects, and technical skillset

🔧 Deployment & Configuration
All necessary R packages are checked and installed automatically at startup (required_packages block). You can spin up a local instance by running:


### Install & load dependencies
source("app_dependencies.R")

### Launch the Shiny app
shiny::runApp("app.R", launch.browser = TRUE)
To deploy on ShinyApps.io, simply authenticate via rsconnect::setAccountInfo() and call:


rsconnect::deployApp("path/to/your/project")
A robust load_data() function gracefully handles missing CSV files by falling back to generated sample data—ideal for quick demos or containerized deployments.

🔍 Classification Engine
Our enhanced_classify_text() function embodies the core research insights:

Strong Indicators (weight ×3):

Misinformation cues: “plandemic,” “microchip,” “5g,” “Bill Gates,” etc.

Reliable cues: “CDC,” “WHO,” “peer-reviewed,” “clinical trial,” etc.

Moderate Indicators (weight ×1): general health and risk-related vocabulary.

Text Quality Signals: punctuation bursts, ALL-CAPS penalties, word-count heuristics.

Decision thresholds tuned to reflect the original CoAID class imbalance (≈ 95 % real, 5 % fake)

Outputs not only a binary verdict but also a fully transparent breakdown of indicator counts, enabling users to understand why the model reached its conclusion.

📊 Visualization & UX
We’ve used:

shinydashboard + shinydashboardPlus for a clean, multi-tab layout

plotly for immersive, interactive charts

DT for spreadsheet-style data exploration

fresh + bslib for a custom color theme that highlights reliability vs. misinformation in green and red

Every plot and table is wrapped in shinycssloaders spinners for a slick, responsive feel.

# ============================================================
# Universal Data Analyzer
# ============================================================

library(shiny)
library(readr)
library(readxl)
library(DT)
library(moments)
library(corrplot)
library(RColorBrewer)
library(plotrix)
library(shinycssloaders)
library(shinyjs)

# ==============================
# HELPER FUNCTIONS
# ==============================

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

is_categorical <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  format(round(p, 4), nsmall = 4)
}

missing_pct <- function(x) {
  round(sum(is.na(x)) / length(x) * 100, 1)
}

get_top_correlations <- function(df, n = 3) {
  nums <- df[, sapply(df, is.numeric), drop = FALSE]
  if (ncol(nums) < 2) return(NULL)
  
  cm <- cor(nums, use = "complete.obs")
  cm[upper.tri(cm, diag = TRUE)] <- NA
  
  pairs <- which(!is.na(cm), arr.ind = TRUE)
  if (nrow(pairs) == 0) return(NULL)
  
  cors <- data.frame(
    Var1 = rownames(cm)[pairs[, 1]],
    Var2 = colnames(cm)[pairs[, 2]],
    Correlation = cm[pairs],
    stringsAsFactors = FALSE
  )
  
  cors <- cors[order(abs(cors$Correlation), decreasing = TRUE), ]
  head(cors, n)
}

# ==============================
# CUSTOM CSS - VIBRANT DESIGN
# ==============================

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800&display=swap');

:root {
  --primary: #6366f1;
  --primary-dark: #4f46e5;
  --secondary: #ec4899;
  --success: #10b981;
  --warning: #f59e0b;
  --danger: #ef4444;
  --info: #06b6d4;
  --purple: #a855f7;
  --orange: #f97316;
}

* {
  font-family: 'Poppins', sans-serif;
}

body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #f093fb 100%);
  min-height: 100vh;
}

.container-fluid {
  background: white;
  margin: 0;
  padding: 0;
}

/* ========== HEADER ========== */
.app-header {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  padding: 28px 40px;
  color: white;
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
}

.app-header h1 {
  margin: 0;
  font-size: 32px;
  font-weight: 700;
  color: white;
  text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
}

.app-header .tagline {
  margin: 6px 0 0 0;
  font-size: 15px;
  color: rgba(255, 255, 255, 0.95);
  font-weight: 400;
}

/* ========== SIDEBAR ========== */
.sidebar {
  background: linear-gradient(180deg, #f8f9ff 0%, #f0f1ff 100%);
  border-right: 2px solid #e0e7ff;
  padding: 24px;
  min-height: calc(100vh - 120px);
  box-shadow: 2px 0 10px rgba(0, 0, 0, 0.05);
}

.sidebar-section {
  background: white;
  border-radius: 12px;
  padding: 20px;
  margin-bottom: 20px;
  box-shadow: 0 2px 8px rgba(99, 102, 241, 0.1);
  border: 1px solid #e0e7ff;
}

.section-title {
  font-size: 14px;
  font-weight: 700;
  color: #4f46e5;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin: 0 0 16px 0;
  display: flex;
  align-items: center;
  gap: 8px;
}

.helper-text {
  font-size: 12px;
  color: #6b7280;
  margin-top: 6px;
  line-height: 1.5;
}

/* ========== FILE UPLOAD ========== */
.file-upload-wrapper {
  position: relative;
  margin-bottom: 12px;
}

.file-upload-label {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 32px 16px;
  border: 2px dashed #a5b4fc;
  border-radius: 12px;
  background: linear-gradient(135deg, #eef2ff 0%, #f5f3ff 100%);
  cursor: pointer;
  transition: all 0.3s ease;
}

.file-upload-label:hover {
  border-color: #6366f1;
  background: linear-gradient(135deg, #e0e7ff 0%, #ede9fe 100%);
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(99, 102, 241, 0.2);
}

.upload-icon {
  font-size: 48px;
  margin-bottom: 12px;
}

.upload-text {
  font-size: 15px;
  font-weight: 600;
  color: #4f46e5;
}

.upload-subtext {
  font-size: 13px;
  color: #6b7280;
  margin-top: 4px;
}

input[type='file'] {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0,0,0,0);
  border: 0;
}

.file-name-display {
  font-size: 13px;
  font-weight: 500;
  color: #059669;
  padding: 10px 14px;
  background: linear-gradient(135deg, #d1fae5 0%, #a7f3d0 100%);
  border-radius: 8px;
  margin-top: 12px;
  display: none;
  border: 1px solid #6ee7b7;
}

.file-name-display.active {
  display: block;
}

/* ========== FORM CONTROLS ========== */
.form-group {
  margin-bottom: 20px;
}

.form-group label {
  display: block;
  font-size: 13px;
  font-weight: 600;
  color: #374151;
  margin-bottom: 8px;
}

.form-control,
.selectize-input,
input[type='number'] {
  width: 100%;
  padding: 11px 14px;
  font-size: 14px;
  border: 2px solid #e5e7eb;
  border-radius: 8px;
  background: white;
  transition: all 0.2s ease;
  color: #111827;
}

.form-control:focus,
.selectize-input.focus,
input[type='number']:focus {
  outline: none;
  border-color: #6366f1;
  box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1);
}

.selectize-input {
  box-shadow: none;
  line-height: 1.5;
}

.selectize-dropdown {
  border: 2px solid #e5e7eb;
  border-radius: 8px;
  box-shadow: 0 10px 25px rgba(0, 0, 0, 0.15);
}

/* ========== CHECKBOX ========== */
.checkbox {
  display: flex;
  align-items: center;
  margin: 14px 0;
  cursor: pointer;
}

.checkbox input[type='checkbox'] {
  width: 20px;
  height: 20px;
  margin-right: 10px;
  cursor: pointer;
  accent-color: #6366f1;
}

.checkbox label {
  font-size: 14px;
  font-weight: 500;
  color: #374151;
  cursor: pointer;
  margin: 0;
}

/* ========== BUTTONS ========== */
.btn {
  padding: 11px 20px;
  font-size: 14px;
  font-weight: 600;
  border: none;
  border-radius: 8px;
  cursor: pointer;
  transition: all 0.2s ease;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 8px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
}

.btn-primary {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: white;
}

.btn-primary:hover {
  background: linear-gradient(135deg, #4f46e5 0%, #7c3aed 100%);
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(99, 102, 241, 0.4);
}

.btn-secondary {
  background: linear-gradient(135deg, #64748b 0%, #475569 100%);
  color: white;
}

.btn-secondary:hover {
  background: linear-gradient(135deg, #475569 0%, #334155 100%);
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(100, 116, 139, 0.4);
}

.btn-success {
  background: linear-gradient(135deg, #10b981 0%, #059669 100%);
  color: white;
}

.btn-success:hover {
  background: linear-gradient(135deg, #059669 0%, #047857 100%);
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(16, 185, 129, 0.4);
}

.btn-warning {
  background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%);
  color: white;
}

.btn-info {
  background: linear-gradient(135deg, #06b6d4 0%, #0891b2 100%);
  color: white;
}

.btn-block {
  width: 100%;
  margin-bottom: 10px;
}

/* ========== MAIN PANEL ========== */
.main-panel {
  background: #fafbfc;
  padding: 0;
}

/* ========== TABS ========== */
.nav-tabs {
  background: white;
  border-bottom: 2px solid #e5e7eb;
  padding: 0 32px;
  margin: 0;
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
}

.nav-tabs > li > a {
  padding: 16px 24px;
  font-size: 14px;
  font-weight: 600;
  color: #6b7280;
  border: none;
  background: transparent;
  border-radius: 0;
  border-bottom: 3px solid transparent;
  transition: all 0.2s ease;
  margin-bottom: -2px;
}

.nav-tabs > li > a:hover {
  color: #6366f1;
  background: #f9fafb;
  border-bottom: 3px solid #c7d2fe;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  color: #6366f1;
  background: transparent;
  border: none;
  border-bottom: 3px solid #6366f1;
  font-weight: 700;
}

.nav-tabs > li.disabled > a {
  opacity: 0.35;
  cursor: not-allowed;
  pointer-events: none;
}

/* ========== TAB CONTENT ========== */
.tab-content {
  padding: 32px;
  background: #fafbfc;
  min-height: calc(100vh - 250px);
}

.tab-pane {
  animation: fadeInUp 0.3s ease;
}

@keyframes fadeInUp {
  from { 
    opacity: 0; 
    transform: translateY(20px); 
  }
  to { 
    opacity: 1; 
    transform: translateY(0); 
  }
}

/* ========== CARDS ========== */
.content-card {
  background: white;
  border-radius: 16px;
  padding: 28px;
  margin-bottom: 28px;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
  border: 1px solid #e5e7eb;
}

.card-header {
  font-size: 20px;
  font-weight: 700;
  color: #111827;
  margin: 0 0 24px 0;
  padding-bottom: 16px;
  border-bottom: 2px solid #f3f4f6;
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-wrap: wrap;
  gap: 12px;
}

/* ========== STATS CARDS ========== */
.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 20px;
  margin-bottom: 24px;
}

.stat-card {
  background: linear-gradient(135deg, #ffffff 0%, #f9fafb 100%);
  border-radius: 14px;
  padding: 24px;
  border-left: 5px solid;
  transition: all 0.3s ease;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.06);
}

.stat-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.12);
}

.stat-card.primary { border-left-color: #6366f1; }
.stat-card.success { border-left-color: #10b981; }
.stat-card.warning { border-left-color: #f59e0b; }
.stat-card.info { border-left-color: #06b6d4; }
.stat-card.purple { border-left-color: #a855f7; }

.stat-label {
  font-size: 12px;
  font-weight: 700;
  color: #6b7280;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 8px;
}

.stat-value {
  font-size: 32px;
  font-weight: 800;
  color: #111827;
  line-height: 1.2;
}

.stat-subtitle {
  font-size: 13px;
  color: #9ca3af;
  margin-top: 4px;
}

/* ========== INFO CARDS ========== */
.info-card {
  background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%);
  border: 2px solid #93c5fd;
  border-radius: 12px;
  padding: 16px 20px;
  margin-bottom: 20px;
  display: flex;
  align-items: start;
  gap: 14px;
  font-size: 14px;
  line-height: 1.6;
  color: #1e40af;
  font-weight: 500;
}

.info-card.success {
  background: linear-gradient(135deg, #d1fae5 0%, #a7f3d0 100%);
  border-color: #6ee7b7;
  color: #065f46;
}

.info-card.warning {
  background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%);
  border-color: #fcd34d;
  color: #92400e;
}

/* ========== VISUAL STATS ========== */
.visual-stats {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 16px;
  margin-bottom: 24px;
}

.visual-stat-item {
  background: white;
  border-radius: 12px;
  padding: 20px;
  border: 2px solid #e5e7eb;
  transition: all 0.2s ease;
}

.visual-stat-item:hover {
  border-color: #6366f1;
  box-shadow: 0 4px 12px rgba(99, 102, 241, 0.15);
}

.visual-stat-item h4 {
  font-size: 14px;
  font-weight: 700;
  color: #6366f1;
  margin: 0 0 12px 0;
}

.kv {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 8px 14px;
  font-size: 14px;
}

.kv .k {
  color: #6b7280;
  font-weight: 500;
}

.kv .v {
  font-weight: 700;
  color: #111827;
  text-align: right;
}

/* ========== PLOTS ========== */
.shiny-plot-output {
  border-radius: 12px;
  overflow: hidden;
  background: white;
  border: 2px solid #e5e7eb;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
}

.plot-row {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 24px;
  margin-top: 24px;
}

/* ========== DOWNLOAD SECTION ========== */
.download-section {
  display: flex;
  gap: 12px;
  flex-wrap: wrap;
  margin-top: 24px;
  padding-top: 24px;
  border-top: 2px solid #f3f4f6;
}

/* ========== TEST CARDS ========== */
.test-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 20px;
  margin-top: 24px;
}

.test-card {
  background: white;
  border-radius: 14px;
  padding: 24px;
  border: 2px solid #e5e7eb;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.06);
}

.test-card h4 {
  margin: 0 0 16px 0;
  font-size: 17px;
  font-weight: 700;
  color: #111827;
}

.interpretation {
  margin-top: 16px;
  padding: 14px 16px;
  background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%);
  border-radius: 8px;
  font-size: 14px;
  font-weight: 600;
  color: #1e40af;
  border-left: 4px solid #6366f1;
}

/* ========== TABLE CARDS ========== */
.table-card {
  background: white;
  border-radius: 14px;
  padding: 24px;
  margin-bottom: 24px;
  border: 2px solid #e5e7eb;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.06);
}

.table-card h4 {
  margin: 0 0 16px 0;
  font-size: 17px;
  font-weight: 700;
  color: #111827;
}

/* ========== DATATABLES ========== */
.dataTables_wrapper {
  font-size: 14px;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: white;
  font-weight: 700;
  padding: 14px 12px;
  border: none;
}

table.dataTable tbody td {
  padding: 12px;
  border-bottom: 1px solid #f3f4f6;
}

table.dataTable tbody tr:hover {
  background: #f9fafb;
}

/* ========== BADGE ========== */
.badge {
  display: inline-block;
  padding: 6px 14px;
  border-radius: 20px;
  font-size: 13px;
  font-weight: 700;
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: white;
  box-shadow: 0 2px 8px rgba(99, 102, 241, 0.3);
}

/* ========== RESPONSIVE ========== */
@media (max-width: 1200px) {
  .plot-row {
    grid-template-columns: 1fr;
  }
}

@media (max-width: 768px) {
  .app-header {
    padding: 20px;
  }
  
  .tab-content {
    padding: 20px;
  }
  
  .stats-grid,
  .visual-stats,
  .test-grid {
    grid-template-columns: 1fr;
  }
}
"

# ==============================
# UI
# ==============================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML("
      $(document).on('change', '#file', function() {
        var fileName = $(this).val().split('\\\\').pop();
        if (fileName) {
          $('#file-name-display').html('<strong>✓</strong> ' + fileName).addClass('active');
        } else {
          $('#file-name-display').text('').removeClass('active');
        }
      });
    "))
  ),
  
  # Header
  div(class = "app-header",
      h1("📊 Universal Data Analyzer"),
      div(class = "tagline", "Upload, explore, and analyze your data with beautiful visualizations")
  ),
  
  # Main Layout
  sidebarLayout(
    # ========== SIDEBAR ==========
    sidebarPanel(
      class = "sidebar",
      width = 3,
      
      # Data Upload
      div(class = "sidebar-section",
          div(class = "section-title", "📁 DATA UPLOAD"),
          div(class = "file-upload-wrapper",
              tags$label(
                class = "file-upload-label",
                `for` = "file",
                div(class = "upload-icon", "📂"),
                div(class = "upload-text", "Click to Upload"),
                div(class = "upload-subtext", "CSV, XLSX, or XLS files")
              ),
              fileInput("file", NULL, accept = c(".csv", ".xlsx", ".xls")),
              div(id = "file-name-display", class = "file-name-display")
          ),
          div(class = "checkbox",
              checkboxInput("stringsAsFactors", NULL, TRUE),
              tags$label(`for` = "stringsAsFactors", "Treat text as categories")
          )
      ),
      
      # Variables
      div(class = "sidebar-section",
          div(class = "section-title", "🎯 VARIABLES"),
          uiOutput("var_select_ui")
      ),
      
      # Test Settings
      div(class = "sidebar-section",
          div(class = "section-title", "🔬 TEST SETTINGS"),
          numericInput("mu0", "Hypothesized mean (μ₀)", value = 0, step = 0.1),
          div(class = "helper-text", "For one-sample tests"),
          br(),
          div(class = "checkbox",
              checkboxInput("equal_var", NULL, TRUE),
              tags$label(`for` = "equal_var", "Equal variance")
          )
      ),
      
      # Actions
      div(class = "sidebar-section",
          div(class = "section-title", "⚙️ ACTIONS"),
          actionButton("reset_selections", "Reset Selections", class = "btn btn-secondary btn-block"),
          actionButton("reset_app", "Reset App", class = "btn btn-warning btn-block")
      )
    ),
    
    # ========== MAIN PANEL ==========
    mainPanel(
      class = "main-panel",
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        # Summary
        tabPanel("📌 Summary",
                 value = "tab_summary",
                 uiOutput("dashboard_ui")
        ),
        
        # Data Preview
        tabPanel("📊 Data Preview",
                 value = "tab_preview",
                 div(class = "content-card",
                     div(class = "card-header", "📋 Your Dataset"),
                     withSpinner(DTOutput("table"), color = "#6366f1", type = 4)
                 ),
                 div(class = "content-card",
                     div(class = "card-header", "📊 Quick Summary"),
                     uiOutput("data_summary_cards")
                 )
        ),
        
        # Single Numeric
        tabPanel("📈 Single Numeric",
                 value = "tab_single_num",
                 
                 div(class = "content-card",
                     div(class = "card-header", 
                         "📊 Descriptive Statistics",
                         downloadButton("download_num_stats", "Download Stats", class = "btn btn-success")
                     ),
                     uiOutput("num_stats_cards")
                 ),
                 
                 div(class = "content-card",
                     div(class = "card-header", "📈 Visualizations"),
                     div(class = "plot-row",
                         withSpinner(plotOutput("hist", height = "400px"), color = "#6366f1", type = 4),
                         withSpinner(plotOutput("box", height = "400px"), color = "#6366f1", type = 4)
                     ),
                     div(class = "download-section",
                         downloadButton("download_hist", "📥 Download Histogram", class = "btn btn-info"),
                         downloadButton("download_box", "📥 Download Boxplot", class = "btn btn-info")
                     )
                 )
        ),
        
        # Two Numeric
        tabPanel("📉 Two Numeric",
                 value = "tab_two_num",
                 div(class = "content-card",
                     div(class = "card-header", "🔗 Correlation Analysis"),
                     uiOutput("two_num_stats_cards")
                 ),
                 
                 div(class = "content-card",
                     div(class = "card-header", "📊 Scatter Plot"),
                     withSpinner(plotOutput("scatter", height = "550px"), color = "#6366f1", type = 4),
                     div(class = "download-section",
                         downloadButton("download_scatter", "📥 Download Plot", class = "btn btn-info")
                     )
                 )
        ),
        
        # Multi Numeric
        tabPanel("🔗 Multi Numeric",
                 value = "tab_multi_num",
                 div(class = "content-card",
                     div(class = "card-header", 
                         "📊 Correlation Matrix",
                         downloadButton("download_corr_csv", "📥 Download CSV", class = "btn btn-success")
                     ),
                     withSpinner(DTOutput("corr_tbl"), color = "#6366f1", type = 4)
                 ),
                 
                 div(class = "content-card",
                     div(class = "card-header", "🎨 Visualizations"),
                     div(class = "plot-row",
                         withSpinner(plotOutput("pairs_plot", height = "500px"), color = "#6366f1", type = 4),
                         withSpinner(plotOutput("corr_plot", height = "500px"), color = "#6366f1", type = 4)
                     ),
                     div(class = "download-section",
                         downloadButton("download_pairs", "📥 Pairs Plot", class = "btn btn-info"),
                         downloadButton("download_corrplot", "📥 Correlation Plot", class = "btn btn-info")
                     )
                 )
        ),
        
        # Single Categorical
        tabPanel("🏷️ Single Categorical",
                 value = "tab_single_cat",
                 div(class = "content-card",
                     div(class = "card-header", "📊 Frequency Analysis"),
                     uiOutput("cat_stats_cards")
                 ),
                 
                 div(class = "content-card",
                     div(class = "card-header", "📊 Visualizations"),
                     div(class = "plot-row",
                         withSpinner(plotOutput("bar", height = "450px"), color = "#6366f1", type = 4),
                         withSpinner(plotOutput("pie", height = "450px"), color = "#6366f1", type = 4)
                     ),
                     div(class = "download-section",
                         downloadButton("download_bar", "📥 Bar Chart", class = "btn btn-info"),
                         downloadButton("download_pie", "📥 Pie Chart", class = "btn btn-info")
                     )
                 )
        ),
        
        # Two Categorical
        tabPanel("📋 Two Categorical",
                 value = "tab_two_cat",
                 div(class = "content-card",
                     div(class = "card-header", "📊 Contingency Tables"),
                     div(class = "table-card",
                         h4("📈 Frequency Table"),
                         withSpinner(DTOutput("freq_table"), color = "#6366f1", type = 4)
                     ),
                     div(class = "table-card",
                         h4("📊 Proportions"),
                         withSpinner(DTOutput("prop_table"), color = "#6366f1", type = 4)
                     ),
                     div(class = "download-section",
                         downloadButton("download_freq_csv", "📥 Frequency CSV", class = "btn btn-success"),
                         downloadButton("download_prop_csv", "📥 Proportions CSV", class = "btn btn-success")
                     )
                 ),
                 
                 div(class = "content-card",
                     div(class = "card-header", "📊 Visual Comparisons"),
                     fluidRow(
                       column(4, withSpinner(plotOutput("stacked", height = "500px"), color = "#6366f1", type = 4)),
                       column(4, withSpinner(plotOutput("side", height = "500px"), color = "#6366f1", type = 4)),
                       column(4, withSpinner(plotOutput("mosaic", height = "500px"), color = "#6366f1", type = 4))
                     )
                 )
        ),
        
        # Hypothesis Tests
        tabPanel("🧪 Tests",
                 value = "tab_tests",
                 div(class = "content-card",
                     withSpinner(uiOutput("tests_out_ui"), color = "#6366f1", type = 4)
                 )
        )
      )
    )
  )
)

# ==============================
# SERVER
# ==============================

server <- function(input, output, session) {
  
  upload_time <- reactiveVal(NULL)
  
  data_in <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    df <- NULL
    if (ext == "csv") {
      df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(input$file$datapath)
    } else {
      validate("Unsupported file type")
    }
    
    if (isTRUE(input$stringsAsFactors)) {
      for (nm in names(df)) {
        if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
      }
    }
    
    upload_time(Sys.time())
    df
  })
  
  numeric_vars <- reactive({
    df <- data_in()
    names(df)[sapply(df, is.numeric)]
  })
  
  categorical_vars <- reactive({
    df <- data_in()
    names(df)[sapply(df, is_categorical)]
  })
  
  # Tab management
  observe({
    req(data_in())
    
    nums <- numeric_vars()
    cats <- categorical_vars()
    
    if (length(nums) >= 1) shinyjs::enable(selector = "a[data-value='tab_single_num']")
    else shinyjs::disable(selector = "a[data-value='tab_single_num']")
    
    if (length(nums) >= 2) {
      shinyjs::enable(selector = "a[data-value='tab_two_num']")
      shinyjs::enable(selector = "a[data-value='tab_multi_num']")
    } else {
      shinyjs::disable(selector = "a[data-value='tab_two_num']")
      shinyjs::disable(selector = "a[data-value='tab_multi_num']")
    }
    
    if (length(cats) >= 1) shinyjs::enable(selector = "a[data-value='tab_single_cat']")
    else shinyjs::disable(selector = "a[data-value='tab_single_cat']")
    
    if (length(cats) >= 2) shinyjs::enable(selector = "a[data-value='tab_two_cat']")
    else shinyjs::disable(selector = "a[data-value='tab_two_cat']")
    
    if (length(nums) >= 1) shinyjs::enable(selector = "a[data-value='tab_tests']")
    else shinyjs::disable(selector = "a[data-value='tab_tests']")
  })
  
  # Reset handlers
  observeEvent(input$reset_selections, {
    nums <- numeric_vars()
    cats <- categorical_vars()
    
    if (length(nums) >= 1) updateSelectInput(session, "num1", selected = nums[1])
    if (length(nums) >= 2) updateSelectInput(session, "num2", selected = nums[2])
    if (length(cats) >= 1) {
      updateSelectInput(session, "cat1", selected = cats[1])
      updateSelectInput(session, "group_var", selected = cats[1])
    }
    if (length(cats) >= 2) updateSelectInput(session, "cat2", selected = cats[2])
    
    showNotification("✓ Selections reset!", type = "message", duration = 2)
  })
  
  observeEvent(input$reset_app, {
    session$reload()
  })
  
  # Variable selectors
  output$var_select_ui <- renderUI({
    req(data_in())
    
    nums <- numeric_vars()
    cats <- categorical_vars()
    
    tagList(
      selectInput("num1", "Numeric X", choices = nums,
                  selected = if (length(nums) >= 1) nums[1] else NULL),
      div(class = "helper-text", "For single & two-sample analysis"),
      br(),
      
      selectInput("num2", "Numeric Y", choices = nums,
                  selected = if (length(nums) >= 2) nums[2] else if (length(nums) >= 1) nums[1] else NULL),
      div(class = "helper-text", "For correlation & scatter plots"),
      br(),
      
      selectInput("cat1", "Categorical A", choices = cats,
                  selected = if (length(cats) >= 1) cats[1] else NULL),
      div(class = "helper-text", "Primary categorical variable"),
      br(),
      
      selectInput("cat2", "Categorical B", choices = cats,
                  selected = if (length(cats) >= 2) cats[2] else if (length(cats) >= 1) cats[1] else NULL),
      div(class = "helper-text", "For cross-tabulation"),
      br(),
      
      selectInput("group_var", "Group", choices = cats,
                  selected = if (length(cats) >= 1) cats[1] else NULL),
      div(class = "helper-text", "For group comparisons")
    )
  })
  
  # Dashboard (continued in next part due to length...)
  
  output$dashboard_ui <- renderUI({
    req(data_in())
    df <- data_in()
    
    nums <- numeric_vars()
    cats <- categorical_vars()
    
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    n_numeric <- length(nums)
    n_categorical <- length(cats)
    
    missing_cols <- sapply(df, function(x) sum(is.na(x)))
    total_missing <- sum(missing_cols)
    missing_pct_total <- round(total_missing / (n_rows * n_cols) * 100, 1)
    
    top_cors <- get_top_correlations(df, n = 3)
    
    tagList(
      div(class = "content-card",
          div(class = "card-header", "📊 Dataset Overview"),
          
          div(class = "stats-grid",
              div(class = "stat-card primary",
                  div(class = "stat-label", "Total Rows"),
                  div(class = "stat-value", format(n_rows, big.mark = ",")),
                  div(class = "stat-subtitle", "observations")
              ),
              div(class = "stat-card info",
                  div(class = "stat-label", "Total Columns"),
                  div(class = "stat-value", n_cols),
                  div(class = "stat-subtitle", "variables")
              ),
              div(class = "stat-card success",
                  div(class = "stat-label", "Numeric"),
                  div(class = "stat-value", n_numeric),
                  div(class = "stat-subtitle", "quantitative")
              ),
              div(class = "stat-card purple",
                  div(class = "stat-label", "Categorical"),
                  div(class = "stat-value", n_categorical),
                  div(class = "stat-subtitle", "qualitative")
              ),
              div(class = "stat-card", class = if (missing_pct_total > 5) "warning" else "info",
                  div(class = "stat-label", "Missing Data"),
                  div(class = "stat-value", paste0(missing_pct_total, "%")),
                  div(class = "stat-subtitle", "overall")
              )
          ),
          
          div(class = "info-card",
              HTML(paste0(
                "📁 <strong>", input$file$name, "</strong><br>",
                "⏰ Uploaded: ", format(upload_time(), "%B %d, %Y at %I:%M %p")
              ))
          )
      ),
      
      if (n_numeric > 0) {
        div(class = "content-card",
            div(class = "card-header", "📈 Numeric Variables Quick Stats"),
            div(class = "visual-stats",
                lapply(1:min(4, n_numeric), function(i) {
                  var_name <- nums[i]
                  x <- df[[var_name]]
                  
                  div(class = "visual-stat-item",
                      h4(var_name),
                      div(class = "kv",
                          div(class = "k", "Mean"), div(class = "v", round(mean(x, na.rm = TRUE), 2)),
                          div(class = "k", "Median"), div(class = "v", round(median(x, na.rm = TRUE), 2)),
                          div(class = "k", "Std Dev"), div(class = "v", round(sd(x, na.rm = TRUE), 2)),
                          div(class = "k", "Missing"), div(class = "v", paste0(missing_pct(x), "%"))
                      )
                  )
                })
            )
        )
      },
      
      if (n_categorical > 0) {
        div(class = "content-card",
            div(class = "card-header", "🏷️ Categorical Variables Distribution"),
            div(class = "visual-stats",
                lapply(1:min(3, n_categorical), function(i) {
                  var_name <- cats[i]
                  x <- df[[var_name]]
                  freq <- table(x, useNA = "ifany")
                  top_level <- names(sort(freq, decreasing = TRUE))[1]
                  
                  div(class = "visual-stat-item",
                      h4(var_name),
                      div(class = "kv",
                          div(class = "k", "Unique"), div(class = "v", length(freq)),
                          div(class = "k", "Mode"), div(class = "v", as.character(top_level)),
                          div(class = "k", "Frequency"), div(class = "v", as.numeric(freq[top_level]))
                      )
                  )
                })
            )
        )
      },
      
      if (!is.null(top_cors)) {
        div(class = "content-card",
            div(class = "card-header", "🔗 Top Correlations"),
            lapply(1:nrow(top_cors), function(i) {
              row <- top_cors[i, ]
              cor_val <- round(row$Correlation, 3)
              cor_class <- if (abs(cor_val) > 0.7) "success" else if (abs(cor_val) > 0.4) "info" else "info"
              
              div(class = paste("info-card", cor_class),
                  HTML(paste0(
                    "<strong>", row$Var1, " ↔ ", row$Var2, "</strong><br>",
                    "Correlation: <strong>", cor_val, "</strong>"
                  ))
              )
            })
        )
      }
    )
  })
  
  # Data Preview Summary Cards
  output$data_summary_cards <- renderUI({
    df <- data_in()
    
    div(class = "visual-stats",
        div(class = "visual-stat-item",
            h4("Dataset Size"),
            div(class = "kv",
                div(class = "k", "Rows"), div(class = "v", format(nrow(df), big.mark = ",")),
                div(class = "k", "Columns"), div(class = "v", ncol(df))
            )
        ),
        div(class = "visual-stat-item",
            h4("Variable Types"),
            div(class = "kv",
                div(class = "k", "Numeric"), div(class = "v", length(numeric_vars())),
                div(class = "k", "Categorical"), div(class = "v", length(categorical_vars()))
            )
        ),
        div(class = "visual-stat-item",
            h4("Data Quality"),
            div(class = "kv",
                div(class = "k", "Complete"), div(class = "v", paste0(round((1 - sum(is.na(df))/(nrow(df)*ncol(df))) * 100, 1), "%")),
                div(class = "k", "Missing"), div(class = "v", sum(is.na(df)))
            )
        )
    )
  })
  
  output$table <- renderDT({
    df <- data_in()
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), 
                  class = "cell-border stripe hover",
                  rownames = FALSE)
  })
  
  # Single Numeric - Card Stats
  output$num_stats_cards <- renderUI({
    req(input$num1)
    df <- data_in()
    x <- df[[input$num1]]
    validate(need(is.numeric(x), "Select a numeric variable"))
    
    div(class = "visual-stats",
        div(class = "visual-stat-item",
            h4("Central Tendency"),
            div(class = "kv",
                div(class = "k", "Mean"), div(class = "v", round(mean(x, na.rm = TRUE), 3)),
                div(class = "k", "Median"), div(class = "v", round(median(x, na.rm = TRUE), 3)),
                div(class = "k", "Mode"), div(class = "v", get_mode(x))
            )
        ),
        div(class = "visual-stat-item",
            h4("Spread"),
            div(class = "kv",
                div(class = "k", "Std Dev"), div(class = "v", round(sd(x, na.rm = TRUE), 3)),
                div(class = "k", "Variance"), div(class = "v", round(var(x, na.rm = TRUE), 3)),
                div(class = "k", "Range"), div(class = "v", round(diff(range(x, na.rm = TRUE)), 3))
            )
        ),
        div(class = "visual-stat-item",
            h4("Quartiles"),
            div(class = "kv",
                div(class = "k", "Q1"), div(class = "v", round(quantile(x, 0.25, na.rm = TRUE), 3)),
                div(class = "k", "Q2 (Median)"), div(class = "v", round(quantile(x, 0.5, na.rm = TRUE), 3)),
                div(class = "k", "Q3"), div(class = "v", round(quantile(x, 0.75, na.rm = TRUE), 3))
            )
        ),
        div(class = "visual-stat-item",
            h4("Shape"),
            div(class = "kv",
                div(class = "k", "Skewness"), div(class = "v", round(moments::skewness(x, na.rm = TRUE), 3)),
                div(class = "k", "Kurtosis"), div(class = "v", round(moments::kurtosis(x, na.rm = TRUE), 3)),
                div(class = "k", "IQR"), div(class = "v", round(IQR(x, na.rm = TRUE), 3))
            )
        )
    )
  })
  
  output$hist <- renderPlot({
    req(input$num1)
    x <- data_in()[[input$num1]]
    validate(need(is.numeric(x), "Select a numeric variable"))
    
    hist(x,
         main = paste("Distribution of", input$num1),
         col = "#6366f1",
         xlab = input$num1,
         ylab = "Frequency",
         border = "white",
         breaks = 30,  # ← CHANGED TO 30
         las = 1,
         cex.main = 1.4,
         cex.lab = 1.2,
         col.main = "#111827",
         col.lab = "#374151")
    grid(col = "white", lty = 1, lwd = 1.5)
  })
  
  output$box <- renderPlot({
    req(input$num1)
    x <- data_in()[[input$num1]]
    validate(need(is.numeric(x), "Select a numeric variable"))
    
    boxplot(x,
            main = paste("Boxplot of", input$num1),
            horizontal = TRUE,
            col = "#06b6d4",
            border = "#1f2937",
            xlab = input$num1,
            las = 1,
            cex.main = 1.4,
            cex.lab = 1.2,
            col.main = "#111827",
            col.lab = "#374151",
            lwd = 2)
    grid(col = "gray85", lty = 1)
  })
  
  output$download_num_stats <- downloadHandler(
    filename = function() paste0("stats_", input$num1, "_", Sys.Date(), ".txt"),
    content = function(file) {
      x <- data_in()[[input$num1]]
      sink(file)
      cat("Statistics for:", input$num1, "\n\n")
      cat("Mean:", mean(x, na.rm = TRUE), "\n")
      cat("Median:", median(x, na.rm = TRUE), "\n")
      cat("SD:", sd(x, na.rm = TRUE), "\n")
      sink()
    }
  )
  
  output$download_hist <- downloadHandler(
    filename = function() paste0("histogram_", input$num1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 700, res = 120)
      x <- data_in()[[input$num1]]
      hist(x, main = paste("Distribution of", input$num1), 
           col = "#6366f1", border = "white", breaks = 30)
      dev.off()
    }
  )
  
  output$download_box <- downloadHandler(
    filename = function() paste0("boxplot_", input$num1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 700, res = 120)
      x <- data_in()[[input$num1]]
      boxplot(x, main = paste("Boxplot of", input$num1),
              horizontal = TRUE, col = "#06b6d4")
      dev.off()
    }
  )
  
  # Two Numeric - Cards
  output$two_num_stats_cards <- renderUI({
    req(input$num1, input$num2)
    df <- data_in()
    x <- df[[input$num1]]
    y <- df[[input$num2]]
    validate(need(is.numeric(x) && is.numeric(y), "Both must be numeric"))
    
    pear <- cor(x, y, method = "pearson", use = "complete.obs")
    spear <- cor(x, y, method = "spearman", use = "complete.obs")
    
    div(class = "visual-stats",
        div(class = "visual-stat-item",
            h4("Pearson Correlation"),
            div(class = "kv",
                div(class = "k", "r"), div(class = "v", round(pear, 4)),
                div(class = "k", "Strength"), div(class = "v", 
                    if (abs(pear) > 0.7) "Strong" else if (abs(pear) > 0.4) "Moderate" else "Weak")
            )
        ),
        div(class = "visual-stat-item",
            h4("Spearman Correlation"),
            div(class = "kv",
                div(class = "k", "ρ"), div(class = "v", round(spear, 4)),
                div(class = "k", "Type"), div(class = "v", "Rank-based")
            )
        )
    )
  })
  
  output$scatter <- renderPlot({
    req(input$num1, input$num2)
    df <- data_in()
    x <- df[[input$num1]]
    y <- df[[input$num2]]
    validate(need(is.numeric(x) && is.numeric(y), "Both must be numeric"))
    
    plot(x, y,
         main = paste(input$num1, "vs", input$num2),
         xlab = input$num1,
         ylab = input$num2,
         pch = 19,
         col = adjustcolor("#6366f1", alpha.f = 0.7),
         cex = 1.5,
         cex.main = 1.5,
         cex.lab = 1.2,
         col.main = "#111827",
         col.lab = "#374151",
         las = 1)
    
    abline(lm(y ~ x), col = "#ef4444", lwd = 3)
    grid(col = "gray85", lty = 1)
  })
  
  output$download_scatter <- downloadHandler(
    filename = function() paste0("scatter_", input$num1, "_vs_", input$num2, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 900, res = 120)
      x <- data_in()[[input$num1]]
      y <- data_in()[[input$num2]]
      plot(x, y, pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.7), cex = 1.5)
      abline(lm(y ~ x), col = "#ef4444", lwd = 3)
      dev.off()
    }
  )
  
  # Multi Numeric
  output$corr_tbl <- renderDT({
    df <- data_in()
    nums <- df[, numeric_vars(), drop = FALSE]
    validate(need(ncol(nums) >= 2, "Need at least 2 numeric columns"))
    cm <- cor(nums, use = "complete.obs")
    DT::datatable(round(cm, 3), options = list(dom = "t", scrollX = TRUE),
                  class = "cell-border stripe hover")
  })
  
  output$pairs_plot <- renderPlot({
    df <- data_in()
    nums <- df[, numeric_vars(), drop = FALSE]
    validate(need(ncol(nums) >= 2, "Need at least 2 numeric columns"))
    pairs(nums, main = "Scatterplot Matrix", pch = 19, 
          col = adjustcolor("#6366f1", alpha.f = 0.5), cex = 0.8,
          col.main = "#111827", cex.main = 1.4)
  })
  
  output$corr_plot <- renderPlot({
    df <- data_in()
    nums <- df[, numeric_vars(), drop = FALSE]
    validate(need(ncol(nums) >= 2, "Need at least 2 numeric columns"))
    cm <- cor(nums, use = "complete.obs")
    corrplot(cm, method = "circle", type = "full", diag = FALSE,
             col = colorRampPalette(c("#ef4444", "white", "#6366f1"))(100),
             tl.col = "#111827", tl.cex = 1)
  })
  
  output$download_corr_csv <- downloadHandler(
    filename = function() paste0("correlation_", Sys.Date(), ".csv"),
    content = function(file) {
      cm <- cor(data_in()[, numeric_vars(), drop = FALSE], use = "complete.obs")
      write.csv(cm, file, row.names = TRUE)
    }
  )
  
  output$download_pairs <- downloadHandler(
    filename = function() paste0("pairs_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 1200, res = 120)
      pairs(data_in()[, numeric_vars(), drop = FALSE], pch = 19,
            col = adjustcolor("#6366f1", alpha.f = 0.5))
      dev.off()
    }
  )
  
  output$download_corrplot <- downloadHandler(
    filename = function() paste0("corrplot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 1000, res = 120)
      cm <- cor(data_in()[, numeric_vars(), drop = FALSE], use = "complete.obs")
      corrplot(cm, method = "circle", 
               col = colorRampPalette(c("#ef4444", "white", "#6366f1"))(100))
      dev.off()
    }
  )
  
  # Single Categorical - Cards
  output$cat_stats_cards <- renderUI({
    req(input$cat1)
    x <- data_in()[[input$cat1]]
    validate(need(is_categorical(x), "Select a categorical variable"))
    
    freq <- table(x, useNA = "ifany")
    prop <- prop.table(freq)
    
    div(class = "visual-stats",
        div(class = "visual-stat-item",
            h4("Summary"),
            div(class = "kv",
                div(class = "k", "Categories"), div(class = "v", length(freq)),
                div(class = "k", "Most Common"), div(class = "v", names(which.max(freq))),
                div(class = "k", "Max Count"), div(class = "v", max(freq))
            )
        )
    )
  })

  
  output$bar <- renderPlot({
    req(input$cat1)
    x <- data_in()[[input$cat1]]
    validate(need(is_categorical(x), "Select a categorical variable"))
    
    freq <- table(x, useNA = "ifany")
    cols <- rainbow(length(freq))
    
    barplot(freq,
            main = paste("Distribution of", input$cat1),
            col = cols,
            border = "white",
            las = 2,
            cex.names = 0.9,
            cex.main = 1.4,
            cex.lab = 1.2,
            col.main = "#111827",
            ylab = "Frequency")
    grid(col = "white", lty = 1, lwd = 1.5)
  })
  
  output$pie <- renderPlot({
    req(input$cat1)
    x <- data_in()[[input$cat1]]
    validate(need(is_categorical(x), "Select a categorical variable"))
    
    freq <- table(x, useNA = "ifany")
    prop <- prop.table(freq)
    cols <- rainbow(length(freq))
    
    pie(freq,
        main = paste("Pie Chart:", input$cat1),
        col = cols,
        labels = paste(names(freq), "\n", round(prop * 100, 1), "%"),
        cex.main = 1.4,
        col.main = "#111827",
        border = "white",
        cex = 1.1)
  })
  
  output$download_bar <- downloadHandler(
    filename = function() paste0("bar_", input$cat1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 700, res = 120)
      freq <- table(data_in()[[input$cat1]], useNA = "ifany")
      cols <- rainbow(length(freq))
      barplot(freq, col = cols, border = "white")
      dev.off()
    }
  )
  
  output$download_pie <- downloadHandler(
    filename = function() paste0("pie_", input$cat1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 1000, res = 120)
      freq <- table(data_in()[[input$cat1]], useNA = "ifany")
      prop <- prop.table(freq)
      cols <- rainbow(length(freq))
      pie(freq, col = cols, labels = paste(names(freq), "\n", round(prop * 100, 1), "%"))
      dev.off()
    }
  )
  
  # Two Categorical
  output$freq_table <- renderDT({
    req(input$cat1, input$cat2)
    df <- data_in()
    a <- df[[input$cat1]]
    b <- df[[input$cat2]]
    validate(need(is_categorical(a) && is_categorical(b), "Both must be categorical"))
    
    tab <- table(a, b, useNA = "ifany")
    DT::datatable(as.data.frame.matrix(tab), options = list(dom = "t", scrollX = TRUE),
                  class = "cell-border stripe hover")
  })
  
  output$prop_table <- renderDT({
    req(input$cat1, input$cat2)
    df <- data_in()
    a <- df[[input$cat1]]
    b <- df[[input$cat2]]
    validate(need(is_categorical(a) && is_categorical(b), "Both must be categorical"))
    
    tab <- table(a, b, useNA = "ifany")
    prop_tab <- prop.table(tab)
    DT::datatable(round(as.data.frame.matrix(prop_tab), 4), 
                  options = list(dom = "t", scrollX = TRUE),
                  class = "cell-border stripe hover")
  })
  
  output$download_freq_csv <- downloadHandler(
    filename = function() paste0("freq_", input$cat1, "_", input$cat2, "_", Sys.Date(), ".csv"),
    content = function(file) {
      tab <- table(data_in()[[input$cat1]], data_in()[[input$cat2]], useNA = "ifany")
      write.csv(as.data.frame.matrix(tab), file, row.names = TRUE)
    }
  )
  
  output$download_prop_csv <- downloadHandler(
    filename = function() paste0("prop_", input$cat1, "_", input$cat2, "_", Sys.Date(), ".csv"),
    content = function(file) {
      tab <- table(data_in()[[input$cat1]], data_in()[[input$cat2]], useNA = "ifany")
      write.csv(round(as.data.frame.matrix(prop.table(tab)), 4), file, row.names = TRUE)
    }
  )
  
  output$stacked <- renderPlot({
    req(input$cat1, input$cat2)
    tab <- table(data_in()[[input$cat1]], data_in()[[input$cat2]], useNA = "ifany")
    cols <- c("#6366f1", "#a855f7", "#ec4899", "#10b981", "#f97316")
    barplot(tab, main = "Stacked Bar Plot", 
            col = cols[1:nrow(tab)],
            border = "white",
            las = 1,
            cex.main = 1.3,
            col.main = "#111827")
    legend("topright", legend = rownames(tab), fill = cols[1:nrow(tab)], cex = 0.9)
    grid(col = "white", lty = 1)
  })
  
  output$side <- renderPlot({
    req(input$cat1, input$cat2)
    tab <- table(data_in()[[input$cat1]], data_in()[[input$cat2]], useNA = "ifany")
    cols <- c("#6366f1", "#a855f7", "#ec4899", "#10b981", "#f97316")
    barplot(tab, beside = TRUE, main = "Side-by-Side Bar Plot",
            col = cols[1:nrow(tab)],
            border = "white",
            las = 1,
            cex.main = 1.3,
            col.main = "#111827")
    legend("topright", legend = rownames(tab), fill = cols[1:nrow(tab)], cex = 0.9)
    grid(col = "white", lty = 1)
  })
  
  output$mosaic <- renderPlot({
    req(input$cat1, input$cat2)
    tab <- table(data_in()[[input$cat1]], data_in()[[input$cat2]], useNA = "ifany")
    mosaicplot(tab, main = "Mosaic Plot",
               color = c("#6366f1", "#a855f7", "#ec4899", "#10b981"),
               cex.axis = 1,
               col.main = "#111827",
               cex.main = 1.3)
  })
  
  # Hypothesis Tests
  output$tests_out_ui <- renderUI({
    df <- data_in()
    req(input$num1)
    
    x <- df[[input$num1]]
    validate(need(is.numeric(x), "Choose a numeric variable"))
    
    sh <- shapiro.test(x)
    is_normal <- sh$p.value > 0.05
    
    tt1 <- t.test(x, mu = input$mu0)
    wx1 <- wilcox.test(x, mu = input$mu0)
    
    two_ok <- FALSE
    comp_label <- NULL
    tt2 <- NULL
    wx2 <- NULL
    
    req(input$group_var)
    g <- df[[input$group_var]]
    
    if (is_categorical(g)) {
      g <- as.factor(g)
      lv <- levels(g)
      
      if (length(lv) >= 2) {
        keep <- g %in% lv[1:2] & !is.na(g) & !is.na(x)
        g2 <- droplevels(g[keep])
        x2 <- x[keep]
        
        if (length(unique(g2)) == 2 && length(x2) > 2) {
          tt2 <- t.test(x2 ~ g2, var.equal = input$equal_var)
          wx2 <- wilcox.test(x2 ~ g2, exact = FALSE)
          two_ok <- TRUE
          comp_label <- paste(levels(g2)[1], "vs", levels(g2)[2])
        }
      }
    }
    
    tagList(
      div(class = "card-header",
          paste("📊 Statistical Tests:", input$num1),
          span(class = "badge", paste("μ₀ =", input$mu0))
      ),
      
      div(class = "test-card",
          h4("🔬 Normality Test (Shapiro-Wilk)"),
          div(class = "kv",
              div(class = "k", "W statistic"), div(class = "v", round(sh$statistic, 4)),
              div(class = "k", "p-value"), div(class = "v", fmt_p(sh$p.value)),
              div(class = "k", "Sample size"), div(class = "v", sum(!is.na(x)))
          ),
          div(class = "interpretation",
              if (is_normal) {
                "✓ Data appears normally distributed (p > 0.05)"
              } else {
                "⚠ Data may not be normal (p < 0.05) — consider non-parametric tests"
              }
          )
      ),
      
      div(class = "test-grid",
          div(class = "test-card",
              h4("📈 One-Sample t-test"),
              div(class = "kv",
                  div(class = "k", "t statistic"), div(class = "v", round(tt1$statistic, 4)),
                  div(class = "k", "df"), div(class = "v", round(tt1$parameter, 2)),
                  div(class = "k", "p-value"), div(class = "v", fmt_p(tt1$p.value)),
                  div(class = "k", "Sample mean"), div(class = "v", round(tt1$estimate, 4)),
                  div(class = "k", "95% CI"), div(class = "v",
                      paste0("[", round(tt1$conf.int[1], 3), ", ", round(tt1$conf.int[2], 3), "]"))
              ),
              div(class = "interpretation",
                  if (tt1$p.value < 0.05) {
                    paste0("✓ p < 0.05 → Reject H₀: Mean ≠ ", input$mu0)
                  } else {
                    paste0("✗ p ≥ 0.05 → Fail to reject H₀: Mean = ", input$mu0)
                  }
              )
          ),
          
          div(class = "test-card",
              h4("📊 Wilcoxon Signed-Rank"),
              div(class = "kv",
                  div(class = "k", "V statistic"), div(class = "v", round(wx1$statistic, 4)),
                  div(class = "k", "p-value"), div(class = "v", fmt_p(wx1$p.value))
              ),
              div(class = "interpretation", "Non-parametric alternative for non-normal data")
          ),
          
          if (two_ok) {
            div(class = "test-card",
                h4(paste("👥 Two-Sample Comparison")),
                div(class = "kv",
                    div(class = "k", "Groups"), div(class = "v", comp_label),
                    div(class = "k", "t-test p-value"), div(class = "v", fmt_p(tt2$p.value)),
                    div(class = "k", "Wilcoxon p-value"), div(class = "v", fmt_p(wx2$p.value))
                ),
                div(class = "interpretation",
                    if (tt2$p.value < 0.05) {
                      "✓ p < 0.05 → Groups differ significantly"
                    } else {
                      "✗ p ≥ 0.05 → No significant difference between groups"
                    }
                )
            )
          }
      )
    )
  })
}

shinyApp(ui, server)

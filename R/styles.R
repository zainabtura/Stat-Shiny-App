custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800&display=swap');

:root {
  --bg: #f8f9ff;
  --panel: #ffffff;
  --panel-soft: #f4f3ff;
  --ink: #132a23;
  --muted: #5d7268;
  --line: #d6e3dc;
  --brand: #6366f1;
  --brand-dark: #4f46e5;
  --brand-soft: #eef2ff;
  --accent: #8b5cf6;
  --success: #10b981;
  --warning: #f59e0b;
  --danger: #ef4444;
  --sidebar: linear-gradient(180deg, #667eea 0%, #764ba2 100%);
}

* {
  box-sizing: border-box;
  font-family: 'Poppins', sans-serif;
}

html, body {
  min-height: 100%;
}

body {
  margin: 0;
  background:
    radial-gradient(circle at top left, rgba(102, 126, 234, 0.16), transparent 26%),
    radial-gradient(circle at bottom right, rgba(240, 147, 251, 0.11), transparent 24%),
    var(--bg);
  color: var(--ink);
}

.container-fluid {
  margin: 0;
  padding: 0;
}

.app-shell {
  min-height: 100vh;
}

.shell-topbar {
  position: sticky;
  top: 0;
  z-index: 30;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 20px;
  padding: 18px 24px;
  background: rgba(255, 255, 255, 0.9);
  backdrop-filter: blur(18px);
  border-bottom: 1px solid rgba(19, 42, 35, 0.08);
}

.topbar-left {
  display: flex;
  align-items: center;
  gap: 16px;
}

.shell-toggle.btn {
  width: 40px;
  height: 40px;
  padding: 0;
  border-radius: 12px;
  border: 1px solid rgba(255, 255, 255, 0.14);
  background: rgba(255, 255, 255, 0.08);
  color: rgba(239, 252, 247, 0.92);
  box-shadow: none;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}

.shell-toggle .action-label {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 100%;
  height: 100%;
}

.shell-toggle.btn:hover {
  transform: none;
  background: rgba(255, 255, 255, 0.16);
  border-color: rgba(255, 255, 255, 0.22);
  color: #ffffff;
  box-shadow: none;
}

.brand-lockup {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.brand-title {
  font-size: 24px;
  font-weight: 800;
  color: var(--ink);
  line-height: 1.1;
}

.brand-subtitle {
  color: var(--muted);
  font-size: 13px;
}

.topbar-status {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  padding: 10px 14px;
  border-radius: 999px;
  background: var(--panel);
  border: 1px solid rgba(19, 42, 35, 0.08);
  color: var(--muted);
  font-size: 13px;
  font-weight: 600;
  white-space: nowrap;
}

.topbar-status strong {
  color: var(--brand-dark);
}

.shell-body {
  display: flex;
  align-items: stretch;
  min-height: 100vh;
}

.study-sidebar {
  width: 320px;
  flex: 0 0 320px;
  align-self: flex-start;
  position: sticky;
  top: 0;
  height: 100vh;
  background: var(--sidebar);
  color: #effcf7;
  transition: width 0.25s ease, flex-basis 0.25s ease;
  box-shadow: 12px 0 40px rgba(8, 26, 24, 0.18);
}

.sidebar-toggle-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  margin-bottom: 18px;
  padding: 0 4px;
}

.sidebar-brand-title {
  font-size: 16px;
  font-weight: 800;
  color: #ffffff;
  line-height: 1.2;
  letter-spacing: 0.01em;
}

.app-shell.sidebar-collapsed .sidebar-toggle-row {
  justify-content: center;
  padding: 0;
}

.app-shell.sidebar-collapsed .sidebar-brand-title {
  display: none;
}

.sidebar-scroll {
  height: 100%;
  overflow-y: auto;
  padding: 22px 18px 26px;
}

.sidebar-nav {
  display: grid;
  gap: 18px;
  margin-bottom: 22px;
}

.nav-group {
  display: grid;
  gap: 8px;
}

.nav-group-title {
  padding: 0 12px;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: rgba(234, 255, 247, 0.58);
}

.nav-button.btn {
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: flex-start;
  gap: 12px;
  padding: 13px 14px;
  border-radius: 14px;
  border: none;
  background: transparent;
  color: rgba(239, 252, 247, 0.85);
  box-shadow: none;
  text-align: left;
  transition: background 0.2s ease, color 0.2s ease, transform 0.2s ease;
}

.nav-button .action-label {
  display: flex;
  align-items: center;
  gap: 12px;
  width: 100%;
}

.nav-button.btn i {
  width: 18px;
  text-align: center;
  font-size: 15px;
}

.nav-button.btn:hover {
  transform: none;
  background: rgba(255, 255, 255, 0.08);
  color: #ffffff;
  box-shadow: none;
}

.nav-button.btn.is-active {
  background: linear-gradient(135deg, rgba(255, 255, 255, 0.22) 0%, rgba(199, 210, 254, 0.28) 100%);
  color: #ffffff;
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.1);
}

.nav-button.btn[disabled],
.study-card.btn[disabled],
.home-feature-card.btn[disabled],
.home-hero-button.btn[disabled] {
  opacity: 0.42;
  cursor: not-allowed;
  pointer-events: none;
}

.nav-label {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.sidebar-section {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid rgba(224, 231, 255, 0.14);
  border-radius: 18px;
  padding: 18px;
  margin-bottom: 16px;
}

.section-title {
  margin: 0 0 14px 0;
  color: #ebfff7;
  font-size: 12px;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.helper-text {
  color: rgba(232, 252, 244, 0.68);
  font-size: 12px;
  line-height: 1.5;
}

.workspace-main {
  flex: 1 1 auto;
  min-width: 0;
  padding: 28px 40px 42px;
  overflow-x: hidden;
}

.tab-content {
  padding: 0;
  background: transparent;
}

.tab-pane {
  animation: fadeInUp 0.28s ease;
}

@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(16px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.home-panel {
  display: flex;
  justify-content: center;
  padding: 0;
}

.home-shell {
  width: 100%;
  max-width: 1100px;
  display: grid;
  gap: 28px;
}

/* ── Hero ── */
.hero-section {
  position: relative;
  padding: 56px 52px 52px;
  border-radius: 32px;
  overflow: hidden;
  background: linear-gradient(160deg, #f8f9ff 0%, #eef2ff 40%, #e0e7ff 100%);
  border: 1px solid rgba(99, 102, 241, 0.08);
}

.hero-bg-shape {
  position: absolute;
  border-radius: 50%;
  pointer-events: none;
}

.hero-bg-1 {
  width: 500px;
  height: 500px;
  top: -200px;
  right: -100px;
  background: radial-gradient(circle, rgba(99, 102, 241, 0.15), transparent 65%);
}

.hero-bg-2 {
  width: 400px;
  height: 400px;
  bottom: -200px;
  left: -80px;
  background: radial-gradient(circle, rgba(139, 92, 246, 0.10), transparent 65%);
}

.hero-inner {
  position: relative;
  z-index: 2;
  max-width: 620px;
}

.hero-badge-row {
  margin-bottom: 24px;
}

.hero-badge {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 8px 16px 8px 12px;
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.7);
  border: 1px solid rgba(99, 102, 241, 0.12);
  color: var(--brand-dark);
  font-size: 12px;
  font-weight: 700;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  backdrop-filter: blur(8px);
}

.hero-badge i {
  color: var(--brand);
}

.hero-title {
  margin: 0 0 18px;
  font-size: 48px;
  font-weight: 800;
  line-height: 1.1;
  color: var(--ink);
  letter-spacing: -0.02em;
}

.hero-accent {
  background: linear-gradient(135deg, var(--brand) 0%, #8b5cf6 60%, #a78bfa 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.hero-subtitle {
  margin: 0 0 32px;
  color: var(--muted);
  font-size: 16px;
  line-height: 1.75;
}

.hero-cta-row {
  display: flex;
  align-items: center;
  gap: 18px;
  flex-wrap: wrap;
}

.hero-cta-primary.btn {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  padding: 15px 30px;
  border-radius: 16px;
  font-size: 15px;
  font-weight: 700;
  border: none;
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
  box-shadow: 0 8px 28px rgba(99, 102, 241, 0.30);
  transition: all 0.25s ease;
}

.hero-cta-primary.btn:hover {
  transform: translateY(-3px);
  box-shadow: 0 14px 36px rgba(99, 102, 241, 0.40);
  color: #ffffff;
}

.hero-cta-hint {
  display: flex;
  align-items: center;
  gap: 6px;
  color: var(--muted);
  font-size: 13px;
}

.hero-cta-hint i {
  color: var(--brand);
  font-size: 14px;
}

/* ── Preview Stats Bar ── */
.preview-bar {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0;
  padding: 22px 36px;
  border-radius: 22px;
  background: #ffffff;
  border: 1px solid rgba(0, 0, 0, 0.06);
  box-shadow: 0 4px 20px rgba(15, 23, 42, 0.04);
}

.preview-stat {
  display: flex;
  align-items: center;
  gap: 14px;
  flex: 1;
  justify-content: center;
}

.preview-stat-icon {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 44px;
  height: 44px;
  border-radius: 14px;
  background: var(--brand-soft);
  color: var(--brand);
  font-size: 18px;
}

.preview-stat-text {
  display: grid;
  gap: 2px;
}

.preview-stat-num {
  font-size: 20px;
  font-weight: 800;
  color: var(--ink);
  line-height: 1.2;
}

.preview-stat-label {
  color: var(--muted);
  font-size: 12px;
  font-weight: 600;
}

.preview-divider {
  width: 1px;
  height: 40px;
  margin: 0 24px;
  background: rgba(0, 0, 0, 0.08);
  flex-shrink: 0;
}

/* ── Bento Grid ── */
.bento {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: auto auto;
  gap: 18px;
}

.bento-card.btn {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  text-align: left;
  padding: 28px 26px 24px;
  border-radius: 24px;
  border: 1px solid rgba(0, 0, 0, 0.06);
  background: #ffffff;
  box-shadow: 0 4px 20px rgba(15, 23, 42, 0.04);
  transition: all 0.3s ease;
  cursor: pointer;
  min-height: 200px;
}

.bento-card.btn .action-label {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 0;
  width: 100%;
  height: 100%;
}

.bento-card.btn:hover {
  transform: translateY(-5px);
  box-shadow: 0 16px 40px rgba(15, 23, 42, 0.10);
  border-color: rgba(99, 102, 241, 0.15);
}

.bento-card.btn[disabled] {
  opacity: 0.45;
  cursor: not-allowed;
  pointer-events: none;
}

.bento-tag {
  display: inline-flex;
  padding: 5px 12px;
  border-radius: 999px;
  background: var(--brand-soft);
  color: var(--brand-dark);
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  margin-bottom: 10px;
}

.bento-heading {
  font-size: 20px;
  font-weight: 800;
  color: var(--ink);
  line-height: 1.25;
  margin-bottom: 8px;
}

.bento-copy {
  color: var(--muted);
  font-size: 13px;
  line-height: 1.65;
  white-space: normal;
}

.bento-icon-block {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 52px;
  height: 52px;
  border-radius: 16px;
  font-size: 22px;
  margin-bottom: 16px;
}

.bento-icon-blue   { background: #eff6ff; color: #2563eb; }
.bento-icon-indigo { background: #eef2ff; color: #4f46e5; }

/* ── Bento Visual Elements ── */
.bento-visual {
  display: flex;
  align-items: flex-end;
  gap: 8px;
  height: 72px;
  margin-bottom: 18px;
  padding: 8px 0;
}

.bento-mini-bar {
  width: 28px;
  border-radius: 6px 6px 2px 2px;
  background: linear-gradient(180deg, var(--brand) 0%, #a5b4fc 100%);
  opacity: 0.7;
  transition: opacity 0.2s ease;
}

.bento-card:hover .bento-mini-bar {
  opacity: 1;
}

.bento-visual-check {
  flex-direction: column;
  align-items: flex-start;
  height: auto;
  gap: 6px;
  padding: 12px 16px;
  border-radius: 14px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
}

.bento-check-row {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  font-weight: 600;
}

.bento-check-row span {
  color: var(--ink);
}

.bento-check-row i.fa-circle-check {
  color: #10b981;
}

.bento-check-row i.fa-circle-xmark {
  color: #ef4444;
}

/* ── More Section ── */
.more-section {
  display: grid;
  gap: 18px;
}

.more-header {
  padding: 0 4px;
}

.more-header h3 {
  margin: 0 0 6px;
  font-size: 22px;
  font-weight: 800;
  color: var(--ink);
}

.more-header p {
  margin: 0;
  color: var(--muted);
  font-size: 14px;
}

.more-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 14px;
}

.more-card.btn {
  display: flex;
  align-items: center;
  gap: 14px;
  width: 100%;
  padding: 18px 20px;
  border-radius: 18px;
  border: 1px solid rgba(0, 0, 0, 0.06);
  background: #ffffff;
  text-align: left;
  box-shadow: 0 2px 10px rgba(15, 23, 42, 0.03);
  transition: all 0.2s ease;
  cursor: pointer;
}

.more-card.btn .action-label {
  display: flex;
  align-items: center;
  gap: 14px;
  width: 100%;
}

.more-card.btn:hover {
  background: #fafaff;
  border-color: rgba(99, 102, 241, 0.16);
  box-shadow: 0 6px 20px rgba(99, 102, 241, 0.08);
  transform: translateY(-3px);
}

.more-card.btn[disabled] {
  opacity: 0.45;
  cursor: not-allowed;
  pointer-events: none;
}

.more-icon {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 42px;
  height: 42px;
  border-radius: 12px;
  font-size: 17px;
  flex-shrink: 0;
}

.more-icon-amber   { background: #fffbeb; color: #d97706; }
.more-icon-rose    { background: #fff1f2; color: #e11d48; }
.more-icon-teal    { background: #f0fdfa; color: #0d9488; }
.more-icon-orange  { background: #fff7ed; color: #ea580c; }
.more-icon-cyan    { background: #ecfeff; color: #0891b2; }
.more-icon-emerald { background: #ecfdf5; color: #059669; }
.more-icon-violet  { background: #f5f3ff; color: #7c3aed; }

.more-body {
  flex: 1;
  min-width: 0;
}

.more-title {
  font-size: 14px;
  font-weight: 700;
  color: var(--ink);
  line-height: 1.3;
}

.more-desc {
  margin-top: 2px;
  color: var(--muted);
  font-size: 12px;
  line-height: 1.5;
}

.home-static-button,
.nav-static-button {
  cursor: default;
}

.home-static-button:focus,
.nav-static-button:focus {
  outline: none;
  box-shadow: none;
}

.upload-page-wrapper {
  margin-bottom: 14px;
}

.upload-control-card {
  background:
    linear-gradient(180deg, rgba(255, 255, 255, 0.98) 0%, rgba(244, 251, 248, 0.98) 100%);
  border: 1px solid rgba(99, 102, 241, 0.12);
}

.upload-card-intro {
  margin: -2px 0 14px;
  display: grid;
  gap: 6px;
}

.upload-card-kicker {
  display: inline-flex;
  width: fit-content;
  padding: 6px 10px;
  border-radius: 999px;
  background: rgba(99, 102, 241, 0.10);
  color: var(--brand);
  font-size: 11px;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.upload-card-intro p {
  margin: 0;
  color: var(--muted);
  line-height: 1.6;
  font-size: 13px;
}

.upload-page-label {
  position: relative;
  min-height: 156px;
  gap: 8px;
  background:
    radial-gradient(circle at top right, rgba(29, 78, 216, 0.10), transparent 34%),
    linear-gradient(135deg, rgba(99, 102, 241, 0.14) 0%, rgba(255, 255, 255, 0.92) 100%);
  border: 2px dashed rgba(99, 102, 241, 0.42);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.7), 0 16px 30px rgba(99, 102, 241, 0.08);
}

.upload-page-label:hover {
  transform: translateY(-2px);
  border-color: rgba(99, 102, 241, 0.65);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.82), 0 22px 38px rgba(99, 102, 241, 0.12);
}

.upload-page-label .upload-icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 52px;
  height: 52px;
  border-radius: 16px;
  background: rgba(255, 255, 255, 0.72);
  color: var(--brand);
  font-size: 22px;
  box-shadow: 0 10px 22px rgba(99, 102, 241, 0.12);
}

.upload-badge {
  display: inline-flex;
  padding: 5px 10px;
  border-radius: 999px;
  background: rgba(99, 102, 241, 0.14);
  color: var(--brand);
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.upload-page-label .upload-text {
  color: var(--ink);
  font-size: 18px;
  font-weight: 800;
  line-height: 1.1;
}

.upload-page-label .upload-subtext {
  color: var(--muted);
  font-size: 12px;
  font-weight: 500;
}

.upload-checkbox {
  margin: 10px 0 14px;
  padding: 10px 14px;
  border-radius: 14px;
  background: linear-gradient(135deg, rgba(99, 102, 241, 0.07) 0%, rgba(29, 78, 216, 0.05) 100%);
  border: 1px solid rgba(99, 102, 241, 0.12);
}

.upload-checkbox-row {
  display: flex;
  align-items: center;
  justify-content: flex-start;
  gap: 12px;
  margin: 0;
  width: 100%;
}

.upload-checkbox-input {
  width: 18px;
  height: 18px;
  margin: 0;
  flex: 0 0 auto;
  accent-color: var(--brand);
}

.upload-checkbox-copy {
  flex: 0 1 auto;
  color: var(--ink);
  font-size: 14px;
  font-weight: 600;
  line-height: 1.4;
  text-align: left;
  margin: 0;
  cursor: pointer;
}

.sample-dataset-card {
  display: grid;
  gap: 12px;
  margin: 0 0 14px;
  padding: 14px 14px 12px;
  border-radius: 16px;
  background:
    radial-gradient(circle at top right, rgba(99, 102, 241, 0.08), transparent 36%),
    linear-gradient(135deg, rgba(255, 255, 255, 0.96) 0%, rgba(243, 244, 255, 0.96) 100%);
  border: 1px solid rgba(99, 102, 241, 0.12);
  box-shadow: 0 12px 28px rgba(99, 102, 241, 0.06);
}

.sample-dataset-header {
  display: grid;
  gap: 6px;
}

.sample-dataset-header-copy {
  color: var(--muted);
  font-size: 13px;
  line-height: 1.5;
}

.sample-dataset-card .form-group {
  margin-bottom: 0;
}

.sample-dataset-card label,
.sample-dataset-card .control-label {
  display: none !important;
}

.sample-dataset-actions {
  display: grid;
  grid-template-columns: 1fr;
  gap: 10px;
  align-items: end;
}

.sample-dataset-card .form-control,
.sample-dataset-card .selectize-input {
  min-height: 44px;
  border-radius: 12px;
}

.sample-dataset-actions > .form-group,
.sample-dataset-actions > .shiny-input-container {
  margin-bottom: 0;
  min-width: 0;
}

.sample-dataset-button {
  width: 100%;
  min-height: 44px;
  padding: 10px 18px;
  margin-bottom: 0;
}

.dataset-page {
  display: grid;
  gap: 20px;
  justify-items: stretch;
}

.dataset-page-header {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 16px;
}

.dataset-page-title h2 {
  margin: 0;
  color: var(--ink);
  font-size: 30px;
  font-weight: 800;
}

.dataset-page-title p {
  margin: 8px 0 0 0;
  color: var(--muted);
  line-height: 1.7;
}

.upload-page .dataset-page-title p {
  margin-bottom: 10px;
}

.descriptive-page .dataset-page-title p {
  margin: 8px 0 18px 0;
}

.dataset-layout {
  display: grid;
  grid-template-columns: minmax(280px, 340px) minmax(0, 1fr);
  gap: 20px;
  align-items: start;
}

.upload-page .dataset-layout {
  margin-top: 8px;
}

.dataset-control-card {
  position: sticky;
  top: 104px;
}

.upload-page .dataset-control-card {
  position: static;
  top: auto;
}

.descriptive-page {
  gap: 0;
  background: transparent;
}

.descriptive-tab-shell {
  padding: 0;
  background: transparent;
  border-bottom: none;
  box-shadow: none;
  width: 100%;
  justify-self: stretch;
  border-radius: 0;
  margin-bottom: 0;
  overflow: visible;
  margin-left: 0;
  margin-right: 0;
}

.descriptive-tab-row {
  display: flex;
  justify-content: flex-start;
  flex-wrap: wrap;
  gap: 8px;
  align-items: flex-end;
  margin: 0;
  padding: 0;
  border-bottom: none;
  margin-bottom: 0;
  width: 100%;
}

.descriptive-tab-button.btn {
  min-height: auto;
  padding: 10px 20px;
  border-radius: 8px 8px 0 0;
  border: none;
  background: transparent;
  color: #6b7280;
  box-shadow: none;
  text-align: center;
  transition: background 0.2s ease, color 0.2s ease;
  margin-bottom: -1px;
  font-weight: 500;
  cursor: pointer;
}

.descriptive-tab-button.btn:hover {
  background: rgba(255, 255, 255, 0.65);
  color: #374151;
}

.descriptive-tab-button.btn.is-active {
  background: #6366f1;
  color: #ffffff;
  border-color: #6366f1;
  border-bottom: none;
  border-style: solid;
  border-width: 1px;
  border-bottom-width: 0;
  font-weight: 500;
  margin-bottom: -1px;
  position: relative;
  z-index: 3;
  border-radius: 8px 8px 0 0;
}

.descriptive-tab-button .action-label {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: auto;
}

.descriptive-tab-title {
  color: currentColor;
  font-size: 14px;
  font-weight: 500;
  line-height: 1.2;
}

.descriptive-mode-hidden {
  display: none;
}

.inferential-tab-shell {
  padding: 0;
  background: transparent;
  border-bottom: none;
  box-shadow: none;
  width: 100%;
  justify-self: stretch;
  border-radius: 0;
  margin-bottom: 0;
  overflow: visible;
  margin-left: 0;
  margin-right: 0;
  margin-top: 4px;
}

.inferential-tab-row {
  display: flex;
  justify-content: flex-start;
  flex-wrap: wrap;
  gap: 8px;
  align-items: flex-end;
  margin: 0;
  padding: 0;
  border-bottom: none;
  margin-bottom: 0;
  width: 100%;
}

.inferential-tab-button.btn {
  min-height: auto;
  padding: 10px 20px;
  border-radius: 8px 8px 0 0;
  border: none;
  background: transparent;
  color: #6b7280;
  box-shadow: none;
  text-align: center;
  transition: background 0.2s ease, color 0.2s ease;
  margin-bottom: -1px;
  font-weight: 500;
  cursor: pointer;
}

.inferential-tab-button.btn:hover {
  background: rgba(255, 255, 255, 0.65);
  color: #374151;
}

.inferential-tab-button.btn[disabled],
.inferential-tab-button.btn.is-disabled {
  opacity: 0.45;
  cursor: not-allowed;
  pointer-events: none;
}

.inferential-tab-button.btn.is-active {
  background: #6366f1;
  color: #ffffff;
  border-color: #6366f1;
  border-bottom: none;
  border-style: solid;
  border-width: 1px;
  border-bottom-width: 0;
  font-weight: 500;
  margin-bottom: -1px;
  position: relative;
  z-index: 3;
  border-radius: 8px 8px 0 0;
}

.inferential-tab-button .action-label {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: auto;
  white-space: nowrap;
}

.inferential-tab-title {
  color: currentColor;
  font-size: 14px;
  font-weight: 500;
  line-height: 1.2;
}

div.inferential-tab-shell + div.inferential-results-shell .tests-panel.tests-input-panel {
  margin-top: 0 !important;
}

.inferential-results-shell {
  min-width: 0;
  margin: 0;
  padding: 0;
  position: relative;
  z-index: 2;
}

.indicator-input-grid {
  max-width: none;
  margin-top: 10px;
  gap: 16px 18px;
  align-items: end;
}

.indicator-input-grid.indicator-input-grid-standard,
.indicator-input-grid.indicator-input-grid-two-vars {
  grid-template-columns: repeat(4, minmax(220px, 1fr));
}

.indicator-input-grid .tests-control {
  min-width: 0;
  align-self: end;
}

.indicator-input-grid .tests-control > .form-group,
.indicator-input-grid .tests-control > .shiny-input-container {
  width: 100%;
  margin-bottom: 0;
}

.indicator-input-grid label,
.indicator-input-grid .control-label {
  min-height: 0;
  display: block;
}

.indicator-input-grid .form-control,
.indicator-input-grid .selectize-input,
.indicator-model-toggle .checkbox {
  width: 100% !important;
  min-height: 48px;
  box-sizing: border-box;
}

.indicator-model-toggle {
  display: flex;
  flex-direction: column;
  min-width: 0;
}

.indicator-model-toggle .checkbox {
  display: flex;
  align-items: center;
  width: 100%;
  min-height: 48px;
  padding: 10px 14px;
  border: 1px solid #e5e7eb;
  border-radius: 12px;
  background: #ffffff;
  margin: 0;
  gap: 0;
  box-sizing: border-box;
}

.indicator-model-toggle .checkbox label {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  width: 100%;
  justify-content: flex-start;
  white-space: normal;
  margin: 0;
  line-height: 1.35;
  color: #4b5563;
  font-weight: 600;
}

.indicator-model-toggle .checkbox input[type='checkbox'],
.indicator-model-toggle .checkbox label input[type='checkbox'] {
  position: static;
  margin: 0;
}

@media (max-width: 1200px) {
  .indicator-input-grid.indicator-input-grid-standard,
  .indicator-input-grid.indicator-input-grid-two-vars {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

.indicator-empty-state-panel {
  display: grid;
  gap: 16px;
  max-width: 760px;
  margin-top: 22px;
  padding: 32px 34px;
  border-radius: 28px;
  background:
    radial-gradient(circle at top right, rgba(99, 102, 241, 0.08), transparent 32%),
    linear-gradient(135deg, rgba(245, 243, 255, 0.96) 0%, rgba(255, 255, 255, 0.98) 100%);
  box-shadow: 0 18px 38px rgba(15, 23, 42, 0.06);
}

.indicator-empty-state-kicker {
  display: inline-flex;
  width: fit-content;
  padding: 8px 14px;
  border-radius: 999px;
  background: rgba(99, 102, 241, 0.10);
  color: var(--brand-dark);
  font-size: 12px;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.indicator-empty-state-panel h3 {
  margin: 0;
  color: var(--ink);
  font-size: 28px;
  font-weight: 800;
  line-height: 1.2;
}

.indicator-empty-state-panel p {
  margin: 0;
  color: var(--muted);
  font-size: 15px;
  line-height: 1.8;
}

.indicator-empty-state-list {
  display: grid;
  gap: 12px;
}

.indicator-empty-state-item {
  display: grid;
  gap: 4px;
  padding: 14px 16px;
  border-radius: 18px;
  background: rgba(255, 255, 255, 0.78);
  border: 1px solid rgba(99, 102, 241, 0.10);
}

.indicator-empty-state-item strong {
  color: var(--ink);
  font-size: 13px;
  font-weight: 700;
}

.indicator-empty-state-item span {
  color: var(--muted);
  font-size: 14px;
  line-height: 1.7;
}

.inferential-results-shell::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 28px;
  height: 28px;
  background: transparent;
  z-index: 1;
  pointer-events: none;
}

.regression-page-shell > .tests-page-hero {
  margin-bottom: 34px;
}

.regression-tab-shell {
  margin-top: 4px;
}

.regression-results-shell {
  display: grid;
  gap: 28px;
  min-width: 0;
  width: 100%;
  max-width: 100%;
  overflow-x: hidden;
  contain: inline-size layout;
}

.regression-tab-shell.regression-scope-single + .regression-results-shell::before {
  background: #6366f1;
}

.regression-lead-panel {
  margin-top: 0;
  margin-bottom: 12px;
  padding-bottom: 34px;
}

.regression-lead-panel .info-card {
  margin-bottom: 22px;
}

.regression-results-shell > .content-card,
.regression-results-shell > .tests-panel,
.regression-results-shell .table-card,
.regression-results-shell .shiny-spinner-output-container,
.regression-results-shell .dataTables_wrapper {
  min-width: 0;
  width: 100%;
  max-width: 100%;
  box-sizing: border-box;
}

.regression-results-shell > .content-card,
.regression-results-shell > .tests-panel {
  margin-bottom: 0;
}

.regression-results-shell .content-card,
.regression-results-shell .tests-panel {
  overflow-x: auto;
  overflow-y: hidden;
}

.regression-page-shell .tests-page-hero,
.regression-page-shell .tests-page-hero h2,
.regression-page-shell .tests-page-hero p,
.regression-results-shell .card-header {
  min-width: 0;
  max-width: 100%;
  overflow-wrap: anywhere;
}

.regression-card-header {
  display: block;
}

.regression-card-title {
  display: block;
  min-width: 0;
  max-width: 100%;
  white-space: normal;
  overflow-wrap: anywhere;
}

.regression-results-shell .visual-stats {
  min-width: 0;
}

.regression-results-shell .visual-stat-item {
  min-width: 0;
}

.regression-metrics-grid {
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  width: 100%;
  max-width: 100%;
}

.regression-metrics-grid .visual-stat-item {
  overflow: hidden;
}

.regression-metrics-grid .kv {
  grid-template-columns: minmax(0, 1fr) auto;
  min-width: 0;
}

.regression-metrics-grid .kv .k,
.regression-metrics-grid .kv .v {
  min-width: 0;
  overflow-wrap: anywhere;
}

.regression-results-shell .table-card {
  overflow-x: auto;
}

.regression-results-shell table.dataTable {
  width: 100% !important;
}

.regression-results-shell .dataTables_wrapper,
.regression-results-shell .dataTables_scroll,
.regression-results-shell .dataTables_scrollHead,
.regression-results-shell .dataTables_scrollHeadInner,
.regression-results-shell .dataTables_scrollBody,
.regression-results-shell .shiny-plot-output,
.regression-results-shell .html-fill-item,
.regression-results-shell canvas,
.regression-results-shell img,
.regression-results-shell svg {
  max-width: 100% !important;
  min-width: 0 !important;
  box-sizing: border-box;
}

.regression-results-shell .dataTables_scrollHeadInner,
.regression-results-shell .dataTables_scrollHeadInner table {
  width: 100% !important;
}

.regression-mode-note {
  margin-bottom: 0;
}

div.inferential-tab-shell.inferential-scope-one + div.inferential-results-shell::before {
  background: #6366f1;
}

.tests-panel {
  position: relative;
  z-index: 2;
}

.tests-input-grid.tests-input-grid-two {
  max-width: none;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 16px 18px;
  margin-top: 10px;
}

.tests-input-grid.tests-input-grid-three {
  max-width: none;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 16px 18px;
  margin-top: 10px;
}

.descriptive-filter-row {
  display: grid;
  grid-template-columns: repeat(4, minmax(180px, 1fr));
  gap: 16px;
  align-items: end;
  padding-top: 2px;
}

.descriptive-filter-row-three {
  display: grid;
  grid-template-columns: repeat(3, minmax(220px, 1fr));
  gap: 16px;
  align-items: end;
  padding-top: 2px;
}

.descriptive-filter-block {
  min-width: 0;
}

/* Keep checkbox controls aligned with select inputs in descriptive filters. */
.descriptive-filter-block .checkbox {
  display: flex;
  align-items: center;
  min-height: 44px;
  margin: 0;
  padding-top: 6px;
}

.descriptive-filter-block .checkbox label {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  margin: 0;
  padding-left: 0;
  color: var(--ink);
  font-size: 13px;
  font-weight: 600;
  line-height: 1.25;
}

.descriptive-filter-block .checkbox input[type='checkbox'] {
  position: static;
  margin: 0;
  width: 18px;
  height: 18px;
  accent-color: var(--brand);
}

.descriptive-filter-block-wide {
  grid-column: span 2;
}

.descriptive-filter-block-note {
  display: flex;
  align-items: center;
}

.descriptive-inline-hint {
  width: 100%;
  padding: 13px 15px;
  border-radius: 16px;
  background: #f9fafb;
  border: 1px solid #e5e7eb;
  color: #6b7280;
  font-size: 13px;
  line-height: 1.65;
}

.descriptive-plain-note {
  margin: 0;
  color: #6b7280;
  font-size: 13px;
  line-height: 1.6;
}

.descriptive-tab-shell .form-group {
  margin-bottom: 12px;
}

.descriptive-tab-shell label {
  color: #f8fafc;
  font-size: 13px;
  font-weight: 600;
}

.descriptive-content-shell {
  padding: 0 0 30px;
  display: grid;
  gap: 28px;
  background: transparent;
  border: none;
  box-shadow: none;
  margin-top: 0;
  border-radius: 0;
}

.descriptive-results-shell {
  min-width: 0;
  margin: 0;
  padding: 0;
  position: relative;
  z-index: 2;
}

.descriptive-results-shell::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 28px;
  height: 28px;
  background: transparent;
  z-index: 1;
}

.descriptive-tab-shell:has(.descriptive-tab-button:first-child.is-active)
  + .descriptive-content-shell .descriptive-results-shell::before {
  background: #6366f1;
}

.descriptive-results-shell > .dataset-results {
  background: #ffffff;
  border: 1px solid #eef2f7;
  border-radius: 28px;
  padding: 32px 40px 28px;
  box-shadow: 0 12px 28px rgba(15, 23, 42, 0.04);
  margin-top: 0;
  position: relative;
  z-index: 2;
}

.descriptive-overview-section {
  display: grid;
  gap: 18px;
}

.descriptive-section-intro {
  display: grid;
  gap: 6px;
}

.descriptive-section-intro h3 {
  margin: 0;
  color: #111827;
  font-size: 22px;
  font-weight: 700;
}

.descriptive-section-intro p {
  margin: 0;
  color: #6b7280;
  font-size: 14px;
  line-height: 1.7;
}

.descriptive-section-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
}

.descriptive-section-head h3 {
  margin: 0;
  color: #111827;
  font-size: 22px;
  font-weight: 700;
}

.descriptive-section-meta {
  color: #6b7280;
  font-size: 13px;
  font-weight: 500;
}

.descriptive-metric-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 16px;
}

.descriptive-metric-card {
  padding: 22px 22px 20px;
  border-radius: 20px;
  border: 1px solid #e5e7eb;
  background: #ffffff;
  box-shadow: none;
}

.descriptive-metric-icon {
  margin-bottom: 14px;
  color: #111827;
  font-size: 18px;
  line-height: 1;
}

.descriptive-metric-label {
  color: #6b7280;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.descriptive-metric-value {
  margin-top: 10px;
  color: #111827;
  font-size: 32px;
  font-weight: 700;
  line-height: 1.1;
}

.descriptive-metric-subtitle {
  margin-top: 6px;
  color: #9ca3af;
  font-size: 13px;
  font-weight: 500;
}

.descriptive-tab-content {
  display: grid;
  gap: 12px;
  margin: 0;
  padding: 0;
}

.descriptive-controls-panel {
  padding-bottom: 14px;
}

.descriptive-tab-content > .content-card {
  margin-bottom: 0;
}

.descriptive-panel-note {
  margin: -2px 0 16px;
  color: #6b7280;
  font-size: 13px;
  line-height: 1.7;
}

.descriptive-filter-row-two {
  grid-template-columns: repeat(2, minmax(220px, 1fr));
}

.descriptive-overview-section + .descriptive-results-shell {
  padding-top: 2px;
}

.descriptive-results {
  min-width: 0;
}

.dataset-results {
  min-width: 0;
}

.dataset-empty p {
  margin: 0 0 18px 0;
  color: var(--muted);
  line-height: 1.7;
}

.upload-empty-state {
  background:
    radial-gradient(circle at top right, rgba(99, 102, 241, 0.08), transparent 30%),
    linear-gradient(180deg, rgba(255, 255, 255, 0.97) 0%, rgba(244, 251, 248, 0.95) 100%);
  border: 1px solid rgba(99, 102, 241, 0.12);
}

.upload-empty-kicker {
  display: inline-flex;
  margin-bottom: 12px;
  padding: 6px 10px;
  border-radius: 999px;
  background: rgba(29, 78, 216, 0.08);
  color: var(--accent);
  font-size: 11px;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.upload-empty-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  gap: 16px;
}

.upload-empty-item {
  padding: 18px;
  border-radius: 18px;
  background: rgba(255, 255, 255, 0.78);
  border: 1px solid rgba(99, 102, 241, 0.10);
  box-shadow: 0 10px 24px rgba(19, 42, 35, 0.05);
}

.upload-empty-item h4 {
  margin: 0 0 8px 0;
  color: var(--brand-dark);
  font-size: 15px;
  font-weight: 700;
}

.upload-empty-item p {
  margin: 0;
  color: var(--muted);
  font-size: 13px;
  line-height: 1.6;
}

.upload-results-card {
  background:
    radial-gradient(circle at top right, rgba(29, 78, 216, 0.06), transparent 28%),
    linear-gradient(180deg, rgba(255, 255, 255, 0.97) 0%, rgba(246, 251, 248, 0.96) 100%);
  padding: 26px;
}

.upload-ready-message {
  margin-top: -4px;
}

.upload-summary-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 14px;
  margin-bottom: 16px;
}

.upload-stat-card {
  position: relative;
  min-height: 132px;
  padding: 18px 18px;
  border-radius: 18px;
  border: 1px solid rgba(99, 102, 241, 0.10);
  background: linear-gradient(180deg, rgba(255, 255, 255, 0.96) 0%, rgba(244, 251, 248, 0.96) 100%);
  box-shadow: 0 10px 24px rgba(19, 42, 35, 0.05);
  overflow: hidden;
}

.upload-stat-card::before {
  content: \"\";
  position: absolute;
  inset: 0 auto auto 0;
  width: 100%;
  height: 5px;
  background: linear-gradient(135deg, rgba(99, 102, 241, 0.95) 0%, rgba(29, 78, 216, 0.9) 100%);
}

.upload-stat-card-file {
  background:
    radial-gradient(circle at top right, rgba(99, 102, 241, 0.10), transparent 32%),
    linear-gradient(180deg, rgba(255, 255, 255, 0.98) 0%, rgba(242, 250, 247, 0.98) 100%);
}

.upload-stat-card-size {
  background:
    radial-gradient(circle at top right, rgba(29, 78, 216, 0.10), transparent 32%),
    linear-gradient(180deg, rgba(255, 255, 255, 0.98) 0%, rgba(244, 248, 255, 0.98) 100%);
}

.upload-stat-card-vars {
  background:
    radial-gradient(circle at top right, rgba(16, 185, 129, 0.10), transparent 32%),
    linear-gradient(180deg, rgba(255, 255, 255, 0.98) 0%, rgba(243, 252, 247, 0.98) 100%);
}

.upload-stat-title {
  margin-bottom: 12px;
  color: var(--brand-dark);
  font-size: 12px;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.upload-stat-value {
  color: var(--ink);
  font-size: 23px;
  font-weight: 800;
  line-height: 1.15;
}

.upload-stat-filename {
  max-width: 100%;
  font-size: 15px;
  overflow-wrap: anywhere;
}

.upload-stat-meta {
  margin-top: 10px;
  color: var(--muted);
  font-size: 12px;
  line-height: 1.6;
}

.upload-stat-split {
  display: grid;
  gap: 10px;
}

.upload-stat-pair {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 12px;
  padding-bottom: 12px;
  border-bottom: 1px solid rgba(19, 42, 35, 0.08);
}

.upload-stat-pair:last-child {
  padding-bottom: 0;
  border-bottom: none;
}

.upload-stat-label {
  color: var(--muted);
  font-size: 14px;
  font-weight: 600;
}

.upload-stat-number {
  color: var(--ink);
  font-size: 22px;
  font-weight: 800;
  line-height: 1;
}

.upload-preview-card {
  padding: 20px;
  border-radius: 20px;
  background: rgba(255, 255, 255, 0.88);
}

.upload-preview-note {
  margin: -2px 0 12px;
  color: var(--muted);
  font-size: 13px;
  line-height: 1.6;
}

.upload-preview-card .dataTables_wrapper {
  padding: 8px 8px 2px;
  border-radius: 16px;
  background: linear-gradient(180deg, rgba(246, 251, 248, 0.96) 0%, rgba(255, 255, 255, 0.98) 100%);
  border: 1px solid rgba(99, 102, 241, 0.10);
  width: 100%;
  overflow-x: auto;
}

.upload-preview-card table.dataTable {
  width: 98% !important;
  margin: 0 auto;
}

.upload-preview-card table.dataTable thead th {
  border-radius: 10px 10px 0 0;
  padding: 10px 12px;
  font-size: 13px;
}

.upload-preview-card table.dataTable tbody td {
  background: transparent;
  padding: 9px 12px;
  font-size: 13px;
}

.upload-preview-card .dataTables_info,
.upload-preview-card .dataTables_paginate {
  padding: 10px 4px 0 !important;
}

.upload-preview-card .dataTables_paginate .paginate_button {
  border-radius: 10px !important;
}

.content-card {
  background: rgba(255, 255, 255, 0.92);
  border-radius: 22px;
  padding: 28px;
  margin-bottom: 28px;
  border: 1px solid rgba(19, 42, 35, 0.08);
  box-shadow: 0 16px 34px rgba(19, 42, 35, 0.06);
}

.content-card > * + * {
  margin-top: 26px;
}

.card-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  margin: 0 0 22px 0;
  padding-bottom: 14px;
  border-bottom: 1px solid rgba(19, 42, 35, 0.08);
  font-size: 21px;
  font-weight: 700;
  color: var(--ink);
}

.stats-grid,
.visual-stats,
.test-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 18px;
}

.test-grid {
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 16px;
  align-items: stretch;
}

.tests-page-header {
  display: grid;
  gap: 8px;
  margin-bottom: 18px;
}

.tests-page-header h3 {
  margin: 0;
  color: var(--ink);
  font-size: 28px;
  font-weight: 800;
  line-height: 1.2;
}

.tests-page-header p {
  margin: 0;
  color: var(--muted);
  font-size: 14px;
  line-height: 1.7;
}

.tests-section-title {
  margin: 0 0 16px 0;
  color: var(--ink);
  font-size: 20px;
  font-weight: 700;
}

.tests-page-shell {
  display: grid;
  gap: 0;
}

.tests-page-hero {
  background: transparent;
  color: var(--ink);
  padding: 0;
  border-radius: 0;
  display: grid;
  gap: 6px;
}

.tests-page-hero h2 {
  margin: 0;
  font-size: 30px;
  font-weight: 800;
  line-height: 1.1;
}

.tests-page-hero p {
  margin: 0;
  color: var(--muted);
  font-size: 14px;
  line-height: 1.7;
  max-width: none;
  white-space: normal;
  overflow-wrap: anywhere;
}

/* Create a visible gap between the hero subtitle and the inferential tabs.
   (Descriptive uses subtitle bottom margin; we mirror that here.) */
.inferential-tab-shell {
  margin-top: 18px;
}

.tests-page-shell > .tests-page-hero {
  margin-bottom: 26px !important;
}

.tests-panel {
  background: #ffffff;
  border: 1px solid #e5e7eb;
  border-radius: 28px;
  padding: 28px 30px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
}

.tests-page-shell > .tests-panel + .tests-panel {
  margin-top: 0 !important;
}

.tests-input-panel {
  margin-top: 24px !important;
}

.tests-results-panel {
  margin-top: 24px !important;
}

.tests-input-section {
  padding: 24px 26px;
}

.tests-input-grid {
  display: grid;
  grid-template-columns: 1fr;
  gap: 16px;
  max-width: 520px;
  margin-top: 10px;
}

.tests-input-grid.tests-input-grid-location {
  max-width: none;
  grid-template-columns: repeat(4, minmax(200px, 1fr));
  gap: 16px 18px;
}

.tests-input-grid.tests-input-grid-normality {
  max-width: none;
  grid-template-columns: repeat(4, minmax(200px, 1fr));
  gap: 16px 18px;
}

.tests-input-grid .form-group {
  margin-bottom: 0;
}

.tests-input-grid label,
.tests-input-grid .control-label {
  color: #4b5563 !important;
  font-weight: 600;
}

.tests-input-grid .tests-control {
  min-width: 0;
}

/* Selectize sometimes keeps its container at an intrinsic min-width,
   which breaks the grid column layout. Force it to shrink. */
.tests-input-grid .selectize-control {
  width: 100% !important;
  min-width: 0;
}

.tests-control-checkbox {
  display: block;
}

.tests-control-checkbox .checkbox {
  align-items: center;
  margin-top: 0;
  gap: 12px;
  min-height: 44px;
  margin-left: 0;
  padding-left: 0;
}

.tests-control-checkbox .checkbox label {
  display: inline-flex;
  align-items: center;
  gap: 12px;
  margin: 0;
  color: #4b5563;
  line-height: 1.45;
  white-space: nowrap;
  word-break: normal;
  overflow-wrap: break-word;
  padding-left: 0;
  font-weight: 600;
}

.tests-control-checkbox .checkbox input[type='checkbox'],
.tests-control-checkbox .checkbox label input[type='checkbox'] {
  position: static;
  margin: 0;
}

.tests-results-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  margin: 8px 0 14px;
}

.tests-results-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 18px;
  align-items: stretch;
}

.tests-results-grid .test-card {
  display: flex;
  flex-direction: column;
  height: 100%;
}

.tests-action-row {
  margin-top: 16px;
}

.tests-analyze-btn.btn {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
  border-radius: 8px;
  padding: 12px 32px;
  font-weight: 600;
}

.tests-analyze-btn.btn:hover {
  background: linear-gradient(135deg, #4f46e5 0%, #7c3aed 100%);
}

.regression-builder-grid {
  max-width: none;
  grid-template-columns: repeat(3, minmax(220px, 1fr));
  gap: 18px;
  align-items: end;
}

.regression-builder-grid .tests-control {
  display: flex;
  flex-direction: column;
  min-width: 0;
}

.regression-build-action {
  justify-content: flex-end;
}

.regression-build-action label,
.regression-build-action .control-label {
  display: none !important;
}

.regression-build-btn.btn {
  width: 100%;
  min-height: 44px;
  margin-top: 0;
}

.regression-builder-note {
  margin-top: 18px;
}

.regression-predictor-control .selectize-control,
.regression-predictor-control .selectize-input,
.regression-builder-grid .form-control,
.regression-builder-grid .selectize-input {
  min-height: 44px;
}

.regression-builder-grid .selectize-input {
  display: flex;
  align-items: center;
}

.descriptive-num-stats-grid {
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 14px;
}

.descriptive-num-stats-grid.stats-count-5 {
  grid-template-columns: repeat(5, minmax(0, 1fr));
}

.descriptive-num-stats-grid.stats-count-6 {
  grid-template-columns: repeat(6, minmax(0, 1fr));
}

.descriptive-num-stats-grid .visual-stat-item {
  min-height: 150px;
  padding: 18px;
}

.stat-card,
.visual-stat-item,
.test-card,
.table-card {
  background: #ffffff;
  border-radius: 18px;
  border: 1px solid rgba(19, 42, 35, 0.08);
  box-shadow: 0 10px 24px rgba(19, 42, 35, 0.05);
}

.content-card,
.table-card,
.shiny-spinner-output-container,
.dataTables_wrapper {
  min-width: 0;
}

.stat-card {
  padding: 22px;
  border-left: 5px solid var(--brand);
}

.stat-card.primary { border-left-color: var(--brand); }
.stat-card.success { border-left-color: var(--success); }
.stat-card.warning { border-left-color: var(--warning); }
.stat-card.info { border-left-color: var(--accent); }
.stat-card.purple { border-left-color: #7c3aed; }

.stat-label {
  font-size: 12px;
  font-weight: 700;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.08em;
}

.stat-value {
  margin-top: 8px;
  font-size: 32px;
  font-weight: 800;
  color: var(--ink);
}

.stat-subtitle {
  margin-top: 4px;
  color: #8ca095;
  font-size: 13px;
}

.visual-stat-item,
.test-card,
.table-card {
  padding: 22px;
}

.test-card {
  min-width: 0;
  overflow: hidden;
  padding: 24px;
  border-radius: 12px;
}

.test-card-normality {
  border-top: 4px solid #6366f1;
  margin-bottom: 18px;
}

.test-card-parametric {
  border-top: 4px solid #2563eb;
}

.test-card-nonparametric {
  border-top: 4px solid #7c3aed;
}

.test-card-comparison {
  border-top: 4px solid #8b5cf6;
  margin-top: 18px;
}

.visual-stat-item h4,
.test-card h4,
.table-card h4 {
  margin: 0 0 14px 0;
  color: var(--brand-dark);
  font-size: 17px;
  font-weight: 700;
}

.table-card-title,
.card-subheader {
  font-size: 16px;
  font-weight: 700;
  color: var(--brand-dark);
  letter-spacing: 0.01em;
}

.tests-card-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  margin-bottom: 14px;
}

.tests-card-head h4 {
  margin: 0;
}

.test-chip {
  display: inline-flex;
  align-items: center;
  padding: 5px 10px;
  border-radius: 999px;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.02em;
  white-space: nowrap;
  border: 1px solid transparent;
}

.chip-diagnostic {
  background: #eef2ff;
  color: #6366f1;
  border-color: #c7d2fe;
}

.chip-parametric {
  background: #eef2ff;
  color: #4f46e5;
  border-color: #c7d2fe;
}

.chip-nonparametric {
  background: #ede9fe;
  color: #6d28d9;
  border-color: #c4b5fd;
}

.chip-comparison {
  background: #f5f3ff;
  color: #7c3aed;
  border-color: #d8b4fe;
}

.tests-summary-table {
  width: 100%;
  border-collapse: collapse;
  border: 1px solid rgba(19, 42, 35, 0.10);
  border-radius: 10px;
  overflow: hidden;
  background: #ffffff;
}

.tests-summary-table thead th {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
  font-size: 12px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  padding: 10px 12px;
  border-bottom: none;
}

.tests-summary-table tbody td {
  padding: 10px 12px;
  font-size: 13px;
  border-bottom: 1px solid #eef4f1;
}

.tests-summary-table tbody tr:last-child td {
  border-bottom: none;
}

.tests-summary-table tbody td:last-child {
  text-align: right;
  font-weight: 700;
  color: var(--ink);
  font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;
}

.tests-summary-table tbody td:first-child {
  color: var(--muted);
  font-weight: 500;
}

.info-card {
  background: linear-gradient(135deg, #eef2ff 0%, #f5f3ff 100%);
  border: 1px solid rgba(99, 102, 241, 0.16);
  border-radius: 18px;
  padding: 16px 18px;
  margin-bottom: 18px;
  color: var(--brand-dark);
  font-size: 14px;
  line-height: 1.7;
  font-weight: 500;
}

.info-card.success {
  background: linear-gradient(135deg, #def7ec 0%, #effcf6 100%);
  border-color: rgba(16, 185, 129, 0.22);
  color: #0f5132;
}

.info-card.warning {
  background: linear-gradient(135deg, #fef3c7 0%, #fff7df 100%);
  border-color: rgba(245, 158, 11, 0.28);
  color: #92400e;
}

.kv {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 8px 14px;
  font-size: 13px;
}

.kv .k {
  color: var(--muted);
  font-weight: 500;
}

.kv .v {
  color: var(--ink);
  font-weight: 700;
  text-align: right;
  overflow-wrap: break-word;
  word-break: break-word;
  font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;
}

.test-card-body {
  display: flex;
  flex-direction: column;
  gap: 14px;
  min-height: 0;
}

.interpretation {
  margin-top: 4px;
  padding: 14px 16px;
  border-radius: 14px;
  border: 1px solid transparent;
  font-size: 14px;
  font-weight: 600;
  width: 100%;
  box-sizing: border-box;
}

.tests-results-grid .test-card-body {
  flex: 1 1 auto;
  min-height: 180px;
}

.tests-results-grid .interpretation {
  margin-top: auto;
}

.interpretation-warning {
  background: #e8f1fb;
  border-color: #c7dcf4;
  color: #1e3a5f;
}

.interpretation-success {
  background: #e7f7ef;
  border-color: #bfe9cf;
  color: #115737;
}

.interpretation-info {
  background: #eef2ff;
  border-color: #d4dcff;
  color: #283d8f;
}

.plot-row {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(360px, 1fr));
  gap: 22px;
  margin-top: 18px;
}

.download-section {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  margin-top: 22px;
  padding-top: 20px;
  border-top: 1px solid rgba(19, 42, 35, 0.08);
}

.shiny-plot-output {
  border-radius: 18px;
  overflow: hidden;
  background: #ffffff;
  border: 1px solid rgba(19, 42, 35, 0.08);
  box-shadow: 0 10px 24px rgba(19, 42, 35, 0.05);
}

.dataTables_wrapper {
  font-size: 14px;
  width: 100%;
  overflow-x: auto;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
  font-weight: 700;
  border: none;
}

table.dataTable tbody td {
  border-bottom: 1px solid #eef4f1;
}

table.dataTable tbody tr:hover {
  background: #f7f5ff;
}

.badge {
  display: inline-block;
  padding: 6px 12px;
  border-radius: 999px;
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
  font-size: 12px;
  font-weight: 700;
}

.btn {
  padding: 11px 18px;
  border-radius: 12px;
  border: none;
  font-size: 14px;
  font-weight: 600;
  transition: all 0.2s ease;
}

.btn-primary {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
}

.btn-primary:hover {
  box-shadow: 0 14px 30px rgba(99, 102, 241, 0.16);
}

.btn-secondary {
  background: linear-gradient(135deg, #64748b 0%, #475569 100%);
  color: #ffffff;
}

.btn-success {
  background: linear-gradient(135deg, #10b981 0%, #059669 100%);
  color: #ffffff;
}

.btn-warning {
  background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%);
  color: #ffffff;
}

.btn-info {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
  color: #ffffff;
}

.btn-block {
  width: 100%;
  margin-bottom: 10px;
}

.form-group {
  margin-bottom: 18px;
}

.form-group label {
  display: block;
  margin-bottom: 8px;
  color: #ebfff7;
  font-size: 13px;
  font-weight: 600;
}

.content-card .form-group label {
  color: var(--ink);
}

.content-card .helper-text {
  color: var(--muted);
}

.content-card .checkbox label {
  color: var(--ink);
}

.form-control,
.selectize-input,
input[type='number'] {
  width: 100%;
  padding: 11px 13px;
  border-radius: 12px;
  border: 1px solid rgba(19, 42, 35, 0.10);
  background: rgba(255, 255, 255, 0.96);
  color: var(--ink);
  font-size: 14px;
}

.form-control:focus,
.selectize-input.focus,
input[type='number']:focus {
  border-color: rgba(99, 102, 241, 0.40);
  box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15);
}

.selectize-input {
  box-shadow: none;
}

.selectize-dropdown {
  border: 1px solid rgba(19, 42, 35, 0.10);
  border-radius: 12px;
  box-shadow: 0 16px 30px rgba(19, 42, 35, 0.14);
}

.checkbox {
  display: flex;
  align-items: center;
  gap: 10px;
  margin: 0;
}

.checkbox .form-group {
  margin-bottom: 0;
}

.checkbox input[type='checkbox'] {
  width: 18px;
  height: 18px;
  margin: 0;
  accent-color: var(--brand);
}

.checkbox label {
  margin: 0;
  color: #ebfff7;
  font-size: 13px;
  font-weight: 500;
}

.file-upload-wrapper {
  position: relative;
  margin-bottom: 12px;
}

.file-upload-label {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 6px;
  padding: 22px 16px;
  border-radius: 16px;
  border: 1px dashed rgba(199, 210, 254, 0.42);
  background: rgba(255, 255, 255, 0.05);
  cursor: pointer;
  text-align: center;
  transition: all 0.2s ease;
}

.file-upload-label:hover {
  background: rgba(255, 255, 255, 0.08);
  border-color: rgba(199, 210, 254, 0.64);
}

.upload-icon {
  color: #d8fff4;
  font-size: 13px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
}

.upload-text {
  color: #ffffff;
  font-size: 15px;
  font-weight: 700;
}

.upload-subtext {
  color: rgba(232, 252, 244, 0.72);
  font-size: 12px;
}

input[type='file'] {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  border: 0;
}

.file-name-display {
  display: none !important;
}

.file-upload-wrapper .shiny-input-container,
.file-upload-wrapper .form-control {
  width: 100%;
}

.file-upload-wrapper .form-control {
  margin-top: 8px;
  text-align: center;
  background: rgba(255, 255, 255, 0.96);
  border: 1px solid rgba(99, 102, 241, 0.16);
  box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.8);
}

.file-upload-wrapper .progress {
  margin-top: 8px;
  border-radius: 10px;
  overflow: hidden;
  box-shadow: inset 0 1px 2px rgba(19, 42, 35, 0.08);
}

.file-upload-wrapper .progress-bar {
  background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
}

#file-progress,
.btn-file {
  display: none !important;
}

.app-shell.sidebar-collapsed .study-sidebar {
  width: 96px;
  flex-basis: 96px;
}

.app-shell.sidebar-collapsed .nav-group-title,
.app-shell.sidebar-collapsed .nav-label,
.app-shell.sidebar-collapsed .utility-section {
  display: none;
}

.app-shell.sidebar-collapsed .sidebar-scroll {
  display: flex;
  flex-direction: column;
  align-items: stretch;
  padding: 18px 10px 22px;
}

.app-shell.sidebar-collapsed .sidebar-nav {
  width: 100%;
  gap: 14px;
  margin-bottom: 0;
}

.app-shell.sidebar-collapsed .nav-group {
  justify-items: center;
  gap: 10px;
  padding: 0;
}

.app-shell.sidebar-collapsed .nav-group + .nav-group {
  margin-top: 2px;
  padding-top: 16px;
  border-top: 1px solid rgba(235, 255, 247, 0.12);
}

.app-shell.sidebar-collapsed .nav-button.btn {
  width: 56px;
  min-width: 56px;
  height: 56px;
  padding: 0;
  justify-content: center;
  border-radius: 18px;
}

.app-shell.sidebar-collapsed .nav-button .action-label {
  justify-content: center;
  gap: 0;
}

.app-shell.sidebar-collapsed .nav-button.btn i {
  width: auto;
  margin: 0;
  font-size: 18px;
}

.app-shell.sidebar-collapsed .nav-button.btn:hover {
  background: rgba(255, 255, 255, 0.11);
}

.app-shell.sidebar-collapsed .nav-button.btn.is-active {
  background: linear-gradient(135deg, rgba(255, 255, 255, 0.22) 0%, rgba(199, 210, 254, 0.28) 100%);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.08), 0 12px 22px rgba(6, 27, 24, 0.28);
}

@media (max-width: 1100px) {
  .shell-body {
    flex-direction: column;
  }

  .study-sidebar,
  .app-shell.sidebar-collapsed .study-sidebar {
    position: static;
    height: auto;
    width: 100%;
    flex-basis: auto;
  }

  .app-shell.sidebar-collapsed .nav-group-title,
  .app-shell.sidebar-collapsed .nav-label,
  .app-shell.sidebar-collapsed .utility-section {
    display: initial;
  }

  .app-shell.sidebar-collapsed .nav-button.btn {
    justify-content: flex-start;
    padding-left: 14px;
    padding-right: 14px;
  }

  .app-shell.sidebar-collapsed .nav-button.btn i {
    margin-right: 0;
  }

  .upload-summary-grid {
    grid-template-columns: 1fr;
  }

  .descriptive-filter-row,
  .descriptive-metric-grid,
  .descriptive-filter-row-two,
  .descriptive-filter-row-three {
    grid-template-columns: 1fr;
  }

  .descriptive-filter-block-wide {
    grid-column: auto;
  }
}

@media (max-width: 768px) {
  .shell-topbar {
    align-items: flex-start;
    flex-direction: column;
  }

  .workspace-main {
    padding: 20px 16px 32px;
  }

  .hero-section {
    padding: 32px 24px 28px;
    border-radius: 24px;
  }

  .hero-title {
    font-size: 30px;
  }

  .preview-bar {
    flex-direction: column;
    gap: 18px;
    padding: 20px 24px;
  }

  .preview-divider {
    width: 100%;
    height: 1px;
    margin: 0;
  }

  .preview-stat {
    justify-content: flex-start;
    width: 100%;
  }

  .bento {
    grid-template-columns: 1fr;
  }

  .more-grid {
    grid-template-columns: 1fr;
  }

  .dataset-page-header {
    flex-direction: column;
    align-items: stretch;
  }

  .plot-row,
  .stats-grid,
  .visual-stats,
  .test-grid,
  .dataset-layout,
  .descriptive-metric-grid,
  .regression-builder-grid {
    grid-template-columns: 1fr;
  }

  .tests-input-grid,
  .tests-results-grid {
    grid-template-columns: 1fr;
  }

  .tests-page-hero {
    padding: 0;
  }

  .tests-page-hero p {
    white-space: normal;
  }

  .tests-control-checkbox .checkbox label {
    white-space: normal;
  }

  .tests-panel {
    padding: 20px 16px;
  }

  .descriptive-num-stats-grid,
  .descriptive-num-stats-grid.stats-count-5,
  .descriptive-num-stats-grid.stats-count-6 {
    grid-template-columns: 1fr;
  }

  .dataset-control-card {
    position: static;
  }

  .upload-stat-card {
    min-height: auto;
  }

  .descriptive-tab-shell {
    padding: 0;
    border-radius: 0;
  }

  .descriptive-tab-row {
    gap: 10px;
    justify-content: flex-start;
  }

  .descriptive-tab-button.btn {
    width: 100%;
    justify-content: center;
  }

  .descriptive-tab-button.btn {
    min-height: 64px;
    border-radius: 10px;
  }

  .inferential-tab-button.btn {
    width: 100%;
    justify-content: center;
  }

  .inferential-tab-button.btn {
    min-height: 64px;
    border-radius: 10px;
  }

  .descriptive-content-shell {
    padding: 24px 20px;
  }
}
"

---
layout: post
title: Validating Research While Protecting Health Data
---

Independent validation can strengthen confidence in research findings. ADSI uses different validation approaches across analyses, depending on the data and research questions.

For analyses involving linked UW Health electronic health records and Wisconsin birth records, an additional challenge arises: how can an external team independently evaluate the analysis without direct access to protected health data?

### How does it work?

Our approach is designed around two separate secure computing environments.

**The external validation team will be able to:**

<ul>
  <li>Review the analysis code</li>
  <li>Initiate a validation run</li>
  <li>Review approved, non-sensitive results</li>
</ul>

**The protected data:**

<ul>
  <li>Remain in a separate secure environment</li>
  <li>Are accessible only to authorized researchers</li>
  <li>Are not directly accessible to the external validation team</li>
</ul>

<div style="text-align:center; margin:2em 0;">
  <img src="{{ 'images/valrep.png' | relative_url }}"
       alt="Diagram illustrating how independent validation of ADSI analyses can take place across two separate secure computing environments, allowing external researchers to review the analytic workflow without directly accessing protected health data."
       title="Independent Validation While Protecting Health Data"
       width="85%"
       style="display:block; margin:auto;">
</div>

<div style="width:85%; margin:-0.75em auto 1.5em; text-align:center; font-size:0.9em;">
  <em>Separate secure computing environments allow independent validation of the analytic workflow while protecting access to linked health data.</em>
</div>

### Why separate the environments?

The separation allows independent researchers to evaluate whether the analytic workflow performs as intended without needing access to the protected linked health data themselves.

Together with synthetic-data testing and cybersecurity review, this approach is designed to balance:

<div style="text-align:center; margin:1.5em auto; font-size:1.05em;">
  <strong>Independent Validation &bull; Reproducibility &bull; Data Privacy</strong>
</div>

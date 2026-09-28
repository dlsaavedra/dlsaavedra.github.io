---
layout: page
title: Current Projects
permalink: /projects/
description: Research in Bayesian nonparametrics, quantile regression, and species sampling
nav: true
nav_order: 4
---

<div class="projects current-projects">
  <div class="grid">
    <div class="grid-sizer"></div>
    {%- assign sorted_projects = site.projects | sort: 'importance' -%}
    {%- for project in sorted_projects -%}
      {% include projects.html %}
    {%- endfor -%}
  </div>
</div>

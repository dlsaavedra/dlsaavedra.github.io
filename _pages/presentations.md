---
layout: page
permalink: /presentations/
title: Presentations
description: Talks and presentation materials
nav: true
nav_order: 5
---

{% assign presentation_folder = '/assets/pdf/presentations/' %}
{% assign presentation_count = 0 %}
{% capture presentation_links %}
  {% assign presentation_files = site.static_files | sort: 'path' %}
  {% for file in presentation_files %}
    {% if file.path contains presentation_folder and file.extname == '.pdf' %}
      {% assign presentation_count = presentation_count | plus: 1 %}
      {% assign filename = file.path | split: '/' | last %}
      {% assign presentation_title = filename | remove: '.pdf' | replace: '-', ' ' | replace: '_', ' ' %}
      <li><a href="{{ file.path | relative_url }}">{{ presentation_title }}</a></li>
    {% endif %}
  {% endfor %}
{% endcapture %}
{% if presentation_count > 0 %}
<ul>{{ presentation_links }}</ul>
{% else %}
<p>Presentation PDFs will be added here.</p>
{% endif %}

---
layout: page
permalink: /teaching/
title: Teaching
description: Courses taught and teaching assistantships
nav: true
nav_order: 5
---

## Courses taught

Each course links to a page for its PDF materials.

{% assign courses = site.courses | sort: 'order' %}
<ul>
{% for course in courses %}
  <li><a href="{{ course.url | relative_url }}">{{ course.title }}</a> — {{ course.institution }} ({{ course.period }})</li>
{% endfor %}
</ul>

## Teaching assistantships

Pontificia Universidad Católica de Chile, Schools of Engineering and Mathematics (2014–2019):

- Calculus
- Differential Equations
- Probabilistic Models
- Statistical Inference
- Stochastic Models
- Image Processing
- Pattern Recognition
- Specialization Project

---
layout: page
permalink: /teaching/
title: Teaching
description: Courses taught and teaching assistantships
nav: true
nav_order: 6
---

<details open>
<summary><h2 style="display: inline">Courses taught</h2></summary>

<ul>
{% for course in site.data.teaching_courses.docencia %}
  <li><a href="{{ course.url | relative_url }}">{{ course.title | escape }}</a>{% if course.details != empty %} — {{ course.details | escape }}{% endif %}</li>
{% endfor %}
</ul>
</details>

<details open>
<summary><h2 style="display: inline">Teaching assistantships</h2></summary>

<ul>
{% for course in site.data.teaching_courses.ayudantias %}
  <li><a href="{{ course.url | relative_url }}">{{ course.title | escape }}</a>{% if course.details != empty %} — {{ course.details | escape }}{% endif %}</li>
{% endfor %}
</ul>
</details>

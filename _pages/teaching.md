---
layout: page
permalink: /teaching/
title: Teaching
description: Courses taught and teaching assistantships
nav: true
nav_order: 6
---

## Courses taught

<ul>
{% for course in site.data.teaching_courses.docencia %}
  <li><a href="{{ course.url | relative_url }}">{{ course.title | escape }}</a>{% if course.details != empty %} — {{ course.details | escape }}{% endif %}</li>
{% endfor %}
</ul>

## Teaching assistantships

<ul>
{% for course in site.data.teaching_courses.ayudantias %}
  <li><a href="{{ course.url | relative_url }}">{{ course.title | escape }}</a>{% if course.details != empty %} — {{ course.details | escape }}{% endif %}</li>
{% endfor %}
</ul>

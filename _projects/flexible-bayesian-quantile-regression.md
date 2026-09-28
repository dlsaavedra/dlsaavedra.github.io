---
layout: page
title: Flexible Bayesian Quantile Regression
description: Flexible error distributions for nonlinear conditional quantiles and more informative uncertainty estimates.
img: assets/img/projects/flexible-bayesian-quantile-regression.png
importance: 2
---

<figure class="text-center"><img src="{{ '/assets/img/projects/flexible-bayesian-quantile-regression.png' | relative_url }}" alt="Nonlinear quantile curves and a changing uncertainty band through scattered observations" class="img-fluid rounded" style="width: 100%; max-width: 520px;"></figure>

Classical nonparametric quantile regression estimates a conditional quantile curve by minimizing the check loss. For a target quantile \(\tau\), its basic form is

$$
\widehat q_\tau
=\underset{q\in\mathcal F}{\operatorname{argmin}}
\sum_{i=1}^{n}\rho_\tau\!\left(y_i-q(x_i)\right),
\qquad
\rho_\tau(u)=u\bigl(\tau-\mathbf 1\{u<0\}\bigr).
$$

This project aims to obtain comparably flexible quantile curves in a Bayesian model while allowing the error distribution to adapt to features of the data. Rather than relying on one fixed error shape, the model combines a flexible curve with a distribution whose \(\tau\)-quantile remains at zero:

$$
Y_i=q_\tau(x_i)+\varepsilon_i,
\qquad
\Pr(\varepsilon_i\leq 0\mid x_i)=\tau,
\qquad
f_{\varepsilon\mid x}(e)=\int k(e\mid\vartheta,x)\,dG_x(\vartheta).
$$

The quantile constraint is imposed on the flexible error model. This makes it possible to study both the quantile curve and the conditional spread, including

$$
\operatorname{Var}(Y\mid x)
=\int \bigl(e-\mathbb E[\varepsilon\mid x]\bigr)^2
f_{\varepsilon\mid x}(e)\,de.
$$

The goal is to compare the estimated curves with classical nonparametric quantile regression while obtaining an uncertainty and variance assessment that reflects the fitted error distribution.
